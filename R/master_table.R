# Stop on duplicate keys instead of silently keeping an arbitrary record.
check_unique_key <- function(table, columns, label) {
  keys <- table[, columns, drop = FALSE]
  if (anyNA(keys) || anyDuplicated(keys)) stop(label, " has missing or duplicate keys.")
  invisible(table)
}

# CSV files do not retain R types. Use our extraction template to read them back
# consistently: IDs stay text, measurements stay numeric, and flags stay logical.
read_source_csv <- function(path, template) {
  types <- vapply(template, \(column) switch(typeof(column),
    character = "c", double = "d", integer = "i", logical = "l",
    stop("Unexpected CSV column type")), character(1))
  table <- readr::read_csv(path, col_types = paste0(types, collapse = ""),
                            na = "", show_col_types = FALSE)
  if (!identical(names(table), names(template)) || nrow(readr::problems(table))) {
    stop("CSV columns or types do not match the expected structure: ", path)
  }
  table
}

# Parse dates in UTC, and reject unparseable nonmissing values.
parse_utc <- function(x) {
  result <- lubridate::ymd_hms(x, tz = "UTC", quiet = TRUE)
  if (any(!is.na(x) & is.na(result))) stop("An API timestamp could not be parsed.")
  result
}

# This is the shared join. Each lookup contributes at most one row to a launch.
build_master_table <- function(launches, agencies, configs, statuses, agency_types,
                               year_range = NULL) {
  check_unique_key(launches, "launch_id", "Launches")
  check_unique_key(agencies, "agency_id", "Agencies")
  check_unique_key(configs, "config_id", "Configurations")
  check_unique_key(statuses, "status_id", "Statuses")
  check_unique_key(agency_types, "agency_type_id", "Agency types")
  master <- launches |>
    dplyr::left_join(agencies, by = c("provider_id" = "agency_id"), relationship = "many-to-one") |>
    dplyr::left_join(agency_types, by = "agency_type_id", relationship = "many-to-one") |>
    dplyr::left_join(configs, by = c("rocket_config_id" = "config_id"), relationship = "many-to-one") |>
    dplyr::left_join(statuses, by = "status_id", relationship = "many-to-one") |>
    dplyr::mutate(
      net = parse_utc(net), year = lubridate::year(net),
      last_updated = parse_utc(last_updated),
      config_maiden_flight = as.Date(config_maiden_flight),
      provider_name = dplyr::coalesce(agency_name, provider_name_reported),
      outcome = classify_outcome(status_name),
      sector = classify_sector(agency_type, provider_id),
      sector_rule = dplyr::if_else(provider_id %in% sector_overrides$provider_id,
                                   "team_override", "current_agency_type"),
      analysis_eligible = outcome %in% c("success", "failure", "partial_failure"),
      provider_join_missing = is.na(agency_name),
      config_join_missing = is.na(config_name),
      status_join_missing = is.na(status_name),
      agency_type_join_missing = is.na(agency_type),
      provider_type_disagrees = !is.na(provider_type_id_reported) & !is.na(agency_type_id) &
        provider_type_id_reported != agency_type_id
    )
  if (nrow(master) != nrow(launches)) stop("A join changed the number of launches.")
  if (anyNA(master$net) || any(master$net >= as.POSIXct("2026-01-01", tz = "UTC"))) {
    stop("Missing launch dates or launches outside the requested upper cutoff.")
  }
  if (any(master$status_join_missing)) stop("Some launch statuses have no reference entry.")
  if (any(master$status_name_reported != master$status_name, na.rm = TRUE)) {
    stop("Embedded status labels disagree with the status reference snapshot.")
  }

  # Count only prior records with terminal outcomes for identified configurations.
  # Same-timestamp launches receive the same prior count; do not invent an order.
  experience <- master |>
    dplyr::filter(!is.na(rocket_config_id), !config_join_missing,
                  !is.na(config_is_placeholder), !config_is_placeholder) |>
    dplyr::group_by(rocket_config_id, net) |>
    dplyr::summarise(completed_here = sum(analysis_eligible),
                     unresolved_here = sum(!analysis_eligible),
                     records_at_timestamp = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(rocket_config_id, net) |>
    dplyr::group_by(rocket_config_id) |>
    dplyr::mutate(prior_flights = cumsum(completed_here) - completed_here,
                   prior_unresolved_records = cumsum(unresolved_here) - unresolved_here) |>
    dplyr::ungroup() |>
    dplyr::select(-completed_here, -unresolved_here)
  master <- master |>
    dplyr::left_join(experience, by = c("rocket_config_id", "net"), relationship = "many-to-one") |>
    dplyr::mutate(
      flight_number = dplyr::if_else(analysis_eligible & records_at_timestamp == 1L,
                                      prior_flights + 1L, NA_integer_),
      experience_order_uncertain = is.na(net_precision) |
        !(net_precision %in% c("Second", "Minute")) |
        dplyr::coalesce(records_at_timestamp > 1L, FALSE),
      # This is age of the configuration, not its launch experience.
      config_age_days = as.numeric(as.Date(net) - config_maiden_flight)
    ) |>
    dplyr::arrange(net, launch_id)
  # Filtering happens last, after experience has used the available pre-2000 history.
  if (!is.null(year_range)) master <- dplyr::filter(master, dplyr::between(year, year_range[1], year_range[2]))
  check_unique_key(master, "launch_id", "Master table")
  master
}

# Reports expose missingness and unmatched joins rather than quietly dropping rows.
write_quality_reports <- function(master, stages, families, folder) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  missingness <- tibble::tibble(
    field = names(master),
    missing = vapply(master, \(x) sum(is.na(x)), integer(1)),
    share_missing = vapply(master, \(x) mean(is.na(x)), double(1)))
  readr::write_csv(missingness, file.path(folder, "missingness.csv"), na = "")
  readr::write_csv(dplyr::count(master, sector, outcome), file.path(folder, "sector_outcomes.csv"), na = "")
  readr::write_csv(dplyr::count(master, year, sector, outcome), file.path(folder, "year_sector_outcomes.csv"), na = "")
  flags <- c("provider_join_missing", "config_join_missing", "status_join_missing",
             "agency_type_join_missing", "provider_type_disagrees", "experience_order_uncertain")
  readr::write_csv(
    dplyr::filter(master, dplyr::if_any(dplyr::all_of(flags), \(x) x)),
    file.path(folder, "records_to_review.csv"), na = "")
  # A first observed flight later than the stated maiden date is a coverage warning.
  coverage <- master |>
    dplyr::filter(!is.na(rocket_config_id)) |>
    dplyr::group_by(rocket_config_id, config_name, config_maiden_flight) |>
    dplyr::summarise(first_observed_date = min(as.Date(net)),
                     records = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(first_observed_after_maiden = first_observed_date > config_maiden_flight)
  readr::write_csv(coverage, file.path(folder, "configuration_history_coverage.csv"), na = "")
  if (any(!stages$launch_id %in% master$launch_id)) stop("Stage records refer to absent launches.")
  if (any(!families$config_id %in% unique(master$rocket_config_id))) {
    message("Some configurations have no launches in this history; their family links are retained.")
  }
}
