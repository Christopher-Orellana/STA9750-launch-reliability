# Convert nested API objects into small, rectangular tables for CSV files.
# NULL means genuinely missing. A missing field name raises an error instead of
# silently making an all-NA column when the API changes or a path is misspelled.
pick <- function(x, ...) {
  for (key in list(...)) {
    if (is.null(x)) return(NULL)
    if (!is.list(x) || !(key %in% names(x))) stop("Expected API field is absent: ", key)
    x <- x[[key]]
  }
  x
}

# These helpers keep missing values and column types consistent across records.
text_value <- function(x, ...) {
  value <- pick(x, ...)
  if (is.null(value)) return(NA_character_)
  if (length(value) != 1L || is.list(value)) stop("Expected a single text value.")
  value <- as.character(value)
  if (!nzchar(trimws(value))) NA_character_ else value
}
number_value <- function(x, ...) {
  value <- pick(x, ...)
  if (is.null(value)) return(NA_real_)
  if (!is.numeric(value) || length(value) != 1L) stop("Expected a single number.")
  as.double(value)
}
logical_value <- function(x, ...) {
  value <- pick(x, ...)
  if (is.null(value)) return(NA)
  if (!is.logical(value) || length(value) != 1L) stop("Expected TRUE or FALSE.")
  value
}

# A NULL template supplies column names even if an endpoint or nested list is empty.
rows_to_table <- function(records, convert) {
  dplyr::bind_rows(convert(NULL)[0, ], purrr::map(records, convert))
}

flatten_launches <- function(records) {
  rows_to_table(records, \(x) tibble::tibble(
    launch_id = text_value(x, "id"),
    launch_name = text_value(x, "name"),
    source_url = text_value(x, "url"),
    launch_designator = text_value(x, "launch_designator"),
    net = text_value(x, "net"),
    net_precision = text_value(x, "net_precision", "name"),
    last_updated = text_value(x, "last_updated"),
    status_id = text_value(x, "status", "id"),
    status_name_reported = text_value(x, "status", "name"),
    failure_description = text_value(x, "failreason"),
    provider_id = text_value(x, "launch_service_provider", "id"),
    provider_name_reported = text_value(x, "launch_service_provider", "name"),
    provider_type_id_reported = text_value(x, "launch_service_provider", "type", "id"),
    rocket_config_id = text_value(x, "rocket", "configuration", "id"),
    rocket_name_reported = text_value(x, "rocket", "configuration", "full_name"),
    mission_type = text_value(x, "mission", "type"),
    mission_description = text_value(x, "mission", "description"),
    orbit_id = text_value(x, "mission", "orbit", "id"),
    orbit_name = text_value(x, "mission", "orbit", "name"),
    orbit_body_name = text_value(x, "mission", "orbit", "celestial_body", "name"),
    pad_id = text_value(x, "pad", "id"),
    pad_name = text_value(x, "pad", "name"),
    pad_country = text_value(x, "pad", "country", "alpha_3_code"),
    location_name = text_value(x, "pad", "location", "name"),
    launch_body_name = text_value(x, "pad", "location", "celestial_body", "name"),
    # Keep this as an API-reported value, not a replacement for configuration experience.
    orbital_launch_attempt_count = number_value(x, "orbital_launch_attempt_count"),
    stage_records = length(pick(x, "rocket", "launcher_stage"))
  ))
}

flatten_agencies <- function(records) {
  rows_to_table(records, \(x) tibble::tibble(
    agency_id = text_value(x, "id"), agency_name = text_value(x, "name"),
    agency_abbrev = text_value(x, "abbrev"),
    agency_type_id = text_value(x, "type", "id"),
    agency_type_reported = text_value(x, "type", "name"),
    agency_founding_year = number_value(x, "founding_year"),
    agency_description = text_value(x, "description")
  ))
}

flatten_configs <- function(records) {
  rows_to_table(records, \(x) tibble::tibble(
    config_id = text_value(x, "id"), config_name = text_value(x, "full_name"),
    config_variant = text_value(x, "variant"),
    config_is_placeholder = logical_value(x, "is_placeholder"),
    config_reusable = logical_value(x, "reusable"),
    config_maiden_flight = text_value(x, "maiden_flight"),
    manufacturer_id = text_value(x, "manufacturer", "id"),
    config_description = text_value(x, "description"),
    config_min_stages = number_value(x, "min_stage"),
    config_max_stages = number_value(x, "max_stage"),
    leo_capacity_kg = number_value(x, "leo_capacity"),
    gto_capacity_kg = number_value(x, "gto_capacity"),
    family_records = length(pick(x, "families"))
  ))
}

flatten_statuses <- function(records) {
  rows_to_table(records, \(x) tibble::tibble(
    status_id = text_value(x, "id"), status_name = text_value(x, "name"),
    status_abbrev = text_value(x, "abbrev"),
    status_description = text_value(x, "description")
  ))
}
flatten_agency_types <- function(records) {
  rows_to_table(records, \(x) tibble::tibble(
    agency_type_id = text_value(x, "id"), agency_type = text_value(x, "name")
  ))
}

# A configuration may belong to several families. Preserve every relationship.
flatten_config_families <- function(records) {
  template <- tibble::tibble(config_id = character(), family_id = character(),
                             family_name = character(), parent_family_id = character())
  rows <- purrr::map(records, \(config) {
    purrr::map(pick(config, "families"), \(family) tibble::tibble(
      config_id = text_value(config, "id"), family_id = text_value(family, "id"),
      family_name = text_value(family, "name"),
      parent_family_id = text_value(family, "parent", "id")
    )) |> dplyr::bind_rows()
  })
  dplyr::bind_rows(template, rows)
}

# A launch may have zero, one, or several recorded stages. No stage record is
# missing coverage, not evidence that no landing was attempted.
flatten_stages <- function(records) {
  convert <- function(launch, stage) tibble::tibble(
    launch_id = text_value(launch, "id"), stage_flight_id = text_value(stage, "id"),
    stage_type = text_value(stage, "type"),
    launcher_id = text_value(stage, "launcher", "id"),
    launcher_serial_number = text_value(stage, "launcher", "serial_number"),
    launcher_is_placeholder = logical_value(stage, "launcher", "is_placeholder"),
    reused = logical_value(stage, "reused"),
    launcher_flight_number = number_value(stage, "launcher_flight_number"),
    previous_flight_date = text_value(stage, "previous_flight_date"),
    turnaround_duration = text_value(stage, "turn_around_time"),
    landing_id = text_value(stage, "landing", "id"),
    landing_attempt = logical_value(stage, "landing", "attempt"),
    landing_success = logical_value(stage, "landing", "success"),
    landing_description = text_value(stage, "landing", "description"),
    landing_type = text_value(stage, "landing", "type", "name")
  )
  rows <- purrr::map(records, \(launch) {
    purrr::map(pick(launch, "rocket", "launcher_stage"), \(stage) convert(launch, stage)) |>
      dplyr::bind_rows()
  })
  dplyr::bind_rows(convert(NULL, NULL)[0, ], rows) |>
    dplyr::mutate(recovery_outcome = dplyr::case_when(
      is.na(landing_attempt) ~ "unknown",
      !landing_attempt ~ "not_attempted",
      is.na(landing_success) ~ "unknown",
      landing_success ~ "success",
      TRUE ~ "failure"
    ))
}
