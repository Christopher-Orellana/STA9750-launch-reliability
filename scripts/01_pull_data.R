# Run from the open RStudio project. Use --sample on the command line for one page
# per endpoint, or set sample_only <- TRUE below while checking a small extract.
# If the system library is read-only, install packages into R's normal user library.
if (file.access(.libPaths()[1], 2) != 0) {
  user_library <- path.expand(strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep, fixed = TRUE)[[1]][1])
  if (!nzchar(user_library)) stop("Please configure a writable R user library.")
  dir.create(user_library, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(user_library, .libPaths()))
}
if (!requireNamespace("here", quietly = TRUE)) install.packages("here", repos = "https://cloud.r-project.org")
here::i_am("scripts/01_pull_data.R")
source(here::here("R", "ll2_api.R"))
load_packages()
source(here::here("R", "flatten.R"))
source(here::here("R", "classify.R"))
source(here::here("R", "master_table.R"))

# Read local credentials without displaying them. No key is needed for free access.
if (file.exists(here::here(".Renviron"))) readRenviron(here::here(".Renviron"))
sample_only <- "--sample" %in% commandArgs(trailingOnly = TRUE)

run_download <- function(sample_only = FALSE) {
  lock <- acquire_download_lock()
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  max_pages <- if (sample_only) 1L else Inf
  output <- if (sample_only) path_processed("sample", "source_tables") else path_processed("source_tables")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  manifest_path <- file.path(output, "source_manifest.csv")
  # The manifest is the completion marker. Do not let a failed run look complete.
  if (file.exists(manifest_path)) unlink(manifest_path)

  # All lists use 100 records per page, the documented maximum. Preserve suborbital
  # records for review; the team must decide its final orbital/suborbital scope.
  definitions <- list(
    launch_statuses = list("config/launch_statuses/", list(limit = 100)),
    agency_types = list("config/agency_types/", list(limit = 100)),
    agencies = list("agencies/", list(mode = "detailed", limit = 100, ordering = "id")),
    launcher_configurations = list("launcher_configurations/", list(mode = "detailed", limit = 100, ordering = "name")),
    launches = list("launches/previous/", list(mode = "detailed", limit = 100,
      ordering = "-net,id", net__lt = "2026-01-01T00:00:00Z", include_suborbital = "true"))
  )
  downloaded <- purrr::map(definitions, \(item) ll2_fetch_all(item[[1]], item[[2]], max_pages))
  if (!sample_only && !all(vapply(downloaded, \(x) x$complete, logical(1)))) {
    stop("Download incomplete. Final source CSVs were not published.")
  }
  # Separate extraction functions handle the different nested structures.
  tables <- list(
    launches = flatten_launches(downloaded$launches$records),
    agencies = flatten_agencies(downloaded$agencies$records),
    launcher_configurations = flatten_configs(downloaded$launcher_configurations$records),
    launch_statuses = flatten_statuses(downloaded$launch_statuses$records),
    agency_types = flatten_agency_types(downloaded$agency_types$records),
    configuration_families = flatten_config_families(downloaded$launcher_configurations$records),
    launch_stage_recovery = flatten_stages(downloaded$launches$records)
  )
  keys <- list("launch_id", "agency_id", "config_id", "status_id", "agency_type_id",
               c("config_id", "family_id"), c("launch_id", "stage_flight_id"))
  for (i in seq_along(tables)) check_unique_key(tables[[i]], keys[[i]], names(tables)[i])
  for (name in names(tables)) readr::write_csv(tables[[name]], file.path(output, paste0(name, ".csv")), na = "")
  files <- paste0(names(tables), ".csv")
  manifest <- tibble::tibble(
    file = files, rows = vapply(tables, nrow, integer(1)),
    md5 = unname(tools::md5sum(file.path(output, files))),
    complete = !sample_only,
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  readr::write_csv(dplyr::bind_rows(purrr::map(downloaded, "progress")),
                   file.path(output, "download_summary.csv"), na = "")
  readr::write_csv(manifest, manifest_path)
  if (sample_only) {
    message("SAMPLE ONLY: files are in data/processed/sample/source_tables; do not analyze them as complete data.")
  } else {
    message("All source files are complete. Run scripts/02_build_master_tables.R next.")
  }
  invisible(manifest)
}

run_download(sample_only)
