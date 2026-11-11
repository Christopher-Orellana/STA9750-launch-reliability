# This script reads local CSVs only. It never calls the API.
if (file.access(.libPaths()[1], 2) != 0) {
  user_library <- path.expand(strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep, fixed = TRUE)[[1]][1])
  if (!nzchar(user_library)) stop("Please configure a writable R user library.")
  dir.create(user_library, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(user_library, .libPaths()))
}
if (!requireNamespace("here", quietly = TRUE)) install.packages("here", repos = "https://cloud.r-project.org")
here::i_am("scripts/02_build_master_tables.R")
source(here::here("R", "ll2_api.R"))
load_packages()
source(here::here("R", "flatten.R"))
source(here::here("R", "classify.R"))
source(here::here("R", "master_table.R"))

build_shared_tables <- function() {
  folder <- path_processed("source_tables")
  manifest_path <- file.path(folder, "source_manifest.csv")
  if (!file.exists(manifest_path)) stop("No complete source manifest. Finish 01_pull_data.R first.")
  manifest <- readr::read_csv(manifest_path, show_col_types = FALSE)
  templates <- list(
    launches = flatten_launches(list()), agencies = flatten_agencies(list()),
    launcher_configurations = flatten_configs(list()),
    launch_statuses = flatten_statuses(list()), agency_types = flatten_agency_types(list()),
    configuration_families = flatten_config_families(list()),
    launch_stage_recovery = flatten_stages(list())
  )
  expected_files <- paste0(names(templates), ".csv")
  if (!identical(manifest$file, expected_files) || anyNA(manifest$complete) || !all(manifest$complete)) {
    stop("Source manifest is incomplete or lists unexpected files.")
  }
  hashes <- unname(tools::md5sum(file.path(folder, expected_files)))
  if (anyNA(hashes) || !identical(hashes, manifest$md5)) stop("Source files changed or are missing. Rebuild them from the cache.")
  tables <- purrr::imap(templates, \(template, name) read_source_csv(
    file.path(folder, paste0(name, ".csv")), template))
  if (!identical(unname(vapply(tables, nrow, integer(1))), as.integer(manifest$rows))) stop("Source row counts changed.")

  output <- path_processed("combined_launch_tables")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  completion <- file.path(output, "master_manifest.csv")
  if (file.exists(completion)) unlink(completion)
  master <- build_master_table(tables$launches, tables$agencies, tables$launcher_configurations,
                                tables$launch_statuses, tables$agency_types)
  study <- dplyr::filter(master, dplyr::between(year, 2000, 2025))
  write_quality_reports(master, tables$launch_stage_recovery, tables$configuration_families,
                          path_processed("quality_checks"))
  files <- c("master_launches_all_history_through_2025.csv", "master_launches_2000_2025.csv")
  readr::write_csv(master, file.path(output, files[1]), na = "")
  readr::write_csv(study, file.path(output, files[2]), na = "")
  readr::write_csv(tibble::tibble(
    file = files, rows = c(nrow(master), nrow(study)),
    md5 = unname(tools::md5sum(file.path(output, files))),
    source_manifest_md5 = unname(tools::md5sum(manifest_path)),
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  ), completion)
  print(dplyr::count(study, sector, outcome))
  message("Master tables saved. Review data/processed/quality_checks before analysis.")
  invisible(master)
}

build_shared_tables()
