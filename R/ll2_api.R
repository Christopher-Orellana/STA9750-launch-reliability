# Shared setup and download helpers. Sourcing this file does not call the API.

load_packages <- function() {
  packages <- c("httr2", "jsonlite", "dplyr", "purrr", "tibble",
                "lubridate", "here", "readr")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (requireNamespace("dplyr", quietly = TRUE) && utils::packageVersion("dplyr") < "1.1.0") {
    missing <- unique(c(missing, "dplyr"))
  }
  if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
  invisible(lapply(packages, \(package) suppressPackageStartupMessages(
    library(package, character.only = TRUE))))
}

# All paths are relative to the project, so teammates can use different computers.
path_raw <- function(...) here::here("data", "raw", "production", ...)
path_processed <- function(...) here::here("data", "processed", ...)

# Write to a temporary file first, so an interruption cannot leave half an RDS file.
save_rds_safely <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp")
  saveRDS(object, temporary)
  if (!file.rename(temporary, path)) stop("Cannot replace: ", path, ". The temporary file is retained.")
}

# Only use production for the shared snapshot. Development data must stay separate.
ll2_base_url <- function() {
  if (tolower(Sys.getenv("LL2_USE_DEV")) == "true") {
    stop("This pipeline is production-only. Set LL2_USE_DEV=false and restart R.")
  }
  "https://ll.thespacedevs.com/2.3.0/"
}

# Prevent two downloaders in this repository from spending the same request budget.
acquire_download_lock <- function() {
  dir.create(path_raw(), recursive = TRUE, showWarnings = FALSE)
  lock <- path_raw("download.lock")
  if (!dir.create(lock, showWarnings = FALSE)) {
    stop("A download lock exists. If no downloader is running, remove data/raw/",
         "production/download.lock and retry. Never run two downloaders together.")
  }
  writeLines(as.character(Sys.getpid()), file.path(lock, "process_id.txt"))
  lock
}

# Remember request times across restarts. All five endpoints share the same budget.
# The first 15 requests may fit in one hour; later requests wait for a slot.
# API use outside this repository can still cause HTTP 429, handled below.
wait_for_request_slot <- function() {
  history_path <- path_raw("request_times.rds")
  history <- if (file.exists(history_path)) readRDS(history_path) else numeric()
  now <- as.numeric(Sys.time())
  history <- history[history > now - 3605]
  if (!nzchar(Sys.getenv("LL2_API_KEY")) && length(history) >= 15L) {
    wait <- max(1, history[length(history) - 14L] + 3605 - now)
    message("Rate limit: waiting approximately ", ceiling(wait / 60), " minutes.")
    # Short sleeps keep R responsive to the Stop button.
    end <- Sys.time() + wait
    while (Sys.time() < end) Sys.sleep(min(5, as.numeric(difftime(end, Sys.time(), units = "secs"))))
  }
  if (nzchar(Sys.getenv("LL2_API_KEY"))) Sys.sleep(1)
  history <- c(history, as.numeric(Sys.time()))
  save_rds_safely(history, history_path)
}

# Fetch a page only if it has not already been saved for this exact URL.
ll2_get_page <- function(url, cache_path) {
  # Do not send an API key to a different host via an unexpected pagination link.
  if (!startsWith(url, ll2_base_url())) stop("Unexpected pagination URL: ", url)
  if (file.exists(cache_path)) {
    saved <- readRDS(cache_path)
    if (!identical(saved$url, url)) stop("Cached URL differs. Use a fresh snapshot folder.")
    return(saved)
  }
  for (attempt in seq_len(4L)) {
    wait_for_request_slot()
    request <- httr2::request(url) |>
      httr2::req_user_agent("Mozilla/5.0 (Team Rocket STA9750 academic data project)") |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_timeout(120) |>
      httr2::req_options(followlocation = FALSE) |>
      httr2::req_error(is_error = \(response) FALSE)
    key <- Sys.getenv("LL2_API_KEY")
    if (nzchar(key)) request <- httr2::req_headers(request, Authorization = paste("Token", key))
    response <- tryCatch(httr2::req_perform(request), error = \(e) NULL)
    code <- if (is.null(response)) 0L else httr2::resp_status(response)
    if (code == 200L) {
      # Keep the original JSON text, plus retrieval metadata, in each cache file.
      saved <- list(url = url, retrieved_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
                    json = httr2::resp_body_string(response))
      body <- jsonlite::fromJSON(saved$json, simplifyVector = FALSE)
      if (!all(c("count", "next", "results") %in% names(body)) || !is.list(body$results)) {
        stop("Unexpected page structure at ", url)
      }
      save_rds_safely(saved, cache_path)
      return(saved)
    }
    if (!(code %in% c(0L, 429L, 500L, 502L, 503L, 504L))) {
      stop("HTTP ", code, " at ", url, ". No CSV was published for this run.")
    }
    if (attempt == 4L) stop("Request failed four times. Cached pages are safe; rerun later.")
    retry_after <- if (is.null(response)) NULL else httr2::resp_header(response, "Retry-After")
    seconds <- suppressWarnings(as.numeric(retry_after))
    # Retry-After may contain an HTTP date instead of a number of seconds.
    if (length(seconds) != 1L || is.na(seconds)) {
      retry_date <- if (is.null(retry_after)) NA else as.POSIXct(
        retry_after, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
      seconds <- as.numeric(difftime(retry_date, Sys.time(), units = "secs"))
    }
    if (length(seconds) != 1L || !is.finite(seconds)) seconds <- if (code == 429L) 3605 else 30
    end <- Sys.time() + max(seconds + 5, 5)
    message("HTTP ", code, "; waiting before retry. Saved pages will be reused.")
    while (Sys.time() < end) Sys.sleep(min(5, as.numeric(difftime(end, Sys.time(), units = "secs"))))
  }
}

# Follow the API's next links. A capped sample is explicitly marked incomplete.
ll2_fetch_all <- function(endpoint, query, max_pages = Inf) {
  url <- httr2::request(paste0(ll2_base_url(), endpoint)) |>
    httr2::req_url_query(!!!query) |>
    purrr::pluck("url")
  first_url <- url
  folder <- path_raw(gsub("/", "_", sub("/$", "", sub("/previous/", "/", endpoint))))
  records <- list()
  seen_urls <- character()
  expected_count <- NULL
  page_number <- 0L
  while (!is.null(url) && page_number < max_pages) {
    if (url %in% seen_urls) stop("Repeated pagination link: ", url)
    seen_urls <- c(seen_urls, url)
    page_number <- page_number + 1L
    saved <- ll2_get_page(url, file.path(folder, sprintf("page_%05d.rds", page_number)))
    page <- jsonlite::fromJSON(saved$json, simplifyVector = FALSE)
    if (is.null(expected_count)) expected_count <- page$count
    if (!identical(page$count, expected_count)) {
      stop("API total changed during pagination for ", endpoint,
           ". Review the cached snapshot before continuing.")
    }
    if (!length(page$results) && !is.null(page[["next"]])) stop("Empty page with a next link.")
    records <- c(records, page$results)
    url <- page[["next"]]
    progress <- tibble::tibble(endpoint, initial_url = first_url, pages = page_number,
                               records = length(records), expected_count,
                               complete = is.null(url), next_url = if (is.null(url)) NA_character_ else url,
                               last_retrieval_utc = saved$retrieved_at_utc)
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(progress, file.path(folder, "progress.csv"))
    message(endpoint, " page ", page_number, ": ", length(records), "/", expected_count, " records")
  }
  ids <- vapply(records, \(x) as.character(x$id), character(1))
  if (anyNA(ids) || anyDuplicated(ids)) stop("Missing or duplicate IDs in ", endpoint)
  complete <- is.null(url)
  if (complete && length(records) != expected_count) stop("Record count does not match API total.")
  list(records = records, complete = complete, progress = progress)
}
