# Baseline rules verified against the downloaded LL2 reference labels.
# Preserve partial failure as its own outcome, not a fractional success score.
classify_outcome <- function(status_name) {
  dplyr::case_when(
    status_name == "Launch Successful" ~ "success",
    status_name == "Launch Failure" ~ "failure",
    status_name == "Launch was a Partial Failure" ~ "partial_failure",
    TRUE ~ "other"
  )
}

# Leave this empty until the team agrees on an exception and records its reason.
# Match by provider ID instead of a name that may change spelling.
sector_overrides <- tibble::tibble(
  provider_id = character(), sector = character(), reason = character()
)

classify_sector <- function(agency_type, provider_id, overrides = sector_overrides) {
  if (anyDuplicated(overrides$provider_id) || anyNA(overrides$provider_id) ||
      anyNA(overrides$sector) || anyNA(overrides$reason) ||
      any(!nzchar(trimws(overrides$reason))) ||
      any(!overrides$sector %in% c("commercial", "government", "unknown"))) {
    stop("Each sector override needs one provider ID, a valid sector, and a reason.")
  }
  baseline <- dplyr::case_when(
    agency_type == "Commercial" ~ "commercial",
    agency_type == "Government" ~ "government",
    TRUE ~ "unknown"
  )
  # Private, multinational, educational, and missing types remain unknown.
  replacement <- overrides$sector[match(provider_id, overrides$provider_id)]
  dplyr::coalesce(replacement, baseline)
}
