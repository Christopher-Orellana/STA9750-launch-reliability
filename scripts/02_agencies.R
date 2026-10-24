# =============================================================================
# LAUNCH LIBRARY 2: DOWNLOAD AND CLEAN AGENCY DATA
# =============================================================================

# Load the packages used in this script.
#
# httr2:
#   Creates and performs API requests.
#
# purrr:
#   Works with lists and nested JSON data.
#
# dplyr:
#   Cleans, transforms, joins, and summarizes data.
#
# tibble:
#   Creates tidyverse-friendly data frames.
library(httr2)
library(purrr)
library(dplyr)
library(tibble)


# =============================================================================
# 1. SET API AND CACHE LOCATIONS
# =============================================================================

# Base URL for version 2.3.0 of the Launch Library 2 development API.
BASE <- "https://lldev.thespacedevs.com/2.3.0"

# Cache file containing the original nested agency records returned by the API.
AGENCY_CACHE_FILE <- "ll2_agencies_raw.rds"

# File containing the cleaned one-row-per-agency table.
AGENCY_CLEAN_FILE <- "ll2_agencies.rds"

# CSV version of the cleaned agency table.
AGENCY_CSV_FILE <- "ll2_agencies.csv"


# =============================================================================
# 2. CREATE HELPER FUNCTIONS
# =============================================================================

# Return the value on the left unless it is NULL.
#
# If the value on the left is NULL, return the fallback value on the right.
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}


# Convert an API field into one character value.
#
# This helper safely handles:
#   NULL values
#   Empty values
#   Character vectors containing more than one value
#
# Multiple values are combined using a comma and a space.
as_single_character <- function(x) {
  
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  
  paste(
    as.character(x),
    collapse = ", "
  )
}


# =============================================================================
# 3. DEFINE A FUNCTION TO DOWNLOAD AGENCY RECORDS
# =============================================================================

# This function retrieves agency records from the LL2 /agencies/ endpoint.
#
# page_size:
#   Maximum number of agency records requested during each API call.
fetch_agencies <- function(page_size = 100) {
  
  # Create an empty list that will eventually contain all agency records.
  out <- list()
  
  # Begin with the first API record.
  #
  # The API uses limit and offset for pagination:
  #   offset = 0 retrieves the first page.
  #   offset = 100 retrieves the next page when page_size = 100.
  offset <- 0
  
  
  # Continue making requests until the API reports that there is no next page.
  repeat {
    
    # -------------------------------------------------------------------------
    # Construct and perform the API request
    # -------------------------------------------------------------------------
    
    resp <- request(BASE) |>
      
      # Add the agencies endpoint to the base URL.
      #
      # Result:
      # https://lldev.thespacedevs.com/2.3.0/agencies/
      req_url_path_append("agencies/") |>
      
      # Add response and pagination parameters.
      req_url_query(
        
        # Request detailed nested agency information.
        mode = "detailed",
        
        # Request up to page_size records per API call.
        limit = page_size,
        
        # Specify where the current page begins.
        offset = offset,
        
        # Sort agency records by ID.
        ordering = "id",
        
        # Ask the API to return JSON.
        format = "json"
      ) |>
      
      # Retry temporary failures, such as rate-limit or server responses.
      req_retry(
        max_tries = 5,
        backoff = ~ 10
      ) |>
      
      # Send the API request.
      req_perform()
    
    
    # -------------------------------------------------------------------------
    # Parse and accumulate the API response
    # -------------------------------------------------------------------------
    
    # Convert the JSON response into nested R lists.
    body <- resp_body_json(
      resp,
      simplifyVector = FALSE
    )
    
    
    # Add the current page's agency records to the cumulative list.
    out <- c(
      out,
      body$results %||% list()
    )
    
    
    # Display the download progress.
    message(
      sprintf(
        "Fetched %d / %d agencies",
        length(out),
        body$count %||% length(out)
      )
    )
    
    
    # Stop if the API reports that there is no next page.
    if (is.null(body[["next"]])) {
      break
    }
    
    
    # Increase the offset to retrieve the next group of records.
    offset <- offset + page_size
    
    
    # Pause before making the next request.
    Sys.sleep(1)
    
  } # Ends the repeat loop
  
  
  # Return the complete nested list of agency records.
  out
  
} # Ends the fetch_agencies() function


# =============================================================================
# 4. LOAD THE RAW CACHE OR DOWNLOAD THE AGENCY DATA
# =============================================================================

# Check whether the raw agency cache already exists.
if (file.exists(AGENCY_CACHE_FILE)) {
  
  # Load the original nested agency records from the local cache.
  agencies_raw <- readRDS(
    AGENCY_CACHE_FILE
  )
  
  message(
    "Loaded raw agencies from cache: ",
    AGENCY_CACHE_FILE
  )
  
} else {
  
  # Download the original nested agency records from the API.
  agencies_raw <- fetch_agencies()
  
  # Save the nested agency records for future use.
  saveRDS(
    agencies_raw,
    AGENCY_CACHE_FILE
  )
  
  message(
    "Saved raw agencies to cache: ",
    AGENCY_CACHE_FILE
  )
}


# Stop the script if no agency records were returned.
if (length(agencies_raw) == 0) {
  stop("No agency records were returned by the API.")
}


# =============================================================================
# 5. INSPECT THE RAW AGENCY RECORDS
# =============================================================================

# Show the total number of raw agency records retrieved.
length(agencies_raw)


# Show the top-level fields in the first agency record.
names(agencies_raw[[1]])


# Display the complete first nested agency record.
agencies_raw[[1]]


# Inspect the nested structure of the first agency record.
str(
  agencies_raw[[1]],
  max.level = 4
)


# =============================================================================
# 6. CONVERT RAW AGENCY RECORDS INTO A CLEAN AGENCY TABLE
# =============================================================================

# agencies_raw contains the original nested API records.
#
# map_dfr() performs the following steps:
#
# 1. Goes through every agency record in agencies_raw.
# 2. Converts each agency into a one-row tibble.
# 3. Combines the one-row tibbles into one clean agency table.
#
# In the resulting agencies table:
#   One row represents one agency.
agencies <- map_dfr(
  agencies_raw,
  \(x) {
    
    tibble(
      
      # -----------------------------------------------------------------------
      # Agency identification
      # -----------------------------------------------------------------------
      
      # Unique agency identifier.
      agency_id = as.character(
        x$id %||% NA_character_
      ),
      
      
      # Full agency name.
      agency_name = as_single_character(
        x$name
      ),
      
      
      # Agency abbreviation, such as NASA, ESA, or ISRO.
      agency_abbrev = as_single_character(
        x$abbrev
      ),
      
      
      # URL-friendly agency name used by the API.
      agency_slug = as_single_character(
        x$slug
      ),
      
      
      # -----------------------------------------------------------------------
      # Agency classification
      # -----------------------------------------------------------------------
      
      # Safely extract the agency-type ID.
      agency_type_id = as.character(
        purrr::pluck(
          x,
          "type",
          "id",
          .default = NA_character_
        )
      ),
      
      
      # Safely extract the human-readable agency type.
      agency_type = as.character(
        purrr::pluck(
          x,
          "type",
          "name",
          .default = NA_character_
        )
      ),
      
      
      # Whether LL2 designates the agency as featured.
      featured = as.logical(
        x$featured %||% NA
      ),
      
      
      # -----------------------------------------------------------------------
      # Agency background
      # -----------------------------------------------------------------------
      
      # Year the agency was founded.
      founding_year = x$founding_year %||%
        NA_integer_,
      
      
      # Name or title of the agency's administrator.
      administrator = as_single_character(
        x$administrator
      ),
      
      
      # Agency description supplied by LL2.
      description = as_single_character(
        x$description
      ),
      
      
      # Launch vehicles associated with the agency.
      launchers = as_single_character(
        x$launchers
      ),
      
      
      # Spacecraft associated with the agency.
      spacecraft = as_single_character(
        x$spacecraft
      ),
      
      
      # -----------------------------------------------------------------------
      # Parent organization
      # -----------------------------------------------------------------------
      
      # Extract the parent agency name.
      #
      # The API may return parent as either:
      #   A nested list
      #   A character value
      #   NULL
      parent_agency_name = if (
        is.null(x$parent) || length(x$parent) == 0
      ) {
        
        NA_character_
        
      } else if (is.list(x$parent)) {
        
        as.character(
          purrr::pluck(
            x$parent,
            "name",
            .default = NA_character_
          )
        )
        
      } else {
        
        as_single_character(x$parent)
      },
      
      
      # Extract a parent ID only if parent is represented as a nested list.
      parent_agency_id = if (
        is.list(x$parent) &&
        length(x$parent) > 0
      ) {
        
        as.character(
          purrr::pluck(
            x$parent,
            "id",
            .default = NA_character_
          )
        )
        
      } else {
        
        NA_character_
      },
      
      
      # -----------------------------------------------------------------------
      # Image and online information
      # -----------------------------------------------------------------------
      
      # Safely extract the primary agency image URL.
      image_url = as.character(
        purrr::pluck(
          x,
          "image",
          "image_url",
          .default = NA_character_
        )
      ),
      
      
      # Safely extract the agency logo URL.
      logo_url = as.character(
        purrr::pluck(
          x,
          "logo",
          "image_url",
          .default = NA_character_
        )
      ),
      
      
      # Safely extract the social-media logo URL.
      social_logo_url = as.character(
        purrr::pluck(
          x,
          "social_logo",
          "image_url",
          .default = NA_character_
        )
      )
    )
  }
) |>
  
  # ---------------------------------------------------------------------------
# Clean variables and create analysis-ready columns
# ---------------------------------------------------------------------------

mutate(
  
  # Convert founding_year explicitly to an integer.
  #
  # Values that cannot be converted become NA.
  founding_year = suppressWarnings(
    as.integer(founding_year)
  ),
  
  
  # Create a simplified sector variable from agency_type.
  sector = case_when(
    agency_type == "Commercial" ~ "Commercial",
    agency_type == "Government" ~ "Government",
    is.na(agency_type)          ~ NA_character_,
    TRUE                        ~ "Other"
  )
)


# =============================================================================
# 7. INSPECT THE CLEANED AGENCY TABLE
# =============================================================================

# Display the columns, data types, and sample values.
glimpse(agencies)


# Display the first six agency records.
head(agencies)


# Show the number of rows and columns.
dim(agencies)


# Count agencies by agency type.
agencies |>
  count(
    agency_type,
    sort = TRUE,
    .drop = FALSE
  )


# Count agencies by simplified sector.
agencies |>
  count(
    sector,
    sort = TRUE,
    .drop = FALSE
  )


# Count missing values in every agency column.
colSums(
  is.na(agencies)
)


# Check for duplicated nonmissing agency IDs.
#
# Ideally, this should return zero rows.
agencies |>
  filter(!is.na(agency_id)) |>
  count(
    agency_id,
    name = "number_of_records"
  ) |>
  filter(number_of_records > 1)


# =============================================================================
# 8. SAVE THE CLEANED AGENCY TABLE
# =============================================================================

# Save the clean one-row-per-agency table in an R-native format.
saveRDS(
  agencies,
  AGENCY_CLEAN_FILE
)


# Save the clean agency table as a CSV file.
write.csv(
  agencies,
  AGENCY_CSV_FILE,
  row.names = FALSE,
  na = ""
)


message(
  "Saved cleaned agency table as: ",
  AGENCY_CLEAN_FILE
)

message(
  "Saved cleaned agency CSV as: ",
  AGENCY_CSV_FILE
)


# =============================================================================
# 9. JOIN THE LAUNCH AND AGENCY TABLES
# =============================================================================

# The launches object must already exist before this section runs.
if (!exists("launches")) {
  stop(
    paste(
      "The launches object does not exist.",
      "Run or load the launch-data script before joining agencies."
    )
  )
}


# Confirm that the required launch columns exist.
required_launch_columns <- c(
  "lsp_id",
  "lsp_name"
)

missing_launch_columns <- setdiff(
  required_launch_columns,
  names(launches)
)

if (length(missing_launch_columns) > 0) {
  stop(
    paste(
      "The launches table is missing these required columns:",
      paste(missing_launch_columns, collapse = ", ")
    )
  )
}


# Convert the launch service provider ID to character so that it has the same
# data type as agencies$agency_id.
launches <- launches |>
  mutate(
    lsp_id = as.character(lsp_id)
  )


# Table relationship:
#
# launches$lsp_id
#       matches
# agencies$agency_id
#
# This is generally a many-to-one relationship:
#
# Many launches may share the same launch service provider agency.
launches_with_agencies <- launches |>
  left_join(
    agencies,
    by = c("lsp_id" = "agency_id"),
    suffix = c("_launch", "_agency"),
    relationship = "many-to-one"
  )


# Inspect the joined table.
glimpse(launches_with_agencies)


# Determine the name of the sector column after the join.
#
# If launches already contained a sector column, the agency version will be
# named sector_agency. Otherwise, it will remain sector.
agency_sector_column <- if (
  "sector_agency" %in% names(launches_with_agencies)
) {
  "sector_agency"
} else if (
  "sector" %in% names(launches_with_agencies)
) {
  "sector"
} else {
  NA_character_
}


# Create a list of useful columns that may be displayed.
display_columns <- c(
  "launch_id",
  "net",
  "status",
  "lsp_id",
  "lsp_name",
  "agency_name",
  "agency_abbrev",
  "agency_type",
  "founding_year"
)


# Add the appropriate agency sector column if it exists.
if (!is.na(agency_sector_column)) {
  display_columns <- c(
    display_columns,
    agency_sector_column
  )
}


# Display only columns that actually exist in the joined table.
launches_with_agencies |>
  select(
    any_of(display_columns)
  ) |>
  head()


# =============================================================================
# 10. CHECK THE JOIN QUALITY
# =============================================================================

# Find launch service providers that did not match an agency record.
unmatched_agencies <- launches |>
  distinct(
    lsp_id,
    lsp_name
  ) |>
  filter(
    !is.na(lsp_id)
  ) |>
  anti_join(
    agencies,
    by = c("lsp_id" = "agency_id")
  )


# Display unmatched launch service providers.
unmatched_agencies


# Calculate an agency ID match summary.
agency_join_summary <- launches |>
  distinct(
    lsp_id,
    lsp_name
  ) |>
  filter(
    !is.na(lsp_id)
  ) |>
  summarise(
    provider_ids = n(),
    
    matched_ids = sum(
      lsp_id %in% agencies$agency_id
    ),
    
    unmatched_ids = sum(
      !(lsp_id %in% agencies$agency_id)
    ),
    
    match_rate = mean(
      lsp_id %in% agencies$agency_id
    )
  )


# Display the agency join summary.
agency_join_summary


# Display the match rate as a percentage.
agency_join_summary |>
  mutate(
    match_rate_percent = scales::percent(
      match_rate,
      accuracy = 0.1
    )
  )


# =============================================================================
# 11. SAVE THE JOINED TABLE
# =============================================================================

# Save the joined launch-and-agency table in an R-native format.
saveRDS(
  launches_with_agencies,
  "launches_with_agencies.rds"
)


# Save the joined launch-and-agency table as a CSV file.
write.csv(
  launches_with_agencies,
  "launches_with_agencies.csv",
  row.names = FALSE,
  na = ""
)


message(
  "Saved joined table as launches_with_agencies.rds"
)

message(
  "Saved joined CSV as launches_with_agencies.csv"
)