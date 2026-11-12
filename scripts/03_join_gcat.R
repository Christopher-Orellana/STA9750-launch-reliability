# 03_join_gcat.R
# Purpose: match LL2 launches to GCAT (J. McDowell) in 3 steps:
#          1) exact designator, 2) same UTC day with a single candidate,
#          3) manual review (31 launches, each with a note).
# Input:   master_launches_2000_2025.csv (built by 02_build_master_tables.R)
#          GCAT launch.tsv (downloaded here if missing)
# Run:     after scripts 01 and 02, from the open RStudio project
# Output:  master_launches_2000_2025_gcat.csv (one row per LL2 launch)
#          gcat_lookup.csv (launch_id -> GCAT Launch_Tag)
#          ll2_corrections.csv (LL2 rows changed or dropped, with reasons)
# Note:    manual matches were checked against the GCAT snapshot of
#          2026-10-24. A fresh download may differ slightly; the checks
#          below stop the script if a manual tag no longer exists.
# Data credit: GCAT, J. McDowell, planet4589.org/space/gcat (CC-BY)

library(tidyverse)
library(here)
here::i_am("scripts/03_join_gcat.R")

# ---- Paths ----
# Same folder where 02_build_master_tables.R saves the master file
out_dir     <- here("data", "processed", "combined_launch_tables")
ll2_file    <- here(out_dir, "master_launches_2000_2025.csv")
ll2_manifest <- here(out_dir, "master_manifest.csv")
gcat_dir    <- here("data", "raw", "gcat")
gcat_file   <- here(gcat_dir, "gcat_launch.tsv")
out_joined  <- here(out_dir, "master_launches_2000_2025_gcat.csv")
out_lookup  <- here(out_dir, "gcat_lookup.csv")
out_fixes   <- here(out_dir, "ll2_corrections.csv")

dir.create(gcat_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir,  recursive = TRUE, showWarnings = FALSE)

# ---- LL2 input and corrections ----
# Same safety rule as script 02: only build from a complete, unchanged master.
# master_manifest.csv is written by 02 only when it finishes.
if (!file.exists(ll2_manifest)) {
  stop("No master manifest. Run scripts/02_build_master_tables.R first.")
}
master_manifest <- read_csv(ll2_manifest, show_col_types = FALSE)
expected_md5 <- master_manifest$md5[master_manifest$file == "master_launches_2000_2025.csv"]
if (length(expected_md5) != 1 || unname(tools::md5sum(ll2_file)) != expected_md5) {
  stop("master_launches_2000_2025.csv is missing or changed since script 02. Rerun script 02.")
}

launches_raw <- read_csv(ll2_file, show_col_types = FALSE)

# Corrections log: every LL2 row we change or drop, with the reason
ll2_corrections <- tribble(
  ~launch_id,                             ~action,   ~reason,
  "b364f80f-2044-4d6d-aefa-c16c58bd7774", "dropped", "Duplicate of Pegasus XL / ST-5 (2006). Kept 9f603738..., which has the real date (2006-03-22) and designator 2006-008, confirmed with GCAT",
  NA_character_,                          "tag excluded from exact join", "H3 / Michibiki 5 (Dec 2025) carries designator 2025-308, which belongs to a Long March 12A in GCAT"
)

launches <- launches_raw |>
  filter(!launch_id %in% ll2_corrections$launch_id[ll2_corrections$action == "dropped"]) |>
  mutate(
    launch_date   = as_date(net),   # not ymd_hms: keeps midnight launches
    is_suborbital = orbit_name %in% "Suborbital" |
      mission_type %in% c("Suborbital", "Tourism"),
    is_earth      = launch_body_name %in% "Earth"
  )

stopifnot(nrow(launches) == nrow(launches_raw) - 1)

# ---- GCAT download ----
# Downloaded only if the file is missing
if (!file.exists(gcat_file)) {
  download.file("https://planet4589.org/space/gcat/tsv/launch/launch.tsv",
                gcat_file, mode = "wb")
}

# The header line starts with "#"; read it separately, then skip comment lines
first_lines <- readLines(gcat_file, n = 5)
gcat_header <- first_lines[1] |> str_remove("^#") |> str_split_1("\t")
n_skip      <- sum(str_starts(first_lines, "#"))

gcat <- read_tsv(gcat_file, col_names = gcat_header, skip = n_skip,
                 col_types = cols(.default = "c"), quote = "")

# ---- GCAT clean ----
gcat_clean <- gcat |>
  mutate(
    gcat_date    = ymd(str_extract(Launch_Date, "^\\d{4} \\w{3} +\\d{1,2}")),
    # LaunchCode: 1st letter = type of flight (O orbital, S suborbital, D deep space...)
    #             2nd letter = outcome (S success, F failure)
    # Inferred from the data and GCAT's success page; to verify in lcols.html
    gcat_outcome = case_when(
      str_sub(LaunchCode, 2, 2) == "S" ~ "success",
      str_sub(LaunchCode, 2, 2) == "F" ~ "failure"
    ),
    gcat_is_failure_tag = str_detect(Launch_Tag, "-F")
  ) |>
  select(Launch_Tag, gcat_date, LV_Type, LaunchCode, gcat_outcome,
         FailCode, gcat_service = Group, gcat_is_failure_tag)

# ---- Join step 1: exact designator ----
match_step1 <- launches |>
  filter(!is.na(launch_designator)) |>
  # LL2 error: the H3 / Michibiki 5 failure carries 2025-308,
  # which in GCAT is a Long March 12A launch (see ll2_corrections)
  filter(!(launch_designator == "2025-308" &
             str_detect(rocket_name_reported, "H3"))) |>
  inner_join(gcat_clean, by = c("launch_designator" = "Launch_Tag")) |>
  select(launch_id, Launch_Tag = launch_designator)

# ---- Join step 2: same UTC day, single candidate ----
unmatched <- launches |> anti_join(match_step1, by = "launch_id")

candidates <- unmatched |>
  inner_join(gcat_clean |> filter(!Launch_Tag %in% match_step1$Launch_Tag),
             by = c("launch_date" = "gcat_date"),
             relationship = "many-to-many") |>
  add_count(launch_id, name = "n_candidates")

# Wrong rocket despite a single candidate that day:
# MOMO v0 -> Hwasong-11Ga, Starship Prototype -> Topol'-E
wrong_matches <- c("2019-M02", "2019-S35")

match_step2 <- candidates |>
  filter(n_candidates == 1, !Launch_Tag %in% wrong_matches) |>
  select(launch_id, Launch_Tag)

# ---- Manual candidates (exploration, already done) ----
# Used once to build match_manual below: orbital launches still unmatched,
# crossed with GCAT launches within 3 days. Not needed for the output.
run_manual_review <- FALSE

if (run_manual_review) {
  to_check <- launches |>
    filter(!is_suborbital) |>
    anti_join(bind_rows(match_step1, match_step2), by = "launch_id")

  manual_candidates <- to_check |>
    select(launch_id, launch_date, launch_name, rocket_name_reported, outcome) |>
    cross_join(
      gcat_clean |>
        filter(!Launch_Tag %in% c(match_step1$Launch_Tag, match_step2$Launch_Tag)) |>
        select(Launch_Tag, gcat_date, LV_Type, LaunchCode)
    ) |>
    filter(abs(as.numeric(gcat_date - launch_date)) <= 3) |>
    arrange(launch_date, gcat_date)

  write_csv(manual_candidates, here(out_dir, "gcat_manual_candidates.csv"))
}

# ---- Join step 3: manual matches ----
# Reviewed by hand from gcat_manual_candidates.csv.
# Rule: same rocket (LL2 name vs GCAT LV_Type), same or nearby date, same outcome.
match_manual <- tribble(
  ~launch_id,                             ~Launch_Tag, ~match_note,
  "5195b5e3-2e8f-46d2-91d2-1d187faa2419", "2000-F02",  "Zenit-3SL, ICO F1 failure",
  "5c5da2d3-7e9e-4dc1-a4d7-a524531d40e6", "2000-F03",  "Tsiklon-3 failure",
  "9ddcb285-f111-4f90-8e76-d7a703c40848", "2001-U01",  "Taurus 2110 = Minotaur-C 2110, OrbView-4",
  "5ec966fd-6feb-4f07-894c-17c8ccb852ab", "2002-F02",  "Soyuz-U-PVB, Foton-M1",
  "05cc2e97-f0d5-45a4-bfd6-f60ae8fe4729", "2002-F03",  "Ariane 5 ECA maiden flight",
  "b79acbdb-9ef2-4783-ab17-8874811a979b", "2005-F04",  "Kaituozhe-1 = KT-1",
  "eed27674-7719-42f3-9c00-65cd0ee96afd", "2005-F01",  "Molniya-M = Molniya 8K78M (same day as Volna)",
  "f49d890e-09a1-4627-91fd-ac4884a5bd51", "2005-F02",  "Volna, Cosmos-1 (same day as Molniya)",
  "9a50023f-07fe-45ac-9ac6-7fb6b5ea970d", "2007-F02",  "Falcon 1 DemoSat",
  "3f3c6a26-33e9-4bfc-8606-a7f3bce71246", "2010-F03",  "Proton-M/DM-3, Glonass-M",
  "abeb23a1-ee77-433a-abd5-7da5e338f9d6", "2011-F04",  "Soyuz-2-1B, Meridian 5",
  "d8f30f66-6e15-4f27-ac52-c5d532b9addd", "2012-F02",  "Safir, May 2012",
  "04073b47-8f05-404c-9435-6762138b99ca", "2012-F03",  "Safir, Sep 2012",
  "2e6be816-fd8a-44e3-8ace-b78658db9ba2", "2013-F02",  "Proton-M/DM-3, Uragan-M",
  "f802155a-6816-4ed5-9c1c-a4ebc43df820", "2016-F01",  "Long March 4C = Chang Zheng 4C, Gaofen 10",
  "decf3c30-a1af-46dd-a016-559af24e259b", "2017-F01",  "SS-520-4 = SS-520",
  "c1d2ea04-0884-4cff-82f6-a7a408e6b5ab", "2017-S112", "Simorgh; GCAT classes it as a suborbital test (TF)",
  "f7dce879-b3ff-40dd-aa23-0c1162ab203d", "2017-F05",  "Soyuz-2-1B, Meteor-M 2-1",
  "62267793-5458-4202-aefa-cea4cf12e113", "2018-F01",  "Soyuz-FG, Soyuz MS-10 crew abort",
  "4a35280f-2665-4d3a-89cc-8d520c72edc0", "2018-F02",  "Zhuque-1",
  "9e4da066-b31e-490d-9ade-fe52cf0bd114", "2019-F01",  "Simorgh, Payam",
  "8e87b56f-70e3-4874-a09f-a0586d11280a", "2019-F03",  "OS-M1",
  "38ed8ff1-0335-4e68-9dd4-925c76ad01ed", "2020-F07",  "Astra Rocket 3.1 (same day as Kuaizhou)",
  "6f3ece0b-7a3c-4e75-94cb-5a0e6bc71b71", "2020-F08",  "Kuaizhou-1A (same day as Astra)",
  "d9486816-bb61-4b16-bcfa-0d1e9ba40696", "2020-U01",  "Vega, SEOSAT-Ingenio",
  "397c50c5-8a2b-4ca5-807d-8911c14b7126", "2021-F06",  "GSLV Mk II, EOS-03",
  "1f79ce8e-dabc-4287-bb59-2a40f567c4e4", "2021-F09",  "Nuri first flight",
  "25d652f7-7e20-42d5-967e-b8e01305746c", "2023-F04",  "Terran 1",
  "bc6530e6-f53e-4621-8336-3838403737d9", "2025-F07",  "Zhuque-2E",
  "a81fcb21-ed57-47ef-ab57-33ddd9c6ffcd", "2025-F09",  "HANBIT-Nano (same day as Long March 12A)",
  "c7ae20f6-347c-4039-a933-0ed9b28ad4b9", "2025-308",  "Long March 12A; LL2 had this tag on the H3 row by mistake"
  # Team decision pending: Amos-6 exploded on the pad before launch.
  # GCAT lists it as 2016-E01 (code OE, not counted as a launch attempt).
  # "3622669f-6e06-467a-86f3-47b56b1114c1", "2016-E01", "Falcon 9 Amos-6 pad explosion"
)

# Checks: each manual launch and tag used once; every tag exists in GCAT
stopifnot(
  !anyDuplicated(match_manual$launch_id),
  !anyDuplicated(match_manual$Launch_Tag),
  all(match_manual$Launch_Tag %in% gcat_clean$Launch_Tag)
)

# ---- Final join ----
gcat_lookup <- bind_rows(
  match_step1,
  match_step2,
  match_manual |> select(launch_id, Launch_Tag)
)

launches_gcat <- launches |>
  left_join(gcat_lookup, by = "launch_id") |>
  left_join(gcat_clean,  by = "Launch_Tag")

# Checks: all LL2 rows kept, one row per launch, each GCAT tag used once
stopifnot(
  nrow(launches_gcat) == nrow(launches),
  !anyDuplicated(launches_gcat$launch_id),
  !anyDuplicated(na.omit(launches_gcat$Launch_Tag))
)

# ---- Summary (printed when the script is sourced) ----
message("Matches: step 1 = ", nrow(match_step1),
        ", step 2 = ", nrow(match_step2),
        ", manual = ", nrow(match_manual),
        ", total = ", nrow(gcat_lookup))

# Match rate, orbital vs suborbital
print(launches_gcat |>
        summarise(n = n(), matched = mean(!is.na(Launch_Tag)), .by = is_suborbital))

# Outcome agreement, LL2 vs GCAT
print(launches_gcat |> count(outcome, gcat_outcome))

# Sector vs GCAT service code (first part of the code; a different definition)
print(launches_gcat |>
        mutate(gcat_service_type = str_extract(gcat_service, "^[A-Z]+")) |>
        count(sector, gcat_service_type))

# ---- Export ----
write_csv(launches_gcat,   out_joined)
write_csv(gcat_lookup,     out_lookup)
write_csv(ll2_corrections, out_fixes)

message("Saved: ", out_joined)
