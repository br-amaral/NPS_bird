#!/usr/bin/env Rscript

library(tidyverse)
library(readr)
library(stringr)
library(glue)

out_dir <- "data/out"

# 1. Discover expected number of rows from mod_key.csv -------------------------

master_tab <- read_csv("code/fit_model/mod_key.csv") %>%
  filter(step %in% c(2, 3, 4)) %>%      # same filters you used for super_test.r
  distinct() %>%
  arrange(AOU_Code, step) %>%
  filter(AOU_Code != "BCCH")

expected_n <- nrow(master_tab)
cat("Expected number of rows (master_tab):", expected_n, "\n")

# 2. List the per-row files ----------------------------------------------------

files <- list.files(
  out_dir,
  pattern = "^super_test_table_row[0-9]+_.*\\.rds$",
  full.names = TRUE
)

cat("Found", length(files), "per-row files.\n")

if (length(files) == 0) {
  stop("No per-row files found in data/out/. Did the array job finish?")
}

# Optional hard check: length must match expected_n
if (length(files) != expected_n) {
  warning(glue("Expected {expected_n} files, but found {length(files)}. Checking which indices are missing..."))
}

# 3. Extract row indices from the filenames and check for gaps ------------------

row_idx <- str_match(basename(files), "^super_test_table_row([0-9]+)_")[, 2] %>%
  as.integer()

expected_idx <- 1:expected_n

missing_idx <- setdiff(expected_idx, row_idx)
extra_idx   <- setdiff(row_idx, expected_idx)

if (length(missing_idx) > 0) {
  stop(glue("Missing per-row files for indices: {paste(missing_idx, collapse = ', ')}"))
}

if (length(extra_idx) > 0) {
  warning(glue("Found per-row files for unexpected indices: {paste(extra_idx, collapse = ', ')}"))
}

cat("All expected indices present. Proceeding to combine.\n")

# 4. Combine everything --------------------------------------------------------

coef_fim <- files |>
  map(read_rds) |>
  list_rbind()

final_file <- file.path(out_dir, "super_test_table_all.rds")
write_rds(coef_fim, final_file)

cat("Combined table written to:", final_file, "\n")
cat("Total rows in combined table:", nrow(coef_fim), "\n")
