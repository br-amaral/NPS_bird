# super_test.r -- per-row version for Slurm array
# Each run handles ONE row of master_tab, chosen by a command-line index.

freshr::freshr()

# Load packages -------------------------------------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(MCMCvis)
library(rjags)
library(BayesPostEst)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

`%!in%` <- Negate(`%in%`)

# ------------------------------------------------------------------------------
# 1. Detect environment (local vs HPCC) and read master_tab
# ------------------------------------------------------------------------------

if (substr(getwd(), 1, 3) == "/Us") {
  direc <- "local"
} else {
  direc <- "hpc"
}

if (direc == "local") {
  master_tab <- read_csv(
    "/Users/bamaral/Library/CloudStorage/OneDrive-MichiganStateUniversity/GitHubOne/NPS_bird_copy/code/fit_model/mod_key.csv"
  ) %>%
    filter(step %in% c(2, 3, 4)) %>%
    distinct() %>%
    arrange(AOU_Code, step)
} else {
  master_tab <- read_csv("code/fit_model/mod_key.csv") %>%
    filter(step %in% c(2, 3, 4)) %>%
    distinct() %>%
    arrange(AOU_Code, step)
}

# Remove BCCH as in your original code
master_tab <- master_tab %>% filter(AOU_Code != "BCCH")

# ------------------------------------------------------------------------------
# 2. Get row index from command line (for Slurm array)
# ------------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("You must supply a row index (1..nrow(master_tab)) as the first argument.")
}

row_index <- suppressWarnings(as.integer(args[1]))
if (is.na(row_index)) {
  stop("First argument must be an integer row index.")
}

if (row_index < 1 || row_index > nrow(master_tab)) {
  stop(glue("Row index {row_index} is outside valid range 1..{nrow(master_tab)}"))
}

cat("\n\n========================================\n")
cat(glue("  super_test.r running row {row_index} of {nrow(master_tab)}\n"))
cat("========================================\n\n")

# ------------------------------------------------------------------------------
# 3. Function to compute coefficients for ONE row
# ------------------------------------------------------------------------------

coef_tab <- function(row_index) {

  sps_loop <- master_tab[row_index, ]

  spslp    <- sps_loop$AOU_Code
  steplp   <- sps_loop$step
  resultlp <- sps_loop$result
  scalelp  <- sps_loop$select

  cat(glue("Processing species = {spslp}, step = {steplp}\n"))
  cat(glue("  result file: {resultlp}.rds\n"))
  cat(glue("  scale file : {scalelp}.rds\n\n"))

  # Safety check that filenames are consistent with species code
  if (substr(resultlp, 1, 4) != spslp ||
      substr(scalelp,  1, 4) != spslp) {
    stop(glue("Filename/species mismatch on row {row_index}: {spslp}, {resultlp}, {scalelp}"))
  }

  # Read model results -----------------------------------------------------------
  res_mod <- read_rds(glue("data/model_res/{resultlp}.rds"))

  # Parameter names (as in your original)
  scales_names <- grep("^scales_", colnames(res_mod[[1]]), value = TRUE)
  all_params   <- c("mu.alpha0", "mu.beta0", "beta", "alpha", scales_names)

  coef_tablp <- MCMCsummary(
    res_mod,
    params = all_params,
    probs  = c(0.1, 0.5, 0.9),  # 80% credible intervals
    round  = 2
  )

  coef_tablp1 <- coef_tablp %>%
    rownames_to_column("coef") %>%
    mutate(
      sps         = spslp,
      step        = steplp,
      result_file = resultlp,
      select_file = scalelp,
      overlap_zero = case_when(
        `10%` > 0 & `90%` > 0 ~ "no",
        `10%` < 0 & `90%` < 0 ~ "no",
        `10%` <= 0 & `90%` >= 0 ~ "yes",
        `90%` <= 0 & `10%` >= 0 ~ "yes",
        TRUE ~ "unknown"
      ),
      effect_direction = case_when(
        `10%` > 0 & `90%` > 0 ~ "positive",
        `10%` < 0 & `90%` < 0 ~ "negative",
        `10%` <= 0 & `90%` >= 0 ~ "non_significant",
        TRUE ~ "unclear"
      ),
      is_significant_80 = overlap_zero == "no"
    )

  # Only for step 2 models: attach scale-selection results ----------------------
  if (str_detect(resultlp, "step2")) {

    sca_mod <- read_rds(glue("data/model_res/{scalelp}.rds"))

    sca_mod1 <- sca_mod %>%
      mutate(coef = as.character(glue("{substr(betas, 1, 4)}[{substr(betas, 5, 5)}]"))) %>%
      select(coef, sca_sel, sca1, sca2, sca3)

    coef_tablp2 <- coef_tablp1 %>%
      relocate(
        sps, step,
        coef, mean, sd, `10%`, `50%`, `90%`, Rhat, n.eff,
        overlap_zero, effect_direction, is_significant_80
      ) %>%
      as_tibble() %>%
      left_join(sca_mod1, by = "coef")

    } else {coef_tablp2 <- coef_tablp1}
    
    return(coef_tablp2)
}

# ------------------------------------------------------------------------------
# 4. Run for this single row and write a per-row result file
# ------------------------------------------------------------------------------

coef_fim_row <- coef_tab(row_index)

# Make sure the output directory exists
out_dir <- "data/out"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

spslp  <- master_tab$AOU_Code[row_index]
steplp <- master_tab$step[row_index]

out_file <- glue("{out_dir}/super_test_table_row{row_index}_{spslp}_step{steplp}.rds")

cat(glue("Writing per-row table to: {out_file}\n"))
readr::write_rds(coef_fim_row, out_file)

cat("\nDone.\n")
