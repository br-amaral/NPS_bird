library(tidyverse)
library(glue)
library(jagsUI)
library(MCMCvis)

COEF_TABLE_PATH <- "code/fit_model/mod_key.csv"

# --- Tuning parameters ---
BATCH_ITERS     <- 2000   # iterations per update batch
MAX_BATCHES     <- 500      # max batches per species before giving up (= 100k total)
RHAT_THRESHOLD  <- 1.1
CHECKPOINT_DIR  <- "data/model_res/checkpoints"

dir.create(CHECKPOINT_DIR, showWarnings = FALSE, recursive = TRUE)

coef_update <- read_csv(COEF_TABLE_PATH) %>%
  filter(run == "yes") %>%
  filter(step == 3) %>%
  mutate(AOU_Code = substr(result, 1, 4)) %>%
  filter(AOU_Code %in% c("HAWO"))

cat("Total rows in mod_key.csv:", nrow(coef_update), "\n")

for (ii in 1:nrow(coef_update)) {

  sp_name    <- coef_update$result[ii]
  out_path   <- glue("data/model_res/{sp_name}_updated.rds")
  ckpt_path  <- glue("{CHECKPOINT_DIR}/{sp_name}_checkpoint.rds")

  cat(paste("\n=== Processing", ii, "of", nrow(coef_update), ":", sp_name, "===\n"))
  gc()

  # Skip if already fully converged
  if (file.exists(out_path)) {
    cat("Final output already exists, skipping.\n")
    next
  }

  tryCatch({

    # --- Resume from checkpoint if available, else load original ---
    if (file.exists(ckpt_path)) {
      cat("Resuming from checkpoint:", ckpt_path, "\n")
      current_model <- read_rds(ckpt_path)
    } else {
      cat("Loading original model...\n")
      current_model <- read_rds(glue("data/model_res/{sp_name}.rds"))
    }

    converged <- FALSE

    for (batch in 1:MAX_BATCHES) {

      # Check convergence before running more iterations
      rhat_summary <- MCMCsummary(current_model, round = 3)
      max_rhat     <- max(rhat_summary$Rhat, na.rm = TRUE)
      cat(glue("  Batch {batch} | Max Rhat: {round(max_rhat, 4)}\n"))

      if (max_rhat <= RHAT_THRESHOLD) {
        cat("  Converged! Saving final model.\n")
        write_rds(current_model, file = out_path)
        # Clean up checkpoint now that we're done
        if (file.exists(ckpt_path)) file.remove(ckpt_path)
        converged <- TRUE
        break
      }

      # Run one batch of additional iterations
      cat(glue("  Running {BATCH_ITERS} more iterations...\n"))
      current_model <- update(current_model, n.iter = BATCH_ITERS, n.thin = 5)

      # Save checkpoint after every batch
      write_rds(current_model, file = ckpt_path)
      cat(glue("  Checkpoint saved: {ckpt_path}\n"))

      rm(rhat_summary)
      gc()
    }

    if (!converged) {
      cat(glue("  WARNING: {sp_name} did not converge after {MAX_BATCHES} batches ({MAX_BATCHES * BATCH_ITERS} total iterations).\n"))
      cat("  Checkpoint retained for manual inspection.\n")
    }

  }, error = function(e) {
    cat(paste("  ERROR processing", sp_name, ":", e$message, "\n"))
  })

  rm(current_model)
  gc()
  cat("---\n")
}

