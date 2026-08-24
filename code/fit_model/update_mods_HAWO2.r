library(tidyverse)
library(glue)
library(jagsUI)
library(MCMCvis)

COEF_TABLE_PATH <- "code/fit_model/mod_key.csv"

# --- Tuning parameters ---
BATCH_ITERS     <- 2000    # iterations per batch (small = checkpoint often, survive OOM)
MAX_BATCHES     <- 500     # 500 * 2000 = 1,000,000 max total iterations
RHAT_THRESHOLD  <- 1.1
N_THIN          <- 5       # thin to reduce memory footprint
CHECKPOINT_DIR  <- "data/model_res/checkpoints"

dir.create(CHECKPOINT_DIR, showWarnings = FALSE, recursive = TRUE)

coef_update <- read_csv(COEF_TABLE_PATH) %>%
  filter(run == "yes") %>%
  filter(step == 3) %>%
  mutate(AOU_Code = substr(result, 1, 4)) %>%
  filter(AOU_Code %in% c("HAWO"))

cat("Species to process:", nrow(coef_update), "\n")

for (ii in 1:nrow(coef_update)) {

  sp_name   <- coef_update$result[ii]
  out_path  <- glue("data/model_res/{sp_name}_updated.rds")
  ckpt_path <- glue("{CHECKPOINT_DIR}/{sp_name}_checkpoint.rds")

  cat(paste("\n=== Processing", ii, "of", nrow(coef_update), ":", sp_name, "===\n"))
  invisible(gc()); invisible(gc())

  # Skip if already fully converged and saved
  if (file.exists(out_path)) {
    cat("Final output already exists, skipping.\n")
    next
  }

  tryCatch({

    # --- Resume from checkpoint if available, else load original ---
    if (file.exists(ckpt_path)) {
      cat("Resuming from checkpoint:", ckpt_path, "\n")
      current_model <- readRDS(ckpt_path)
    } else {
      cat("Loading original model from disk...\n")
      current_model <- readRDS(glue("data/model_res/{sp_name}.rds"))
    }

    # Report current memory use right after loading
    mem_mb <- sum(gc()[, 2]) * 8 / 1024
    cat(glue("  Memory after load: ~{round(mem_mb)} MB\n"))

    converged <- FALSE

    for (batch in 1:MAX_BATCHES) {

      # Check convergence BEFORE running more iterations
      rhat_vals   <- MCMCsummary(current_model, round = 3)$Rhat
      max_rhat    <- max(rhat_vals, na.rm = TRUE)
      n_high_rhat <- sum(rhat_vals > RHAT_THRESHOLD, na.rm = TRUE)
      cat(glue("  Batch {batch} | Max Rhat: {round(max_rhat, 4)} | Params > {RHAT_THRESHOLD}: {n_high_rhat}\n"))

      if (max_rhat <= RHAT_THRESHOLD) {
        cat("  Converged! Saving final model.\n")
        saveRDS(current_model, file = out_path)
        if (file.exists(ckpt_path)) file.remove(ckpt_path)
        converged <- TRUE
        break
      }

      # Run one small batch of additional iterations
      cat(glue("  Running {BATCH_ITERS} more iterations (thin = {N_THIN})...\n"))
      current_model <- update(current_model, n.iter = BATCH_ITERS, n.thin = N_THIN)

      # Save checkpoint immediately after each batch
      saveRDS(current_model, file = ckpt_path)
      cat(glue("  Checkpoint saved: {ckpt_path}\n"))

      # Save a lightweight CSV summary snapshot every 10 batches
      if (batch %% 10 == 0) {
        snap_summary <- MCMCsummary(current_model, round = 3)
        snap_csv <- glue("{CHECKPOINT_DIR}/{sp_name}_batch{batch}_summary.csv")
        write_csv(snap_summary %>% tibble::rownames_to_column("param"), snap_csv)
        prev_csv <- glue("{CHECKPOINT_DIR}/{sp_name}_batch{batch - 10}_summary.csv")
        if (file.exists(prev_csv)) file.remove(prev_csv)
        cat(glue("  Summary snapshot: {snap_csv}\n"))
      }

      # Aggressive memory cleanup between batches
      rm(rhat_vals)
      invisible(gc()); invisible(gc())
    }

    if (!converged) {
      cat(glue(
        "  WARNING: {sp_name} did not converge after {MAX_BATCHES} batches ",
        "({MAX_BATCHES * BATCH_ITERS} total iterations).\n"
      ))
      cat("  Checkpoint retained for next job submission.\n")
    }

  }, error = function(e) {
    cat(paste("  ERROR processing", sp_name, ":", conditionMessage(e), "\n"))
    cat("  Checkpoint (if saved) will be used on next run.\n")
  })

  rm(current_model)
  invisible(gc()); invisible(gc())
  cat("---\n")
}

cat("\nDone. Check", CHECKPOINT_DIR, "for any remaining checkpoints.\n")