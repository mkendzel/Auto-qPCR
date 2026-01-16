# ============================================================
# R/logging.R
# ============================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

init_logger <- function(exp_dir, exp_name_safe) {
  meta_dir <- file.path(exp_dir, "meta_data")
  dir.create(meta_dir, recursive = TRUE, showWarnings = FALSE)
  
  log_dir <- file.path(exp_dir, "logs")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  
  log_path <- file.path(log_dir, "run_log.jsonl")
  
  write_jsonl <- function(rec) {
    txt <- jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null", pretty = TRUE)
    cat(txt, "\n\n", file = log_path, append = TRUE)
  }
  
  stamp_id <- function() format(Sys.time(), "%Y%m%d_%H%M%S")
  
  save_table_xlsx <- function(df, prefix) {
    if (is.null(df)) return(NA_character_)
    p <- file.path(meta_dir, paste0(prefix, "_", stamp_id(), ".xlsx"))
    openxlsx::write.xlsx(df, p, rowNames = FALSE)
    p
  }
  
  snapshot_run <- function(
    trigger,
    input_list,
    file_names = character(0),
    qc_table = NULL,
    qc_calc_table = NULL,
    outputs = list(),
    session_pkgs = list(),
    tab = NA_character_
  ) {
    qc_xlsx      <- save_table_xlsx(qc_table, "qc_table_failures")
    qc_calc_xlsx <- save_table_xlsx(qc_calc_table, "qc_table_calcs")
    
    rec <- list(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      trigger = trigger,
      tab = tab,
      experiment_name_safe = exp_name_safe,
      files = list(raw_files = file_names),
      inputs = input_list,
      tables = list(
        qc_table_xlsx = qc_xlsx,
        qc_calc_table_xlsx = qc_calc_xlsx
      ),
      outputs = outputs,
      packages = session_pkgs
    )
    
    write_jsonl(rec)
    invisible(rec)
  }
  
  append_event <- function(tab, action, details = list()) {
    rec <- list(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      trigger = "event",
      tab = tab,
      experiment_name_safe = exp_name_safe,
      action = action,
      details = details
    )
    write_jsonl(rec)
    invisible(rec)
  }
  
  list(
    meta_dir = meta_dir,
    log_dir = log_dir,
    log_path = log_path,
    snapshot_run = snapshot_run,
    append_event = append_event
  )
}
