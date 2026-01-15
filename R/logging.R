library(jsonlite)

`%||%` <- function(a, b) if (!is.null(a)) a else b

sanitize_experiment_name <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[^A-Za-z0-9_-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^[_-]+|[_-]+$", "", x)
  if (!nzchar(x)) return("")
  tolower(x)
}

ensure_experiment_dirs <- function(base_dir) {
  dir.create(file.path(base_dir, "raw"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(base_dir, "processed"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(base_dir, "qc"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(base_dir, "meta_data"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(base_dir, "exports"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(base_dir, "logs"), recursive = TRUE, showWarnings = FALSE)
  invisible(TRUE)
}

qc_thresholds <- function() {
  list(
    ref_gene_ct_range = list(min = 18, max = 22),
    mock_dCt_sd_warn = 0.5,
    mock_dCt_sd_fail = 1.0,
    mock_rel_expr_warn_abs_diff = 1,
    mock_rel_expr_fail_abs_diff = 4
  )
}

init_logger <- function(exp_dir, exp_name_safe) {
  run_id_val <- NULL
  
  ensure_run_id <- function() {
    if (!is.null(run_id_val) && nzchar(run_id_val)) return(run_id_val)
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    run_id_val <<- paste0(exp_name_safe, "__", ts)
    run_id_val
  }
  
  events_log_path <- function() file.path(exp_dir, "logs", "events.csv")
  
  append_event <- function(tab, action, details = list()) {
    rid <- ensure_run_id()
    
    evt <- data.frame(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      run_id = rid,
      tab = as.character(tab %||% ""),
      action = as.character(action),
      details_json = jsonlite::toJSON(details, auto_unbox = TRUE, null = "null"),
      stringsAsFactors = FALSE
    )
    
    p <- events_log_path()
    if (!file.exists(p)) {
      write.csv(evt, p, row.names = FALSE)
    } else {
      write.table(evt, p, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE, quote = TRUE)
    }
    
    invisible(TRUE)
  }
  
  snapshot_run <- function(trigger, input_list, file_names, qc_table = NULL, outputs = list(), session_pkgs = list()) {
    rid <- ensure_run_id()
    
    meta_dir <- file.path(exp_dir, "meta_data")
    dir.create(meta_dir, recursive = TRUE, showWarnings = FALSE)
    
    snap <- list(
      snapshot_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      experiment_dir = exp_dir,
      experiment_name_safe = exp_name_safe,
      run_id = rid,
      trigger = trigger,
      inputs = input_list,
      file_names = file_names,
      qc_thresholds = qc_thresholds(),
      outputs = outputs,
      session = list(
        r_version = R.version.string,
        packages = session_pkgs
      )
    )
    
    meta_path <- file.path(meta_dir, paste0(rid, "__snapshot.json"))
    jsonlite::write_json(snap, meta_path, pretty = TRUE, auto_unbox = TRUE, null = "null")
    
    if (!is.null(qc_table)) {
      qc_path <- file.path(meta_dir, paste0(rid, "__qc_table.csv"))
      write.csv(qc_table, qc_path, row.names = FALSE)
    }
    
    invisible(meta_path)
  }
  
  list(
    ensure_run_id = ensure_run_id,
    append_event = append_event,
    snapshot_run = snapshot_run
  )
}
