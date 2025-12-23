preprocess_qpcr_files <- function(files, combine_multiple = FALSE) {
  # files: input$raw_files (data.frame with columns name, datapath, etc.)
  # combine_multiple: TRUE/FALSE (logical)
  
  dfs <- lapply(seq_len(nrow(files)), function(i) {
    path <- files$datapath[i]
    nm   <- files$name[i]
    
    raw <- readxl::read_excel(
      path,
      sheet = "Results",
      col_names = FALSE
    )
    
    header_row <- which(raw[[1]] == "Well")[1]
    
    df <- raw |>
      dplyr::slice(header_row:dplyr::n()) |>
      as.data.frame()
    
    colnames(df) <- as.character(df[1, ])
    df <- df |>
      dplyr::slice(-1)
    
    df$source_file <- nm
    df
  })
  
  names(dfs) <- files$name
  
  combined <- if (isTRUE(combine_multiple)) dplyr::bind_rows(dfs) else dfs[[1]]
  
  ntc <- combined |>
    dplyr::filter(tolower(trimws(`Sample Name`)) == "ntc")
  
  main <- combined |>
    dplyr::filter(tolower(trimws(`Sample Name`)) != "ntc") |>
    dplyr::mutate(
      CT = dplyr::case_when(
        tolower(trimws(as.character(CT))) == "undetermined" ~ 40,
        TRUE ~ suppressWarnings(as.numeric(CT))
      )
    )
  
  list(
    per_file  = dfs,
    combined  = combined,
    main      = main,
    ntc       = ntc
  )
}