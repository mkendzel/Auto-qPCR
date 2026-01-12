library(shiny)
library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(DT)
library(janitor)
library(shinyjs)

## =========================
## UI
## =========================
ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Auto-qPCR"),
  
  tabsetPanel(
    id = "page",
    
    # ============================================================
    # TAB 1) Import raw qPCR files + header confirmation
    # ============================================================
    tabPanel(
      title = "1) Import",
      value = "import",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "combine_files",
            "Do you have multiple qPCR files that should be combined?",
            choices = c("No (single file)" = FALSE, "Yes (multiple files)" = TRUE),
            selected = FALSE
          ),
          uiOutput("file_upload_ui"),
          tags$hr(),
          checkboxInput(
            "header_ok",
            "I confirm the header row and first data row look correct.",
            FALSE
          ),
          actionButton("continue_btn", "Continue", class = "btn-primary")
        ),
        mainPanel(
          h4("Data Preview"),
          radioButtons(
            "preview_table",
            "Preview table",
            choices = c("Main data (NTC removed)" = "main", "NTC rows only" = "ntc"),
            selected = "main",
            inline = TRUE
          ),
          DTOutput("data_preview"),
          tags$hr(),
          verbatimTextOutput("status_msg")
        )
      )
    ),
    
    # ============================================================
    # TAB 2) Parse Sample Name + define reference structure
    # ============================================================
    tabPanel(
      title = "2) Parse Sample Names",
      value = "parse",
      h4("Sample Name parsing"),
      
      fluidRow(
        column(
          6,
          helpText("Expected format: 1_w_x_y ... (underscore-delimited)."),
          numericInput(
            "expected_parts",
            "How many underscore-delimited parts?",
            value = 4,
            min = 2
          ),
          tags$hr(),
          
          uiOutput("part_labels_ui"),
          
          tags$hr(),
          h4("Reference gene setup"),
          
          radioButtons(
            "multi_gapdh",
            "Does an individual contribute multiple GAPDH measurements? (e.g. mouse 1 has different GAPDH for each tissue)",
            choices = c("No" = "no", "Yes" = "yes"),
            selected = "no",
            inline = TRUE
          ),
          
          uiOutput("gapdh_group_cols_ui"),
          
          tags$hr(),
          uiOutput("ref_gene_ui"),
          
          tags$hr(),
          h4("Delta-Delta Ct setup (treatment + mock)"),
          
          uiOutput("treatment_col_ui"),
          uiOutput("mock_value_ui"),
          uiOutput("ddct_id_col_ui"),
          
          tags$hr(),
          checkboxInput(
            "parse_ok",
            "I confirm the parsed columns and reference setup look correct.",
            FALSE
          ),
          actionButton("continue_to_tab3", "Continue", class = "btn-primary")
        ),
        column(
          6,
          h5("Parsed preview (updates live)"),
          DTOutput("sample_parse_preview")
        )
      )
    ),
    
    # ============================================================
    # TAB 3) Review + save long format table
    # ============================================================
    tabPanel(
      title = "3) Review",
      value = "review_match",
      
      h4("Reference configuration summary"),
      verbatimTextOutput("review_match_status"),
      
      tags$hr(),
      h4("Long format preview"),
      DTOutput("ctref_preview"),
      
      tags$hr(),
      h4("Save output (long format)"),
      
      textInput(
        "out_title",
        "Output table title (used for filename)",
        value = ""
      ),
      
      checkboxInput(
        "save_ok",
        "I confirm this table is correct and should be saved as a CSV.",
        value = FALSE
      ),
      
      actionButton("save_csv", "Save CSV", class = "btn-primary"),
      
      tags$hr(),
      verbatimTextOutput("save_status")
    ),
    
    # ============================================================
    # TAB 4) Prism export setup
    # ============================================================
    tabPanel(
      title = "4) Prism Export",
      value = "prism",
      
      h4("Prism export setup"),
      verbatimTextOutput("prism_detect_status"),
      tags$hr(),
      
      uiOutput("prism_split_ui"),
      uiOutput("prism_primary_ui"),
      uiOutput("prism_secondary_ui"),
      
      tags$hr(),
      uiOutput("prism_rowvar_ui"),
      uiOutput("prism_valuevar_ui"),
      uiOutput("prism_repid_ui"),
      
      tags$hr(),
      h4("Prism-formatted preview"),
      DTOutput("prism_preview"),
      
      tags$hr(),
      h4("Save output (Prism format)"),
      textInput(
        "prism_title",
        "Output table title (used for filename prefix)",
        value = ""
      ),
      checkboxInput(
        "prism_ok",
        "I confirm this Prism export configuration is correct and should be saved.",
        value = FALSE
      ),
      actionButton("save_prism", "Save Prism CSV(s)", class = "btn-primary"),
      tags$hr(),
      verbatimTextOutput("prism_save_status")
    )
  )
)

## =========================
## SERVER
## =========================
server <- function(input, output, session) {
  
  r3 <- function(x) round(x, 3)
  
  # ------------------------------------------------------------
  # Global tab locks (only unlock when prerequisites met)
  # ------------------------------------------------------------
  observe({
    shinyjs::disable(selector = 'a[data-value="parse"]')
    shinyjs::disable(selector = 'a[data-value="review_match"]')
    shinyjs::disable(selector = 'a[data-value="prism"]')
  })
  
  # ------------------------------------------------------------
  # TAB 1) File upload + preprocessing
  # ------------------------------------------------------------
  output$file_upload_ui <- renderUI({
    if (identical(input$combine_files, "TRUE")) {
      fileInput("raw_files", "Upload qPCR .xls files", multiple = TRUE, accept = ".xls")
    } else {
      fileInput("raw_files", "Upload a qPCR .xls file", multiple = FALSE, accept = ".xls")
    }
  })
  
  qpcr <- reactive({
    req(input$raw_files)
    preprocess_qpcr_files(
      files = input$raw_files,
      combine_multiple = identical(input$combine_files, "TRUE")
    )
  })
  
  output$data_preview <- renderDT({
    req(qpcr())
    df <- if (input$preview_table == "ntc") qpcr()$ntc else qpcr()$main
    DT::datatable(df, rownames = FALSE, options = list(scrollX = TRUE))
  })
  
  approved_data <- reactiveVal(NULL)
  parsed_data   <- reactiveVal(NULL)
  
  observeEvent(input$continue_btn, {
    validate(
      need(!is.null(input$raw_files), "Upload file(s) first."),
      need(isTRUE(input$header_ok), "Confirm the header row.")
    )
    
    approved_data(qpcr()$main)
    shinyjs::enable(selector = 'a[data-value="parse"]')
    updateTabsetPanel(session, "page", selected = "parse")
  })
  
  # ------------------------------------------------------------
  # TAB 2) Parsing logic
  # ------------------------------------------------------------
  output$part_labels_ui <- renderUI({
    req(input$expected_parts)
    k <- input$expected_parts
    
    tagList(
      lapply(seq_len(k), function(i) {
        textInput(
          paste0("part_label_", i),
          paste("Part", i, "label"),
          value = paste0("part", i)
        )
      })
    )
  })
  
  parsed_part_cols <- reactive({
    req(input$expected_parts)
    vapply(seq_len(input$expected_parts), function(i) {
      lbl <- input[[paste0("part_label_", i)]]
      if (!is.null(lbl) && nzchar(lbl)) lbl else paste0("part", i)
    }, character(1))
  })
  
  split_df_live <- reactive({
    req(approved_data(), parsed_part_cols(), input$expected_parts)
    
    split_sample_name(
      df = approved_data(),
      n_parts = input$expected_parts,
      part_labels = parsed_part_cols(),
      keep_sample_name = FALSE
    )
  })
  
  split_df_typed <- reactive({
    req(split_df_live(), parsed_part_cols())
    
    df <- split_df_live()
    
    # All parsed parts -> factors (could change if need be later)
    for (col in parsed_part_cols()) {
      df[[col]] <- as.factor(df[[col]])
    }
    
    # CT numeric + rounding on ingest
    df$CT <- r3(as.numeric(df$CT))
    
    # Target Name factor
    df$`Target Name` <- as.factor(df$`Target Name`)
    
    df
  })
  
  output$sample_parse_preview <- renderDT({
    req(split_df_typed())
    DT::datatable(split_df_typed(), rownames = FALSE, options = list(scrollX = TRUE))
  })
  
  # ------------------------------------------------------------
  # ddCt setup (treatment + mock + unique id)
  # ------------------------------------------------------------
  output$treatment_col_ui <- renderUI({
    req(parsed_part_cols())
    cols <- parsed_part_cols()
    
    selectInput(
      "treatment_col",
      "Which parsed column stores the treatment?",
      choices  = c("— Select a column —" = "", stats::setNames(cols, cols)),
      selected = "",
      multiple = FALSE,
      selectize = TRUE
    )
  })
  
  # ------------------------------------------------------------
  # GAPDH grouping
  # ------------------------------------------------------------
  output$gapdh_group_cols_ui <- renderUI({
    req(parsed_part_cols())
    cols <- parsed_part_cols()
    
    if (input$multi_gapdh == "yes") {
      selectInput(
        inputId   = "gapdh_group_cols",
        label     = "Select the columns that define a unique GAPDH measurement",
        choices   = cols,
        selected  = character(0),
        multiple  = TRUE,
        selectize = TRUE
      )
    } else {
      selectInput(
        inputId   = "gapdh_group_cols",
        label     = "Sample ID column for GAPDH",
        choices   = c("— Select a column —" = "", stats::setNames(cols, cols)),
        selected  = "",
        multiple  = FALSE,
        selectize = TRUE
      )
    }
  })
  
  # ------------------------------------------------------------
  # Reference gene selector
  # ------------------------------------------------------------
  output$ref_gene_ui <- renderUI({
    req(split_df_typed())
    genes <- sort(unique(as.character(split_df_typed()[["Target Name"]])))
    
    selectInput(
      "ref_gene",
      "Select reference (normalization) gene",
      choices  = c("— Select a gene —" = "", stats::setNames(genes, genes)),
      selected = "",
      multiple = FALSE,
      selectize = TRUE
    )
  })
  
  observeEvent(split_df_typed(), {
    updateSelectInput(session, "ref_gene", selected = "")
  }, ignoreInit = TRUE)
  
  output$mock_value_ui <- renderUI({
    req(split_df_typed())
    req(!is.null(input$treatment_col))
    
    if (!nzchar(input$treatment_col)) return(NULL)
    
    vals <- sort(unique(as.character(split_df_typed()[[input$treatment_col]])))
    
    selectInput(
      "mock_value",
      "Which value represents the Mock control?",
      choices  = c("— Select Mock —" = "", stats::setNames(vals, vals)),
      selected = "",
      multiple = FALSE,
      selectize = TRUE
    )
  })
  
  output$ddct_id_col_ui <- renderUI({
    req(parsed_part_cols())
    cols <- parsed_part_cols()
    
    selectInput(
      "ddct_id_col",
      "Which parsed column is the unique Sample ID?",
      choices  = c("— Select a column —" = "", stats::setNames(cols, cols)),
      selected = "",
      multiple = FALSE,
      selectize = TRUE
    )
  })
  
  observeEvent(input$treatment_col, {
    updateSelectInput(session, "mock_value", selected = "")
  }, ignoreInit = TRUE)
  
  observeEvent(split_df_typed(), {
    updateSelectInput(session, "treatment_col", selected = "")
    updateSelectInput(session, "mock_value", selected = "")
    updateSelectInput(session, "ddct_id_col", selected = "")
  }, ignoreInit = TRUE)
  
  # ------------------------------------------------------------
  # Disable Tab 2 continue until prerequisites met
  # ------------------------------------------------------------
  observe({ shinyjs::disable("continue_to_tab3") })
  
  observe({
    cols_ok <- !is.null(input$gapdh_group_cols) &&
      length(input$gapdh_group_cols) >= 1 &&
      !any(input$gapdh_group_cols == "")
    
    ref_ok   <- !is.null(input$ref_gene) && nzchar(input$ref_gene)
    treat_ok <- !is.null(input$treatment_col) && nzchar(input$treatment_col)
    mock_ok  <- !is.null(input$mock_value) && nzchar(input$mock_value)
    id_ok    <- !is.null(input$ddct_id_col) && nzchar(input$ddct_id_col)
    
    if (cols_ok && ref_ok && treat_ok && mock_ok && id_ok) shinyjs::enable("continue_to_tab3") else shinyjs::disable("continue_to_tab3")
  })
  
  # ------------------------------------------------------------
  # Freeze parsed data + proceed
  # ------------------------------------------------------------
  observeEvent(input$continue_to_tab3, {
    validate(
      need(isTRUE(input$parse_ok), "Confirm before continuing."),
      need(
        !is.null(input$gapdh_group_cols) &&
          length(input$gapdh_group_cols) >= 1 &&
          !any(input$gapdh_group_cols == ""),
        "Select sample grouping column(s)."
      ),
      need(
        input$multi_gapdh == "no" || length(input$gapdh_group_cols) > 1,
        "Multiple GAPDH requires more than one grouping column."
      ),
      need(!is.null(input$ref_gene) && nzchar(input$ref_gene), "Select a reference gene."),
      need(!is.null(input$treatment_col) && nzchar(input$treatment_col), "Select the treatment column."),
      need(!is.null(input$mock_value) && nzchar(input$mock_value), "Select which value is Mock."),
      need(!is.null(input$ddct_id_col) && nzchar(input$ddct_id_col), "Select the unique Sample ID column."),
      need(input$ddct_id_col != input$treatment_col, "Unique Sample ID column cannot be the same as the treatment column.")
    )
    
    parsed_data(split_df_typed())
    shinyjs::enable(selector = 'a[data-value="review_match"]')
    shinyjs::enable(selector = 'a[data-value="prism"]')
    updateTabsetPanel(session, "page", selected = "review_match")
  })
  
  # ------------------------------------------------------------
  # Create ct_ref + ddCt columns
  # ------------------------------------------------------------
  data_with_ct_ref <- eventReactive(input$continue_to_tab3, {
    req(parsed_data(), input$ref_gene, input$gapdh_group_cols, input$treatment_col, input$mock_value, input$ddct_id_col)
    
    df <- parsed_data()
    
    baseline_group_cols <- setdiff(parsed_part_cols(), c(input$treatment_col, input$ddct_id_col))
    
    ref_df <- df %>%
      dplyr::filter(`Target Name` == input$ref_gene) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(input$gapdh_group_cols))) %>%
      dplyr::summarise(ct_ref = r3(mean(CT, na.rm = TRUE)), .groups = "drop")
    
    out <- df %>%
      dplyr::left_join(ref_df, by = input$gapdh_group_cols) %>%
      dplyr::filter(`Target Name` != input$ref_gene) %>%
      dplyr::mutate(
        dCt = r3(CT - ct_ref),
        .is_mock = as.character(.data[[input$treatment_col]]) == input$mock_value
      )
    
    mock_means <- out %>%
      dplyr::filter(.is_mock) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(baseline_group_cols, "Target Name")))) %>%
      dplyr::summarise(mock_mean_dCt = r3(mean(dCt, na.rm = TRUE)), .groups = "drop")
    
    out %>%
      dplyr::left_join(mock_means, by = c(baseline_group_cols, "Target Name")) %>%
      dplyr::mutate(
        ddCt = r3(dCt - mock_mean_dCt),
        relative_expression = r3(2^(-ddCt))
      ) %>%
      dplyr::select(-.is_mock)
  })
  
  # ------------------------------------------------------------
  # TAB 3) Preview + save
  # ------------------------------------------------------------
  output$ctref_preview <- renderDT({
    req(data_with_ct_ref())
    DT::datatable(
      data_with_ct_ref(),
      rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 10, lengthMenu = c(10, 25, 50, 100))
    )
  })
  
  output$review_match_status <- renderPrint({
    if (is.null(parsed_data())) {
      cat("No parsed data locked yet.\n")
    } else {
      baseline_group_cols <- setdiff(parsed_part_cols(), c(input$treatment_col, input$ddct_id_col))
      
      cat("Reference gene:", input$ref_gene, "\n")
      cat("GAPDH grouping column(s):", paste(input$gapdh_group_cols, collapse = ", "), "\n")
      cat("Treatment column:", input$treatment_col, "\n")
      cat("Mock value:", input$mock_value, "\n")
      cat("Unique Sample ID column:", input$ddct_id_col, "\n")
      cat("Mock mean grouping columns:", paste(baseline_group_cols, collapse = ", "), "\n")
    }
  })
  
  save_status_val <- reactiveVal("")
  output$save_status <- renderText({ save_status_val() })
  
  observeEvent(input$save_csv, {
    req(data_with_ct_ref())
    req(isTRUE(input$save_ok))
    req(nzchar(input$out_title))
    
    out_dir <- file.path("data", "output", "long")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    safe_name <- gsub("[^A-Za-z0-9_-]+", "_", input$out_title)
    out_path <- file.path(out_dir, paste0(safe_name, ".csv"))
    
    write.csv(data_with_ct_ref(), out_path, row.names = FALSE)
    
    showNotification(
      paste("File saved:", out_path),
      type = "message",
      duration = 6
    )
    
    save_status_val(
      paste(
        "Saved successfully:", out_path,
        "\nRows:", nrow(data_with_ct_ref()),
        "\nTime:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
    )
  })
  
  output$status_msg <- renderPrint({
    if (is.null(input$raw_files)) "Upload file(s) to begin."
    else if (!isTRUE(input$header_ok)) "Confirm the header row."
    else "Ready."
  })
  
  # ============================================================
  # TAB 4) Prism export logic
  # ============================================================
  
  prism_n_parsed <- reactive({
    req(parsed_part_cols())
    length(parsed_part_cols())
  })
  
  prism_group_candidates <- reactive({
    req(parsed_part_cols(), input$ddct_id_col)
    setdiff(parsed_part_cols(), input$ddct_id_col)
  })
  
  output$prism_detect_status <- renderPrint({
    req(parsed_part_cols(), prism_group_candidates(), prism_n_parsed())
    all_parsed <- parsed_part_cols()
    g <- prism_group_candidates()
    n_parsed <- prism_n_parsed()
    
    if (n_parsed >= 3) {
      cat(
        "Detected at least 3 parsed columns:\n",
        paste("-", all_parsed, collapse = "\n"),
        "\n\nSeparate graphs suggested. Which variable do you want to split the graphs by?"
      )
    } else {
      cat(
        "Detected", n_parsed, "parsed column(s):\n",
        paste("-", all_parsed, collapse = "\n")
      )
    }
    
    cat(
      "\n\nEligible grouping/splitting columns (replicate ID excluded):\n",
      paste("-", g, collapse = "\n")
    )
  })
  
  output$prism_split_ui <- renderUI({
    req(prism_group_candidates(), prism_n_parsed())
    g <- prism_group_candidates()
    n_parsed <- prism_n_parsed()
    
    if (n_parsed >= 3) {
      selectInput(
        "prism_split_var",
        "Which variable do you want to split the graphs by?",
        choices = c("— Select a column —" = "", stats::setNames(g, g)),
        selected = ""
      )
    } else {
      NULL
    }
  })
  
  prism_remaining_candidates <- reactive({
    req(prism_group_candidates(), prism_n_parsed())
    g <- prism_group_candidates()
    n_parsed <- prism_n_parsed()
    
    if (n_parsed >= 3) {
      req(!is.null(input$prism_split_var))
      if (!nzchar(input$prism_split_var)) return(character(0))
      setdiff(g, input$prism_split_var)
    } else {
      g
    }
  })
  
  output$prism_primary_ui <- renderUI({
    req(prism_remaining_candidates())
    g <- prism_remaining_candidates()
    
    selectInput(
      "prism_primary_group",
      "Column grouping variable (Prism columns)",
      choices = c("— Select a column —" = "", stats::setNames(g, g)),
      selected = ""
    )
  })
  
  output$prism_secondary_ui <- renderUI({
    req(prism_remaining_candidates(), input$prism_primary_group)
    g <- prism_remaining_candidates()
    g2 <- setdiff(g, input$prism_primary_group)
    
    if (!nzchar(input$prism_primary_group) || length(g2) == 0) return(NULL)
    
    selectInput(
      "prism_secondary_group",
      "Secondary column grouping variable (optional)",
      choices = c("— None —" = "", stats::setNames(g2, g2)),
      selected = ""
    )
  })
  
  output$prism_rowvar_ui <- renderUI({
    req(data_with_ct_ref())
    df <- data_with_ct_ref()
    
    choices <- names(df)
    default <- if ("Target Name" %in% choices) "Target Name" else choices[1]
    
    selectInput(
      "prism_row_var",
      "What should each row represent? (typically Target Name)",
      choices = stats::setNames(choices, choices),
      selected = default
    )
  })
  
  output$prism_valuevar_ui <- renderUI({
    req(data_with_ct_ref())
    df <- data_with_ct_ref()
    
    numeric_candidates <- intersect(
      c("CT", "ct_ref", "dCt", "ddCt", "relative_expression"),
      names(df)
    )
    
    default <- if ("relative_expression" %in% numeric_candidates) "relative_expression" else numeric_candidates[1]
    
    selectInput(
      "prism_value_var",
      "Which value should Prism plot?",
      choices = stats::setNames(numeric_candidates, numeric_candidates),
      selected = default
    )
  })
  
  output$prism_repid_ui <- renderUI({
    req(data_with_ct_ref(), input$ddct_id_col)
    df <- data_with_ct_ref()
    
    choices <- names(df)
    default <- if (input$ddct_id_col %in% choices) input$ddct_id_col else choices[1]
    
    selectInput(
      "prism_replicate_id",
      "Replicate ID column",
      choices = stats::setNames(choices, choices),
      selected = default
    )
  })
  
  prism_make_one_table <- function(df) {
    req(input$prism_primary_group, input$prism_row_var, input$prism_value_var, input$prism_replicate_id)
    req(nzchar(input$prism_primary_group))
    
    col_primary <- input$prism_primary_group
    col_secondary <- if (!is.null(input$prism_secondary_group) && nzchar(input$prism_secondary_group)) input$prism_secondary_group else NULL
    
    row_var   <- input$prism_row_var
    value_var <- input$prism_value_var
    rep_id    <- input$prism_replicate_id
    
    # Column-group label
    if (is.null(col_secondary)) {
      df2 <- df %>% dplyr::mutate(.col_group = as.character(.data[[col_primary]]))
    } else {
      df2 <- df %>% dplyr::mutate(.col_group = paste(as.character(.data[[col_primary]]),
                                                     as.character(.data[[col_secondary]]),
                                                     sep = " | "))
    }
    
    # One value per (row_var, col_group, replicate_id)
    df3 <- df2 %>%
      dplyr::select(dplyr::all_of(row_var), .col_group, dplyr::all_of(rep_id), .value = dplyr::all_of(value_var)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(row_var, ".col_group", rep_id)))) %>%
      dplyr::summarise(.value = r3(mean(.value, na.rm = TRUE)), .groups = "drop")
    
    # Replicate index within each (row_var, col_group) -> Y1..Yn
    df4 <- df3 %>%
      dplyr::arrange(.data[[row_var]], .col_group, .data[[rep_id]]) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(row_var, ".col_group")))) %>%
      dplyr::mutate(.rep_n = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        .rep_label = as.character(.data[[rep_id]]),
        .colname   = paste0(.col_group, ": ", .rep_label)
      ) %>%
      dplyr::select(-.rep_label)
    
    wide <- df4 %>%
      dplyr::select(dplyr::all_of(row_var), .colname, .value) %>%
      tidyr::pivot_wider(
        id_cols = dplyr::all_of(row_var),
        names_from = .colname,
        values_from = .value
      ) %>%
      dplyr::arrange(.data[[row_var]])
    
    wide
  }
  
  prism_tables <- reactive({
    req(data_with_ct_ref(), prism_n_parsed(), prism_group_candidates())
    df <- data_with_ct_ref()
    n_parsed <- prism_n_parsed()
    
    if (n_parsed >= 3) {
      req(!is.null(input$prism_split_var))
      if (!nzchar(input$prism_split_var)) return(list())
      
      split_var <- input$prism_split_var
      split_levels <- sort(unique(as.character(df[[split_var]])))
      
      out <- lapply(split_levels, function(lvl) {
        df_sub <- df %>% dplyr::filter(as.character(.data[[split_var]]) == lvl)
        prism_make_one_table(df_sub)
      })
      names(out) <- split_levels
      out
    } else {
      list("ALL" = prism_make_one_table(df))
    }
  })
  
  output$prism_preview <- renderDT({
    tabs <- prism_tables()
    if (length(tabs) == 0) {
      return(DT::datatable(data.frame(), rownames = FALSE, options = list(dom = "t")))
    }
    nm <- names(tabs)[1]
    DT::datatable(
      tabs[[nm]],
      rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 10, lengthMenu = c(10, 25, 50, 100))
    )
  })
  
  prism_save_status_val <- reactiveVal("")
  output$prism_save_status <- renderText({ prism_save_status_val() })
  
  observeEvent(input$save_prism, {
    tabs <- prism_tables()
    req(length(tabs) >= 1)
    req(isTRUE(input$prism_ok))
    req(nzchar(input$prism_title))
    
    out_dir <- file.path("data", "output", "prism")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    safe_prefix <- gsub("[^A-Za-z0-9_-]+", "_", input$prism_title)
    
    paths <- character(0)
    for (nm in names(tabs)) {
      tag <- if (nm == "ALL") "" else paste0("_", gsub("[^A-Za-z0-9_-]+", "_", nm))
      out_path <- file.path(out_dir, paste0(safe_prefix, tag, "_prism.csv"))
      write.csv(tabs[[nm]], out_path, row.names = FALSE)
      paths <- c(paths, out_path)
    }
    
    showNotification(
      paste("Prism file(s) saved to:", out_dir),
      type = "message",
      duration = 6
    )
    
    prism_save_status_val(
      paste(
        "Saved Prism export successfully.\n",
        "Files:\n- ", paste(paths, collapse = "\n- "),
        "\n\nTime: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        sep = ""
      )
    )
  })
}

shinyApp(ui, server)
