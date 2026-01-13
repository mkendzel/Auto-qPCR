# ============================================================
# Auto-qPCR (Shiny)
# ============================================================

# ============================================================
# Dependencies
# ============================================================
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
    # TAB 1) Import
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
    # TAB 2) Parse Sample Names
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
    # TAB 3) QC
    # ============================================================
    tabPanel(
      title = "3) QC",
      value = "qc",
      h4("QC summary"),
      verbatimTextOutput("qc_status"),
      tags$hr(),
      h4("QC failures"),
      DTOutput("qc_fail_preview"),
      tags$hr(),
      h4("Save for editing"),
      textInput(
        "qc_title",
        "Edit file name (saved to data/edit/ as .xlsx)",
        value = ""
      ),
      checkboxInput(
        "qc_save_ok",
        "I confirm I want to save a copy for editing (Tab 1 dataset + QC index).",
        value = FALSE
      ),
      actionButton("save_qc_edit", "Save edit XLSX", class = "btn-primary"),
      tags$hr(),
      verbatimTextOutput("qc_save_status"),
      tags$hr(),
      h4("Continue"),
      checkboxInput(
        "qc_override_ok",
        "Continue to analysis anyway (not recommended if QC failures are present).",
        value = FALSE
      ),
      actionButton("continue_to_review", "Continue", class = "btn-primary")
    ),
    
    # ============================================================
    # TAB 4) Long format Export
    # ============================================================
    tabPanel(
      title = "4) Long format Export",
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
    # TAB 5) Prism Export
    # ============================================================
    tabPanel(
      title = "5) Prism Export",
      value = "prism",
      h4("Prism export setup"),
      radioButtons(
        "prism_table_type",
        "Which Prism table format do you want?",
        choices = c(
          "Grouped (Prism Grouped table)" = "grouped",
          "Column (Prism Column table: one column per group)" = "column"
        ),
        selected = character(0),
        inline = TRUE
      ),
      tags$hr(),
      verbatimTextOutput("prism_detect_status"),
      tags$hr(),
      conditionalPanel(
        condition = "input.prism_table_type && input.prism_table_type.length > 0",
        uiOutput("prism_split_ui"),
        tags$hr(),
        uiOutput("prism_primary_ui"),
        uiOutput("prism_secondary_ui"),
        tags$hr(),
        uiOutput("prism_rowvar_ui"),
        uiOutput("prism_column_note_ui"),
        conditionalPanel(
          condition = "input.prism_table_type == 'column'",
          radioButtons(
            "prism_column_layout",
            "Column table layout",
            choices = c(
              "Compact (one row per replicate slot)" = "compact",
              "Expanded (replicate-labeled rows)"   = "expanded"
            ),
            selected = "compact",
            inline = TRUE
          )
        ),
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
)

## =========================
## SERVER
## =========================
server <- function(input, output, session) {
  
  r3 <- function(x) round(x, 3)
  
  approved_data <- reactiveVal(NULL)
  approved_data_original <- reactiveVal(NULL)
  parsed_data <- reactiveVal(NULL)
  
  detected_parts <- reactive({
    req(approved_data())
    df <- approved_data()
    req("Sample Name" %in% names(df))
    x <- as.character(df[["Sample Name"]])
    x <- trimws(x)
    x <- x[nzchar(x)]
    parts_n <- vapply(strsplit(x, "_", fixed = TRUE), length, integer(1))
    tab <- table(parts_n)
    top <- as.integer(names(tab)[tab == max(tab)])
    if (length(top) == 0) return(4L)
    max(top)
  })
  
  observeEvent(approved_data(), {
    updateNumericInput(session, "expected_parts", value = detected_parts())
  }, ignoreInit = TRUE)
  
  observe({
    shinyjs::disable(selector = 'a[data-value="parse"]')
    shinyjs::disable(selector = 'a[data-value="qc"]')
    shinyjs::disable(selector = 'a[data-value="review_match"]')
    shinyjs::disable(selector = 'a[data-value="prism"]')
  })
  
  output$file_upload_ui <- renderUI({
    if (identical(input$combine_files, "TRUE")) {
      fileInput(
        "raw_files",
        "Upload qPCR file(s) (.xls or .xlsx)",
        multiple = TRUE,
        accept = c(".xls", ".xlsx")
      )
    } else {
      fileInput(
        "raw_files",
        "Upload a qPCR file (.xls or .xlsx)",
        multiple = FALSE,
        accept = c(".xls", ".xlsx")
      )
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
  
  observeEvent(input$continue_btn, {
    validate(
      need(!is.null(input$raw_files), "Upload file(s) first."),
      need(isTRUE(input$header_ok), "Confirm the header row.")
    )
    
    approved_data_original(qpcr()$main)
    
    df0 <- qpcr()$main
    df0 <- df0 %>% dplyr::mutate(.import_row = dplyr::row_number())
    approved_data(df0)
    
    shinyjs::enable(selector = 'a[data-value="parse"]')
    updateTabsetPanel(session, "page", selected = "parse")
  })
  
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
    for (col in parsed_part_cols()) df[[col]] <- as.factor(df[[col]])
    df$CT <- r3(as.numeric(df$CT))
    df$`Target Name` <- as.factor(df$`Target Name`)
    df
  })
  
  output$sample_parse_preview <- renderDT({
    req(split_df_typed())
    DT::datatable(split_df_typed(), rownames = FALSE, options = list(scrollX = TRUE))
  })
  
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
  
  observe({ shinyjs::disable("continue_to_tab3") })
  
  observe({
    cols_ok <- !is.null(input$gapdh_group_cols) &&
      length(input$gapdh_group_cols) >= 1 &&
      !any(input$gapdh_group_cols == "")
    
    ref_ok   <- !is.null(input$ref_gene) && nzchar(input$ref_gene)
    treat_ok <- !is.null(input$treatment_col) && nzchar(input$treatment_col)
    mock_ok  <- !is.null(input$mock_value) && nzchar(input$mock_value)
    id_ok    <- !is.null(input$ddct_id_col) && nzchar(input$ddct_id_col)
    
    if (cols_ok && ref_ok && treat_ok && mock_ok && id_ok) {
      shinyjs::enable("continue_to_tab3")
    } else {
      shinyjs::disable("continue_to_tab3")
    }
  })
  
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
    shinyjs::enable(selector = 'a[data-value="qc"]')
    updateTabsetPanel(session, "page", selected = "qc")
  })
  
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
  

  qc_results <- reactive({
    req(parsed_data(), data_with_ct_ref())
    req(input$ref_gene, input$treatment_col, input$mock_value, input$ddct_id_col)
    
    df_all    <- parsed_data()
    df_long   <- data_with_ct_ref()
    id_col    <- input$ddct_id_col
    trt_col   <- input$treatment_col
    meta_cols <- parsed_part_cols()
    
    meta_extra <- setdiff(meta_cols, c(id_col, trt_col))
    
    meta_map <- df_all %>%
      dplyr::transmute(
        sample_id = as.character(.data[[id_col]]),
        dplyr::across(dplyr::all_of(meta_extra), ~ as.character(.x))
      ) %>%
      dplyr::filter(!is.na(sample_id) & nzchar(sample_id)) %>%
      dplyr::group_by(sample_id) %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(meta_extra), ~ dplyr::first(.x)),
        .groups = "drop"
      )
    
    # ------------------------------------------------------------
    # QC1) Reference gene CT outside 18–22 (YELLOW if <4 away; RED if >=4 away)
    # ------------------------------------------------------------
    qc1 <- df_all %>%
      dplyr::filter(as.character(.data[["Target Name"]]) == input$ref_gene) %>%
      dplyr::mutate(
        observed = r3(as.numeric(CT)),
        how_off = dplyr::case_when(
          is.na(observed) ~ NA_real_,
          observed < 18   ~ 18 - observed,
          observed > 22   ~ observed - 22,
          TRUE            ~ 0
        ),
        severity = dplyr::case_when(
          is.na(how_off) ~ NA_character_,
          how_off >= 4   ~ "RED",
          how_off > 0    ~ "YELLOW",
          TRUE           ~ NA_character_
        )
      ) %>%
      dplyr::filter(!is.na(severity)) %>%
      dplyr::transmute(
        sample_id = as.character(.data[[id_col]]),
        treatment = as.character(.data[[trt_col]]),
        target    = as.character(input$ref_gene),
        flag      = "Ref CT out of range",
        observed  = observed,
        expected  = "18–22",
        how_off   = r3(how_off),
        severity  = severity
      ) %>%
      dplyr::distinct()
    
    df_long2 <- df_long %>%
      dplyr::mutate(.is_mock = as.character(.data[[trt_col]]) == input$mock_value)
    
    # ------------------------------------------------------------
    # QC2) Mock dCt SD (YELLOW 0.5–1.0, RED >1.0)
    # ------------------------------------------------------------
    qc2 <- df_long2 %>%
      dplyr::filter(.is_mock) %>%
      dplyr::group_by(
        sample_id = as.character(.data[[id_col]]),
        treatment = as.character(.data[[trt_col]]),
        target    = as.character(.data[["Target Name"]])
      ) %>%
      dplyr::summarise(
        observed = r3(stats::sd(dCt, na.rm = TRUE)),
        .groups  = "drop"
      ) %>%
      dplyr::mutate(
        how_off = dplyr::case_when(
          is.na(observed) ~ NA_real_,
          observed >= 0.5 ~ observed - 0.5,
          TRUE            ~ 0
        ),
        severity = dplyr::case_when(
          is.na(observed) ~ NA_character_,
          observed > 1.0  ~ "RED",
          observed >= 0.5 ~ "YELLOW",
          TRUE            ~ NA_character_
        )
      ) %>%
      dplyr::filter(!is.na(severity)) %>%
      dplyr::transmute(
        sample_id = sample_id,
        treatment = treatment,
        target    = target,
        flag      = "Mock dCt SD high",
        observed  = observed,
        expected  = "≤0.5 ok; >1 fail",
        how_off   = r3(how_off),
        severity  = severity
      ) %>%
      dplyr::distinct()
    
    # ------------------------------------------------------------
    # QC3) Mock relative_expression distance from 1
    # ------------------------------------------------------------
    qc3 <- df_long2 %>%
      dplyr::filter(.is_mock) %>%
      dplyr::mutate(
        observed = r3(as.numeric(relative_expression)),
        how_off  = dplyr::if_else(is.na(observed), NA_real_, abs(observed - 1)),
        severity = dplyr::case_when(
          is.na(how_off) ~ NA_character_,
          how_off >= 4   ~ "RED",
          how_off > 1    ~ "YELLOW",
          TRUE           ~ NA_character_
        )
      ) %>%
      dplyr::filter(!is.na(severity)) %>%
      dplyr::transmute(
        sample_id = as.character(.data[[id_col]]),
        treatment = as.character(.data[[trt_col]]),
        target    = as.character(.data[["Target Name"]]),
        flag      = "Mock rel expr far from 1",
        observed  = observed,
        expected  = "1±1 warn; 1±4 fail",
        how_off   = r3(how_off),
        severity  = severity
      ) %>%
      dplyr::distinct()
    
    qc_table <- dplyr::bind_rows(qc1, qc2, qc3) %>%
      dplyr::filter(!is.na(sample_id) & nzchar(sample_id)) %>%
      dplyr::left_join(meta_map, by = "sample_id") %>%
      dplyr::mutate(
        severity_rank = dplyr::case_when(
          severity == "RED"    ~ 2L,
          severity == "YELLOW" ~ 1L,
          TRUE                 ~ 0L
        )
      ) %>%
      dplyr::arrange(dplyr::desc(severity_rank), treatment, sample_id, flag, target) %>%
      dplyr::select(
        sample_id,
        treatment,
        dplyr::all_of(meta_extra),
        target, flag, observed, expected, how_off, severity
      )
    
    list(qc_table = qc_table)
  })
  

  
  
  # ---- Output the QC results
  output$qc_status <- renderPrint({
    req(qc_results())
    qt <- qc_results()$qc_table
    cat("Total QC flags:", nrow(qt), "\n")
    cat("RED:", sum(qt$severity == "RED"), "\n")
    cat("YELLOW:", sum(qt$severity == "YELLOW"), "\n")
  })
  
  output$qc_fail_preview <- renderDT({
    req(qc_results())
    DT::datatable(
      qc_results()$qc_table,
      rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 10, lengthMenu = c(10, 25, 50, 100))
    )
  })
  
  qc_save_status_val <- reactiveVal("")
  output$qc_save_status <- renderText({ qc_save_status_val() })
  
  observeEvent(input$save_qc_edit, {
    req(qc_results(), approved_data_original())
    req(isTRUE(input$qc_save_ok))
    req(nzchar(input$qc_title))
    
    out_dir <- file.path("data", "edit")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    safe_name <- paste0(gsub("[^A-Za-z0-9_-]+", "_", input$qc_title), "_edited")
    out_path <- file.path(out_dir, paste0(safe_name, ".xlsx"))
    
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Results")
    openxlsx::writeData(wb, "Results", approved_data_original())
    openxlsx::addWorksheet(wb, "qc_index")
    openxlsx::writeData(wb, "qc_index", qc_results()$qc_table)
    openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
    
    showNotification(
      paste("Edit XLSX saved:", out_path),
      type = "message",
      duration = 6
    )
    
    qc_save_status_val(
      paste(
        "Saved successfully:\n- ", out_path, "\n",
        "Time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        sep = ""
      )
    )
  })
  
  observeEvent(input$continue_to_review, {
    req(qc_results())
    n_fail <- nrow(qc_results()$qc_table)
    if (n_fail > 0) {
      validate(need(isTRUE(input$qc_override_ok), "QC failures detected."))
    }
    shinyjs::enable(selector = 'a[data-value="review_match"]')
    shinyjs::enable(selector = 'a[data-value="prism"]')
    updateTabsetPanel(session, "page", selected = "review_match")
  })
  
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
    
    safe_name <- paste0(gsub("[^A-Za-z0-9_-]+", "_", input$out_title), "_long")
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
    
    if (is.null(input$prism_table_type) || !nzchar(input$prism_table_type)) {
      cat("Select a Prism table type (Grouped or Column) to configure export options.\n\n")
      cat(
        "Detected parsed columns:\n",
        paste("-", all_parsed, collapse = "\n"),
        "\n\nEligible grouping/splitting columns (replicate ID excluded):\n",
        paste("-", g, collapse = "\n"),
        sep = ""
      )
      return(invisible(NULL))
    }
    
    cat("Table type:", if (input$prism_table_type == "grouped") "Grouped" else "Column", "\n\n")
    
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
    
    if (input$prism_table_type == "column") {
      cat("\n\nColumn mode behavior:\n- Exports a Prism Column table (one column per group)\n- Saves one file per Target Name")
    }
  })
  
  output$prism_split_ui <- renderUI({
    req(prism_group_candidates(), prism_n_parsed())
    g <- prism_group_candidates()
    
    if (prism_n_parsed() >= 3) {
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
    
    if (prism_n_parsed() >= 3) {
      req(!is.null(input$prism_split_var))
      if (!nzchar(input$prism_split_var)) return(character(0))
      setdiff(g, input$prism_split_var)
    } else {
      g
    }
  })
  
  output$prism_primary_ui <- renderUI({
    req(prism_remaining_candidates(), input$prism_table_type)
    g <- prism_remaining_candidates()
    
    selectInput(
      "prism_primary_group",
      if (input$prism_table_type == "grouped") {
        "Column grouping variable (Prism columns)"
      } else {
        "Group column (each Prism column; e.g., treatment)"
      },
      choices = c("— Select a column —" = "", stats::setNames(g, g)),
      selected = ""
    )
  })
  
  output$prism_secondary_ui <- renderUI({
    req(prism_remaining_candidates(), input$prism_primary_group, input$prism_table_type)
    if (input$prism_table_type != "grouped") return(NULL)
    
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
    req(data_with_ct_ref(), input$prism_table_type)
    if (input$prism_table_type != "grouped") return(NULL)
    
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
  
  output$prism_column_note_ui <- renderUI({
    req(input$prism_table_type)
    if (input$prism_table_type != "column") return(NULL)
    helpText("Column mode exports ONE file per Target Name. Each file is a Prism Column table: columns = groups; rows = replicates; blanks are allowed.")
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
  
  prism_make_grouped_table <- function(df) {
    req(input$prism_primary_group, input$prism_row_var, input$prism_value_var, input$prism_replicate_id)
    req(nzchar(input$prism_primary_group))
    
    col_primary <- input$prism_primary_group
    col_secondary <- if (!is.null(input$prism_secondary_group) && nzchar(input$prism_secondary_group)) input$prism_secondary_group else NULL
    
    row_var   <- input$prism_row_var
    value_var <- input$prism_value_var
    rep_id    <- input$prism_replicate_id
    
    if (is.null(col_secondary)) {
      df2 <- df %>% dplyr::mutate(.col_group = as.character(.data[[col_primary]]))
    } else {
      df2 <- df %>% dplyr::mutate(.col_group = paste(
        as.character(.data[[col_primary]]),
        as.character(.data[[col_secondary]]),
        sep = " | "
      ))
    }
    
    df3 <- df2 %>%
      dplyr::select(dplyr::all_of(row_var), .col_group, dplyr::all_of(rep_id), .value = dplyr::all_of(value_var)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(row_var, ".col_group", rep_id)))) %>%
      dplyr::summarise(.value = r3(mean(.value, na.rm = TRUE)), .groups = "drop")
    
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
    
    df4 %>%
      dplyr::select(dplyr::all_of(row_var), .colname, .value) %>%
      tidyr::pivot_wider(
        id_cols = dplyr::all_of(row_var),
        names_from = .colname,
        values_from = .value
      ) %>%
      dplyr::arrange(.data[[row_var]])
  }
  
  prism_make_column_table_one_target <- function(df, target_name) {
    req(input$prism_primary_group, input$prism_value_var, input$prism_replicate_id)
    req(nzchar(input$prism_primary_group))
    
    group_var <- input$prism_primary_group
    value_var <- input$prism_value_var
    rep_id    <- input$prism_replicate_id
    
    df2 <- df %>%
      dplyr::filter(as.character(.data[["Target Name"]]) == target_name) %>%
      dplyr::transmute(
        Group = as.character(.data[[group_var]]),
        ID    = as.character(.data[[rep_id]]),
        Value = as.numeric(.data[[value_var]])
      )
    
    df2 <- df2 %>%
      dplyr::group_by(Group, ID) %>%
      dplyr::summarise(Value = r3(mean(Value, na.rm = TRUE)), .groups = "drop") %>%
      dplyr::arrange(Group, ID)
    
    df2 %>%
      dplyr::group_by(Group) %>%
      dplyr::arrange(ID, .by_group = TRUE) %>%
      dplyr::mutate(.row = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::select(.row, Group, Value) %>%
      tidyr::pivot_wider(
        id_cols = .row,
        names_from = Group,
        values_from = Value
      ) %>%
      dplyr::arrange(.row) %>%
      dplyr::select(-.row)
  }
  
  prism_make_column_table_one_target_expanded <- function(df, target_name) {
    req(input$prism_primary_group, input$prism_value_var, input$prism_replicate_id)
    req(nzchar(input$prism_primary_group))
    
    group_var <- input$prism_primary_group
    value_var <- input$prism_value_var
    rep_id    <- input$prism_replicate_id
    
    df2 <- df %>%
      dplyr::filter(as.character(.data[["Target Name"]]) == target_name) %>%
      dplyr::transmute(
        RowTitle = as.character(.data[[rep_id]]),
        Group    = as.character(.data[[group_var]]),
        Value    = as.numeric(.data[[value_var]])
      ) %>%
      dplyr::group_by(RowTitle, Group) %>%
      dplyr::summarise(Value = r3(mean(Value, na.rm = TRUE)), .groups = "drop")
    
    df2 %>%
      tidyr::pivot_wider(
        id_cols     = RowTitle,
        names_from  = Group,
        values_from = Value
      ) %>%
      dplyr::arrange(RowTitle)
  }
  
  prism_tables <- reactive({
    req(data_with_ct_ref(), prism_n_parsed(), prism_group_candidates(), input$prism_table_type)
    df <- data_with_ct_ref()
    
    if (input$prism_table_type == "grouped") {
      make_tbl <- function(d) prism_make_grouped_table(d)
      
      if (prism_n_parsed() >= 3) {
        req(!is.null(input$prism_split_var))
        if (!nzchar(input$prism_split_var)) return(list())
        
        split_var <- input$prism_split_var
        split_levels <- sort(unique(as.character(df[[split_var]])))
        
        out <- lapply(split_levels, function(lvl) {
          df_sub <- df %>% dplyr::filter(as.character(.data[[split_var]]) == lvl)
          make_tbl(df_sub)
        })
        names(out) <- split_levels
        return(out)
      } else {
        return(list("ALL" = make_tbl(df)))
      }
    }
    
    targets <- sort(unique(as.character(df[["Target Name"]])))
    if (length(targets) == 0) return(list())
    
    build_for_df <- function(d, split_label) {
      out <- lapply(targets, function(tg) {
        if (identical(input$prism_column_layout, "expanded")) {
          prism_make_column_table_one_target_expanded(d, tg)
        } else {
          prism_make_column_table_one_target(d, tg)
        }
      })
      names(out) <- paste0(split_label, "__TARGET__", targets)
      out
    }
    
    if (prism_n_parsed() >= 3) {
      req(!is.null(input$prism_split_var))
      if (!nzchar(input$prism_split_var)) return(list())
      
      split_var <- input$prism_split_var
      split_levels <- sort(unique(as.character(df[[split_var]])))
      
      out_list <- list()
      for (lvl in split_levels) {
        df_sub <- df %>% dplyr::filter(as.character(.data[[split_var]]) == lvl)
        out_list <- c(out_list, build_for_df(df_sub, split_label = lvl))
      }
      return(out_list)
    } else {
      return(build_for_df(df, split_label = "ALL"))
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
    
    layout_suffix <- if (input$prism_table_type == "grouped") {
      "_grouped"
    } else if (identical(input$prism_column_layout, "expanded")) {
      "_column_expanded"
    } else {
      "_column_compact"
    }
    
    safe_prefix <- paste0(
      gsub("[^A-Za-z0-9_-]+", "_", input$prism_title),
      layout_suffix,
      "_prism"
    )
    
    paths <- character(0)
    
    for (nm in names(tabs)) {
      tag <- ""
      
      if (input$prism_table_type == "grouped") {
        tag <- if (nm == "ALL") "" else paste0("_", gsub("[^A-Za-z0-9_-]+", "_", nm))
      } else {
        parts <- strsplit(nm, "__TARGET__", fixed = TRUE)[[1]]
        split_label <- parts[1]
        target_name <- if (length(parts) >= 2) parts[2] else "UNKNOWN"
        
        split_tag  <- if (split_label == "ALL") "" else paste0("_", gsub("[^A-Za-z0-9_-]+", "_", split_label))
        target_tag <- paste0("_", gsub("[^A-Za-z0-9_-]+", "_", target_name))
        tag <- paste0(split_tag, target_tag)
      }
      
      out_path <- file.path(out_dir, paste0(safe_prefix, tag, ".csv"))
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

# ============================================================
# Run app
# ============================================================
shinyApp(ui, server)
