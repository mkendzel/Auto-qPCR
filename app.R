# ============================================================
# Auto-qPCR (Shiny)
# - Import qPCR .xls
# - Parse Sample Name into structured columns
# - Compute ct_ref, dCt, ddCt, relative_expression
# - Export Prism-ready tables (Grouped or Column formats)
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
    # TAB 1) Import raw qPCR files + header confirmation
    # ============================================================
    tabPanel(
      title = "1) Import",
      value = "import",
      sidebarLayout(
        sidebarPanel(
          # File combining mode (single vs multiple)
          radioButtons(
            "combine_files",
            "Do you have multiple qPCR files that should be combined?",
            choices = c("No (single file)" = FALSE, "Yes (multiple files)" = TRUE),
            selected = FALSE
          ),
          
          # File input switches between single/multiple
          uiOutput("file_upload_ui"),
          tags$hr(),
          
          # Gate to proceed once header is verified
          checkboxInput(
            "header_ok",
            "I confirm the header row and first data row look correct.",
            FALSE
          ),
          actionButton("continue_btn", "Continue", class = "btn-primary")
        ),
        mainPanel(
          h4("Data Preview"),
          
          # Toggle between main (NTC-removed) and NTC-only views
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
          
          # Number of underscore-delimited parts expected in Sample Name
          numericInput(
            "expected_parts",
            "How many underscore-delimited parts?",
            value = 4,
            min = 2
          ),
          tags$hr(),
          
          # User-defined labels for parsed parts (become new column names)
          uiOutput("part_labels_ui"),
          
          tags$hr(),
          h4("Reference gene setup"),
          
          # GAPDH grouping logic: single ID vs multi-dimensional grouping
          radioButtons(
            "multi_gapdh",
            "Does an individual contribute multiple GAPDH measurements? (e.g. mouse 1 has different GAPDH for each tissue)",
            choices = c("No" = "no", "Yes" = "yes"),
            selected = "no",
            inline = TRUE
          ),
          
          # Columns used to match reference gene measurements
          uiOutput("gapdh_group_cols_ui"),
          
          tags$hr(),
          
          # Reference gene selection (Target Name)
          uiOutput("ref_gene_ui"),
          
          tags$hr(),
          h4("Delta-Delta Ct setup (treatment + mock)"),
          
          # Treatment column + mock value + unique sample ID
          uiOutput("treatment_col_ui"),
          uiOutput("mock_value_ui"),
          uiOutput("ddct_id_col_ui"),
          
          tags$hr(),
          
          # Gate to proceed once parsing and setup are verified
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
      
      # Filename title
      textInput(
        "out_title",
        "Output table title (used for filename)",
        value = ""
      ),
      
      # Save gating
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
      
      # Prism table type selection
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
      
      # Configuration shows only after selecting a table type
      conditionalPanel(
        condition = "input.prism_table_type && input.prism_table_type.length > 0",
        
        # Optional split (one file per split level for grouped; split+target for column)
        uiOutput("prism_split_ui"),
        
        tags$hr(),
        
        # Primary and secondary grouping (secondary is grouped-only)
        uiOutput("prism_primary_ui"),
        uiOutput("prism_secondary_ui"),
        
        tags$hr(),
        
        # Row variable for grouped tables
        uiOutput("prism_rowvar_ui"),
        
        # Column-mode note + layout selector
        uiOutput("prism_column_note_ui"),
        
        # Column layout selector (compact vs expanded)
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
        
        # Value variable to export + replicate ID column
        uiOutput("prism_valuevar_ui"),
        uiOutput("prism_repid_ui"),
        
        tags$hr(),
        h4("Prism-formatted preview"),
        DTOutput("prism_preview"),
        
        tags$hr(),
        h4("Save output (Prism format)"),
        
        # Filename prefix
        textInput(
          "prism_title",
          "Output table title (used for filename prefix)",
          value = ""
        ),
        
        # Save gating
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
  
  # ============================================================
  # Helpers
  # ============================================================
  
  # Standard rounding used during calculations
  r3 <- function(x) round(x, 3)
  
  # ============================================================
  # Sample Name parsing: detect typical underscore part count
  # ============================================================
  detected_parts <- reactive({
    req(approved_data())
    df <- approved_data()
    req("Sample Name" %in% names(df))
    
    x <- as.character(df[["Sample Name"]])
    x <- trimws(x)
    x <- x[nzchar(x)]
    
    # number of parts = number of underscores + 1
    parts_n <- vapply(strsplit(x, "_", fixed = TRUE), length, integer(1))
    
    # mode (most frequent); if tie, take max among tied
    tab <- table(parts_n)
    top <- as.integer(names(tab)[tab == max(tab)])
    if (length(top) == 0) return(4L)
    max(top)
  })
  
  # Auto-fill expected_parts once Tab 1 data is approved
  observeEvent(approved_data(), {
    updateNumericInput(session, "expected_parts", value = detected_parts())
  }, ignoreInit = TRUE)
  
  # ============================================================
  # Global tab locks
  # ============================================================
  observe({
    shinyjs::disable(selector = 'a[data-value="parse"]')
    shinyjs::disable(selector = 'a[data-value="review_match"]')
    shinyjs::disable(selector = 'a[data-value="prism"]')
  })
  
  # ============================================================
  # TAB 1) File upload + preprocessing
  # ============================================================
  
  # File input UI switches based on combine mode
  output$file_upload_ui <- renderUI({
    if (identical(input$combine_files, "TRUE")) {
      fileInput("raw_files", "Upload qPCR .xls files", multiple = TRUE, accept = ".xls")
    } else {
      fileInput("raw_files", "Upload a qPCR .xls file", multiple = FALSE, accept = ".xls")
    }
  })
  
  # Preprocess uploaded files (expects preprocess_qpcr_files() helper)
  qpcr <- reactive({
    req(input$raw_files)
    preprocess_qpcr_files(
      files = input$raw_files,
      combine_multiple = identical(input$combine_files, "TRUE")
    )
  })
  
  # Data preview (main vs NTC-only)
  output$data_preview <- renderDT({
    req(qpcr())
    df <- if (input$preview_table == "ntc") qpcr()$ntc else qpcr()$main
    DT::datatable(df, rownames = FALSE, options = list(scrollX = TRUE))
  })
  
  # Shared state across tabs
  approved_data <- reactiveVal(NULL)
  parsed_data   <- reactiveVal(NULL)
  
  # Gate from Tab 1 -> Tab 2
  observeEvent(input$continue_btn, {
    validate(
      need(!is.null(input$raw_files), "Upload file(s) first."),
      need(isTRUE(input$header_ok), "Confirm the header row.")
    )
    
    approved_data(qpcr()$main)
    shinyjs::enable(selector = 'a[data-value="parse"]')
    updateTabsetPanel(session, "page", selected = "parse")
  })
  
  # ============================================================
  # TAB 2) Sample Name parsing + setup (live)
  # ============================================================
  
  # Labels for each parsed part
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
  
  # Parsed part column names (with fallback defaults)
  parsed_part_cols <- reactive({
    req(input$expected_parts)
    vapply(seq_len(input$expected_parts), function(i) {
      lbl <- input[[paste0("part_label_", i)]]
      if (!is.null(lbl) && nzchar(lbl)) lbl else paste0("part", i)
    }, character(1))
  })
  
  # Split Sample Name into parts (expects split_sample_name() helper)
  split_df_live <- reactive({
    req(approved_data(), parsed_part_cols(), input$expected_parts)
    
    split_sample_name(
      df = approved_data(),
      n_parts = input$expected_parts,
      part_labels = parsed_part_cols(),
      keep_sample_name = FALSE
    )
  })
  
  # Apply typing and rounding consistently
  split_df_typed <- reactive({
    req(split_df_live(), parsed_part_cols())
    df <- split_df_live()
    
    # Parsed parts as factors (for consistent grouping/joins)
    for (col in parsed_part_cols()) df[[col]] <- as.factor(df[[col]])
    
    # CT numeric + rounding on ingest
    df$CT <- r3(as.numeric(df$CT))
    
    # Target Name as factor
    df$`Target Name` <- as.factor(df$`Target Name`)
    
    df
  })
  
  # Parsed table preview
  output$sample_parse_preview <- renderDT({
    req(split_df_typed())
    DT::datatable(split_df_typed(), rownames = FALSE, options = list(scrollX = TRUE))
  })
  
  # ============================================================
  # ddCt setup: treatment column + mock value + unique ID
  # ============================================================
  
  # Treatment column selector (from parsed parts)
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
  
  # ============================================================
  # GAPDH grouping
  # ============================================================
  
  # If multi_gapdh == yes: user selects multiple grouping columns
  # If multi_gapdh == no: user selects a single column ID for GAPDH matching
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
  
  # ============================================================
  # Reference gene selection
  # ============================================================
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
  
  # Reset reference selection when parsed table changes
  observeEvent(split_df_typed(), {
    updateSelectInput(session, "ref_gene", selected = "")
  }, ignoreInit = TRUE)
  
  # Mock value selector depends on selected treatment column
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
  
  # Unique sample ID column used for replicate identity
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
  
  # Reset mock selection when treatment changes
  observeEvent(input$treatment_col, {
    updateSelectInput(session, "mock_value", selected = "")
  }, ignoreInit = TRUE)
  
  # Reset ddCt selectors when parsed data changes
  observeEvent(split_df_typed(), {
    updateSelectInput(session, "treatment_col", selected = "")
    updateSelectInput(session, "mock_value", selected = "")
    updateSelectInput(session, "ddct_id_col", selected = "")
  }, ignoreInit = TRUE)
  
  # ============================================================
  # Tab 2 gating: enable Continue only when setup is complete
  # ============================================================
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
  
  # ============================================================
  # Freeze parsed state + unlock downstream tabs
  # ============================================================
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
  
  # ============================================================
  # ddCt pipeline: ct_ref, dCt, ddCt, relative_expression
  # ============================================================
  data_with_ct_ref <- eventReactive(input$continue_to_tab3, {
    req(parsed_data(), input$ref_gene, input$gapdh_group_cols, input$treatment_col, input$mock_value, input$ddct_id_col)
    
    df <- parsed_data()
    
    # Baseline grouping excludes treatment + replicate ID
    baseline_group_cols <- setdiff(parsed_part_cols(), c(input$treatment_col, input$ddct_id_col))
    
    # 1) Reference CT per GAPDH grouping (mean CT for reference gene)
    ref_df <- df %>%
      dplyr::filter(`Target Name` == input$ref_gene) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(input$gapdh_group_cols))) %>%
      dplyr::summarise(ct_ref = r3(mean(CT, na.rm = TRUE)), .groups = "drop")
    
    # 2) Attach ct_ref, drop reference gene rows, compute dCt
    out <- df %>%
      dplyr::left_join(ref_df, by = input$gapdh_group_cols) %>%
      dplyr::filter(`Target Name` != input$ref_gene) %>%
      dplyr::mutate(
        dCt = r3(CT - ct_ref),
        .is_mock = as.character(.data[[input$treatment_col]]) == input$mock_value
      )
    
    # 3) Mock mean dCt per baseline grouping + gene
    mock_means <- out %>%
      dplyr::filter(.is_mock) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(baseline_group_cols, "Target Name")))) %>%
      dplyr::summarise(mock_mean_dCt = r3(mean(dCt, na.rm = TRUE)), .groups = "drop")
    
    # 4) ddCt and relative expression
    out %>%
      dplyr::left_join(mock_means, by = c(baseline_group_cols, "Target Name")) %>%
      dplyr::mutate(
        ddCt = r3(dCt - mock_mean_dCt),
        relative_expression = r3(2^(-ddCt))
      ) %>%
      dplyr::select(-.is_mock)
  })
  
  # ============================================================
  # TAB 3) Preview + save long format
  # ============================================================
  
  # Long format preview
  output$ctref_preview <- renderDT({
    req(data_with_ct_ref())
    DT::datatable(
      data_with_ct_ref(),
      rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 10, lengthMenu = c(10, 25, 50, 100))
    )
  })
  
  # Setup summary for quick verification
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
  
  # Save status message storage
  save_status_val <- reactiveVal("")
  output$save_status <- renderText({ save_status_val() })
  
  # Save long format CSV
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
  
  # Tab 1 status text
  output$status_msg <- renderPrint({
    if (is.null(input$raw_files)) "Upload file(s) to begin."
    else if (!isTRUE(input$header_ok)) "Confirm the header row."
    else "Ready."
  })
  
  # ============================================================
  # TAB 4) Prism export logic
  # ============================================================
  
  # Parsed part count (used to decide whether to offer split-by)
  prism_n_parsed <- reactive({
    req(parsed_part_cols())
    length(parsed_part_cols())
  })
  
  # Eligible grouping/splitting candidates exclude replicate ID
  prism_group_candidates <- reactive({
    req(parsed_part_cols(), input$ddct_id_col)
    setdiff(parsed_part_cols(), input$ddct_id_col)
  })
  
  # Console-style status: detected columns + guidance
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
  
  # Split-by selector offered when >= 3 parsed parts
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
  
  # Remaining candidates after optional split selection
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
  
  # Primary grouping variable (Prism columns)
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
  
  # Secondary grouping (grouped-only)
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
  
  # Row variable (grouped-only)
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
  
  # Column mode informational note
  output$prism_column_note_ui <- renderUI({
    req(input$prism_table_type)
    if (input$prism_table_type != "column") return(NULL)
    helpText("Column mode exports ONE file per Target Name. Each file is a Prism Column table: columns = groups; rows = replicates; blanks are allowed.")
  })
  
  # Numeric value selector for Prism plotting
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
  
  # Replicate ID selector (defaults to ddct_id_col)
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
  
  # ============================================================
  # Prism builders
  # ============================================================
  
  # ------------------------------------------------------------
  # Grouped export: single wide table (split-aware upstream)
  # - columns are group:replicate labels
  # - rows are row_var (typically Target Name)
  # ------------------------------------------------------------
  prism_make_grouped_table <- function(df) {
    req(input$prism_primary_group, input$prism_row_var, input$prism_value_var, input$prism_replicate_id)
    req(nzchar(input$prism_primary_group))
    
    col_primary <- input$prism_primary_group
    col_secondary <- if (!is.null(input$prism_secondary_group) && nzchar(input$prism_secondary_group)) input$prism_secondary_group else NULL
    
    row_var   <- input$prism_row_var
    value_var <- input$prism_value_var
    rep_id    <- input$prism_replicate_id
    
    # Build a single column-group label
    if (is.null(col_secondary)) {
      df2 <- df %>% dplyr::mutate(.col_group = as.character(.data[[col_primary]]))
    } else {
      df2 <- df %>% dplyr::mutate(.col_group = paste(
        as.character(.data[[col_primary]]),
        as.character(.data[[col_secondary]]),
        sep = " | "
      ))
    }
    
    # Reduce duplicates to mean per (row, col_group, replicate)
    df3 <- df2 %>%
      dplyr::select(dplyr::all_of(row_var), .col_group, dplyr::all_of(rep_id), .value = dplyr::all_of(value_var)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(row_var, ".col_group", rep_id)))) %>%
      dplyr::summarise(.value = r3(mean(.value, na.rm = TRUE)), .groups = "drop")
    
    # Build Prism column names as "<group>: <replicate>"
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
    
    # Wide Prism Grouped layout
    df4 %>%
      dplyr::select(dplyr::all_of(row_var), .colname, .value) %>%
      tidyr::pivot_wider(
        id_cols = dplyr::all_of(row_var),
        names_from = .colname,
        values_from = .value
      ) %>%
      dplyr::arrange(.data[[row_var]])
  }
  
  # ------------------------------------------------------------
  # Column export (compact)
  # - one column per group
  # - rows are replicate slots (1..N), unlabeled
  # ------------------------------------------------------------
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
    
    # Collapse duplicates per (Group, ID)
    df2 <- df2 %>%
      dplyr::group_by(Group, ID) %>%
      dplyr::summarise(Value = r3(mean(Value, na.rm = TRUE)), .groups = "drop") %>%
      dplyr::arrange(Group, ID)
    
    # Compact layout: replicate slots by group
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
  
  # ------------------------------------------------------------
  # Column export (expanded)
  # - one row per replicate ID (labeled)
  # - value appears only under its group column
  # ------------------------------------------------------------
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
  
  # ============================================================
  # Prism table assembly (split-aware)
  # - Grouped: one table per split level (or ALL)
  # - Column: one table per target (and per split level if enabled)
  # ============================================================
  prism_tables <- reactive({
    req(data_with_ct_ref(), prism_n_parsed(), prism_group_candidates(), input$prism_table_type)
    df <- data_with_ct_ref()
    
    # -----------------------------
    # Grouped mode
    # -----------------------------
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
    
    # -----------------------------
    # Column mode (one file per target)
    # -----------------------------
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
  
  # Prism preview (first table only)
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
  
  # Save status message storage
  prism_save_status_val <- reactiveVal("")
  output$prism_save_status <- renderText({ prism_save_status_val() })
  
  # ============================================================
  # Save Prism exports (one or many files)
  # ============================================================
  observeEvent(input$save_prism, {
    tabs <- prism_tables()
    req(length(tabs) >= 1)
    req(isTRUE(input$prism_ok))
    req(nzchar(input$prism_title))
    
    out_dir <- file.path("data", "output", "prism")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Filename suffix indicates table organization choice
    layout_suffix <- if (input$prism_table_type == "grouped") {
      "_grouped"
    } else if (identical(input$prism_column_layout, "expanded")) {
      "_column_expanded"
    } else {
      "_column_compact"
    }
    
    safe_prefix <- paste0(
      gsub("[^A-Za-z0-9_-]+", "_", input$prism_title),
      layout_suffix
    )
    
    paths <- character(0)
    
    for (nm in names(tabs)) {
      tag <- ""
      
      # Grouped filenames: optional split tag
      if (input$prism_table_type == "grouped") {
        tag <- if (nm == "ALL") "" else paste0("_", gsub("[^A-Za-z0-9_-]+", "_", nm))
      } else {
        # Column filenames: split + target tags
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
