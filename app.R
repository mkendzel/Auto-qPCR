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
library(shinyjs)

# Source helper functions from R/ (file parsing, directory creation, logging helpers, etc.)
invisible(lapply(list.files("R", "\\.R$", full.names = TRUE), source, local = FALSE))

# Keep validate/need in short names for input gating messages
validate <- shiny::validate
need <- shiny::need

## =========================
## UI
## =========================
ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Auto-qPCR"),
  
  # Main navigation across the 5-step workflow
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
          # Experiment folder name (sanitized into data/<experiment_name>/)
          textInput(
            "experiment_name",
            "Project name (folder created in data/ to store experiment files)",
            value = ""
          ),
          helpText("Allowed: letters, numbers, underscore, hyphen. Other characters will be replaced with '_'."),
          
          tags$hr(),
          
          # Single vs multi-file import handling
          radioButtons(
            "combine_files",
            "Do you have multiple qPCR files that should be combined?",
            choices = c("No (single file)" = FALSE, "Yes (multiple files)" = TRUE),
            selected = FALSE
          ),
          
          # File input UI (switches between single vs multiple fileInput)
          uiOutput("file_upload_ui"),
          
          tags$hr(),
          
          # Manual confirmation gate before allowing workflow to proceed
          checkboxInput(
            "header_ok",
            "I confirm the header row and first data row look correct.",
            FALSE
          ),
          
          # Locks import + copies files into experiment directory + enables next tab
          actionButton("continue_btn", "Continue", class = "btn-primary")
        ),
        mainPanel(
          h4("Data Preview"),
          
          # Toggle preview between main dataset (NTC removed) and NTC-only rows
          radioButtons(
            "preview_table",
            "Preview table",
            choices = c("Main data (NTC removed)" = "main", "NTC rows only" = "ntc"),
            selected = "main",
            inline = TRUE
          ),
          
          # Data preview table (based on preprocess_qpcr_files output)
          DTOutput("data_preview"),
          
          tags$hr(),
          
          # Status message for current readiness / gating
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
          # Expected sample naming convention (underscore-delimited)
          helpText("Expected format: 1_w_x_y ... (underscore-delimited)."),
          numericInput(
            "expected_parts",
            "How many underscore-delimited parts?",
            value = 4,
            min = 2
          ),
          
          tags$hr(),
          
          # Dynamic UI for naming each parsed part (part1, part2, ...)
          uiOutput("part_labels_ui"),
          
          tags$hr(),
          
          # Reference gene setup for ct_ref calculation (single or multiple GAPDH grouping)
          h4("Reference gene setup"),
          radioButtons(
            "multi_gapdh",
            "Does an individual contribute multiple GAPDH measurements? (e.g. mouse 1 has different GAPDH for each tissue)",
            choices = c("No" = "no", "Yes" = "yes"),
            selected = "no",
            inline = TRUE
          ),
          
          # Dynamic UI for choosing grouping columns used to compute ct_ref
          uiOutput("gapdh_group_cols_ui"),
          
          tags$hr(),
          
          # Reference gene selection from Target Name values
          uiOutput("ref_gene_ui"),
          
          tags$hr(),
          
          # ΔΔCt setup: choose treatment column, mock value, and unique sample_id column
          h4("Delta-Delta Ct setup (treatment + mock)"),
          uiOutput("treatment_col_ui"),
          uiOutput("mock_value_ui"),
          uiOutput("ddct_id_col_ui"),
          
          tags$hr(),
          
          # Manual confirmation gate before locking parse choices and proceeding to QC
          checkboxInput(
            "parse_ok",
            "I confirm the parsed columns and reference setup look correct.",
            FALSE
          ),
          
          # Locks parsed_data and enables QC tab
          actionButton("continue_to_tab3", "Continue", class = "btn-primary")
        ),
        column(
          6,
          # Live preview of parsing choices (updates as labels/parts change)
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
      
      # Prints overall QC flag counts (RED/YELLOW) based on qc_results()
      verbatimTextOutput("qc_status"),
      
      tags$hr(),
      
      h4("QC failures"),
      
      # Table of all QC flags (one row per sample/target/flag)
      DTOutput("qc_fail_preview"),
      
      tags$hr(),
      
      h4("Save for editing"),
      
      # Title used to write QC edit workbook into experiment qc/
      textInput(
        "qc_title",
        "Edit file name (saved to experiment qc/)",
        value = ""
      ),
      helpText("Name for current QC pass (example: experimentname_qc_1)"),
      
      # Confirmation gate for writing the XLSX with Results + qc_index sheets
      checkboxInput(
        "qc_save_ok",
        "I confirm I want to save a copy for editing (Tab 1 dataset + QC index).",
        value = FALSE
      ),
      
      # Writes <qc_title>_edited.xlsx to qc/
      actionButton("save_qc_edit", "Save edit XLSX", class = "btn-primary"),
      
      tags$hr(),
      
      # Save status text for QC edit workbook
      verbatimTextOutput("qc_save_status"),
      
      tags$hr(),
      
      h4("Continue"),
      
      # Confirmation gate for proceeding despite QC flags
      checkboxInput(
        "qc_override_ok",
        "Continue to analysis anyway (not recommended if QC failures are present).",
        value = FALSE
      ),
      
      # Enables downstream tabs and advances to long-format export
      actionButton("continue_to_review", "Continue", class = "btn-primary")
    ),
    
    # ============================================================
    # TAB 4) Long format Export
    # ============================================================
    tabPanel(
      title = "4) Long format Export",
      value = "review_match",
      h4("Reference configuration summary"),
      
      # Prints the locked configuration used for ct_ref, dCt, ddCt, etc.
      verbatimTextOutput("review_match_status"),
      
      tags$hr(),
      
      h4("Long format preview"),
      
      # Preview of data_with_ct_ref() (long format with ct_ref/dCt/ddCt/relative_expression)
      DTOutput("ctref_preview"),
      
      tags$hr(),
      
      h4("Save output (long format)"),
      
      # Title used as prefix for long export CSV filename
      textInput(
        "out_title",
        "Output table title (used for filename)",
        value = ""
      ),
      
      # Confirmation gate for writing long CSV
      checkboxInput(
        "save_ok",
        "I confirm this table is correct and should be saved as a CSV.",
        value = FALSE
      ),
      
      # Writes <out_title>_long.csv to exports/
      actionButton("save_csv", "Save CSV", class = "btn-primary"),
      
      tags$hr(),
      
      # Save status text for long CSV export
      verbatimTextOutput("save_status")
    ),
    
    # ============================================================
    # TAB 5) Prism Export
    # ============================================================
    tabPanel(
      title = "5) Prism Export",
      value = "prism",
      h4("Prism export setup"),
      
      # Select Prism export mode (Grouped or Column)
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
      
      # Prints detected candidate columns and guidance based on chosen table type
      verbatimTextOutput("prism_detect_status"),
      
      tags$hr(),
      
      # Only show configuration UI after table type is chosen
      conditionalPanel(
        condition = "input.prism_table_type && input.prism_table_type.length > 0",
        
        # Optional split variable (only offered when >=3 parsed columns)
        uiOutput("prism_split_ui"),
        tags$hr(),
        
        # Primary grouping variable (required)
        uiOutput("prism_primary_ui"),
        
        # Optional secondary grouping variable (grouped mode only)
        uiOutput("prism_secondary_ui"),
        tags$hr(),
        
        # Row variable selection (grouped mode only)
        uiOutput("prism_rowvar_ui"),
        
        # Column mode help text (column mode only)
        uiOutput("prism_column_note_ui"),
        
        # Column mode layout controls (compact vs expanded)
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
        
        # Select which numeric field to plot/export
        uiOutput("prism_valuevar_ui"),
        
        # Select replicate ID column used to align rows/replicate labels
        uiOutput("prism_repid_ui"),
        
        tags$hr(),
        
        h4("Prism-formatted preview"),
        
        # Preview of first Prism table from prism_tables()
        DTOutput("prism_preview"),
        
        tags$hr(),
        
        h4("Save output (Prism format)"),
        
        # Filename prefix for Prism export(s)
        textInput(
          "prism_title",
          "Output table title (used for filename prefix)",
          value = ""
        ),
        
        # Confirmation gate for writing Prism CSV(s)
        checkboxInput(
          "prism_ok",
          "I confirm this Prism export configuration is correct and should be saved.",
          value = FALSE
        ),
        
        # Writes one or more Prism CSVs into exports/
        actionButton("save_prism", "Save Prism CSV(s)", class = "btn-primary"),
        
        tags$hr(),
        
        # Save status text listing written files
        verbatimTextOutput("prism_save_status")
      )
    )
  )
)

## =========================
## SERVER
## =========================
server <- function(input, output, session) {
  
  # Rounding helper used throughout exported metrics
  r3 <- function(x) round(x, 3)
  
  # Data state across tabs:
  # - approved_data_original: raw imported main dataset (before .import_row)
  # - approved_data: main dataset with .import_row tracking
  # - parsed_data: parsed + typed dataset locked at end of Tab 2
  approved_data <- reactiveVal(NULL)
  approved_data_original <- reactiveVal(NULL)
  parsed_data <- reactiveVal(NULL)
  
  # Sanitized experiment name used for folder paths
  exp_name_safe <- reactive({
    sanitize_experiment_name(input$experiment_name)
  })
  
  # Root experiment directory: data/<experiment_name_safe>
  exp_dir <- reactive({
    nm <- exp_name_safe()
    if (!nzchar(nm)) return(NULL)
    file.path("data", nm)
  })
  
  # Logger handle (set once experiment is initialized)
  logger <- reactiveVal(NULL)
  
  # Initialize experiment directories + logger on first Continue from Tab 1
  observeEvent(input$continue_btn, {
    req(nzchar(exp_name_safe()))
    ensure_experiment_dirs(exp_dir())
    logger(init_logger(exp_dir(), exp_name_safe()))
  }, ignoreInit = TRUE)
  
  # Detect round-2 uploads: edited QC workbooks contain a qc_index sheet
  is_processed_round2_xlsx <- function(path) {
    ext <- tolower(tools::file_ext(path))
    if (ext != "xlsx") return(FALSE)
    sheets <- openxlsx::getSheetNames(path)
    "qc_index" %in% sheets
  }
  
  # Default: lock downstream tabs until workflow gates unlock them
  observe({
    shinyjs::disable(selector = 'a[data-value="parse"]')
    shinyjs::disable(selector = 'a[data-value="qc"]')
    shinyjs::disable(selector = 'a[data-value="review_match"]')
    shinyjs::disable(selector = 'a[data-value="prism"]')
  })
  
  # Tab 1 file upload UI switches based on combine_files choice
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
  
  # Read and preprocess qPCR files into main + ntc tables
  qpcr <- reactive({
    req(input$raw_files)
    preprocess_qpcr_files(
      files = input$raw_files,
      combine_multiple = identical(input$combine_files, "TRUE")
    )
  })
  
  # Tab 1 preview table (main vs ntc)
  output$data_preview <- renderDT({
    req(qpcr())
    df <- if (input$preview_table == "ntc") qpcr()$ntc else qpcr()$main
    DT::datatable(df, rownames = FALSE, options = list(scrollX = TRUE))
  })
  
  # Auto-detect most common underscore-part count from Sample Name to prefill expected_parts
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
  
  # Update expected_parts when import is approved
  observeEvent(approved_data(), {
    updateNumericInput(session, "expected_parts", value = detected_parts())
  }, ignoreInit = TRUE)
  
  # Tab 1 Continue: validate inputs, copy files, write import metadata, lock approved_data, enable Tab 2
  observeEvent(input$continue_btn, {
    validate(
      need(nzchar(exp_name_safe()), "Enter an experiment name."),
      need(!is.null(input$raw_files), "Upload file(s) first."),
      need(isTRUE(input$header_ok), "Confirm the header row.")
    )
    
    # Log button click + context
    if (!is.null(logger())) {
      logger()$append_event(
        tab = input$page,
        action = "continue_btn_clicked",
        details = list(
          header_ok = isTRUE(input$header_ok),
          combine_files = input$combine_files,
          file_names = input$raw_files$name
        )
      )
    }
    
    # Ensure experiment directory structure exists
    base_dir <- exp_dir()
    ensure_experiment_dirs(base_dir)
    
    # Copy uploads into raw/ vs processed/ depending on whether they look like round-2 edited files
    raw_dir <- file.path(base_dir, "raw")
    processed_dir <- file.path(base_dir, "processed")
    
    for (i in seq_len(nrow(input$raw_files))) {
      src <- input$raw_files$datapath[i]
      dst_dir <- if (is_processed_round2_xlsx(src)) processed_dir else raw_dir
      dst <- file.path(dst_dir, basename(input$raw_files$name[i]))
      file.copy(src, dst, overwrite = TRUE)
    }
    
    # Log file copy operation
    if (!is.null(logger())) {
      logger()$append_event(
        tab = input$page,
        action = "files_copied_to_experiment",
        details = list(
          copied_files = input$raw_files$name,
          experiment_dir = exp_dir()
        )
      )
    }
    
    # Write a simple import metadata record
    meta_dir <- file.path(base_dir, "meta_data")
    meta <- data.frame(
      saved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      experiment_name_input = input$experiment_name,
      experiment_name_safe = exp_name_safe(),
      file_names = paste(input$raw_files$name, collapse = "; "),
      stringsAsFactors = FALSE
    )
    write.csv(meta, file.path(meta_dir, "import_meta.csv"), row.names = FALSE)
    
    # Store baseline main dataset (used later when saving QC edit workbook)
    approved_data_original(qpcr()$main)
    
    # Store working dataset with an import row index for traceability
    df0 <- qpcr()$main
    df0 <- df0 %>% dplyr::mutate(.import_row = dplyr::row_number())
    approved_data(df0)
    
    # Unlock Tab 2 and navigate forward
    shinyjs::enable(selector = 'a[data-value="parse"]')
    updateTabsetPanel(session, "page", selected = "parse")
  })
  
  # Dynamic UI for labeling each parsed part (part_label_1 ... part_label_k)
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
  
  # Vector of parsed part column names, using UI labels (fallback to part1/part2/...)
  parsed_part_cols <- reactive({
    req(input$expected_parts)
    vapply(seq_len(input$expected_parts), function(i) {
      lbl <- input[[paste0("part_label_", i)]]
      if (!is.null(lbl) && nzchar(lbl)) lbl else paste0("part", i)
    }, character(1))
  })
  
  # Live split of Sample Name into parsed columns (no type conversions)
  split_df_live <- reactive({
    req(approved_data(), parsed_part_cols(), input$expected_parts)
    split_sample_name(
      df = approved_data(),
      n_parts = input$expected_parts,
      part_labels = parsed_part_cols(),
      keep_sample_name = FALSE
    )
  })
  
  # Apply typing to parsed columns and key fields for cleaner downstream behavior
  split_df_typed <- reactive({
    req(split_df_live(), parsed_part_cols())
    df <- split_df_live()
    for (col in parsed_part_cols()) df[[col]] <- as.factor(df[[col]])
    df$CT <- r3(as.numeric(df$CT))
    df$`Target Name` <- as.factor(df$`Target Name`)
    df
  })
  
  # Tab 2 preview of parsed/typed dataset
  output$sample_parse_preview <- renderDT({
    req(split_df_typed())
    DT::datatable(split_df_typed(), rownames = FALSE, options = list(scrollX = TRUE))
  })
  
  # Choose which parsed column corresponds to treatment
  output$treatment_col_ui <- renderUI({
    req(parsed_part_cols())
    cols <- parsed_part_cols()
    selectInput(
      "treatment_col",
      "Which is your treatment column?",
      choices  = c("— Select a column —" = "", stats::setNames(cols, cols)),
      selected = "",
      multiple = FALSE,
      selectize = TRUE
    )
  })
  
  # GAPDH grouping column selector changes depending on whether multiple GAPDH per individual is expected
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
  
  # Reference gene selection (ct_ref baseline)
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
  
  # Clear ref_gene selection when upstream data changes
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
  
  # Unique sample ID column used for QC aggregation and exports
  output$ddct_id_col_ui <- renderUI({
    req(parsed_part_cols())
    cols <- parsed_part_cols()
    selectInput(
      "ddct_id_col",
      "Which column stores your unique Sample IDs?",
      choices  = c("— Select a column —" = "", stats::setNames(cols, cols)),
      selected = "",
      multiple = FALSE,
      selectize = TRUE
    )
  })
  
  # Reset mock selection when treatment column changes
  observeEvent(input$treatment_col, {
    updateSelectInput(session, "mock_value", selected = "")
  }, ignoreInit = TRUE)
  
  # Reset key Tab 2 selectors if the parsed dataset changes
  observeEvent(split_df_typed(), {
    updateSelectInput(session, "treatment_col", selected = "")
    updateSelectInput(session, "mock_value", selected = "")
    updateSelectInput(session, "ddct_id_col", selected = "")
  }, ignoreInit = TRUE)
  
  # Default: disable Tab 2 Continue until required fields are chosen
  observe({ shinyjs::disable("continue_to_tab3") })
  
  # Enable Tab 2 Continue only when all required selections are present
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
  
  # Tab 2 Continue: validate + log + lock parsed_data + enable QC tab
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
    
    # Log parse configuration used for downstream calculations
    if (!is.null(logger())) {
      logger()$append_event(
        tab = input$page,
        action = "continue_to_tab3_clicked",
        details = list(
          expected_parts = input$expected_parts,
          part_labels = parsed_part_cols(),
          multi_gapdh = input$multi_gapdh,
          gapdh_group_cols = input$gapdh_group_cols,
          ref_gene = input$ref_gene,
          treatment_col = input$treatment_col,
          mock_value = input$mock_value,
          ddct_id_col = input$ddct_id_col
        )
      )
    }
    
    # Lock parsed dataset used for QC + exports
    parsed_data(split_df_typed())
    
    # Unlock Tab 3 and navigate forward
    shinyjs::enable(selector = 'a[data-value="qc"]')
    updateTabsetPanel(session, "page", selected = "qc")
  })
  
  # Compute ct_ref, dCt, mock mean dCt, ddCt, and relative expression after Tab 2 is confirmed
  data_with_ct_ref <- eventReactive(input$continue_to_tab3, {
    req(parsed_data(), input$ref_gene, input$gapdh_group_cols, input$treatment_col, input$mock_value, input$ddct_id_col)
    df <- parsed_data()
    baseline_group_cols <- setdiff(parsed_part_cols(), c(input$treatment_col, input$ddct_id_col))
    
    # Reference gene ct_ref computed per selected GAPDH grouping
    ref_df <- df %>%
      dplyr::filter(`Target Name` == input$ref_gene) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(input$gapdh_group_cols))) %>%
      dplyr::summarise(ct_ref = r3(mean(CT, na.rm = TRUE)), .groups = "drop")
    
    # Join ct_ref back; drop ref gene rows; compute dCt
    out <- df %>%
      dplyr::left_join(ref_df, by = input$gapdh_group_cols) %>%
      dplyr::filter(`Target Name` != input$ref_gene) %>%
      dplyr::mutate(
        dCt = r3(CT - ct_ref),
        .is_mock = as.character(.data[[input$treatment_col]]) == input$mock_value
      )
    
    # Mock mean dCt computed per baseline group columns + Target Name
    mock_means <- out %>%
      dplyr::filter(.is_mock) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(baseline_group_cols, "Target Name")))) %>%
      dplyr::summarise(mock_mean_dCt = r3(mean(dCt, na.rm = TRUE)), .groups = "drop")
    
    # ddCt + relative expression (2^-ddCt)
    out %>%
      dplyr::left_join(mock_means, by = c(baseline_group_cols, "Target Name")) %>%
      dplyr::mutate(
        ddCt = r3(dCt - mock_mean_dCt),
        relative_expression = r3(2^(-ddCt))
      ) %>%
      dplyr::select(-.is_mock)
  })
  
  # Build QC flags table (ref CT range, mock dCt SD, mock rel expr deviation)
  qc_results <- reactive({
    req(parsed_data(), data_with_ct_ref())
    req(input$ref_gene, input$treatment_col, input$mock_value, input$ddct_id_col)
    
    df_all    <- parsed_data()
    df_long   <- data_with_ct_ref()
    id_col    <- input$ddct_id_col
    trt_col   <- input$treatment_col
    meta_cols <- parsed_part_cols()
    
    # Extra metadata columns carried into QC output (excluding id + treatment)
    meta_extra <- setdiff(meta_cols, c(id_col, trt_col))
    
    # Map sample_id -> metadata values (first seen per sample)
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
    
    # QC1: reference gene CT outside expected range (18–22)
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
    
    # Add .is_mock flag for mock-only QC checks
    df_long2 <- df_long %>%
      dplyr::mutate(.is_mock = as.character(.data[[trt_col]]) == input$mock_value)
    
    # QC2: mock dCt SD too high within (sample_id, treatment, target)
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
    
    # QC3: mock relative expression too far from 1 (absolute deviation thresholds)
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
    
    # Combine QC flags + attach metadata + order by severity
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
  
  # Tab 3: QC summary counts
  qc_metric_labels <- c(
    "GAPDH Ct range (18-22)",
    "Standard deviation of reference ΔCt (<0.5)",
    "Mock relative expression (~1)"
  )
  
  output$qc_status <- renderPrint({
    req(qc_results())
    qt <- qc_results()$qc_table
    cat("Quality control metrics evaluated:\n")
    cat(paste0("- ", qc_metric_labels, collapse = "\n"), "\n\n")
    cat("Total QC flags:", nrow(qt), "\n")
    cat("RED:", sum(qt$severity == "RED"), "\n")
    cat("YELLOW:", sum(qt$severity == "YELLOW"), "\n")
  })
  
  # Tab 3: QC failures table
  output$qc_fail_preview <- renderDT({
    req(qc_results())
    DT::datatable(
      qc_results()$qc_table,
      rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 10, lengthMenu = c(10, 25, 50, 100))
    )
  })
  
  # QC edit save status text buffer
  qc_save_status_val <- reactiveVal("")
  output$qc_save_status <- renderText({ qc_save_status_val() })
  
  # Save QC edit workbook: Results sheet (Tab 1 dataset) + qc_index sheet (QC flags)
  observeEvent(input$save_qc_edit, {
    req(qc_results(), approved_data_original())
    req(isTRUE(input$qc_save_ok))
    req(nzchar(input$qc_title))
    req(exp_dir())
    
    # Log the save intent + summary QC counts
    if (!is.null(logger())) {
      logger()$append_event(
        tab = input$page,
        action = "save_qc_edit_clicked",
        details = list(
          qc_title = input$qc_title,
          qc_save_ok = isTRUE(input$qc_save_ok),
          qc_flags_n = nrow(qc_results()$qc_table),
          qc_red_n = sum(qc_results()$qc_table$severity == "RED", na.rm = TRUE),
          qc_yellow_n = sum(qc_results()$qc_table$severity == "YELLOW", na.rm = TRUE)
        )
      )
    }
    
    # Ensure qc/ exists
    qc_dir <- file.path(exp_dir(), "qc")
    dir.create(qc_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Write <title>_edited.xlsx into qc/
    base_name <- gsub("[^A-Za-z0-9_-]+", "_", input$qc_title)
    out_path_xlsx <- file.path(qc_dir, paste0(base_name, "_edited.xlsx"))
    
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Results")
    openxlsx::writeData(wb, "Results", approved_data_original())
    openxlsx::addWorksheet(wb, "qc_index")
    openxlsx::writeData(wb, "qc_index", qc_results()$qc_table)
    openxlsx::saveWorkbook(wb, out_path_xlsx, overwrite = TRUE)
    
    # Log the output path
    if (!is.null(logger())) {
      logger()$append_event(
        tab = input$page,
        action = "qc_edit_xlsx_saved",
        details = list(out_path_xlsx = out_path_xlsx)
      )
    }
    
    showNotification(
      paste("QC XLSX saved:", out_path_xlsx),
      type = "message",
      duration = 6
    )
    
    qc_save_status_val(
      paste(
        "Saved successfully:\n- ", out_path_xlsx, "\n",
        "Time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        sep = ""
      )
    )
  })
  
  # Tab 3 Continue: block if QC flags exist unless override is checked; then unlock Tabs 4/5
  observeEvent(input$continue_to_review, {
    req(qc_results())
    
    # Log attempt to proceed and whether override was used
    if (!is.null(logger())) {
      logger()$append_event(
        tab = input$page,
        action = "continue_to_review_clicked",
        details = list(
          qc_override_ok = isTRUE(input$qc_override_ok),
          qc_flags_n = nrow(qc_results()$qc_table)
        )
      )
    }
    
    n_fail <- nrow(qc_results()$qc_table)
    if (n_fail > 0) {
      validate(need(isTRUE(input$qc_override_ok), "QC failures detected."))
    }
    
    shinyjs::enable(selector = 'a[data-value="review_match"]')
    shinyjs::enable(selector = 'a[data-value="prism"]')
    updateTabsetPanel(session, "page", selected = "review_match")
  })
  
  # Tab 4 preview: long table with ct_ref/dCt/ddCt/relative_expression
  output$ctref_preview <- renderDT({
    req(data_with_ct_ref())
    DT::datatable(
      data_with_ct_ref(),
      rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 10, lengthMenu = c(10, 25, 50, 100))
    )
  })
  
  # Tab 4: print a configuration summary showing the locked inputs
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
      cat("Experiment folder:", exp_dir(), "\n")
    }
  })
  
  # Long export save status text buffer
  save_status_val <- reactiveVal("")
  output$save_status <- renderText({ save_status_val() })
  
  # Save long CSV + write a logger snapshot capturing inputs/QC/outputs/packages
  observeEvent(input$save_csv, {
    req(data_with_ct_ref())
    req(isTRUE(input$save_ok))
    req(nzchar(input$out_title))
    req(exp_dir())
    
    # Log save action and row count
    if (!is.null(logger())) {
      logger()$append_event(
        tab = input$page,
        action = "save_csv_clicked",
        details = list(
          out_title = input$out_title,
          save_ok = isTRUE(input$save_ok),
          rows = nrow(data_with_ct_ref())
        )
      )
    }
    
    # Write to exports/
    out_dir <- file.path(exp_dir(), "exports")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    safe_name <- paste0(gsub("[^A-Za-z0-9_-]+", "_", input$out_title), "_long")
    out_path <- file.path(out_dir, paste0(safe_name, ".csv"))
    
    write.csv(data_with_ct_ref(), out_path, row.names = FALSE)
    
    # Snapshot run context (inputs + QC + output paths + session package versions)
    if (!is.null(logger())) {
      in_list <- shiny::reactiveValuesToList(input)
      in_list$raw_files <- NULL
      
      # Drop DT (DataTables) widget state inputs to avoid massive repeated JSON
      dt_prefixes <- c(
        "data_preview", "sample_parse_preview", "qc_fail_preview",
        "ctref_preview", "prism_preview"
      )
      
      drop_names <- names(in_list)[
        vapply(names(in_list), function(nm) {
          any(startsWith(nm, paste0(dt_prefixes, "_")))
        }, logical(1))
      ]
      
      in_list[drop_names] <- NULL
      
      pkgs <- list(
        shiny = as.character(utils::packageVersion("shiny")),
        dplyr = as.character(utils::packageVersion("dplyr")),
        tidyr = as.character(utils::packageVersion("tidyr")),
        readxl = as.character(utils::packageVersion("readxl")),
        openxlsx = as.character(utils::packageVersion("openxlsx")),
        DT = as.character(utils::packageVersion("DT")),
        janitor = as.character(utils::packageVersion("janitor")),
        shinyjs = as.character(utils::packageVersion("shinyjs"))
      )
      
      logger()$snapshot_run(
        trigger = "save_csv",
        input_list = in_list,
        file_names = if (!is.null(input$raw_files)) input$raw_files$name else character(0),
        qc_table = if (!is.null(qc_results())) qc_results()$qc_table else NULL,
        qc_calc_table = if (!is.null(qc_results())) qc_results()$qc_calc_table else NULL,
        outputs = list(long_csv = out_path, rows = nrow(data_with_ct_ref())),
        session_pkgs = pkgs
      )
      
      logger()$append_event(
        tab = input$page,
        action = "snapshot_written",
        details = list(trigger = "save_csv", long_csv = out_path)
      )
    }
    
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
  
  # Tab 1 status messaging for basic gating visibility
  output$status_msg <- renderPrint({
    if (!nzchar(exp_name_safe())) "Enter an experiment name."
    else if (is.null(input$raw_files)) "Upload file(s) to begin."
    else if (!isTRUE(input$header_ok)) "Confirm the header row."
    else paste0("Ready. Experiment folder: ", exp_dir())
  })
  
  # Parsed column count drives whether Prism suggests splitting into separate graphs
  prism_n_parsed <- reactive({
    req(parsed_part_cols())
    length(parsed_part_cols())
  })
  
  # Prism grouping candidates exclude the replicate id column (ddct_id_col)
  prism_group_candidates <- reactive({
    req(parsed_part_cols(), input$ddct_id_col)
    setdiff(parsed_part_cols(), input$ddct_id_col)
  })
  
  # Prism setup helper text + detected parsed columns
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
  
  # Optional split var UI offered when >=3 parsed columns
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
  
  # Remaining candidates after removing split var (when used)
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
  
  # Primary Prism grouping variable selection (required for both grouped and column export)
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
  
  # Secondary grouping variable (grouped mode only; optional)
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
  
  # Row variable selector used for grouped table rows
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
  
  # Column mode note about file granularity and expected structure
  output$prism_column_note_ui <- renderUI({
    req(input$prism_table_type)
    if (input$prism_table_type != "column") return(NULL)
    helpText("Column mode exports ONE file per Target Name. Each file is a Prism Column table: columns = groups; rows = replicates; blanks are allowed.")
  })
  
  # Select which numeric value to export/plot in Prism
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
  
  # Replicate ID column selector used for arranging replicates and labeling rows
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
  
  # Build Prism grouped table (rows = row_var; columns = group|replicate; values = averaged per replicate)
  prism_make_grouped_table <- function(df) {
    req(input$prism_primary_group, input$prism_row_var, input$prism_value_var, input$prism_replicate_id)
    req(nzchar(input$prism_primary_group))
    
    col_primary <- input$prism_primary_group
    col_secondary <- if (!is.null(input$prism_secondary_group) && nzchar(input$prism_secondary_group)) input$prism_secondary_group else NULL
    
    row_var   <- input$prism_row_var
    value_var <- input$prism_value_var
    rep_id    <- input$prism_replicate_id
    
    # Build a column-group label (primary or primary|secondary)
    if (is.null(col_secondary)) {
      df2 <- df %>% dplyr::mutate(.col_group = as.character(.data[[col_primary]]))
    } else {
      df2 <- df %>% dplyr::mutate(.col_group = paste(
        as.character(.data[[col_primary]]),
        as.character(.data[[col_secondary]]),
        sep = " | "
      ))
    }
    
    # Collapse duplicates by averaging within (row_var, .col_group, rep_id)
    df3 <- df2 %>%
      dplyr::select(dplyr::all_of(row_var), .col_group, dplyr::all_of(rep_id), .value = dplyr::all_of(value_var)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(row_var, ".col_group", rep_id)))) %>%
      dplyr::summarise(.value = r3(mean(.value, na.rm = TRUE)), .groups = "drop")
    
    # Encode replicate IDs into column names for Prism grouped format
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
  
  # Build Prism column table (compact): rows = replicate slots; columns = groups; single Target Name per file
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
    
    # Collapse duplicates by averaging within (Group, ID)
    df2 <- df2 %>%
      dplyr::group_by(Group, ID) %>%
      dplyr::summarise(Value = r3(mean(Value, na.rm = TRUE)), .groups = "drop") %>%
      dplyr::arrange(Group, ID)
    
    # Re-index each Group into replicate slots and pivot to Prism column layout
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
  
  # Build Prism column table (expanded): rows are replicate-labeled; columns = groups; single Target Name per file
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
  
  # Assemble all Prism tables to preview/save:
  # - Grouped mode: one table per split level (or ALL)
  # - Column mode: one table per (split level × Target Name)
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
    
    # Column mode: build one table per Target Name (and per split, if used)
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
  
  # Prism preview shows the first generated table as a sanity check
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
  
  # Prism export save status text buffer
  prism_save_status_val <- reactiveVal("")
  output$prism_save_status <- renderText({ prism_save_status_val() })
  
  # Save Prism export(s) + write a logger snapshot capturing configuration and outputs
  observeEvent(input$save_prism, {
    tabs <- prism_tables()
    req(length(tabs) >= 1)
    req(isTRUE(input$prism_ok))
    req(nzchar(input$prism_title))
    req(exp_dir())
    
    # Log save configuration for Prism export
    if (!is.null(logger())) {
      logger()$append_event(
        tab = input$page,
        action = "save_prism_clicked",
        details = list(
          prism_title = input$prism_title,
          prism_ok = isTRUE(input$prism_ok),
          prism_table_type = input$prism_table_type,
          prism_column_layout = input$prism_column_layout %||% NA_character_,
          prism_split_var = input$prism_split_var %||% NA_character_,
          prism_primary_group = input$prism_primary_group,
          prism_secondary_group = input$prism_secondary_group %||% NA_character_,
          prism_row_var = input$prism_row_var %||% NA_character_,
          prism_value_var = input$prism_value_var,
          prism_replicate_id = input$prism_replicate_id
        )
      )
    }
    
    # Write to exports/
    out_dir <- file.path(exp_dir(), "exports")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Suffix describes organization choice (grouped vs column compact/expanded)
    layout_suffix <- if (input$prism_table_type == "grouped") {
      "_grouped"
    } else if (identical(input$prism_column_layout, "expanded")) {
      "_column_expanded"
    } else {
      "_column_compact"
    }
    
    # Base filename prefix shared across all written files for this export
    safe_title <- gsub("[^A-Za-z0-9_-]+", "_", input$prism_title)
    safe_base  <- paste0(safe_title, layout_suffix)
    
    paths <- character(0)
    
    # Write each table to its own CSV; include split/target tags in filename when needed
    for (nm in names(tabs)) {
      split_tag <- ""
      target_tag <- ""
      
      if (input$prism_table_type == "grouped") {
        split_tag <- if (nm == "ALL") "" else paste0("_", gsub("[^A-Za-z0-9_-]+", "_", nm))
      } else {
        parts <- strsplit(nm, "__TARGET__", fixed = TRUE)[[1]]
        split_label <- parts[1]
        target_name <- if (length(parts) >= 2) parts[2] else "UNKNOWN"
        
        split_tag  <- if (split_label == "ALL") "" else paste0("_", gsub("[^A-Za-z0-9_-]+", "_", split_label))
        target_tag <- paste0("_", gsub("[^A-Za-z0-9_-]+", "_", target_name))
      }
      
      out_path <- file.path(out_dir, paste0(safe_base, split_tag, "_prism", target_tag, ".csv"))
      write.csv(tabs[[nm]], out_path, row.names = FALSE)
      paths <- c(paths, out_path)
    }
    
    # Snapshot run context (inputs + QC + output paths + session package versions)
    if (!is.null(logger())) {
      in_list <- shiny::reactiveValuesToList(input)
      in_list$raw_files <- NULL
      
      # Drop DT (DataTables) widget state inputs to avoid massive repeated JSON
      dt_prefixes <- c(
        "data_preview", "sample_parse_preview", "qc_fail_preview",
        "ctref_preview", "prism_preview"
      )
      
      drop_names <- names(in_list)[
        vapply(names(in_list), function(nm) {
          any(startsWith(nm, paste0(dt_prefixes, "_")))
        }, logical(1))
      ]
      
      in_list[drop_names] <- NULL
      
      pkgs <- list(
        shiny = as.character(utils::packageVersion("shiny")),
        dplyr = as.character(utils::packageVersion("dplyr")),
        tidyr = as.character(utils::packageVersion("tidyr")),
        readxl = as.character(utils::packageVersion("readxl")),
        openxlsx = as.character(utils::packageVersion("openxlsx")),
        DT = as.character(utils::packageVersion("DT")),
        janitor = as.character(utils::packageVersion("janitor")),
        shinyjs = as.character(utils::packageVersion("shinyjs"))
      )
      
      logger()$snapshot_run(
        trigger = "save_prism",
        input_list = in_list,
        file_names = if (!is.null(input$raw_files)) input$raw_files$name else character(0),
        qc_table = if (!is.null(qc_results())) qc_results()$qc_table else NULL,
        outputs = list(prism_files = paths, prism_out_dir = out_dir),
        session_pkgs = pkgs
      )
      
      logger()$append_event(
        tab = input$page,
        action = "snapshot_written",
        details = list(trigger = "save_prism", prism_files_n = length(paths))
      )
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

