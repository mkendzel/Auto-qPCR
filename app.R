library(shiny)
library(readxl)
library(openxlsx)
library(dplyr)
library(DT)
library(janitor)
library(shinyjs)


library(shiny)
library(readxl)
library(openxlsx)
library(dplyr)
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
            "Does your individual sample have multiple GAPDH measurements?",
            choices = c("No" = "no", "Yes" = "yes"),
            selected = "no",
            inline = TRUE
          ),
          
          uiOutput("gapdh_group_cols_ui"),
          
          tags$hr(),
          selectInput(
            "ref_gene",
            "Select reference (normalization) gene",
            choices = NULL,
            multiple = FALSE
          ),
          
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
      
      # --- bottom save controls ---
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
    )
  )
)

## =========================
## SERVER
## =========================
server <- function(input, output, session) {
  
  # ------------------------------------------------------------
  # Global tab locks (only unlock when prerequisites met)
  # ------------------------------------------------------------
  observe({
    shinyjs::disable(selector = 'a[data-value="parse"]')
    shinyjs::disable(selector = 'a[data-value="review_match"]')
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
        fluidRow(
          column(
            6,
            textInput(
              paste0("part_label_", i),
              paste("Part", i, "label"),
              value = paste0("part", i)
            )
          ),
          column(
            6,
            selectInput(
              paste0("part_type_", i),
              paste("Part", i, "data type"),
              choices = c("Categorical" = "categorical", "Continuous numeric" = "numeric"),
              selected = "categorical"
            )
          )
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
  
  parsed_part_types <- reactive({
    req(input$expected_parts, parsed_part_cols())
    stats::setNames(
      vapply(seq_len(input$expected_parts), function(i) {
        t <- input[[paste0("part_type_", i)]]
        if (!is.null(t) && nzchar(t)) t else "categorical"
      }, character(1)),
      parsed_part_cols()
    )
  })
  
  # ---- live split (GATED: will not run until approved_data exists) ----
  split_df_live <- reactive({
    req(approved_data(), parsed_part_cols(), input$expected_parts)
    
    split_sample_name(
      df = approved_data(),
      n_parts = input$expected_parts,
      part_labels = parsed_part_cols(),
      keep_sample_name = FALSE
    )
  })
  
  # ---- typed split (GATED) ----
  split_df_typed <- reactive({
    req(split_df_live(), parsed_part_types())
    
    df <- split_df_live()
    
    for (col in names(parsed_part_types())) {
      if (parsed_part_types()[[col]] == "numeric") {
        df[[col]] <- as.numeric(df[[col]])
      } else {
        df[[col]] <- as.factor(df[[col]])
      }
    }
    
    df$CT <- as.numeric(df$CT)
    df$`Target Name` <- as.factor(df$`Target Name`)
    df
  })
  
  # preview table (GATED)
  output$sample_parse_preview <- renderDT({
    req(split_df_typed())
    DT::datatable(split_df_typed(), rownames = FALSE, options = list(scrollX = TRUE))
  })
  
  # ------------------------------------------------------------
  # Reference grouping + gene selection
  # ------------------------------------------------------------
  output$gapdh_group_cols_ui <- renderUI({
    req(parsed_part_cols())
    
    if (input$multi_gapdh == "yes") {
      selectInput(
        "gapdh_group_cols",
        "Grouping column(s) for GAPDH",
        choices = parsed_part_cols(),
        multiple = TRUE
      )
    } else {
      selectInput(
        "gapdh_group_cols",
        "Sample column for GAPDH",
        choices = parsed_part_cols(),
        multiple = FALSE
      )
    }
  })
  
  # populate reference gene choices dynamically (DO NOT fire on init)
  observeEvent(split_df_typed(), {
    updateSelectInput(
      session,
      "ref_gene",
      choices = sort(unique(as.character(split_df_typed()[["Target Name"]]))),
      selected = NULL
    )
  }, ignoreInit = TRUE)
  
  # ------------------------------------------------------------
  # Freeze parsed data + proceed
  # ------------------------------------------------------------
  observeEvent(input$continue_to_tab3, {
    validate(
      need(isTRUE(input$parse_ok), "Confirm before continuing."),
      need(!is.null(input$gapdh_group_cols) && length(input$gapdh_group_cols) >= 1,
           "Select sample grouping column(s)."),
      need(
        input$multi_gapdh == "no" || length(input$gapdh_group_cols) > 1,
        "Multiple GAPDH requires more than one grouping column."
      ),
      need(!is.null(input$ref_gene) && nzchar(input$ref_gene), "Select a reference gene.")
    )
    
    parsed_data(split_df_typed())
    shinyjs::enable(selector = 'a[data-value="review_match"]')
    updateTabsetPanel(session, "page", selected = "review_match")
  })
  
  # ------------------------------------------------------------
  # Create ct_ref column (core normalization prep)
  # ------------------------------------------------------------
  data_with_ct_ref <- eventReactive(input$continue_to_tab3, {
    req(parsed_data(), input$ref_gene, input$gapdh_group_cols)
    
    df <- parsed_data()
    
    ref_df <- df %>%
      dplyr::filter(`Target Name` == input$ref_gene) %>%
      dplyr::select(
        dplyr::all_of(input$gapdh_group_cols),
        ct_ref = CT
      )
    
    df %>%
      dplyr::left_join(ref_df, by = input$gapdh_group_cols) %>%
      dplyr::filter(`Target Name` != input$ref_gene)
  })
  
  # ------------------------------------------------------------
  # TAB 3) Final Long table preview + save
  # ------------------------------------------------------------
  # ------------------------------------------------------------
  # TAB 3) Final Long table preview + save
  # ------------------------------------------------------------
  
  output$ctref_preview <- renderDT({
    req(data_with_ct_ref())
    
    DT::datatable(
      data_with_ct_ref(),
      rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 10, lengthMenu = c(10, 25, 50, 100))
    )
  })
  
  save_status_val <- reactiveVal("")
  
  output$save_status <- renderText({
    save_status_val()
  })
  
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
}

shinyApp(ui, server)


