library(shiny)
library(readxl)
library(openxlsx)
library(dplyr)
library(DT)
library(janitor)
library(shinyjs)


## UI ----
ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Auto-qPCR"),
  
  tabsetPanel(
    id = "page",
    
    # Page 1
    tabPanel(
      title = "1) Import",
      value = "import",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = "combine_files",
            label   = "Do you have multiple qPCR files that should be combined?",
            choices = c("No (single file)" = FALSE, "Yes (multiple files)" = TRUE),
            selected = FALSE
          ),
          uiOutput("file_upload_ui"),
          tags$hr(),
          checkboxInput(
            inputId = "header_ok",
            label   = "I confirm the header row and first data row look correct.",
            value   = FALSE
          ),
          actionButton("continue_btn", "Continue", class = "btn-primary")
        ),
        mainPanel(
          h4("Data Preview"),
          radioButtons(
            inputId = "preview_table",
            label   = "Preview table",
            choices = c("Main data (NTC removed)" = "main", "NTC rows only" = "ntc"),
            selected = "main",
            inline   = TRUE
          ),
          DTOutput("data_preview"),
          tags$hr(),
          verbatimTextOutput("status_msg")
        )
      )
    ),
    
    # Page 2
    tabPanel(
      title = "2) Parse Sample Names",
      value = "parse",
      h4("Sample Name parsing"),
      
      fluidRow(
        column(
          6,
          helpText("Expected format: 1_w_x_y ... (underscore-delimited)."),
          numericInput(
            inputId = "expected_parts",
            label   = "How many underscore-delimited parts should there be?",
            value   = 4,
            min     = 2
          ),
          tags$hr(),
          uiOutput("part_labels_ui")
        ),
        column(
          6,
          h5("Parsed preview (updates live)"),
          DTOutput("sample_parse_preview")
        )
      ),
      
      tags$hr(),
      verbatimTextOutput("parse_status")
    )
  )
)

# SERVER ----
server <- function(input, output, session) {
  
  # Disable tab 2 until Continue succeeds
  observe({
    shinyjs::disable(selector = 'a[data-value="parse"]')
  })
  
  # Upload UI
  output$file_upload_ui <- renderUI({
    if (identical(input$combine_files, "TRUE")) {
      fileInput("raw_files", "Upload qPCR .xls files", multiple = TRUE, accept = c(".xls"))
    } else {
      fileInput("raw_files", "Upload a qPCR .xls file", multiple = FALSE, accept = c(".xls"))
    }
  })
  
  # Preprocess
  qpcr <- reactive({
    req(input$raw_files)
    
    preprocess_qpcr_files(
      files = input$raw_files,
      combine_multiple = identical(input$combine_files, "TRUE")
    )
  })
  
  # Page 1 preview
  output$data_preview <- renderDT({
    req(qpcr())
    
    preview_df <- if (identical(input$preview_table, "ntc")) qpcr()$ntc else qpcr()$main
    
    DT::datatable(
      preview_df,
      options = list(
        scrollY = "400px",
        scrollX = TRUE,
        pageLength = 10,
        lengthMenu = c(10, 25, 50)
      )
    )
  })
  
  # Approved store
  approved_data <- reactiveVal(NULL)
  
  observeEvent(input$continue_btn, {
    validate(
      need(!is.null(input$raw_files), "Upload file(s) first."),
      need(isTRUE(input$header_ok), "Confirm the header row before continuing.")
    )
    
    approved_data(list(
      data = qpcr()$main,
      ntc  = qpcr()$ntc
    ))
    
    shinyjs::enable(selector = 'a[data-value="parse"]')
    updateTabsetPanel(session, "page", selected = "parse")
  })
  
  # Page 2: dynamic label inputs
  output$part_labels_ui <- renderUI({
    req(input$expected_parts)
    k <- input$expected_parts
    
    tagList(
      lapply(1:k, function(i) {
        textInput(
          inputId = paste0("part_label_", i),
          label   = paste0("Part ", i, " label"),
          value   = paste0("part", i)
        )
      })
    )
  })
  
  # Page 2: live split df (calls helper)
  split_df_live <- reactive({
    req(approved_data(), input$expected_parts)
    
    k <- input$expected_parts
    
    part_labels <- vapply(
      1:k,
      function(i) {
        lbl <- input[[paste0("part_label_", i)]]
        if (!is.null(lbl) && nzchar(lbl)) lbl else paste0("part", i)
      },
      character(1)
    )
    
    split_sample_name(
      df = approved_data()$data,
      n_parts = k,
      part_labels = part_labels,
      keep_sample_name = FALSE
    )
  })
  
  # Render table
  output$sample_parse_preview <- renderDT({
    req(split_df_live())
    
    DT::datatable(
      split_df_live(),
      rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 25, lengthMenu = c(25, 50, 100))
    )
  })
  
  output$parse_status <- renderPrint({
    req(split_df_live())
    paste("Rows in preview:", nrow(split_df_live()))
  })
  
  # Status message
  output$status_msg <- renderPrint({
    if (is.null(input$raw_files)) {
      "Upload file(s) to begin."
    } else if (!isTRUE(input$header_ok)) {
      "Review the preview and confirm the header row."
    } else if (is.null(approved_data())) {
      "Ready. Click Continue."
    } else {
      "Header confirmed. Proceeding to analysis."
    }
  })
}

shinyApp(ui, server)
