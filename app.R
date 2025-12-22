library(shiny)
library(readxl)
library(openxlsx)
library(dplyr)
library(DT)
library(janitor)


## UI ----
ui <- fluidPage(
  
  titlePanel("Auto-qPCR"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "combine_files",
        label   = "Do you have multiple qPCR files that should be combined?",
        choices = c(
          "No (single file)"     = FALSE,
          "Yes (multiple files)" = TRUE
        ),
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
      DTOutput("data_preview"),
      tags$hr(),
      verbatimTextOutput("status_msg")
    )
  )
)

server <- function(input, output, session) {
  
  # 1) Upload UI
  output$file_upload_ui <- renderUI({
    if (isTRUE(input$combine_files)) {
      fileInput(
        inputId  = "raw_files",
        label    = "Upload qPCR .xls files",
        multiple = TRUE,
        accept   = c(".xls")
      )
    } else {
      fileInput(
        inputId  = "raw_files",
        label    = "Upload a qPCR .xls file",
        multiple = FALSE,
        accept   = c(".xls")
      )
    }
  })
  
  # 2) Read + preprocess
  raw_dfs <- reactive({
    req(input$raw_files)
    
    files <- input$raw_files
    
    dfs <- lapply(files$datapath, function(path) {
      
      raw <- readxl::read_excel(
        path,
        sheet = "Results",
        col_names = FALSE
      )
      
      ########## HEADER DETECTION ##########
      header_row <- which(raw[[1]] == "Well")[1]
      
      df <- raw %>%
        slice(header_row:n())
      
      colnames(df) <- as.character(df[1, ])
      df <- df %>% slice(-1)
      ########## END HEADER DETECTION ##########
      
      df
    })
    
    names(dfs) <- files$name
    if (isTRUE(input$combine_files)) dfs else dfs[[1]]
  })
  
  # 3) Preview
  output$data_preview <- renderDT({
    req(raw_dfs())
    
    preview_df <- if (isTRUE(input$combine_files)) {
      raw_dfs()[[1]]
    } else {
      raw_dfs()
    }
    
    DT::datatable(
      head(preview_df, 20),
      options = list(scrollX = TRUE, pageLength = 20)
    )
  })
  
  # 4) Approved data store
  approved_data <- reactiveVal(NULL)
  
  observeEvent(input$continue_btn, {
    
    validate(
      need(!is.null(input$raw_files), "Upload file(s) first."),
      need(isTRUE(input$header_ok), "Confirm the header row before continuing.")
    )
    
    approved_data(raw_dfs())
    
    ########## NEXT STEPS START ##########
    # column validation
    # Ct numeric coercion
    # reference gene selection
    # deltaCt / deltaDeltaCt
    # multi-file merge logic
    ########## NEXT STEPS END ##########
  })
  
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
