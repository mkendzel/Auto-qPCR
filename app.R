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
      
      radioButtons(
        inputId = "preview_table",
        label   = "Preview table",
        choices = c(
          "Main data (NTC removed)" = "main",
          "NTC rows only"           = "ntc"
        ),
        selected = "main",
        inline   = TRUE
      ),
      
      DTOutput("data_preview"),
      
      tags$hr(),
      
      verbatimTextOutput("status_msg")
    )
  )
)

server <- function(input, output, session) {
  
  # 1) Upload UI
  output$file_upload_ui <- renderUI({
    if (identical(input$combine_files, "TRUE")) {
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
  
  # 2) Read + preprocess (always returns a LIST of dfs)
  raw_dfs <- reactive({
    req(input$raw_files)
    
    files <- input$raw_files
    
    dfs <- lapply(seq_len(nrow(files)), function(i) {
      path <- files$datapath[i]
      nm   <- files$name[i]
      
      raw <- readxl::read_excel(
        path,
        sheet = "Results",
        col_names = FALSE
      )
      
      # HEADER DETECTION (per file)
      header_row <- which(raw[[1]] == "Well")[1]
      
      df <- raw %>%
        dplyr::slice(header_row:n()) %>%
        as.data.frame()
      
      colnames(df) <- as.character(df[1, ])
      df <- df %>% dplyr::slice(-1)
      
      # optional: keep track of which file a row came from
      df$source_file <- nm
      
      df
    })
    
    names(dfs) <- files$name
    dfs
  })
  
  # 2b) Combine rows across files when multiple selected
  combined_df <- reactive({
    req(raw_dfs())
    
    if (identical(input$combine_files, "TRUE")) {
      dplyr::bind_rows(raw_dfs())
    } else {
      raw_dfs()[[1]]
    }
  })
  
  # Separate out ntc
  
  ntc_df <- reactive({
    req(combined_df())
    
    combined_df() %>%
      dplyr::filter(tolower(trimws(`Sample Name`)) == "ntc")
  })
  
  df_no_ntc <- reactive({
    req(combined_df())
    
    combined_df() %>%
      dplyr::filter(tolower(trimws(`Sample Name`)) != "ntc")
  })
  
  # Change "Undetermined" Ct to 40
  df_no_ntc <- reactive({
    req(combined_df())
    
    combined_df() |>
      dplyr::filter(tolower(trimws(`Sample Name`)) != "ntc") |>
      dplyr::mutate(
        CT = dplyr::if_else(
          tolower(trimws(as.character(CT))) == "undetermined",
          40,
          as.numeric(CT)
        )
      )
  })
  


  # 3) Preview renderDT
  output$data_preview <- renderDT({
    req(combined_df())
    
    preview_df <- if (identical(input$preview_table, "ntc")) {
      ntc_df()
    } else {
      df_no_ntc()
    }
    
    DT::datatable(
      preview_df,
      options = list(
        scrollY = "400px",
        scrollX = TRUE,
        pageLength = 50,
        lengthMenu = c(25, 50, 100)
      )
    )
  })
  
  
  # 4) Approved data store
  approved_data <- reactiveVal(NULL)
  
  observeEvent(input$continue_btn, {
    validate(
      need(!is.null(input$raw_files), "Upload file(s) first."),
      need(isTRUE(input$header_ok), "Confirm the header row before continuing.")
    )
    
    approved_data(list(
      data = df_no_ntc(),
      ntc  = ntc_df()
    ))
  })
  
  # 5) Status message
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
