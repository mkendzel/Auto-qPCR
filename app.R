library(shiny)
library(readxl)
library(openxlsx)
library(dplyr)
library(DT)
library(janitor)


#Place holder app
ui <- fluidPage(
  titlePanel("Auto-qPCR"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload qPCR file")
    ),
    mainPanel(
      verbatimTextOutput("info")
    )
  )
)

server <- function(input, output, session) {
  output$info <- renderPrint({
    input$file
  })
}

shinyApp(ui, server)