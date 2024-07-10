library(dplyr)
library(tidyr)
library(shiny)

ui <- fluidPage(
  fileInput("data_file", "Upload your claims data here. (Only .csv file is accepted.)", accept = ".csv"),
  numericInput("tail_factor", "Tail factor:", value = 1.1, min = 1.0),
  tableOutput("triangle")
)

server <- function(input, output, session) {
  
  validated_file <- reactive({
    req(input$data_file)
    ext <- tools::file_ext(input$data_file$datapath)
    validate(need(ext == "csv", "Please upload a csv file."))
    read.csv(input$data_file$datapath)
  })
  
  # Reactive expression to process the data
  processed_data <- reactive({
    data <- validated_file()
    data$Amount.of.Claims.Paid <- as.numeric(gsub(",", "", data$Amount.of.Claims.Paid))
  
  data <- data %>%
    group_by(Loss.Year) %>%
    arrange(Loss.Year, Development.Year) %>%
    mutate(Cumulative.Amount = cumsum(Amount.of.Claims.Paid)) %>%
    ungroup()
  
  triangle <- data %>%
    select(Loss.Year, Development.Year, Cumulative.Amount) %>%
    pivot_wider(names_from = Development.Year, values_from = Cumulative.Amount) %>%
    arrange(Loss.Year)
  
  # Convert the triangle to a data frame
  triangle <- as.data.frame(triangle)
  
  # Fill NA values dynamically based on provided rules
  DY <- ncol(triangle)
  AY <- nrow(triangle)
  
  for (i in 1:AY) {
    for (j in 2:DY) {
      if (is.na(triangle[i, j])) {
        factor <- sum(triangle[1:i-1, j], na.rm = TRUE) / sum(triangle[1:i-1, j - 1], na.rm = TRUE)
        triangle[i, j] <- triangle[i, j-1] * factor
      }
    }
  }
  
  if (input$tail_factor != 1) {
    for (i in 1:AY) {
      triangle[i, DY+1] <- triangle[i, DY] * input$tail_factor
      colnames(triangle)[DY+1] <- DY
    }
  }
  
  triangle
  })
  
  output$triangle <- renderTable({
    processed_data()
  })
  
}
shinyApp(ui, server)