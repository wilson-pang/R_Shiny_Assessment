library(dplyr)
library(tidyr)
library(shiny)
library(ggplot2)
library(rsconnect)

ui <- fluidPage(
  
  titlePanel("Basic Chain Ladder"),
  
  sidebarLayout(
    
    sidebarPanel(
      textOutput("instructions"),
      imageOutput("image_instructions"),
      fileInput("data_file", "Upload your claims data here. (Only .csv file is accepted.)", accept = ".csv"),
      numericInput("tail_factor", "Tail factor:", value = 1.1, min = 1.0),
      width = 4
    ),
    
    mainPanel(
      tableOutput("triangle"),
      uiOutput("download_ui"),
      plotOutput("graph", width = "800px")
    )
  )
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
  
  output$instructions <- renderText({
    "Important !!! : Your .csv file should look like this. 
    The names of headers MUST be exactly the same as in the image provided. 
    The cell position of the data MUST be exactly the same as in the image provided."
  })
  
  output$image_instructions <- renderImage({
    list(src = "sample_csv.jpg", 
         contentType = "image/jpeg", 
         width = "350px",
         deleteFile = FALSE)}, deleteFile = FALSE
  )
  
  output$triangle <- renderTable({
    processed_data()
  })
  
  output$download_ui <- renderUI({
    req(input$data_file)
    downloadButton("download_button", "Download as Excel here", icon = shiny::icon("download"))
  })
  
  output$download_button <- downloadHandler(
    filename = function() {
      "basic chain ladder.csv"
    },
    content = function(file) {
      write.csv(processed_data(), file)
    }
  )
  
  output$graph <- renderPlot({
    processed_data() %>%
      pivot_longer(cols = -Loss.Year, names_to = "Development.Year", values_to = "Cumulative.Amount") %>%
      mutate(Development.Year = as.numeric(Development.Year)) %>%
      ggplot(aes(x = Development.Year, y = Cumulative.Amount, color = factor(Loss.Year))) +
      geom_point() +
      geom_text(aes(label = round(Cumulative.Amount, 0)), vjust = -1.0) +
      geom_smooth() +
      labs(x = "Development Year", y = "Cumulative Paid Claims", color = "Loss Year") +
      theme_bw()
  })
}

shinyApp(ui, server)
# deployApp()
# Successfully deployed to <https://wilson-pang.shinyapps.io/app1/>