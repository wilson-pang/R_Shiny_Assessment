library(dplyr)
library(tidyr)

data <- read.csv("C:\\Users\\mingw\\Nicholas Actuarial Solutions Sdn Bhd\\Intern - General\\Wilson\\GitHub for R\\R_Shiny_Assessment\\Test_data.csv")
data$Amount.of.Claims.Paid <- as.numeric(gsub(",", "", data$Amount.of.Claims.Paid))

tail_factor <- 1.1

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

if (tail_factor != 1) {
  for (i in 1:AY) {
    triangle[i, DY+1] <- triangle[i, DY] * tail_factor
    colnames(triangle)[DY+1] <- DY
  }
}

library(shiny)

ui <- fluidPage(
  fileInput("data_file", "Upload your claims data here."),
  numericInput("tail_factor", "Tail factor:", value = 1.1, min = 1.0),
  tableOutput("triangle", accept = ".csv")
)

server <- function(input, output, session) {
  
  file <- reactive({
    get(input$data_file)
  })
  ext <- tools::file_ext(file$datapath)
  
  req(file)
  validate(need(ext == "csv", "Please upload a csv file."))
  
  validated_file <- reactive({
    read.csv(file$datapath)
  })
  
  output$dvp_triangle <- renderTable({
        
  })
  
}
