library(shiny)
library(ggplot2)
library(dplyr)

# Load the diamonds dataset
data("diamonds")

# Define UI
ui <- fluidPage(
  titlePanel("Diamonds Explorer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("carat", "Carat", min = 0.01, max = 5.01, value = c(0.01, 5.01), step = 0.01),
      checkboxGroupInput("clarity", "Clarity", choices = c("IF", "VVS1", "VVS2", "VS1", "VS2", "SI1", "SI2", "I1")),
      checkboxGroupInput("color", "Color", choices = c("D", "E", "F", "G", "H", "I", "J")),
      checkboxGroupInput("cut", "Cut", choices = c("Ideal", "Premium", "Very Good", "Good", "Fair")),
    ),
    mainPanel(
      plotOutput("histogram")
    )
  )
)

# Define server
server <- function(input, output) {
  # Filter the diamonds dataset based on the inputs
  filteredData <- reactive({
    diamonds %>%
      filter(carat >= input$carat[1] & carat <= input$carat[2] &
               clarity %in% input$clarity &
               color %in% input$color &
               cut %in% input$cut)
  })
  
  # Generate the histogram based on the filtered data
  output$histogram <- renderPlot({
    ggplot(filteredData(), aes(x = price)) +
      geom_histogram(binwidth = 1000) +
      labs(title = "Histogram of Price",
           x = "Price",
           y = "Count")
  })
}

# Run the application
shinyApp(ui = ui, server = server)