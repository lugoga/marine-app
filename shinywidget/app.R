#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(shinydashboard)
require(shinyWidgets)
require(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

  dropdownButton(
    tags$h3("List of Input"),
    selectInput(inputId = 'xcol', label = 'X Variable', choices = names(iris)),
    selectInput(inputId = 'ycol', label = 'Y Variable', choices = names(iris), selected = names(iris)[[2]]),
    sliderInput(inputId = 'clusters', label = 'Cluster count', value = 3, min = 1, max = 9),
    circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
    tooltip = tooltipOptions(title = "Click to see inputs !"),
    plotOutput(outputId = "distPlot")
  ),
  
  sliderTextInput(
    inputId = "mySliderText", 
    label = "Your choice:", 
    grid = TRUE, 
    force_edges = TRUE,
    choices = c("Strongly disagree",
                "Disagree", "Neither agree nor disagree", 
                "Agree", "Strongly agree")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      
       iris %>% 
        ggplot(aes(x = input$xcol, y = input$ycol, color = Species))+
        geom_point()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
