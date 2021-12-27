library(shinydashboard)
library(shiny)
library(shinyWidgets) # needed if using shinydashboard items outside of shinydashboard

my_value <- 12

ui <- fluidPage(
  
  useShinydashboard(), # added this
  
  fluidRow (
    HTML("<h3><center>Here is my App</center></h3>")
  ),
  fluidRow(
    infoBoxOutput(outputId = "chl", width = 3),
    infoBoxOutput(outputId = "sst", width = 3),
    infoBoxOutput(outputId = "wind", width = 3),
    infoBoxOutput(outputId = "rain", width = 3),
    infoBoxOutput(outputId = "visibility", width = 3),
    infoBoxOutput(outputId = "waves", width = 3),
    infoBoxOutput(outputId = "current", width = 3),
    infoBoxOutput(outputId = "oxygen", width = 3) ,
    infoBoxOutput(outputId = "air", width = 3),
    infoBoxOutput(outputId = "water", width = 3),
    infoBoxOutput(outputId = "turbidity", width = 3),
    infoBoxOutput(outputId = "soil", width = 3)
    
    
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  output$chl <- renderInfoBox({
    infoBox(title = HTML("Chl<br>"),
            value = HTML("<p style='font-size:40px'>",
                         0.24,"</p>"),
            color = "green",
            icon = shiny::icon(name = "cloud-sun"),
            fill = TRUE
    )
  })
  
  output$sst = renderInfoBox({
    infoBox(title = HTML("SST<br>"),
            value = HTML("<p style='font-size:50px'>",
                         25,"</p>"),
            color = "maroon", 
            icon = shiny::icon(name = "temperature-high"),
            fill = TRUE)
  })
  
  output$wind = renderInfoBox({
    infoBox(title = HTML("Wind<br>"),
            value = HTML("<p style='font-size:50px'>",
                         12,"</p>"),
            color = "blue",
            icon = shiny::icon(name = "wind"),
            fill = TRUE)
  })
  
  output$rain = renderInfoBox({
    infoBox(title = HTML("Rain<br>"),
            value = HTML("<p style='font-size:50px'>",
                         180,"</p>"),
            color = "yellow",
            icon = shiny::icon(name = "cloud-shower-heavy"),
            fill = TRUE)
  })
  
  output$visibility = renderInfoBox({
    infoBox(title = HTML("Visibility<br>"),
            value = HTML("<p style='font-size:50px'>",
                         80,"</p>"),
            color = "aqua",
            icon = shiny::icon(name = "sun"),
            fill = TRUE)
  })
  
  
  output$waves = renderInfoBox({
    infoBox(title = HTML("Wave<br>"),
            value = HTML("<p style='font-size:50px'>",
                         0.2,"</p>"),
            color = "navy",
            icon = shiny::icon(name = "meteor"),
            fill = TRUE)
  })
  
  output$current = renderInfoBox({
    infoBox(title = HTML("Current<br>"),
            value = HTML("<p style='font-size:50px'>",
                         0.24,"</p>"),
            color = "orange",
            icon = shiny::icon(name = "poo-storm"),
            fill = TRUE)
  })
  
  output$oxygen = renderInfoBox({
    infoBox(title = HTML("Oxygen<br>"),
            value = HTML("<p style='font-size:50px'>",
                         4.25,"</p>"),
            color = "teal",
            icon = shiny::icon(name = "snowflake"),
            fill = TRUE)
  })
  
    output$soil = renderInfoBox({
      infoBox(title = HTML("Soil<br>"),
              value = HTML("<p style='font-size:50px'>",
                           80,"</p>"),
              color = "olive",
              icon = shiny::icon(name = "water"),
              fill = TRUE)
    })
    
#
  
  
  output$air = renderInfoBox({
    infoBox(title = HTML("Air<br>"),
            value = HTML("<p style='font-size:50px'>",
                         0.2,"</p>"),
            color = "lime",
            icon = shiny::icon(name = "bolt"),
            fill = TRUE)
  })
  
  output$water = renderInfoBox({
    infoBox(title = HTML("Water<br>"),
            value = HTML("<p style='font-size:50px'>",
                         0.24,"</p>"),
            color = "purple",
            icon = shiny::icon(name = "umbrella"),
            fill = TRUE)
  })
  
  output$turbidity = renderInfoBox({
    infoBox(title = HTML("Turbidity<br>"),
            value = HTML("<p style='font-size:50px'>",
                         4.25,"</p>"),
            color = "fuchsia",
            icon = shiny::icon(name = "smog"),
            fill = TRUE)
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)