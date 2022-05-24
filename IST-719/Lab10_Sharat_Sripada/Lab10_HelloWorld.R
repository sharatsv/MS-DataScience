# Lab-10: Shiny package /Interactive dashboards
library(shiny)

server <- function(input,output) {
  output$myPie <- renderPlot({
    pie(c(8,12,3), main='Hello World!')
    })
}

ui <- fluidPage(
  mainPanel(plotOutput('myPie'))
)

shinyApp(ui, server)

