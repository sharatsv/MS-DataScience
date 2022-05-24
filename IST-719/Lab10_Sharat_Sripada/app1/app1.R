# app-1

library(shiny)
library(lubridate)

server <- function(input, output){
  
}

ui <- fluidPage(
  mainPanel(paste("Sharat's shiny app at", now())) #now comes from lubridate
)

shinyApp(ui, server)

