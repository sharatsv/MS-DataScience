library(shiny)
library(leaflet) #raster
library(ggmap)

fname <- '/Users/venkatasharatsripada/Downloads/newyorklibraries.csv'
libs <- read.csv(fname, stringsAsFactors = F, header = T)

ny.libs <- nrow(libs)

server <- function(input, output, session) {
  print("server:: start")
  
  points <- eventReactive(input$num, lib, {
    index <- sample(1:nrow(libs), input$num.libs)
    addys <- paste(libs$ADDRESS[index]
                  ,libs$CITY[index], libs$STABR[index], sep=',')
    g.codes <- geocode(addys, source='disk')
    df <- data.frame(lon=g.codes$lon, lat=g.codes$lat, addy=addys)
    df
  }, ignoreNULL = FALSE)
  output$mymap <- renderLeaflet()


  output$mymap <- renderLeaflet({
    M <- leaflet()
    M < addProviderTiles(M, providers$OpenStreetMap
                         , options = providerTileOptions(noWrap = T))
    df <- points()
    addMarkers(M, lng=df[,1], lat=df[,2], popup=df[,3])
  })
}

ui <- fluidPage(
  leafletOutput('mymap'),
  numericInput('num.libs', "Number of libraries", 10, min = 1, max = ny.libs)
)

shinyApp(ui, server)

