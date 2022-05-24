# app-3

fname <- '/Users/venkatasharatsripada/Documents/Masters@Syracuse/Course-Related(Study)/IST-719/art.csv'

artserver <- function(input, output){
  art <- read.csv(fname, header=T, stringsAsFactors = F)
  watercolor.col <- 'cadetblue1'
  drawing.col <- 'antiquewhite'
  
  # plotOutput('yearlyRecipts')
  
  output$yearlyRecipts <- renderPlot({
    print('Inside yearlyRecipts')
    my.title <- 'Number of sales per year'
    barplot(table(art$year), main=my.title, border='white'
            ,col='chartreuse4')
  })
  
  output$storePaper <- renderPlot({
    print('Inside storePaper')
    if (input$store != 'None') {
      print(paste('storePaper:: store:', input$store))
      sub.index <- which(art$store == input$store)
      tmp.data <- art[sub.index, ]
      pie(table(tmp.data$paper), col=c(watercolor.col,drawing.col)
          ,border = NA)
    }
  })
  
}

artui <- fluidPage(
  titlePanel('ACME Art company dashboard'),
  
  sidebarLayout(
    sidebarPanel(
      plotOutput('yearlyRecipts'), 
      selectInput('store', 'Select Store:'
                  ,choices=c('None','Portland','Davenport','Syracuse','Dublin'))
      ),
    mainPanel(
      plotOutput('storePaper')
  )
 )
)

shinyApp(ui=artui, server=artserver)