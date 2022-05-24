# app-2

fname <- '/Users/venkatasharatsripada/Documents/Masters@Syracuse/Course-Related(Study)/IST-719/art.csv'

artserver <- function(input, output){
  art <- read.csv(fname, header=T, stringsAsFactors = F)
  plotOutput('yearlyRecipts')
  output$yearlyRecipts <- renderPlot({
    my.title <- 'Number of sales per year'
    barplot(table(art$year), main=my.title, border='white'
            ,col='chartreuse4')
  })
  
}

artui <- fluidPage(
  titlePanel('ACME Art company dashboard'),
  mainPanel(
    plotOutput('yearlyRecipts')
  )
)

shinyApp(ui=artui, server=artserver)