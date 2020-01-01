# 03-reactive

library(shiny)
library(quantmod)
library(plotly)
library(dplyr)

data1 <- read.csv("C:/Users/Shyg-PC/Desktop/RatnaparkPM10.csv") 
library(lubridate)

#To convert 'datetime in factor' to 'datetime in POSIXct
data1$datetime <- ymd_h(data1$datetime)

ui <- fluidPage(
  pageWithSidebar(
    headerPanel("Practice1"),
    sidebarPanel(
      selectInput("Location", "Please Select a Location",
                  choices = c("Ratnapark","Pulchowk")
      ),
     dateRangeInput("daterange1", "Choose Date range:",
                    start  = "2018-02-01",
                    end    = "2018-03-29",
                    min    = "2018-02-01",
                    max    = "2018/03/30",
                    format = "M dd, yyyy",
                    separator = " to "
      ),
      textOutput("range")
      
    ),
   
    mainPanel(
      plotlyOutput("myPlot")
    )
   
  )
  
)

server <- function(input, output) {
  output$range <-renderText({ paste(input$daterange1[1], "to", input$daterange1[2] ) })
  
  #output$myPlot <- renderPlotly({
   # plot_ly(data1, x = ~datetime, y = ~PM10, type = 'scatter')
    
  #})
 
  
  reactiveMaster <- reactive({
    filter(data1,datetime>=input$daterange1[1] & datetime<input$daterange1[2])
  })
  
  output$myPlot <- renderPlotly({
    plot_ly(reactiveMaster(), x = ~datetime, y = ~PM10, type = 'scatter') %>%
      layout( title = "PM10 Concentration in Ratnapark-2018",
              xaxis = list(title= "Date"), 
              yaxis = list(title="PM 10 concentration (ug/m3)")
              )
    
  })
  #output$myPlot <- renderPlotly({
   # plot_ly(reactiveMaster(), x = data1$datetime, y = data1$PM10, name = 'One Arrival',  visible = "legendonly") %>%
    #  add_trace(type="scatter",x= data1$datetime, y = data1$PM10, name = 'Two Arrival', mode = 'lines', visible = "legendonly" ) %>%
     # layout( xaxis = list(title= "Date and Time",
      #                     rangeslider = list (type = "date"))) 
  #})
  
 
  
  
}

shinyApp(ui = ui, server = server)



