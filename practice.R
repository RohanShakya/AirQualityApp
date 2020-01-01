# 03-reactive

library(shiny)
library(quantmod)

data1 <- read.csv("C:/Users/Shyg-PC/Desktop/Ratnapark, Kathmandu_PM2.5 Inst_2018-02-01_2018-03-28_Hourly.csv") 

#Assigning mean value to NA valeus for Avg
for (i in 1:nrow(data1)){
  if(is.na(data1[i,"Avg"])){
    data1[i,"Avg"]<- round(mean(data1$Avg,na.rm = T))
  }
}

dt1 <- paste(data1$Date, data1$Time)
date1 <- as.POSIXct(dt1, format = "%m/%d/%Y %H:%M")

ui <- fluidPage(
  pageWithSidebar(
    headerPanel("Practice"),
    sidebarPanel(
      selectInput("Location", "Please Select a Location",
                  choices = c("Ratnapark","Pulchowk")
      ),
      
      conditionalPanel(condition = "input.Location=='Ratnapark'",
                       dateRangeInput("daterange1", "Choose Date range:",
                                      start  = "2018-02-01",
                                      end    = "2018-03-29",
                                      min    = "2018-02-01",
                                      max    = "2018/03/30",
                                      format = "M dd, yyyy",
                                      separator = " to ")
      )
      
    ),
    mainPanel(
      plotOutput("myPlot")
    )
  )
  
)

server <- function(input, output) {
  
  dataInput <- reactive({
    getSymbols(
               from = input$daterange1[1],
               to = input$daterange1[2],
               src=data1,
               auto.assign = FALSE)
              
  })
  
  output$myPlot <- renderPlot({
    
    chartSeries(dataInput(), theme = chartTheme("white"), 
                type = "line", log.scale = input$log, TA = NULL)
  })
  
  
    library(ggplot2)
    #ggplot(data = data1, aes(x=date1, y=data1$Avg)) + geom_point(color = "#FC4E07")+labs(x="Date",y="PM2.5 concentration") + ggtitle("PM 2.5 Concentration in Ratnapark")
    
}

shinyApp(ui = ui, server = server)