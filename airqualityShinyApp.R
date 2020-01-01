#Air quality shiny web app - initial


library(shiny)
library(ggplot2)

ui <- fluidPage(
  pageWithSidebar(
    headerPanel("Air Quality Web App"),
    sidebarPanel(
      selectInput("Location", "Please Select a Location",
                  choices = c("Ratnapark","Pulchowk")
                  ),
      radioButtons("parameter","Select Parameter",
                   choices = list("PM 10" = 1, "PM 2.5" = 2),
                                  selected = 1),
      
      conditionalPanel(condition = "input.Location=='Ratnapark'",
                       dateRangeInput("daterange1", "Choose Date range:",
                                      start  = "2018-02-01",
                                      end    = "2018-03-29",
                                      min    = "2018-02-01",
                                      max    = "2018/03/30",
                                      format = "M dd, yyyy",
                                      separator = " to ")
                       ),
      conditionalPanel(condition = "input.Location=='Pulchowk'",
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
      tabsetPanel(type="tab",
                  tabPanel(title = "Data", tableOutput("mytable")),
                  tabPanel(title = "Plot",plotOutput("myPlot"))
                  )
      
    )
  )
  
)

server <- function(input, output) {

  output$myPlot <- renderPlot({
    location = input$Location
    parameter = input$parameter
    inputdate1 = input$daterange1
    
    if(location == "Ratnapark"){
      data1 <- read.csv("C:/Users/Shyg-PC/Desktop/RatnaparkPM10.csv") 
      
      output$mytable <- renderTable(
        data1 
      )
      
      library(lubridate)
      
      #To convert 'datetime in factor' to 'datetime in POSIXct
      data1$datetime <- ymd_h(data1$datetime)
      
      if(parameter == 1){
        ggplot(data = data1, aes(x=data1$datetime, y=data1$PM10)) + 
          geom_point(color = "#FC4E07") +
          labs(x="Date",y="PM 10 concentration (ug/m3)", caption="Data Source: Department of Environment") + 
          ggtitle("PM 10 Concentration in Ratnapark-2018")
        
      }else if(parameter == 2){
        ggplot(data = data1, aes(x=data1$datetime, y=data1$PM2.5)) + 
          geom_point(color = "#0842a0") +
          labs(x="Date",y="PM 2.5 concentration (ug/m3)", caption="Data Source: Department of Environment") + 
          ggtitle("PM 2.5 Concentration in Ratnapark-2018")
      }
      
      
    }else if(location == "Pulchowk"){
      data2 <- read.csv("C:/Users/Shyg-PC/Desktop/PulchowkPM10_PM2.5.csv") 
      
      output$mytable <- renderTable(
        data2 
      )
      
      library(lubridate)
      
      #To convert 'datetime in factor' to 'datetime in POSIXct
      data2$datetime <- ymd_h(data2$datetime)
      
      if(parameter == 1){
        ggplot(data = data2, aes(x=data2$datetime, y=data2$PM10)) + 
          geom_point(color = "#FC4E07") +
          labs(x="Date",y="PM 10 concentration (ug/m3)", caption="Data Source: Department of Environment") + 
          ggtitle("PM 10 Concentration in Pulchowk-2018")
      }else if(parameter == 2){
        ggplot(data = data2, aes(x=data2$datetime, y=data2$PM2.5)) + 
          geom_point(color = "#0842a0") +
          labs(x="Date",y="PM 2.5 concentration (ug/m3)", caption="Data Source: Department of Environment") + 
          ggtitle("PM 2.5 Concentration in Pulchowk-2018")
      }
      
    }
    
  })
}

shinyApp(ui = ui, server = server)