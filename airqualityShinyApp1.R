#Air quality shiny web app - final

library(shiny)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  pageWithSidebar(
    headerPanel("Air Quality Web App"),
    sidebarPanel(
      selectInput("Location", "Please Select a Location",
                  choices = c("Ratnapark","Pulchowk","Birendra School","Tribhuvan University")
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
                       dateRangeInput("daterange2", "Choose Date range:",
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
                  tabPanel(title = "Plot",plotlyOutput("myPlot")),
                  tabPanel(title = "Predict",plotlyOutput("myPredict"))
      )
      
    )
  )
  
)

server <- function(input, output) {
    output$myPlot <- renderPlotly({
    location = input$Location
    parameter = input$parameter
    #inputdate1 = input$daterange1
    
    
    if(location == "Ratnapark"){
      data1 <- read.csv("C:/Users/Shyg-PC/Desktop/RatnaparkPM10.csv") 
      
      #Table for Data tabpanel
      output$mytable <- renderTable(
        data1 
      )
      
      
      #To convert 'datetime in factor' to 'datetime in POSIXct
      library(lubridate)
      data1$datetime <- ymd_h(data1$datetime)
      
      #Plot for Predict tabpanel
      output$myPredict <- renderPlotly({
        library(caTools)
        split_index <- sample.split(data1$PM2.5, SplitRatio = 0.7)
        
        train <- subset(data1, split_index == T)
        test <- subset(data1, split_index == F)
        
        model1 <- lm(PM2.5 ~ PM10, data = train)
        predict(model1, test) -> result
        cbind(actual_PM2.5= test$PM2.5, predicted_PM2.5= result) -> compare_result
        
        as.data.frame(compare_result) -> compare_result
        compare_result$actual_PM2.5 - compare_result$predicted_PM2.5 -> error
        cbind(compare_result, error) -> compare_result
        compare_data <- cbind(PM10_test=test$PM10, compare_result)
        
        
        compare_plot <- plot_ly(compare_data, x = ~PM10_test, y = ~actual_PM2.5, type = 'scatter', name='actual PM2.5') %>%
          add_trace(y = ~predicted_PM2.5, mode = 'markers', name='predicted PM2.5',color = I('#f7920e')) %>%
          layout( title = "Actual vs Predicted PM 2.5 concentration",
                  xaxis = list(title= "PM 10 (ug/m3)"), 
                  yaxis = list(title="PM 2.5 (ug/m3)"),
                  annotations = 
                    list(x = 1, y = -0.1, text = "Data Source: Department of Environment", 
                         showarrow = F, xref='paper', yref='paper', 
                         xanchor='right', yanchor='auto', xshift=10, yshift=-10,
                         font=list(size=12, color="#22205b"))
          )
        compare_plot
        
        
      })
      
      reactiveMaster <- reactive({
        filter(data1,datetime>=input$daterange1[1] & datetime<input$daterange1[2])
      })
      
      if(parameter == 1){
        plot_ly(reactiveMaster(), x = ~datetime, y = ~PM10, type = 'scatter') %>%
          layout( title = "PM10 Concentration in Ratnapark-2018",
                  xaxis = list(title= "Date"), 
                  yaxis = list(title="PM 10 concentration (ug/m3)"),
                  annotations = 
                    list(x = 1, y = -0.1, text = "Data Source: Department of Environment", 
                         showarrow = F, xref='paper', yref='paper', 
                         xanchor='right', yanchor='auto', xshift=10, yshift=-10,
                         font=list(size=12, color="#22205b"))
          )
      }else if(parameter == 2){
        plot_ly(reactiveMaster(), x = ~datetime, y = ~PM2.5, type = 'scatter', color = I('#c64f75')) %>%
          layout( title = "PM2.5 Concentration in Ratnapark-2018",
                  xaxis = list(title= "Date"), 
                  yaxis = list(title="PM 2.5 concentration (ug/m3)"),
                  annotations = 
                    list(x = 1, y = -0.1, text = "Data Source: Department of Environment", 
                         showarrow = F, xref='paper', yref='paper', 
                         xanchor='right', yanchor='auto', xshift=10, yshift=-10,
                         font=list(size=12, color="#22205b"))
          )
      }
      
    }else if(location == "Pulchowk"){
      data2 <- read.csv("C:/Users/Shyg-PC/Desktop/PulchowkPM10_PM2.5.csv") 
      output$mytable <- renderTable(
        data2 
      )
      
      #To convert 'datetime in factor' to 'datetime in POSIXct
      library(lubridate)
      data2$datetime <- ymd_h(data2$datetime)
      
      reactiveMaster <- reactive({
        filter(data2,datetime>=input$daterange2[1] & datetime<input$daterange2[2])
      })
      
      if(parameter == 1){
        plot_ly(reactiveMaster(), x = ~datetime, y = ~PM10, type = 'scatter') %>%
          layout( title = "PM10 Concentration in Pulchowk-2018",
                  xaxis = list(title= "Date"), 
                  yaxis = list(title="PM 10 concentration (ug/m3)"),
                  annotations = 
                    list(x = 1, y = -0.1, text = "Data Source: Department of Environment", 
                         showarrow = F, xref='paper', yref='paper', 
                         xanchor='right', yanchor='auto', xshift=10, yshift=-10,
                         font=list(size=12, color="#22205b"))
          )
      }else if(parameter == 2){
        plot_ly(reactiveMaster(), x = ~datetime, y = ~PM2.5, type = 'scatter',color = I('#c64f75')) %>%
          layout( title = "PM2.5 Concentration in Pulchowk-2018",
                  xaxis = list(title= "Date"), 
                  yaxis = list(title="PM 2.5 concentration (ug/m3)"),
                  annotations = 
                    list(x = 1, y = -0.1, text = "Data Source: Department of Environment", 
                         showarrow = F, xref='paper', yref='paper', 
                         xanchor='right', yanchor='auto', xshift=10, yshift=-10,
                         font=list(size=12, color="#22205b"))
          )
      }
    }
    
    
  })
    
}

shinyApp(ui = ui, server = server)