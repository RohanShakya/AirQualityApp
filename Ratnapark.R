library(shiny)
data1 <- read.csv("C:/Users/Shyg-PC/Desktop/Ratnapark, Kathmandu_PM2.5 Inst_2018-02-01_2018-03-28_Hourly.csv") 
data1
View(data1)
str(data1)
class(data1)

#Assigning mean value to NA valeus for Avg
for (i in 1:nrow(data1)){
  if(is.na(data1[i,"Avg"])){
    #data1[i,"Avg"]<- round(mean(data1[which(data1[,"Date"]==data1[i,"Date"]),"Avg"],na.rm = TRUE))
    data1[i,"Avg"]<- round(mean(data1$Avg,na.rm = T))
  }
}

dt1 <- paste(data1$Date, data1$Time)
dt1
class(dt1)
View(dt1)
date1 <- as.POSIXct(dt1, format = "%m/%d/%Y %H:%M")
class(date1)
View(date1)

library(ggplot2)
ggplot(data = data1, aes(x=date1, y=data1$Avg)) + geom_point(color = "#FC4E07")+labs(x="Date",y="PM2.5 concentration") + ggtitle("PM 2.5 Concentration in Ratnapark")

+scale_x_datetime(date_breaks = "10 day")