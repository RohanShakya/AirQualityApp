library(assertr)
library(dplyr)
x1=5
x2=6

data1 <- read.csv("C:/Users/Shyg-PC/Desktop/PM2.5_Ozone_2017_YTD.csv") 
data1
data1 %>% assertr::verify(nrow(.) == 10418)

#View(data1)
str(data1)
class(data1)
col1<- mapply(anyNA,data1)
col1

#na= not assigned, rm=remove
round(mean(data1$PM2.5,na.rm = T))

#Assign daily mean to NA values in PM2.5 and Ozone and AQI
data2 <- read.csv("C:/Users/Shyg-PC/Desktop/PM2.5_Ozone_2017_YTD.csv") 
str(data2)
#View(data2)
for (i in 1:nrow(data2)){
  if(is.na(data2[i,"PM2.5"])){
    data2[i,"PM2.5"]<- round(mean(data2[which(data2[,"Day"]==data2[i,"Day"]),"PM2.5"],na.rm = TRUE))
  }
  
  if(is.na(data2[i,"Ozone"])){
    data2[i,"Ozone"]<- round(mean(data2[which(data2[,"Day"]==data2[i,"Day"]),"Ozone"],na.rm = TRUE))
  }
  
  if(is.na(data2[i,"AQI"])){
    data2[i,"AQI"]<- round(mean(data2[which(data2[,"Day"]==data2[i,"Day"]),"AQI"],na.rm = TRUE))
  }
}
View(data2)

#adding a new column date
data2$date <- as.Date(with(data2, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
data2$date

library(ggplot2)
ggplot(data = data2, aes(x=data2$PM2.5, y=data2$Ozone)) + geom_point()
ggplot(data = data2, aes(x=data2$date, y=data2$PM2.5)) + geom_point(color = "#FC4E07")
ggplot(data = data2, aes(x=data2$date, y=data2$PM2.5)) + geom_line(color = "#FC4E07")
ggplot(data = data2, aes(x=data2$date, y=data2$Ozone)) + geom_point(color = "#00AFBB")
ggplot(data = data2, aes(x=data2$date, y=data2$AQI)) + geom_point(color = "#00AFBB")

p <- ggplot(data = data2, aes(x=data2$PM2.5, y=data2$AQI)) + geom_point(color = "purple")
p + labs(x="PM 2.5 (ug/m3)", y="AQI", title="PM 2.5 and AQI", caption="based on data from USembassy")

ggplot(data = data2, aes(x=data2$Ozone, y=data2$AQI)) + geom_point(color = "#00AFBB")

#Predicting AQI(dependent) using PM2.5(independent)
#Splitting data into training and test data
library(caTools)
split_index <- sample.split(data2$AQI, SplitRatio = 0.7)

train <- subset(data2, split_index == T)
test <- subset(data2, split_index == F)
nrow(train)
nrow(test)

#Model building
model1 <- lm(AQI ~ PM2.5, data = train)
summary(model1)

predict(model1, test) -> result
result
cbind(actualAQI= test$AQI, predictedAQI= result) -> compare_result
View(compare_result)
as.data.frame(compare_result) -> compare_result
compare_result$actualAQI - compare_result$predictedAQI -> error
cbind(compare_result, error) -> compare_result
View(compare_result)


compare_data <- cbind(PM2.5_test=test$PM2.5, compare_result)
View(compare_data)

compare_plot <- ggplot(data = compare_data, aes(x=compare_data$PM2.5_test)) 
compare_plot+ geom_point(aes(y= compare_data$actualAQI, color = "actualAQI"))
compare_plot+ geom_point(aes(y= compare_data$predictedAQI, color = "predictedAQI"))
compare_plot + labs(x="PM 2.5 (ug/m3)", y="AQI", title="PM 2.5 and AQI", caption="based on data from USembassy")

#calculating root mean square error
sqrt(mean(compare_result$error^2)) -> rmse1
rmse1

summary(model1)



######
for (i in 1:nrow(airquality)){
  if(is.na(airquality[i,"Ozone"])){
    airquality[i,"Ozone"]<- round(mean(airquality[which(airquality[,"Month"]==airquality[i,"Month"]),"Ozone"],na.rm = TRUE))
  }
}

library(dplyr)

plot(data1$PM2.5.Raw.Conc., type="l")
plot(data1$Ozone.Raw.Conc., type="l")
plot(data1$PM2.5.Raw.Conc., data1$Ozone.Raw.Conc.)
plot(data1$Month,data1$Ozone.Raw.Conc.)

View(airquality)
airquality

frequency(data1)
data(AirPassengers)
AirPassengers

