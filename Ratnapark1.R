
data1 <- read.csv("C:/Users/Shyg-PC/Desktop/RatnaparkPM10.csv") 
View(data1)

str(data1)

library(dplyr)
data1 <- data1 %>% select(c(1,6,7))

library(lubridate)

#To convert 'datetime in factor' to 'datetime in POSIXct
data1$datetime <- ymd_h(data1$datetime)
class(data1$datetime)
str(data1)

library(ggplot2)
ggplot(data = data1, aes(x=data1$datetime, y=data1$PM10)) + 
  geom_point(color = "#FC4E07") +
  labs(x="Date",y="PM 10 concentration (ug/m3)") + 
  ggtitle("PM 10 Concentration in Ratnapark-2018")

ggplot(data = data1, aes(x=data1$datetime, y=data1$PM2.5)) + 
  geom_point(color = "#0e3f8e") +
  labs(x="Date",y="PM 2.5 concentration (ug/m3)") + 
  ggtitle("PM 2.5 Concentration in Ratnapark-2018")


ggplot(data1, aes(datetime)) + 
  geom_point(aes(y = PM10, colour = "PM10")) + 
  geom_point(aes(y = PM2.5, colour = "PM2.5")) +
  scale_colour_manual(values=c("#2533ce", "#2aed4b")) +
  labs(x="Date",y="Concentration (ug/m3)", caption="Data Source: Department of Environment") + 
  ggtitle("PM2.5  and PM10 Concentration in Ratnapark-2018")

ggplot(data = data1, aes(x=data1$PM10, y=data1$PM2.5)) + geom_point(color = "purple")


#Predicting PM2.5(dependent) using PM10(independent)
#Splitting data into training and test data
library(caTools)
split_index <- sample.split(data1$PM2.5, SplitRatio = 0.7)

train <- subset(data1, split_index == T)
test <- subset(data1, split_index == F)
nrow(train)
nrow(test)

#Model building
model1 <- lm(PM2.5 ~ PM10, data = train)
summary(model1)

predict(model1, test) -> result
result

p1<- predict(model1,data.frame("PM10" = 100))
p1
             
cbind(actual_PM2.5= test$PM2.5, predicted_PM2.5= result) -> compare_result
View(compare_result)
as.data.frame(compare_result) -> compare_result
compare_result$actual_PM2.5 - compare_result$predicted_PM2.5 -> error
cbind(compare_result, error) -> compare_result
View(compare_result)

#Binding PM10 to compare_result
compare_data <- cbind(PM10_test=test$PM10, compare_result)
View(compare_data)

compare_plot <- ggplot(data = compare_data, aes(x=compare_data$PM10_test)) + 
  geom_point(aes(y= compare_data$actual_PM2.5, color = "actual_PM2.5")) +
  geom_point(aes(y= compare_data$predicted_PM2.5, color = "predicted_PM2.5")) + 
  labs(x="PM 10 (ug/m3)", y="PM 2.5 (ug/m3)", caption="Data Source: Department of Environment") +
  ggtitle("Actual vs Predicted PM 2.5 concentration") +
  scale_colour_manual(values=c("#e03875", "#000000"))
compare_plot

#calculating root mean square error
sqrt(mean(compare_result$error^2)) -> rmse1
rmse1

summary(model1)


