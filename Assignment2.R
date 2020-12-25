
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
ourdata<-read_csv2("assignment2_data.csv")
ourdata

#1
set.seed(2016555014)
newdf<-ourdata[sample(1:nrow(ourdata),100,replace=TRUE), ]
newdf


#2
g_color<-group_by(newdf,Color)%>%
  summarize(color_mean=mean(Sales,na.rm = TRUE))
g_color


#3 
g_region<-group_by(newdf,Region)%>%
  summarize(region_mean=mean(Sales,na.rm = TRUE))
g_region

#4 

max_date<-group_by(newdf,Date)%>%
  summarize(max_sales_date=max(Sales))
max_date
head(max_date[order(-max_date$max_sales_date),],5)


max_color<-newdf%>%group_by(Color)%>%
  summarize(max_sales_color=max(Sales))
max_color
head(max_color[order(-max_color$max_sales_color),],5)


max_region<-group_by(newdf,Region)%>%
  summarize(max_sales_region=max(Sales))
max_region
head(max_region[order(-max_region$max_sales_region),],5)




#5


date_time <- dmy(newdf$Date)
month_Time <- month(date_time, label = TRUE, abbr = TRUE)
month_Sales <- summarize(newdf, month_Time, Sales)
by_month <- group_by(month_Sales, month_Time)
result <- summarize(by_month, Mean_sales = mean(Sales, na.rm = TRUE))
high_sales <- arrange(result, desc(Mean_sales))
head(high_sales)


#6 


Year <- year(date_time)
year_sales<-newdf%>%summarise(Year,Sales)

ggplot(data = year_sales) +
  geom_bar(mapping = aes(x = Year, fill = Sales ))













