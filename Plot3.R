
#loading packages

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
options(scipen=999)

#importing data

data <- tbl_df(read.table("./household_power_consumption.txt", sep = ";", header = TRUE))

#filtering for dates needed & transforming data ready to plot

data2 <- data %>% 
  filter(Date == '1/2/2007' | Date == '2/2/2007') %>%
  mutate(datetime = as.POSIXct(strptime(paste(Date, Time, sep=" "), "%d/%m/%Y %H:%M:%S"))) %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  mutate(Global_active_power = as.numeric(Global_active_power)) %>%
  mutate(Global_active_power = Global_active_power/1000) %>%
  mutate(Sub_metering_1 = as.numeric(Sub_metering_1)) %>% 
  mutate(Sub_metering_2 = as.numeric(Sub_metering_2)) %>%
  mutate(Sub_metering_3 = as.numeric(Sub_metering_3)) %>%
  mutate(Global_reactive_power = as.numeric(Global_reactive_power)) %>%
  mutate(Voltage = as.numeric(Voltage)) %>%
  mutate(Global_intensity = as.numeric(Global_intensity))


#Plot3

png("plot3.png", width=480, height=480)

data2 %>%
  gather(key, value, Sub_metering_1, Sub_metering_2, Sub_metering_3) %>%
  ggplot(aes(x= datetime, y = value, colour = key)) + 
  geom_line() +
  ylab("Enery sub-metering") +
  xlab("")+
  scale_x_datetime(date_breaks  = '1 day' , date_labels = "%a") 

dev.off()

