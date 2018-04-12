library(tidyr)
library (dplyr)
library(plotly)
library(plyr)
library(readxl)
library(lubridate)
library(data.table)
library(tidyverse)
library(openair)

dataset1 <- read_excel('/Users/playsafe/Desktop/R/Sensor_Data.xlsx')
Appluimonia <- filter(dataset1, Chemical == "Appluimonia")
Appluimonia$Time <- format(as.POSIXct(Appluimonia$`Date Time`,format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")
Appluimonia$Date <- format(as.POSIXct(Appluimonia$`Date Time`,format="%Y:%m:%d %H:%M:%S"),"%Y:%m:%d")
Appluimonia <-within(Appluimonia, rm(`Date Time` ))
Appluimonia <-spread(Appluimonia, Monitor, Reading)
Appluimonia <- rename(Appluimonia, c("1" = "Sensor1", "2" = "Sensor2", 
                                           "3" = "Sensor3", "4" = "Sensor4", "5" = "Sensor5",
                                           "6" = "Sensor6","7" = "Sensor7","8" = "Sensor8",
                                           "9" = "Sensor9"))
Appluimonia <-within(Appluimonia, rm(Chemical,Time ))
Appluimonia <- setDT(Appluimonia)[, lapply(.SD, mean), by = Date]
Appluimonia$Date<-as.Date(parse_date_time(Appluimonia$Date,"%Y-%m-%d"))
Appluimonia$Chemical <- "Appluimonia"
FixingDate <- data.frame(date = Appluimonia$Date,
                         year = as.numeric(format(Appluimonia$Date, format = "%Y")),
                         month = as.numeric(format(Appluimonia$Date, format = "%m")),
                         day = as.numeric(format(Appluimonia$Date, format = "%d")))

Appluimonia <-cbind (Appluimonia,FixingDate$month)
colnames (Appluimonia)[colnames(Appluimonia) == 'V2'] <- 'Month'
Appluimonia <-cbind (Appluimonia,FixingDate$day)
colnames (Appluimonia)[colnames(Appluimonia) == 'V2'] <- 'Day'
Appluimonia$Month [Appluimonia$Month %in% "12"]  <- "December"
Appluimonia$Month [Appluimonia$Month %in% "8"]  <- "August"
Appluimonia$Month [Appluimonia$Month %in% "4"]  <- "April"
Appluimonia <-Appluimonia[do.call(order, Appluimonia), ]
Appluimonia[is.na(Appluimonia)] <- 0

AppluimoniaPlot <- plot_ly(Appluimonia, x = ~Day, y = ~Sensor1, text = ~paste("Chemical:", Chemical),frame=~Month, type = 'scatter', mode = 'markers', name = 'Sensor 1',fill = 'tozeroy') %>%
        add_trace(y = ~Sensor2, name = 'Sensor 2') %>%
        add_trace(y = ~Sensor3, name = 'Sensor 3') %>%
        add_trace(y = ~Sensor4, name = 'Sensor 4') %>%
        add_trace(y = ~Sensor5, name = 'Sensor 5') %>%
        add_trace(y = ~Sensor6, name = 'Sensor 6') %>%
        add_trace(y = ~Sensor7, name = 'Sensor 7') %>%
        add_trace(y = ~Sensor8, name = 'Sensor 8') %>%
        add_trace(y = ~Sensor9, name = 'Sensor 9') %>%
        layout(legend = list(x = 100, y = 0.25))

layout <- 
        list(
                title = "Appluimonia Chemical Data on All Sensors",
                xaxis = list(title = "Date"),
                yaxis = list(title = "Average Reading Per Day in PPM")
        )

AppluimoniaPlot <- layout(AppluimoniaPlot, title=layout$title, 
                             xaxis=layout$xaxis, yaxis=layout$yaxis)
AppluimoniaPlot

