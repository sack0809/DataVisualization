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

Chlorodinine <- filter(dataset1, Chemical == "Chlorodinine")
Methylosmolene <- filter(dataset1, Chemical == "Methylosmolene")
Appluimonia <- filter(dataset1, Chemical == "Appluimonia")
AGOC <- filter(dataset1, Chemical == "AGOC-3A")
Chlorodinine$Time <- format(as.POSIXct(Chlorodinine$`Date Time`,format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")
Chlorodinine$Date <- format(as.POSIXct(Chlorodinine$`Date Time`,format="%Y:%m:%d %H:%M:%S"),"%Y:%m:%d")
Chlorodinine <-within(Chlorodinine, rm(`Date Time` ))
Chlorodinine <-spread(Chlorodinine, Monitor, Reading)
Chlorodinine <- rename(Chlorodinine, c("1" = "Sensor1", "2" = "Sensor2", 
                                 "3" = "Sensor3", "4" = "Sensor4", "5" = "Sensor5",
                                 "6" = "Sensor6","7" = "Sensor7","8" = "Sensor8",
                                 "9" = "Sensor9"))
Chlorodinine <-within(Chlorodinine, rm(Chemical,Time ))
Chlorodinine <- setDT(Chlorodinine)[, lapply(.SD, mean), by = Date]
Chlorodinine$Date<-as.Date(parse_date_time(Chlorodinine$Date,"%Y-%m-%d"))
Chlorodinine$Chemical <- "Chlorodinine"
FixingDate <- data.frame(date = Chlorodinine$Date,
                         year = as.numeric(format(Chlorodinine$Date, format = "%Y")),
                         month = as.numeric(format(Chlorodinine$Date, format = "%m")),
                         day = as.numeric(format(Chlorodinine$Date, format = "%d")))

Chlorodinine <-cbind (Chlorodinine,FixingDate$month)
colnames (Chlorodinine)[colnames(Chlorodinine) == 'V2'] <- 'Month'
Chlorodinine <-cbind (Chlorodinine,FixingDate$day)
colnames (Chlorodinine)[colnames(Chlorodinine) == 'V2'] <- 'Day'
Chlorodinine$Month [Chlorodinine$Month %in% "12"]  <- "December"
Chlorodinine$Month [Chlorodinine$Month %in% "8"]  <- "August"
Chlorodinine$Month [Chlorodinine$Month %in% "4"]  <- "April"
Chlorodinine <-Chlorodinine[do.call(order, Chlorodinine), ]

ChlorodininePlot <- plot_ly(Chlorodinine, x = ~Day, y = ~Sensor1, text = ~paste("Chemical:", Chemical),frame=~Month, type = 'scatter', mode = 'markers', name = 'Sensor 1',fill = 'tozeroy') %>%
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
                title = "Chlorodinine Chemical Data on All Sensors",
                xaxis = list(title = "Date"),
                yaxis = list(title = "Total Reading Per Day in PPM")
        )

ChlorodininePlot <- layout(ChlorodininePlot, title=layout$title, 
                           xaxis=layout$xaxis, yaxis=layout$yaxis)
ChlorodininePlot

       