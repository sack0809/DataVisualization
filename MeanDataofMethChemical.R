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
Methylosmolene <- filter(dataset1, Chemical == "Methylosmolene")
Methylosmolene$Time <- format(as.POSIXct(Methylosmolene$`Date Time`,format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")
Methylosmolene$Date <- format(as.POSIXct(Methylosmolene$`Date Time`,format="%Y:%m:%d %H:%M:%S"),"%Y:%m:%d")
Methylosmolene <-within(Methylosmolene, rm(`Date Time` ))
Methylosmolene <-spread(Methylosmolene, Monitor, Reading)
Methylosmolene <- rename(Methylosmolene, c("1" = "Sensor1", "2" = "Sensor2", 
                                       "3" = "Sensor3", "4" = "Sensor4", "5" = "Sensor5",
                                       "6" = "Sensor6","7" = "Sensor7","8" = "Sensor8",
                                       "9" = "Sensor9"))
Methylosmolene <-within(Methylosmolene, rm(Chemical,Time ))
Methylosmolene <- setDT(Methylosmolene)[, lapply(.SD, mean), by = Date]
Methylosmolene$Date<-as.Date(parse_date_time(Methylosmolene$Date,"%Y-%m-%d"))
Methylosmolene$Chemical <- "Methylosmolene"
FixingDate <- data.frame(date = Methylosmolene$Date,
                         year = as.numeric(format(Methylosmolene$Date, format = "%Y")),
                         month = as.numeric(format(Methylosmolene$Date, format = "%m")),
                         day = as.numeric(format(Methylosmolene$Date, format = "%d")))

Methylosmolene <-cbind (Methylosmolene,FixingDate$month)
colnames (Methylosmolene)[colnames(Methylosmolene) == 'V2'] <- 'Month'
Methylosmolene <-cbind (Methylosmolene,FixingDate$day)
colnames (Methylosmolene)[colnames(Methylosmolene) == 'V2'] <- 'Day'
Methylosmolene$Month [Methylosmolene$Month %in% "12"]  <- "December"
Methylosmolene$Month [Methylosmolene$Month %in% "8"]  <- "August"
Methylosmolene$Month [Methylosmolene$Month %in% "4"]  <- "April"
Methylosmolene <-Methylosmolene[do.call(order, Methylosmolene), ]
Methylosmolene[is.na(Methylosmolene)] <- 0

MethylosmolenePlot <- plot_ly(Methylosmolene, x = ~Day, y = ~Sensor1, text = ~paste("Chemical:", Chemical),frame=~Month, type = 'scatter', mode = 'markers', name = 'Sensor 1',fill = 'tozeroy') %>%
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
                title = "Methylosmolene Chemical Data on All Sensors",
                xaxis = list(title = "Date"),
                yaxis = list(title = "Average Reading Per Day in PPM")
        )

MethylosmolenePlot <- layout(MethylosmolenePlot, title=layout$title, 
                           xaxis=layout$xaxis, yaxis=layout$yaxis)
MethylosmolenePlot

