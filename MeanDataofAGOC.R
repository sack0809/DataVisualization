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
AGOC <- filter(dataset1, Chemical == "AGOC-3A")
AGOC$Time <- format(as.POSIXct(AGOC$`Date Time`,format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")
AGOC$Date <- format(as.POSIXct(AGOC$`Date Time`,format="%Y:%m:%d %H:%M:%S"),"%Y:%m:%d")
AGOC <-within(AGOC, rm(`Date Time` ))
AGOC <-AGOC %>%
        rownames_to_column() %>%
        spread(Monitor, Reading)
AGOC[is.na(AGOC)] <- 0
AGOC <- rename(AGOC, c("1" = "Sensor1", "2" = "Sensor2", 
                                           "3" = "Sensor3", "4" = "Sensor4", "5" = "Sensor5",
                                           "6" = "Sensor6","7" = "Sensor7","8" = "Sensor8",
                                           "9" = "Sensor9"))
AGOC <-within(AGOC, rm(Chemical,Time, rowname ))
AGOC <- setDT(AGOC)[, lapply(.SD, mean), by = Date]
AGOC$Date<-as.Date(parse_date_time(AGOC$Date,"%Y-%m-%d"))
AGOC$Chemical <- "AGOC"
FixingDate <- data.frame(date = AGOC$Date,
                         year = as.numeric(format(AGOC$Date, format = "%Y")),
                         month = as.numeric(format(AGOC$Date, format = "%m")),
                         day = as.numeric(format(AGOC$Date, format = "%d")))

AGOC <-cbind (AGOC,FixingDate$month)
colnames (AGOC)[colnames(AGOC) == 'V2'] <- 'Month'
AGOC <-cbind (AGOC,FixingDate$day)
colnames (AGOC)[colnames(AGOC) == 'V2'] <- 'Day'
AGOC$Month [AGOC$Month %in% "12"]  <- "December"
AGOC$Month [AGOC$Month %in% "8"]  <- "August"
AGOC$Month [AGOC$Month %in% "4"]  <- "April"
AGOC <-AGOC[do.call(order, AGOC), ]


AGOCPlot <- plot_ly(AGOC, x = ~Day, y = ~Sensor1, text = ~paste("Chemical:", Chemical),frame=~Month, type = 'scatter', mode = 'markers', name = 'Sensor 1',fill = 'tozeroy') %>%
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
                title = "AGOC Chemical Data on All Sensors",
                xaxis = list(title = "Date"),
                yaxis = list(title = "Average Reading Per Day in PPM")
        )

AGOCPlot <- layout(AGOCPlot, title=layout$title, 
                             xaxis=layout$xaxis, yaxis=layout$yaxis)
AGOCPlot


