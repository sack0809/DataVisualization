library(tidyr)
library (dplyr)
library(plotly)
library(plyr)
library(readxl)
library(lubridate)
library(data.table)
library(tidyverse)
library(openair)

Sensor1 <- filter(dataset1, Monitor == "1")
Sensor1$ID <- seq.int(nrow(Sensor1))
Sensor2 <- filter(dataset1, Monitor == "2")
Sensor2$ID <- seq.int(nrow(Sensor2))
Sensor3 <- filter(dataset1, Monitor == "3")
Sensor3$ID <- seq.int(nrow(Sensor3))
Sensor4 <- filter(dataset1, Monitor == "4")
Sensor4$ID <- seq.int(nrow(Sensor4))
Sensor5 <- filter(dataset1, Monitor == "5")
Sensor5$ID <- seq.int(nrow(Sensor5))
Sensor6 <- filter(dataset1, Monitor == "6")
Sensor6$ID <- seq.int(nrow(Sensor6))
Sensor7 <- filter(dataset1, Monitor == "7")
Sensor7$ID <- seq.int(nrow(Sensor7))
Sensor8 <- filter(dataset1, Monitor == "8")
Sensor8$ID <- seq.int(nrow(Sensor8))
Sensor9 <- filter(dataset1, Monitor == "9")
Sensor9$ID <- seq.int(nrow(Sensor9))

hideXaxis <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
)

p3 <- plot_ly(Sensor1, x = Sensor1$ID, y = ~Reading) %>%
        add_lines(name = ~"Sensor1")%>%
        layout( xaxis=hideXaxis,yaxis = list(range = c(0,10)),
                showlegend = FALSE)
p4 <- plot_ly(Sensor2, x = Sensor2$ID, y = ~Reading) %>%
        add_lines(name = ~"Sensor2")%>%
        layout( xaxis=hideXaxis,yaxis = list(range = c(0,10)),
                showlegend = TRUE)
p5 <- plot_ly(Sensor3, x = Sensor3$ID, y = ~Reading) %>%
        add_lines(name = ~"Sensor3")%>%
        layout( xaxis=hideXaxis,yaxis = list(range = c(0,10)),
                showlegend = TRUE)
p6 <- plot_ly(Sensor4, x = Sensor4$ID, y = ~Reading) %>%
        add_lines(name = ~"Sensor4")%>%
        layout( xaxis=hideXaxis,yaxis = list(range = c(0,10)),
                showlegend = FALSE)
p7 <- plot_ly(Sensor5, x = Sensor5$ID, y = ~Reading) %>%
        add_lines(name = ~"Sensor5")%>%
        layout( xaxis=hideXaxis,yaxis = list(title="Reading",range = c(0,10)),
                showlegend = TRUE)
p8 <- plot_ly(Sensor6, x = Sensor6$ID, y = ~Reading) %>%
        add_lines(name = ~"Sensor6")%>%
        layout( xaxis=hideXaxis,yaxis = list(range = c(0,10)),
                showlegend = TRUE)
p9 <- plot_ly(Sensor7, x = Sensor7$ID, y = ~Reading) %>%
        add_lines(name = ~"Sensor7")%>%
        layout( xaxis=hideXaxis,yaxis = list(range = c(0,10)),
                showlegend = TRUE)
p10 <- plot_ly(Sensor8, x = Sensor8$ID, y = ~Reading) %>%
        add_lines(name = ~"Sensor8")%>%
        layout( xaxis=hideXaxis,yaxis = list(range = c(0,10)),
                showlegend = TRUE)
p11 <- plot_ly(Sensor9, x = Sensor9$ID, y = ~Reading) %>%
        add_lines(name = ~"Sensor9")%>%
        layout( xaxis=hideXaxis,yaxis = list(range = c(0,10)),
                showlegend = TRUE)

sensorSubplot <- subplot(p3,p4,p5, p6,p7,p8,p9,p10,p11, shareX=TRUE , nrows = 9, margin= 0.02)%>%
        layout(paper_bgcolor='#F0FFF0 ', plot_bgcolor='#F0FFF0' ,title="CUMULATIVE SENSOR DATA VISUALIZATION",
               xaxis = list (title="Total Sensor Reading Per hour "))

sensorSubplot


