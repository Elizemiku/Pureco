#https://shiny.rstudio.com/gallery/download-knitr-reports.html

#pacotes necessários para o código
packages <- c("dplyr", "tibble", "purrr", "sf", "mapview", "lubridate",
              "ggplot2", "forecast", "tseries", "curl", "stringr", "DT",
              "stringr", "plotly", "leaflet", "tidyr", "geosphere",
              "shiny", "readr", "tsibble", "zoo", "xts", "rsconnect", "dygraphs")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(dplyr)
library(tibble)
library(purrr)
library(sf)
library(mapview)
library(lubridate)
library(ggplot2)
library(forecast)
library(tseries)
library(curl)
library(stringr)
library(plotly)
library(leaflet)
library(tidyr)
library(geosphere)
library(shiny)
library(shinythemes)
library(readr)
library(DT)
library(tsibble)
library(zoo)
library(xts)
library(rsconnect)
library(dygraphs)
library(htmltools)

source("Tabs.R")

server <- function(input,output,session){
    
    observe({
  
        data = input$faxina
        if(is.null(data))
            return(NULL)
        
        data2 = input$disponibilidade
        if(is.null(data2))
            return(NULL)
        
        faxinas = read_csv2(data$datapath,locale = locale(encoding = "latin1"))
        faxinas$Data<-as.POSIXct(faxinas$Data, "UTC", format="%d/%m/%Y")
        faxinas<-faxinas%>%filter(Data>=input$selecionarperiodo & Data<input$selecionarperiodo2)
        faxinas$`Dia da Semana`<-weekdays(faxinas$Data)
        semana <- c("segunda-feira", "terça-feira", "quarta-feira", "quinta-feira", "sexta-feira", "sábado", "domingo")
        faxinas$`Dia da Semana` <- factor(faxinas$`Dia da Semana`, order=TRUE, levels=semana)
        
        disponibilidade <- read_csv2(data2$datapath,locale = locale(encoding = "latin1"))
        disponibilidade$Data<-as.POSIXct(disponibilidade$Data, "UTC", format="%d/%m/%Y")
        disponibilidade<-disponibilidade%>%filter(Data>=input$selecionarperiodo & Data<input$selecionarperiodo2)
        
        if (input$botao != 0) {
          output$inicio <- renderText({
            ##imagem do pureco
            src = "https://static.wixstatic.com/media/02e186_1928f72d50254d83a45117a9d6dc5332~mv2_d_1600_1600_s_2.png/v1/fill/w_400,h_400,al_c,q_80,usm_0.66_1.00_0.01/02e186_1928f72d50254d83a45117a9d6dc5332~mv2_d_1600_1600_s_2.webp"
            c('<img src="', src, '">')
          })
        }
        
        
        output$geral2<-renderPlotly({
            g2<-ggplot(faxinas%>%mutate(Quantidade=1)%>%filter(Tipo!="NA" & `Ocorreu?`=="TRUE"),
                       aes(x=`Dia da Semana`,y=Quantidade,fill=`Dia da Semana`))+
                stat_summary(fun.y="sum", geom="bar")+
                facet_wrap(~Tipo,scales = "free_x")+ 
                ggtitle("Quantidade de Faxinas por Tipo e Dia da Semana")+
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank()) +
                ylab("Quantidade de Faxinas")
            g2<-ggplotly(g2)
            g2
        })
        
    })
}  