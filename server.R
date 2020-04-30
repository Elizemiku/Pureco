#https://shiny.rstudio.com/gallery/download-knitr-reports.html

#pacotes necessários para o código
packages <- c("tidyverse", "sf", "mapview", "lubridate","forecast", 
              "tseries", "curl", "DT", "plotly", "leaflet", "geosphere",
              "shiny", "tsibble", "zoo", "xts", "rsconnect", "dygraphs")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
library(forecast)
library(tseries)
library(curl)
library(plotly)
library(leaflet)
library(geosphere)
library(shiny)
library(shinythemes)
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
        
        # se nao tiver colocado as planilhas retorna nulo
        data = input$faxina
        if(is.null(data))
            return(NULL)
        
        data2 = input$disponibilidade
        if(is.null(data2))
            return(NULL)
        
        # leitura do banco de dados de faxinas
        faxinas = read_csv2(data$datapath,locale = locale(encoding = "latin1"))
        
        # o intervalo de datas posso deixar 
        faxinas$Data<-as.POSIXct(faxinas$Data, "UTC", format="%d/%m/%Y")
        faxinas<-faxinas%>%filter(Data>=input$selecionarperiodo & Data<input$selecionarperiodo2)
        
        faxinas$`Dia da Semana`<- weekdays(faxinas$Data)
        semana <- c("segunda", "terça", "quarta", "quinta", "sexta", "sábado", "domingo")
        faxinas$`Dia da Semana` <- factor(faxinas$`Dia da Semana`, order = TRUE, levels = semana)
        
        # leitura do banco de dados de disponibilidade      
        disponibilidade <- read_csv2(data2$datapath,locale = locale(encoding = "latin1"))
        disponibilidade$Data<-as.POSIXct(disponibilidade$Data, "UTC", format="%d/%m/%Y")
        disponibilidade<-disponibilidade%>%filter(Data>=input$selecionarperiodo & Data<input$selecionarperiodo2)
        
        # funcao que faz aparecer a imagem do pureco
        if (input$botao != 0) {
          output$inicio <- renderText({
            ##imagem do pureco
            src = "https://static.wixstatic.com/media/02e186_1928f72d50254d83a45117a9d6dc5332~mv2_d_1600_1600_s_2.png/v1/fill/w_400,h_400,al_c,q_80,usm_0.66_1.00_0.01/02e186_1928f72d50254d83a45117a9d6dc5332~mv2_d_1600_1600_s_2.webp"
            c('<img src="', src, '">')
          })
        }

        output$infgeral1parte1<-renderPlotly({
          # Gráfico de Faxinas por dia da semana
          g1<-ggplot(faxinas %>% filter(Mulher != "NA") %>% mutate(Quantidade = 1),aes(x =`Dia da Semana`,y = Quantidade,fill=`Dia da Semana`))+
            stat_summary(fun.y = "sum", geom = "bar") + 
            ggtitle("Quantidade de Faxinas por Dia da Semana") + 
            ylab("Quantidade de Faxinas") + 
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank()) 
          # grafico interativo usando plotly
          g1<-ggplotly(g1)
          
          g1
        })
        
        # adionar ou explicacao para a variavel tipo, ou uma legenda para ela
        output$infgeral2parte1<-renderPlotly({
          # Gráfico de Faxinas por dia da semana de acordo com o tipo de faxina
            g2<-ggplot(faxinas%>%mutate(Quantidade=1)%>%filter(Tipo!="NA" & `Ocorreu?`=="TRUE"),
                       aes(x=`Dia da Semana`,y=Quantidade,fill=`Dia da Semana`))+
                stat_summary(fun.y="sum", geom="bar")+
                facet_wrap(~Tipo, scales = "free_x") +
                ggtitle("Quantidade de Faxinas por Tipo e Dia da Semana")+
                ylab("Quantidade de Faxinas") +
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank()) 
            
            g2<-ggplotly(g2)
            
            g2
        })

    })
}  