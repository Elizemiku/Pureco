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
library(readr)
library(DT)
library(tsibble)
library(zoo)
library(xts)
library(rsconnect)
library(dygraphs)
library(htmltools)

# Opcoes de periodos:
problemList <- list(
    Periodos = list(
        Selecionar = 0, Diario = 1, Semanal = 2, Mensal = 4, Trimestral = 3, Anual = 5)
)

# Opcoes de mulheres(faxineiras do pureco):
problemList2 <- list(
    mulher = list(
        Selecionar = 0, Lourdes = 1, Vilanir = 2, Zilza = 3)
)
#ver qual o nome da outra moça  
# Alterar pra ter mais opcoes de mulheres para o caso de aumentar colaboradoras
# Adionar nome da nova mulher caso entre!


#o que e a funcao gen_array?
gen_array <- function(forecast_obj){
    
    actuals <- forecast_obj$x
    lower <- forecast_obj$lower[,2]
    upper <- forecast_obj$upper[,2]
    point_forecast <- forecast_obj$mean
    
    cbind(actuals, lower, upper, point_forecast)
}

# Com o inicio pronto:
# funcoes do server
server <- function(input, output,session) {
        
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
        
        if(input$botao != 0){
            output$inicio<-renderText({
                ##imagem do pureco    
                    src = "https://static.wixstatic.com/media/02e186_1928f72d50254d83a45117a9d6dc5332~mv2_d_1600_1600_s_2.png/v1/fill/w_400,h_400,al_c,q_80,usm_0.66_1.00_0.01/02e186_1928f72d50254d83a45117a9d6dc5332~mv2_d_1600_1600_s_2.webp"
                c('<img src="',src,'">')
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
        # Adicionar futuramente se e pos festa ou pos mudança!
            
        output$mulheres1<-renderPlotly({
            m1<-ggplot(faxinas%>%mutate(Quantidade=1)%>%filter(Mulher!="NA"&`Ocorreu?`=="TRUE"),
                       aes(x=Mulher,y=Quantidade,fill=Mulher))+stat_summary(fun.y="sum", geom="bar")+ 
                ggtitle("Quantidade de Faxinas por Mulher")+
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank()) +
              ylab("Quantidade de faxinas")
            m1<-ggplotly(m1)
            m1
        })
        
        output$mulheres2<-renderPlotly({
            m2<-ggplot(faxinas%>%mutate(Quantidade=1)%>%filter(Mulher!="NA" & `Ocorreu?`=="TRUE"),
                       aes(x=`Dia da Semana`,y=Quantidade,fill=`Dia da Semana`))+
                stat_summary(fun.y="sum", geom="bar")+
                facet_wrap(~Mulher,scales = "free_x")+ 
                ggtitle("Quantidade de Faxinas por Mulher e Dia da Semana")+
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank()) +
              ylab("Quantidade de faxinas")
            m2<-ggplotly(m2)
            m2
        })
        
        output$mapa<-renderLeaflet({
            #Mapa Geral:
            faxinas2<-faxinas%>%filter(`Ocorreu?`!="NA")%>%filter(`Ocorreu?`!="FALSE")%>%filter(Cliente!="NA")%>%mutate(n=1)%>%group_by(Cliente)%>%summarise(soma=sum(n))
            faxinas<-left_join(faxinas,faxinas2,by=c("Cliente"="Cliente"))
            
            a <- as.vector(distm(c(-47.081400,-22.817700), c(-47.057500,-22.834400) , fun=distVincentyEllipsoid))
            a2<- as.vector(distm(c(-47.089700,-22.787200), c(-47.066500,-22.779900) , fun=distVincentyEllipsoid))
            a3<- as.vector(distm(c(-47.045100,-22.831500), c(-47.065300,-22.829400) , fun=distVincentyEllipsoid))
            
            faxinas$grupos<-ifelse((faxinas$soma>1 & faxinas$soma<10),faxinas$grupos<-"C",
                                   ifelse((faxinas$soma>=10 & faxinas$soma<20),faxinas$grupos<-"B",
                                          ifelse(faxinas$soma>=20,faxinas$grupos<-"A",faxinas$grupos<-"D")))
            faxinas<-faxinas%>%filter(`Latitude Mulher`!="NA")%>%filter(Latitude!="NA")
            
            #Colocar os grupos aqui:
            beatCol <- colorFactor(c("lightgreen","lightblue","orange","red"), faxinas$grupos)
            Encoding(x = faxinas$Mulher) <- "UTF-8"
            Encoding(x = faxinas$Cliente) <- "UTF-8"
            Encoding(x = faxinas$Instituto) <- "UTF-8"
            faxinas$Mulher <- iconv(x = faxinas$Mulher, 
                                          from = "UTF-8", 
                                          to = "UTF-8", sub = "")
            faxinas$Cliente <- iconv(x = faxinas$Cliente, 
                                          from = "UTF-8", 
                                          to = "UTF-8", sub = "")
            faxinas$Instituto <- iconv(x = faxinas$Instituto, 
                                          from = "UTF-8", 
                                          to = "UTF-8", sub = "")
            mapinha<-leaflet(data = faxinas) %>%
                addTiles() %>%
                addCircles(lng = -47.081400,
                           lat = -22.817700,
                           radius = a,
                           opacity = 0.008) %>%
                addCircles(lng = -47.089700,
                           lat = -22.787200,
                           radius = a2,
                           opacity = 0.008) %>%
                addCircles(lng = -47.045100,
                           lat = -22.831500,
                           radius = a3,
                           opacity = 0.008) %>%
                addCircleMarkers(lng = ~Longitude,
                                 lat = ~Latitude,
                                 color = ~beatCol(faxinas$grupos),
                                 opacity = 0.4,
                                 popup = paste("Cliente:", faxinas$Cliente, "<br>", 
                                               "Instituto:", faxinas$Instituto, "<br>",
                                               "Faxinas:", faxinas$soma)) %>%
                addAwesomeMarkers(lng = ~`Longitude Mulher`,
                                  lat = ~`Latitude Mulher`,
                                  popup = paste("Mulher:", faxinas$Mulher, "<br>"))
            mapinha
            
        })
        
        if(input$localmulher==1){
            output$mapa2<-renderLeaflet({
                # Mapa para Lourdes:
                faxinas<-faxinas%>%filter(Mulher=="Lourdes")
                faxinas$DistanciaMetros <- as.vector(distm(faxinas[,c('Longitude','Latitude')], c(-47.108257,-22.847706) , fun=distVincentyEllipsoid))
                faxinas$DistanciaMetros<-round(faxinas$DistanciaMetros/1000, 4)
                
                faxinas2<-faxinas%>%filter(`Ocorreu?`!="NA")%>%filter(`Ocorreu?`!="FALSE")%>%filter(Cliente!="NA")%>%mutate(n=1)%>%group_by(Cliente)%>%summarise(soma=sum(n))
                faxinas<-left_join(faxinas,faxinas2,by=c("Cliente"="Cliente"))
                
                a <- as.vector(distm(c(-47.081400,-22.817700), c(-47.057500,-22.834400) , fun=distVincentyEllipsoid))
                a2<- as.vector(distm(c(-47.089700,-22.787200), c(-47.066500,-22.779900) , fun=distVincentyEllipsoid))
                a3<- as.vector(distm(c(-47.045100,-22.831500), c(-47.065300,-22.829400) , fun=distVincentyEllipsoid))
                
                #faxinas$grupos<-ifelse((faxinas$soma>1 & faxinas$soma<10),faxinas$grupos<-"C",
                #ifelse((faxinas$soma>=10 & faxinas$soma<20),faxinas$grupos<-"B",
                #ifelse(faxinas$soma>=20,faxinas$grupos<-"A",faxinas$grupos<-"D")))
                faxinas<-faxinas%>%filter(`Latitude Mulher`!="NA")%>%filter(Latitude!="NA")
                Encoding(x = faxinas$Mulher) <- "UTF-8"
                Encoding(x = faxinas$Cliente) <- "UTF-8"
                Encoding(x = faxinas$Instituto) <- "UTF-8"
                faxinas$Mulher <- iconv(x = faxinas$Mulher, 
                                        from = "UTF-8", 
                                        to = "UTF-8", sub = "")
                faxinas$Cliente <- iconv(x = faxinas$Cliente, 
                                         from = "UTF-8", 
                                         to = "UTF-8", sub = "")
                faxinas$Instituto <- iconv(x = faxinas$Instituto, 
                                           from = "UTF-8", 
                                           to = "UTF-8", sub = "")
                #Colocar os grupos aqui:
                #beatCol <- colorFactor(c("lightgreen","lightblue","orange","red"), faxinas$grupos)
                
                mapinha<-leaflet(data = faxinas) %>%
                    addTiles() %>%
                    addCircles(lng = -47.081400,
                               lat = -22.817700,
                               radius = a,
                               opacity = 0.008) %>%
                    addCircles(lng = -47.089700,
                               lat = -22.787200,
                               radius = a2,
                               opacity = 0.008) %>%
                    addCircles(lng = -47.045100,
                               lat = -22.831500,
                               radius = a3,
                               opacity = 0.008) %>%
                    addCircleMarkers(lng = ~Longitude,
                                     lat = ~Latitude,
                                     #color = ~beatCol(faxinas$grupos),
                                     opacity = 0.4,
                                     popup = paste("Cliente:", faxinas$Cliente, "<br>",
                                                   "Instituto:", faxinas$Instituto, "<br>",
                                                   "Distância (km):", faxinas$DistanciaMetros, "<br>",
                                                   "Faxinas:", faxinas$soma)) %>%
                    addAwesomeMarkers(lng = ~`Longitude Mulher`,
                                      lat = ~`Latitude Mulher`,
                                      popup = paste("Mulher:", faxinas$Mulher, "<br>"))
                mapinha
                
            })
            
        }
        
        if(input$localmulher==2){
            output$mapa2<-renderLeaflet({
                # Mapa para Vilanir:
                faxinas<-faxinas%>%filter(Mulher=="Vilanir")
                faxinas$DistanciaMetros <- as.vector(distm(faxinas[,c('Longitude','Latitude')], c(-47.103771,-22.850452) , fun=distVincentyEllipsoid))
                faxinas$DistanciaMetros<-round(faxinas$DistanciaMetros/1000, 4)
                
                faxinas2<-faxinas%>%filter(`Ocorreu?`!="NA")%>%filter(`Ocorreu?`!="FALSE")%>%filter(Cliente!="NA")%>%mutate(n=1)%>%group_by(Cliente)%>%summarise(soma=sum(n))
                faxinas<-left_join(faxinas,faxinas2,by=c("Cliente"="Cliente"))
                
                a <- as.vector(distm(c(-47.081400,-22.817700), c(-47.057500,-22.834400) , fun=distVincentyEllipsoid))
                a2<- as.vector(distm(c(-47.089700,-22.787200), c(-47.066500,-22.779900) , fun=distVincentyEllipsoid))
                a3<- as.vector(distm(c(-47.045100,-22.831500), c(-47.065300,-22.829400) , fun=distVincentyEllipsoid))
                Encoding(x = faxinas$Mulher) <- "UTF-8"
                Encoding(x = faxinas$Cliente) <- "UTF-8"
                Encoding(x = faxinas$Instituto) <- "UTF-8"
                faxinas$Mulher <- iconv(x = faxinas$Mulher, 
                                        from = "UTF-8", 
                                        to = "UTF-8", sub = "")
                faxinas$Cliente <- iconv(x = faxinas$Cliente, 
                                         from = "UTF-8", 
                                         to = "UTF-8", sub = "")
                faxinas$Instituto <- iconv(x = faxinas$Instituto, 
                                           from = "UTF-8", 
                                           to = "UTF-8", sub = "")
                mapinha<-leaflet(data = faxinas) %>%
                    addTiles() %>%
                    addCircles(lng = -47.081400,
                               lat = -22.817700,
                               radius = a,
                               opacity = 0.008) %>%
                    addCircles(lng = -47.089700,
                               lat = -22.787200,
                               radius = a2,
                               opacity = 0.008) %>%
                    addCircles(lng = -47.045100,
                               lat = -22.831500,
                               radius = a3,
                               opacity = 0.008) %>%
                    addCircleMarkers(lng = ~Longitude,
                                     lat = ~Latitude,
                                     opacity = 0.4,
                                     popup = paste("Cliente:", faxinas$Cliente, "<br>",
                                                   "Instituto:", faxinas$Instituto, "<br>",
                                                   "Distância (km):", faxinas$DistanciaMetros, "<br>",
                                                   "Faxinas:", faxinas$soma)) %>%
                    addAwesomeMarkers(lng = ~`Longitude Mulher`,
                                      lat = ~`Latitude Mulher`,
                                      popup = paste("Mulher:", faxinas$Mulher, "<br>"))
                mapinha
                
            })
            
        }
        
        if(input$localmulher==3){
            output$mapa2<-renderLeaflet({
                # Mapa para Zilza:
                faxinas<-faxinas%>%filter(Mulher=="Zilza")
                faxinas$DistanciaMetros <- as.vector(distm(faxinas[,c('Longitude','Latitude')], c(-47.11493,-22.84460) , fun=distVincentyEllipsoid))
                faxinas$DistanciaMetros<-round(faxinas$DistanciaMetros/1000, 4)
                
                faxinas2<-faxinas%>%filter(`Ocorreu?`!="NA")%>%filter(`Ocorreu?`!="FALSE")%>%filter(Cliente!="NA")%>%mutate(n=1)%>%group_by(Cliente)%>%summarise(soma=sum(n))
                faxinas<-left_join(faxinas,faxinas2,by=c("Cliente"="Cliente"))
                
                a <- as.vector(distm(c(-47.081400,-22.817700), c(-47.057500,-22.834400) , fun=distVincentyEllipsoid))
                a2<- as.vector(distm(c(-47.089700,-22.787200), c(-47.066500,-22.779900) , fun=distVincentyEllipsoid))
                a3<- as.vector(distm(c(-47.045100,-22.831500), c(-47.065300,-22.829400) , fun=distVincentyEllipsoid))
                
                #faxinas$grupos<-ifelse((faxinas$soma>1 & faxinas$soma<10),faxinas$grupos<-"C",
                #ifelse((faxinas$soma>=10 & faxinas$soma<20),faxinas$grupos<-"B",
                #ifelse(faxinas$soma>=20,faxinas$grupos<-"A",faxinas$grupos<-"D")))
                faxinas<-faxinas%>%filter(`Latitude Mulher`!="NA")%>%filter(Latitude!="NA")
                
                #Colocar os grupos aqui:
                #beatCol <- colorFactor(c("lightgreen","lightblue","orange","red"), faxinas$grupos)
                Encoding(x = faxinas$Mulher) <- "UTF-8"
                Encoding(x = faxinas$Cliente) <- "UTF-8"
                Encoding(x = faxinas$Instituto) <- "UTF-8"
                faxinas$Mulher <- iconv(x = faxinas$Mulher, 
                                        from = "UTF-8", 
                                        to = "UTF-8", sub = "")
                faxinas$Cliente <- iconv(x = faxinas$Cliente, 
                                         from = "UTF-8", 
                                         to = "UTF-8", sub = "")
                faxinas$Instituto <- iconv(x = faxinas$Instituto, 
                                           from = "UTF-8", 
                                           to = "UTF-8", sub = "")
                mapinha<-leaflet(data = faxinas) %>%
                    addTiles() %>%
                    addCircles(lng = -47.081400,
                               lat = -22.817700,
                               radius = a,
                               opacity = 0.008) %>%
                    addCircles(lng = -47.089700,
                               lat = -22.787200,
                               radius = a2,
                               opacity = 0.008) %>%
                    addCircles(lng = -47.045100,
                               lat = -22.831500,
                               radius = a3,
                               opacity = 0.008) %>%
                    addCircleMarkers(lng = ~Longitude,
                                     lat = ~Latitude,
                                     #color = ~beatCol(faxinas$grupos),
                                     opacity = 0.4,
                                     popup = paste("Cliente:", faxinas$Cliente, "<br>",
                                                   "Instituto:", faxinas$Instituto, "<br>",
                                                   "Distância (km):", faxinas$DistanciaMetros, "<br>",
                                                   "Faxinas:", faxinas$soma)) %>%
                    addAwesomeMarkers(lng = ~`Longitude Mulher`,
                                      lat = ~`Latitude Mulher`,
                                      popup = paste("Mulher:", faxinas$Mulher, "<br>"))
                mapinha
                
            })
            
        }
        
        if(input$periodos==2){
            output$qtde1<-renderDygraph({
                # Por semana
                faxinas<-faxinas%>%filter(`Ocorreu?`=="TRUE")%>%mutate(fax=1)
                
                x<-data.frame(Data=seq.POSIXt(from=min(faxinas$Data), to=max(faxinas$Data),by = "day"),QTDE=0)
                faxinas<-full_join(faxinas,x,by=c("Data"="Data"))
                faxinas<-faxinas%>%select(-QTDE)
                
                faxinas$fax[is.na(faxinas$fax)]<-0
                
                data <- as.xts(faxinas$fax,order.by=as.Date(faxinas$Data))
                weekly <- apply.weekly(data,sum)
                dyBase <- dygraphs::dygraph(weekly)
                
                dyBase%>%
                    # allow zooming
                    dygraphs::dyRangeSelector() %>% 
                    # make unzoom button
                    dygraphs::dyUnzoom() %>% 
                    # control legend width
                    dygraphs::dyLegend(width=weekly,show = c("follow"))
            })
            
            output$pred1<-renderDygraph({
                # por semana:
                faxinas<-faxinas%>%filter(`Ocorreu?`=="TRUE")%>%mutate(fax=1)
                
                x<-data.frame(Data=seq.POSIXt(from=min(faxinas$Data), to=max(faxinas$Data),by = "day"),QTDE=0)
                faxinas<-full_join(faxinas,x,by=c("Data"="Data"))
                faxinas<-faxinas%>%select(-QTDE)
                
                faxinas$fax[is.na(faxinas$fax)]<-0
                
                data <- as.xts(faxinas$fax,order.by=as.Date(faxinas$Data))
                weekly <- apply.weekly(data,sum)
                
                model<-auto.arima(weekly)
                
                forecast_obj<-forecast(model)
                
                tsweek<-gen_array(forecast_obj)
                y<-seq.Date(from=min(index(weekly)),by = "week",length.out = max(index(tsweek)))
                    
                tsweek<-as.data.frame(tsweek)
                
                rownames(tsweek)<-as.vector(y)
                tsweek<-xts(tsweek,order.by = y)
                
                
                dygraph(tsweek, main = "Previsão da Quantidade de Faxinas") %>% 
                    dyRangeSelector() %>%
                    dySeries(name = "actuals", label = "Observado") %>%
                    dySeries(c("lower","point_forecast","upper"), label = "Predito") %>%
                    dyLegend(show = "always",showZeroValues = TRUE) %>%
                    dyHighlight(highlightCircleSize = 5,highlightSeriesOpts = list(strokeWidth = 2)) %>%
                    dyOptions(gridLineColor = "grey",drawXAxis = FALSE)%>%
                    dyUnzoom()
                
            })
        }
        
        if(input$periodos==1){
            
            output$qtde1<-renderDygraph({
                # Por dia:
                faxinas<-faxinas%>%filter(`Ocorreu?`=="TRUE")%>%mutate(fax=1)
                
                x<-data.frame(Data=seq.POSIXt(from=min(faxinas$Data), to=max(faxinas$Data),by = "day"),QTDE=0)
                faxinas<-full_join(faxinas,x,by=c("Data"="Data"))
                faxinas<-faxinas%>%select(-QTDE)
                
                faxinas$fax[is.na(faxinas$fax)]<-0
                
                data <- as.xts(faxinas$fax,order.by=as.Date(faxinas$Data))
                weekly <- apply.daily(data,sum)
                dyBase <- dygraphs::dygraph(weekly)
                
                dyBase%>%
                    # allow zooming
                    dygraphs::dyRangeSelector() %>% 
                    # make unzoom button
                    dygraphs::dyUnzoom() %>% 
                    # control legend width
                    dygraphs::dyLegend(width=weekly,show = c("follow"))
            })
            
            output$pred1<-renderDygraph({
                # Por dia:
                faxinas<-faxinas%>%filter(`Ocorreu?`=="TRUE")%>%mutate(fax=1)
                
                x<-data.frame(Data=seq.POSIXt(from=min(faxinas$Data), to=max(faxinas$Data),by = "day"),QTDE=0)
                faxinas<-full_join(faxinas,x,by=c("Data"="Data"))
                faxinas<-faxinas%>%select(-QTDE)
                
                faxinas$fax[is.na(faxinas$fax)]<-0
                
                data <- as.xts(faxinas$fax,order.by=as.Date(faxinas$Data))
                weekly <- apply.daily(data,sum)
                
                model<-auto.arima(weekly)
                
                forecast_obj<-forecast(model,h=30)
                
                tsweek<-gen_array(forecast_obj)
                
                y<-seq.Date(from=min(index(weekly)),by = "day",length.out = max(index(tsweek)))
                
                tsweek<-as.data.frame(tsweek)
                
                rownames(tsweek)<-as.vector(y)
                tsweek<-xts(tsweek,order.by = as.Date(y))
                
                
                dygraph(tsweek, main = "Previsão da Quantidade de Faxinas") %>% 
                    dyRangeSelector() %>%
                    dySeries(name = "actuals", label = "Observado") %>%
                    dySeries(c("lower","point_forecast","upper"), label = "Predito") %>%
                    dyLegend(showZeroValues = TRUE) %>%
                    dyHighlight(highlightCircleSize = 5,highlightSeriesOpts = list(strokeWidth = 2)) %>%
                    dyOptions(gridLineColor = "grey",drawXAxis = FALSE)%>%
                    dyUnzoom()
                
            })
        }
        
        output$clientes2<-renderPlotly({
            # Clientes fidelizados 
            aux = faxinas[, "Cliente"]
            aux = aux %>% mutate(n = 1) %>% group_by(Cliente) %>% 
              summarise(soma = sum(n))
            aux = aux %>% filter(Cliente != "NA")
            aux$fidelizados = NA
            index_f = which(aux$soma > 1)
            aux$fidelizados[index_f] = "Fidelizado"
            index_nf = which(aux$soma <= 1)
            aux$fidelizados[index_nf] = "Não Fidelizado"
           aux$`Quantidade faxinas` = NA
           index_1 = which(aux$soma == 1)
           aux$`Quantidade faxinas`[index_1] = "1"
           index_5 = which(aux$soma > 1 & aux$soma <= 5)
           aux$`Quantidade faxinas`[index_5] = "1 a 5"
           index_10 = which(aux$soma > 5 & aux$soma <= 10)
           aux$`Quantidade faxinas`[index_10] = "5 a 10"
           index_15 = which(aux$soma > 10 & aux$soma <= 15)
           aux$`Quantidade faxinas`[index_15] = "10 a 15"
           index_20 = which(aux$soma > 15 & aux$soma <= 20)
           aux$`Quantidade faxinas`[index_20] = "15 a 20"
           index_mais20 = which(aux$soma > 20)
           aux$`Quantidade faxinas`[index_mais20] = "mais de 20"
           faxinasquant = c("1", "1 a 5", "5 a 10", "10 a 15", "15 a 20", "mais de 20")
           aux$`Quantidade faxinas` = factor(aux$`Quantidade faxinas`, order= TRUE, levels = faxinasquant)
           plot2 = ggplot(aux %>% mutate(Quantidade=1), aes(x = `Quantidade faxinas`, y=Quantidade, fill = `Quantidade faxinas`)) + 
             stat_summary(fun.y="sum",geom="bar") + 
             ylab("Quantidade de Clientes") + xlab("Quantidade de faxinas") +  
             ggtitle("Fidelização dos clientes")
           plot2 = ggplotly(plot2)
            plot2
        })
        
        output$clientes1exp<-renderText({
            "Aqui estão os 5 melhores clientes (no período selecionado)! Talvez uma promoção caia bem..."
        })
        
        output$clientes4exp<-renderText({
            "Gráfico apenas de Clientes novos no aplicativo. Esperamos que essas barrinhas crescam ao longo dos meses!"
        })
        
        output$clientes3exp<-renderText({
            "Gráfico que mostra as idades dos clientes que pediram faxinas no aplicativo, de acordo com o sexo!"
        })
        
        output$geral1<-renderPlotly({
            # Faxinas por dia da semana
            g1<-ggplot(faxinas %>% filter(Mulher != "NA") %>% mutate(Quantidade = 1),aes(x =`Dia da Semana`,y = Quantidade,fill=`Dia da Semana`))+
                stat_summary(fun.y = "sum", geom = "bar") + 
                ggtitle("Quantidade de Faxinas por Dia da Semana") + 
                ylab("Quantidade de Faxinas") + 
                xlab("Dias da semana")+
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank()) 
            g1<-ggplotly(g1)
            g1
        })
        
        output$geral3<-renderDataTable({
            # Faxinas por mes
            faxinas<-faxinas%>%filter(`Ocorreu?`=="TRUE")%>%mutate(fax=1)
            
            x<-data.frame(Data=seq.POSIXt(from=min(faxinas$Data), to=max(faxinas$Data),by = "day"),QTDE=0)
            faxinas<-full_join(faxinas,x,by=c("Data"="Data"))
            faxinas<-faxinas%>%select(-QTDE)
            
            faxinas$fax[is.na(faxinas$fax)]<-0
            faxinas$mesano<-format(as.Date(faxinas$Data), "%Y-%m")
            faxinas<-faxinas%>%mutate(mes=yearmonth(Data))
            
            faxinas2<-faxinas %>%group_by(mesano)%>%summarise(soma=sum(fax))%>%select("Ano-Mês"=mesano,"Faxinas"=soma)
            
            datatable(faxinas2, options = list(pageLength = 5))
        })
        
        output$geral4<-renderDataTable({
            institutos<-data.frame(Institutos=c("IA","IB","IC","IE","IEL","IFCH","IFGW","IG","IMECC","IQ","FCF","FENF","FEA","FEAGRI","FEC","FEEC","FEM","FEQ","FCM","FEF","FE","COTUCA"),n=0)
            inst2<-faxinas%>%mutate(a=1)%>%filter(Instituto!="NA")%>%group_by(Instituto)%>%summarise(soma=sum(a))
            inst3<-left_join(institutos,inst2,by=c("Institutos"="Instituto"))
            inst3<-inst3%>% mutate(Faxinas = rowSums(.[2:3],na.rm = TRUE))%>%select(Institutos,Faxinas)%>%arrange(-Faxinas)
            
            datatable(inst3, options = list(pageLength = 5))
            
        })
        
        output$clientes3<-renderPlotly({
            # Idade e Sexo dos clientes
            aux = faxinas[, c("Cliente", "Sexo Cliente","Idade Cliente")]
            aux = unique(aux) %>% filter(Cliente != "NA")
            aux<-aux %>% mutate(n = 1) %>%filter(`Idade Cliente`!="NA")
            aux<-aux%>%group_by(`Idade Cliente`,`Sexo Cliente`)%>%summarise(Quantidade=sum(n))
            plot3<-ggplot(aux,aes(x = `Idade Cliente`, y = Quantidade,fill=`Sexo Cliente`))+
                geom_bar(stat="identity",position="dodge")+ 
                ylab("Quantidade de Clientes") + 
                xlab("Idade") +
              ggtitle("Sexo e idade dos clientes")
            plot3<-ggplotly(plot3)
            plot3
        })
        
        if(input$periodos==4){
            # Por mes
            output$qtde1<-renderDygraph({
                faxinas<-faxinas%>%filter(`Ocorreu?`=="TRUE")%>%mutate(fax=1)
                
                x<-data.frame(Data=seq.POSIXt(from=min(faxinas$Data), to=max(faxinas$Data),by = "day"),QTDE=0)
                faxinas<-full_join(faxinas,x,by=c("Data"="Data"))
                faxinas<-faxinas%>%select(-QTDE)
                
                faxinas$fax[is.na(faxinas$fax)]<-0
                
                data <- as.xts(faxinas$fax,order.by=as.Date(faxinas$Data))
                weekly <- apply.monthly(data,sum)
                dyBase <- dygraphs::dygraph(weekly)
                
                dyBase%>%
                    # allow zooming
                    dygraphs::dyRangeSelector() %>% 
                    # make unzoom button
                    dygraphs::dyUnzoom() %>% 
                    # control legend width
                    dygraphs::dyLegend(width=weekly,show = c("follow"))
            })
            
            output$serie1<-renderPlotly({
                # Por mes
                faxinas<-faxinas%>%mutate(mes=yearmonth(Data))
                faxinas<-faxinas%>%filter(Data>"2018-01-03")
                faxinas<-faxinas%>%group_by(mes,Mulher)%>%count()%>%filter(!is.na(Mulher))
                x<-seq(from=0, to=max(faxinas$n), by=1)
                plot1<-ggplot(faxinas,aes(x=mes,y=n,color=Mulher))+
                    geom_point()+
                    geom_line()+
                    ggtitle("Quantidade de Faxinas por Mês para cada Mulher")+
                    xlab("Mês e Ano")+
                    ylab("Quantidade de Faxinas")+
                    scale_y_continuous(breaks=x)
                    geom_vline(xintercept=as.numeric(faxinas$mes[which(as.character(faxinas$mes)=="2018 set")]), linetype=4) #Colocar algum jeito melhor, que não interfira se mudarmos de tabela
                plot1<-ggplotly(plot1)
                plot1
            })
            
            output$horas1<-renderPlotly({
                #Por Mês
                faxinas<-faxinas%>%filter(`Ocorreu?`=="TRUE")%>%mutate(fax=1)
                
                x<-data.frame(Data=seq.POSIXt(from=min(faxinas$Data), to=max(faxinas$Data),by = "day"),QTDE=0)
                faxinas<-full_join(faxinas,x,by=c("Data"="Data"))
                faxinas<-faxinas%>%select(-QTDE)
                
                faxinas$fax[is.na(faxinas$fax)]<-0
                faxinas$mesano<-format(as.Date(faxinas$Data), "%Y-%m")
                faxinas<-faxinas%>%mutate(mes=yearmonth(Data))
                faxinas$disponibilidade = NA
                
                faxinas$disponibilidade[which(faxinas$Tipo == "K")] = 1
                faxinas$disponibilidade[which(faxinas$Tipo != "K")] = 2 
                disponibilidade$Disponibilidade[is.na(disponibilidade$Disponibilidade)]<-0
                
                horasGERAL<-faxinas%>%group_by(Data)%>%summarise(PeriodosGastos=sum(disponibilidade))%>%filter(PeriodosGastos!="NA")
                
                horasMULHER<-faxinas%>%group_by(Data,Mulher)%>%summarise(PeriodosGastos=sum(disponibilidade))%>%filter(PeriodosGastos!="NA")
                
                dispGERAL<-disponibilidade%>%group_by(Data)%>%summarise(PeriodosDisponiveis=sum(Disponibilidade))
                
                dispMULHER<-disponibilidade%>%group_by(Data,Mulher)%>%summarise(PeriodosDisponiveis=sum(Disponibilidade))
                
                dispGERAL<-left_join(dispGERAL,horasGERAL,by=c("Data"="Data"))
                dispMULHER<-left_join(dispMULHER,horasMULHER,by=c("Data"="Data","Mulher"="Mulher"))
                
                dispGERAL$PeriodosGastos[is.na(dispGERAL$PeriodosGastos)]<-0
                dispMULHER$PeriodosGastos[is.na(dispMULHER$PeriodosGastos)]<-0
                
                dispGERAL<-dispGERAL%>%mutate(PeriodosPorc=PeriodosGastos/PeriodosDisponiveis)
                dispMULHER<-dispMULHER%>%mutate(PeriodosPorc=PeriodosGastos/PeriodosDisponiveis)
                dispGERAL$PeriodosPorc[is.nan(dispGERAL$PeriodosPorc)]<-0
                dispMULHER$PeriodosPorc[is.nan(dispMULHER$PeriodosPorc)]<-0
                
                faxinas$mesh <- as.Date(cut(faxinas$Data,
                                            breaks = "month"))
                
                faxinas$Week <- as.Date(cut(faxinas$Data,
                                            breaks = "week",
                                            start.on.monday = TRUE))
                
                horasGERALmes<-faxinas%>%group_by(mesh)%>%summarise(PeriodosGastos=sum(disponibilidade,na.rm = TRUE))
                
                horasMULHERmes<-faxinas%>%group_by(mesh,Mulher)%>%summarise(PeriodosGastos=sum(disponibilidade))%>%filter(PeriodosGastos!="NA")
                
                dispGERALmes<-disponibilidade%>%mutate(mesano=as.Date(cut(disponibilidade$Data,
                                                                          breaks = "month")))%>%group_by(mesano)%>%summarise(PeriodosDisponiveis=sum(Disponibilidade))
                
                dispMULHERmes<-disponibilidade%>%mutate(mesano=as.Date(cut(disponibilidade$Data,
                                                                           breaks = "month")))%>%group_by(mesano,Mulher)%>%summarise(PeriodosDisponiveis=sum(Disponibilidade))
                
                dispGERALmes<-left_join(dispGERALmes,horasGERALmes,by=c("mesano"="mesh"))
                
                dispMULHERmes<-left_join(dispMULHERmes,horasMULHERmes,by=c("mesano"="mesh","Mulher"="Mulher"))
                
                dispGERALmes$PeriodosGastos[is.na(dispGERALmes$PeriodosGastos)]<-0
                dispMULHERmes$PeriodosGastos[is.na(dispMULHERmes$PeriodosGastos)]<-0
                
                dispGERALmes<-dispGERALmes%>%mutate(PeriodosPorc=PeriodosGastos/PeriodosDisponiveis)
                dispMULHERmes<-dispMULHERmes%>%mutate(PeriodosPorc=PeriodosGastos/PeriodosDisponiveis)
                dispGERALmes$PeriodosPorc[is.nan(dispGERALmes$PeriodosPorc)]<-0
                dispMULHERmes$PeriodosPorc[is.nan(dispMULHERmes$PeriodosPorc)]<-0
                dispGERALmes = gather(dispGERALmes, "Disponibilidade", "Periodos", 2:3)
                h1<-ggplot(dispGERALmes)+
                    geom_line(aes(mesano,Periodos, col = Disponibilidade))+
                    ggtitle("Periodos de Faxina e Periodos Disponiveis")+
                    xlab("Mes e Ano") +
                    ylab("Numero de Periodos") 
                h1<-ggplotly(h1)
                h1
            
                })
            
            output$pred1<-renderDygraph({
                #por mês:
                faxinas<-faxinas%>%filter(`Ocorreu?`=="TRUE")%>%mutate(fax=1)
                
                x<-data.frame(Data=seq.POSIXt(from=min(faxinas$Data), to=max(faxinas$Data),by = "day"),QTDE=0)
                faxinas<-full_join(faxinas,x,by=c("Data"="Data"))
                faxinas<-faxinas%>%select(-QTDE)
                
                faxinas$fax[is.na(faxinas$fax)]<-0
                
                data <- as.xts(faxinas$fax,order.by=as.Date(faxinas$Data))
                weekly <- apply.monthly(data,sum)
                
                model<-auto.arima(weekly)
                
                forecast_obj<-forecast(model)
                
                tsweek<-gen_array(forecast_obj)
                y<-seq.Date(from=min(index(weekly)),by = "month",length.out = max(index(tsweek)))-3
                
                tsweek<-as.data.frame(tsweek)
                
                rownames(tsweek)<-as.vector(y)
                tsweek<-xts(tsweek,order.by = as.Date(y))
                
                
                dygraph(tsweek, main = "Previsão da Quantidade de Faxinas") %>% 
                    dyRangeSelector() %>%
                    dySeries(name = "actuals", label = "Observado") %>%
                    dySeries(c("lower","point_forecast","upper"), label = "Predito") %>%
                    dyLegend(show = "always",showZeroValues = TRUE) %>%
                    dyHighlight(highlightCircleSize = 5,highlightSeriesOpts = list(strokeWidth = 2)) %>%
                    dyOptions(gridLineColor = "grey",drawXAxis = FALSE)%>%
                    dyUnzoom()
                
            })
            
        }
        
        if(input$periodos==3){
            output$qtde1<-renderDygraph({
                #Por trimestre:
                faxinas<-faxinas%>%filter(`Ocorreu?`=="TRUE")%>%mutate(fax=1)
                
                x<-data.frame(Data=seq.POSIXt(from=min(faxinas$Data), to=max(faxinas$Data),by = "day"),QTDE=0)
                faxinas<-full_join(faxinas,x,by=c("Data"="Data"))
                faxinas<-faxinas%>%select(-QTDE)
                
                faxinas$fax[is.na(faxinas$fax)]<-0
                
                data <- as.xts(faxinas$fax,order.by=as.Date(faxinas$Data))
                weekly <- apply.quarterly(data,sum)
                dyBase <- dygraphs::dygraph(weekly)
                
                dyBase%>%
                    # allow zooming
                    dygraphs::dyRangeSelector() %>% 
                    # make unzoom button
                    dygraphs::dyUnzoom() %>% 
                    # control legend width
                    dygraphs::dyLegend(width=weekly,show = c("follow"))
            })
            
            output$pred1<-renderDygraph({
                #por trimestre:
                faxinas<-faxinas%>%filter(`Ocorreu?`=="TRUE")%>%mutate(fax=1)
                
                x<-data.frame(Data=seq.POSIXt(from=min(faxinas$Data), to=max(faxinas$Data),by = "day"),QTDE=0)
                faxinas<-full_join(faxinas,x,by=c("Data"="Data"))
                faxinas<-faxinas%>%select(-QTDE)
                
                faxinas$fax[is.na(faxinas$fax)]<-0
                
                data <- as.xts(faxinas$fax,order.by=as.Date(faxinas$Data))
                weekly <- apply.quarterly(data,sum)
                
                model<-auto.arima(weekly)
                
                forecast_obj<-forecast(model)
                
                tsweek<-gen_array(forecast_obj)
                y<-seq.Date(from=min(index(weekly)),by = "quarter",length.out = max(index(tsweek)))
                
                tsweek<-as.data.frame(tsweek)
                
                rownames(tsweek)<-as.vector(y)
                tsweek<-xts(tsweek,order.by = y)
                
                
                dygraph(tsweek, main = "Previsão da Quantidade de Faxinas") %>% 
                    dyRangeSelector() %>%
                    dySeries(name = "actuals", label = "Observado") %>%
                    dySeries(c("lower","point_forecast","upper"), label = "Predito") %>%
                    dyLegend(show = "always",showZeroValues = TRUE) %>%
                    dyHighlight(highlightCircleSize = 5,highlightSeriesOpts = list(strokeWidth = 2)) %>%
                    dyOptions(gridLineColor = "grey",drawXAxis = FALSE)%>%
                    dyUnzoom()
                
            })
        }
        
        if(input$periodos==5){
            output$qtde1<-renderDygraph({
                #Por ano:
                faxinas<-faxinas%>%filter(`Ocorreu?`=="TRUE")%>%mutate(fax=1)
                
                x<-data.frame(Data=seq.POSIXt(from=min(faxinas$Data), to=max(faxinas$Data),by = "day"),QTDE=0)
                faxinas<-full_join(faxinas,x,by=c("Data"="Data"))
                faxinas<-faxinas%>%select(-QTDE)
                
                faxinas$fax[is.na(faxinas$fax)]<-0
                
                data <- as.xts(faxinas$fax,order.by=as.Date(faxinas$Data))
                weekly <- apply.yearly(data,sum)
                dyBase <- dygraphs::dygraph(weekly)
                
                dyBase%>%
                    # allow zooming
                    dygraphs::dyRangeSelector() %>% 
                    # make unzoom button
                    dygraphs::dyUnzoom() %>% 
                    # control legend width
                    dygraphs::dyLegend(width=weekly,show = c("follow"))
            })
            
            output$pred1<-renderDygraph({
                #por ano:
                faxinas<-faxinas%>%filter(`Ocorreu?`=="TRUE")%>%mutate(fax=1)
                
                x<-data.frame(Data=seq.POSIXt(from=min(faxinas$Data), to=max(faxinas$Data),by = "day"),QTDE=0)
                faxinas<-full_join(faxinas,x,by=c("Data"="Data"))
                faxinas<-faxinas%>%select(-QTDE)
                
                faxinas$fax[is.na(faxinas$fax)]<-0
                
                data <- as.xts(faxinas$fax,order.by=as.Date(faxinas$Data))
                weekly <- apply.yearly(data,sum)
                
                model<-auto.arima(weekly)
                
                forecast_obj<-forecast(model)
                
                tsweek<-gen_array(forecast_obj)
                y<-seq.Date(from=min(index(weekly)),by = "year",length.out = length(index(tsweek)))
                
                tsweek<-as.data.frame(tsweek)
                
                rownames(tsweek)<-as.vector(y)
                tsweek<-xts(tsweek,order.by = y)
                
                
                dygraph(tsweek, main = "Previsão da Quantidade de Faxinas") %>% 
                    dyRangeSelector() %>%
                    dySeries(name = "actuals", label = "Observado") %>%
                    dySeries(c("lower","point_forecast","upper"), label = "Predito") %>%
                    dyLegend(show = "always",showZeroValues = TRUE) %>%
                    dyHighlight(highlightCircleSize = 5,highlightSeriesOpts = list(strokeWidth = 2)) %>%
                    dyOptions(gridLineColor = "grey",drawXAxis = FALSE)%>%
                    dyUnzoom()
                
            })
        }
        
        output$clientes4<-renderPlot({
            planilha_cliente_data = faxinas[order(faxinas$Cliente, faxinas$Data), ]
            planilha_cliente_data = planilha_cliente_data %>% filter(!is.na(planilha_cliente_data$Cliente))
            planilha_cliente_data = planilha_cliente_data[!duplicated(planilha_cliente_data$Cliente),]
            planilha_cliente_data$MesAno = yearmonth(planilha_cliente_data$Data)
            
            a <- ggplot(planilha_cliente_data %>% mutate(n=1), 
                        aes(x = MesAno, y = n)) + 
              stat_summary(fun.y="sum", geom="bar", fill = "dodgerblue") + 
              ggtitle("Clientes novos") + xlab("Data") + 
              ylab("Quantidade de clientes novos") + 
              theme(axis.text.x = element_text(angle = 90)) + 
              scale_y_continuous(breaks=seq(0, 40, 1)) + 
              scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
            
            a
        })
        
        output$clientes1<-renderDataTable({
            #Faxinas por clientes
          Encoding(x = faxinas$Mulher) <- "UTF-8"
          Encoding(x = faxinas$Cliente) <- "UTF-8"
          Encoding(x = faxinas$Instituto) <- "UTF-8"
          faxinas$Mulher <- iconv(x = faxinas$Mulher, 
                                  from = "UTF-8", 
                                  to = "UTF-8", sub = "")
          faxinas$Cliente <- iconv(x = faxinas$Cliente, 
                                   from = "UTF-8", 
                                   to = "UTF-8", sub = "")
          faxinas$Instituto <- iconv(x = faxinas$Instituto, 
                                     from = "UTF-8", 
                                     to = "UTF-8", sub = "")
            faxinas<-faxinas%>%filter(`Ocorreu?`=="TRUE")%>%filter(Cliente!="NA")%>%mutate(fax=1)
            
            faxinas2<-faxinas %>%group_by(Cliente)%>%summarise(soma=sum(fax))%>%select("Cliente"=Cliente,"Faxinas"=soma)%>%arrange(-Faxinas)
            
            datatable(faxinas2, options = list(pageLength = 5))
        })
        
    })
}
