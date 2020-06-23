#https://shiny.rstudio.com/gallery/download-knitr-reports.html

#pacotes necessários para o código
packages <- c("tidyverse", "sf", "mapview", "lubridate","forecast", "readxl",
              "tseries", "curl", "DT", "plotly", "leaflet", "geosphere",
              "shiny", "tsibble", "zoo", "xts", "rsconnect", "dygraphs")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

#addResourcePath("Pureco", getwd())

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
library(readxl)

source("Tabs.R")

server <- function(input,output,session){
  
  
  ## passar tudo pra ja ter limpado as tabelas lendo o arquivo????
  
  observe({
    #         data <- datasetServer("data")
    #         data2 <- datasetServer("data2")
    #      
    # se nao tiver colocado as planilhas retorna nulo
    faxinas = input$faxina
    
    disponibilidade = input$disponibilidade
    
    #     
    #     # leitura do banco de dados de faxinas
    #     faxinas = read_csv2(data$datapath,locale = locale(encoding = "latin1"))
    #     # 
    #     # o intervalo de datas posso deixar
    #     faxinas$Data<-as.POSIXct(faxinas$Data, "UTC", format="%d/%m/%Y")
    #     faxinas<-faxinas%>%filter(Data>=input$selecionarperiodo & Data<input$selecionarperiodo2)
    #  
    #     # faxinas$`Dia da Semana`<- weekdays(faxinas$Data)
    #     
    #     #planilha de faxina 2018-2019
    #     # faxinas  <- read_xlsx(data$datapath, sheet=1, col_names = TRUE, skip = 1)
    #     #precisa fazer essa conversao
    #     # faxinas <- faxinas %>% mutate(Data = as.numeric(Data)) %>% 
    #     #     mutate(Data = excel_numeric_to_date(Data))
    #     
    #planilha de faxina 2019-2020
    #nao precisa fazer conversao de data, planilhas ja foram limpas
    #faxinas  <- read_csv(data$datapath)
    #planilha de faxina 2019-2020
    #nao precisa fazer conversao de data, planilhas ja foram limpas
    # vou ter que fazer um if aq se for arquivo xlsx ler xlsx fazer essas manipulacoes aq
    
    faxinas  <- read_csv("/home/elizemiku/Documents/PURECO-BAS/Pureco/dados/faxinas.csv")
    
    ### Tentativa de ja manipular os dados diretamente por aqui 
    #     if(grepl("2018-2019", data$datapath)){
    #         ## manipulação dos dados 
    #         faxinas <- faxinas%>%
    #             mutate(Data = as.numeric(Data), `Ocorreu?` = as.logical(`Ocorreu?`)) %>%
    #             mutate(Data = convert_to_date(Data)) %>%
    #             mutate(Val    or = as.numeric(Valor),
    #                    `Ocorreu?` = as.character(`Ocorreu?`),
    #                    `Feedback Colhido?` = as.character(`Feedback Colhido?`)
    #             ) %>%
    #             mutate_at(c("Cliente", "Endereço"), 
    #                       funs(ifelse(. == "-" |. == "--" | . == "/" | . == "ccc" | . == "rua", NA, .))) %>%
    #             select(c(1:11)) %>% rename(Comentarios = "...11") %>%
    #             remove_empty("rows")
    #         
    #         faxinas[faxinas == TRUE] <- "Sim"
    #         faxinas[faxinas == FALSE] <- "Não"
    #         faxinas$Data <- format(faxinas$Data, "%d/%m/%Y")
    #         faxinas$Valor <- formatC(faxinas$Valor, format = "f", digits = 2, big.mark = ",")
    #         faxinas$Cliente <- str_trim(str_to_title(faxinas$Cliente))
    #         faxinas$Comentarios <- str_trim(faxinas$Comentarios)
    #         faxinas$Endereço <-  str_replace_all(faxinas$Endereço, "R\\.", "Rua ")
    #         faxinas$Endereço <- str_replace_all(faxinas$Endereço, "R ", "Rua ")
    #         faxinas$Endereço <- str_trim(str_to_title(faxinas$Endereço))
    #     }
    # }    
    
    
    faxinas$Data <- as.POSIXct(faxinas$Data, "UTC", format = "%d/%m/%Y")    
    
    faxinas<-faxinas%>%filter(Data>=input$selecionarperiodo & Data<input$selecionarperiodo2)
    
    faxinas$`Dia da Semana`<- weekdays(faxinas$Data, abbreviate = FALSE)
    semana <- c("segunda", "terça", "quarta", "quinta", "sexta", "sábado", "domingo")
    faxinas$`Dia da Semana` <- factor(faxinas$`Dia da Semana`, ordered =TRUE, levels=semana)
    
    # selecionando período das análises para trabalhar com os gráficos nesse período
    # faxinas$Data <- as.Date(faxinas$Data, format = "%d/%m/%Y")
    
    
    # leitura do banco de dados de disponibilidade
    disponibilidade <- read_csv("/home/elizemiku/Documents/PURECO-BAS/Pureco/dados/disponibilidade.csv")
    disponibilidade$Data<-as.POSIXct(disponibilidade$Data, "UTC", format="%d/%m/%Y")
    disponibilidade<-disponibilidade%>%filter(Data>=input$selecionarperiodo & Data<input$selecionarperiodo2)
    
    # se o botao de inicio foi apertado:
    if(input$botao != 0){
      
      # funcao que faz aparecer a imagem do pureco
      output$inicio <- renderText({
        ##imagem do pureco
        src = "https://static.wixstatic.com/media/02e186_1928f72d50254d83a45117a9d6dc5332~mv2_d_1600_1600_s_2.png/v1/fill/w_400,h_400,al_c,q_80,usm_0.66_1.00_0.01/02e186_1928f72d50254d83a45117a9d6dc5332~mv2_d_1600_1600_s_2.webp"
        c('<img src="', src, '">')
      })
      
      output$infgeral1parte1<-renderPlotly({
        
        
        # Gráfico de Faxinas por dia da semana 
        # valor acumulado ao longo do periodo inserido para análise 
        # boxplot que faz parecido tentar o mesmo aqui stat_summary(
        # ggplot(faxinas %>% filter(Mulher != "NA") %>% mutate(Quantidade = 1) %>%
        #          group_by(`Dia da Semana`) %>% select(`Dia da Semana`, Quantidade) %>%
        #          mutate(Quantidade = cumsum(Quantidade)),
        #        aes(x = `Dia da Semana`, y = Quantidade, fill = `Dia da Semana`)) +
        #   geom_boxplot() 
        
        # quantidade totais de faxinas feitas pelo pureco de todos os anos 
        g1<-ggplot(faxinas %>% filter(Mulher != "NA") %>% mutate(Quantidade = 1),
                   aes(x =`Dia da Semana`,y = Quantidade, fill = `Dia da Semana`))+
          stat_summary(fun = "sum", geom = "bar") +
          ggtitle("Quantidade de Faxinas por Dia da Semana") +
          ylab("Quantidade de Faxinas") +
          theme(legend.position = 'none',
                axis.line = element_line(colour = "black"),
                panel.background = element_rect(fill = "white", size = 2),
                panel.grid.major = element_line(colour = "gray", 
                                                size = 1, linetype = "solid"),
                panel.grid.minor = element_line(colour = "gray", 
                                                size = 1, linetype = "solid")) + 
          scale_fill_viridis_d()
        
        g1<-ggplotly(g1, tooltip = c("x", "y"))
        
        g1
        
      })
      
      output$infgeral2parte1<-renderPlotly({
        
        # Gráfico de Faxinas por dia Tipo e dia da semana 
        # valor acumulado ao longo do periodo inserido para análise 
        
        g2<-ggplot(faxinas%>%mutate(Quantidade=1)%>%
                     filter(Tipo!="NA" & `Ocorreu?`=="Sim"),
                   aes(x=`Dia da Semana`,y=Quantidade,fill=`Dia da Semana`))+
          stat_summary(fun ="sum", geom="bar")+ facet_wrap(~Tipo) +
          ggtitle("Quantidade de Faxinas por Tipo de faxina e Dia da Semana")+
          ylab("Quantidade de Faxinas") +
          theme(axis.text.x =  element_blank(),
                axis.line = element_line(colour = "black"),
                panel.background = element_rect(fill = "white", size = 2),
                panel.grid.major = element_line(colour = "gray", 
                                                size = 1, linetype = "solid"),
                panel.grid.minor = element_line(colour = "gray", 
                                                size = 1, linetype = "solid"),
                strip.background = element_rect(colour = "black", fill = "#99CCFF")) + 
          scale_fill_viridis_d()
        
        g2<-ggplotly(g2, tooltip = c("x", "y"))
        
        g2
      })
      
      output$infgeral3parte2<-DT::renderDataTable({
        # Faxinas por mes
        faxinas2<-faxinas%>%filter(`Ocorreu?`=="Sim")%>%
          mutate(fax=1)

        x<-data.frame(Data=seq.POSIXt(from=min(faxinas2$Data), to=max(faxinas2$Data),
                                      by = "day"),QTDE=0)
        faxinas2<-full_join(faxinas2,x,by=c("Data"))
        faxinas2<-faxinas2%>%select(-QTDE)

        faxinas2$fax[is.na(faxinas2$fax)]<-0
        faxinas2$mesano<-format(as.Date(faxinas2$Data), "%m/%Y")
        faxinas2<-faxinas2%>%mutate(mes=yearmonth(Data))
        
        faxinas2<-faxinas2 %>%group_by(mesano) %>% 
        summarise(soma=sum(fax))%>%
          select("Mês-Ano"=mesano,"Número de Faxinas"=soma)

        faxinas2<- faxinas2[
        order(match(faxinas2$`Mês-Ano`, 
                    c("01/2018", "02/2018", "03/2018", "04/2018", "05/2018",
                      "06/2018", "07/2018", "08/2018", "09/2018", "10/2018",
                      "11/2018", "12/2018", "01/2019", "02/2019", "03/2019",
                      "04/2019", "05/2019", "06/2019", "07/2019", "08/2019",
                      "09/2019", "10/2019", "11/2019", "12/2019", "01/2020",
                      "02/2020", "03/2020"))),]
        
        datatable(faxinas2, options = list(pageLength = 5, 
                            language = list(search = 'Busca:')))

      })
      
      output$mulheres1<-renderPlotly({
        
        m1<-ggplot(faxinas%>%mutate(Quantidade=1)%>%
                     filter(Mulher != "NA" & Mulher != "Maria" & `Ocorreu?`=="Sim"),
                   aes(x=Mulher,y=Quantidade,fill=Mulher)) +
          stat_summary(fun="sum", geom="bar") + 
          ggtitle("Quantidade de Faxinas por Mulher")+
          ylab("Quantidade de faxinas") +
          theme(legend.position = 'none',
                axis.line = element_line(colour = "black"),
                panel.background = element_rect(fill = "white", size = 2),
                panel.grid.major = element_line(colour = "gray", 
                                                size = 1, linetype = "solid"),
                panel.grid.minor = element_line(colour = "gray", 
                                                size = 1, linetype = "solid")) +
          scale_fill_viridis_d()
        
        m1<-ggplotly(m1, tooltip = c("x", "y"))
        
        m1
      })
      
      output$mulheres2<-renderPlotly({
        
        m2<-ggplot(faxinas%>%mutate(Quantidade=1)%>%
                     filter(Mulher != "NA" & Mulher != "Maria" & `Ocorreu?`=="Sim"),
                   aes(x=`Dia da Semana`,y=Quantidade,fill=`Dia da Semana`))+
          stat_summary(fun="sum", geom="bar")+
          facet_wrap(~Mulher, scales = "free_x")+ 
          ggtitle("Quantidade de Faxinas por Mulher e Dia da Semana")+
          ylab("Quantidade de faxinas") +
          theme(axis.text.x =  element_blank(),
                axis.line = element_line(colour = "black"),
                panel.background = element_rect(fill = "white", size = 2),
                panel.grid.major = element_line(colour = "gray", 
                                                size = 1, linetype = "solid"),
                panel.grid.minor = element_line(colour = "gray", 
                                                size = 1, linetype = "solid"),
                strip.background = element_rect(colour = "black", fill = "#99CCFF")) +
          scale_fill_viridis_d() + scale_x_discrete(expand = c(0, 0))
        
        m2<-ggplotly(m2, tooltip = c("x", "y"))
        
        m2
      })
    }
    
  })    
  
  ## documento html: relatorio de dados
  output$Relatoriodados <- renderUI({
    tags$iframe(seamless="seamless", src= "Relatoriodados.html", 
                width=1350, height=1000, allowfullscreen = "true")
  })
  
  ## Quadros do tutorial 
  output$tutorial <- renderUI({
    
    p("Um fator importante para melhorar a análise estatística dos dados do PURECO 
      é padronizar o jeito que as informações coletadas do aplicativo são inseridas 
      na tabela. A planilha faxinas atual deve manter o seu formato e poderia seguir alguns 
      conselhos para a padronização:",
      style="padding:25px;background-color:LightBlue;
      border-top: 1px solid black;border-right:1px solid black;
      border-left:1px solid black; border-bottom: 1px solid black;color:black;text-align:center",
      hr(),
      p("Ideias para melhorar as planilhas:",style="font-size:18px;color:Navy;
       text-align:center"), 
      br(),
      p("Na coluna endereços anotar os endereços corretamente para facilitar a produção de gráfico com mapas.
      Escolher apenas Rua para o nome de ruas, e apenas Avenida para o nome de avenidas, seguir este padrão e não anotar o endereço sem as iniciais: Rua ou Avenida.",
        style="font-size:14px;color:black;padding:10px;background-color:PaleTurquoise"),
      p("Colocar dados numéricos sempre como 5.0 ; por exemplo.", 
        style="font-size:14px;color:black;padding:10px;background-color:PaleTurquoise"),
      p("Evitar preencher poucas colunas e deixar outras em branco.", 
        style="font-size:14px;color:black;padding:10px;background-color:PaleTurquoise"),
      p("Colocar apenas o número na coluna valor, invés de R$ antes do valor.", 
        style="font-size:14px;color:black;padding:10px;background-color:PaleTurquoise"),
      p("Padronizar o espaço entre as palavras escritas nas colunas 
      para que não falte espaço ou sobre espaço entre as palavras, principalmente por causa das
        anotações de comentários.",
        style="font-size:14px;color:black;padding:10px;background-color:PaleTurquoise"),
    )
    
  })
  
}  
