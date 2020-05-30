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
    data = input$faxina
    if(is.null(data))
      return(NULL)
    
    data2 = input$disponibilidade
    if(is.null(data2))
      return(NULL)
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
    if(grepl("\\.csv$", data$datapath)){
      faxinas  <- read_csv(data$datapath)
    }
    else{
      faxinas  <- read_xlsx(data$datapath)
    }    
    
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
      g1<-ggplot(faxinas %>% filter(Mulher != "NA") %>% mutate(Quantidade = 1),
                 aes(x =`Dia da Semana`,y = Quantidade,fill=`Dia da Semana`))+
        stat_summary(fun = "sum", geom = "bar") + 
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
      # Gráfico de Faxinas por dia da semana de acordo com o tipo de faxina, muda essa parte
      #ggplot(faxinas%>%mutate(Quantidade=1)%>% mutate(`Ocorreu?` = as.logical(as.integer(teste2 $`Ocorreu?`))
      #pra planilha de 2018-2020 nao precisa
      g2<-ggplot(faxinas%>%mutate(Quantidade=1)%>%
                   filter(Tipo!="NA" & `Ocorreu?`=="Sim"),
                 aes(x=`Dia da Semana`,y=Quantidade,fill=`Dia da Semana`))+
        stat_summary(fun ="sum", geom="bar")+
        facet_wrap(~Tipo) +
        ggtitle("Quantidade de Faxinas por Tipo de faxina e Dia da Semana")+
        ylab("Quantidade de Faxinas") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
      
      g2<-ggplotly(g2)
      
      g2
    })
    
  })    
  output$Relatoriodados <- renderUI({
    tags$iframe(seamless="seamless", src= "Relatoriodados.html", 
                width=1350, height=1000, allowfullscreen = "true")
  })
  
  
}  