# Códigos do server shiny 

## opção para não aparecer warnings dos summarize
options(dplyr.summarise.inform = FALSE)

# não precisar colocar os install, o pacote rsconnect já instala ao renderizar para o shinyapps.io
# instalar pacotes somente no console
## Carregando pacotes
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
library(shinyWidgets)
library(shinythemes)
library(DT)
library(zoo)
library(xts)
library(rsconnect)
library(dygraphs)
library(htmltools)
library(readxl)

# carregando o codigo com as modifacoes para os graficos
source("faxinas.R")
# carregando o codigo com todas configuracoes para o ui
source("Tabs.R")
# carregando o codigo com os temas dos graficos
source("Temas.R")
## modules
# source("Secoes.R")

# refatorar codigo aq
# função do server 
server <- function(input, output, session) {
  
  observe({
      
    ## colocando os inputs, funções de entradas das tabelas 
    faxinas = input$faxina
    disponibilidade = input$disponibilidade
    
    ## lendo as tabelas 
    
    # faxinas  <- read_csv("www/faxinas.csv") # informações de faxinas 
    faxinas <- carregando_dados()
    
    disponibilidade <-read_csv("www/disponibilidade.csv") # disponibilidade das faxineiras
    
    ## conversao das datas das tabelas para o input$selecionarperiodo deixar a data em formato brasileiro   
  
    # faxinas$Data <- as.POSIXct(faxinas$Data, "UTC", format = "%d/%m/%Y")
    faxinas <- faxinas %>% 
      filter(Data >= input$selecionarperiodo & Data < input$selecionarperiodo2)
    
    disponibilidade$Data <-as.POSIXct(disponibilidade$Data, "UTC", format = "%d/%m/%Y")
    disponibilidade <-disponibilidade %>% 
      filter(Data >= input$selecionarperiodo & Data < input$selecionarperiodo2)
    
    # Trabalhando com a tabela faxinas 
    
    ## criando as colunas de datas especificas
    
    # faxinas <- faxinas %>% 
    #   mutate(Semana = wday(Data, label = TRUE, abbr = TRUE),
    #          mes = month(Data, label = TRUE, abbr = TRUE),
    #          ano = year(Data))
    
    # se o botao de inicio foi apertado:
    if (input$botao != 0) {
      # funcao que faz aparecer a imagem do pureco
      output$inicio <- renderText({
        ##imagem do pureco
        src = "https://static.wixstatic.com/media/02e186_1928f72d50254d83a45117a9d6dc5332~mv2_d_1600_1600_s_2.png/v1/fill/w_400,h_400,al_c,q_80,usm_0.66_1.00_0.01/02e186_1928f72d50254d83a45117a9d6dc5332~mv2_d_1600_1600_s_2.webp"
        c('<img src="', src, '">')
      })
      
     # estudar modules no futuro  
     # callModule(faxinasgeraisServer, "gerais", faxinas)
      
      observeEvent(input$escolhido, {
          
          faxinas_escolha <- reactive(
            
          faxinas_secao1 (faxinas, input$ano, input$eixo_x, input$eixo_y)
            # faxinas %>%
            #   filter(ano %in% !!input$ano) %>%
            #   group_by(ano, !!input$eixo_x) %>%
            #   summarize("Quantidade" = sum(Quantidade)) %>%
            #   mutate("Proporcao" = round(Quantidade/sum(Quantidade), 2)) %>%
            #   select(ano, !!input$eixo_x, !!input$eixo_y)
           
          )
            
        #   output$infgeral1parte1 <- renderTable({
        # 
        #     faxinas_escolha_f()
        # 
        #   })
        #   
        # }  
            # if(input$eixo_y == "Proporcao"){
            #   
            #   faxinas %>%
            #     mutate(Quantidade = 1) %>%
            #     filter(Mulher != "NA" &
            #              input$eixo_x != "NA" &
            #              ano %in% input$ano) %>%
            #     group_by(ano, input$eixo_x)  %>%
            #     summarize(Quantidade = sum(Quantidade)) %>%
            #     mutate(Prop = round(Quantidade/sum(Quantidade), 2))
            #   
            # }
         
          # TEM QUE ARRUMAR AQ NAO TA DANDO CERTO COM A PROPORCAO
          output$infgeral1parte1 <- renderPlotly({
            g1 <- ggplot(faxinas_escolha() %>%
                          summarize(Quantidade = sum(Quantidade)) %>%
                          mutate(Proporcao = round(Quantidade/sum(Quantidade), 2)),
                          aes_string(x =  input$eixo_x , y =  input$eixo_y))
            
            if (input$grafico == "Barras"){
              g1 <- g1 + geom_bar(stat = "identity", position = "stack",
                                  aes_string(fill = input$eixo_x)) +
              facet_grid(~ano, scales = "free_x") +
              xlab("Dia da Semana") +
              ylab("Quantidade de Faxinas") +
              ggtitle("Quantidade de Faxinas por Dia da Semana e Ano") +
              scale_fill_viridis_d() +
              tema_facets
            }
            
            else if (input$grafico == "Linhas"){
              g1 <- g1 + geom_line(aes(group=1), col = "blue") +
                facet_grid(~ano) +
                xlab("Dia da Semana") +
                ylab("Quantidade de Faxinas") +
                ggtitle("Quantidade de Faxinas por Dia da Semana e Ano") +
                tema_facets
            }    
            # esse grafico usa cumulative inves de sum nao da pra usar proporcao
            else if (input$grafico == "Boxplot" & input$eixo_y == "Quantidade"){
              g1 <- ggplot(faxinas_escolha()  %>%
                             summarize(Quantidade = cumsum(Quantidade)),
                           aes_string(x =  input$eixo_x, y = input$eixo_y, 
                                      fill = input$eixo_x)) + 
                geom_boxplot() +
                facet_grid(~ano, scales = "free_x") +
                xlab("Dia da Semana") +
                ylab("Quantidade de Faxinas") +
                ggtitle("Quantidade de Faxinas por Dia da Semana e Ano") +
                scale_fill_viridis_d() +
                tema_facets
            }               
          
          g1 <- ggplotly(g1) %>%
            layout(showlegend = FALSE)
          
          g1
        })
        
      })  
        
        # else{
        #   
        #   print("a")
          # faxinas_escolha <- reactive(
          #   
          #   if(input$eixo_y == "Quantidade"){
          #     
          #     faxinas %>%
          #       mutate(Quantidade = 1) %>%
          #       filter(Mulher != "NA" &
          #                input$eixo_x != "NA" &
          #                ano %in% input$ano) %>%
          #       group_by(ano, input$eixo_x)  %>%
          #       summarize(Quantidade = cumsum(Quantidade)) %>%
          #       arrange(ano, Quantidade)
          #   }
          #   
          #   if(input$eixo_y == "Proporção"){
          #     
          #     faxinas %>%
          #       mutate(Quantidade = 1) %>%
          #       filter(Mulher != "NA" &
          #                input$eixo_x != "NA" &
          #                ano %in% input$ano) %>%
          #       group_by(ano, input$eixo_x)  %>%
          #       summarize(Quantidade = cumsum(Quantidade)) %>%
          #       mutate(Prop = round(Quantidade/sum(Quantidade), 2))
          #   }
          # )
          # output$infgeral1parte1 <- renderPlotly({
          #   g1b <- ggplot(faxinas_escolha(),
          #                 aes_string(x = input$eixo_x,
          #                            y = input$eixo_y,
          #                            fill = input$eixo_x)) +
          #     geom_boxplot() +
          #     facet_grid( ~ ano, scales = "free_x") +
          #     xlab("Dia da Semana") +
          #     ylab("Quantidade de Faxinas") +
          #     ggtitle("Quantidade de Faxinas por Dia da Semana e Ano") +
          #     scale_fill_viridis_d(aesthetics = "fill") +
          #     tema_facets
          #   
          #   g1b <- ggplotly(g1b) %>%
          #     layout(showlegend = FALSE)
          #   
          #   g1b
          # })
          
        
        
        # else if(input$grafico == "Linhas"){
        #   
        #   faxinas_escolha <- reactive(
        #     
        #     if(input$eixo_y == "Quantidade"){
        #       
        #       faxinas %>%
        #         mutate(Quantidade = 1) %>%
        #         filter(Mulher != "NA" &
        #                  input$eixo_x != "NA" &
        #                  ano %in% input$ano) %>%
        #         group_by(ano, input$eixo_x)  %>%
        #         summarize(Quantidade = sum(Quantidade)) %>%
        #         arrange(ano, Quantidade)
        #     }
        #     
        #    else if(input$eixo_y == "Proporção"){
        #       
        #       faxinas %>%
        #         mutate(Quantidade = 1) %>%
        #         filter(Mulher != "NA" &
        #                  input$eixo_x != "NA" &
        #                  ano %in% input$ano) %>%
        #         group_by(ano, input$eixo_x)  %>%
        #         summarize(Quantidade = sum(Quantidade)) %>%
        #         mutate(Prop = round(Quantidade/sum(Quantidade), 2))
        #     }
        #   )
        #   
        #   ## plotando o grafico depois de escolher as opcoes
        #   output$infgeral1parte1 <- renderPlotly({
        #     g1l <- ggplot(faxinas_escolha(),
        #                   aes_string(x = input$eixo_x,
        #                              y = input$eixo_y,
        #                              fill = input$eixo_x)) +
        #       geom_line() +
        #       facet_grid( ~ ano, scales = "free_x") +
        #       xlab("Dia da Semana") +
        #       ylab("Quantidade de Faxinas") +
        #       ggtitle("Quantidade de Faxinas por Dia da Semana e Ano") +
        #       scale_fill_viridis_d(aesthetics = "fill") +
        #       tema_facets
        #     
        #     g1l <- ggplotly(g1l) %>%
        #       layout(showlegend = FALSE)
        #     
        #     g1l
        #   })
        # }
        
      
      ## secao 2 Mulheres (colocar tipo na secao anterior)
      output$infgeral2parte1 <- renderPlotly({
        # Gráfico de Faxinas por dia Tipo e dia da semana
        # valor acumulado ao longo do periodo inserido para análise
        
        g2 <- ggplot(faxinas %>% mutate(Quantidade = 1) %>%
                       filter(Tipo != "NA" & `Ocorreu?` == "Sim" & `Dia da Semana` != is.na(NA)),
                     aes(
                       x = `Dia da Semana`,
                       y = Quantidade,
                       fill = `Dia da Semana`
                     )) +
          stat_summary(fun = "sum", geom = "bar") + 
          ggtitle("Quantidade de Faxinas por Tipo de faxina e Dia da Semana") +
          ylab("Quantidade de Faxinas") + 
          theme(axis.text.x =  element_blank(),
                axis.title.x = element_blank(),
                strip.background = element_rect(colour = "black", fill = "#99CCFF")) +
          tema_geral + 
          scale_fill_viridis_d()
        
        g2 <- g2 + facet_wrap(~Tipo) 
        
        g2 <- ggplotly(g2, tooltip = c("x", "y"))
        
        g2
        
      })
      
      output$infgeral3parte2 <- renderPlotly({
        # Faxinas por mes
        faxinas2 <- faxinas %>% filter(`Ocorreu?` == "Sim") %>%
          mutate(fax = 1)
        
        x <- data.frame(Data = seq.POSIXt(from = min(faxinas2$Data),
                                          to = max(faxinas2$Data),
                                          by = "day"),QTDE = 0)
        
        faxinas2 <- full_join(faxinas2, x, by = c("Data"))
        faxinas2 <- faxinas2 %>% select(-QTDE)
        faxinas2$fax[is.na(faxinas2$fax)] <- 0
        
        faxinas2 <- faxinas2  %>% 
          mutate(`Mês` = month(Data, label = TRUE, abbr = FALSE), 
                 ano = year(Data)) %>%
          group_by(ano,`Mês`) %>%
          summarize(Quantidade = sum(fax)) %>% 
          mutate(Quantidade = as.integer(Quantidade)) %>%
          select(`Mês`, Quantidade, ano ) 
        
       # grafico de faxinas por meses e por ano  
       f2 <- ggplot(faxinas2, aes(x=`Mês`, y = Quantidade, group = 1)) +
         geom_line(col = "blue") +
         facet_grid(~ano) + ggtitle("Quantidade de Faxinas por Ano") +
         ylab("Quantidades de faxinas por mês") + tema_faxinas2 +
         theme(strip.background = element_rect(colour = "black", fill = "#99CCFF"),
               axis.text.x = element_text(angle = 20, size = 8),
               axis.title.x = element_blank())

       f2 <- ggplotly(f2, tooltip = c("x", "y"))

       f2
        
      })
      
      output$mulheres1 <- renderPlotly({
        
        m1 <- ggplot(faxinas %>% mutate(Quantidade = 1) %>%
                       filter(Mulher != "NA" & Mulher != "Maria" & 
                                `Ocorreu?` == "Sim"),
                     aes(x = Mulher,y = Quantidade,fill = Mulher)) +
          stat_summary(fun = "sum", geom = "bar") +
          ggtitle("Quantidade de Faxinas por Mulher") +
          ylab("Quantidade de faxinas") +
          theme(
            legend.position = 'none',
            axis.line = element_line(colour = "black"),
            panel.background = element_rect(fill = "white", size = 2),
            panel.grid.major = element_line(
              colour = "gray",
              size = 1,
              linetype = "solid"
            ),
            panel.grid.minor = element_line(
              colour = "gray",
              size = 1,
              linetype = "solid"
            )
          ) + scale_fill_viridis_d()
        
        m1 <- ggplotly(m1, tooltip = c("x", "y"))
        
        m1
        
      })
      
      output$mulheres2 <- renderPlotly({
        
        m2 <- ggplot(
          faxinas %>% mutate(Quantidade = 1) %>%
            filter(Mulher != "NA" & Mulher != "Maria" & `Ocorreu?` == "Sim"
                   & `Dia da Semana` != is.na(NA)),
          aes(
            x = `Dia da Semana`,
            y = Quantidade,
            fill = `Dia da Semana`
          )
        ) +
          stat_summary(fun = "sum", geom = "bar") +
          facet_wrap( ~ Mulher, scales = "free_x") +
          ggtitle("Quantidade de Faxinas por Mulher e Dia da Semana") +
          ylab("Quantidade de faxinas") +
          theme(
            axis.text.x =  element_blank(),
            axis.line = element_line(colour = "black"),
            panel.background = element_rect(fill = "white", size = 2),
            panel.grid.major = element_line(
              colour = "gray",
              size = 1,
              linetype = "solid"
            ),
            panel.grid.minor = element_line(
              colour = "gray",
              size = 1,
              linetype = "solid"
            ),
            strip.background = element_rect(colour = "black", fill = "#99CCFF") 
          ) + scale_fill_viridis_d() + scale_x_discrete(expand = c(0, 0))
        
        m2 <- ggplotly(m2, tooltip = c("x", "y"))
        
        m2
        
      })
    }
  })
  
  
  ## documento html: relatorio de dados
  output$Relatoriodados <- renderUI({
    
    tags$iframe(
      seamless = "seamless",
      src = "Relatoriodados.html",
      width = 1350,
      height = 1000,
      allowfullscreen = "true")
    
  })
  
  ## Quadros do tutorial
  output$tutorial <- renderUI({
    
    p(
      "Um fator importante para melhorar a análise estatística dos dados do PURECO
      é padronizar o jeito que as informações coletadas do aplicativo são inseridas
      na tabela. A planilha faxinas atual deve manter o seu formato e poderia seguir alguns
      conselhos para a padronização:",
      style = "padding:25px;background-color:LightBlue;
      border-top: 1px solid black;border-right:1px solid black;
      border-left:1px solid black; border-bottom: 1px solid black;color:black;text-align:center",
      hr(),
      p(
        "Ideias para melhorar as planilhas:",
        style = "font-size:18px;color:Navy;
       text-align:center"
      ),
      br(),
      p(
        "Na coluna endereços anotar os endereços corretamente para facilitar a produção de gráfico com mapas.
      Escolher apenas Rua para o nome de ruas, e apenas Avenida para o nome de avenidas, seguir este padrão e não anotar o endereço sem as iniciais: Rua ou Avenida.",
        style = "font-size:14px;color:black;padding:10px;background-color:PaleTurquoise"
      ),
      p(
        "Colocar dados numéricos sempre como 5.0 ; por exemplo.",
        style = "font-size:14px;color:black;padding:10px;background-color:PaleTurquoise"
      ),
      p(
        "Evitar preencher poucas colunas e deixar outras em branco.",
        style = "font-size:14px;color:black;padding:10px;background-color:PaleTurquoise"
      ),
      p(
        "Colocar apenas o número na coluna valor, invés de R$ antes do valor.",
        style = "font-size:14px;color:black;padding:10px;background-color:PaleTurquoise"
      ),
      p(
        "Padronizar o espaço entre as palavras escritas nas colunas
      para que não falte espaço ou sobre espaço entre as palavras, principalmente por causa das
        anotações de comentários.",
        style = "font-size:14px;color:black;padding:10px;background-color:PaleTurquoise"
      )
    )
    
  })
  
  # depois testar se pega o rmd com runtime e flexdashboard
  # output$graficos <- renderUI({
  #   incldR
  # })
  
} 