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
library(lemon)
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

# carregando o codigo com as modifacoes para os graficos #

# codigo com as modificacoes dos dados de faxinas
source("faxinas.R")

# codigo com as modificacoes dos dados de disponibilidade
source("disponibilidade.R")

# carregando o codigo com todas configuracoes para o ui
source("Tabs.R")

# carregando o codigo com os temas dos graficos
source("Temas.R")

## modules
source("Secoes.R")

# refatorar codigo aq
# função do server 
server <- function(input, output, session) {
  
  observe({
      
    ## colocando os inputs, funções de entradas das tabelas 
    faxinas = input$faxina
    disponibilidade = input$disponibilidade
    
    ## lendo as tabelas 
    
    # faxinas  <- read_csv("www/faxinas.csv") # informações de faxinas 
    faxinas <- carregando_dados_f()
    
    disponibilidade <-carregando_dados_d() # disponibilidade das faxineiras
    
    ## conversao das datas das tabelas para o input$selecionarperiodo deixar a data em formato brasileiro   
  
    # faxinas$Data <- as.POSIXct(faxinas$Data, "UTC", format = "%d/%m/%Y")
    faxinas <- faxinas %>% 
      filter(Data >= input$selecionarperiodo & Data < input$selecionarperiodo2)
    
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
     # Eventos da secao 1 #
      lista_de_eventos <- reactive({
        list(input$escolhido, input$ano, input$eixo_x, input$eixo_y, input$grupo, input$grafico)
      })
      
      
      # pensar no futuro em como fazer observeEvent como um module 
      observeEvent(lista_de_eventos(), {
        
        if(input$escolhido == 1){  
            
            faxinas_escolha <- reactive(
              faxinas_secao1(faxinas, input$ano, input$eixo_x, input$grupo)
            )
            
            # mudancas no grafico
            output$infgeral1parte1 <- renderPlotly({
                
            if(is.null(input$ano)){
                return()
            }
            
            else if(2018 %in% input$ano && 
                    (input$grupo != "Nenhum" && input$grupo != "Ocorreu?" && 
                     input$grupo != "Valor")){
              showModal(modalDialog(
                title = "Aviso :",
                "Escolha os anos de 2019 ou 2020, pois essa informação não consta na planilha de 2018!",
                easyClose = TRUE,
                fade = TRUE,
                size = "s",
                footer = modalButton("Ok")
              ))
            }  
              
            else if((input$grafico == "Pontos" && input$grupo == "Nenhum")){
              showModal(modalDialog(
                title = "Aviso :",
                "Escolha uma opção adicional ou escolha outro tipo de gráfico!",
                easyClose = TRUE,
                fade = TRUE,
                size = "s",
                footer = modalButton("Ok")
              ))
            }

            else if(input$grafico == "Boxplot" && (input$eixo_y != "Valor"
                                                   || input$grupo != "Nenhum")){
                showModal(modalDialog(
                  title = "Aviso :",
                  "Escolha outra opção de gráfico ou selecione
                  o tipo numérico Valor sem nenhuma opção adicional!",
                  easyClose = TRUE,
                  fade = TRUE,
                  size = "s",
                  footer = modalButton("Ok")
                ))
              }

            else if(input$grafico == "Linhas" && input$grupo != "Nenhum"){
                showModal(modalDialog(
                  title = "Aviso :",
                  "Escolha outra opção de gráfico!",
                  easyClose = TRUE,
                  fade = TRUE,
                  size = "s",
                  footer = modalButton("Ok")
                ))
            }
             
            else if(input$grafico == "Pontos" && input$eixo_y != "Quantidade"){
              showModal(modalDialog(
                title = "Aviso :",
                "Para este gráfico, escolha por Quantidade!",
                easyClose = TRUE,
                fade = TRUE,
                size = "s",
                footer = modalButton("Ok")
              ))
            }

              else if(input$grafico != "Boxplot"  && input$eixo_y == "Valor"){
                showModal(modalDialog(
                  title = "Aviso :",
                  "Para este gráfico, escolha por Quantidade ou Proporção!",
                  easyClose = TRUE,
                  fade = TRUE,
                  size = "s",
                  footer = modalButton("Ok")
                ))
              }

            else{   
              
              if(input$grupo == "Nenhum"){  
      
                if (input$grafico == "Barras" && input$eixo_y != "Valor"){
                  g1 <- barplot_secao1(remove_faxinas_duplicadas(faxinas_escolha()), 
                                       input$eixo_x,
                                       input$eixo_y,
                                       input$eixo_x) + scale_fill_brewer(palette = "Set3")
                }
                
                else if (input$grafico == "Linhas"){
                  g1 <- lineplot_secao1(remove_faxinas_duplicadas(faxinas_escolha()), 
                                        input$eixo_x,
                                        input$eixo_y)
                }  
                
                
                else if (input$grafico == "Boxplot" & input$eixo_y == "Valor"){
                  g1 <- boxplot_secao1(faxinas_escolha(),
                                       input$eixo_x)
                }
              }
              
              else{
                
                  if (input$grafico == "Barras" & input$grupo != "Valor"){
                    g1 <- barplot_secao1(remove_faxinas_duplicadas(faxinas_escolha()), 
                                         input$eixo_x,
                                         input$eixo_y,
                                         input$grupo) + scale_fill_brewer(palette = "Set2")
                  }
                
                  else if (input$grafico == "Barras" & input$grupo == "Valor"){
                  g1 <- barplot_secao1(remove_faxinas_duplicadas(faxinas_escolha()),
                                       input$eixo_x,
                                       input$eixo_y,
                                       input$grupo)
                   }
                  
                  else if(input$grafico == "Pontos"){
                    g1 <- point_secao1(remove_faxinas_duplicadas(faxinas_escolha()), 
                                       input$eixo_x, 
                                       input$eixo_y, 
                                       input$grupo)
                  }
              }  
        
          g1 <- ggplotly(g1, tooltip = "text")
          
          g1
        }  
     })
    
    }    
  })  
        
    # Eventos da secao 2 #  
    lista_de_eventos2 <- reactive({
      list(input$escolhido_m, input$ano_m, input$eixo_x_m, input$eixo_y_m, 
           input$grafico_m, input$mulher, input$grupo_m)
    })
     
    observeEvent(lista_de_eventos2(), {
      
      if(input$escolhido_m == 1){  
        
        faxinas_escolha2 <- reactive(
          faxinas_secao2(faxinas, input$ano_m, input$eixo_x_m, input$mulher, input$grupo_m)
        )
        
        # mudancas no grafico, #ver opcoes de grafico e arrumar mensagens
        output$mulheres <- renderPlotly({
          
          if(is.null(input$ano_m) || is.null(input$mulher)){
            return()
          }
          
          
          else if((2018 %in% input$ano_m && input$grupo_m != "Nenhum" && 
                   input$grupo_m != "Ocorreu?" &&  input$grupo_m != "Valor") || 
                  (2018 %in% input$ano_m && input$eixo_x_m == "Remarcou")){
            showModal(modalDialog(
              title = "Aviso :",
              "Escolha os anos de 2019 ou 2020, pois essa informação não consta na planilha de 2018!",
              easyClose = TRUE,
              fade = TRUE,
              size = "s",
              footer = modalButton("Ok")
            ))
          }  
          
          else if(input$grafico_m == "Barras" && input$grupo_m != "Nenhum"
                  && input$eixo_x_m != "Colaboradora"){
            showModal(modalDialog(
              title = "Aviso :",
              "Escolha como ocorrência a opção Colaboradora!",
              easyClose = TRUE,
              fade = TRUE,
              size = "s",
              footer = modalButton("Ok")
            ))
          }
          
          else if(input$grafico_m == "Barras" && input$eixo_y_m == "Valor"){
            showModal(modalDialog(
              title = "Aviso :",
              "Escolha o tipo numérico por Quantidade ou Proporção!",
              easyClose = TRUE,
              fade = TRUE,
              size = "s",
              footer = modalButton("Ok")
            ))
          }  
          

          else if(input$grafico_m == "Linhas e Pontos" && input$grupo_m != "Nenhum"){
            showModal(modalDialog(
              title = "Aviso :",
              "Escolha outra opção de gráfico!",
              easyClose = TRUE,
              fade = TRUE,
              size = "s",
              footer = modalButton("Ok")
            ))
          }
          
          else if((input$grafico_m == "Boxplot" && input$eixo_x_m != "Colaboradora") || 
                  (input$grafico_m == "Boxplot" && input$eixo_y_m != "Valor") || 
                  (input$grafico_m == "Boxplot" && input$grupo_m != "Nenhum")){
            showModal(modalDialog(
              title = "Aviso :",
              "Escolha a ocorrência por Colaboradora, Tipo numérico por Valor e nenhuma opção
              adicional para visualizar o Boxplot!",
              easyClose = TRUE,
              fade = TRUE,
              size = "s",
              footer = modalButton("Ok")
            ))
          }

          else if((input$grafico_m == "Linhas e Pontos" && input$eixo_x_m == "Colaboradora") || 
                  (input$grafico_m == "Linhas e Pontos" && input$eixo_y_m == "Valor")){
            showModal(modalDialog(
              title = "Aviso :",
              "Escolha ocorrência por Dia da Semana ou por Mês e o Tipo numérico diferente de Valor !",
              easyClose = TRUE,
              fade = TRUE,
              size = "s",
              footer = modalButton("Ok")
            ))
          }  
          
          else if((input$grafico_m == "Barras" && input$eixo_x_m == "Remarcou"
                   && input$eixo_y_m != "Quantidade")){
            showModal(modalDialog(
              title = "Aviso :",
              "Escolha o tipo numérico por Quantidade!",
              easyClose = TRUE,
              fade = TRUE,
              size = "s",
              footer = modalButton("Ok")
            ))
          }  
          
          
          else{

            if(input$grafico_m == "Barras"){
                m1 <- barplot_secao2(faxinas_escolha2(), 
                                     input$eixo_x_m,
                                     input$eixo_y_m,
                                     input$grupo_m)
            }
            
            else if (input$grafico_m == "Linhas e Pontos" & input$grupo_m == "Nenhum"){
              m1 <- linepointplot_secao2(faxinas_escolha2(),input$eixo_x_m)
            }
            
            else if(input$grafico_m == "Boxplot" &&  
                    (input$grupo_m == "Nenhum" & input$eixo_y_m == "Valor")){
              m1 <- boxplot_secao2(faxinas_escolha2(), input$eixo_x_m)
            }

          
            m1 <- ggplotly(m1, tooltip = "text")
            
            m1
          }
        })
        
      }    
    })
    
    ## secao disponibilidade
    
    eventos_disponibilidade <- reactive({
      list(input$escolhido_d, input$grafico_d, input$ano_d, input$mulher_d)
    })
    
    observeEvent(eventos_disponibilidade(), {
      
        if(input$escolhido_d == 1){
          
          disponibilidade_s1 <- reactive(
            disponibilidade_c1(disponibilidade, input$ano_d)
          )
        
          disponibilidade_s2 <- reactive(
            disponibilidade_m1(disponibilidade, input$ano_d, input$mulher_d)
          )
          
        output$calendario <- renderPlotly({
          
          if(input$grafico_d == 1){
            
           d1 <- ggplotly(calendario_c(disponibilidade_s1(), input$ano_d),
                          tooltip = "text")
          }
          
          else if(input$grafico_d == 2){
            
            # mostra uma mensagem pois não consta o dados dessas das colaboradoras neste ano
            # analisando manualmente quais colaboradoras não tem informação em cada ano da planilha
            if((2018 %in% input$ano_d && input$mulher_d == "Ledinha") ||
               (2021 %in% input$ano_d && input$mulher_d == "Ledinha") || 
               (2021 %in% input$ano_d && input$mulher_d == "Zilza")){
              showModal(modalDialog(
                title = "Aviso :",
                "Escolha os anos de 2019 ou 2020, 
              pois essa informação não consta na planilha para este ano!",
                easyClose = TRUE,
                fade = TRUE,
                size = "s",
                footer = modalButton("Ok")
              ))
              return()
            }
            
            # como na planilha os anos da Marcela sao diferentes fiz outro if
            else if(input$ano_d != "2019" && input$mulher_d == "Marcela"){
              showModal(modalDialog(
                title = "Aviso :",
                "Escolha o ano de 2019, 
              pois essa informação não consta na planilha para este ano!",
                easyClose = TRUE,
                fade = TRUE,
                size = "s",
                footer = modalButton("Ok")
              ))
              return()    
            }     
                      
           
            # como na planilha os anos da Terezinha sao diferentes fiz outro if
            else if((2018 %in% input$ano_d && input$mulher_d == "Terezinha") || 
                    (2019 %in% input$ano_d && input$mulher_d == "Terezinha")){
              showModal(modalDialog(
                title = "Aviso :",
                "Escolha os anos de 2020 ou 2021, 
              pois essa informação não consta na planilha para este ano!",
                easyClose = TRUE,
                fade = TRUE,
                size = "s",
                footer = modalButton("Ok")
              ))
              return()    
            }
            
            else{
            d1 <- ggplotly(calendario_m(disponibilidade_s2(), input$ano_d, input$mulher_d),
                           tooltip = "text")
            
            }
          
            d1
          }  
        })
      }  
    })
    
    
  ## secao clientes

    lista_de_eventos3 <- reactive({
      list(input$escolhido_c, input$ano_c, input$eixo_x_c)
    })
    
    observeEvent(lista_de_eventos3(), {
    
      if(input$escolhido_c == 1){  
    
        faxinas_escolha3 <- reactive(
          faxinas_clientes(faxinas, input$ano_c, input$eixo_x_c)
        )
    
        output$clientes <- renderPlotly({
    
          if(is.null(input$ano_m)){
            return()
          }
          
          else{
            c1 <- barplot_clientes(faxinas_escolha3(),
                                   input$eixo_x_c)
          }
          
          c1 <-  ggplotly(c1, tooltip = "text")
          
          c1
        })
      }
      
    })  
          
    
  ### secao feedbacks
    
    lista_de_eventos4 <- reactive({
      list(input$escolhido_f, input$ano_f, input$eixo_x_f, input$grupo_f, input$mulher_f)
    })
    
    observeEvent(lista_de_eventos4(), {
      
      if(input$escolhido_f == 1){  
        
        faxinas_escolha4 <- reactive(
          faxinas_feedbacks(faxinas, input$ano_f,input$eixo_x_f, input$grupo_f, input$mulher_f)
        )
        
        output$feedbacks <- renderPlotly({
          
          if(is.null(input$ano_f) || is.null(input$mulher_f)){
            return()
          }
          
          else if((2018 %in% input$ano_f && input$eixo_x_f == "Onde foi colhido?") || 
                  (2018 %in% input$ano_f && input$grupo_f == "Onde foi colhido?")){
            showModal(modalDialog(
              title = "Aviso :",
              "Escolha os anos de 2019 ou 2020, pois essa informação não consta na planilha de 2018!",
              easyClose = TRUE,
              fade = TRUE,
              size = "s",
              footer = modalButton("Ok")
            ))
          return()  
          }  
          
          else if(input$eixo_x_f == "Onde foi colhido?" && input$grupo_f == "Onde foi colhido?"){
            showModal(modalDialog(
              title = "Aviso :",
              "Se escolher Onde foi colhido? na opção anterior, escolha Nenhum ou Colaboradora
              como opção adicional para visualizar os gráficos.",
              easyClose = TRUE,
              fade = TRUE,
              size = "s",
              footer = modalButton("Ok")
            ))
            return()  
          }  
          
          # # mostra uma mensagem pois não consta o dados dessas moças na planilha
          # if((2018 %in% input$ano_f && input$mulher_f == "Ledinha") ||
          #    (2020 %in% input$ano_f && input$mulher_f == "Ledinha") ||
          #    (2018 %in% input$ano_f && input$mulher_f == "Marcela") ||
          #    (2020 %in% input$ano_f && input$mulher_f == "Marcela") ||
          #    (2018 %in% input$ano_f && input$mulher_f == "Terezinha") || 
          #    (2019 %in% input$ano_f && input$mulher_f == "Terezinha")){
          #   showModal(modalDialog(
          #     title = "Aviso :",
          #     "Escolha outro ano, pois essa informação não consta na planilha para este ano!",
          #     easyClose = TRUE,
          #     fade = TRUE,
          #     size = "s",
          #     footer = modalButton("Ok")
          #   ))
          #   return()
          # }  
          # 
          
          else{
            f1 <- barplot_feedbacks(faxinas_escolha4(),
                                   input$eixo_x_f,
                                   input$grupo_f)
          }
        
          f1 <-  ggplotly(f1, tooltip = "text")
          
          f1
          
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
      ),
      p(
        "Adicionar novas variáveis como por exemplo tamanho da casa, quantidade de quartos, quantidade de 
        banheiros, tipo de casa: apartamento, república, casa, kitnet, pensionato, etc...",
        style = "font-size:14px;color:black;padding:10px;background-color:PaleTurquoise"
      ),
      p(
        "Classificar valores dos preços das faxinas de acordo com o tamanho da casa é um meio de padronizar
        os preços das faxinas, por exemplo, classificando por tipo de casa ou por dificuldade de limpeza, 
        o local a ser faxinado esta muito tempo sem limpar...",
        style = "font-size:14px;color:black;padding:10px;background-color:PaleTurquoise"
      ),
      p(
        "Com a informação de CEP e o endereço escrito de forma padronizada, é possível adquirir latitudes e 
        longetidudes dos locais para criar gráficos de mapas.",
        style = "font-size:14px;color:black;padding:10px;background-color:PaleTurquoise"
      )
    )
  })
    
  }
})

}  
  

 