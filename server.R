# Códigos do server shiny 

## opção para não aparecer warnings dos summarize do pacote dplyr
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

# codigo com as modificacoes dos dados de faxinas
source("faxinas.R")

# codigo com as modificacoes dos dados de disponibilidade
source("disponibilidade.R")

# carregando o codigo com todas configuracoes para o Ui.R
source("Tabs.R")

# carregando o codigo com os temas dos graficos
source("Temas.R")

# carregando o codigo das funcoes que contem os graficos das secoes do site 
source("Secoes.R")


## função do server ## (seria como se fosse um backend do site) 
## 

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
      
      
     # Eventos da secao 1 (informacoes gerais de faxina) # 
     
     # a funcao reactive faz com que essa lista de inputs fique reativa 
      lista_de_eventos <- reactive({
        list(input$escolhido, input$ano, input$eixo_x, input$eixo_y, input$grupo, input$grafico)
      })
      
      # observeEvent observa se algum evento ocorreu e retorna um valor sobre este evento
      # quando tiver alguma mudança relacionada a lista de eventos e com os outputs do grafico 
      observeEvent(lista_de_eventos(), {
        
        # se o action button do ui.R (Gerar grafico) for pressionado pega os inputs e modifica 
        # os dados que quer filtrar de faxinas.R com a faxinas_secao1
        if(input$escolhido == 1){  

           # seleciona as opcoes de input para filtrar o que precisa da planilha de faxinas na secao 1
            faxinas_escolha <- reactive(
              faxinas_secao1(faxinas, input$ano, input$eixo_x, input$grupo)
            )
            
            # mudancas no grafico
            # o renderPlotly constroi um output para graficos do tipo plotly
            # retorna os graficos da secao 1 interativos 
            output$infgeral1parte1 <- renderPlotly({
            
            # se a informcao de ano for vazia nao roda os graficos      
            if(is.null(input$ano)){
                return()
            }
            
            # para prevenir alguns erros criei varios if/else if/else #  
            # o else if so pode ocorrer isoladamente, ja o if pode ocorrer ao mesmo tempo que um else if
              
            # caso o ano escolhido nao conste na planilha com os valores destas variaveis mostra um aviso  
            else if(2018 %in% input$ano && (input$grupo != "Nenhum" && input$grupo != "Ocorreu?" && input$grupo != "Valor")){
              
              # showModal: retorna uma caixinha com alguma mensagem e um botao para sair dessa caixa  
              # o modalDialog especifica a mensagem que ira aparecer na caixa de aviso
              # o  modalButton especifica como sera o botao que aparece para sair dessa mensagem
              showModal(modalDialog(
                title = "Aviso :",
                "Escolha outra opção de ano, pois essa informação não consta na planilha de 2018!",
                easyClose = TRUE,
                fade = TRUE,
                size = "s",
                footer = modalButton("Ok")
              ))
            }  
              
            # Se a opcao de graficos de pontos estiver selecionada ao mesmo tempo que nenhum grupo 
            # nao da para plotar o grafico pois o grafico de pontos so aparece com algum grupo associado
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

            # Se a opcao de graficos de boxplot estiver selecionada ao mesmo tempo que algum grupo escolhido
            # ou diferente da variavel Valor como escolha para o eixo y
            # nao da para plotar o grafico pois o grafico de boxplot aqui so funciona para a variavel Valor sem 
            # associacao de grupo
            else if(input$grafico == "Boxplot" && (input$eixo_y != "Valor" || input$grupo != "Nenhum")){
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

            # Se a opcao de graficos de Linhas estiver selecionada ao mesmo tempo que algum grupo escolhido
            # nao da para plotar o grafico pois o grafico de Linhas so aparece com nenhum grupo associado
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
             
              
            # Se a opcao de graficos de pontos estiver selecionada ao mesmo tempo que for diferente de quantidade
            # nao da para plotar o grafico pois o grafico de pontos so aparece por quantidade, nao pode usar proporcao 
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

           # se o grafico escolhido for diferente de Boxplot e o eixo y escolhido nas opcoes do grafico for Valor
           # mostra mensagem falando que apenas e possivel escolher os outros graficos pelos eixo y de quantidade/proporcao    
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

          # senao for nenhuma das opcoes anteriores     
          else{   
            
            # se nao estiver escolhido grupo  
            if(input$grupo == "Nenhum"){  
      
              # se a opcao escolhida for grafico de barras e o eixo y diferente de Valor 
              # faz o grafico de barras (barplot) para as opcoes em secao 1
              # como faxinas_escolha e reativo tem que chamar como se fosse uma funcao: faxinas_escolha() 
              if (input$grafico == "Barras" && input$eixo_y != "Valor"){
                g1 <- barplot_secao1(faxinas_escolha(), 
                                       input$eixo_x,
                                       input$eixo_y,
                                       input$eixo_x) + scale_fill_brewer(palette = "Set3")
              }
              
              # se a opcao escolhida for grafico de linhas faz o grafico para as opcoes em secao 1
              else if (input$grafico == "Linhas"){
                g1 <- lineplot_secao1(faxinas_escolha(), 
                                        input$eixo_x,
                                        input$eixo_y)
              }  
                
              # se a opcao escolhida for grafico de boxplot e no eixo y for Valor
              # faz o grafico para as opcoes em secao 1  
              else if (input$grafico == "Boxplot" & input$eixo_y == "Valor"){
                g1 <- boxplot_secao1(faxinas_escolha(),
                                       input$eixo_x)
              }
            }
            
            # senao (se tiver escolhido algum grupo, passamos para como opcao para o fill dos graficos)  
            else{
              
              # se a opcao escolhida for grafico de barras e no eixo y for diferente de Valor
              # faz o grafico para as opcoes em secao 1 com uma paleta de cores diferente 
              if (input$grafico == "Barras" & input$grupo != "Valor"){
                g1 <- barplot_secao1(faxinas_escolha(), 
                                     input$eixo_x,
                                     input$eixo_y,
                                     input$grupo) + 
                  scale_fill_brewer(palette = "Set2")
                }
                
              # se a opcao escolhida for grafico de barras e no eixo y for igual a Valor
              # faz o grafico para as opcoes em secao 1  
              else if (input$grafico == "Barras" & input$grupo == "Valor"){
                g1 <- barplot_secao1(faxinas_escolha(),
                                     input$eixo_x,
                                     input$eixo_y,
                                     input$grupo)
              }
               
              # se a opcao escolhida for grafico de pontos
              # faz o grafico para as opcoes em secao 1     
              else if(input$grafico == "Pontos"){
                g1 <- point_secao1(faxinas_escolha(), 
                                   input$eixo_x, 
                                   input$eixo_y, 
                                   input$grupo)
              }
           }  
        
          # ggplotly transforma um objeto ggplot num grafico interativo do plotly  
          g1 <- ggplotly(g1, tooltip = "text")
          
          g1
          
        }  
     })
    
    }    
  })  
        
    # Eventos da secao 2 # 
    
    # inputs dos eventos da secao de colaboradoras  
    lista_de_eventos2 <- reactive({
      list(input$escolhido_m, input$ano_m, input$eixo_x_m, input$eixo_y_m, 
           input$grafico_m, input$mulher, input$grupo_m)
    })
     
    # observando e retornando valores dos inputs da lista_de_eventos2 relacionado as opções da seção 2
    observeEvent(lista_de_eventos2(), {
      
      # se o botao gerar grafico da secao 2 for clicado
      if(input$escolhido_m == 1){  
        
        # seleciona as opcoes de input para filtrar o que precisa da planilha de faxinas na secao 2
        faxinas_escolha2 <- reactive(
          faxinas_secao2(faxinas, input$ano_m, input$eixo_x_m, input$mulher, input$grupo_m)
        )
        
        # output com renderPlotly, retorna os graficos da secao 2 interativos 
        output$mulheres <- renderPlotly({
          
          # se o usuario nao tiver escolhido nenhuma opcao de ano e de colaboradora retorna vazio 
          if(is.null(input$ano_m) || is.null(input$mulher)){
            return()
          }
          
          # caso o ano escolhido nao conste na planilha com os valores destas variaveis mostra um aviso
          else if((2018 %in% input$ano_m && input$grupo_m != "Nenhum" && 
                   input$grupo_m != "Ocorreu?" &&  input$grupo_m != "Valor") || 
                  (2018 %in% input$ano_m && input$eixo_x_m == "Remarcou")){
            showModal(modalDialog(
              title = "Aviso :",
              "Escolha outra opção de ano, pois essa informação não consta na planilha de 2018!",
              easyClose = TRUE,
              fade = TRUE,
              size = "s",
              footer = modalButton("Ok")
            ))
          }  
          
          # Se a opcao de graficos de barras estiver selecionada ao mesmo tempo que algum grupo e o eixo x for 
          # diferente de colaboradora nao da para plotar o grafico pois o grafico de barras com grupo so aparece
          # se colaboradora for selecionado aqui 
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
          
          # Se a opcao de graficos de Barras estiver selecionada ao mesmo tempo que o eixo y Valor 
          # nao da para plotar o grafico pois o grafico de barras so aparece com o eixo x sendo Quantidade ou Proporção
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
          
          # Se a opcao de graficos de Linhas e pontos estiver selecionada ao mesmo tempo que algum grupo
          # nao da para plotar este grafico
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
          
          # Se a opcao de graficos de Boxplot estiver selecionada ao mesmo tempo que estas opcoes mostra uma mensagem 
          # avisando que para o boxplot so é possivel ver o grafico se estiver selecionando desta maneira do Aviso
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

          # Se a opcao de graficos de pontos estiver selecionada ao mesmo tempo que for diferente de quantidade
          # nao da para plotar o grafico pois o grafico de pontos so aparece por quantidade, nao pode usar proporcao
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
          
          # Se a opcao de graficos de barras estiver selecionadaoao mesmo tempo que for diferente de Quantidade
          # no eixo y e for igual Remarcou no eixo x
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
          
          # Senao ocorrer nenhuma das opcoes anteriormente 
          else{

            # faz o graficos de barras de acordo com as opcoes de escolha da secao 2
            if(input$grafico_m == "Barras"){
                m1 <- barplot_secao2(faxinas_escolha2(), 
                                     input$eixo_x_m,
                                     input$eixo_y_m,
                                     input$grupo_m)
            }
            
            # se a opcao escolhida for grafico de Linhas e pontos e a opcao de grupo for Nenhum
            # faz o grafico para as opcoes em secao 2 
            else if (input$grafico_m == "Linhas e Pontos" & input$grupo_m == "Nenhum"){
              m1 <- linepointplot_secao2(faxinas_escolha2(),input$eixo_x_m)
            }
            
            # se a opcao escolhida for grafico de Boxplot, o eixo y for Valor e a opcao de grupo for Nenhum
            # faz o grafico para as opcoes em secao 2 
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
    
    # lista de eventos da secao de disponibilidade
    eventos_disponibilidade <- reactive({
      list(input$escolhido_d, input$grafico_d, input$ano_d, input$mulher_d)
    })
    
    
    observeEvent(eventos_disponibilidade(), {
      
      # se o botao gerar grafico for clicado na secao de disponibilidade
        if(input$escolhido_d == 1){
          
          # seleciona as opcoes de input para filtrar o que precisa da planilha de disponibilidade
          
          # inputs para o primeiro grafico
          disponibilidade_s1 <- reactive(
            disponibilidade_c1(disponibilidade, input$ano_d)
          )
        
          # inputs para o segundo grafico 
          disponibilidade_s2 <- reactive(
            disponibilidade_m1(disponibilidade, input$ano_d, input$mulher_d)
          )
          
        # output com renderPlotly, retorna os graficos da secao de disponibilidade interativos 
        output$calendario <- renderPlotly({
          
          # se o botao gerar grafico deste secao for clicado ao selecionar a primeira opcao 
          # aparece o grafico de Calendário por Disponibilidade
          if(input$grafico_d == 1){
            
           d1 <- ggplotly(calendario_c(disponibilidade_s1(), input$ano_d),
                          tooltip = "text")
          }
          
          # se o botao gerar grafico deste secao for clicado ao selecionar a segunda opcao 
          # aparece o grafico de Calendário por Colaboradora
          else if(input$grafico_d == 2){
            
            # mostra uma mensagem pois não consta o dados dessas das colaboradoras neste ano
            # analisando manualmente quais colaboradoras não tem informação em cada ano da planilha
            if((2018 %in% input$ano_d && input$mulher_d == "Ledinha") ||
               (2021 %in% input$ano_d && input$mulher_d == "Ledinha")){
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
            
            # senao ocorrer nenhuma das opcoes anteriores faz o grafico de Calendário por Colaboradora
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

    # lista de eventos da secao de clientes
    lista_de_eventos3 <- reactive({
      list(input$escolhido_c, input$ano_c, input$eixo_x_c)
    })
    
    observeEvent(lista_de_eventos3(), {
    
      # se o botao de gerar grafico for clicado 
      if(input$escolhido_c == 1){  
    
        # seleciona as opcoes de input para filtrar o que precisa da planilha de faxinas na secao de clientes 
        faxinas_escolha3 <- reactive(
          faxinas_clientes_novos(faxinas, input$ano_c, input$eixo_x_c)
        )
    
        
        output$clientes <- renderPlotly({
    
          # se nenhum ano for selecionando retorna vazio
          if(is.null(input$ano_m)){
            return()
          }
          
          # senao faz os graficos de barras da secao de clientes
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
    
    # lista de eventos da secao de feedbacks
    
    lista_de_eventos4 <- reactive({
      list(input$escolhido_f, input$ano_f, input$eixo_x_f, input$grupo_f, input$mulher_f)
    })
    
    observeEvent(lista_de_eventos4(), {
      
      if(input$escolhido_f == 1){  
        
        # seleciona as opcoes de input para filtrar o que precisa da planilha de faxinas na secao de feedbacks
        faxinas_escolha4 <- reactive(
          faxinas_feedbacks(faxinas, input$ano_f,input$eixo_x_f, input$grupo_f, input$mulher_f)
        )
        
        output$feedbacks <- renderPlotly({

          # se nenhum ano for selecionando ou nenhuma colaboradora retorna vazio
          if(is.null(input$ano_f) || is.null(input$mulher_f)){
            return()
          }
          
          ## contem apenas valores NA em 2020 sobre notas de feedback das mulheres por isso quando
          ## seleciona este ano retorna este aviso
          else if(input$ano_f == 2020 && input$eixo_x_f == "Nota feedback mulher"){
            showModal(modalDialog(
              title = "Aviso :",
              "Escolha outro ano, pois essa informação não consta na planilha!",
              easyClose = TRUE,
              fade = TRUE,
              size = "s",
              footer = modalButton("Ok")
            ))
            return()  
          }  
          
          ## contem apenas valores NA em 2021 sobre notas de feedback das mulheres por isso quando
          ## seleciona este ano retorna este aviso
          else if(input$ano_f == 2021 &&  input$eixo_x_f == "Nota feedback mulher"){
            showModal(modalDialog(
              title = "Aviso :",
              "Escolha outro ano, pois essa informação não consta na planilha!",
              easyClose = TRUE,
              fade = TRUE,
              size = "s",
              footer = modalButton("Ok")
            ))
            return()  
          }  
          
          ## contem apenas valores NA em 2018 para a variavel: Onde foi colhido? por isso quando
          ## seleciona este ano retorna este aviso
          else if((input$ano_f == 2018 && input$eixo_x_f == "Onde foi colhido?") || 
                  (input$ano_f == 2018 && input$grupo_f == "Onde foi colhido?")){
            showModal(modalDialog(
              title = "Aviso :",
              "Escolha outro ano, pois 'Onde foi colhido?' não consta na planilha de 2018!",
              easyClose = TRUE,
              fade = TRUE,
              size = "s",
              footer = modalButton("Ok")
            ))
          return()  
          }  
          
          # se o eixo x for Onde foi colhido? e o grupo for o mesmo retorna esse aviso
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
          
          # # of if abaixo mostra uma mensagem para cada mulher pois não consta o dados dessas moças na planilha para cada ano
          # # esta manual por isso se uma moça começar a trabalhar neste ano tem que tirar do if
          
          # mulher == "Ledinha"
          if((is.null(input$mulher_f == "Ledinha")) && is.null(input$ano_f != 2019)){
            showModal(modalDialog(
              title = "Aviso :",
              "Escolha o ano de 2019, pois essa colaboradora não consta na planilha para outras datas!",
              easyClose = TRUE,
              fade = TRUE,
              size = "s",
              footer = modalButton("Ok")
            ))
            return()
          }
          
          # se nenhuma das opcoes anteriores ocorrer faz o grafico de barras da secao de feedbacks
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
  ## renderUI renderiza uma pagina html 
  output$Relatoriodados <- renderUI({
    
    # tags sao opcoes de configuracao da pag provenientes do HTML
    # tags$iframe cria um quadro embutido para incorporar um documento HTML 
    tags$iframe(
      seamless = "seamless",
      src = "Relatoriodados.html",
      width = "100%",
      height = 1000,
      allowfullscreen = "true")
    
  })
  
  ## Quadros de conselhos da secao do tutorial
  output$tutorial <- renderUI({
    
    # texto que aparecera quando clicar em Ok e aparecer a imagem do Pureco no ínicio 
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
  

 