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

#Opções de períodos:
problemList <- list(
    Periodos = list(
        Selecionar = 0, Diario = 1, Semanal = 2, Mensal=4, Trimestral= 3,Anual=5)
)

#Opçõees de mulheres:
problemList2 <- list(
    mulher = list(
        Selecionar = 0, Lourdes = 1, Vilanir = 2, Zilza = 3)
)

#para configurar a forma que aparece a predição         
gen_array <- function(forecast_obj){
    
    actuals <- forecast_obj$x
    lower <- forecast_obj$lower[,2]
    upper <- forecast_obj$upper[,2]
    point_forecast <- forecast_obj$mean
    
    cbind(actuals, lower, upper, point_forecast)
}

ui <- navbarPage("Pureco - Serviços de Limpeza",
                 tabPanel("Início",
                                        titlePanel("Seja Bem-Vindo(a)!"),
                          fluidPage(
                              sidebarLayout(
                                  sidebarPanel(
                                      fileInput(inputId = "faxina",label = "Insira aqui a planilha de Gerenciamento do App (.csv):",multiple = FALSE,accept = c(".csv"),buttonLabel = "Selecionar arquivo"
                                      ),
                                      fileInput(inputId = "disponibilidade",label = "Insira aqui a Disponibilidade das Mulheres:",multiple = FALSE,accept = c(".csv"),buttonLabel = "Selecionar arquivo"
                                      ),
                                      dateInput("selecionarperiodo", language = "pt-BR", label = "Selecione a Data Inicial das Análises:", min="2018-04-12",format="dd/mm/yyyy",startview = "month",value = "2018-04-12"),
                                      dateInput("selecionarperiodo2", language = "pt-BR", label = "Selecione a Data Final das Análises:", min="2018-06-01",format="dd/mm/yyyy",startview = "month"),
                                      actionButton(label="OK!","botao",icon = icon("bar-chart-o"))
                                  ),
                                  mainPanel(
                                      htmlOutput("inicio")
                                  )
                              )
                          )
                 ),
                 #Incluir aqui:
                 tabPanel("Análises Descritivas",
                          fluidPage(
                              
                              titlePanel("Análises Descritivas"),
                              
                              navlistPanel(fluid = TRUE,
                                           "Informações Gerais",
                                           tabPanel(value="geral1","Faxinas por Dia da Semana",
                                                    mainPanel(
                                                        plotlyOutput("geral1",width = 800,height = 500)

                                                    )),
                                           tabPanel(value="geral2","Faxinas por Tipo e Dia da Semana",
                                                    mainPanel(
                                                        plotlyOutput("geral2",width = 800,height = 500)

                                                    )),
                                           tabPanel(value="geral3","Faxinas por Mês",
                                                    mainPanel(
                                                        dataTableOutput("geral3")

                                                    )),
                                           tabPanel(value="geral4","Faxinas por Instituto",
                                                    mainPanel(
                                                        dataTableOutput("geral4")

                                                    )),
                                           tabPanel(value="horas1","Periodos ocupados por faxinas",
                                                    mainPanel(
                                                        plotlyOutput("horas1",width = 800,height = 500)
                                                    )),
                                         
                                           "Informações das Mulheres",
                                           tabPanel(value = "mulheres1","Faxinas por Mulher",
                                                    mainPanel(
                                                        plotlyOutput("mulheres1",width = 800,height = 500)
                                                    )),
                                           tabPanel(value = "mulheres2","Faxinas por Mulher e Dia da Semana",
                                                    mainPanel(
                                                        plotlyOutput("mulheres2",width = 800,height = 500)

                                                    )),
                                           "Informações dos Clientes",
                                           tabPanel(value = "clientes1","Melhores Clientes",
                                                    mainPanel(
                                                        textOutput("clientes1exp"),
                                                        dataTableOutput("clientes1",width = 800,height = 500)
                                                    )),
                                           tabPanel(value = "clientes2","Clientes Fidelizados",
                                                    mainPanel(
            
                                                        plotlyOutput("clientes2",width = 800,height = 500)
                                                    )),
                                           tabPanel(value = "clientes3","Sexo e Idade",
                                                    mainPanel(
                                                     textOutput("clientes3exp"),
                                                    plotlyOutput("clientes3",width = 800,height = 500)
                                                 )),
                                           tabPanel(value = "clientes4","Clientes Novos",
                                                    mainPanel(
                                                        textOutput("clientes4exp"),
                                                        plotOutput("clientes4",width = 800,height = 500)
                                                    ))
                              )
                          )),
                 navbarMenu("Extras",
                            tabPanel("Análise de Demanda",
                                     titlePanel("Análise de Demanda"),
                                     
                                     verticalLayout(fluid = TRUE,
                                                    selectInput("Período",inputId = "periodos", problemList[[1]]),
                                                    navlistPanel(well = TRUE,
                                                                 
                                                                 tabPanel(value="serie1","Quantidade de Faxinas por Mulher",
                                                                          mainPanel(
                                                                              plotlyOutput("serie1",width = 800,height = 500)
                                                                          )),
                
                                                                 
                                                                 tabPanel(value="qtde1","Quantidade de Faxinas",
                                                                          mainPanel(
                                                                              dygraphOutput("qtde1",width = 800,height = 500)
                                                                          )),
                                                                 
                                                                 tabPanel(value = "pred1","Previsão - Quantidade de Faxinas",
                                                                          mainPanel(
                                                                              dygraphOutput("pred1",width = 800,height = 500)
                                                                          ))
                                                    )
                                     )),
                            tabPanel("Localização dos Clientes",
                                     titlePanel("Localização dos Clientes"),
                                     
                                     verticalLayout(fluid = TRUE,
                                                    selectInput("Mulher",inputId = "localmulher", problemList2[[1]]),
                                                    navlistPanel(well = TRUE,
                                                                 "Geral",
                                                                 tabPanel(value="mapa","Localização de Todos os Clientes",
                                                                          mainPanel(
                                                                              leafletOutput("mapa",width = 800,height = 500)
                                                                          )),
                                                                 "Por Mulher",
                                                                 tabPanel(value="mapa2","Localização por Mulher",
                                                                          mainPanel(
                                                                              leafletOutput("mapa2",width = 800,height = 500)
                                                                          ))
                                                    )
                                     )
                            ))
)
