# Tabelas dinâmicas separadas

  
########## Comentar tabs #######

# Tabela de Inicio
Tab1 <- tabPanel("Início",
                 titlePanel("Seja Bem-Vindo(a)!"
                 ),
                 fluidPage(sidebarLayout(
                   sidebarPanel(
                     fileInput(
                       inputId = "faxina",
                       label = "Insira aqui a planilha de Gerenciamento do App (.xlsx) ou (.csv):",
                       multiple = FALSE,
                       accept = c(".xlsx", ".csv"),
                       buttonLabel = "Selecionar arquivo"
                     ),
                     fileInput(
                       inputId = "disponibilidade",
                       label = "Insira aqui a Disponibilidade das Mulheres:",
                       multiple = FALSE,
                       accept = c(".csv"),
                       buttonLabel = "Selecionar arquivo"
                     ),
                     dateInput(
                       "selecionarperiodo",
                       language = "pt-BR",
                       label = "Selecione a Data Inicial das Análises:",
                       min = as.character("2018-04-12"),
                       format = "dd/mm/yyyy",
                       startview = "month",
                       value = "2018-04-12"
                     ),
                     dateInput(
                       "selecionarperiodo2",
                       language = "pt-BR",
                       label = "Selecione a Data Final das Análises:",
                       min = "2018-06-01",
                       format = "dd/mm/yyyy",
                       startview = "month",
                       value = as.character(Sys.Date())
                     ),
                     # botao e o nome que e chamado em input$botao no server 
                     actionButton(label = "OK!", "botao", icon = icon ("bar-chart-o"))
                   ),
                   # chamada do output$inicio no server
                   mainPanel(htmlOutput("inicio"))
                 )))


# Tabelas de analises descritas
Tab2 <- tabPanel("Análises Descritivas",
                 fluidPage(
                   titlePanel("Análises Descritivas"),
                   navlistPanel(
                     fluid = TRUE,
                     "Informações Gerais das Faxinas",
                     tabPanel(value = "infgeral1parte1", "Quantidade de Faxinas por Dia da Semana",
                              mainPanel(
                                plotlyOutput("infgeral1parte1", width = 800, height = 500)
                              )),
                     tabPanel(
                       value = "infgeral2parte1",
                       "Quantidade de Faxinas por Tipo de Faxina e Dia da Semana",
                       mainPanel(plotlyOutput(
                         "infgeral2parte1", width = 800, height = 500
                       ))
                     ),
                     tabPanel(value = "geral3", "Faxinas por Mês",
                              mainPanel(dataTableOutput("geral3"))),
                     tabPanel(
                       value = "horas1",
                       "Periodos ocupados por faxinas",
                       mainPanel(plotlyOutput(
                         "horas1", width = 800, height = 500
                       ))
                     ),
                     "Informações das Mulheres",
                     tabPanel(value = "mulheres1", "Faxinas por Mulher",
                              mainPanel(
                                plotlyOutput("mulheres1", width = 800, height = 500)
                              )),
                     tabPanel(
                       value = "mulheres2",
                       "Faxinas por Mulher e Dia da Semana",
                       mainPanel(plotlyOutput(
                         "mulheres2", width = 800, height = 500
                       ))
                     ),
                     "Informações dos Clientes",
                     tabPanel(
                       value = "clientes1",
                       "Melhores Clientes",
                       mainPanel(
                         textOutput("clientes1exp"),
                         dataTableOutput("clientes1")
                       )
                     ),
                     tabPanel(value = "clientes2", "Clientes Fidelizados",
                              mainPanel(
                                plotlyOutput("clientes2", width = 800, height = 500)
                              )),
                     tabPanel(value = "clientes3", "Sexo e Idade",
                              mainPanel(
                                textOutput("clientes3exp"),
                                plotlyOutput("clientes3", width = 800, height = 500)
                              )),
                     tabPanel(value = "clientes4", "Clientes Novos",
                              mainPanel(
                                textOutput("clientes4exp"),
                                plotOutput("clientes4", width = 800, height = 500)
                              ))
                   )
                 ))

# Dados já tratados
Tab3 <- tabPanel("Relatório dos dados",
                 htmlOutput("Relatoriodados"))

# Tabela tutorial, colocar um tutorial pode ser em rmd...md..html 
Tab4 <- tabPanel("Tutorial", 
                 titlePanel("Tutorial de como ver as análises dos dados do PURECO."))

#Extras
# Tab5   <- navbarMenu(
#   "Extras",
#   tabPanel(
#     "Análise de Demanda",
#     titlePanel("Análise de Demanda"),
#     verticalLayout(
#       fluid = TRUE,
#       selectInput("Período", inputId = "periodos", problemList[[1]]),
#       navlistPanel(
#         well = TRUE,
#         tabPanel(
#           value = "serie1",
#           "Quantidade de Faxinas por Mulher",
#           mainPanel(plotlyOutput(
#             "serie1", width = 800, height = 500
#           ))
#         ),
#         tabPanel(value =
#                    "qtde1", "Quantidade de Faxinas",
#                  mainPanel(
#                    dygraphOutput("qtde1", width = 800, height = 500)
#                  )),
#         
#         tabPanel(
#           value = "pred1",
#           "Previsão - Quantidade de Faxinas",
#           mainPanel(dygraphOutput(
#             "pred1", width = 800, height = 500
#           ))
#         )
#       )
#     )
#   ),
#   tabPanel(
#     "Localização dos Clientes",
#     titlePanel("Localização dos Clientes"),
#     verticalLayout(
#       fluid = TRUE,
#       selectInput("Mulher", inputId = "localmulher", problemList2[[1]]),
#       navlistPanel(
#         well = TRUE,
#         "Geral",
#         tabPanel(
#           value = "mapa",
#           "Localização de Todos os Clientes",
#           mainPanel(leafletOutput(
#             "mapa", width = 800, height = 500
#           ))
#         ),
#         "Por Mulher",
#         tabPanel(value =
#                    "mapa2", "Localização por Mulher",
#                  mainPanel(
#                    leafletOutput("mapa2", width = 800, height = 500)
#                  ))
#       )
#     )
#   )
# )

