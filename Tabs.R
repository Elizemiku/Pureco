# Tabelas dinâmicas separadas


########## Comentar tabs #######

# Tabela de Inicio
Tab1 <- tabPanel("Início",
                 icon = icon("home"),
                 titlePanel(
                   h1("Seja Bem-Vindo(a)!", style = "padding:40px;
                               text-align: left;")
                 ),
                 fluidPage(sidebarLayout(
                   sidebarPanel(
                     style = "background-color:SkyBlue",
                     selectInput(
                       "faxinas",
                       selected = c("faxinas", "disponibilidade"),
                       label = "Selecione as Planilhas de Gerenciamento do App",
                       choices = c("faxinas", "disponibilidade"),
                       multiple = TRUE
                     ),
                     # fileInput(
                     #   inputId = "faxina",
                     #   label = "Insira aqui a planilha de Gerenciamento do App (.xlsx) ou (.csv):",
                     #   multiple = FALSE,
                     #   accept = c(".xlsx", ".csv"),
                     #   buttonLabel = "Selecionar arquivo"
                     # ),
                     # fileInput(
                     #   inputId = "disponibilidade",
                     #   label = "Insira aqui a Disponibilidade das Mulheres:",
                     #   multiple = FALSE,
                     #   accept = c(".csv"),
                     #   buttonLabel = "Selecionar arquivo"
                     # ),
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
                     actionButton(
                       label = "OK!",
                       "botao",
                       icon = icon ("bar-chart-o"),
                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                     )
                   ),
                   # chamada do output$inicio no server
                   mainPanel(htmlOutput("inicio"))
                 )))

# Dados já tratados
Tab2 <- tabPanel("Relatório dos dados",
                 icon = icon("table"),
                 htmlOutput("Relatoriodados"))

# Tabelas de analises descritas
Tab3 <- navbarMenu(
  "Análises Descritivas",
  icon = icon("bar-chart"),
  tabPanel("Informações Gerais das Faxinas",
           fluidRow(
             tabsetPanel(
               tabPanel("Faxinas por Dia da Semana", value = "infgeral1parte1",
                        mainPanel(
                          plotlyOutput("infgeral1parte1", width = 800, height = 500)
                        )),
               tabPanel(
                 "Faxinas por Tipo de Faxina e Dia da Semana",
                 value = "infgeral2parte1",
                 mainPanel(plotlyOutput(
                   "infgeral2parte1", width = 800, height = 500
                 ))
               ),
               tabPanel("Faxinas por Mês", value = "infgeral3parte2",
                        mainPanel(DT::dataTableOutput("infgeral3parte2"))),
               tabPanel(
                 "Periodos ocupados por faxinas",
                 value = "horas1",
                 mainPanel(plotlyOutput(
                   "horas1", width = 800, height = 500
                 ))
               )
             )
           )),
  tabPanel("Informações das Mulheres",
           fluidRow(tabsetPanel(
             tabPanel("Faxinas por Mulher", value = "mulheres1",
                      mainPanel(
                        plotlyOutput("mulheres1", width = 800, height = 500)
                      )),
             tabPanel(
               "Faxinas por Mulher e Dia da Semana",
               value = "mulheres2",
               mainPanel(plotlyOutput(
                 "mulheres2", width = 800, height = 500
               ))
             )
           ))),
  tabPanel("Informações dos Clientes",
           fluidRow(
             tabsetPanel(
               tabPanel(
                 "Melhores Clientes",
                 value = "clientes1",
                 mainPanel(textOutput("clientes1exp"), dataTableOutput("clientes1"))
               ),
               tabPanel("Clientes Fidelizados", value = "clientes2",
                        mainPanel(
                          plotlyOutput("clientes2", width = 800, height = 500)
                        )),
               tabPanel("Sexo e Idade", value = "clientes3",
                        mainPanel(
                          textOutput("clientes3exp"),
                          plotlyOutput("clientes3", width = 800, height = 500)
                        )),
               tabPanel("Clientes Novos", value = "clientes4",
                        mainPanel(
                          textOutput("clientes4exp"),
                          plotOutput("clientes4", width = 800, height = 500)
                        ))
             )
           ))
)

# Tabela tutorial, colocar um tutorial pode ser em rmd...md..html 
Tab4 <- tabPanel(
  "Tutorial",
  icon = icon("question-circle"),
  align = "bottom",
  titlePanel(h2(
    "Tutorial sobre as análises de dados do PURECO"
  )),
  fluidPage(sidebarLayout(
    sidebarPanel(
      "Como manusear o site:",
      style = "background-color:SkyBlue",
      align = "center",
      br(),
      br(),
      p(
        "→ Início: Carregue os dados e selecione o período de data que deseja
                                  saber para gerar as análises",
        style = "color:black"
      ),
      br(),
      p(
        "→ Relatório dos dados: Demonstração sucinta das modificações nas planilhas,
                                  ideias de gráficos e tabelas para o site",
        style = "color:black"
      ),
      br(),
      p(
        "→ Análises Descritivas: Várias opções de gráficos e tabelas",
        style = "color:black"
      ),
    ),
    mainPanel(uiOutput("tutorial"))
  ))
)

Tab5 <- tabPanel("Sobre", icon = icon("info-circle"))

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
