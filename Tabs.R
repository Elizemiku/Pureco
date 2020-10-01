# Tabelas dinâmicas separadas
## Primeira parte bem comentada para entender melhor como funciona cada função

# Aba de Inicio
## tabPanel cria uma aba dentro da pagina
Tab1 <- tabPanel(
  # nome da aba
  title = "Início",
  # icone escolhido
  icon = icon("home"),
  
  # Mensagem ao abrir esta aba
  titlePanel(
    h1("Seja Bem-Vindo(a)!",
       style = "padding:40px; text-align: left;")
  ),
  
  # interface que se ajusta as dimensoes da janela do navegador do usuario
  fluidPage(# Barra lateral com as definições do input e do output.
    sidebarLayout(
      # Barra lateral para os inputs.
      sidebarPanel(
        style = "background-color:SkyBlue",
        
        # Input: planilhas selecionadas para a analise
        selectInput(
          inputId = "faxinas",
          selected = c("faxinas", "disponibilidade"),
          label = "Selecione as Planilhas de gerenciamento do app:",
          choices = c("faxinas", "disponibilidade"),
          multiple = TRUE
        ),
        
        # Input: data inicial
        dateInput(
          inputId = "selecionarperiodo",
          language = "pt-BR",
          label = "Selecione a Data Inicial das Análises:",
          min = as.character("2018-01-03"),
          format = "dd/mm/yyyy",
          startview = "month",
          value = "2018-01-03"
        ),
        
        # Input: data final
        dateInput(
          inputId = "selecionarperiodo2",
          language = "pt-BR",
          label = "Selecione a Data Final das Análises:",
          min = "2018-04-12",
          format = "dd/mm/yyyy",
          startview = "month",
          value = as.character(Sys.Date())
        ),
        
        # botao que e chamado em input$botao no server
        actionButton(
          inputId = "botao",
          label = "OK!",
          icon = icon ("bar-chart-o"),
          style = "color: #fff;
                           background-color: #337ab7; border-color: #2e6da4"
        )
      ),
      
      # chamada do output$inicio no server
      # o mainPainel e o painel principal para mostrar os outputs.
      mainPanel(htmlOutput("inicio"))
    )))

# Aba das planilhas
## Dados já tratados 
Tab2 <- tabPanel(
  title = "Relatório dos dados",
  icon = icon("table"),
  # gera uma pagina html nessa aba
  htmlOutput("Relatoriodados"))


# abas de analises descritivas
## navbarMenu cria um menu com navegações
# refatorar codigo aq 
Tab3 <- navbarMenu(title = "Análises Descritivas", 
                   icon = icon("bar-chart"),
          
          ## secao analises descritivas gerais das faxinas                  
          tabPanel(title = "Informações Gerais das Faxinas",
                   icon = icon("chart-line", lib = "font-awesome"),
                   fluid = TRUE, 
                   
            sidebarLayout(
              sidebarPanel(
                p(h4(strong("Escolha as informações que deseja:"))), 
                br(),
                style = "background-color:SkyBlue",
                position = "left",
                
                # Inputs
                
                ## graficos
                pickerInput(
                  inputId = "grafico",
                  label = "Selecione um estilo de gráfico:",
                  choices = c("Barras","Boxplot","Linhas"),
                  selected = "Barras",
                  multiple = FALSE,
                  options = list(title='Selecione um estilo de gráfico:', 
                                 style = "color: black; background: white; font-weight: bold;")),
                
                ## ano
                pickerInput(
                  inputId = "ano",
                  label = "Selecione o ano que deseja ver nos gráficos:",
                  choices = c(2018,2019,2020),
                  selected = 2018,
                  multiple = TRUE,
                  options = list(title='Escolha um ou mais anos:',
                                 style = "color: black; background: white; font-weight: bold;")),
                
                ## variavel do eixo x
                pickerInput(
                  inputId = "eixo_x",
                  label = "Selecione o tipo de ocorrência que deseja analisar:",
                  choices = c("Dia da Semana" = "Semana", "Mês" = "mes"),
                  selected = "Semana",
                  multiple = FALSE,
                  options = list(title='Escolha um ou mais anos:',
                                 style = "color: black; background: white; font-weight: bold;")),
                
                ## variavel do eixo y
                pickerInput(
                  inputId = "eixo_y",
                  label = "Selecione o tipo numérico que deseja visualizar:",
                  choices = c("Quantidade","Proporção" = "Proporcao", "Média"),
                  selected = "Quantidade",
                  multiple = FALSE,
                  options = list(title='Escolha um...:',
                                 style = "color: black; background: white; font-weight: bold;")),
              
                ## variavel do fill 
                pickerInput(
                  inputId = "variavel",
                  label = "Selecione uma opção adicional caso deseje analisar:",
                  choices = c("Nenhum","Tipo","Valor", "Ocorreu?", "Remarcou", "Região"), 
                  selected = "Nenhum",
                  multiple = FALSE,
                  options = list(title='Escolha uma opção:',
                                 style = "color: black; background: white; font-weight: bold;")),
                
              # botao de ok depois de escolhida as opcoes  
              actionButton(
                inputId = "escolhido",
                label = "Ok",
                style = "color: #fff;
                           background-color: #337ab7; border-color: #2e6da4")
            ),
              
             ## painel onde ficarao os graficos usar funcao plotlyOutput
             mainPanel(title = "Gráfico", 
                      plotlyOutput("infgeral1parte1", width = 800, height = 500))
             )),
             
            ## mudar a apartir daqui 
            # tabsetPanel(
            #    tabPanel("Faxinas por Dia da Semana", value = "infgeral1parte1",
            #             mainPanel(
            #               plotlyOutput("infgeral1parte1", width = 800, height = 500)
            #             )),
            #    tabPanel(
            #      "Faxinas por Tipo de Faxina e Dia da Semana",
            #      value = "infgeral2parte1",
            #      mainPanel(plotlyOutput(
            #        "infgeral2parte1", width = 800, height = 500
            #      ))
            #    ),
            #    tabPanel("Faxinas por Mês", 
            #             value = "infgeral3parte2",
            #             mainPanel(plotlyOutput(
            #               "infgeral3parte2",  width = 800, height = 500
            #               ))
            #             ),
            #    tabPanel(
            #      "Periodos ocupados por faxinas",
            #      value = "horas1",
            #      mainPanel(plotlyOutput(
            #        "horas1", width = 800, height = 500
            #      ))
            #    )
            #  )
           
  tabPanel(title = "Informações das Mulheres",
           icon = icon("female", lib = "font-awesome"),
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
  # tentar por grafico de calendario aqui , por sidebar e inputs...
  tabPanel(title = "Disponibilidade das Mulheres",
           icon = icon("calendar")),
  # clientes 
  tabPanel(title = "Informações dos Clientes",
           icon = icon("users", lib = "font-awesome"),
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
           )),
  # feedbacks
  tabPanel(title = "Informações sobre os Feedbacks",
           icon = icon("comments", lib = "font-awesome"))
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

# Tab6 <- tabPanel("Graficos", uiOutput("graficos"))

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
