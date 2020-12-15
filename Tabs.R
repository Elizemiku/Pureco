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
        style = "background-color:SkyBlue", width = 3,
        
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
                position = "left", width = 4,
                
                # Inputs
                
                ## graficos
                pickerInput(
                  inputId = "grafico",
                  label = "Selecione um estilo de gráfico:",
                  choices = c("Barras","Boxplot","Linhas","Pontos"),
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
                  choices = c("Dia da Semana" = "Semana", "Mês"),
                  selected = "Semana",
                  multiple = FALSE,
                  options = list(title='Escolha um ou mais anos:',
                                 style = "color: black; background: white; font-weight: bold;")),
                
                ## variavel do eixo y
                pickerInput(
                  inputId = "eixo_y",
                  label = "Selecione o tipo numérico que deseja visualizar:",
                  choices = c("Quantidade","Proporção","Valor"),
                  selected = "Quantidade",
                  multiple = FALSE,
                  options = list(title='Escolha um...:',
                                 style = "color: black; background: white; font-weight: bold;")),
              
                ## variavel do fill 
                pickerInput(
                  inputId = "grupo",
                  label = "Selecione uma opção adicional:",
                  choices = c("Nenhum","Ocorreu?", "Região", "Remarcou", "Tipo", "Valor"), 
                  selected = "Nenhum",
                  multiple = FALSE,
                  options = list(title='Escolha uma opção:',
                                 style = "color: black; background: white; font-weight: bold;")),
                
              # botao de ok depois de escolhida as opcoes  
              actionButton(
                inputId = "escolhido",
                label = "Gerar Gráfico",
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
           fluid = TRUE,
           
           sidebarLayout(
             sidebarPanel(
               p(h4(strong("Escolha as informações que deseja:"))), 
                  br(), style = "background-color:SkyBlue",
                position = "left", width = 4,
                                   
           
               ## graficos
               pickerInput(
                 inputId = "grafico_m",
                 label = "Selecione um estilo de gráfico:",
                 choices = c("Barras", "Boxplot", "Linhas e Pontos"),
                 selected = "Barras",
                 multiple = FALSE,
                 options = list(title='Selecione um estilo de gráfico:', 
                                style = "color: black; background: white; font-weight: bold;")),
               
               ## ano
               pickerInput(
                 inputId = "ano_m",
                 label = "Selecione o ano que deseja ver nos gráficos:",
                 choices = c(2018,2019,2020),
                 selected = 2018,
                 multiple = TRUE,
                 options = list(title='Escolha um ou mais anos:',
                                style = "color: black; background: white; font-weight: bold;")),

               ## variavel do eixo x
               pickerInput(
                 inputId = "eixo_x_m",
                 label = "Selecione o tipo de ocorrência que deseja analisar:",
                 choices = c("Dia da Semana" = "Semana", "Mês", "Colaboradora", "Remarcou"),
                 selected = "Semana",
                 multiple = FALSE,
                 options = list(style = "color: black; background: white; font-weight: bold;")),
               
               ## variavel do eixo y
               pickerInput(
                 inputId = "eixo_y_m",
                 label = "Selecione o tipo numérico que deseja visualizar:",
                 choices = c("Quantidade","Proporção", "Valor"),
                 selected = "Quantidade",
                 multiple = FALSE,
                 options = list(title='Escolha um...:',
                                style = "color: black; background: white; font-weight: bold;")),
               
               
               ## variavel do fill 
               pickerInput(
                 inputId = "mulher",
                 label = "Caso queira ver outras análises por colaboradora selecione uma ou mais mulheres:",
                 choices = c("Ledinha", "Lourdes", "Marcela", "Vilanir", "Zilza"),
                 selected = "Lourdes",
                 multiple = TRUE,
                 options = list(title='Escolha uma opção ou mais:',
                                style = "color: black; background: white; font-weight: bold;")),
               
               ## VER DE COLOCAR VALOR DEPOIS
               #variavel do facet
               pickerInput(
                 inputId = "grupo_m",
                 label = "Selecione uma opção adicional caso deseje analisar:",
                 choices = c("Nenhum","Ocorreu?", "Região","Tipo", "Valor"), 
                 selected = "Nenhum",
                 multiple = FALSE,
                 options = list(title='Escolha uma opção:',
                                style = "color: black; background: white; font-weight: bold;")),
               
               # botao de ok depois de escolhida as opcoes  
               actionButton(
                 inputId = "escolhido_m",
                 label = "Gerar Gráfico",
                 style = "color: #fff;
                           background-color: #337ab7; border-color: #2e6da4")
             ),
             
              mainPanel(title = "Gráfico",   
                        plotlyOutput("mulheres", width = 800, height = 500))
               
             )),                                  
  # tentar por grafico de calendario aqui , por sidebar e inputs...
  tabPanel(title = "Disponibilidade das Mulheres",
           icon = icon("calendar"),
           fluid = TRUE,
             
                 sidebarLayout(
                   sidebarPanel(
                     style = "background-color:SkyBlue",
                     position = "left", width = 2,
                  
                     radioButtons(
                       inline = FALSE,
                       inputId = "grafico_d",
                       label = "Selecione o gráfico:",
                       choices = c("Calendário por Disponibilidade" = 1,
                                   "Calendário por Colaboradora" = 2),
                       selected = 1),
                     
                     radioButtons(
                       inputId = "ano_d",
                       label = "Selecione o ano que deseja:",
                       choices = c(2018,2019,2020),
                       selected = 2018,
                       inline = FALSE),
                     
                     radioButtons(
                     inputId = "mulher_d",
                     label = "Para o segundo gráfico selecione uma Colaboradora:",
                     choices = c("Ledinha", "Lourdes", "Marcela", "Vilanir", "Zilza"),
                     selected = "Lourdes",
                     inline = FALSE),
                    
                    actionButton(
                      inputId = "escolhido_d",
                      label = "Gerar Gráfico",
                      style = "color: #fff;
                           background-color: #337ab7; border-color: #2e6da4")
                   ),
                 
                     
                 mainPanel(plotlyOutput("calendario", width = 800, height = 500))
             )
           ),
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
  fluid = TRUE,
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
        "→ Análises Descritivas: Várias opções de gráficos. Nesta seção há 5 abas, na qual cada uma foca na 
        visualização de gráficos relacionados ao nome que se encontra na aba escolhida. "
        ,
        style = "color:black"
      ),
    ),
    mainPanel(uiOutput("tutorial"))
  ))
)

Tab5 <- tabPanel("Sobre",
                 icon = icon("info-circle"), 
                 fluid = TRUE,
                 fluidRow(
                   column(6,
                          h4("Sobre o aplicativo Pureco", 
                             img(src = "https://static.wixstatic.com/media/02e186_1928f72d50254d83a45117a9d6dc5332~mv2_d_1600_1600_s_2.png/v1/fill/w_400,h_400,al_c,q_80,usm_0.66_1.00_0.01/02e186_1928f72d50254d83a45117a9d6dc5332~mv2_d_1600_1600_s_2.webp",
                                 height = "80px")),
                          h5("O Pureco é um dos primeiros projetos da Enactus Unicamp", 
                               img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQpTJX5Nu6HG04ddU7raVenaQpz9KxEtWWxEA&usqp=CAU.png",
                                   height = "60px")), 
                          p("Foi inspirado em outro projeto já existente nos EUA chamado Housecleaners Vida Verde,
                          que visa o empoderamento feminino e a possibilidade de trabalho com uma linha de produtos
                          ecologicamente sustentáveis para a limpeza de casas."),
                          br(),
                          h4("Sobre este projeto"),
                          h5(p("A ideia do projeto iniciou-se a partir de um trabalho da estudante Marília do 
                               curso de Estatística da Unicamp para a matéria de consultoria I,
                               no qual ela realizou análises estatísticas sobre as faxinas do aplicativo
                               e apresentou gráficos interativos, análises e tabelas em um site.")),
                          h5(p("O Projeto Análise de Dados do aplicativo Pureco consiste em realizar análises 
                               descritivas para contribuir com a melhoria da coleta de dados do aplicativo e
                               principalmente para compreender como o Pureco esta impactando 
                               a vida das colaboradoras que trabalham com as faxinas.")),
                          h5(p("A partir da bolsa BAS (Bolsa Auxílio Social) da Unicamp, este projeto foi 
                               orientado pela docente",
                               a("Tatiana Benaglia", href = "https://tatibenaglia.github.io/"), 
                               "e realizado pela estudante",
                               a("Elizabeth Borgognoni Souto.", href = "https://github.com/Elizemiku"))),
                          br(),
                          h5("Site feito em",
                             img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "40px"),
                             "por",
                             img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "40px"),
                             ".",
                             img(src = "https://www.unicamp.br/unicamp/sites/default/files/styles/large/public/Logo_Unicamp__0.jpg?itok=sO9EjTTS.png",
                                 height = "60px"),
                             img(src = "https://www.ime.unicamp.br/sites/default/files/informatica/logo-imecc.svg",
                                 height = "60px"),
                             img(src = "https://www.sae.unicamp.br/portal/images/saelogocorF.png", height = "60px"))
                          )
                          
                   )
                 )
                         

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
