
#Tabela de Inicio
Tab1 <- tabPanel("Inicio", 
                 titlePanel("Seja Bem-Vindo(a)!"),
                 fluidPage(
                   sidebarLayout(
                     sidebarPanel(
                       fileInput(inputId = "faxina",label = "Insira aqui a planilha de Gerenciamento do App (.csv):",
                                 multiple = FALSE,accept = c(".csv"),buttonLabel = "Selecionar arquivo"),
                       fileInput(inputId = "disponibilidade",label = "Insira aqui a Disponibilidade das Mulheres:",
                                 multiple = FALSE,accept = c(".csv"),buttonLabel = "Selecionar arquivo"),
                       dateInput("selecionarperiodo", language = "pt-BR", 
                                 label = "Selecione a Data Inicial das Análises:", 
                                 min="2018-04-12", format="dd/mm/yyyy",startview = "month", value = "2018-04-12"),
                       dateInput("selecionarperiodo2", language = "pt-BR", 
                                 label = "Selecione a Data Final das Análises:", 
                                 min="2018-06-01", format="dd/mm/yyyy", startview = "month"),
                       actionButton(label = "OK!","botao", icon = icon ("bar-chart-o"))
                       ),
                     mainPanel( htmlOutput("Inicio"))
                     )
                  )
                 )


#Tabelas de analises descritas
Tab2 <- tabPanel("Análises Descritivas",
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
                 ))

#Extras
Tab3 <- tabPanel("Second Tab")