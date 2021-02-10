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
                  choices = c(2018,2019,2020,2021),
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
           
      tabPanel(title = "Informações das Colaboradoras",
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
                     choices = c(2018,2019,2020,2021),
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
                     choices = c("Ledinha", "Lourdes", "Marcela", "Terezinha", "Vilanir", "Zilza"),
                     selected = c("Ledinha", "Lourdes", "Marcela", "Terezinha", "Vilanir", "Zilza"),
                     multiple = TRUE,
                     options = list(title='Escolha uma opção ou mais:',
                                    style = "color: black; background: white; font-weight: bold;")),
                   
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
  
  
        # calendario
        tabPanel(title = "Disponibilidade das Colaboradoras",
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
                             choices = c(2018,2019,2020,2021),
                             selected = 2018,
                             inline = FALSE),
                           
                           radioButtons(
                             inputId = "mulher_d",
                             label = "Para o segundo gráfico selecione uma Colaboradora:",
                             choices = c("Ledinha", "Lourdes", "Marcela", "Terezinha", "Vilanir", "Zilza"),
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
                 fluid = TRUE,
                 
                 sidebarLayout(
                   sidebarPanel(
                     p(h4(strong("Escolha as informações que deseja:"))), 
                     br(), style = "background-color:SkyBlue",
                     position = "left", width = 4,
                 
                     ## ano (nao coloquei 2018 pois nao tem a variavel tipo)
                     pickerInput(
                       inputId = "ano_c",
                       label = "Selecione o ano que deseja:",
                       choices = c(2019,2020,2021),
                       selected = 2019,
                       multiple = FALSE,
                       options = list(title='Escolha um ou mais anos:',
                                      style = "color: black; background: white; font-weight: bold;")),
                
                     ## variavel do eixo x
                     pickerInput(
                       inputId = "eixo_x_c",
                       label = "Selecione o tipo de ocorrência que deseja analisar:",
                       choices = c("Dia da Semana" = "Semana", "Mês"),
                       selected = "Semana",
                       multiple = FALSE,
                       options = list(title='Escolha um ou mais anos:',
                                      style = "color: black; background: white; font-weight: bold;")),
                     
                     
                     # botao de ok depois de escolhida as opcoes  
                     actionButton(
                       inputId = "escolhido_c",
                       label = "Gerar Gráfico",
                       style = "color: #fff;
                                 background-color: #337ab7; border-color: #2e6da4")
                   ),
                   
                   mainPanel(title = "Gráfico",   
                             plotlyOutput("clientes", width = 800, height = 500))
                   
                 )), 
  
      # feedbacks
      tabPanel(title = "Informações sobre os Feedbacks",
                icon = icon("comments", lib = "font-awesome"),
                fluid = TRUE,
                 
                 sidebarLayout(
                   sidebarPanel(
                    p(h4(strong("Escolha as informações que deseja:"))), 
                    br(), style = "background-color:SkyBlue",
                    position = "left", width = 4,
                    
                    ## ano
                    pickerInput(
                      inputId = "ano_f",
                      label = "Selecione o ano que deseja:",
                      choices = c(2018, 2019,2020, 2021),
                      selected = 2018,
                      multiple = TRUE,
                      options = list(title='Escolha um ou mais anos:',
                                     style = "color: black; background: white; font-weight: bold;")),
                    
                    ## variavel do eixo x
                    pickerInput(
                      inputId = "eixo_x_f",
                      label = "Selecione o tipo de ocorrência que deseja analisar:",
                      choices = c("Nota feedback cliente", "Nota feedback mulher", "Onde foi colhido?"),
                      selected = "Nota feedback cliente",
                      multiple = FALSE,
                      options = list(title='Escolha um ou mais anos:',
                                     style = "color: black; background: white; font-weight: bold;")),
                    
                    
                    #variavel do facet
                    pickerInput(
                      inputId = "grupo_f",
                      label = "Selecione uma opção adicional caso deseje analisar:",
                      choices = c("Nenhum","Colaboradora","Onde foi colhido?"), 
                      selected = "Nenhum",
                      multiple = FALSE,
                      options = list(title='Escolha uma opção:',
                                     style = "color: black; background: white; font-weight: bold;")),
                    
                    
                    
                    ## variavel do fill 
                    pickerInput(
                      inputId = "mulher_f",
                      label = "Caso escolha a opção colaboradora como opção adicional:",
                      choices = c("Ledinha", "Lourdes", "Marcela", "Terezinha", "Vilanir", "Zilza"),
                      selected = c("Ledinha", "Lourdes", "Marcela", "Terezinha", "Vilanir", "Zilza"),
                      multiple = TRUE,
                      options = list(title='Escolha uma opção ou mais:',
                                     style = "color: black; background: white; font-weight: bold;")),
                    
                    # botao de ok depois de escolhida as opcoes  
                    actionButton(
                      inputId = "escolhido_f",
                      label = "Gerar Gráfico",
                      style = "color: #fff;
                               background-color: #337ab7; border-color: #2e6da4")
                   ),
                   
                   mainPanel(title = "Gráfico",   
                             plotlyOutput("feedbacks", width = 800, height = 500))
                   
                 ))      
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
      p("→ Início: Carregue os dados e selecione o período de data que deseja saber para gerar as análises.",
        style = "color:black", align = "left"
      ),
      p("→ Relatório dos dados: Demonstração das modificações nas planilhas, ideias de gráficos e 
        tabelas para o site.",
        style = "color:black", align = "left"
      ),
      p("→ Análises Descritivas: Várias opções de gráficos. Nesta seção há 5 abas, na qual cada uma foca na 
        visualização de gráficos relacionados ao nome que se encontra na aba escolhida.", 
        style = "color:black", align = "left"),
      p("- Gráfico de Barras: Apresenta dados em categoria dispostos em barras retangulares
      nos quais os retângulos correspondentes a cada categoria são proporcionais ao número de observações 
      na respectiva categoria.", style = "color:black",align = "left"),
      p("- Gráfico do tipo Boxplot: É um diagrama de caixa construído utilizando as referências de valores mínimos e máximos,
        primeiro e terceiro quartil, mediana e outliers(valores discrepantes), em relação a uma variável escolhida. Permite visualizar
        a distribuição dos dados e seus outliers, assim podemos visualizar com o boxplot a 
        dispersão do meu conjunto de dados e se há ou não alguma simetria nesses dados.",
        style = "color:black", align = "left"),
      p("- Gráfico de Linhas ou pontos: Usados para observar alterações ao longo do tempo e
        para facilitar a identificação de tendências nos dados.", style = "color:black",align = "left"),
      p("- Gráfico do tipo Calendário: Parecido com um gráfico de calor, no qual colorimos os quadrados à medida que quantidades
        aumentam ou diminuem. Para os dados do Pureco utilizei 2 gráficos: O Primeiro se refere a quantidade de colaboradoras 
        disponíveis no dia do respectivo calendário por ano para realizar a faxina (disponibilidade por colaboradora), 
        e o outro gráfico com um quadrado colorindo por coloraboradora, mostra apenas o dia que determinada colaboradora 
        escolhida nas opções esta disponível para a faxina.", 
        style = "color:black", align = "left")
    ),
    mainPanel(uiOutput("tutorial"))
  ))
)

Tab5 <- tabPanel("Sobre",
                 icon = icon("info-circle"), 
                 fluid = TRUE,
                 fluidRow(
                   column(6,
                          h4(tags$strong("Sobre o aplicativo"), a("Pureco", href = "https://www.enactusunicamp.org/pureco"),
                             img(src = "data:image/jpeg;base64,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",
                                 height = "80px")),
                          h5(p("O Pureco é um dos primeiros projetos da Enactus Unicamp. Foi inspirado em outro projeto 
                          já existente nos EUA chamado Housecleaners Vida Verde, que visa o empoderamento feminino e a 
                          possibilidade de trabalho com uma linha de produtos ecologicamente sustentáveis para a limpeza 
                          de casas. Surgiu a partir de necessidades percebidas em visitas à comunidade Campo dos Amarais, 
                          no qual ajudam as colaboradoras cadastradas desta comunidade com uma melhoria na sua renda 
                          a partir das faxinas agendadas e ministradas pelo aplicativo Pureco. O Aplicativo Pureco
                          também auxilia sua equipe na tomada de decisão, dessa forma capacitando as colaboradoras em quesitos
                          de auto-confiança e tecnologia.")),
                          br(),
                          h4(tags$strong("Sobre este projeto")),
                          h5(p("A ideia do projeto iniciou-se a partir de um trabalho da estudante Marília do 
                               curso de Estatística da Unicamp para a matéria de consultoria I,
                               no qual ela realizou análises estatísticas sobre as faxinas do aplicativo
                               e apresentou gráficos interativos, análises e tabelas em um site.")),
                          h5(p("O Projeto Análise de Dados do aplicativo Pureco consiste em realizar análises 
                               descritivas para contribuir com a melhoria da coleta de dados do aplicativo e
                               principalmente para compreender como o Pureco esta impactando 
                               a vida das colaboradoras que trabalham com as faxinas. O projeto, portanto,
                               promove uma integração direta da universidade com a comunidade externa,
                               integrando alunos, docentes, pessoas da comunidade externa e demais colaboradores.")),
                          h5(p("A partir da bolsa BAS (Bolsa Auxílio Social) da Unicamp, este projeto foi 
                               orientado pela professora conselheira do Pureco",
                               a("Tatiana Benaglia", href = "https://tatibenaglia.github.io/"), 
                               "e realizado pela estudante",
                               a("Elizabeth Borgognoni Souto.", href = "https://github.com/Elizemiku"))),
                          br(),
                          h5("© Elizabeth Borgognoni Souto. 2020",
                             img(src = "https://www.unicamp.br/unicamp/sites/default/files/styles/large/public/Logo_Unicamp__0.jpg?itok=sO9EjTTS.png",
                                 height = "50px"),
                             img(src = "https://www.ime.unicamp.br/sites/default/files/informatica/logo-imecc.svg",
                                 height = "50px"),
                             img(src = "https://www.sae.unicamp.br/portal/images/saelogocorF.png", height = "50px")),
                          h5("Site feito em",
                             img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "40px"),
                             "por",
                             img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                             ".",)
                          )
                          
                   )
                 )
                         