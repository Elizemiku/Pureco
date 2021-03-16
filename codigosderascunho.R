############################### RASCUNHO DE CÓDIGOS ######################################

############### CÓDIGOS IDEIAS DE MODULES PARA O server.R #####################

# estudar modules no futuro  
# callModule(faxinasgeraisServer, "gerais", faxinas)
# pensar no futuro em como fazer observeEvent como um module 
## por enquanto nao sei ainda se vou fazer um module 
## 
## 
## 
## 
## 
## 
## 
## 
## funcao das seções 
# histogramServer <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     data <- reactive(mtcars[[input$var]])
#     output$hist <- renderPlot({
#       hist(data(), breaks = input$bins, main = input$var)
#     }, res = 96)
#   })
# }
# 
# faxinasgeraisServer <- function(id) {
#   moduleServer(id, function(input, output, session, faxinas){ 
#     
#     observeEvent(input$escolhido, {
#       if (input$grafico == "Barras") {
#         if (input$eixo_y == "Quantidade" & input$eixo_x == "Dia da Semana") {
#           faxinas_g <- reactive(
#             faxinas %>%
#               mutate(Quantidade = 1, ano = year(Data)) %>%
#               filter(Mulher != "NA" & `Dia da Semana` != "NA" &
#                        ano %in% input$ano) %>%
#               group_by(ano, `Dia da Semana`)  %>%
#               summarize(Quantidade = sum(Quantidade)) %>%
#               arrange(ano, Quantidade)
#           )
#           output$infgeral1parte1 <- renderPlotly({
#             g1a <- ggplot(faxinas_g(),
#                    aes(
#                      x = `Dia da Semana`,
#                      y = Quantidade,
#                      fill = `Dia da Semana`
#                    )) +
#               geom_bar(stat = "identity", position = "stack") +
#               facet_grid(~ ano, scales = "free_x") +
#               xlab("Dia da Semana") +
#               ylab("Quantidade de Faxinas") +
#               ggtitle("Quantidade de Faxinas por Dia da Semana e Ano") +
#               scale_fill_viridis_d(aesthetics = "fill") +
#               tema_facets
# 
#             g1a <- ggplotly(g1a) %>%
#               layout(showlegend = FALSE)
# 
#             g1a
#           })
#         }
#       }
#     })
#   })
# }



############### CÓDIGOS DO server.R #####################

        # data <- datasetServer("data")
        # data2 <- datasetServer("data2")

# se nao tiver colocado as planilhas retorna nulo
        #
        #     # leitura do banco de dados de faxinas
        #     faxinas = read_csv2(data$datapath,locale = locale(encoding = "latin1"))
        #     #
        #     # o intervalo de datas posso deixar
        #     faxinas$Data<-as.POSIXct(faxinas$Data, "UTC", format="%d/%m/%Y")
        #     faxinas<-faxinas%>%filter(Data>=input$selecionarperiodo & Data<input$selecionarperiodo2)
        #
        #     # faxinas$`Dia da Semana`<- weekdays(faxinas$Data)
        #
        #     #planilha de faxina 2018-2019
        #     # faxinas  <- read_xlsx(data$datapath, sheet=1, col_names = TRUE, skip = 1)
        #     #precisa fazer essa conversao
        #     # faxinas <- faxinas %>% mutate(Data = as.numeric(Data)) %>%
        #     #     mutate(Data = excel_numeric_to_date(Data))
        #

        #planilha de faxina 2019-2020
        #nao precisa fazer conversao de data, planilhas ja foram limpas
        #faxinas  <- read_csv(data$datapath)
        #planilha de faxina 2019-2020
        #nao precisa fazer conversao de data, planilhas ja foram limpas
        # vou ter que fazer um if aq se for arquivo xlsx ler xlsx fazer essas manipulacoes aq


### Tentativa de ja manipular os dados diretamente por aqui
#     if(grepl("2018-2019", data$datapath)){
#         ## manipulação dos dados
#         faxinas <- faxinas%>%
#             mutate(Data = as.numeric(Data), `Ocorreu?` = as.logical(`Ocorreu?`)) %>%
#             mutate(Data = convert_to_date(Data)) %>%
#             mutate(Val    or = as.numeric(Valor),
#                    `Ocorreu?` = as.character(`Ocorreu?`),
#                    `Feedback Colhido?` = as.character(`Feedback Colhido?`)
#             ) %>%
#             mutate_at(c("Cliente", "Endereço"),
#                       funs(ifelse(. == "-" |. == "--" | . == "/" | . == "ccc" | . == "rua", NA, .))) %>%
#             select(c(1:11)) %>% rename(Comentarios = "...11") %>%
#             remove_empty("rows")
#
#         faxinas[faxinas == TRUE] <- "Sim"
#         faxinas[faxinas == FALSE] <- "Não"
#         faxinas$Data <- format(faxinas$Data, "%d/%m/%Y")
#         faxinas$Valor <- formatC(faxinas$Valor, format = "f", digits = 2, big.mark = ",")
#         faxinas$Cliente <- str_trim(str_to_title(faxinas$Cliente))
#         faxinas$Comentarios <- str_trim(faxinas$Comentarios)
#         faxinas$Endereço <-  str_replace_all(faxinas$Endereço, "R\\.", "Rua ")
#         faxinas$Endereço <- str_replace_all(faxinas$Endereço, "R ", "Rua ")
#         faxinas$Endereço <- str_trim(str_to_title(faxinas$Endereço))
#     }
# }

## Maneiras diferentes de manipular as datas ##

# selecionando período das análises para trabalhar com os gráficos nesse período
# faxinas$Data <- as.Date(faxinas$Data, format = "%d/%m/%Y")
# faxinas$Data <- as.Date(faxinas$Data, tz = "GMT")

# faxinas$Data <- parse_datetime(faxinas$Data, format = "%d/%m/%Y", na = c("NA"))

# faxinas$`Dia da Semana` <- strftime(faxinas$Data, format = "%A")
# semana <- c("segunda","terça","quarta","quinta","sexta","sábado","domingo")

# faxinas$`Dia da Semana` <- factor(faxinas$`Dia da Semana`,
#                                  ordered = TRUE,
#                                  levels = semana)
# 


## modificando do ingles para portugues 

# levels(x) <- list(
#   Segunda = "Monday",
#   Terça = "Tuesday",
#   Quarta = "Wednesday",
#   Quinta = "Thursday",
#   Sexta = "Friday",
#   Sábado = "Saturday",
#   Domingo = "Sunday"
# )

# faxinas$`Dia da Semana`[faxinas$`Dia da Semana`=="Monday"] <- "Segunda"
# faxinas$`Dia da Semana`[faxinas$`Dia da Semana`=="Tuesday"] <- "Terça"
# faxinas$`Dia da Semana`[faxinas$`Dia da Semana`=="Wednesday"] <- "Quarta"
# faxinas$`Dia da Semana`[faxinas$`Dia da Semana`=="Thursday"] <- "Quinta"
# faxinas$`Dia da Semana`[faxinas$`Dia da Semana`=="Friday"] <- "Sexta"
# faxinas$`Dia da Semana`[faxinas$`Dia da Semana`=="Saturday"] <- "Sábado"
# faxinas$`Dia da Semana`[faxinas$`Dia da Semana`=="Sunday"] <- "Domingo"

## criando os niveis da semana na ordem 
# semana <-c("Segunda",
#            "Terça",
#            "Quarta",
#            "Quinta",
#            "Sexta",
#            "Sábado",
#            "Domingo")

# faxinas$`Dia da Semana` <- factor(faxinas$`Dia da Semana`, levels = semana, ordered = TRUE)

##boxplot




## como o shinyapps.io nao reconhecia os meses em formato brasileiro tive que modificar 1 por 1
## usei o comando Sys.setlocale("LC_TIME","en_US.UTF-8") para mudar o tempo do sistema para ingles 
## essa modificacao afeta as funcoes as.POSIxct e strftime

## modificando do ingles para portugues 
# faxinas2$mes[faxinas2$mes == "January"] <- "Janeiro"
# faxinas2$mes[faxinas2$mes == "February"] <- "Fevereiro"
# faxinas2$mes[faxinas2$mes == "March"] <- "Março"
# faxinas2$mes[faxinas2$mes == "April"] <- "Abril"
# faxinas2$mes[faxinas2$mes == "May"] <- "Maio"
# faxinas2$mes[faxinas2$mes == "June"] <- "Junho"
# faxinas2$mes[faxinas2$mes == "July"] <- "Julho"
# faxinas2$mes[faxinas2$mes == "August"] <- "Agosto"
# faxinas2$mes[faxinas2$mes == "September"] <- "Setembro"
# faxinas2$mes[faxinas2$mes == "October"] <- "Outubro"
# faxinas2$mes[faxinas2$mes == "November"] <- "Novembro"
# faxinas2$mes[faxinas2$mes == "December"] <- "Dezembro"



### Proporção de faxinas por Mulher e Ano - 17/08/2020 ###

# # Lourdes 
#     m3_Lo <- faxinas %>%
#       filter(Mulher != "NA" & `Ocorreu?` == "Sim" & `Dia da Semana` != is.na(NA)) %>%
#       mutate(Quantidade = 1, ano = year(Data)) %>%
#       group_by(ano, Mulher, `Dia da Semana`)  %>%
#       summarize(Quantidade = sum(Quantidade)) %>%
#       arrange(ano, Mulher, Quantidade) %>%
#       filter(Mulher == "Lourdes") %>%
#       mutate(Prop = Quantidade / sum(Quantidade)) %>%
#       ggplot(aes(
#         x = reorder(`Dia da Semana`, Prop),
#         y = Prop,
#         fill = `Dia da Semana`,
#         text = paste(
#           'Dia da Semana:',
#           reorder(`Dia da Semana`, Prop),
#           '<br>Proporção de faxinas realizadas:',
#           Quantidade
#         )
#       )) +
#       geom_bar(stat = "identity", position = "dodge") +
#       facet_grid( ~ ano) +
#       xlab("Dia da Semana") +
#       ylab("Proporção de faxinas realizadas") +
#       ggtitle("Proporção de Faxinas por Ano feitas pela Lourdes") +
#       scale_fill_viridis_d() +
#       tema_facets
#   
#   m3_Lo <- ggplotly(m3_Lo, tooltip = "text") %>%
#     layout(showlegend = FALSE)
#   
# # Marcela  
#     m3_Ma  <- faxinas %>%
#       filter(Mulher != "NA" & `Ocorreu?` == "Sim" & `Dia da Semana` != is.na(NA)) %>%
#       mutate(Quantidade = 1, ano = year(Data)) %>%
#       group_by(ano, Mulher, `Dia da Semana`)  %>%
#       summarize(Quantidade = sum(Quantidade)) %>%
#       arrange(ano, Mulher, Quantidade) %>%
#       filter(Mulher == "Marcela") %>%
#       mutate(Prop = Quantidade / sum(Quantidade)) %>%
#       ggplot(aes(
#         x = reorder(`Dia da Semana`, Prop),
#         y = Prop,
#         fill = `Dia da Semana`,
#         text = paste(
#           'Dia da Semana:',
#           reorder(`Dia da Semana`, Prop),
#           '<br>Proporção de faxinas realizadas:',
#           Quantidade
#         )
#       )) +
#       geom_bar(stat = "identity", position = "dodge") +
#       facet_grid( ~ ano) +
#       xlab("Dia da Semana") +
#       ylab("Proporção de faxinas realizadas") +
#       ggtitle("Proporção de Faxinas por Ano feitas pela Marcela") +
#       scale_fill_viridis_d() +
#       tema_facets
#   
#   m3_Ma <- ggplotly(m3_Ma , tooltip = "text") %>%
#     layout(showlegend = FALSE)
#   
# 
# # Vilanir  
#       m3_Vi  <- faxinas %>%
#         filter(Mulher != "NA" & `Ocorreu?` == "Sim" & `Dia da Semana` != is.na(NA)) %>%
#         mutate(Quantidade = 1, ano = year(Data)) %>%
#         group_by(ano, Mulher, `Dia da Semana`)  %>%
#         summarize(Quantidade = sum(Quantidade)) %>%
#         arrange(ano, Mulher, Quantidade) %>%
#         filter(Mulher == "Vilanir") %>%
#         mutate(Prop = Quantidade / sum(Quantidade)) %>%
#         ggplot(aes(
#           x = reorder(`Dia da Semana`, Prop),
#           y = Prop,
#           fill = `Dia da Semana`,
#           text = paste(
#             'Dia da Semana:',
#             reorder(`Dia da Semana`, Prop),
#             '<br>Proporção de faxinas realizadas:',
#             Quantidade
#           )
#         )) +
#         geom_bar(stat = "identity", position = "dodge") +
#         facet_grid( ~ ano) +
#         xlab("Dia da Semana") +
#         ylab("Proporção de faxinas realizadas") +
#         ggtitle("Proporção de Faxinas por Ano feitas pela Vilanir") +
#         scale_fill_viridis_d() +
#         tema_facets
#     
#   m3_Vi <- ggplotly(m3_Vi , tooltip = "text") %>%
#     layout(showlegend = FALSE)
# 
# # Zilza    
#     m3_Zi  <- faxinas %>%
#       filter(Mulher != "NA" & `Ocorreu?` == "Sim" & `Dia da Semana` != is.na(NA)) %>%
#       mutate(Quantidade = 1, ano = year(Data)) %>%
#       group_by(ano, Mulher, `Dia da Semana`)  %>%
#       summarize(Quantidade = sum(Quantidade)) %>%
#       arrange(ano, Mulher, Quantidade) %>%
#       filter(Mulher == "Zilza") %>%
#       mutate(Prop = Quantidade / sum(Quantidade)) %>%
#       ggplot(aes(
#         x = reorder(`Dia da Semana`, Prop),
#         y = Prop,
#         fill = `Dia da Semana`,
#         text = paste(
#           'Dia da Semana:',
#           reorder(`Dia da Semana`, Prop),
#           '<br>Proporção de faxinas realizadas:',
#           Quantidade
#         )
#       )) +
#       geom_bar(stat = "identity", position = "dodge") +
#       facet_grid(~ano) +
#       xlab("Dia da Semana") +
#       ylab("Proporção de faxinas realizadas") +
#       ggtitle("Proporção de Faxinas por Ano feitas pela Zilza") +
#       scale_fill_viridis_d() +
#       tema_facets
#   
#   m3_Zi <- ggplotly(m3_Zi , tooltip = "text") %>%
#     layout(showlegend = FALSE)

# juntado os graficos caso queira juntos usa subplot 


################ relatorio de dados rmd #############



# se der futuramente testar o rmd com runtime e flexdashboard
# output$graficos <- renderUI({
#   incldR
# })


## para o codigo de disponibilidade ##

# funcao exemplo que retorna o numero de dias da semana caso for usar futuramente
# numero_de_dias_da_semana <- function(dia_da_semana, ano){
#   datas <- seq.Date(as.Date(paste(ano,"01-01", sep="-")),
#                     as.Date(paste(ano,"12-31", sep="-")),
#                     by="1 day")
#   nrow(as.data.frame(datas[weekdays(datas, abbreviate = TRUE)== dia_da_semana]))
# }
# 
# 
# 
# 
# 
# 


# ggplotly(ggplot(disponibilidade %>% filter(Colaboradora == "Zilza"),
#                 aes(x = mês_semana, y = Semana, fill = Colaboradora,
#                      text = paste('Dia da Semana: ', Semana, '<br>',
#                              'Semana do mês: ', mês_semana, '<br>',
#                              'Colaboradora: ', Colaboradora, '<br>',
#                              'Dia: ', dia))) +
#            geom_tile(colour = "white") + 
#            facet_wrap(~ano_mês, as.table = TRUE) +
#            scale_x_continuous(breaks = c(1,2,3,4,5,6)) +
#            scale_y_discrete(breaks = c("sáb","sex","qui","qua","ter","seg","dom")) +
#            theme(
#              axis.line = element_line(colour = "black"),
#              legend.title = element_text(size = 10),
#              legend.text = element_text(size = 8),
#              strip.background = element_rect(colour = "black", fill = "#99CCFF"),
#              panel.background = element_rect(fill = "white", size = 2),
#              panel.grid.major = element_blank()),  tooltip = "text")

