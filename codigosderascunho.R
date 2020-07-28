############################### RASCUNHO DE CÓDIGOS ######################################

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