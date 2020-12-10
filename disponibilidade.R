library("tidyverse")
library("lubridate")
library("zoo")
library("plyr")

numero_de_dias_da_semana <- function(dia_da_semana, ano){
  datas <- seq.Date(as.Date(paste(ano,"01-01", sep="-")),
                    as.Date(paste(ano,"12-31", sep="-")),
                    by="1 day")
  nrow(as.data.frame(datas[weekdays(datas, abbreviate = TRUE)== dia_da_semana]))
}

#automatizar anos
carregando_dados_d <- function() {
  
  # passar o parametro de anos(sao todos os anos ate a data atual)
  # dias_anos = quantidade de dias dos anos 
  # criando uma tabela so de datas dos anos 2018,2019 e 2020...
  Datas <- tibble(Data = as.POSIXct(seq(as.Date("2018-01-01"),
                                    by = "day", length.out = 1096),
                                    "UTC", format = "%d/%m/%Y"))
  
  ## carregando a planilha faxinas 
  disponibilidade <- read_csv("www/disponibilidade.csv")
  
  ## modificando e criando as colunas de datas especificas
  disponibilidade <- disponibilidade %>%
    mutate(Data = as.POSIXct(Data, "UTC", format = "%d/%m/%Y"))

  disponibilidade <- full_join(Datas,disponibilidade, by ="Data")
           
  disponibilidade <- disponibilidade %>% 
    mutate(ano = year(Data),
           Mês = month(Data, label = TRUE, abbr = TRUE),
           Mês_n =  month(Data),
           ano_mês = factor(as.yearmon(Data)),
           Semana = wday(Data, label = TRUE, abbr = TRUE),
           Semana_d = wday(Data),
           Semana_n = week(Data),
           mês_semana = stringi::stri_datetime_fields(Data)$WeekOfMonth,
           dia = day(Data)) %>%
    group_by(Semana_n,ano) %>%
    rename(Colaboradora = Mulher) 

  # ceiling(as.numeric(format(disponibilidade$Data, "%d"))/7))
  # 
  disponibilidade
}    
  

disponibilidade_secao <- function(dados, data, mulher){

  dados <- dados %>% 
    filter(ano %in% data,
           Colaboradora %in% mulher)
 
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
}