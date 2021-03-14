# carregando pacotes necessários
library("tidyverse")
library("lubridate")
library("zoo")


# função que carrega os dados da planilha de disponibilidade
carregando_dados_d <- function() {
  
  # faltou automatizar os anos
  # passar o parametro de anos(sao todos os anos ate a data atual)
  # criando uma tabela do tipo tibble (tipo de dados) so de datas dos anos 2018,2019, 2020 a 2021...
  # dias_anos = quantidade de dias dos anos usados
  
  dias_anos = 1462
  
  # cria a tabela de Datas 
  Datas <- tibble(Data = as.POSIXct(seq(as.Date("2018-01-01"),
                                    by = "day", length.out = dias_anos),
                                    "UTC", format = "%d/%m/%Y"))
  
  # carregando a planilha de disponibilidade das colaboradoras 
  disponibilidade <- read_csv("www/disponibilidade.csv")
  
  # modificando e criando as colunas de datas especificas
  disponibilidade <- disponibilidade %>%
    mutate(Data = as.POSIXct(Data, "UTC", format = "%d/%m/%Y"))
  
  # juntando todas as datas de dos anos com a planilha de disponibilidade e criando uma tabela so
  disponibilidade <- full_join(Datas,disponibilidade, by ="Data") 
  
  # informações necessarias para utilizar no grafico de calendario           
  disponibilidade <- disponibilidade %>% 
    mutate(ano = year(Data),
           Mês = month(Data, label = TRUE, abbr = FALSE),
           Mês_n =  month(Data),
           ano_mês = factor(as.yearmon(Data)),
           Semana = wday(Data, label = TRUE, abbr = TRUE),
           Semana_d = wday(Data),
           Semana_n = week(Data),
           # a funcao seguinte retorna qual é a semana do mês
           mês_semana = stringi::stri_datetime_fields(Data)$WeekOfMonth,
           dia = day(Data)) %>%
    group_by(Semana_n,ano)
  
  disponibilidade
}    

# dados utilizados para o segundo calendario 
disponibilidade_c1 <- function(dados, data){
  
  # coloco os dias que nao disponibilidade com 0 e que há disponibilidade como 1
  dados$Disponibilidade[is.na(dados$Disponibilidade)] <- 0
  dados$Disponibilidade[dados$Disponibilidade == 2] <- 1
  
  dados <- dados %>% 
    filter(ano %in% data) %>% 
    # agrupando pelas informações sobre data que preciso
    group_by(Data,dia,ano,Semana,mês_semana,Mês) %>%
    # preciso colocar de 0 até o numero de colaboradoras em levels
    # conto quantas colaboradoras tem disponibilidade por dia 
    mutate(Quantidade = factor(sum(Disponibilidade), levels = c(0,1,2,3,4,5))) 
  
  dados
  
}


# dados utilizados para o segundo calendario
disponibilidade_m1 <- function(dados, data, mulher){
  
  dados <- dados %>% 
    filter(ano %in% data,
           Colaboradora %in% mulher) 
  dados
  
}