library("tidyverse")
library("lubridate")

carregando_dados_f <-  function() {
  ## carregando a planilha faxinas 
  faxinas <- read_csv("www/faxinas.csv")
  
  ## modificando e criando as colunas de datas especificas
  faxinas <- faxinas %>%
    mutate(Data = as.POSIXct(faxinas$Data, "UTC", format = "%d/%m/%Y"),
           Semana = wday(Data, label = TRUE, abbr = TRUE),
           `Mês` = month(Data, label = TRUE, abbr = TRUE),
           ano = year(Data))
  
  faxinas
}  

faxinas_secao1 <- function(dados, data, eixo_x, grupo){
  
  ## adicionando opcoes que preciso nos dados 
  
  dados <- dados %>%
    filter(ano %in% data,
           Colaboradora != "NA",
           all_of(eixo_x) != "NA") %>%
    mutate(Quantidade = 1) 
  
  # porem nao posso usar a variavel Tipo!= "NA" aqui pois ela nao contem o ano de 2018 
  # se coloco ela o facet_grid dos graficos trava para o ano de 2018 
    
  if(grupo == "Nenhum"){
    
    dados <- dados %>% 
        filter(`Ocorreu?` == "Sim") %>%  
        group_by_at(vars(ano, all_of(eixo_x))) 
  }
  
  else{
    
    dados <- dados %>% 
      select(ano, all_of(eixo_x), all_of(grupo), Quantidade) %>% 
      drop_na() %>% 
      group_by_at(vars(ano, all_of(eixo_x), all_of(grupo))) 
  }
  
  dados
  
}

# ver se da pra encaixar numero médio de faxinas 
# else if(eixo_y == "Media"){
#   
# }  
# 
# 
faxinas_secao2 <- function(dados, data, eixo_x, mulher, grupo_m){
  
  ## adicionando opcoes que preciso nos dados 
  
  dados <- dados %>% 
    filter(ano %in% data,
           Colaboradora %in% mulher,
           all_of(eixo_x) != "NA") %>%
    mutate(Quantidade = 1) 
    
  if(eixo_x == "Remarcou"){
      dados <- dados %>% filter(`Ocorreu?` == "Não", Remarcou != "NA") %>%
        select(ano, all_of(eixo_x), Colaboradora, Quantidade) %>% 
        drop_na() %>%
        group_by_at(vars(all_of(eixo_x), Colaboradora, ano)) 
  }
  
  else if(grupo_m == "Nenhum"){
    dados <- dados %>%
      filter(`Ocorreu?` == "Sim") %>%
      group_by_at(vars(ano, all_of(eixo_x), Colaboradora)) 
  }
  
  
  else{
    #facets e o grupo_m
    dados <- dados %>% 
      select(ano, all_of(eixo_x), Colaboradora, grupo_m, Quantidade) %>% 
      drop_na() %>% 
      group_by_at(vars(ano, all_of(eixo_x), Colaboradora, all_of(grupo_m))) 
  }
    
  
  dados 
}

faxinas_clientes <- function(dados, data, eixo_x){
  
  dados <- dados %>% 
    filter(ano %in% data,
           Cliente != "NA", 
           Tipo == "Novo") %>%
    group_by_at(vars(ano,eixo_x)) %>%
    mutate(Quantidade = 1) %>% 
    summarise(Quantidade = sum(Quantidade))
  
  dados
}



