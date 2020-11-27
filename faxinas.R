
carregando_dados <-  function() {
  ## carregando a planilha faxinas 
  faxinas <- read_csv("www/faxinas.csv")
  
  ## modificando e criando as colunas de datas especificas
  faxinas <- faxinas %>%
    mutate(Data = as.POSIXct(faxinas$Data, "UTC", format = "%d/%m/%Y"),
           Semana = wday(Data, label = TRUE, abbr = TRUE),
           `Mês` = month(Data, label = TRUE, abbr = TRUE),
           ano = year(Data)) %>% 
    rename(Colaboradora = Mulher) 
  
  faxinas
}  

numero_de_dias_da_semana <- function(dia_da_semana, ano){
  datas <- seq.Date(as.Date(paste(ano,"01-01", sep="-")),
                    as.Date(paste(ano,"12-31", sep="-")),
                    by="1 day")
  nrow(as.data.frame(datas[weekdays(datas, abbreviate = TRUE)== dia_da_semana]))
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

