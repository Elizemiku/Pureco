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

# funcao que modifica os dados de faxinas para nao aparecer faxinas duplicada na secao 1 de faxinas 
remove_faxinas_duplicadas <- function(dados){
  
  # removendo os clientes que sao NA para poder fazer comparacoes com o IF mais a frente
  faxinas_filtro <- dados[!is.na(dados[,"Cliente"]),] 
  indices <- as.numeric()
  j=0
  
  for(i in 1:nrow(faxinas_filtro)){ 
    
    if(i == nrow(faxinas_filtro)){
      return(dados)
    }  
  
    else{
      # se o nome do cliente e a data da linha seguinte forem iguais a linha anterior
      if( (faxinas_filtro[[i,"Cliente"]] == faxinas_filtro[[i+1,"Cliente"]]) && 
          (faxinas_filtro[[i,"Data"]] == faxinas_filtro[[i+1,"Data"]]) ){
        j <- j+1
        indices[j] <- i
        faxinas_filtro2 <- faxinas_filtro[c(indices),]
      }
    }
    
    # o anti_join retira as mesmas linhas iguais contidas em faxinas_filtro
    dados <- anti_join(dados,faxinas_filtro2)
  }
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
           Tipo == "Novo", 
           all_of(eixo_x) != "NA") %>%
    group_by_at(vars(ano,eixo_x)) %>%
    mutate(Quantidade = 1) %>% 
    summarise(Quantidade = sum(Quantidade))
  
  dados
}


faxinas_feedbacks <- function(dados, data, eixo_x, grupo , mulher){
  
  dados <- dados %>% 
    filter(ano %in% data,
           Colaboradora %in% mulher,
           `Feedback Colhido?` == "Sim",
           `Ocorreu?`=="Sim",
           all_of(eixo_x) != "NA") 
  
  if(grupo == "Nenhum"){
    dados <- dados %>% 
      group_by_at(vars(ano,eixo_x)) %>%
      mutate(Quantidade = 1) %>% 
      summarise(Quantidade = sum(Quantidade))
  }
  
  else{
    dados <- dados %>% 
      filter(all_of(grupo) != "NA") %>% 
      group_by_at(vars(ano,eixo_x,grupo)) %>%
      mutate(Quantidade = 1) %>% 
      summarise(Quantidade = sum(Quantidade))
  }
  
  dados
  
}

