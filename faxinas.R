
carregando_dados <-  function() {
  ## carregando a planilha faxinas 
  faxinas <- read_csv("www/faxinas.csv")
  
  ## modificando e criando as colunas de datas especificas
  faxinas <- faxinas %>%
    mutate(Data = as.POSIXct(faxinas$Data, "UTC", format = "%d/%m/%Y"),
           Semana = wday(Data, label = TRUE, abbr = TRUE),
           `Mês` = month(Data, label = TRUE, abbr = TRUE),
           ano = year(Data)) 

}  

faxinas_secao1 <- function(dados, data, eixo_x, eixo_y, grupo){
  
## adicionando opcoes que preciso nos dados 

  dados <- dados %>%
    filter(ano %in% data,
           Mulher != "NA",
           eixo_x != "NA") %>%
    mutate(Quantidade = 1) 
    
# porem nao posso usar a variavel Tipo!= "NA" aqui pois ela nao contem o ano de 2018 
# se coloco ela o facet_grid dos graficos trava para o ano de 2018 
  if(grupo == "Nenhum"){
    
      dados <- dados %>% 
      filter(`Ocorreu?` == "Sim") %>%  
      group_by_at(vars(ano, eixo_x)) 

  }
  
  else{
    
    dados <- dados %>% 
    group_by_at(vars(ano, eixo_x, grupo)) 

  }
  
  dados 
}

# ver se da pra encaixar numero médio de faxinas 
# else if(eixo_y == "Media"){
#   
# }  
