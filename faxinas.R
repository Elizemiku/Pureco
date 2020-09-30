
carregando_dados <-  function() {
  ## carregando a planilha faxinas 
  faxinas <- read_csv("www/faxinas.csv")
  
  ## modificando e criando as colunas de datas especificas
  faxinas <- faxinas %>%
    mutate(Data = as.POSIXct(faxinas$Data, "UTC", format = "%d/%m/%Y"),
           Semana = wday(Data, label = TRUE, abbr = TRUE),
           mes = month(Data, label = TRUE, abbr = TRUE),
           ano = year(Data)) 

}  

faxinas_secao1 <- function(dados, data, eixo_x, eixo_y){
    
    dados %>%
      filter(`Ocorreu?` == "Sim",
             Mulher != "NA",
             eixo_x != "NA",
             Tipo != "NA",
             Valor != "NA",
             ano %in% data) %>%
      mutate(Quantidade = 1) %>%
      group_by_at(vars(ano, eixo_x)) 

  # ver se da pra encaixar numero médio de faxinas 
  # else if(eixo_y == "Media"){
  #   
  # }  

 
}
