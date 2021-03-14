# carregando pacotes necessarios
library("tidyverse")
library("lubridate")

# função que carrega os dados da planilha de faxinas
carregando_dados_f <-  function() {
  ## carregando a planilha faxinas 
  faxinas <- read_csv("www/faxinas.csv")
  
  ## modificando e criando as colunas de datas especificas 
  faxinas <- faxinas %>% 
    mutate(Data = as.POSIXct(faxinas$Data, "UTC", format = "%d/%m/%Y"),
           Semana = wday(Data, label = TRUE, abbr = TRUE),
           `Mês` = month(Data, label = TRUE, abbr = TRUE),
           ano = year(Data)) %>%
   filter(Colaboradora != "Maria") 
  
  faxinas
}  

# funcao que modifica os dados de faxinas para nao aparecer faxinas duplicada na secao 1 de faxinas 
remove_faxinas_duplicadas <- function(dados){
  
  # removendo os clientes que sao NA para poder fazer comparacoes com o IF mais a frente
  faxinas_filtro <- dados[!is.na(dados[,"Cliente"]),] 
  # criando a variavel numerica indices
  indices <- as.numeric()
  # iniciando o contador de j em 0
  j=0
  
  # criando o loop para andar nas linhas da planilha faxinas
  for(i in 1:nrow(faxinas_filtro)){ 
    
    # se i for do tamanho de linhas da planilha retorna os dados da planilha
    if(i == nrow(faxinas_filtro)){
      return(dados)
    }  
  
    # senao continua percorrendo as linhas
    else{
      # se o nome do cliente e a data da linha seguinte forem iguais a linha anterior
      # acrescenta um valor a mais em j para andar para a proxima linha
      # indices na posicao j recebem o que esta na linha atual
      # faxinas_filtro2 contera as linhas duplicadas 
      if( (faxinas_filtro[[i,"Cliente"]] == faxinas_filtro[[i+1,"Cliente"]]) && 
          (faxinas_filtro[[i,"Data"]] == faxinas_filtro[[i+1,"Data"]]) ){
        j <- j+1
        indices[j] <- i
        faxinas_filtro2 <- faxinas_filtro[c(indices),]
        # o anti_join retira as mesmas linhas iguais contidas em faxinas_filtro2 
        # com a planilha de faxinas atual devolvendo apenas os dados que nao estao com faxinas duplicadas
        dados <- anti_join(dados,faxinas_filtro2)
      }
    }
  }    
    dados
}

# funcao que organiza os dados para a secao de informações gerais de faxinas dos graficos do site 
faxinas_secao1 <- function(dados, data, eixo_x, grupo){
  
  ## adicionando opcoes que preciso nos dados 
  # usamos a funcao all_of pois sera mais de uma opçao de variavel no eixo_x e grupo 
  # removendo faxinas duplicadas 
  dados <- remove_faxinas_duplicadas(dados)
  
  dados <- dados %>%
    filter(ano %in% data,
           Colaboradora != "NA",
           all_of(eixo_x) != "NA") %>%
    mutate(Quantidade = 1) 
    
  # se o grupo for nenhum agrupa apenas pelas variaveis do eixo_x
  if(grupo == "Nenhum"){
    
    dados <- dados %>% 
        filter(`Ocorreu?` == "Sim") %>%  
        group_by_at(vars(ano, all_of(eixo_x))) 
  }
  
  # senao agrupa apenas pelas variaveis do eixo_x e do grupo
  else{
    
    dados <- dados %>% 
      select(ano, all_of(eixo_x), all_of(grupo), Quantidade) %>% 
      # remove as linhas que contem informações nulas 
      drop_na() %>% 
      group_by_at(vars(ano, all_of(eixo_x), all_of(grupo))) 
  }
  
  dados
  
}

# funcao que organiza os dados para a secao de colaboradoras dos graficos do site 
faxinas_secao2 <- function(dados, data, eixo_x, mulher, grupo_m){
  
  ## adicionando opcoes que preciso nos dados 
  
  dados <- dados %>% 
    filter(ano %in% data,
           Colaboradora %in% mulher,
           all_of(eixo_x) != "NA") %>%
    mutate(Quantidade = 1) 
  
  # se o eixo_x escolhido for a variavel Remarcou escolhemos os valores de remarcou que não sao nulos 
  # e apenas as faxinas que nao ocorreram 
  if(eixo_x == "Remarcou"){
      dados <- dados %>% filter(`Ocorreu?` == "Não", Remarcou != "NA") %>%
        select(ano, all_of(eixo_x), Colaboradora, Quantidade) %>% 
        drop_na() %>%
        group_by_at(vars(all_of(eixo_x), Colaboradora, ano)) 
  }
  # se o grupo escolhido for Nenhum agrupa somente pelo eixo_x e por Colaboradora 
  else if(grupo_m == "Nenhum"){
    dados <- dados %>%
      filter(`Ocorreu?` == "Sim") %>%
      group_by_at(vars(ano, all_of(eixo_x), Colaboradora)) 
  }
  
  # senao agrupamos por eixo_x, colaboradora e grupo 
  else{
    #facets e o grupo_m
    dados <- dados %>% 
      select(ano, all_of(eixo_x), Colaboradora, grupo_m, Quantidade) %>% 
      drop_na() %>% 
      group_by_at(vars(ano, all_of(eixo_x), Colaboradora, all_of(grupo_m))) 
  }
    
  dados 
}

# funcao que organiza os dados para a secao de clientes dos graficos do site 
faxinas_clientes_novos <- function(dados, data, eixo_x){
  
  # usa a funcao para remover faxinas duplicadas 
  # agrupa por ano e eixo_x e conta a quantidade de faxinas realizadas naquele periodo pelo cliente novo
  dados <- remove_faxinas_duplicadas(dados) %>% 
    filter(ano %in% data,
           Cliente != "NA", 
           Tipo == "Novo", 
           all_of(eixo_x) != "NA") %>%
    group_by_at(vars(ano,eixo_x)) %>%
    mutate(Quantidade = 1) %>% 
    summarise(Quantidade = sum(Quantidade))
  
  dados
}


# funcao que organiza os dados para a secao de feedbacks dos graficos do site 
faxinas_feedbacks <- function(dados, data, eixo_x, grupo , mulher){
  
  # se escolher Nota feedback cliente e nao escolher a opcao de grupo por colaboradora 
  # remove as faxinas duplicadas 
  if(eixo_x == "Nota feedback cliente" & grupo != "Colaboradora"){
    dados <- remove_faxinas_duplicadas(dados) %>% 
      filter(ano %in% data,
             Colaboradora %in% mulher,
             `Feedback Colhido?` == "Sim",
             `Ocorreu?`=="Sim",
             all_of(eixo_x) != "NA") 
  }
  # senao nao remove 
  else{
    dados <- dados %>% 
      filter(ano %in% data,
             Colaboradora %in% mulher,
             `Feedback Colhido?` == "Sim",
             `Ocorreu?`=="Sim",
             all_of(eixo_x) != "NA") 
  }
    
  # se escolher nenhuma opcao como o grupo, agrupa apenas por ano e eixo_x e conta quantos feedbacks recebeu das faxinas
  if(grupo == "Nenhum"){
    
    dados <- dados %>% 
      group_by_at(vars(ano,eixo_x)) %>%
      mutate(Quantidade = 1) %>% 
      summarise(Quantidade = sum(Quantidade))
  }
  
  # senao agrupa por grupo tambem e conta quantos feedbacks recebeu das faxinas 
  # 
  else{
    dados <- dados %>% 
      filter(all_of(grupo) != "NA") %>% 
      group_by_at(vars(ano,eixo_x,grupo)) %>%
      mutate(Quantidade = 1) %>% 
      summarise(Quantidade = sum(Quantidade))
  }
  
  dados
  
}

