#Secoes.R: graficos utilizados para cada opção das seções de análise descritiva do site

# carregando outros arquivos necessarios 
source("faxinas.R")
source("disponibilidade.R")
source("Temas.R")

## Pacotes ##

# usado para a manipulação de dados e visualização 
library("tidyverse")
# usada para as paletas de cores dos gráficos
library("RColorBrewer")
library("grDevices") 
# usada para a funcao facet_rep 
library("lemon")

## explicacao de uma funcao utilizada em todos os graficos:
## facet_wrap transforma uma sequência de uma dimensão de painéis em algo em duas dimensões,
## enquanto facet_grid cria uma matriz de painéis.

## Funcoes de graficos da secao1  ##

# funcao do grafico de barras da secao de informacoes gerais das faxinas
barplot_secao1 <- function(dados, eixo_x, eixo_y, grupo){
  
# se o grupo escolhido for o Valor da faxina
  if(grupo == "Valor"){

    # conta a quantidade de faxinas com determinado valor 
     dados <- dados %>% group_by_at(vars(ano, eixo_x, Valor)) %>% 
       summarize(Quantidade = sum(Quantidade)) %>%
       mutate(`Proporção` = round(Quantidade/sum(Quantidade), 2)) 
     
    # criando grupos dependendo dos preços das faxinas  
     dados$Valor[dados$Valor >= 60 & dados$Valor < 85] <- 1
     dados$Valor[dados$Valor > 80 & dados$Valor <= 105] <- 2
     dados$Valor[dados$Valor > 105 & dados$Valor <= 130] <- 3
     dados$Valor[dados$Valor > 130 & dados$Valor <= 150] <- 4
     dados$Valor[dados$Valor > 150 & dados$Valor <= 170] <- 5
     dados$Valor[dados$Valor > 170 & dados$Valor <= 190] <- 6
     dados$Valor[dados$Valor > 190 & dados$Valor <= 210] <- 7
     dados$Valor[dados$Valor > 210 & dados$Valor <= 230] <- 8
     
     # transformando os grupos em niveis 
     dados <- dados %>% mutate(Valor = factor(Valor))
     
     levels(dados$Valor) <- list("[60-80]" = "1", "[85-105]" = "2", "[105-130]" = "3", 
                                 "[130-150]" = "4", "[150-170]" = "5",  "[170-190]" = "6",
                                 "[190-210]" = "7", "[210-230]" = "8")  
     
     # ggplot e a funcao que cria o objeto de grafico ggplot
     # fazendo o grafico de barras por valor e quantidade de faxinas, nao pego valores de faxinas nulo 
     # aes e onde coloco as informacoes de eixo x e y do grafico alem de divisao dos dados por algum grupo (fill)
     # a opcao text dentro do ggplot nos permite colocar o que aparece ao passar o mouse em cima do grafico
     # geom_bar cria as barras do grafico 
     # o facet_wrap cria graficos para cada ano
     # labs e o local que nomeio os eixos e o titulo do grafico da forma que desejo que apareça
     # scale_fill_brewer me permite usar uma paleta de cores especifica
     # temas_facets e a configuracao do tema dos grafico do ggplot que pego da funcao Temas.R   
     ggplot(dados[!is.na(dados[,grupo]),],
              aes(x = .data[[eixo_x]], y = .data[[eixo_y]], fill = Valor,
                  text = paste0(eixo_x, ": ", get(eixo_x), '<br>',
                                eixo_y, ": ", get(eixo_y), '<br>',
                                "Valor: ", Valor,
                                sep = " "))) +
      geom_bar(stat = "identity", position= "stack") +
      facet_wrap(~ano, scales = "free") +
      labs(x = eixo_x,
           y = paste0(eixo_y, " de Faxinas por Valor"),
           fill = "Valor",
           title = paste0(eixo_y, " de Faxinas por ",
                          eixo_x, " e Ano",
                          sep = " ", collapse = " "))  +
      scale_fill_brewer(palette = "Set2", guide = FALSE) +
      tema_facets
  }

  # senao
  else{
    
    # calcula a quantidade de faxinas e proporçao de faxinas por periodo de tempo (cada dia da semana ou cada mes)
    dados <- dados %>% summarize(Quantidade = sum(Quantidade)) %>%
      mutate(`Proporção` = round(Quantidade/sum(Quantidade), 2))  
      
    # cria um grafico de barras da quantidade/proporcao de faxinas realizadas 
     ggplot(dados,
           aes(x = .data[[eixo_x]], y = .data[[eixo_y]], fill = .data[[grupo]],
               text = paste0(eixo_x, ": ", get(eixo_x), '<br>',
                             eixo_y, ": ", get(eixo_y), sep = " "))) +
      geom_bar(stat = "identity", position = "stack") +
      facet_grid(~ano, scales = "free") + 
      labs(x = eixo_x, 
           y = paste0(eixo_y, " de Faxinas"),
           fill = grupo,
           title = paste0(eixo_y, " de Faxinas por ", 
                          eixo_x, " e Ano",
                          sep = " ", collapse = " "))  +
      tema_facets
  }
  
}


# funcao do grafico de linhas da secao de informacoes gerais das faxinas
lineplot_secao1 <- function(dados, eixo_x, eixo_y){
  
  
  # cria um grafico de linhas com a quantidade/proporcao de faxinas realizadas 
  # por periodo de tempo (cada dia da semana ou cada mes)
  # geom_line cria as linhas do grafico 
  ggplot(dados %>% summarize(Quantidade = sum(Quantidade)) %>%
           mutate(`Proporção` = round(Quantidade/sum(Quantidade), 2)),
         aes(x = .data[[eixo_x]], y = .data[[eixo_y]], 
             text = paste0(eixo_x, ": ", get(eixo_x), '<br>',
                           eixo_y, ": ", get(eixo_y), sep = " "))) +
    geom_line(aes(group=1), col = "blue") +
    facet_wrap(~ano, scales = "free_x") + 
    labs(x = eixo_x, 
         y = paste0(eixo_y, " de Faxinas"), 
         title = paste0(eixo_y, " de Faxinas por ", 
                        eixo_x, " e Ano",
                        sep = " ", collapse = " "))  +
    tema_facets
  
}

# funcao do grafico boxplot da secao de informacoes gerais das faxinas
# funcao que faz o grafico do boxplot, como o grafico so utiliza o eixo y sendo Quantidade 
# ja deixei ele diretamente com a variavel 
boxplot_secao1 <- function(dados, eixo_x){
  
  # cria um grafico de boxplot com o preco(Valor) das faxinas realizadas 
  # por periodo de tempo (cada dia da semana ou cada mes)
  # geom_boxplot cria a caixinha de boxplot do grafico
  # o facet_grid e parecido com o facet_wrap cria graficos para cada ano
  ggplot(dados,
         aes(x = .data[[eixo_x]], y = Valor , fill = .data[[eixo_x]])) +
    geom_boxplot() +
    facet_grid(~ano, scales = "free") + 
    labs(x = eixo_x, 
         y = "Valor da faxina em reais", 
         title = paste0("Preço das faxinas por ", 
                        eixo_x, " e Ano",
                        sep = " ", collapse = " "))  +
    scale_fill_brewer(palette = "Set3") +
    tema_facets
}

# funcao do grafico de pontos da secao de informacoes gerais das faxinas
point_secao1 <- function(dados, eixo_x, eixo_y, grupo){

  # como valor e um dado numerico para aparecer no grafico de pontos converti em fator
  # pensei aqui parecido com o que fiz no grafico de barras da secao 1
  if(grupo == "Valor"){
    dados$Valor[dados$Valor >= 60 & dados$Valor < 85] <- 1
    dados$Valor[dados$Valor > 80 & dados$Valor <= 105] <- 2
    dados$Valor[dados$Valor > 105 & dados$Valor <= 130] <- 3
    dados$Valor[dados$Valor > 130 & dados$Valor <= 150] <- 4
    dados$Valor[dados$Valor > 150 & dados$Valor <= 170] <- 5
    dados$Valor[dados$Valor > 170 & dados$Valor <= 190] <- 6
    dados$Valor[dados$Valor > 190 & dados$Valor <= 210] <- 7
    dados$Valor[dados$Valor > 210 & dados$Valor <= 230] <- 8
    
    dados <- dados %>% mutate(Valor = factor(Valor))
    
    levels(dados$Valor) <- list("[60-80]" = "1", "[85-105]" = "2", "[105-130]" = "3", 
                                "[130-150]" = "4", "[150-170]" = "5",  "[170-190]" = "6",
                                "[190-210]" = "7", "[210-230]" = "8")  
    dados <- dados %>% 
      summarize(Quantidade = sum(Quantidade)) %>%
      mutate(Proporcao = round(Quantidade/sum(Quantidade), 2)) 
  }
  
  else{
      dados <- dados %>% summarize(Quantidade = sum(Quantidade)) %>%
        mutate(Proporcao = round(Quantidade/sum(Quantidade), 2)) 
  }
    
    # geom_point cria os pontos do grafico 
    ggplot(dados[!is.na(dados[,grupo]),],
                aes(x = .data[[eixo_x]], y = .data[[eixo_y]], fill = .data[[grupo]],
                    text = paste0(eixo_x, ": ", get(eixo_x), '<br>',
                                  eixo_y, ": ", get(eixo_y), '<br>',
                                  grupo, ": ", get(grupo), sep = " "))) +
      geom_point(position = "jitter", aes(size =.data[[grupo]])) +
      facet_grid(~ano, scales = "free") +
      labs(x = eixo_x,
           y = paste0(eixo_y, " de Faxinas"), 
           title = paste0(eixo_y, " de Faxinas por ", 
                          eixo_x, " e Ano",
                          sep = " ", collapse = " "))  +
      tema_facets
        
}

## Funcoes de graficos da secao2 ##

# funcao do grafico de barras da secao de colaboradoras 
barplot_secao2 <- function(dados, eixo_x, eixo_y, grupo_m){
  
  dados <- dados %>% summarize(Quantidade = sum(Quantidade)) %>%
    mutate(`Proporção` = round(Quantidade/sum(Quantidade), 3))
  
  # se o eixo_x escolhido for Colaboradora
  if(eixo_x == "Colaboradora"){
      dados <- dados %>% arrange(Quantidade)  %>%
        # reordena da menor quantidade ate a maior
        mutate(Colaboradora = reorder(factor(Colaboradora), -Quantidade))
  }
  
  # se a opcao grupo escolhido for nenhum
  if(grupo_m == "Nenhum"){
    
    # se dentro dessa opcao o eixo x for Remarcou e o y for Quantidade
    if(eixo_x == "Remarcou" & eixo_y == "Quantidade"){  
      
      # cria um grafico de barras com Remarcou com os niveis Nao e Sim por quantidade 
      # scale_y_continuous coloca os intervalos de valores continuos da escala de y 
      # scale_x_discrete coloca os intervalos discretos de escala do eixo x 
      # coord_flip rotaciona o lado do grafico 
       ggplot(dados,
              aes(x = factor(.data[[eixo_x]],levels = c("Não","Sim")), y = Quantidade,
                  fill =  Colaboradora,
                  text = paste0(eixo_x, ": ", get(eixo_x), '<br>',
                                eixo_y, ": ", get(eixo_y), sep = " "))) +
         geom_bar(stat = "identity", position = "stack") +
         facet_grid(~ano, scales = "free") + 
         labs(x = eixo_x, 
              y = paste0(eixo_y, " de faxinas que não ocorreram"),
              title = paste0(eixo_y, " de faxinas que não ocorreram por Colaboradora e Ano",
                             sep = " ", collapse = " "))  +
         scale_y_continuous(breaks = c(0.0,2,4,6,8,10,12)) +
         scale_x_discrete(breaks = c("Não","Sim")) +
         scale_fill_brewer(palette = "Set2") +
         coord_flip() +
         tema_facets 
    }
    
    # se o eixo x dentro dessa opcao for diferente de colaboradora
    else if(eixo_x != "Colaboradora"){
      
      # faz o grafico de barras de quantidade de faxinas e agrupa por colaboradora 
      ggplot(dados,
             aes(x = .data[[eixo_x]], y = .data[[eixo_y]], fill =  Colaboradora,
                 text = paste0(eixo_x, ": ", get(eixo_x), '<br>',
                               eixo_y, ": ", get(eixo_y), '<br>',
                               "Colaboradora: ", Colaboradora,
                               sep = " "))) +
        geom_bar(stat = "identity", position = "stack") +
        facet_grid(~ano, scales = "free") + 
        labs(x = eixo_x, 
             y = paste0(eixo_y, " de Faxinas"),
             fill = "Colaboradora",
             title = paste0(eixo_y, " de Faxinas por ", 
                            eixo_x, " e Ano",
                            sep = " ", collapse = " "))  +
        scale_fill_brewer(palette = "Set2") +
        tema_facets
    }  
        
    #senao reorderna os dados de quantidade por colaboradora podendo ter colaboradora no eixo x e no grupo
    else{
        ggplot(dados,
               aes(x = reorder(.data[[eixo_x]],-(get(!!eixo_y))), y = .data[[eixo_y]], 
                   fill =  Colaboradora,
                   text = paste0(eixo_x, ": ", get(eixo_x), '<br>',
                                 eixo_y, ": ", get(eixo_y), sep = " "))) +
          geom_bar(stat = "identity", position = "stack") +
          facet_grid(~ano, scales = "free") + 
          labs(x = eixo_x, 
               y = paste0(eixo_y, " de Faxinas"),
               fill = "Colaboradora",
               title = paste0(eixo_y, " de Faxinas por ", 
                              eixo_x, " e Ano",
                              sep = " ", collapse = " "))  +
          scale_fill_brewer(palette = "Set2") +
          tema_facets
      }  
      
    }  
   
   #senao (se escolhemos algum grupo)
    else{
      
      # se o grupo for valor da mesma forma que a secao anterior transforma os dados dos precos das faxinas
      if(grupo_m == "Valor"){
        
        dados <- dados %>% group_by_at(vars(ano, eixo_x, Valor)) %>% 
          summarize(Quantidade = sum(Quantidade)) %>%
          mutate(`Proporção` = round(Quantidade/sum(Quantidade), 2))
          
          dados$Valor[dados$Valor >= 60 & dados$Valor <= 80] <- 1
          dados$Valor[dados$Valor > 80 & dados$Valor <= 105] <- 2
          dados$Valor[dados$Valor > 105 & dados$Valor <= 130] <- 3
          dados$Valor[dados$Valor > 130 & dados$Valor <= 150] <- 4
          dados$Valor[dados$Valor > 150 & dados$Valor <= 170] <- 5
          dados$Valor[dados$Valor > 170 & dados$Valor <= 190] <- 6
          dados$Valor[dados$Valor > 190 & dados$Valor <= 210] <- 7
          dados$Valor[dados$Valor > 210 & dados$Valor <= 230] <- 8
          
          dados <- dados %>% mutate(Valor = factor(Valor))
          
          levels(dados$Valor) <- list("[60-80]" = "1", "[85-105]" = "2", "[105-130]" = "3", 
                                      "[130-150]" = "4", "[150-170]" = "5",  "[170-190]" = "6",
                                      "[190-210]" = "7", "[210-230]" = "8")  
        
        # cria o grafico de barras de valor da faxina por colaboradora e por quantidade de faxinas   
        ggplot(dados[!is.na(dados[,grupo_m]),],
               aes(x = Colaboradora, y = .data[[eixo_y]], fill =  Valor,
                   text = paste0(eixo_x, ": ", get(eixo_x), '<br>',
                                 eixo_y, ": ", get(eixo_y), '<br>',
                                 "Valor : ", Valor, sep = " "))) +
          geom_bar(stat = "identity", position = "stack") +
          facet_grid(~ano, scales = "free") + 
          labs(x = eixo_x, 
               y = paste0(eixo_y, " de Faxinas por Valor"),
               fill = "Valor",
               title = paste0(eixo_y, " de Faxinas por ", 
                              eixo_x, " e Ano",
                              sep = " ", collapse = " "))  +
          scale_fill_brewer(palette = "Set2")  +
          tema_facets
      }
      
      # senao (se for outros grupos)
      else{
        
        # remove os valores vazios desses grupos e cria o grafico de barras por grupo 
        ggplot(dados[!is.na(dados[,grupo_m]),],
               aes(x = .data[[eixo_x]], y = .data[[eixo_y]], fill =  .data[[grupo_m]],
                   text = paste0(eixo_x, ": ", get(eixo_x), '<br>',
                                 eixo_y, ": ", get(eixo_y), sep = " "))) +
          geom_bar(stat = "identity", position = "stack") +
          facet_grid(~ano, scales = "free") + 
          labs(x = eixo_x, 
               y = paste0(eixo_y, " de Faxinas"),
               fill = grupo_m,
               title = paste0(eixo_y, " de Faxinas por ", 
                              eixo_x, " e Ano",
                              sep = " ", collapse = " "))  +
        scale_fill_brewer(palette = "Set2") +
        tema_facets
        
      }
      
    }
  
}  

# funcao do grafico de linhas e pontos da secao de colaboradoras 
linepointplot_secao2 <- function(dados, eixo_x){
    
    # se o eixo x por diferente de Colaboradora
    if(eixo_x != "Colaboradora"){
      
      # calcula a quantidade de faxinas por periodo de tempo e colaboradora 
      # geom_line() + geom_point() criam as linhas e os pontos no objeto ggplot 
      ggplot(dados %>% summarize(Quantidade = sum(Quantidade)),
             aes(x = .data[[eixo_x]], y = Quantidade, group = Colaboradora, col = Colaboradora,
                 text = paste0(eixo_x, ": ", get(eixo_x), '<br>',
                               "Quantidade: ", Quantidade, '<br>',
                               "Colaboradora: ", Colaboradora, sep = " "))) +
      geom_line() + geom_point() + 
      facet_grid(~ano, scales = "free") + 
      labs(x = eixo_x, 
           y = paste0("Quantidade de Faxinas"), 
           col = "Colaboradora",
           title = paste0("Quantidade de Faxinas por ", 
                          eixo_x, " e Ano",
                          sep = " ", collapse = " "))  +
      tema_facets
    }
    
   # senao (se o eixo x for colaboradora retorna vazio pois nao tem como fazer um grafico aqui que 
   # nao seja com o eixo x sendo periodo de tempo  
    else{
      return()
    }
    
}
  
# funcao do grafico de boxplot da secao de colaboradoras 
boxplot_secao2 <- function(dados, eixo_x){

    # se o eixo x for Colaboradora
    if(eixo_x == "Colaboradora"){
      
      # cria um boxplot com os preços das faxinas ordenados por colaboradora 
      ggplot(dados,
               aes(x = reorder(.data[[eixo_x]], -Valor), y = Valor, fill = Colaboradora)) +
      geom_boxplot() +
      facet_grid(~ano, scales = "free") +
      labs(x = eixo_x,
           y = "Valor da faxina em reais", 
           title = paste0("Preço das faxinas por ", 
                          eixo_x, " e Ano",
                          sep = " ", collapse = " "))  +
          scale_fill_brewer(palette = "Set3") +    
          tema_facets
      }
      
    #senao (retorna um valor vazio)  
    else{
      return()
    }
      
}
  

## Secao de disponibilidade das colaboradoras ##
        
# funcao dos graficos de calendario da secao de disponilidade de colaboradoras por quantidade de 
# colaboradoras disponiveis em um dia 
calendario_c <- function(dados, data){
  
  # o eixo x do grafico é a Semana (dom a sáb) 
  # o eixo y é a semana do mes que esta 
  # em text aparece as informacoes quando se passa o mouse em cima do grafico
  # geom_tile cria os quadradinhos do grafico
  # geom_text cria o texto dentro dos quadradinhos 
  # scale_fill_brewer cria a paleta de cores Blues da cor azul mais clara ate uma mais forte
  # facet_rep_wrap cria o quadro maior com os nomes dos meses do grafico colocando um em baixo do outro
  ggplot(dados,
         aes(x = Semana, y = -mês_semana,
             text = paste('Dia do mês: ', dia, '<br>',
                          'Dia da Semana: ', Semana,'<br>',
                          'Semana do mês: ', mês_semana,'<br>',
                          'Quantidade: ', Quantidade, sep = " "))) +
    geom_tile(aes(fill = Quantidade), colour = "white") +
    geom_text(aes(label = dia), size = 2.5,  color = "black") + 
    ggtitle(paste("Disponibilidade de Colaboradoras em", data, sep = " ")) +
    scale_fill_brewer(palette = "Blues") + 
    facet_rep_wrap(~Mês, as.table = TRUE, repeat.tick.labels = 'all', scales = "free") +
    # usa o tema de cores para o grafico do estilo de calendario
    tema_calendario              
}

# funcao dos graficos de calendario da secao de disponilidade de colaboradoras
# por cada colaboradora 
calendario_m <- function(dados, data, mulher){

    # segue uma ideia semelhante ao do grafico anterior a diferenca e que inves de quantidade de 
    # colaboradoras disponiveis para a faxina em cada dia, mostra 
    # apenas o dia que uma determinada colaboradora esta disponivel
    # em text aparece as informacoes quando se passa o mouse em cima do grafico
    # geom_tile cria os quadradinhos do grafico
    # geom_text cria o texto dentro dos quadradinhos
    # scale_fill_manual cria uma paleta unica com um cor azul para o dia que a colaboradora esta disponivel 
    # facet_rep_wrap cria o quadro maior com os nomes dos meses do grafico colocando um em baixo do outro
    ggplot(dados,
           aes(x = Semana, y = -mês_semana, fill = Colaboradora,
               text = paste('Dia do mês: ', dia, '<br>',
                            'Dia da Semana: ', Semana, '<br>',
                            'Semana do mês: ', mês_semana, '<br>',
                            'Colaboradora: ', Colaboradora, sep = " "))) +
      geom_tile() +
      geom_text(aes(label = dia), size = 2.5, color = "black") +
      ggtitle(paste("Disponibilidade por Colaboradora em", data, sep = " ")) +
      scale_fill_manual(drop=FALSE, values = c("dodgerblue")) +
      facet_rep_wrap(~Mês, as.table = TRUE, repeat.tick.labels = 'all', scales = "free_x") +
      tema_calendario          
 
}


## Secao de clientes ##

# funcao dos graficos de barras da secao de clientes 
barplot_clientes <- function(dados, eixo_x){
  
  ggplot(dados, 
         aes(x = .data[[eixo_x]], y = Quantidade, fill = .data[[eixo_x]],
             text = paste0(eixo_x, ": ", get(eixo_x), '<br>',
                  "Quantidade : ", Quantidade, sep = " "))) +
  geom_bar(stat = "identity", position = "stack") + 
  labs(x = eixo_x,
       y = "Quantidade", 
       title = paste0("Quantidade de Clientes Novos por ", 
                      eixo_x,
                      sep = " ", collapse = " ")) +
  facet_grid(~ano, scales = "free") + 
  scale_fill_brewer(palette = "Set3") +
  tema_facets

}

## Secao de feedbacks ##

# funcao dos graficos de barras da secao de feedbacks
barplot_feedbacks <- function(dados, eixo_x, grupo){
  
  # removendo valores nulos de eixo_x,
  dados <- dados[!is.na(dados[,eixo_x]),]
  
  # se nao tiver escolhido nenhum grupo 
   if(grupo == "Nenhum"){
     # grafico de barras de notas/Onde foi colhido? por quantidade 
    grafico_feedback <- ggplot(dados,
           aes(x = .data[[eixo_x]], y = Quantidade, fill = "dodgerblue",
               text = paste0(eixo_x, " : ", get(eixo_x), '<br>',
                             "Quantidade : ", Quantidade, sep = " "))) +
      geom_bar(stat = "identity", position = "dodge") + 
      labs(x = eixo_x,
           y = "Quantidade", 
           title = paste0("Quantidade por ", 
                          eixo_x,
                          sep = " ", collapse = " ")) +
      facet_wrap(~ano,shrink = FALSE) + 
      scale_fill_manual(aesthetics = "fill", values = "dodgerblue") +
      tema_facets +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 0, size = 8))
    
    # se o eixo_x for diferente de Onde foi colhido? adiciona uma escala de eixo continuo em x de 0 a 5 para 
    # previnir esticamentos ou encurtamos no grafico
    if(eixo_x != "Onde foi colhido?"){
      grafico_feedback <- grafico_feedback + 
        scale_x_continuous(breaks = c(0,1,2,3,4,5)) 
    }
    
    grafico_feedback
  }
 
  # senao, se tiver escolhido algum grupo, no eixo_x pode agrupar os dados pela opcao de grupo 
  else{
    
    dados <- dados[!is.na(dados[,grupo]),]
    
    # grafico de barras de notas/Onde foi colhido? por quantidade e por grupo  
    grafico_feedback_grupo <- ggplot(dados,
           aes(x = .data[[eixo_x]], y = Quantidade, fill = .data[[grupo]],
               text = paste0(eixo_x, " : ", get(eixo_x), '<br>',
                             "Quantidade : ", Quantidade, '<br>',
                             grupo, " : ", get(grupo), sep = " "))) +
      geom_bar(stat = "identity", position = "stack") + 
      labs(x = eixo_x,
           y = "Quantidade", 
           title = paste0("Quantidade de Notas de Feedbacks por ", 
                          grupo,
                          sep = " ", collapse = " ")) +
      facet_grid(~ano,scales = "fixed",space = "fixed") + 
      scale_fill_brewer(palette = "Set2") +
      tema_facets +
      theme(axis.text.x = element_text(angle = 0, size = 8))
    
    # se o eixo_x for diferente de Onde foi colhido? adiciona uma escala de eixo continuo em x de 0 a 5 para 
    # previnir esticamentos ou encurtamos no grafico
    if(eixo_x != "Onde foi colhido?"){
      grafico_feedback_grupo <- grafico_feedback_grupo + 
        scale_x_continuous(breaks = c(0,1,2,3,4,5)) 
    }  
  
  grafico_feedback_grupo  
  
  }
}