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

## Funcoes de graficos da secao1  ##

# barplot_secao1
barplot_secao1 <- function(dados, eixo_x, eixo_y, grupo){
  

  if(grupo == "Valor"){

     dados <- dados %>% group_by_at(vars(ano, eixo_x, Valor)) %>% 
       summarize(Quantidade = sum(Quantidade)) %>%
       mutate(`Proporção` = round(Quantidade/sum(Quantidade), 2))
     
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
     
     ggplot(dados[!is.na(dados[,grupo]),],
              aes(x = .data[[eixo_x]], y = .data[[eixo_y]], fill = Valor,
                  text = paste0(eixo_x, ": ", get(eixo_x), '<br>',
                                eixo_y, ": ", get(eixo_y), '<br>',
                                "Valor: ", Valor,
                                sep = " "))) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_grid(~ano, scales = "free") +
      labs(x = eixo_x,
           y = paste0(eixo_y, " de Faxinas por Valor"),
           fill = "Valor",
           title = paste0(eixo_y, " de Faxinas por ",
                          eixo_x, " e Ano",
                          sep = " ", collapse = " "))  +
      scale_fill_brewer(palette = "Set2")  +
      coord_flip() + 
      tema_facets
  }

  else{
    
    dados <- dados %>% summarize(Quantidade = sum(Quantidade)) %>%
      mutate(`Proporção` = round(Quantidade/sum(Quantidade), 2)) 
      
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


# lineplot_secao1
lineplot_secao1 <- function(dados, eixo_x, eixo_y){
  ggplot(dados %>% summarize(Quantidade = sum(Quantidade)) %>%
           mutate(`Proporção` = round(Quantidade/sum(Quantidade), 2)),
         aes(x = .data[[eixo_x]], y = .data[[eixo_y]], 
             text = paste0(eixo_x, ": ", get(eixo_x), '<br>',
                           eixo_y, ": ", get(eixo_y), sep = " "))) +
    geom_line(aes(group=1), col = "blue") +
    facet_grid(~ano, scales = "free") + 
    labs(x = eixo_x, 
         y = paste0(eixo_y, " de Faxinas"), 
         title = paste0(eixo_y, " de Faxinas por ", 
                        eixo_x, " e Ano",
                        sep = " ", collapse = " "))  +
    tema_facets
  
}
## boxplot da secao1

# funcao que faz o grafico do boxplot, como o grafico so utiliza o eixo y sendo Quantidade 
# ja deixei ele diretamente com a variavel 
boxplot_secao1 <- function(dados, eixo_x){
  
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

point_secao1 <- function(dados, eixo_x, eixo_y, grupo){

  # como valor e um dado numerico para aparecer no grafico de pontos converti em fator
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

# barplot_secao2
barplot_secao2 <- function(dados, eixo_x, eixo_y, grupo_m){
  
  dados <- dados %>% summarize(Quantidade = sum(Quantidade)) %>%
    mutate(`Proporção` = round(Quantidade/sum(Quantidade), 3))
  
  if(eixo_x == "Colaboradora"){
      dados <- dados %>% arrange(Quantidade)  %>%
        mutate(Colaboradora = reorder(factor(Colaboradora), -Quantidade))

    }

  
    if(grupo_m == "Nenhum"){
      
      if(eixo_x == "Remarcou" & eixo_y == "Quantidade"){  
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
      
      else if(eixo_x != "Colaboradora"){
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

    else{
      
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
          
          ggplot(dados[!is.na(dados[,grupo_m]),],
                 aes(x = Colaboradora, y = .data[[eixo_y]], fill =  Valor,
                     text = paste0(eixo_x, ": ", get(eixo_x), '<br>',
                                   eixo_y, ": ", get(eixo_y), '<br>',
                                   "Valor : ", Valor, sep = " "))) +
            geom_bar(stat = "identity", position = "dodge") +
            facet_grid(~ano, scales = "free") + 
            labs(x = eixo_x, 
                 y = paste0(eixo_y, " de Faxinas por Valor"),
                 fill = "Valor",
                 title = paste0(eixo_y, " de Faxinas por ", 
                                eixo_x, " e Ano",
                                sep = " ", collapse = " "))  +
            scale_fill_brewer(palette = "Set2")  +
            coord_flip() + 
            tema_facets
        }
      
        else{
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

  linepointplot_secao2 <- function(dados, eixo_x){
    
    if(eixo_x != "Colaboradora"){
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
    
    else{
      return()
    }
    
  }
  
  #boxplot_secao2
    boxplot_secao2 <- function(dados, eixo_x){

      if(eixo_x == "Colaboradora"){
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
      
      else{
        return()
      }
      
    }
  

calendario_c <- function(dados, data){
  
  ggplot(dados,
         aes(x = Semana, y = -mês_semana,
             text = paste('Dia da Semana: ', Semana,'<br>',
                          'Semana do mês: ', mês_semana,'<br>',
                          'Quantidade: ', Quantidade, sep = " "))) +
    geom_tile(aes(fill = Quantidade), colour = "white") +
    geom_text(aes(label = dia), size = 2.5,  color = "black") + 
    ggtitle("Disponibilidade de Colaboradoras") +
    scale_fill_brewer(palette = "Blues") + 
    facet_rep_wrap(~Mês, as.table = TRUE, repeat.tick.labels = 'all', scales = "free") +
    tema_calendario              
}


calendario_m <- function(dados, data, mulher){

    ggplot(dados,
           aes(x = Semana, y = -mês_semana, fill = Colaboradora,
               text = paste('Dia da Semana: ', Semana, '<br>',
                            'Semana do mês: ', mês_semana, '<br>',
                            'Colaboradora: ', Colaboradora, sep = " "))) +
      geom_tile() +
      geom_text(aes(label = dia), size = 2.5, color = "black") +
      ggtitle("Disponibilidade por Colaboradora") +
      scale_fill_manual(drop=FALSE, values = c("dodgerblue")) +
      facet_rep_wrap(~Mês, as.table = TRUE, repeat.tick.labels = 'all', scales = "free_x") +
      tema_calendario          
 
}
