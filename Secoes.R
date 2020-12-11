## tentativa de usar shiny modules para as secoes no server

source("faxinas.R")
source("disponibilidade.R")
source("Temas.R")

library("tidyverse")
library("grDevices")
library("RColorBrewer")


## Funcoes de graficos da secao1

# barplot_secao1
barplot_secao1 <- function(dados, eixo_x, eixo_y, grupo){
  

  if(grupo == "Valor"){

     dados <- dados %>% group_by_at(vars(ano, eixo_x, Valor)) %>% 
       summarize(Quantidade = sum(Quantidade)) %>%
       mutate(`Proporção` = round(Quantidade/sum(Quantidade), 2)) %>%
       mutate(Valor = factor(Valor,levels = c(60, 70, 80, 85, 100, 105, 120, 130, 140,
                                              150, 160, 170, 190, 200, 210, 230)))

     nb.cols <- 16
     cores_valor <- colorRampPalette(brewer.pal(10, "Paired"))(nb.cols)
     
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
      scale_fill_manual(values = cores_valor)  +
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
         y = "Valor pedido pela faxina", 
         title = paste0("Preço das faxinas por ", 
                        eixo_x, " e Ano",
                        sep = " ", collapse = " "))  +
    scale_fill_brewer(palette = "Set3") +
    tema_facets
}

point_secao1 <- function(dados, eixo_x, eixo_y, grupo){

  # como valor e um dado numerico para aparecer no grafico de pontos converti em fator
  if(grupo == "Valor"){
    dados <- dados %>% mutate(Valor = factor(Valor, 
                                             levels = c(60, 70, 80, 85, 100, 105, 120, 130, 140, 
                                                        150, 160, 170, 190, 200, 210, 230))) %>% 
      summarize(Quantidade = sum(Quantidade)) %>%
      mutate(Proporcao = round(Quantidade/sum(Quantidade), 2)) 
  }
  else{
      dados <- dados %>% summarize(Quantidade = sum(Quantidade)) %>%
        mutate(Proporcao = round(Quantidade/sum(Quantidade), 2)) 
  }
  
  # if(eixo_y == "Valor"){
  #   p <- ggplot(dados[!is.na(dados[,grupo]),],
  #               aes(x = .data[[eixo_x]], y = Valor, fill = Valor,
  #                   text = paste0(eixo_x, ": ", get(eixo_x), '<br>',
  #                                 eixo_y, ": ", get(eixo_y), '<br>',
  #                                 grupo, ": ", get(grupo), sep = " "))) +
  #     geom_point(position = "jitter") +
  #     facet_grid(~ano, scales = "free") +
  #     labs(x = eixo_x,
  #          y = paste0(eixo_y, " de Faxinas"), 
  #          title = paste0(eixo_y, " de Faxinas por ", 
  #                         eixo_x, " e Ano",
  #                         sep = " ", collapse = " "))  +
  #     tema_facets
  # }
  
  # else{
    
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
#os facets estao estranhos com proporção 
barplot_secao2 <- function(dados, eixo_x, eixo_y, grupo_m){
  
  dados <- dados %>% summarize(Quantidade = sum(Quantidade)) %>%
    mutate(`Proporção` = round(Quantidade/sum(Quantidade), 3))
  
  if(eixo_x == "Colaboradora"){
      dados <- dados %>% arrange(Quantidade)  %>%
        mutate(Colaboradora = reorder(factor(Colaboradora), -Quantidade))
      
      #ver se tem como reordenar casos com grupo_m..tipo..ocorreu..etc
      #faxinas_secao2(faxinas, c(2019,2020), "Mulher", c("Ledinha","Vilanir","Zilza","Lourdes"), "Tipo") %>% select(Mulher, Tipo, Quantidade) %>% drop_na() %>%  summarize(Quantidade = sum(Quantidade)) %>%
      # mutate(`Proporção` = Quantidade/sum(Quantidade)) %>%
      #   arrange(ano,Quantidade,Tipo) %>% ggplot(aes(x=reorder(Mulher,-Quantidade), y = Quantidade, fill = Mulher))
      # + geom_bar(stat = "identity") + facet_wrap(Tipo~ano)
      # 
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
               aes(x = reorder(.data[[eixo_x]],-(get(!!eixo_y))), y = .data[[eixo_y]], fill =  Colaboradora,
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
  
    # testar isso depois pq as proporcao com facets do grupo_m da errado   
    # else if(eixo_x != "Mulher"){
    #   ggplot(dados[!is.na(dados[,grupo_m]),],
    #          aes(x = .data[[eixo_x]], y = .data[[eixo_y]], fill =  Mulher,
    #              text = paste0(eixo_x, ": ", get(eixo_x), '<br>',
    #                            eixo_y, ": ", get(eixo_y), sep = " "))) +
    #     geom_bar(stat = "identity", position = "stack") +
    #     facet_wrap(.data[[grupo_m]]~ano, scales = "free") + 
    #     labs(x = eixo_x, 
    #          y = paste0(eixo_y, " de Faxinas"),
    #          title = paste0(eixo_y, " de Faxinas por ", 
    #                         eixo_x, " e Ano",
    #                         sep = " ", collapse = " "))  +
    #     tema_facets
    #   
    # }  
  
    # ver como faz pra ordenar quando tem grupo #
    else{
      
        if(grupo_m == "Valor"){
          
          dados <- dados %>% group_by_at(vars(ano, eixo_x, Valor)) %>% 
            summarize(Quantidade = sum(Quantidade)) %>%
            mutate(`Proporção` = round(Quantidade/sum(Quantidade), 2)) %>%
            mutate(Valor = factor(Valor,levels = c(60, 70, 80, 85, 100, 105, 120, 130, 140,
                                                   150, 160, 170, 190, 200, 210, 230))) 
          
          nb.cols <- 16
          cores_valor <- colorRampPalette(brewer.pal(10, "Paired"))(nb.cols)
          
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
            scale_fill_manual(values = cores_valor) +
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
    
  # linepointplot_secao2
  #ARRUMAR LEGENDAS(TEXT)
  linepointplot_secao2 <- function(dados, eixo_x){
    
    if(eixo_x != "Colaboradora"){
    ggplot(dados %>% summarize(Quantidade = sum(Quantidade)),
           aes(x = .data[[eixo_x]], y = Quantidade, group = Colaboradora, col = Colaboradora,
               text = paste0(eixo_x, ": ", get(eixo_x), '<br>',
                             " Quantidade: ", Quantidade, '<br>',
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
           y = "Valor pedido pela faxina", 
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
  


calendario_m <- function(dados, data, mulher){

  ggplot(dados,
         aes(y = mês_semana, x = Semana, fill = Colaboradora,
             text = paste('Dia da Semana: ', Semana, '<br>',
                          'Semana do mês: ', mês_semana, '<br>',
                          'Colaboradora: ', Colaboradora, '<br>',
                          'Dia: ', dia))) +
    geom_tile(colour = "white") +
    facet_wrap(~ano_mês, as.table = TRUE) +
    scale_y_continuous(breaks = c(1,2,3,4,5,6)) +
    scale_x_discrete(breaks = c("sáb","sex","qui","qua","ter","seg","dom")) +
    scale_fill_manual(values = c("dodgerblue")) + 
    tema_calendario              
}


