## tentativa de usar shiny modules para as secoes no server

library(tidyverse)

source("faxinas.R")
source("Temas.R")


## Funcoes de graficos da secao1

# barplot_secao1
barplot_secao1 <- function(dados, eixo_x, eixo_y, grupo){
  
  dados <- dados %>% summarize(Quantidade = sum(Quantidade)) %>%
    mutate(`Proporção` = round(Quantidade/sum(Quantidade), 2))
  
  if(grupo == "Valor"){
    return(NULL)
  }
  else{
    
    ggplot(dados[!is.na(dados[,grupo]),],
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
  
  dados <- dados %>% 
    summarize(Quantidade = cumsum(Quantidade))
  
  ggplot(dados,
         aes(x = .data[[eixo_x]], y = Quantidade, fill = .data[[eixo_x]])) +
    geom_boxplot() +
    facet_grid(~ano, scales = "free") + 
    labs(x = eixo_x, 
         y = "Quantidade de Faxinas", 
         title = paste0("Quantidade de Faxinas por ", 
                        eixo_x, " e Ano",
                        sep = " ", collapse = " "))  +
    scale_fill_viridis_d() +
    tema_facets
}

point_secao1 <- function(dados, eixo_x, eixo_y, grupo){

  # como valor e um dado numerico para aparecer no grafico de pontos converti em fator
  dados <- dados %>% mutate(Valor = factor(Valor, 
                                           levels = c(60, 70, 80, 85, 100, 105, 120, 130, 140, 
                                                      150, 160, 170, 190, 200, 210, 230))) %>% 
    summarize(Quantidade = sum(Quantidade)) %>%
    mutate(Proporcao = round(Quantidade/sum(Quantidade), 2)) 

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
  
    if(eixo_x == "Mulher"){
      dados <- dados %>% arrange(Quantidade)  %>%
        mutate(Mulher = reorder(factor(Mulher), -(get(!!eixo_y))))
      
      #ver se tem como reordenar casos com grupo_m..tipo..ocorreu..etc
      #faxinas_secao2(faxinas, c(2019,2020), "Mulher", c("Ledinha","Vilanir","Zilza","Lourdes"), "Tipo") %>% select(Mulher, Tipo, Quantidade) %>% drop_na() %>%  summarize(Quantidade = sum(Quantidade)) %>%
      # mutate(`Proporção` = Quantidade/sum(Quantidade)) %>%
      #   arrange(ano,Quantidade,Tipo) %>% ggplot(aes(x=reorder(Mulher,-Quantidade), y = Quantidade, fill = Mulher))
      # + geom_bar(stat = "identity") + facet_wrap(Tipo~ano)
      # 
    }

      
    if(grupo_m == "Nenhum"){
      ggplot(dados,
             aes(x = .data[[eixo_x]], y = .data[[eixo_y]], fill =  Mulher,
                 text = paste0(eixo_x, ": ", get(eixo_x), '<br>',
                               eixo_y, ": ", get(eixo_y), sep = " "))) +
        geom_bar(stat = "identity", position = "stack") +
        facet_grid(~ano, scales = "free") + 
        labs(x = eixo_x, 
             y = paste0(eixo_y, " de Faxinas"),
             title = paste0(eixo_y, " de Faxinas por ", 
                            eixo_x, " e Ano",
                            sep = " ", collapse = " "))  +
        tema_facets
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
        ggplot(dados[!is.na(dados[,grupo_m]),],
               aes(x = .data[[eixo_x]], y = .data[[eixo_y]], fill =  .data[[grupo_m]],
                   text = paste0(eixo_x, ": ", get(eixo_x), '<br>',
                                 eixo_y, ": ", get(eixo_y), sep = " "))) +
          geom_bar(stat = "identity", position = "stack") +
          facet_grid(~ano, scales = "free") + 
          labs(x = eixo_x, 
               y = paste0(eixo_y, " de Faxinas"),
               title = paste0(eixo_y, " de Faxinas por ", 
                              eixo_x, " e Ano",
                              sep = " ", collapse = " "))  +
          tema_facets

    }
  
}  
    
  # linepointplot_secao2
  
  linepointplot_secao2 <- function(dados, eixo_x){
    
    if(eixo_x != "Mulher"){
    ggplot(dados %>% summarize(Quantidade = sum(Quantidade)),
           aes(x = .data[[eixo_x]], y = Quantidade, group = Mulher, col = Mulher, 
               text = paste0(eixo_x, ": ", get(eixo_x), '<br>',
                             eixo_y, ": ", get(eixo_y), sep = " "))) +
      geom_line() + 
      geom_point() + 
      facet_grid(~ano, scales = "free") + 
      labs(x = eixo_x, 
           y = paste0(eixo_y, " de Faxinas"), 
           title = paste0(eixo_y, " de Faxinas por ", 
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
     
      if(eixo_x == "Mulher"){
      ggplot(dados %>% summarize(Quantidade = cumsum(Quantidade)), 
               aes(x = reorder(.data[[eixo_x]], -Quantidade), y = Quantidade, fill = Mulher)) +
      geom_boxplot() +
      facet_grid(~ano, scales = "free") + 
      labs(x = eixo_x, 
           y = paste0("Quantidade de Faxinas"), 
           title = paste0("Quantidade de Faxinas por ", 
                          eixo_x, " e Ano",
                          sep = " ", collapse = " "))  +
      tema_facets    
      }
      
      else{
        return()
      }
      
    }
  



