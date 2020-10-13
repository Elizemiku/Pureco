## tentativa de usar shiny modules para as secoes no server

library(tidyverse)

source("faxinas.R")
source("Temas.R")


## funcoes de graficos da secao1

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

