## tentativa de usar shiny modules para as secoes no server

library(tidyverse)

source("faxinas.R")
source("Temas.R")


## funcoes de graficos da secao1

# barplot_secao1
barplot_secao1 <- function(dados, eixo_x, eixo_y, grupo){
  
  dados <- dados %>% summarize(Quantidade = sum(Quantidade)) %>%
    mutate(Proporcao = round(Quantidade/sum(Quantidade), 2))
  
  if(grupo == "Valor"){
    return(NULL)
  }
  else{

    ggplot(dados[!is.na(dados[,grupo]),],
           aes(x = .data[[eixo_x]], y = .data[[eixo_y]], fill = .data[[grupo]])) +
      geom_bar(stat = "identity", position = "stack") +
      facet_grid(~ano, scales = "free_x") + 
      labs(x = eixo_x, 
           y = "Quantidade de Faxinas", 
           title = paste0("Quantidade de Faxinas por ", 
                          eixo_x, " e Ano",
                          sep = " ", collapse = " "))  +
      tema_facets
  }
}


# lineplot_secao1
lineplot_secao1 <- function(dados, eixo_x, eixo_y){
  ggplot(dados %>% summarize(Quantidade = sum(Quantidade)) %>%
           mutate(Proporcao = round(Quantidade/sum(Quantidade), 2)),
         aes(x = .data[[eixo_x]], y = .data[[eixo_y]])) +
    geom_line(aes(group=1), col = "blue") +
    facet_grid(~ano, scales = "free_x") + 
    labs(x = eixo_x, 
         y = "Quantidade de Faxinas", 
         title = paste0("Quantidade de Faxinas por ", 
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
    facet_grid(~ano) + 
    labs(x = eixo_x, 
         y = "Quantidade de Faxinas", 
         title = paste0("Quantidade de Faxinas por ", 
                        eixo_x, " e Ano",
                        sep = " ", collapse = " "))  +
    scale_fill_viridis_d() +
    tema_facets
}

point_secao1 <- function(dados, eixo_x, eixo_y, grupo){

  dados <- dados %>% summarize(Quantidade = sum(Quantidade)) %>%
    mutate(Proporcao = round(Quantidade/sum(Quantidade), 2))

  ggplot(dados[!is.na(dados[,grupo]),],
              aes(x = .data[[eixo_x]], y = .data[[eixo_y]], fill = factor(.data[[grupo]]))) +
    geom_point(position = "jitter", aes(size = factor(.data[[grupo]]))) +
    facet_grid(~ano, scales = "free_x") +
    labs(x = eixo_x,
         y = "Quantidade de Faxinas",
         title = paste0("Quantidade de Faxinas por ",
                        eixo_x, " e Ano",
                        sep = " ", collapse = " "))  +
    tema_facets
}

