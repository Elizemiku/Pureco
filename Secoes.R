## tentativa de usar shiny modules para as secoes no server

library(tidyverse)

source("faxinas.R")
source("Temas.R")


## funcoes de graficos da secao1

# barplot_secao1
barplot_secao1 <- function(dados, eixo_x, eixo_y, part){
  
    dados <- dados %>% summarize(Quantidade = sum(Quantidade)) %>%
      mutate(Proporcao = round(Quantidade/sum(Quantidade), 2))

    g <- ggplot(dados[!is.na(dados[,part]),],
           aes(x = .data[[eixo_x]], y = .data[[eixo_y]], fill = .data[[part]])) +
      geom_bar(stat = "identity", position = "stack") +
      facet_grid(~ano, scales = "free_x") + 
      labs(x = eixo_x, 
           y = "Quantidade de Faxinas", 
           title = paste0("Quantidade de Faxinas por ", 
                          eixo_x, " e Ano",
                          sep = " ", collapse = " "))  +
      tema_facets
    
    if(part != "Valor"){
      g <- g + scale_fill_viridis_d()
    }

    g
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
boxplot_secao1 <- function(dados, eixo_x, part){
  ggplot(dados %>% summarize(Quantidade = cumsum(Quantidade)),
         aes(x = .data[[eixo_x]], y = Quantidade, fill = .data[[part]])) +
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



