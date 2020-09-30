## tentativa de usar shiny modules para as secoes no server

library(tidyverse)

source("faxinas.R")
source("Temas.R")


## funcoes de graficos da secao1

# barplot_secao1


# lineplot_secao1

## boxplot da secao1

# funcao que faz o grafico do boxplot, como o grafico so utiliza o eixo y sendo Quantidade 
# ja deixei ele diretamente com a variavel 
boxplot_secao1 <- function(dados, eixo_x){
  ggplot(dados %>% summarize(Quantidade = cumsum(Quantidade)),
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



