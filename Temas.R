## Temas para os gráficos do pacote ggplot utilizados ## 

#temas usados em gráficos.Rmd #
tema_geral <- theme(
  axis.line = element_line(colour = "black"),
  panel.background = element_rect(fill = "white", size = 2),
  panel.grid.major = element_line(colour = "gray",
                                  size = 1,
                                  linetype = "solid"),
  panel.grid.minor = element_line(colour = "gray",
                                  size = 1,
                                  linetype = "solid")) 

tema_faxinas2 <- theme(
  legend.position = 'none',
  axis.line = element_line(colour = "black"),
  axis.text.x = element_text(angle = 20, size = 8),
  panel.background = element_rect(fill = "white", size = 2),
  panel.grid.major = element_line(colour = "gray",
                                  size = 1,
                                  linetype = "solid"),
  panel.grid.minor = element_line(colour = "gray",
                                  size = 1,
                                  linetype = "solid"))


                                   
# tema usado tanto nos gráficos do site quanto no arquivo Graficos.Rmd
tema_facets <- theme(
    axis.text.x = element_text(angle = 15, size = 8),
    axis.line = element_line(colour = "black"),
    legend.text = element_text(size = 8),
    strip.background = element_rect(colour = "black", fill = "#99CCFF"),
    panel.background = element_rect(fill = "white", size = 2),
    panel.grid.major = element_line(colour = "gray",
                                    size = 1,
                                    linetype = "solid"),
    panel.grid.minor = element_line(colour = "gray",
                                    size = 1,
                                    linetype = "solid")) 

# tema do gráfico de calendário do site
tema_calendario <- theme(
               strip.placement = "outside",
               axis.ticks = element_blank(),
               axis.title.x = element_blank(),
               axis.title.y = element_blank(),
               axis.text.y = element_blank(),
               axis.text.x = element_text(size = 7),
               axis.line = element_line(colour = "black"),
               legend.title = element_text(size = 8, vjust = 0.1),
               legend.text = element_text(size = 7),
               strip.text = element_text(face = "bold", size = 12),
               strip.background = element_rect(colour = "black", fill = "white"),
               panel.grid = element_blank(),
               panel.background = element_blank(),
               panel.grid.major = element_blank(),
               panel.border = element_rect(colour = "grey", fill=NA, size=1))


