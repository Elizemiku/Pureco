## Temas utilizados para os graficos ## 

tema_geral <- theme(
  axis.line = element_line(colour = "black"),
  panel.background = element_rect(fill = "white", size = 2),
  panel.grid.major = element_line(colour = "gray",
                                  size = 1,
                                  linetype = "solid"),
  panel.grid.minor = element_line(colour = "gray",
                                  size = 1,
                                  linetype = "solid")) 

tema_facets <- theme(
    axis.text.x = element_text(angle = 20, size = 8),
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

