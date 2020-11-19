#encaixar na secao de mulheres  
#
#quanto cada mulher ganhou no ano de 2019 por ex
View(faxinas %>%  filter(ano %in% 2019,
                         Mulher != "NA", `Ocorreu?` == "Sim") %>% 
       group_by(ano, Mulher) %>% summarise(Valor = sum(Valor))) 

# quanto cada mulher ganhou no mes e por ano
View(faxinas %>%  filter(ano %in% 2019,
                         Mulher != "NA", `Ocorreu?` == "Sim") %>% 
       group_by(ano, mes,Mulher) %>% summarise(Valor = sum(Valor))) 



##encaixar em geral 
#quanto o pureco arrecadou por dia da semana/mes/ano

#semana
View(faxinas %>%  filter(ano %in% 2019,
Mulher != "NA", `Ocorreu?` == "Sim") %>% 
  group_by(ano, Semana) %>% summarise(Valor = sum(Valor))) 

#mes
View(faxinas %>%  filter(ano %in% 2019,
Mulher != "NA", `Ocorreu?` == "Sim") %>% 
  group_by(ano, mes) %>% summarise(Valor = sum(Valor))) 

#ano
View(faxinas %>%  filter(ano %in% c(2018,2019,2020) %>%
Mulher != "NA", `Ocorreu?` == "Sim") %>% 
  group_by(ano) %>% summarise(Valor = sum(Valor))) 