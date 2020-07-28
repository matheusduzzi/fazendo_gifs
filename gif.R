library(ggplot2)
library(gganimate)
library(ggthemes)

library(readr)
library(tidyr)
library(dplyr)
grao = read_csv("grao.csv")
grao = grao[,-1]

p_grao = grao %>% spread(name,rend) %>% drop_na() %>% filter(PRODUTO == "ALGODÃO" | PRODUTO == "MILHO TOTAL" | PRODUTO == "SOJA" |
                                                 PRODUTO == "TRIGO" | PRODUTO == "AVEIA" | PRODUTO == "ARROZ" | PRODUTO == "CENTEIO" 
                                                 | PRODUTO == "CEVADA" | PRODUTO == "FEIJÃO TOTAL" | PRODUTO == "GIRASSOL" | PRODUTO == "AMENDOIM TOTAL")
p_grao$ano = gsub("/[0-9][0-9]","",p_grao$ano) 
p_grao= p_grao %>% filter(ano != "2019 Previsão (¹)")
p_grao$ano= as.integer(p_grao$ano)

p_grao$PRODUTO = gsub("MILHO TOTAL","MILHO",p_grao$PRODUTO)
p_grao$PRODUTO = gsub("FEIJÃO TOTAL","FEIJÃO",p_grao$PRODUTO)
p_grao$PRODUTO = gsub("AMENDOIM TOTAL","AMENDOIM",p_grao$PRODUTO)

p <- ggplot(
  p_grao, 
  aes(x = ano, y=producao, colour = PRODUTO)) +
  geom_line(size=1.5) +
  labs(x = "Ano", y = "Produção (mil toneladas)") + theme_stata() + scale_color_stata() +
  ggtitle("Produção") + 
  # gganimate code
  transition_reveal(along = ano)

animate(p, fps = 10, width = 750, height = 450)


anim_save("producao.gif")

p <- ggplot(
  p_grao, 
  aes(x = ano, y=area, colour = PRODUTO)) +
  geom_line(size=1.5) +
  labs(x = "Ano", y = "Área (mil hectares)") + theme_stata() + scale_color_stata() +
  ggtitle("Área Plantada") + 
  # gganimate code
  transition_reveal(along = ano)

animate(p, fps = 10, width = 750, height = 450)


anim_save("area.gif")
