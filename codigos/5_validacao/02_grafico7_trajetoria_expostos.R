# =============================================================================
# 5.2 Gráfico 7: participação de haitianos no emprego formal segundo a exposição
#     em 2013, municípios do RS, 2010–2023
#
# Entrada: dados/painel_estendido_haitiano.rds   (script 2_painel/06)
# Saída:   resultados/grafico7_trajetoria_expostos.png
#
# Grupos: os 10 municípios mais expostos em 2013, os demais com haitianos em
# 2013 e os sem haitianos em 2013. Média simples da participação em cada grupo.
# =============================================================================

source("00_configuracao.R")
source("codigos/funcoes/padrao_graficos.R")   # cores, tema, num_br e salvar

library(dplyr)
library(tidyr)
library(ggplot2)

exigir("painel_estendido_haitiano.rds")
painel_est <- readRDS(file.path(pasta, "painel_estendido_haitiano.rds"))

top10 <- painel_est %>%
  filter(ano == 2013) %>%
  slice_max(share_hai_2013, n = 10, with_ties = FALSE) %>%
  pull(id_municipio)

traj <- painel_est %>%
  filter(!is.na(share_hai_2013)) %>%
  mutate(grupo = case_when(id_municipio %in% top10 ~ "10 municípios mais expostos em 2013",
                           share_hai_2013 > 0      ~ "Demais municípios com haitianos em 2013",
                           TRUE                    ~ "Municípios sem haitianos em 2013")) %>%
  group_by(grupo, ano) %>%
  summarise(part = mean(share_haitiano_real, na.rm = TRUE) * 100, .groups = "drop")

# Conferência (texto da seção 4.3.1): 10 mais expostos = 1,35 (2013), 2,43 (2018),
# 2,40 (2020), 1,23 (2023); demais com haitianos = 0,12 (2013) e 0,28 (2023)
print(traj %>% pivot_wider(names_from = grupo, values_from = part), n = Inf)

g7 <- ggplot(traj, aes(ano, part, colour = grupo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = c("10 municípios mais expostos em 2013"      = cor_hai,
                                 "Demais municípios com haitianos em 2013" = "#8FB4DE",
                                 "Municípios sem haitianos em 2013"        = "grey60"),
                      name = NULL) +
  scale_x_continuous(breaks = 2010:2023) +
  scale_y_continuous(labels = function(x) num_br(x, 1)) +
  labs(x = NULL,
       y = "Participação de haitianos no emprego formal (%)",
       caption = "Fonte: elaboração própria com base em dados da RAIS (Base dos Dados).") +
  tema +
  theme(legend.position = "top", legend.direction = "horizontal")

print(g7)
salvar(g7, "grafico7_trajetoria_expostos.png", w = 9, h = 5.5)
