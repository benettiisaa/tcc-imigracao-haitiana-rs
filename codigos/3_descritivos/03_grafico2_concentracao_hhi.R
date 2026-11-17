# =============================================================================
# Gráfico 2: municípios com vínculos formais de haitianos e índice de
#            concentração geográfica (HHI), RS, 2013–2025 (índices, 2013 = 100)
#
# Entrada: dados/dados_rais_rs_2010_2024_filtrado.rds
# Saída:   resultados/grafico2_concentracao_hhi.png
#
# HHI = soma, entre os municípios, do quadrado da participação de cada um no
# total de vínculos de haitianos do estado, x 10.000.
# Observação: o cálculo é o do script original; o código do gráfico foi
# refeito para o repositório a partir da figura publicada no artigo.
# =============================================================================

source("00_configuracao.R")
source("codigos/funcoes/padrao_graficos.R")

library(dplyr)
library(tidyr)
library(ggplot2)

rais <- carregar_rais(filtrada = TRUE)

hhi_hai <- rais %>%
  filter(nacionalidade %in% "Haitiano", ano >= 2012) %>%
  group_by(ano, id_municipio) %>%
  summarise(v = sum(qtd_vinculos), .groups = "drop") %>%
  group_by(ano) %>%
  summarise(vinculos   = sum(v),
            municipios = n_distinct(id_municipio[v > 0]),
            hhi        = sum((v / sum(v))^2) * 10000,
            .groups = "drop")

# Conferência (texto da seção 4.1): HHI entre cerca de 750 e 850 em 2013–2019,
# 627 em 2020 e entre 526 e 599 em 2021–2025; municípios: 62 (2013), 121 (2019),
# 131 (2020) e entre 121 e 129 depois
print(hhi_hai, n = Inf)

# Índices com base em 2013
indices <- hhi_hai %>%
  filter(ano >= 2013) %>%
  mutate(`Concentração geográfica (HHI)` = hhi / hhi[ano == 2013] * 100,
         `Municípios com haitianos`      = municipios / municipios[ano == 2013] * 100) %>%
  select(ano, `Concentração geográfica (HHI)`, `Municípios com haitianos`) %>%
  pivot_longer(-ano, names_to = "serie", values_to = "indice")

g2 <- ggplot(indices, aes(x = ano, y = indice, colour = serie)) +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "grey55") +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = c("Concentração geográfica (HHI)" = cor_hai,
                                 "Municípios com haitianos"      = "#8FB4DE"),
                      name = NULL) +
  scale_x_continuous(breaks = 2013:2025) +
  labs(x = NULL, y = "Índice (2013 = 100)",
       caption = "Fonte: elaboração própria a partir dos dados da RAIS (Base dos Dados, 2026).") +
  tema

g2
salvar(g2, "grafico2_concentracao_hhi.png", w = 9, h = 5.5)
