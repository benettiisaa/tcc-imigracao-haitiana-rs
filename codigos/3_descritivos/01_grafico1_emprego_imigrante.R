# =============================================================================
# Gráfico 1: evolução do emprego formal de imigrantes no RS, 2010–2025
#            total de imigrantes, haitianos e venezuelanos (estoque em 31/12)
#
# Entrada: dados/dados_rais_rs_2010_2024.rds (base sem o filtro de remuneração,
#          como na versão do artigo; ver codigos/funcoes/padrao_graficos.R)
# Saída:   resultados/grafico1_emprego_formal_imigrantes.png
# =============================================================================

source("00_configuracao.R")
source("codigos/funcoes/padrao_graficos.R")

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

dados_rais_rs <- carregar_rais(filtrada = FALSE)

# Haitianos e venezuelanos fazem parte do total de imigrantes
base_g1 <- dados_rais_rs %>%
  filter(ano >= 2010, ano <= 2025) %>%
  group_by(ano) %>%
  summarise(
    `Imigrantes (total)` = sum(qtd_vinculos[grupo_nacionalidade == "Imigrante"], na.rm = TRUE),
    Haitianos            = sum(qtd_vinculos[nacionalidade == "Haitiano"], na.rm = TRUE),
    Venezuelanos         = sum(qtd_vinculos[nacionalidade == "Venezuelano"], na.rm = TRUE),
    .groups = "drop"
  )

# Conferência (valores do texto: 3.571 em 2010 e 53.599 em 2025) e peso dos dois grupos
base_g1 %>%
  mutate(pct_hai_ven = (Haitianos + Venezuelanos) / `Imigrantes (total)` * 100) %>%
  print(n = Inf)

dados_g1 <- base_g1 %>%
  pivot_longer(-ano, names_to = "serie", values_to = "vinculos") %>%
  mutate(serie = factor(serie, levels = c("Imigrantes (total)", "Haitianos", "Venezuelanos")))

cores <- c(
  "Imigrantes (total)" = "#0B2E52",
  "Haitianos"          = "#4C7FC4",
  "Venezuelanos"       = "#D9822B"
)

grafico1 <- ggplot(dados_g1, aes(x = ano, y = vinculos, color = serie)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_color_manual(values = cores, name = NULL) +
  scale_x_continuous(breaks = 2010:2025) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  labs(
    x = "Ano", y = "Vínculos ativos (31/12)",
    caption = "Fonte: elaboração própria com base em RAIS (Base dos Dados). Haitianos e venezuelanos estão incluídos no total de imigrantes."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(hjust = 0, size = 8, color = "grey40")
  )

grafico1

ggsave(file.path(saida, "grafico1_emprego_formal_imigrantes.png"),
       grafico1, width = 9, height = 5.5, dpi = 300, bg = "white")
