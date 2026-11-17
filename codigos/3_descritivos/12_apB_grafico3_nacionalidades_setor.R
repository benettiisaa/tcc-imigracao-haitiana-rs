# =============================================================================
# Apêndice B, Gráfico 3: composição por nacionalidade do emprego formal
#                        imigrante, por setor, RS, 2010–2025
#
# Entrada: dados/dados_rais_rs_2010_2024_filtrado.rds
# Saída:   resultados/apB_grafico3_nacionalidades_setor.png
#
# Leitura do gráfico: a composição é calculada sobre o total de vínculos-ano
# imigrantes ativos em 31/12 acumulados em 2010–2025 (soma de todos os anos),
# não sobre pessoas distintas nem sobre um único ano. Um trabalhador empregado
# ao longo de vários anos é contado uma vez em cada ano em que esteve ativo.
# =============================================================================

source("00_configuracao.R")
source("codigos/funcoes/padrao_graficos.R")

library(dplyr)
library(ggplot2)
library(scales)

dados_rais_rs <- carregar_rais(filtrada = TRUE)

dados_composicao_setorial <- dados_rais_rs %>%
  filter(
    grupo_nacionalidade == "Imigrante",
    ano >= 2010, ano <= 2025,
    setor_macro %in% c("Agropecuária", "Indústria", "Serviços")
  ) %>%
  mutate(
    nacionalidade_grafico = case_when(
      nacionalidade %in% c("Argentina", "Haitiano", "Paraguaia", "Uruguaia", "Venezuelano") ~ nacionalidade,
      TRUE ~ "Outras"
    ),
    nacionalidade_grafico = factor(
      nacionalidade_grafico,
      levels = c("Argentina", "Haitiano", "Paraguaia", "Uruguaia", "Venezuelano", "Outras")
    )
  ) %>%
  group_by(setor_macro, nacionalidade_grafico) %>%
  summarise(vinculos = sum(qtd_vinculos), .groups = "drop") %>%
  group_by(setor_macro) %>%
  mutate(pct = vinculos / sum(vinculos) * 100) %>%
  ungroup() %>%
  mutate(setor_macro = factor(setor_macro, levels = c("Agropecuária", "Indústria", "Serviços")))

# Conferência (texto da seção 4.1): na indústria, haitianos 34,6% e venezuelanos 36,8%
dados_composicao_setorial %>% arrange(setor_macro, nacionalidade_grafico) %>% print(n = Inf)

# Azuis graduados por nacionalidade + cinza para "Outras"
cores_nacionalidade <- c(
  "Argentina"    = "#4C7FC4",
  "Haitiano"     = "#0B2E52",
  "Paraguaia"    = "#C7DBF0",
  "Uruguaia"     = "#8FB4DE",
  "Venezuelano"  = "#1F4E86",
  "Outras"       = "#BFBFBF"
)

grafico_composicao_setorial <- ggplot(
  dados_composicao_setorial,
  aes(x = setor_macro, y = pct, fill = nacionalidade_grafico)
) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(
    aes(label = paste0(format(round(pct, 1), decimal.mark = ","), "%")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.2
  ) +
  scale_fill_manual(values = cores_nacionalidade, name = "Nacionalidade") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.02))) +
  labs(
    x = "Setor",
    y = "% dos vínculos imigrantes do setor",
    caption = "Fonte: elaboração própria com base em RAIS (Base dos Dados). Composição acumulada de vínculos-ano ativos, 2010-2025."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, size = 8, color = "grey40")
  )

grafico_composicao_setorial

ggsave(file.path(saida, "apB_grafico3_nacionalidades_setor.png"),
       grafico_composicao_setorial, width = 9, height = 6, dpi = 300, bg = "white")
