# =============================================================================
# Apêndice B, Gráfico 1: distribuição do emprego formal no RS por macrossetor,
#                        brasileiros e imigrantes, 2010–2025 (soma dos anos)
#
# Entrada: dados/dados_rais_rs_2010_2024_filtrado.rds
# Saída:   resultados/apB_grafico1_distribuicao_setorial.png
#
# Extraído do script de análise exploratória da RAIS. Os rótulos usam vírgula
# decimal (na versão do artigo, aparecem com ponto: 52.8%). Os percentuais são
# iguais com a base filtrada e com a base sem filtro.
# =============================================================================

source("00_configuracao.R")
source("codigos/funcoes/padrao_graficos.R")

library(dplyr)
library(ggplot2)

dados_rais_rs <- carregar_rais(filtrada = TRUE)

dados_setor_grafico <- dados_rais_rs %>%
  filter(!is.na(setor_macro)) %>%
  group_by(grupo_nacionalidade, setor_macro) %>%
  summarise(vinculos = sum(qtd_vinculos), .groups = "drop") %>%
  group_by(grupo_nacionalidade) %>%
  mutate(pct = vinculos / sum(vinculos) * 100) %>%
  arrange(grupo_nacionalidade, desc(vinculos))

dados_setor_grafico   # imigrantes: 52,8% na indústria; brasileiros: 69,6% nos serviços

grafico_setorial <- ggplot(dados_setor_grafico, aes(x = setor_macro, y = pct, fill = grupo_nacionalidade)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(num_br(pct, 1), "%")), position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Brasileiro" = "#1F3864", "Imigrante" = "#BFBFBF")) +
  labs(title = "Distribuição setorial do emprego formal no RS (2010-2025)",
       x = "Setor", y = "% dos vínculos do grupo", fill = NULL) +
  scale_y_continuous(limits = c(0, 80)) +
  theme_minimal() +
  theme(legend.position = "top")

grafico_setorial

ggsave(file.path(saida, "apB_grafico1_distribuicao_setorial.png"),
       plot = grafico_setorial, width = 8, height = 5, dpi = 300,
       device = ragg::agg_png)
