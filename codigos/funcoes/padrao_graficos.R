# =============================================================================
# padrao_graficos.R: padrão visual dos gráficos e leitura da RAIS para os
# scripts descritivos (seção 4.1 e Apêndice B)
# Uso: source("codigos/funcoes/padrao_graficos.R"), depois de source("00_configuracao.R")
# =============================================================================

library(dplyr)
library(ggplot2)

cor_hai <- "#0B2E52"
cor_bra <- "#C9CBCE"
fonte   <- "Fonte: elaboração própria com base em RAIS (Base dos Dados)."

num_br <- function(x, d = 0) format(round(x, d), big.mark = ".", decimal.mark = ",", nsmall = d, trim = TRUE)

tema <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "top",
        plot.caption     = element_text(hjust = 0, size = 8, color = "grey40"))

salvar <- function(g, nome, w = 9, h = 6)
  ggsave(file.path(saida, nome), g, width = w, height = h, dpi = 300, bg = "white")

# Lê a RAIS e acrescenta o grupo (haitianos, brasileiros, outros imigrantes) e o
# indicador de frigorífico (CNAE 1011, 1012 e 1013).
#   filtrada = TRUE:  base com o filtro de remuneração (usada no painel e na
#                     maior parte dos gráficos)
#   filtrada = FALSE: base sem o filtro, usada, na versão do artigo, nos números
#                     do total de imigrantes do Gráfico 1 e das Tabelas 1 e 2 do
#                     Apêndice B. A diferença é inferior a 0,1% (ex.: 53.599
#                     vínculos de imigrantes em 2025, contra 53.561 na base
#                     filtrada); para os haitianos, as duas bases coincidem nos
#                     números apresentados.
carregar_rais <- function(filtrada = TRUE) {
  arquivo <- if (filtrada) "dados_rais_rs_2010_2024_filtrado.rds" else "dados_rais_rs_2010_2024.rds"
  exigir(arquivo)
  readRDS(file.path(pasta, arquivo)) %>%
    mutate(id_municipio = as.character(id_municipio),
           ano          = as.integer(ano),
           grupo        = case_when(nacionalidade %in% "Haitiano"         ~ "Haitianos",
                                    grupo_nacionalidade %in% "Brasileiro" ~ "Brasileiros",
                                    TRUE                                  ~ "Outros imigrantes"),
           frigorifico  = substr(cnae_2_subclasse, 1, 4) %in% c("1011", "1012", "1013"))
}
