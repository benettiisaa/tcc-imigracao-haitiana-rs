# =============================================================================
# 2.2 Participação de haitianos no emprego formal, por município e ano
#     share_haitiano_real = vínculos de haitianos / vínculos totais (31/12)
#
# Entrada: dados/dados_rais_rs_2010_2024_filtrado.rds   (script 1_dados/03)
# Saída:   dados/share_real_haitiano.rds (2010–2025)
# =============================================================================

source("00_configuracao.R")

library(dplyr)

exigir("dados_rais_rs_2010_2024_filtrado.rds")
dados_rais_rs <- readRDS(file.path(pasta, "dados_rais_rs_2010_2024_filtrado.rds"))

share_real_haitiano <- dados_rais_rs %>%
  group_by(ano, id_municipio) %>%
  summarise(
    vinculos_totais = sum(qtd_vinculos, na.rm = TRUE),
    vinculos_haitianos = sum(qtd_vinculos[nacionalidade == "Haitiano"], na.rm = TRUE),
    share_haitiano_real = vinculos_haitianos / vinculos_totais,
    .groups = "drop"
  )

# Conferências: 7.949 linhas (497 municípios x 16 anos, menos Pinto Bandeira em
# 2010–2012, que ainda não existia) e nenhum valor ausente
dim(share_real_haitiano)
n_distinct(share_real_haitiano$id_municipio)
sum(is.na(share_real_haitiano$share_haitiano_real))

saveRDS(share_real_haitiano, file.path(pasta, "share_real_haitiano.rds"))
