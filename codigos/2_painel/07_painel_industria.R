# =============================================================================
# 2.7 Produtividade da indústria, por município, 2013–2020
#     VAB industrial real / vínculos formais na indústria (seções B a F da CNAE)
#
# Entradas: dados/painel_final_haitiano.rds              (script 2_painel/05)
#           dados/dados_rais_rs_2010_2024_filtrado.rds   (script 1_dados/03)
# Saída:    dados/painel_industria.rds (3.976 linhas = 497 municípios x 8 anos)
#
# Usado na última linha da Tabela 7 do Apêndice B (produtividade da indústria).
# =============================================================================

source("00_configuracao.R")

library(dplyr)

exigir("painel_final_haitiano.rds", "dados_rais_rs_2010_2024_filtrado.rds")

painel <- readRDS(file.path(pasta, "painel_final_haitiano.rds")) %>%
  mutate(id_municipio = as.character(id_municipio))


# -----------------------------------------------------------------------------
# 1. Vínculos totais e industriais por município e ano (RAIS filtrada)
# -----------------------------------------------------------------------------
rais <- readRDS(file.path(pasta, "dados_rais_rs_2010_2024_filtrado.rds")) %>%
  filter(ano >= 2013, ano <= 2020) %>%
  mutate(id_municipio = as.character(id_municipio),
         ano          = as.integer(ano),
         industria    = grepl("stria", setor_macro))   # Indústria (seções B, C, D, E e F)

agg <- rais %>%
  group_by(id_municipio, ano) %>%
  summarise(vinc_total = sum(qtd_vinculos),
            vinc_ind   = sum(qtd_vinculos[industria]),
            .groups = "drop")

rm(rais); gc()


# -----------------------------------------------------------------------------
# 2. Painel da indústria
# -----------------------------------------------------------------------------
painel_industria <- painel %>%
  filter(ano <= 2020) %>%
  mutate(ano = as.integer(ano)) %>%
  left_join(agg, by = c("id_municipio", "ano")) %>%
  group_by(id_municipio) %>%
  mutate(vinc_ind_2013 = vinc_ind[ano == 2013][1]) %>%
  ungroup() %>%
  mutate(vinc_ind     = coalesce(vinc_ind, 0),
         vab_ind_real = vab_industria * fator_deflator,
         log_prod_ind = suppressWarnings(ifelse(vinc_ind > 0 & vab_ind_real > 0,
                                                log(vab_ind_real / vinc_ind), NA)))

# Conferências
cat("Vínculos totais da RAIS = vinculos_totais do painel? Diferença máxima:",
    max(abs(painel_industria$vinc_total - painel_industria$vinculos_totais)), "\n")   # deve ser 0
cat("Municípios com 30 ou mais vínculos industriais em 2013:",
    n_distinct(painel_industria$id_municipio[painel_industria$vinc_ind_2013 >= 30]), "\n")   # 392

saveRDS(painel_industria, file.path(pasta, "painel_industria.rds"))
