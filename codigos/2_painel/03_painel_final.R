# =============================================================================
# 2.3 Painel municipal, 2013–2023: produtividade, participação de haitianos
#     e população
#
# Entradas: dados/painel_produtividade.rds                (script 2_painel/01)
#           dados/share_real_haitiano.rds                 (script 2_painel/02)
#           dados/populacao_municipios_rs_2010_2025.csv   (IBGE; ver dados/LEIA-ME.md)
# Saída:    dados/painel_final_haitiano.rds (5.467 linhas = 497 municípios x 11 anos)
#
# O painel recebe, nos scripts seguintes, os controles de 2013 (2_painel/04) e
# o instrumento (2_painel/05).
# =============================================================================

source("00_configuracao.R")

library(dplyr)

exigir("painel_produtividade.rds", "share_real_haitiano.rds", "populacao_municipios_rs_2010_2025.csv")

painel_produtividade <- readRDS(file.path(pasta, "painel_produtividade.rds"))
share_real_haitiano  <- readRDS(file.path(pasta, "share_real_haitiano.rds"))


# -----------------------------------------------------------------------------
# 1. População (IBGE): lida como texto para identificar a linha sem dado
#    (Pinto Bandeira, instalado em 2013) e depois convertida para número
# -----------------------------------------------------------------------------
populacao_municipios_rs <- read.csv(
  file.path(pasta, "populacao_municipios_rs_2010_2025.csv"),
  encoding = "UTF-8",
  colClasses = "character"
)

populacao_municipios_rs %>%
  filter(is.na(suppressWarnings(as.numeric(populacao))) | populacao == "")

populacao_municipios_rs <- populacao_municipios_rs %>%
  mutate(
    ano = as.integer(ano),
    populacao = suppressWarnings(as.numeric(populacao))
  )


# -----------------------------------------------------------------------------
# 2. Conferência: cada código de município com um único nome (os nomes foram
#    padronizados no script 2_painel/01) e 497 municípios
# -----------------------------------------------------------------------------
painel_produtividade %>%
  distinct(id_municipio, municipio_nome) %>%
  count(id_municipio) %>%
  filter(n > 1)
n_distinct(painel_produtividade$id_municipio)


# -----------------------------------------------------------------------------
# 3. Junção e variáveis do modelo
# -----------------------------------------------------------------------------
painel_final_haitiano <- painel_produtividade %>%
  left_join(
    share_real_haitiano %>% select(ano, id_municipio, share_haitiano_real),
    by = c("ano", "id_municipio")
  ) %>%
  left_join(
    populacao_municipios_rs %>% select(ano, municipio_nome, populacao),
    by = c("ano", "municipio_nome")
  ) %>%
  mutate(
    log_populacao = log(populacao),
    log_produtividade = log(produtividade_pib_real),   # variável dependente
    dummy_pandemia = if_else(ano %in% c(2020, 2021), 1, 0)
  )

# Conferências: 5.467 linhas e nenhum valor ausente
dim(painel_final_haitiano)
painel_final_haitiano %>%
  summarise(
    sem_share_real = sum(is.na(share_haitiano_real)),
    sem_populacao = sum(is.na(populacao)),
    sem_produtividade = sum(is.na(log_produtividade))
  )

saveRDS(painel_final_haitiano, file.path(pasta, "painel_final_haitiano.rds"))
