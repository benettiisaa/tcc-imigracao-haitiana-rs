# =============================================================================
# 2.1 Produtividade do trabalho municipal, 2013–2023
#     PIB (e VAB) municipal real dividido pelos vínculos formais ativos em 31/12
#
# Entradas: dados/PIB Municipal/19092428-pib-municipios-rs-2002-2023-serie-historica.xlsx
#             (DEE/SPGG; https://dee.rs.gov.br/pib-municipal, acesso em 28 ago. 2026)
#           dados/dados_rais_rs_2010_2024_filtrado.rds   (script 1_dados/03)
# Saída:    dados/painel_produtividade.rds
#
# Deflator: IPCA anual (variação dez./dez.; IBGE, SIDRA, tabela 1737), valores a
# preços de dezembro de 2023.
# =============================================================================

source("00_configuracao.R")

library(dplyr)
library(readxl)

arq_pib <- file.path("PIB Municipal", "19092428-pib-municipios-rs-2002-2023-serie-historica.xlsx")
exigir(arq_pib, "dados_rais_rs_2010_2024_filtrado.rds")


# -----------------------------------------------------------------------------
# 1. PIB e VAB municipais (DEE/SPGG)
# -----------------------------------------------------------------------------
dee_raw <- read_excel(file.path(pasta, arq_pib), sheet = "dados")

dee_rs <- dee_raw %>%
  select(1:11) %>%
  setNames(c(
    "ano", "id_municipio", "municipio_nome",
    "vab_agropecuaria", "vab_industria", "vab_servicos",
    "vab_administracao_publica", "vab_total",
    "impostos_produtos", "pib", "pib_per_capita"
  )) %>%
  mutate(ano = as.integer(ano)) %>%
  filter(ano >= 2013, ano <= 2023)

dim(dee_rs)   # 5.467 linhas (497 municípios x 11 anos)

# Padronização de nomes: três municípios aparecem com grafias diferentes ao
# longo da série da DEE, o que duplicaria municípios na junção com a população
dee_rs <- dee_rs %>%
  mutate(municipio_nome = case_when(
    municipio_nome == "Restinga Sêca" ~ "Restinga Seca",
    municipio_nome == "Vespasiano Corrêa" ~ "Vespasiano Correa",
    municipio_nome == "Westfália" ~ "Westfalia",
    TRUE ~ municipio_nome
  ))

# Conferência: cada código de município com um único nome
dee_rs %>%
  distinct(id_municipio, municipio_nome) %>%
  count(id_municipio) %>%
  filter(n > 1)


# -----------------------------------------------------------------------------
# 2. Vínculos formais ativos em 31/12 por município e ano (RAIS filtrada)
# -----------------------------------------------------------------------------
dados_rais_rs <- readRDS(file.path(pasta, "dados_rais_rs_2010_2024_filtrado.rds"))

vinculos_totais_municipio_ano <- dados_rais_rs %>%
  filter(ano >= 2013, ano <= 2023) %>%
  group_by(ano, id_municipio, municipio_nome) %>%
  summarise(vinculos_totais = sum(qtd_vinculos), .groups = "drop")

rm(dados_rais_rs); gc()


# -----------------------------------------------------------------------------
# 3. Produtividade nominal
# -----------------------------------------------------------------------------
painel_produtividade <- dee_rs %>%
  left_join(
    vinculos_totais_municipio_ano %>% select(ano, id_municipio, vinculos_totais),
    by = c("ano", "id_municipio")
  ) %>%
  mutate(
    produtividade_pib = pib / vinculos_totais,
    produtividade_vab = vab_total / vinculos_totais
  )

sum(is.na(painel_produtividade$vinculos_totais))   # deve ser 0


# -----------------------------------------------------------------------------
# 4. Valores reais: deflacionamento pelo IPCA, a preços de dezembro de 2023
#    IPCA anual (%), variação acumulada de janeiro a dezembro de cada ano
# -----------------------------------------------------------------------------
ipca_anual <- tibble::tribble(
  ~ano, ~ipca_pct,
  2010, 5.91,
  2011, 6.50,
  2012, 5.84,
  2013, 5.91,
  2014, 6.41,
  2015, 10.67,
  2016, 6.29,
  2017, 2.95,
  2018, 3.75,
  2019, 4.31,
  2020, 4.52,
  2021, 10.06,
  2022, 5.78,   # a tabela 1737 do SIDRA informa 5,79; a diferença no deflator é
                #   inferior a 0,01% e é absorvida pelos efeitos fixos de ano
  2023, 4.62
)

ipca_anual <- ipca_anual %>%
  arrange(ano) %>%
  mutate(indice = 100 * cumprod(1 + ipca_pct / 100))

indice_base <- ipca_anual$indice[ipca_anual$ano == 2023]
ipca_anual <- ipca_anual %>%
  mutate(fator_deflator = indice_base / indice)

painel_produtividade <- painel_produtividade %>%
  left_join(ipca_anual %>% select(ano, fator_deflator), by = "ano") %>%
  mutate(
    pib_real = pib * fator_deflator,
    vab_total_real = vab_total * fator_deflator,
    produtividade_pib_real = produtividade_pib * fator_deflator,
    produtividade_vab_real = produtividade_vab * fator_deflator
  )

# Conferência: a série real deve suavizar o crescimento nominal
painel_produtividade %>%
  group_by(ano) %>%
  summarise(
    produtividade_nominal = mean(produtividade_pib, na.rm = TRUE),
    produtividade_real = mean(produtividade_pib_real, na.rm = TRUE)
  ) %>%
  print(n = Inf)

saveRDS(painel_produtividade, file.path(pasta, "painel_produtividade.rds"))
