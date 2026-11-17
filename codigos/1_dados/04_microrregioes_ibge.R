# =============================================================================
# 1.4 Microrregiões geográficas dos municípios do RS (API de Localidades do IBGE)
#
# Fonte:  https://servicodados.ibge.gov.br/api/v1/localidades/estados/43/municipios
#         (acesso em 15 out. 2026)
# Saída:  dados/microrregioes_rs.rds
#
# Usado nos efeitos fixos e no agrupamento por microrregião (Tabela 4 e Tabela 7
# do Apêndice B). Precisa de internet apenas na primeira execução; se o arquivo
# já existir em dados/, ele não é baixado de novo.
# =============================================================================

source("00_configuracao.R")

library(dplyr)
library(jsonlite)

arq_micro <- file.path(pasta, "microrregioes_rs.rds")

if (!file.exists(arq_micro)) {
  ibge <- fromJSON("https://servicodados.ibge.gov.br/api/v1/localidades/estados/43/municipios",
                   flatten = TRUE)
  micro <- tibble(id_municipio    = as.character(ibge$id),
                  microrregiao    = ibge$microrregiao.nome,
                  regiao_imediata = ibge$`regiao-imediata.nome`)
  saveRDS(micro, arq_micro)
}

micro <- readRDS(arq_micro)

# Conferência: 497 municípios e 35 microrregiões
nrow(micro)
n_distinct(micro$microrregiao)
