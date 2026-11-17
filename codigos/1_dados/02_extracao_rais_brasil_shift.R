# =============================================================================
# 1.2 Extração do componente "shift" do instrumento: estoque nacional de
#     vínculos formais por nacionalidade e ano (Brasil inteiro, sem filtro de UF)
#
# Fonte:  Base dos Dados (BigQuery), tabela basedosdados.br_me_rais.microdados_vinculos
# Saída:  dados/dados_shift_nacional.rds
#
# Usado apenas no instrumento em sua forma clássica (exposição de 2013 x
# crescimento nacional do estoque de haitianos; Tabela 5 do Apêndice B).
# Substitua "SEU-PROJETO-GCP" pelo ID do seu projeto no Google Cloud.
# =============================================================================

source("00_configuracao.R")

library(dplyr)
library(basedosdados)
library(bit64)

set_billing_id("SEU-PROJETO-GCP")

query_shift_nacional <- "
WITH
dicionario_nacionalidade AS (
    SELECT chave AS chave_nacionalidade, valor AS descricao_nacionalidade
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE nome_coluna = 'nacionalidade' AND id_tabela = 'microdados_vinculos'
),
dicionario_vinculo_ativo_3112 AS (
    SELECT chave AS chave_vinculo_ativo_3112, valor AS descricao_vinculo_ativo_3112
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE nome_coluna = 'vinculo_ativo_3112' AND id_tabela = 'microdados_vinculos'
)
SELECT
    dados.ano AS ano,
    descricao_nacionalidade AS nacionalidade,
    COUNT(*) AS qtd_vinculos
FROM `basedosdados.br_me_rais.microdados_vinculos` AS dados
LEFT JOIN `dicionario_nacionalidade`
    ON dados.nacionalidade = chave_nacionalidade
LEFT JOIN `dicionario_vinculo_ativo_3112`
    ON dados.vinculo_ativo_3112 = chave_vinculo_ativo_3112
WHERE
    dados.ano >= 2010
    AND descricao_vinculo_ativo_3112 = 'Sim'
GROUP BY
    ano, nacionalidade
"

dados_shift_nacional <- read_sql(query_shift_nacional, billing_project_id = get_billing_id()) %>%
  mutate(ano = as.integer(ano), qtd_vinculos = as.numeric(qtd_vinculos))

dim(dados_shift_nacional)
dados_shift_nacional %>% filter(nacionalidade == "Haitiano") %>% arrange(ano)   # 11.360 vínculos em 2013

saveRDS(dados_shift_nacional, file.path(pasta, "dados_shift_nacional.rds"))
