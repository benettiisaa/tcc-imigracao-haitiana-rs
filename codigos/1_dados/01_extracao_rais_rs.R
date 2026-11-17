# =============================================================================
# 1.1 Extração dos microdados da RAIS: vínculos ativos em 31/12, RS, 2010 em diante
#
# Fonte:  Base dos Dados (BigQuery), tabela basedosdados.br_me_rais.microdados_vinculos
#         (acesso registrado nas referências do artigo: 24 ago. 2026)
# Saída:  dados/dados_rais_rs_2010_2024_bruto.rds
#
# Requer uma conta no Google Cloud com um projeto em que o BigQuery esteja ativo
# (o uso gratuito costuma ser suficiente). Substitua "SEU-PROJETO-GCP" pelo ID
# do seu projeto. O nome do arquivo ("2010_2024") foi mantido por compatibilidade
# com as versões anteriores; a série extraída vai de 2010 a 2025.
# Os vínculos são agregados por município, nacionalidade, subclasse da CNAE 2.0,
# escolaridade, raça/cor e faixa etária.
# =============================================================================

source("00_configuracao.R")

library(basedosdados)
library(bit64)   # as colunas inteiras do BigQuery chegam como integer64

set_billing_id("SEU-PROJETO-GCP")

query <- "
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
),
dicionario_grau_instrucao_1985_2005 AS (
    SELECT chave AS chave_grau_instrucao_1985_2005, valor AS descricao_grau_instrucao_1985_2005
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE nome_coluna = 'grau_instrucao_1985_2005' AND id_tabela = 'microdados_vinculos'
),
dicionario_grau_instrucao_apos_2005 AS (
    SELECT chave AS chave_grau_instrucao_apos_2005, valor AS descricao_grau_instrucao_apos_2005
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE nome_coluna = 'grau_instrucao_apos_2005' AND id_tabela = 'microdados_vinculos'
),
dicionario_raca_cor AS (
    SELECT chave AS chave_raca_cor, valor AS descricao_raca_cor
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE nome_coluna = 'raca_cor' AND id_tabela = 'microdados_vinculos'
),
dicionario_faixa_etaria AS (
    SELECT chave AS chave_faixa_etaria, valor AS descricao_faixa_etaria
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE nome_coluna = 'faixa_etaria' AND id_tabela = 'microdados_vinculos'
)
SELECT
    dados.ano AS ano,
    dados.id_municipio AS id_municipio,
    diretorio_municipio.nome AS municipio_nome,
    descricao_nacionalidade AS nacionalidade,
    dados.cnae_2_subclasse AS cnae_2_subclasse,
    diretorio_cnae_2.descricao_secao AS cnae_secao,
    diretorio_cnae_2.descricao_divisao AS cnae_divisao,
    descricao_grau_instrucao_1985_2005 AS grau_instrucao_1985_2005,
    descricao_grau_instrucao_apos_2005 AS grau_instrucao_apos_2005,
    descricao_raca_cor AS raca_cor,
    descricao_faixa_etaria AS faixa_etaria,
    -- estoque de vínculos ativos em 31/12
    COUNT(*) AS qtd_vinculos,
    -- soma bruta (não a média já pronta) para permitir recalcular médias corretamente ponderadas depois no R
    SUM(dados.valor_remuneracao_media) AS soma_remuneracao_media,
    SUM(dados.valor_remuneracao_dezembro) AS massa_salarial_dezembro
FROM `basedosdados.br_me_rais.microdados_vinculos` AS dados
LEFT JOIN `dicionario_nacionalidade`
    ON dados.nacionalidade = chave_nacionalidade
LEFT JOIN `dicionario_vinculo_ativo_3112`
    ON dados.vinculo_ativo_3112 = chave_vinculo_ativo_3112
LEFT JOIN (SELECT DISTINCT id_municipio, nome FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_municipio
    ON dados.id_municipio = diretorio_municipio.id_municipio
LEFT JOIN (SELECT DISTINCT subclasse, descricao_secao, descricao_divisao FROM `basedosdados.br_bd_diretorios_brasil.cnae_2`) AS diretorio_cnae_2
    ON dados.cnae_2_subclasse = diretorio_cnae_2.subclasse
LEFT JOIN `dicionario_grau_instrucao_1985_2005`
    ON dados.grau_instrucao_1985_2005 = chave_grau_instrucao_1985_2005
LEFT JOIN `dicionario_grau_instrucao_apos_2005`
    ON dados.grau_instrucao_apos_2005 = chave_grau_instrucao_apos_2005
LEFT JOIN `dicionario_raca_cor`
    ON dados.raca_cor = chave_raca_cor
LEFT JOIN `dicionario_faixa_etaria`
    ON dados.faixa_etaria = chave_faixa_etaria
WHERE
    dados.sigla_uf = 'RS'
    AND dados.ano >= 2010  -- toda a série disponível a partir de 2010
    AND descricao_vinculo_ativo_3112 = 'Sim'  -- só vínculos ativos em 31/12
GROUP BY
    ano, id_municipio, municipio_nome, nacionalidade, cnae_2_subclasse, cnae_secao, cnae_divisao,
    grau_instrucao_1985_2005, grau_instrucao_apos_2005, raca_cor, faixa_etaria
"

dados_rais_rs <- read_sql(query, billing_project_id = get_billing_id())

dim(dados_rais_rs)   # na extração original: 10.357.133 linhas x 14 colunas

saveRDS(dados_rais_rs, file.path(pasta, "dados_rais_rs_2010_2024_bruto.rds"))
