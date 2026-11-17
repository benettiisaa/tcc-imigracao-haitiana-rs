# =============================================================================
# 1.3 Preparação da RAIS: tipos, grupo de nacionalidade, macrossetor e filtro
#     de remunerações implausíveis
#
# Entrada: dados/dados_rais_rs_2010_2024_bruto.rds   (script 1_dados/01)
# Saídas:  dados/dados_rais_rs_2010_2024.rds          (sem o filtro de remuneração)
#          dados/dados_rais_rs_2010_2024_filtrado.rds (base usada em todo o trabalho)
#
# A base filtrada é a usada na construção do painel, nas estatísticas
# descritivas e nos gráficos. As duas bases têm 16 colunas.
# =============================================================================

source("00_configuracao.R")

library(dplyr)
library(bit64)

exigir("dados_rais_rs_2010_2024_bruto.rds")
dados_rais_rs <- readRDS(file.path(pasta, "dados_rais_rs_2010_2024_bruto.rds"))


# -----------------------------------------------------------------------------
# 1. Tipos: ano e quantidade de vínculos chegam do BigQuery como integer64
# -----------------------------------------------------------------------------
dados_rais_rs <- dados_rais_rs %>%
  mutate(
    ano = as.integer(ano),
    qtd_vinculos = as.numeric(qtd_vinculos)
  )


# -----------------------------------------------------------------------------
# 2. Grupo de nacionalidade: "Brasileira" e "Naturalidade Brasileira" formam o
#    grupo de brasileiros; as demais nacionalidades, o de imigrantes
# -----------------------------------------------------------------------------
dados_rais_rs <- dados_rais_rs %>%
  mutate(
    grupo_nacionalidade = if_else(
      nacionalidade %in% c("Brasileira", "Naturalidade Brasileira"),
      "Brasileiro",
      "Imigrante"
    )
  )

# Conferência
dados_rais_rs %>%
  group_by(grupo_nacionalidade) %>%
  summarise(total_vinculos = sum(qtd_vinculos))


# -----------------------------------------------------------------------------
# 3. Macrossetores a partir da seção da CNAE 2.0
#    Indústria: seções B a F (extrativas, transformação, eletricidade e gás,
#    água e esgoto, construção)
# -----------------------------------------------------------------------------
dados_rais_rs <- dados_rais_rs %>%
  mutate(
    setor_macro = case_when(
      cnae_secao == "Agricultura, Pecuária, Produção Florestal, Pesca E Aquicultura" ~ "Agropecuária",
      cnae_secao %in% c(
        "Indústrias De Transformação",
        "Indústrias Extrativas",
        "Eletricidade E Gás",
        "Água, Esgoto, Atividades De Gestão De Resíduos E Descontaminação",
        "Construção"
      ) ~ "Indústria",
      is.na(cnae_secao) ~ NA_character_,
      TRUE ~ "Serviços"
    )
  )

# Conferência: cada seção deve cair no macrossetor correto
dados_rais_rs %>%
  count(setor_macro, cnae_secao) %>%
  arrange(setor_macro) %>%
  print(n = Inf)

saveRDS(dados_rais_rs, file.path(pasta, "dados_rais_rs_2010_2024.rds"))


# -----------------------------------------------------------------------------
# 4. Filtro de remunerações implausíveis: exclui os registros cuja remuneração
#    média por vínculo supera 50 salários mínimos de 2022 (R$ 1.212 x 50)
# -----------------------------------------------------------------------------
teto <- 1212 * 50

# Registros e vínculos removidos (na base original: 1.667 registros, 6.489 vínculos)
dados_rais_rs %>%
  mutate(remuneracao_por_vinculo = soma_remuneracao_media / qtd_vinculos) %>%
  filter(remuneracao_por_vinculo > teto) %>%
  summarise(linhas_removidas = n(), vinculos_removidos = sum(qtd_vinculos))

dados_rais_rs <- dados_rais_rs %>%
  mutate(remuneracao_por_vinculo = soma_remuneracao_media / qtd_vinculos) %>%
  filter(remuneracao_por_vinculo <= teto) %>%
  select(-remuneracao_por_vinculo)

nrow(dados_rais_rs)   # na base original: 10.355.466 (10.357.133 - 1.667)

saveRDS(dados_rais_rs, file.path(pasta, "dados_rais_rs_2010_2024_filtrado.rds"))
