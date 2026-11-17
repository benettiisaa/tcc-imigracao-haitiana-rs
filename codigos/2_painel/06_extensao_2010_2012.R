# =============================================================================
# 2.6 Extensão do painel para 2010–2012 (pré-período do estudo de evento)
#
# Cria um arquivo NOVO, painel_estendido_haitiano.rds (2010–2023), usado no
# estudo de evento, no teste de pré-tendência, na Tabela 3, no Gráfico 7 e na
# especificação com ano-base 2012. O painel_final_haitiano.rds (2013–2023) não
# é alterado.
#
# Entradas: dados/painel_final_haitiano.rds              (script 2_painel/05)
#           dados/PIB Municipal/19092428-pib-municipios-rs-2002-2023-serie-historica.xlsx
#           dados/dados_rais_rs_2010_2024_filtrado.rds   (script 1_dados/03)
# Saída:    dados/painel_estendido_haitiano.rds (6.955 linhas)
#
# Fontes das linhas de 2010–2012:
#   - PIB e VAB: planilha da DEE/SPGG (série 2002–2023)
#   - Vínculos totais e de haitianos: RAIS filtrada (mesma base do painel)
#   - Deflator: IPCA, encadeado a partir do fator de 2013 já usado no painel
# =============================================================================

source("00_configuracao.R")

library(dplyr)
library(bit64)
library(readxl)

exigir("painel_final_haitiano.rds", "dados_rais_rs_2010_2024_filtrado.rds",
       file.path("PIB Municipal", "19092428-pib-municipios-rs-2002-2023-serie-historica.xlsx"))

painel_final_haitiano <- readRDS(file.path(pasta, "painel_final_haitiano.rds"))


# -----------------------------------------------------------------------------
# 1. PIB e VAB de 2010–2013 (DEE). 2013 entra só para conferir com o painel.
# -----------------------------------------------------------------------------
arq_pib <- file.path(pasta, "PIB Municipal",
                     "19092428-pib-municipios-rs-2002-2023-serie-historica.xlsx")

pib_dee <- read_excel(arq_pib, sheet = "dados", col_types = "text") %>%
  select(1:11) %>%
  setNames(c("ano", "id_municipio", "municipio_nome", "vab_agropecuaria",
             "vab_industria", "vab_servicos", "vab_administracao_publica",
             "vab_total", "impostos_produtos", "pib", "pib_per_capita")) %>%
  mutate(ano = as.numeric(ano),
         across(vab_agropecuaria:pib_per_capita, as.numeric)) %>%
  filter(ano %in% 2010:2013)

# Conferência: o PIB de 2013 da planilha deve ser igual ao do painel
confere_pib <- painel_final_haitiano %>%
  filter(ano == 2013) %>%
  select(id_municipio, pib_painel = pib) %>%
  inner_join(pib_dee %>% filter(ano == 2013) %>% select(id_municipio, pib), by = "id_municipio")
cat("Maior diferença relativa no PIB de 2013 (deve ser ~0):",
    max(abs(confere_pib$pib - confere_pib$pib_painel) / confere_pib$pib_painel), "\n")

pib_pre <- pib_dee %>% filter(ano %in% 2010:2012)
cat("Municípios por ano na DEE (esperado 496; Pinto Bandeira só existe a partir de 2013):\n")
print(count(pib_pre, ano))


# -----------------------------------------------------------------------------
# 2. Vínculos totais e de haitianos em 2010–2012 (RAIS filtrada)
#    Lê o arquivo grande e filtra os anos logo no início, para poupar memória.
# -----------------------------------------------------------------------------
rais <- readRDS(file.path(pasta, "dados_rais_rs_2010_2024_filtrado.rds"))

# Conferência: como está escrita a nacionalidade haitiana?
print(unique(rais$nacionalidade[grepl("ait", rais$nacionalidade, ignore.case = TRUE)]))

vinc_pre <- rais %>%
  mutate(ano = as.numeric(ano), qtd = as.numeric(qtd_vinculos)) %>%
  filter(ano %in% 2010:2013) %>%
  group_by(id_municipio, ano) %>%
  summarise(vinculos_totais = sum(qtd, na.rm = TRUE),
            vinculos_haitianos = sum(qtd[nacionalidade %in% "Haitiano"], na.rm = TRUE),
            .groups = "drop") %>%
  mutate(share_haitiano_real = vinculos_haitianos / vinculos_totais)

rm(rais); gc()   # libera memória

# Conferência: vínculos e share de 2013 devem bater com o painel
confere_rais <- painel_final_haitiano %>%
  filter(ano == 2013) %>%
  select(id_municipio, v_painel = vinculos_totais, s_painel = share_haitiano_real) %>%
  inner_join(vinc_pre %>% filter(ano == 2013), by = "id_municipio")
cat("Diferença máxima nos vínculos de 2013 (deve ser 0):",
    max(abs(confere_rais$vinculos_totais - confere_rais$v_painel)), "\n")
cat("Diferença máxima na share de 2013 (deve ser ~0):",
    max(abs(confere_rais$share_haitiano_real - confere_rais$s_painel)), "\n")

vinc_pre <- vinc_pre %>% filter(ano %in% 2010:2012)


# -----------------------------------------------------------------------------
# 3. Deflator: IPCA a preços de dezembro de 2023, encadeado a partir de 2013
#    IPCA anual (dez/dez): 2011 = 6,50%; 2012 = 5,84%; 2013 = 5,91%
# -----------------------------------------------------------------------------
fator_2013 <- painel_final_haitiano %>% filter(ano == 2013) %>% pull(fator_deflator) %>% first()

fatores_pre <- tibble(
  ano = c(2012, 2011, 2010),
  fator_deflator = fator_2013 * c(1.0591,
                                  1.0591 * 1.0584,
                                  1.0591 * 1.0584 * 1.0650)
)
print(fatores_pre)   # esperado: cerca de 1,88 (2012), 1,99 (2011) e 2,12 (2010)


# -----------------------------------------------------------------------------
# 4. Monta as linhas de 2010–2012, com as mesmas colunas do painel
# -----------------------------------------------------------------------------
fixas_2013 <- painel_final_haitiano %>%
  filter(ano == 2013) %>%
  select(id_municipio,
         share_hai_2013, share_ind_2013, share_agro_2013, share_frig_2013,
         ql_frig_2013, polo_frig_2013, log_pop_2013,
         share_ind_2012, share_agro_2012, share_frig_2012, ql_frig_2012, polo_frig_2012)

linhas_pre <- pib_pre %>%
  inner_join(vinc_pre, by = c("id_municipio", "ano")) %>%
  left_join(fatores_pre, by = "ano") %>%
  mutate(populacao              = pib / pib_per_capita,
         log_populacao          = log(populacao),
         dummy_pandemia         = 0,
         produtividade_pib      = pib / vinculos_totais,
         produtividade_vab      = vab_total / vinculos_totais,
         pib_real               = pib * fator_deflator,
         vab_total_real         = vab_total * fator_deflator,
         produtividade_pib_real = pib_real / vinculos_totais,
         produtividade_vab_real = vab_total_real / vinculos_totais,
         log_produtividade      = log(produtividade_pib_real)) %>%
  left_join(fixas_2013, by = "id_municipio") %>%
  select(-vinculos_haitianos)

# Colunas do painel que não existem antes de 2013 (instrumento) ficam vazias
painel_estendido_haitiano <- bind_rows(linhas_pre, painel_final_haitiano) %>%
  arrange(id_municipio, ano)


# -----------------------------------------------------------------------------
# 5. Conferências finais
# -----------------------------------------------------------------------------
print(count(painel_estendido_haitiano, ano))          # 496 em 2010–2012; 497 depois
cat("Linhas:", nrow(painel_estendido_haitiano), "(esperado 1.488 + 5.467 = 6.955)\n")
cat("log_produtividade vazia em 2010–2012:",
    sum(is.na(painel_estendido_haitiano$log_produtividade[painel_estendido_haitiano$ano < 2013])), "\n")
cat("share_hai_2013 vazia:", sum(is.na(painel_estendido_haitiano$share_hai_2013)), "\n")

# Share de haitianos antes de 2013: deve ser quase zero
painel_estendido_haitiano %>%
  group_by(ano) %>%
  summarise(share_media = mean(share_haitiano_real, na.rm = TRUE),
            municipios_com_haitianos = sum(share_haitiano_real > 0, na.rm = TRUE)) %>%
  print(n = 14)

# Produtividade mediana por ano: não deve haver salto entre 2012 e 2013
painel_estendido_haitiano %>%
  group_by(ano) %>%
  summarise(prod_mediana = median(produtividade_pib_real, na.rm = TRUE)) %>%
  print(n = 14)


# -----------------------------------------------------------------------------
# 6. Salva como arquivo NOVO (o painel_final_haitiano.rds não é alterado)
# -----------------------------------------------------------------------------
saveRDS(painel_estendido_haitiano, file.path(pasta, "painel_estendido_haitiano.rds"))
