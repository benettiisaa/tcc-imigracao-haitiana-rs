# =============================================================================
# 2.4 Controles fixos no ano-base (2013), que entram no modelo interagidos com ano
#     1. participação da indústria no emprego formal
#     2. participação da agropecuária
#     3. participação dos frigoríficos (CNAE 1011, 1012 e 1013)
#     4. log da população
#   (as mesmas medidas em 2012, o quociente locacional e o indicador de polo
#    frigorífico também são calculados, mas não entram na especificação principal)
#
# Entradas: dados/dados_rais_rs_2010_2024_filtrado.rds   (script 1_dados/03)
#           dados/painel_final_haitiano.rds              (script 2_painel/03)
# Saída:    dados/painel_final_haitiano.rds (atualizado com os controles)
# =============================================================================

source("00_configuracao.R")

library(dplyr)
library(bit64)

exigir("dados_rais_rs_2010_2024_filtrado.rds", "painel_final_haitiano.rds")

d <- readRDS(file.path(pasta, "dados_rais_rs_2010_2024_filtrado.rds"))
painel_final_haitiano <- readRDS(file.path(pasta, "painel_final_haitiano.rds"))


# -----------------------------------------------------------------------------
# 1. Participações setoriais no emprego formal do ano-base
# -----------------------------------------------------------------------------
calc_controles <- function(dados, ano_base) {
  dados %>%
    filter(ano == ano_base) %>%
    mutate(qtd  = as.numeric(qtd_vinculos),
           frig = substr(cnae_2_subclasse, 1, 4) %in% c("1011", "1012", "1013")) %>%
    group_by(id_municipio) %>%
    summarise(
      vinc_total    = sum(qtd, na.rm = TRUE),
      vinc_ind      = sum(qtd[setor_macro %in% "Indústria"],    na.rm = TRUE),
      vinc_agro     = sum(qtd[setor_macro %in% "Agropecuária"], na.rm = TRUE),
      vinc_frig     = sum(qtd[frig],                            na.rm = TRUE),
      vinc_setor_na = sum(qtd[is.na(setor_macro)],              na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(share_ind  = vinc_ind  / vinc_total,
           share_agro = vinc_agro / vinc_total,
           share_frig = vinc_frig / vinc_total,
           tem_frig   = as.integer(vinc_frig > 0))
}

ctrl13 <- calc_controles(d, 2013) %>% rename_with(~ paste0(.x, "_2013"), -id_municipio)
ctrl12 <- calc_controles(d, 2012) %>% rename_with(~ paste0(.x, "_2012"), -id_municipio)

nrow(ctrl13); nrow(ctrl12)   # 497 e 496 (Pinto Bandeira não existia em 2012)
summary(ctrl13 %>% select(share_ind_2013, share_agro_2013, share_frig_2013))


# -----------------------------------------------------------------------------
# 2. Quociente locacional dos frigoríficos e indicador de polo frigorífico
#    (QL >= 3: município ao menos três vezes mais especializado que a média do RS;
#     em 2013, os frigoríficos respondiam por 1,76% do emprego formal gaúcho)
# -----------------------------------------------------------------------------
part_rs_2013 <- sum(ctrl13$vinc_frig_2013) / sum(ctrl13$vinc_total_2013)
part_rs_2012 <- sum(ctrl12$vinc_frig_2012) / sum(ctrl12$vinc_total_2012)

ctrl13 <- ctrl13 %>%
  mutate(ql_frig_2013   = share_frig_2013 / part_rs_2013,
         polo_frig_2013 = as.integer(ql_frig_2013 >= 3))

ctrl12 <- ctrl12 %>%
  mutate(ql_frig_2012   = share_frig_2012 / part_rs_2012,
         polo_frig_2012 = as.integer(ql_frig_2012 >= 3))

# Conferência: o emprego de 2013 calculado aqui deve ser igual ao do painel
painel_final_haitiano %>%
  filter(ano == 2013) %>%
  select(id_municipio, vinculos_totais) %>%
  left_join(ctrl13 %>% select(id_municipio, vinc_total_2013), by = "id_municipio") %>%
  mutate(diferenca = vinculos_totais - vinc_total_2013) %>%
  summarise(municipios_diferentes = sum(diferenca != 0, na.rm = TRUE),
            maior_diferenca = max(abs(diferenca), na.rm = TRUE))


# -----------------------------------------------------------------------------
# 3. População de 2013 (log), fixada por município, e junção ao painel
# -----------------------------------------------------------------------------
pop13 <- painel_final_haitiano %>%
  filter(ano == 2013) %>%
  select(id_municipio, log_pop_2013 = log_populacao)

# Remove versões anteriores dos controles (caso o script seja rodado de novo)
colunas_controles <- c("share_ind_2013", "share_agro_2013", "share_frig_2013", "ql_frig_2013",
                       "polo_frig_2013", "share_ind_2012", "share_agro_2012", "share_frig_2012",
                       "ql_frig_2012", "polo_frig_2012", "log_pop_2013")

painel_final_haitiano <- painel_final_haitiano %>%
  select(-any_of(colunas_controles)) %>%
  left_join(ctrl13 %>% select(id_municipio, share_ind_2013, share_agro_2013,
                              share_frig_2013, ql_frig_2013, polo_frig_2013),
            by = "id_municipio") %>%
  left_join(ctrl12 %>% select(id_municipio, share_ind_2012, share_agro_2012,
                              share_frig_2012, ql_frig_2012, polo_frig_2012),
            by = "id_municipio") %>%
  left_join(pop13, by = "id_municipio")

nrow(painel_final_haitiano)                          # deve continuar 5.467
sum(is.na(painel_final_haitiano$share_frig_2013))    # deve ser 0
sum(is.na(painel_final_haitiano$log_pop_2013))       # deve ser 0
sum(is.na(painel_final_haitiano$share_frig_2012))    # 11 (Pinto Bandeira, sem dados em 2012)

saveRDS(painel_final_haitiano, file.path(pasta, "painel_final_haitiano.rds"))
