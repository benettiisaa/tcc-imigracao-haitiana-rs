# =============================================================================
# 2.5 Instrumento shift-share
#   Parte "share": participação de haitianos no emprego do município em 2013
#                  (share_hai_2013); na especificação principal, interagida com
#                  binárias de ano: i(ano, share_hai_2013, ref = 2013)
#   Forma clássica (Tabela 5 do Apêndice B): share de 2013 x crescimento do
#                  estoque nacional de haitianos em relação a 2013 (fator_shift)
#
# Entradas: dados/painel_final_haitiano.rds   (script 2_painel/04)
#           dados/dados_shift_nacional.rds    (script 1_dados/02)
# Saída:    dados/painel_final_haitiano.rds (atualizado com o instrumento)
# =============================================================================

source("00_configuracao.R")

library(dplyr)
library(bit64)

exigir("painel_final_haitiano.rds", "dados_shift_nacional.rds")

painel_final_haitiano <- readRDS(file.path(pasta, "painel_final_haitiano.rds"))
dados_shift_nacional  <- readRDS(file.path(pasta, "dados_shift_nacional.rds"))

# Peça 1: participação de haitianos no emprego do município em 2013
base13 <- painel_final_haitiano %>%
  filter(ano == 2013) %>%
  transmute(id_municipio, share_hai_2013 = share_haitiano_real)

# Peça 2: fator de crescimento nacional (estoque do ano / estoque de 2013; vale 1 em 2013)
fator <- dados_shift_nacional %>%
  filter(nacionalidade == "Haitiano") %>%
  mutate(ano = as.numeric(ano), qtd_vinculos = as.numeric(qtd_vinculos)) %>%
  group_by(ano) %>%
  summarise(hai_br = sum(qtd_vinculos), .groups = "drop") %>%
  mutate(fator_shift = hai_br / hai_br[ano == 2013])

fator

# Peça 3: junta ao painel (remove versões anteriores, para não duplicar colunas)
painel_final_haitiano <- painel_final_haitiano %>%
  select(-any_of(c("share_hai_2013", "fator_shift", "Z_share_haitiano"))) %>%
  left_join(base13, by = "id_municipio") %>%
  left_join(fator %>% select(ano, fator_shift), by = "ano") %>%
  mutate(Z_share_haitiano = share_hai_2013 * fator_shift)

# Conferências (se algum valor não bater, parar e investigar)
stopifnot(
  nrow(painel_final_haitiano) == 5467,                          # 497 municípios x 11 anos
  sum(is.na(painel_final_haitiano$Z_share_haitiano)) == 0,
  all(c("share_ind_2013", "share_agro_2013", "share_frig_2013",
        "log_pop_2013") %in% names(painel_final_haitiano))
)

# Municípios com haitianos em 2013: 62
sum(base13$share_hai_2013 > 0)

# Correlação entre a participação observada e o instrumento clássico (cerca de 0,67)
cor(painel_final_haitiano$share_haitiano_real, painel_final_haitiano$Z_share_haitiano)

saveRDS(painel_final_haitiano, file.path(pasta, "painel_final_haitiano.rds"))
