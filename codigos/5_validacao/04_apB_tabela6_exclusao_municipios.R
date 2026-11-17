# =============================================================================
# 5.4 Apêndice B, Tabela 6: exclusão de um município por vez (os 10 municípios
#     com maior participação de haitianos em 2013)
#   Para cada exclusão: p-valor do teste de pré-tendência no estudo de evento
#   (coeficientes de 2010 e 2011, referência 2012) e o MQ2E da especificação
#   principal (2013–2020)
#
# Entradas: dados/painel_final_haitiano.rds       (script 2_painel/05)
#           dados/painel_estendido_haitiano.rds   (script 2_painel/06)
# Saída:    resultados/apB_tabela6_exclusao_municipios.docx
# =============================================================================

source("00_configuracao.R")

library(dplyr)
library(fixest)
library(flextable)
library(officer)

exigir("painel_final_haitiano.rds", "painel_estendido_haitiano.rds")

painel     <- readRDS(file.path(pasta, "painel_final_haitiano.rds"))       # 2013–2023
painel_est <- readRDS(file.path(pasta, "painel_estendido_haitiano.rds"))   # 2010–2023

ctrl_slopes <- "ano[share_ind_2013, share_agro_2013, share_frig_2013, log_pop_2013]"
fmt <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",")

iv_rhs <- "| share_haitiano_real ~ i(ano, share_hai_2013, ref = 2013)"
f_base <- as.formula(paste("log_produtividade ~ 1 | id_municipio +", ctrl_slopes, iv_rhs))

es_rhs <- "log_produtividade ~ i(ano, share_hai_2013, ref = 2012) |"
f_es   <- as.formula(paste(es_rhs, "id_municipio +", ctrl_slopes))


# -----------------------------------------------------------------------------
# 1. Os 10 municípios com maior participação de haitianos em 2013
# -----------------------------------------------------------------------------
top10 <- painel %>% filter(ano == 2013) %>% arrange(desc(share_hai_2013)) %>%
  slice(1:10) %>% select(municipio_nome, share_hai_2013)
print(top10)


# -----------------------------------------------------------------------------
# 2. Exclusão de um município por vez
# -----------------------------------------------------------------------------
loo <- lapply(c("Nenhum (amostra completa)", top10$municipio_nome), function(fora) {
  d_es <- if (fora == "Nenhum (amostra completa)") painel_est else filter(painel_est, municipio_nome != fora)
  d_iv <- if (fora == "Nenhum (amostra completa)") painel     else filter(painel,     municipio_nome != fora)
  es <- feols(f_es,   data = d_es, subset = ~ ano <= 2020, cluster = ~id_municipio)
  iv <- feols(f_base, data = d_iv, subset = ~ ano <= 2020, cluster = ~id_municipio)
  ct <- coeftable(iv)["fit_share_haitiano_real", ]
  tibble(`Município excluído` = fora,
         `Pré-tendência (p-valor)` = fmt(wald(es, keep = "ano::2010|ano::2011", print = FALSE)$p, 3),
         `MQ2E: coeficiente` = fmt(ct["Estimate"]),
         `MQ2E: erro-padrão` = fmt(ct["Std. Error"]),
         `F da 1ª etapa` = fmt(fitstat(iv, ~ivwald1)[[1]]$stat, 1))
})
loo <- bind_rows(loo)
print(loo, n = 20)   # só a exclusão de Poço das Antas elimina a pré-tendência (p = 0,597)


# -----------------------------------------------------------------------------
# 3. Tabela (Word)
# -----------------------------------------------------------------------------
estilo <- function(ft) ft |> font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 10, part = "all") |> autofit()

read_docx() |>
  body_add_par("Tabela 6 - Exclusão de um município por vez (10 maiores participações de haitianos em 2013)") |>
  body_add_flextable(estilo(flextable(loo))) |>
  body_add_par("Fonte: elaboração própria a partir dos dados da RAIS (Base dos Dados, 2026), do IBGE (2026b, 2026c) e do DEE (Rio Grande do Sul, 2026).") |>
  body_add_par("Nota: Pré-tendência: p-valor do teste conjunto dos coeficientes de 2010 e 2011 no estudo de evento (referência 2012). MQ2E: especificação principal, 2013–2020.") |>
  print(target = file.path(saida, "apB_tabela6_exclusao_municipios.docx"))
