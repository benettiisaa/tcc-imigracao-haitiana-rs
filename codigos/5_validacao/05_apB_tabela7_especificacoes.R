# =============================================================================
# 5.5 Apêndice B, Tabela 7: especificações alternativas do modelo principal (MQ2E)
#   Reúne as variações da especificação principal numa tabela só e acrescenta,
#   na última linha, a produtividade da indústria.
#
# Entradas: dados/painel_final_haitiano.rds       (script 2_painel/05)
#           dados/painel_estendido_haitiano.rds   (script 2_painel/06)
#           dados/painel_industria.rds            (script 2_painel/07)
#           dados/microrregioes_rs.rds            (script 1_dados/04)
# Saída:    resultados/apB_tabela7_especificacoes.docx
# =============================================================================

source("00_configuracao.R")

library(dplyr)
library(fixest)
library(flextable)
library(officer)

exigir("painel_final_haitiano.rds", "painel_estendido_haitiano.rds", "painel_industria.rds",
       "microrregioes_rs.rds")

painel     <- readRDS(file.path(pasta, "painel_final_haitiano.rds"))       # 2013–2023
painel_est <- readRDS(file.path(pasta, "painel_estendido_haitiano.rds"))   # 2010–2023
micro      <- readRDS(file.path(pasta, "microrregioes_rs.rds"))

painel <- painel %>%
  mutate(id_municipio = as.character(id_municipio)) %>%
  select(-any_of(c("microrregiao", "regiao_imediata"))) %>%
  left_join(micro, by = "id_municipio") %>%
  group_by(id_municipio) %>% mutate(peso_2013 = vinculos_totais[ano == 2013][1]) %>% ungroup()

# Exposição medida em 2012 (para a linha do ano-base 2012)
base12 <- painel_est %>% filter(ano == 2012) %>%
  transmute(id_municipio = as.character(id_municipio), share_hai_2012 = share_haitiano_real)
painel <- painel %>% select(-any_of("share_hai_2012")) %>% left_join(base12, by = "id_municipio")

ctrl <- "ano[share_ind_2013, share_agro_2013, share_frig_2013, log_pop_2013]"
iv   <- "| share_haitiano_real ~ i(ano, share_hai_2013, ref = 2013)"
f    <- function(y, fe = "id_municipio", instr = iv)
  as.formula(paste(y, "~ 1 |", fe, "+", ctrl, instr))

d20 <- filter(painel, ano <= 2020)

m <- list(
  principal = feols(f("log_produtividade"), data = d20, cluster = ~id_municipio),
  vab       = feols(f("log(produtividade_vab_real)"), data = d20, cluster = ~id_municipio),
  micro     = feols(f("log_produtividade", "id_municipio + microrregiao^ano"), data = d20, cluster = ~id_municipio),
  cl_micro  = feols(f("log_produtividade"), data = d20, cluster = ~microrregiao),
  ambos     = feols(f("log_produtividade", "id_municipio + microrregiao^ano"), data = d20, cluster = ~microrregiao),
  base12    = feols(f("log_produtividade", instr = "| share_haitiano_real ~ i(ano, share_hai_2012, ref = 2013)"),
                    data = d20, cluster = ~id_municipio),
  ate2019   = feols(f("log_produtividade"), data = filter(d20, ano <= 2019), cluster = ~id_municipio),
  sem_poco  = feols(f("log_produtividade"), data = filter(d20, municipio_nome != "Poço das Antas"), cluster = ~id_municipio),
  ponderado = feols(f("log_produtividade"), data = d20, cluster = ~id_municipio, weights = ~peso_2013)
)

rotulos <- c(principal = "Principal",
             vab       = "VAB real no numerador da produtividade",
             micro     = "Efeitos fixos de microrregião × ano",
             cl_micro  = "Erros agrupados por microrregião",
             ambos     = "Efeitos fixos de microrregião × ano e erros agrupados por microrregião",
             base12    = "Ano-base 2012 (exposição)",
             ate2019   = "Período 2013–2019 (sem 2020)",
             sem_poco  = "Sem Poço das Antas",
             ponderado = "Ponderado pelo emprego formal de 2013")

fmt <- function(x, d) formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = ".")

linha <- function(nome) {
  mod <- m[[nome]]
  ct  <- coeftable(mod)["fit_share_haitiano_real", ]
  sg  <- if (nome == "ponderado") "—" else fmt(fitstat(mod, ~sargan)[[1]]$p, 3)
  data.frame(`Especificação` = rotulos[[nome]],
             `Coeficiente`   = fmt(ct["Estimate"], 2),
             `Erro-padrão`   = fmt(ct["Std. Error"], 2),
             `F da 1ª etapa` = fmt(fitstat(mod, ~ivwald1)[[1]]$stat, 1),
             `Sargan (p-valor)` = sg,
             `Observações`   = fmt(nobs(mod), 0),
             check.names = FALSE)
}
tab <- bind_rows(lapply(names(m), linha))
print(tab)

# Conferência: principal 0,31 | VAB 0,46 | micro −1,10 | base 2012 −1,94 (F 8,8)
#              2013–2019 0,36 | sem Poço 3,27 | ponderado −2,32


# -----------------------------------------------------------------------------
# Produtividade da indústria: VAB industrial real / vínculos industriais,
# municípios com ao menos 30 vínculos industriais em 2013 (392 municípios)
# -----------------------------------------------------------------------------
pb <- readRDS(file.path(pasta, "painel_industria.rds"))

m_ind <- feols(log_prod_ind ~ 1 |
                 id_municipio + ano[share_ind_2013, share_agro_2013, share_frig_2013, log_pop_2013] |
                 share_haitiano_real ~ i(ano, share_hai_2013, ref = 2013),
               data = filter(pb, vinc_ind_2013 >= 30), cluster = ~id_municipio)

summary(m_ind)
fitstat(m_ind, ~ ivwald1 + sargan)
cat("Observações:", nobs(m_ind), " | Municípios:", n_distinct(filter(pb, vinc_ind_2013 >= 30)$id_municipio), "\n")
# Conferência: 3,44 (EP 4,88), F = 528,4, Sargan p = 0,938, 3.135 observações, 392 municípios

ct_ind <- coeftable(m_ind)["fit_share_haitiano_real", ]
linha_ind <- data.frame(`Especificação`    = "Produtividade da indústria",
                        `Coeficiente`      = fmt(ct_ind["Estimate"], 2),
                        `Erro-padrão`      = fmt(ct_ind["Std. Error"], 2),
                        `F da 1ª etapa`    = fmt(fitstat(m_ind, ~ivwald1)[[1]]$stat, 1),
                        `Sargan (p-valor)` = fmt(fitstat(m_ind, ~sargan)[[1]]$p, 3),
                        `Observações`      = fmt(nobs(m_ind), 0),
                        check.names = FALSE)
tab <- bind_rows(tab, linha_ind)
print(tab)


# -----------------------------------------------------------------------------
# Conferência do texto (seção 4.3.3): vínculos de haitianos no RS e municípios
# com haitianos em 2012 (418 em 28 municípios) e em 2013 (1.545 em 62)
# -----------------------------------------------------------------------------
painel_est %>%
  filter(ano %in% c(2012, 2013)) %>%
  group_by(ano) %>%
  summarise(vinculos_haitianos = round(sum(share_haitiano_real * vinculos_totais)),
            municipios = sum(share_haitiano_real > 0))

fonte <- "Fonte: elaboração própria a partir dos dados da RAIS (Base dos Dados, 2026), do IBGE (2026a, 2026b, 2026c) e do DEE (Rio Grande do Sul, 2026)."
nota <- paste("Nota: Todas as especificações são estimadas por MQ2E, com efeitos fixos de município e de ano,",
              "controles de 2013 × ano e erros agrupados por município, exceto quando indicado.",
              "Período 2013–2020, exceto quando indicado. Microrregiões geográficas do IBGE.",
              "Com o agrupamento por microrregião, a estatística F é calculada com poucos grupos e deve ser lida",
              "apenas como indicação de que o instrumento permanece forte.",
              "Ano-base 2012: exposição medida em 2012; controles de 2013.",
              "Sargan não reportado na especificação ponderada.",
              "Produtividade da indústria: VAB real da indústria (seções B a F da CNAE 2.0) dividido pelos vínculos",
              "formais nessas atividades; amostra restrita aos 392 municípios com ao menos 30 vínculos industriais em 2013.")

ft <- flextable(tab) |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 10, part = "all") |>
  align(j = 2:6, align = "center", part = "all") |>
  bold(i = 1, part = "body") |>
  set_table_properties(width = 1, layout = "autofit")

read_docx() |>
  body_add_par("Tabela 7 - Especificações alternativas do modelo principal (MQ2E)") |>
  body_add_flextable(ft) |>
  body_add_par(fonte) |>
  body_add_par(nota) |>
  print(target = file.path(saida, "apB_tabela7_especificacoes.docx"))
