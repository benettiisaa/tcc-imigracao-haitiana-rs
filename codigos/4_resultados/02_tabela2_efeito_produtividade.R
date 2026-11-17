# =============================================================================
# 4.2 Tabela 2: efeito da participação de haitianos sobre ln(produtividade do
#     trabalho), 2013–2020, com os testes de Hausman e J de Hansen
#
# Entradas: dados/modelos_secao_4_2.rds       (script 4_resultados/01)
#           dados/painel_final_haitiano.rds   (script 2_painel/05)
# Saída:    resultados/tabela2_efeito_produtividade.docx
# =============================================================================

source("00_configuracao.R")
source("codigos/funcoes/formato_abnt.R")     # vírgula decimal e ponto de milhar
source("codigos/funcoes/funcoes_hansen.R")   # teste J de Hansen

library(dplyr)
library(fixest)
library(plm)
library(modelsummary)
library(flextable)
library(officer)

exigir("modelos_secao_4_2.rds", "painel_final_haitiano.rds")
modelos <- readRDS(file.path(pasta, "modelos_secao_4_2.rds"))
painel  <- readRDS(file.path(pasta, "painel_final_haitiano.rds"))


# -----------------------------------------------------------------------------
# 1. Hausman: efeitos fixos x efeitos aleatórios (especificação do modelo 3)
# -----------------------------------------------------------------------------
d20 <- filter(painel, ano <= 2020)
for (k in 2014:2020) d20[[paste0("d", k)]] <- as.integer(d20$ano == k)   # binárias de ano (2013 = referência)
pd <- pdata.frame(d20, index = c("id_municipio", "ano"))
f_h <- log_produtividade ~ share_haitiano_real + log_populacao +
  d2014 + d2015 + d2016 + d2017 + d2018 + d2019 + d2020
ph  <- phtest(plm(f_h, data = pd, model = "within"), plm(f_h, data = pd, model = "random"))
ph       # esperado: qui-quadrado = 30,02; p = 0,000435
p_haus <- ifelse(ph$p.value < 0.001, "< 0,001", sprintf("%.3f", ph$p.value))


# -----------------------------------------------------------------------------
# 2. J de Hansen (versão do Sargan robusta ao agrupamento por município)
# -----------------------------------------------------------------------------
hj <- hansen_j(d20, 2014:2020)
hj       # coef_2sls deve ser 0,313 (igual ao modelo 5); p-valor 0,847
p_hj <- sprintf("%.3f", hj$p_valor)


# -----------------------------------------------------------------------------
# 3. Tabela (Word)
# -----------------------------------------------------------------------------
f_iv <- function(m) sprintf("%.1f", fitstat(m, ~ivwald1)[[1]]$stat)
p_sg <- function(m) sprintf("%.3f", fitstat(m, ~sargan)[[1]]$p)

estrelas <- c("." = 0.1, "*" = 0.05, "**" = 0.01, "***" = 0.001)
nomes <- c("share_haitiano_real"     = "Participação de haitianos",
           "fit_share_haitiano_real" = "Participação de haitianos",
           "log_populacao"           = "Log da população",
           "dummy_pandemia"          = "Pandemia (2020)")
gof  <- data.frame(raw = c("nobs", "r.squared"), clean = c("Observações", "R²"), fmt = c(0, 3))

linhas <- data.frame(
  termo = c("Efeitos fixos de município", "Efeitos fixos de ano", "Controles de 2013 × ano",
            "Estimador", "Hausman, EF × EA (p-valor)", "F da 1ª etapa", "Sargan (p-valor)",
            "J de Hansen (p-valor)"),
  m1 = c("Não", "Não", "Não", "MQO",  "",     "", "", ""),
  m2 = c("Sim", "Não", "Não", "MQO",  "",     "", "", ""),
  m3 = c("Sim", "Sim", "Não", "MQO",  p_haus, "", "", ""),
  m4 = c("Sim", "Sim", "Sim", "MQO",  "",     "", "", ""),
  m5 = c("Sim", "Sim", "Sim", "MQ2E", "",     f_iv(modelos$m4e), p_sg(modelos$m4e), p_hj))

t2 <- tabela_abnt(
  list("(1)" = modelos$m1_20, "(2)" = modelos$m2_20, "(3)" = modelos$m3_20,
       "(4)" = modelos$m3e,   "(5)" = modelos$m4e),
  titulo = "Tabela 2 - Efeito da participação de haitianos sobre ln(produtividade do trabalho), 2013 - 2020",
  nota = c(
    "Fonte: elaboração própria a partir dos dados da RAIS (Base dos Dados, 2026), do IBGE (2026b, 2026c) e do DEE (Rio Grande do Sul, 2026).",
    paste("Nota: Erros-padrão agrupados por município entre parênteses.",
          "Significância: . p<0,1; * p<0,05; ** p<0,01; *** p<0,001.",
          "Instrumentos da coluna (5): participação de haitianos em 2013 × variáveis binárias de ano (2014–2020).",
          "J de Hansen: versão do teste de Sargan robusta ao agrupamento por município.",
          "Teste de Hausman (coluna 3): efeitos fixos × efeitos aleatórios; H0: efeitos municipais não correlacionados com os regressores;",
          "Controles de 2013 (colunas 4 e 5): participação da indústria, da agropecuária e dos frigoríficos (CNAE 1011 a 1013)",
          "no emprego formal e log da população, interagidos com variáveis binárias de ano.",
          "R² da coluna (5) não tem a interpretação usual.")),
  stars = estrelas, coef_map = nomes, gof_map = gof, add_rows = linhas)

read_docx() |>
  body_add_flextable(t2) |>
  print(target = file.path(saida, "tabela2_efeito_produtividade.docx"))
