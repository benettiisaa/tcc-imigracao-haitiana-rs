# =============================================================================
# 4.3 Apêndice B, Tabela 5: relevância do instrumento na primeira etapa,
#     forma clássica e forma flexível
#
# Entrada: dados/modelos_secao_4_2.rds   (script 4_resultados/01)
# Saída:   resultados/apB_tabela5_primeira_etapa.docx
#
# Forma clássica: exposição de 2013 x crescimento do estoque nacional de
# haitianos em relação a 2013, estimada em 2013–2023. Forma flexível: exposição
# de 2013 x binárias de ano, em 2013–2020.
# =============================================================================

source("00_configuracao.R")

library(fixest)
library(flextable)
library(officer)

exigir("modelos_secao_4_2.rds")
modelos <- readRDS(file.path(pasta, "modelos_secao_4_2.rds"))

fmt  <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = ".")
f_iv <- function(m) fmt(fitstat(m, ~ivwald1)[[1]]$stat)
p_iv <- function(m) {
  p <- fitstat(m, ~ivwald1)[[1]]$p
  if (p < 0.001) "< 0,001" else fmt(p, 3)
}

tab <- data.frame(
  Instrumento = c("Clássico: exposição 2013 × crescimento nacional",
                  "Clássico: exposição 2013 × crescimento nacional",
                  "Flexível: exposição 2013 × binárias de ano",
                  "Flexível: exposição 2013 × binárias de ano"),
  `Nº de instrumentos` = c("1", "1", "7", "7"),
  `Controles de 2013 × ano` = c("Não", "Sim", "Não", "Sim"),
  `Período` = c("2013–2023", "2013–2023", "2013–2020", "2013–2020"),
  `F da 1ª etapa` = c(f_iv(modelos$m4a), f_iv(modelos$m4b), f_iv(modelos$m4d), f_iv(modelos$m4e)),
  `p-valor` = c(p_iv(modelos$m4a), p_iv(modelos$m4b), p_iv(modelos$m4d), p_iv(modelos$m4e)),
  check.names = FALSE)

print(tab)   # F: 2,49 | 0,09 | 803,89 | 509,79

fonte <- "Fonte: elaboração própria a partir dos dados da RAIS (Base dos Dados, 2026), do IBGE (2026b, 2026c) e do DEE (Rio Grande do Sul, 2026)."
nota <- paste("Nota: Estatística F de Wald da primeira etapa, robusta ao agrupamento dos erros por município.",
              "Todas as especificações incluem efeitos fixos de município e de ano.",
              "A última linha corresponde à especificação principal.",
              "Valores de F inferiores a 10 sugerem instrumento fraco (Staiger; Stock, 1997).")

ft <- flextable(tab) |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 10, part = "all") |>
  bold(i = 4, part = "body") |>
  autofit()

read_docx() |>
  body_add_par("Tabela 5 - Relevância do instrumento na primeira etapa: forma clássica e forma flexível") |>
  body_add_flextable(ft) |>
  body_add_par(fonte) |>
  body_add_par(nota) |>
  print(target = file.path(saida, "apB_tabela5_primeira_etapa.docx"))
