# =============================================================================
# 5.3 Tabela 4: estudo de evento (2010–2020, referência 2012) e testes de
#     pré-tendência
#   (1) presença de haitianos (primeira etapa ano a ano)
#   (2) produtividade, especificação principal
#   (3) produtividade, sem Poço das Antas
#   (4) produtividade, ponderada pelo emprego formal de 2013
#   (5) produtividade, ponderada, com efeitos fixos de microrregião x ano
#
# Entradas: dados/painel_estendido_haitiano.rds   (script 2_painel/06)
#           dados/microrregioes_rs.rds            (script 1_dados/04)
# Saídas:   resultados/tabela4_estudo_de_evento.docx
#           resultados/figura_auxiliar_es_*.png (gráficos de apoio dos cinco
#             modelos, não incluídos no artigo)
# =============================================================================

source("00_configuracao.R")
source("codigos/funcoes/formato_abnt.R")   # vírgula decimal e ponto de milhar

library(dplyr)
library(fixest)
library(ggplot2)
library(modelsummary)
library(flextable)
library(officer)

exigir("painel_estendido_haitiano.rds", "microrregioes_rs.rds")

painel_est <- readRDS(file.path(pasta, "painel_estendido_haitiano.rds")) %>%
  group_by(id_municipio) %>%
  mutate(peso_2013 = vinculos_totais[ano == 2013][1]) %>%
  ungroup()

# Microrregiões (arquivo criado pelo script 1_dados/04)
micro <- readRDS(file.path(pasta, "microrregioes_rs.rds"))
painel_est <- painel_est %>% mutate(id_municipio = as.character(id_municipio)) %>%
  select(-any_of(c("microrregiao", "regiao_imediata"))) %>% left_join(micro, by = "id_municipio")

ctrl_2013 <- "id_municipio + ano[share_ind_2013, share_agro_2013, share_frig_2013, log_pop_2013]"
f_prod <- as.formula(paste("log_produtividade ~ i(ano, share_hai_2013, ref = 2012) |", ctrl_2013))
f_pres <- as.formula(paste("share_haitiano_real ~ i(ano, share_hai_2013, ref = 2012) |", ctrl_2013))


# -----------------------------------------------------------------------------
# 1. Modelos
# -----------------------------------------------------------------------------
es_presenca  <- feols(f_pres, data = painel_est, cluster = ~id_municipio, subset = ~ ano <= 2020)
es_principal <- feols(f_prod, data = painel_est, cluster = ~id_municipio, subset = ~ ano <= 2020)
es_sem_poco  <- feols(f_prod, data = painel_est, cluster = ~id_municipio,
                      subset = ~ ano <= 2020 & municipio_nome != "Poço das Antas")
es_pond      <- feols(f_prod, data = painel_est, cluster = ~id_municipio,
                      weights = ~peso_2013, subset = ~ ano <= 2020)
f_prod_micro <- as.formula(paste("log_produtividade ~ i(ano, share_hai_2013, ref = 2012) |",
                                 ctrl_2013, "+ microrregiao^ano"))
es_pond_micro <- feols(f_prod_micro, data = painel_est, cluster = ~id_municipio,
                       weights = ~peso_2013, subset = ~ ano <= 2020)


# -----------------------------------------------------------------------------
# 2. Gráficos de apoio (não incluídos no artigo)
# -----------------------------------------------------------------------------
coefs_es <- function(modelo, ref = 2012) {
  ct <- as.data.frame(coeftable(modelo))
  ct$ano <- as.numeric(sub("ano::([0-9]{4}):.*", "\\1", rownames(ct)))
  ct <- ct[!is.na(ct$ano), c("ano", "Estimate", "Std. Error")]
  names(ct) <- c("ano", "coef", "ep")
  bind_rows(ct, data.frame(ano = ref, coef = 0, ep = 0)) %>%
    arrange(ano) %>%
    mutate(lo = coef - 1.96 * ep, hi = coef + 1.96 * ep)
}

grafico_es <- function(modelo, titulo, eixo_y, ref = 2012) {
  ggplot(coefs_es(modelo, ref), aes(x = ano, y = coef)) +
    geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.4) +
    geom_vline(xintercept = ref + 0.5, colour = "grey55", linewidth = 0.4, linetype = "dashed") +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.18, linewidth = 0.6, colour = "#2B5C8A") +
    geom_point(size = 2.6, colour = "#2B5C8A") +
    scale_x_continuous(breaks = 2010:2020) +
    labs(title = titulo, x = NULL, y = eixo_y,
         caption = paste0("Pontos: coeficientes; barras: intervalo de confiança de 95% ",
                          "(erros agrupados por município). Referência: ", ref, ".")) +
    theme_minimal(base_size = 11, base_family = "serif") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(face = "bold", size = 12),
          plot.caption = element_text(size = 8.5, colour = "grey35", hjust = 0))
}

eixo_prod <- "Variação em ln(produtividade), em %,\npor 1 p.p. de haitianos em 2013"

salvar <- function(g, nome) ggsave(file.path(saida, nome), g, width = 7.5, height = 4.2, dpi = 300, bg = "white")

salvar(grafico_es(es_presenca, "Participação de haitianos no emprego formal e exposição em 2013",
                  "Variação na participação de haitianos\npor unidade de exposição em 2013"),
       "figura_auxiliar_es_presenca.png")
salvar(grafico_es(es_principal, "Produtividade do trabalho — especificação principal", eixo_prod),
       "figura_auxiliar_es_produtividade.png")
salvar(grafico_es(es_sem_poco, "Produtividade do trabalho — sem Poço das Antas", eixo_prod),
       "figura_auxiliar_es_sem_poco.png")
salvar(grafico_es(es_pond, "Produtividade do trabalho — ponderado pelo emprego de 2013", eixo_prod),
       "figura_auxiliar_es_ponderado.png")
salvar(grafico_es(es_pond_micro, "Produtividade do trabalho — ponderado, com microrregião × ano", eixo_prod),
       "figura_auxiliar_es_ponderado_micro.png")


# -----------------------------------------------------------------------------
# 3. Tabela (Word)
# -----------------------------------------------------------------------------
p_pre <- function(m) sprintf("%.3f", wald(m, keep = "ano::2010|ano::2011", print = FALSE)$p)

anos <- c(2010, 2011, 2013:2020)
nomes_es <- setNames(paste0("2013 × ", anos), paste0("ano::", anos, ":share_hai_2013"))

estrelas <- c("." = 0.1, "*" = 0.05, "**" = 0.01, "***" = 0.001)
gof <- data.frame(raw = "nobs", clean = "Observações", fmt = 0)

# Pré-tendência (p-valor): 0,388 | 0,046 | 0,597 | 0,879 | 0,743
t4_linhas <- data.frame(
  termo = c("Variável dependente", "Amostra", "Ponderação",
            "Pré-tendência: teste conjunto 2010–2011 (p-valor)"),
  a = c("Participação de haitianos", "Completa",           "Não",                 p_pre(es_presenca)),
  b = c("ln(produtividade)",         "Completa",           "Não",                 p_pre(es_principal)),
  c = c("ln(produtividade)",         "Sem Poço das Antas", "Não",                 p_pre(es_sem_poco)),
  d = c("ln(produtividade)",         "Completa",           "Emprego formal 2013", p_pre(es_pond)),
  e = c("ln(produtividade)",         "Completa",           "Emprego formal 2013", p_pre(es_pond_micro))
)
print(t4_linhas)

t4 <- tabela_abnt(list("(1) Presença" = es_presenca, "(2) Principal" = es_principal,
                       "(3) Sem Poço das Antas" = es_sem_poco, "(4) Ponderado" = es_pond,
                       "(5) Microrregião" = es_pond_micro),
                  titulo = "Tabela 4 - Estudo de evento: exposição à imigração haitiana em 2013 (2010–2020, referência: 2012)",
                  nota = c(
                    "Fonte: elaboração própria a partir dos dados da RAIS (Base dos Dados, 2026), do IBGE (2026a, 2026b, 2026c) e do DEE (Rio Grande do Sul, 2026).",
                    paste("Nota: Erros-padrão agrupados por município entre parênteses.",
                          "Significância: . p<0,1; * p<0,05; ** p<0,01; *** p<0,001.",
                          "Todas as colunas incluem efeitos fixos de município e de ano e os controles de 2013 × ano; a coluna (5) inclui também efeitos fixos de microrregião × ano.",
                          "O teste de pré-tendência verifica se os coeficientes de 2010 e 2011 são conjuntamente nulos.",
                          "Coluna (5): efeitos fixos de microrregião geográfica (IBGE) × ano; o Rio Grande do Sul possui 35 microrregiões.")),
                  stars = estrelas, coef_map = nomes_es, gof_map = gof, add_rows = t4_linhas,
                  primeira_coluna = "Exposição")

read_docx() |>
  body_add_flextable(t4) |>
  body_end_section_landscape() |>
  print(target = file.path(saida, "tabela4_estudo_de_evento.docx"))
