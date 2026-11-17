# =============================================================================
# 5.1 Tabela 3: justificativa do corte temporal em 2020
#   Painel A: teste conjunto da primeira etapa ano a ano (2010–2023, referência
#             2012): a exposição de 2013 prevê a presença de haitianos em
#             2014–2020 e em 2021–2023?
#   Painel B: MQ2E com o período completo (2013–2023) x até 2020
#
# Entradas: dados/painel_estendido_haitiano.rds   (script 2_painel/06)
#           dados/painel_final_haitiano.rds       (script 2_painel/05)
# Saídas:   resultados/tabela3_corte_temporal_2020.docx
#           resultados/figura_auxiliar_primeira_etapa_2010_2023.png
#             (gráfico de apoio, não incluído no artigo)
# =============================================================================

source("00_configuracao.R")
source("codigos/funcoes/funcoes_hansen.R")   # teste J de Hansen

library(dplyr)
library(fixest)
library(ggplot2)
library(flextable)
library(officer)

exigir("painel_estendido_haitiano.rds", "painel_final_haitiano.rds")

painel_est <- readRDS(file.path(pasta, "painel_estendido_haitiano.rds"))   # 2010–2023
painel     <- readRDS(file.path(pasta, "painel_final_haitiano.rds"))       # 2013–2023

ctrl_2013 <- "id_municipio + ano[share_ind_2013, share_agro_2013, share_frig_2013, log_pop_2013]"


# -----------------------------------------------------------------------------
# 1. Primeira etapa ano a ano, período completo (2010–2023), referência 2012
#    Mesma especificação da primeira etapa do modelo principal, mas sem cortar em 2020
# -----------------------------------------------------------------------------
fs_completo <- feols(as.formula(paste(
                 "share_haitiano_real ~ i(ano, share_hai_2013, ref = 2012) |", ctrl_2013)),
               data = painel_est, cluster = ~id_municipio)

etable(fs_completo)

# Teste conjunto: a exposição de 2013 prevê a presença em 2021–2023?
# (p-valor alto = deixa de prever)
teste_pos2020 <- wald(fs_completo, keep = "ano::2021|ano::2022|ano::2023", print = FALSE)
teste_ate2020 <- wald(fs_completo, keep = "ano::2014|ano::2015|ano::2016|ano::2017|ano::2018|ano::2019|ano::2020",
                      print = FALSE)
teste_ate2020   # F = 296,43
teste_pos2020   # F = 28,73


# -----------------------------------------------------------------------------
# 2. Gráfico de apoio (não incluído no artigo)
# -----------------------------------------------------------------------------
ct <- as.data.frame(coeftable(fs_completo))
ct$ano <- as.numeric(sub("ano::([0-9]{4}):.*", "\\1", rownames(ct)))
ct <- ct[!is.na(ct$ano), c("ano", "Estimate", "Std. Error")]
names(ct) <- c("ano", "coef", "ep")
dados_g <- bind_rows(ct, data.frame(ano = 2012, coef = 0, ep = 0)) %>%
  arrange(ano) %>%
  mutate(lo = coef - 1.96 * ep, hi = coef + 1.96 * ep,
         periodo = ifelse(ano <= 2020, "Incluído", "Excluído"))

g <- ggplot(dados_g, aes(x = ano, y = coef, colour = periodo)) +
  annotate("rect", xmin = 2020.5, xmax = 2023.5, ymin = -Inf, ymax = Inf,
           fill = "grey92", colour = NA) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.4) +
  geom_vline(xintercept = 2020.5, colour = "grey40", linewidth = 0.5, linetype = "dashed") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.18, linewidth = 0.6) +
  geom_point(size = 2.6) +
  annotate("text", x = 2020.6, y = Inf, label = "fora da especificação principal",
           hjust = 0, vjust = 1.6, size = 3.1, colour = "grey30") +
  scale_colour_manual(values = c("Incluído" = "#2B5C8A", "Excluído" = "grey55"), guide = "none") +
  scale_x_continuous(breaks = 2010:2023) +
  labs(title = "Poder de previsão da exposição de 2013 sobre a presença de haitianos, 2010–2023",
       x = NULL,
       y = "Variação na participação de haitianos\npor unidade de exposição em 2013",
       caption = paste0("Pontos: coeficientes; barras: intervalo de confiança de 95% ",
                        "(erros agrupados por município). Referência: 2012.\n",
                        "Inclui efeitos fixos de município e de ano e os controles de 2013 × ano.")) +
  theme_minimal(base_size = 11, base_family = "serif") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.caption = element_text(size = 8.5, colour = "grey35", hjust = 0))

ggsave(file.path(saida, "figura_auxiliar_primeira_etapa_2010_2023.png"), g, width = 8, height = 4.4, dpi = 300, bg = "white")
print(g)


# -----------------------------------------------------------------------------
# 3. MQ2E: período completo (2013–2023) × especificação principal (2013–2020)
#    Os dois com os controles de 2013 × ano
# -----------------------------------------------------------------------------
f_iv <- as.formula(paste("log_produtividade ~ 1 |", ctrl_2013,
                         "| share_haitiano_real ~ i(ano, share_hai_2013, ref = 2013)"))

iv_completo <- feols(f_iv, data = painel, cluster = ~id_municipio)
iv_ate2020  <- feols(f_iv, data = painel, cluster = ~id_municipio, subset = ~ ano <= 2020)

etable(iv_completo, iv_ate2020)        # −9,05 (3,12) e 0,31 (2,36)
fitstat(iv_completo, ~ ivwald1 + sargan) # F = 785,5; Sargan p = 0,018
fitstat(iv_ate2020,  ~ ivwald1 + sargan) # F = 509,8; Sargan p = 0,275


# J de Hansen robusto ao agrupamento (versão robusta do Sargan): p = 0,885 e 0,847
hj_completo <- hansen_j(painel, 2014:2023)
hj_ate2020  <- hansen_j(filter(painel, ano <= 2020), 2014:2020)
print(rbind(hj_completo, hj_ate2020))

# -----------------------------------------------------------------------------
# 4. Tabela (Word)
# -----------------------------------------------------------------------------
fmt  <- function(x, d = 3) formatC(x, format = "f", digits = d, decimal.mark = ",")
f_iv1 <- function(m) fmt(fitstat(m, ~ivwald1)[[1]]$stat, 1)
p_sg  <- function(m) fmt(fitstat(m, ~sargan)[[1]]$p)
coef_ep <- function(m) {
  ct <- coeftable(m)["fit_share_haitiano_real", ]
  c(fmt(ct["Estimate"], 2), paste0("(", fmt(ct["Std. Error"], 2), ")"))
}

painel_a <- data.frame(
  `Teste` = c("Coeficientes de 2014 a 2020 conjuntamente nulos",
              "Coeficientes de 2021 a 2023 conjuntamente nulos"),
  `Estatística F` = c(fmt(teste_ate2020$stat, 2), fmt(teste_pos2020$stat, 2)),
  check.names = FALSE)

ce_c <- coef_ep(iv_completo); ce_r <- coef_ep(iv_ate2020)
painel_b <- data.frame(
  ` ` = c("Participação de haitianos", "", "F da 1ª etapa", "Sargan (p-valor)",
          "J de Hansen (p-valor)", "Instrumentos", "Observações"),
  `Período completo (2013–2023)` = c(ce_c, f_iv1(iv_completo), p_sg(iv_completo),
                                     fmt(hj_completo$p_valor),
                                     "10", format(nobs(iv_completo), big.mark = ".")),
  `Especificação principal (2013–2020)` = c(ce_r, f_iv1(iv_ate2020), p_sg(iv_ate2020),
                                            fmt(hj_ate2020$p_valor),
                                            "7", format(nobs(iv_ate2020), big.mark = ".")),
  check.names = FALSE)

estilo <- function(ft) ft |> font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 10, part = "all") |> autofit()

ft_a <- estilo(flextable(painel_a))
ft_b <- estilo(flextable(painel_b))

fonte <- "Fonte: elaboração própria a partir dos dados da RAIS (Base dos Dados, 2026)."
nota <- paste("Notas: Painel A: estatística F do teste de Wald, robusta ao agrupamento por município, sobre a",
              "primeira etapa ano a ano (estimada para 2010–2023, referência 2012), com efeitos fixos de",
              "município e de ano e controles de 2013 × ano. Painel B: MQ2E com os mesmos efeitos fixos e",
              "controles, para o período completo (2013–2023) e para a especificação principal (2013–2020);",
              "instrumentos: participação de haitianos em 2013 × variáveis binárias de ano; erros-padrão",
              "agrupados por município entre parênteses. O teste de Sargan supõe erros homocedásticos; o J de",
              "Hansen é a sua versão robusta ao agrupamento por município.")

read_docx() |>
  body_add_par("Tabela 3 - Justificativa do corte temporal em 2020") |>
  body_add_par("Painel A - A exposição de 2013 prevê a presença de haitianos?") |>
  body_add_flextable(ft_a) |>
  body_add_par("") |>
  body_add_par("Painel B - Estimativas por MQ2E com e sem o corte") |>
  body_add_flextable(ft_b) |>
  body_add_par(fonte) |>
  body_add_par(nota) |>
  print(target = file.path(saida, "tabela3_corte_temporal_2020.docx"))
