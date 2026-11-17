# =============================================================================
# Tabela 1: vínculos formais de haitianos na indústria de transformação, por
#           atividade, RS, 2025
#
# Entrada: dados/dados_rais_rs_2010_2024_filtrado.rds
# Saída:   resultados/tabela1_haitianos_industria_2025.docx
#
# Frigoríficos: classes 1011, 1012 e 1013 da CNAE 2.0; demais atividades
# agregadas por divisão da CNAE (as seis maiores e "Demais atividades").
# =============================================================================

source("00_configuracao.R")
source("codigos/funcoes/padrao_graficos.R")

library(dplyr)
library(flextable)
library(officer)

rais <- carregar_rais(filtrada = TRUE)

fmt <- function(x, d) formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = ".")

ind_hai <- rais %>%
  filter(nacionalidade %in% "Haitiano", ano == 2025,
         grepl("TRANSFORMA", toupper(cnae_secao))) %>%
  mutate(atividade = ifelse(substr(cnae_2_subclasse, 1, 4) %in% c("1011", "1012", "1013"),
                            "Frigoríficos (CNAE 1011 a 1013)",
                            as.character(cnae_divisao))) %>%
  group_by(atividade) %>%
  summarise(vinculos = sum(qtd_vinculos), .groups = "drop") %>%
  arrange(desc(vinculos))

print(ind_hai, n = 20)
cat("Total:", sum(ind_hai$vinculos), "\n")   # 4.639

# Seis maiores + "Demais"
tab <- ind_hai %>%
  mutate(atividade = ifelse(row_number() <= 6, atividade,
                            "Demais atividades da indústria de transformação")) %>%
  group_by(atividade) %>%
  summarise(vinculos = sum(vinculos), .groups = "drop") %>%
  arrange(atividade == "Demais atividades da indústria de transformação", desc(vinculos)) %>%
  mutate(pct = vinculos / sum(vinculos) * 100,
         acum = cumsum(pct))

tab_word <- tab %>%
  transmute(`Atividade` = atividade,
            `Vínculos` = fmt(vinculos, 0),
            `%` = fmt(pct, 2),
            `% acumulado` = fmt(acum, 2))

ft <- flextable(tab_word) |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 10, part = "all") |>
  align(j = 2:4, align = "center", part = "all") |>
  set_table_properties(width = 1, layout = "autofit")

read_docx() |>
  body_add_par("Tabela 1 - Vínculos formais de haitianos na indústria de transformação, por atividade, Rio Grande do Sul, 2025") |>
  body_add_flextable(ft) |>
  body_add_par("Fonte: elaboração própria a partir dos dados da RAIS (Base dos Dados, 2026).") |>
  body_add_par("Nota: Frigoríficos correspondem às classes 1011 (abate de reses), 1012 (abate de suínos, aves e outros pequenos animais) e 1013 (fabricação de produtos de carne) da CNAE 2.0; demais atividades agregadas por divisão da CNAE.") |>
  print(target = file.path(saida, "tabela1_haitianos_industria_2025.docx"))
