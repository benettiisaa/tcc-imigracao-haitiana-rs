# =============================================================================
# Apêndice B, Tabela 4: setores de atuação dos trabalhadores haitianos no RS, 2025
#                       (macrossetor e seção da CNAE 2.0)
#
# Entrada: dados/dados_rais_rs_2010_2024_filtrado.rds
# Saída:   resultados/apB_tabela4_setores_haitianos_2025.docx
# =============================================================================

source("00_configuracao.R")
source("codigos/funcoes/padrao_graficos.R")

library(dplyr)
library(flextable)

dados_rais_rs <- carregar_rais(filtrada = TRUE)

tab_setor_hai <- dados_rais_rs %>%
  filter(ano == 2025, nacionalidade == "Haitiano", !is.na(cnae_secao)) %>%
  group_by(setor_macro, cnae_secao) %>%
  summarise(vinculos = sum(qtd_vinculos, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(vinculos)) %>%
  mutate(pct = vinculos / sum(vinculos) * 100,
         pct_acumulado = cumsum(pct))

tab_setor_hai %>% print(n = Inf)   # indústrias de transformação: 4.639 (62,17%)

ft_setor_hai <- tab_setor_hai %>%
  flextable() %>%
  set_header_labels(setor_macro = "Macrossetor", cnae_secao = "Seção da CNAE",
                    vinculos = "Vínculos", pct = "% do total", pct_acumulado = "% acumulado") %>%
  colformat_double(j = "vinculos", digits = 0, big.mark = ".", decimal.mark = ",") %>%
  colformat_double(j = c("pct", "pct_acumulado"), digits = 2, big.mark = ".", decimal.mark = ",", suffix = "%") %>%
  set_caption("Tabela 4 - Setores de atuação dos trabalhadores haitianos no Rio Grande do Sul, 2025") %>%
  add_footer_lines("Fonte: elaboração própria a partir dos dados da RAIS (Base dos Dados, 2026).") %>%
  theme_vanilla() %>% bold(part = "header") %>%
  align(align = "center", part = "body") %>%
  align(align = "center", part = "header") %>%
  align(j = "cnae_secao", align = "left", part = "all") %>%
  fontsize(size = 10, part = "all") %>% autofit()

save_as_docx(ft_setor_hai, path = file.path(saida, "apB_tabela4_setores_haitianos_2025.docx"))
