# =============================================================================
# Apêndice B, Tabela 1: municípios do RS com maior número de vínculos formais
#                       de imigrantes, 2025
# Apêndice B, Tabela 3: municípios do RS com maior número de vínculos formais
#                       de haitianos, 2025
# Formato: Município | Vínculos | % do total estadual | % acumulado
#
# Entrada: dados/dados_rais_rs_2010_2024.rds (base sem o filtro de remuneração,
#          como na versão do artigo; ver codigos/funcoes/padrao_graficos.R)
# Saída:   resultados/apB_tabelas1e3_maiores_municipios_2025.docx
# =============================================================================

source("00_configuracao.R")
source("codigos/funcoes/padrao_graficos.R")

library(dplyr)
library(flextable)

dados_rais_rs <- carregar_rais(filtrada = FALSE)

ano_ref <- 2025

# % do total estadual = vínculos do município / vínculos do grupo no RS
tabela_maiores_numeros <- function(dados, ano, grupo) {
  base <- dados %>%
    filter(ano == !!ano) %>%
    mutate(
      no_grupo = switch(.env$grupo,   # .env: o argumento da função, não a coluna "grupo"
                        haitiano  = nacionalidade == "Haitiano",
                        imigrante = grupo_nacionalidade == "Imigrante"
      ) %in% TRUE
    ) %>%
    filter(no_grupo)

  total_rs <- sum(base$qtd_vinculos, na.rm = TRUE)

  base %>%
    group_by(municipio_nome) %>%
    summarise(vinculos = sum(qtd_vinculos, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(vinculos)) %>%
    slice_head(n = 10) %>%
    mutate(
      pct_estadual = vinculos / total_rs * 100,
      pct_acumulado = cumsum(pct_estadual)
    )
}

formatar <- function(tab, titulo) {
  tab %>%
    flextable() %>%
    set_header_labels(
      municipio_nome = "Município",
      vinculos = "Vínculos",
      pct_estadual = "% do total estadual",
      pct_acumulado = "% acumulado"
    ) %>%
    colformat_double(j = "vinculos", digits = 0, big.mark = ".", decimal.mark = ",") %>%
    colformat_double(j = c("pct_estadual", "pct_acumulado"), digits = 2, big.mark = ".", decimal.mark = ",", suffix = "%") %>%
    set_caption(titulo) %>%
    add_footer_lines("Fonte: elaboração própria a partir dos dados da RAIS (Base dos Dados, 2026).") %>%
    width(j = "municipio_nome", width = 2) %>%
    width(j = c("vinculos", "pct_estadual", "pct_acumulado"), width = 1.3) %>%
    theme_vanilla() %>%
    bold(part = "header") %>%
    align(align = "center", part = "body") %>%
    align(align = "center", part = "header") %>%
    align(j = "municipio_nome", align = "left", part = "all") %>%
    fontsize(size = 10, part = "all")
}

# ---- Total de imigrantes (Tabela 1) ------------------------------------------
tab_imi <- tabela_maiores_numeros(dados_rais_rs, ano_ref, "imigrante")
tab_imi %>% print(n = Inf)   # Caxias do Sul 8.572 (15,99%); acumulado 52,79%
ft_imi <- formatar(
  tab_imi,
  paste0("Tabela 1 - Municípios do Rio Grande do Sul com maior número de vínculos formais imigrantes ativos do total estadual, ", ano_ref)
)

# ---- Haitianos (Tabela 3) ------------------------------------------------------
tab_hai <- tabela_maiores_numeros(dados_rais_rs, ano_ref, "haitiano")
tab_hai %>% print(n = Inf)   # Porto Alegre 987 (13,23%); acumulado 65,24%
ft_hai <- formatar(
  tab_hai,
  paste0("Tabela 3 - Municípios do Rio Grande do Sul com maior número de vínculos formais haitianos ativos do total estadual, ", ano_ref)
)

save_as_docx(ft_imi, ft_hai,
             path = file.path(saida, paste0("apB_tabelas1e3_maiores_municipios_", ano_ref, ".docx")))
