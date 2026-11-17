# =============================================================================
# Apêndice B, Tabela 2: as 10 atividades (classe CNAE 2.0) com mais vínculos
#                       formais de imigrantes no RS, 2010–2025 (soma dos anos)
#
# Entrada: dados/dados_rais_rs_2010_2024.rds (base sem o filtro de remuneração,
#          como na versão do artigo; ver codigos/funcoes/padrao_graficos.R)
# Saída:   resultados/apB_tabela2_atividades_cnae.docx
#
# Nomes oficiais das classes: Estrutura Detalhada da CNAE 2.0 (CONCLA/IBGE).
# =============================================================================

source("00_configuracao.R")
source("codigos/funcoes/padrao_graficos.R")

library(dplyr)
library(flextable)

dados_rais_rs <- carregar_rais(filtrada = FALSE)

# Tabela de correspondência código -> nome oficial da classe
descricao_classe <- tibble::tribble(
  ~cnae_classe, ~descricao_classe,
  "10121", "Abate de suínos, aves e outros pequenos animais",
  "47113", "Comércio varejista de mercadorias em geral, com predominância de produtos alimentícios - hipermercados e supermercados",
  "56112", "Restaurantes e outros estabelecimentos de serviços de alimentação e bebidas",
  "41204", "Construção de edifícios",
  "86101", "Atividades de atendimento hospitalar",
  "49302", "Transporte rodoviário de carga",
  "22293", "Fabricação de artefatos de material plástico não especificados anteriormente",
  "55108", "Hotéis e similares",
  "28330", "Fabricação de máquinas e equipamentos para a agricultura e pecuária, exceto para irrigação",
  "47814", "Comércio varejista de artigos do vestuário e acessórios"
)

tabela_top10 <- dados_rais_rs %>%
  filter(grupo_nacionalidade == "Imigrante", !is.na(cnae_2_subclasse)) %>%
  mutate(cnae_classe = substr(cnae_2_subclasse, 1, 5)) %>%
  group_by(cnae_classe) %>%
  summarise(vinculos = sum(qtd_vinculos)) %>%
  arrange(desc(vinculos)) %>%
  mutate(
    pct = vinculos / sum(vinculos) * 100,
    pct_acumulado = cumsum(pct)
  ) %>%
  slice_head(n = 10) %>%
  left_join(descricao_classe, by = "cnae_classe") %>%
  mutate(posicao = row_number()) %>%
  select(posicao, cnae_classe, descricao_classe, vinculos, pct, pct_acumulado)

print(tabela_top10)   # 1º: 10121, com 57.982 vínculos (19,4% do total)

tabela_formatada <- tabela_top10 %>%
  flextable() %>%
  set_header_labels(
    posicao = "Posição",
    cnae_classe = "Código",
    descricao_classe = "Atividade (classe CNAE)",
    vinculos = "Número de vínculos",
    pct = "% do total",
    pct_acumulado = "% acumulado"
  ) %>%
  colformat_double(j = "vinculos", digits = 0, big.mark = ".", decimal.mark = ",") %>%
  colformat_double(j = c("pct", "pct_acumulado"), digits = 1, big.mark = ".", decimal.mark = ",", suffix = "%") %>%
  set_caption("Tabela 2 - 10 atividades (classe CNAE 2.0) por vínculos formais imigrantes no RS (2010-2025)") %>%
  add_footer_lines("Fonte: elaboração própria a partir dos dados da RAIS (Base dos Dados, 2026).") %>%
  width(j = "descricao_classe", width = 2.6) %>%
  width(j = c("posicao", "cnae_classe", "vinculos", "pct", "pct_acumulado"), width = 0.9) %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  align(align = "center", part = "body") %>%
  align(align = "center", part = "header") %>%
  align(j = "descricao_classe", align = "left", part = "all") %>%
  fontsize(size = 9, part = "all")

save_as_docx(tabela_formatada, path = file.path(saida, "apB_tabela2_atividades_cnae.docx"))
