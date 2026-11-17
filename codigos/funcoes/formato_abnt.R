# =============================================================================
# formato_abnt.R: tabelas de regressão com vírgula decimal e ponto de milhar
# Uso: source("codigos/funcoes/formato_abnt.R") e depois
#      ft <- tabela_abnt(lista_de_modelos, titulo = "...", nota = "...",
#                        add_rows = linhas_extras, stars = ..., coef_map = ..., gof_map = ...)
#      (nota pode ser um vetor: cada elemento vira uma linha do rodapé;
#       primeira_coluna: cabeçalho opcional da coluna dos nomes das variáveis)
# =============================================================================

library(modelsummary)
library(flextable)

tabela_abnt <- function(modelos, titulo, nota, add_rows = NULL, primeira_coluna = " ", ...) {
  df <- modelsummary(modelos, output = "data.frame", ...)

  # Erro-padrão na linha de baixo do coeficiente, sem repetir o nome
  if (all(c("part", "statistic") %in% names(df))) {
    df$term[df$part == "estimates" & df$statistic != "estimate"] <- ""
  }
  df <- df[, setdiff(names(df), c("part", "statistic")), drop = FALSE]
  df[] <- lapply(df, as.character)

  # Linhas extras (efeitos fixos, F, Sargan...) acrescentadas no fim
  if (!is.null(add_rows)) {
    extra <- as.data.frame(lapply(add_rows, as.character), stringsAsFactors = FALSE)
    names(extra) <- names(df)
    df <- rbind(df, extra)
  }

  # Vírgula decimal (troca o ponto só quando está entre dois dígitos)
  df[] <- lapply(df, function(x) gsub("(\\d)\\.(\\d)", "\\1,\\2", x))

  # Ponto de milhar no número de observações (ex.: 3976 -> 3.976)
  obs <- df$term == "Observações"
  if (any(obs)) {
    for (j in 2:ncol(df)) {
      x <- df[obs, j]
      df[obs, j] <- ifelse(grepl("^[0-9]+$", x), formatC(as.numeric(x), format = "d", big.mark = "."), x)
    }
  }

  names(df)[1] <- primeira_coluna
  flextable(df) |>
    add_header_lines(titulo) |>
    add_footer_lines(nota) |>
    font(fontname = "Times New Roman", part = "all") |>
    fontsize(size = 10, part = "all") |>
    align(j = 2:ncol(df), align = "center", part = "all") |>
    autofit()
}
