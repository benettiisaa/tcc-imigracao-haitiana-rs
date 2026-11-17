# =============================================================================
# 00_configuracao.R: caminhos e funções comuns a todos os scripts
#
# Todos os scripts começam com source("00_configuracao.R"). Para isso, o
# diretório de trabalho deve ser a raiz do repositório: abra o projeto pelo
# arquivo tcc-imigracao-haitiana-rs.Rproj (ou use setwd() nessa pasta).
# =============================================================================

# Pastas do projeto
pasta <- file.path(getwd(), "dados")        # bases de entrada e arquivos intermediários (.rds)
saida <- file.path(getwd(), "resultados")   # tabelas (.docx) e gráficos (.png) do artigo

dir.create(pasta, showWarnings = FALSE)
dir.create(saida, showWarnings = FALSE)

# Pacotes usados no projeto (instalar uma única vez):
# install.packages(c("basedosdados", "bit64", "dplyr", "tidyr", "readxl", "jsonlite",
#                    "fixest", "plm", "modelsummary", "flextable", "officer",
#                    "ggplot2", "scales", "sf", "writexl", "ragg"))

# Interrompe com uma mensagem clara quando falta um arquivo de entrada em dados/
exigir <- function(...) {
  arquivos <- file.path(pasta, c(...))
  faltam <- arquivos[!file.exists(arquivos)]
  if (length(faltam) > 0)
    stop("Arquivo(s) não encontrado(s) na pasta dados/: ",
         paste(basename(faltam), collapse = ", "),
         "\nConsulte dados/LEIA-ME.md para ver qual script gera cada arquivo.", call. = FALSE)
  invisible(arquivos)
}
