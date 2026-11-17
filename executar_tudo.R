

# =============================================================================
# executar_tudo.R: roda as etapas do trabalho, na ordem, a partir de dados/
#
# As extrações do BigQuery (1_dados/01 e 1_dados/02) não rodam aqui, pois
# exigem um projeto no Google Cloud; rode-as à parte apenas se quiser refazer
# a extração. A preparação da RAIS (1_dados/03) só roda se o arquivo bruto
# estiver em dados/; caso contrário, são usadas as bases já preparadas.
#
# Cada script roda numa sessão nova do R, para não acumular objetos na memória.
# Tempo aproximado: 10 minutos (cada leitura da RAIS leva cerca de 20 segundos).
# =============================================================================

source("00_configuracao.R")

# Etapas a rodar. Sem os arquivos da RAIS, é possível reproduzir apenas os
# modelos (seções 4.2 e 4.3) a partir dos painéis já prontos em dados/:
# etapas <- c("4_resultados", "5_validacao")
etapas <- c("1_dados", "2_painel", "3_descritivos", "4_resultados", "5_validacao")

scripts <- unlist(lapply(etapas, function(e)
  sort(list.files(file.path("codigos", e), pattern = "\\.R$", full.names = TRUE))))

# Extrações do BigQuery: rodar manualmente
scripts <- setdiff(scripts, c("codigos/1_dados/01_extracao_rais_rs.R",
                              "codigos/1_dados/02_extracao_rais_brasil_shift.R"))

# Preparação da RAIS: só se houver o arquivo bruto
if (!file.exists(file.path(pasta, "dados_rais_rs_2010_2024_bruto.rds"))) {
  scripts <- setdiff(scripts, "codigos/1_dados/03_preparacao_rais.R")
  message("Arquivo bruto da RAIS não encontrado: usando as bases já preparadas em dados/.")
}

rscript <- file.path(R.home("bin"), "Rscript")
for (s in scripts) {
  message("\n==> ", s)
  status <- system2(rscript, shQuote(s))
  if (status != 0) stop("Erro ao rodar ", s, call. = FALSE)
}

message("\nPronto. Tabelas e gráficos salvos em resultados/.")
