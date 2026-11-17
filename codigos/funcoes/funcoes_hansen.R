# =============================================================================
# funcoes_hansen.R: teste J de Hansen robusto ao agrupamento por município
# (versão do teste de Sargan que não supõe homocedasticidade)
#
# Usada nos scripts 4_resultados/02 (Tabela 2) e 5_validacao/01 (Tabela 3).
# Requer: dplyr, fixest. Dados com log_produtividade, share_haitiano_real,
# share_hai_2013, ano, id_municipio e os controles de 2013.
# Passos: (1) retira efeitos fixos e controles de y, x e instrumentos;
#         (2) MQ2E; (3) matriz de pesos com os resíduos agrupados por município;
#         (4) GMM em dois estágios; (5) estatística J ~ qui-quadrado(nº instr. − 1).
# =============================================================================

fe_hansen <- "id_municipio + ano[share_ind_2013, share_agro_2013, share_frig_2013, log_pop_2013]"

hansen_j <- function(dados, anos_instr) {
  # Instrumentos explícitos: exposição de 2013 × binária de cada ano
  for (k in anos_instr) dados[[paste0("z", k)]] <- dados$share_hai_2013 * (dados$ano == k)
  zs <- paste0("z", anos_instr)

  # (1) Retira efeitos fixos e controles (mesma estrutura do modelo principal)
  vars <- c("log_produtividade", "share_haitiano_real", zs)
  res  <- sapply(vars, function(v)
    resid(feols(as.formula(paste(v, "~ 1 |", fe_hansen)), data = dados, notes = FALSE)))
  y <- res[, 1]; x <- res[, 2, drop = FALSE]; Z <- res[, -(1:2), drop = FALSE]
  g <- dados$id_municipio

  # (2) MQ2E
  PZ   <- Z %*% solve(crossprod(Z)) %*% t(Z)
  b2sls <- solve(t(x) %*% PZ %*% x, t(x) %*% PZ %*% y)
  u    <- as.vector(y - x %*% b2sls)

  # (3) Matriz S robusta ao agrupamento por município
  S_de <- function(u) {
    Zu <- rowsum(Z * u, g)          # soma de Z_i * u_i dentro de cada município
    crossprod(Zu)
  }
  W <- solve(S_de(u))

  # (4) GMM em dois estágios
  bgmm <- solve(t(x) %*% Z %*% W %*% t(Z) %*% x, t(x) %*% Z %*% W %*% t(Z) %*% y)
  u2   <- as.vector(y - x %*% bgmm)

  # (5) Estatística J
  gbar <- t(Z) %*% u2
  J    <- as.numeric(t(gbar) %*% solve(S_de(u2)) %*% gbar)
  gl   <- length(zs) - 1
  data.frame(instrumentos = length(zs), J = round(J, 3), gl = gl,
             p_valor = round(pchisq(J, gl, lower.tail = FALSE), 3),
             coef_2sls = round(as.numeric(b2sls), 3), coef_gmm = round(as.numeric(bgmm), 3))
}
