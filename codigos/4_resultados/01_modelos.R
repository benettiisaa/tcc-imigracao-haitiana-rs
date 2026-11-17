# =============================================================================
# 4.1 Estimação dos modelos da seção 4.2
#   Tabela 2: modelos (1) a (5), 2013–2020
#   Tabela 5 do Apêndice B: instrumento na forma clássica e na forma flexível
#
# Entrada: dados/painel_final_haitiano.rds   (script 2_painel/05)
# Saída:   dados/modelos_secao_4_2.rds        (lido pelos scripts 4_resultados/02 e 03)
#
# Especificação principal (modelo 5): MQ2E, 2013–2020, efeitos fixos de
# município e de ano, controles de 2013 x ano, erros agrupados por município;
# instrumentos: participação de haitianos em 2013 x binárias de ano (2014–2020).
# =============================================================================

source("00_configuracao.R")

library(dplyr)
library(bit64)
library(fixest)

exigir("painel_final_haitiano.rds")
painel_final_haitiano <- readRDS(file.path(pasta, "painel_final_haitiano.rds"))

# Controles de 2013 x ano: sintaxe ano[...] do fixest (cria o efeito fixo de ano
# e uma inclinação por ano para cada controle)
ctrl_2013 <- "id_municipio + ano[share_ind_2013, share_agro_2013, share_frig_2013, log_pop_2013]"


# -----------------------------------------------------------------------------
# 1. Tabela 2: modelos (1) a (5), 2013–2020
#    Em 2013–2020, a variável de pandemia vale 1 apenas em 2020.
# -----------------------------------------------------------------------------

# (1) MQO agrupado
m1_20 <- feols(log_produtividade ~ share_haitiano_real + log_populacao + dummy_pandemia,
               data = painel_final_haitiano, subset = ~ ano <= 2020, cluster = ~id_municipio)

# (2) MQO com efeitos fixos de município
m2_20 <- feols(log_produtividade ~ share_haitiano_real + log_populacao + dummy_pandemia |
                 id_municipio,
               data = painel_final_haitiano, subset = ~ ano <= 2020, cluster = ~id_municipio)

# (3) MQO com efeitos fixos de município e de ano
m3_20 <- feols(log_produtividade ~ share_haitiano_real + log_populacao | id_municipio + ano,
               data = painel_final_haitiano, subset = ~ ano <= 2020, cluster = ~id_municipio)

# (4) MQO com a mesma especificação do modelo 5 (controles de 2013 x ano)
m3e <- feols(as.formula(paste("log_produtividade ~ share_haitiano_real |", ctrl_2013)),
             data = painel_final_haitiano, cluster = ~id_municipio,
             subset = ~ ano <= 2020)

# (5) MQ2E  >>> ESPECIFICAÇÃO PRINCIPAL <<<
m4e <- feols(as.formula(paste("log_produtividade ~ 1 |", ctrl_2013,
                              "| share_haitiano_real ~ i(ano, share_hai_2013, ref = 2013)")),
             data = painel_final_haitiano, cluster = ~id_municipio,
             subset = ~ ano <= 2020)

etable(m1_20, m2_20, m3_20, m3e, m4e)
fitstat(m4e, ~ ivwald1 + sargan)
nobs(m1_20)   # 3.976 = 497 municípios x 8 anos

# Resultados do artigo: (1) −13,077 (1,754); (2) −0,986 (0,801); (3) −1,115 (0,801);
#                       (4) −1,900 (1,012); (5) 0,313 (2,355); F = 509,8; Sargan p = 0,275

# Intervalo de confiança de 95% do modelo 5, em variação percentual da
# produtividade por ponto percentual de haitianos (texto: cerca de −4,2% a 5,1%)
(exp(confint(m4e)["fit_share_haitiano_real", ] / 100) - 1) * 100


# -----------------------------------------------------------------------------
# 2. Tabela 5 do Apêndice B: relevância do instrumento na primeira etapa
# -----------------------------------------------------------------------------

# Forma clássica (exposição de 2013 x crescimento nacional), 2013–2023:
# instrumento fraco (F = 2,49 sem controles; 0,09 com controles)
m4a <- feols(log_produtividade ~ 1 | id_municipio + ano |
               share_haitiano_real ~ Z_share_haitiano,
             data = painel_final_haitiano, cluster = ~id_municipio)

m4b <- feols(as.formula(paste("log_produtividade ~ 1 |", ctrl_2013,
                              "| share_haitiano_real ~ Z_share_haitiano")),
             data = painel_final_haitiano, cluster = ~id_municipio)

# Forma flexível (exposição de 2013 x binárias de ano), 2013–2020, sem controles
m4d <- feols(log_produtividade ~ 1 | id_municipio + ano |
               share_haitiano_real ~ i(ano, share_hai_2013, ref = 2013),
             data = painel_final_haitiano, cluster = ~id_municipio,
             subset = ~ ano <= 2020)

etable(m4a, m4b, m4d, m4e, stage = 1)
for (m in list(m4a, m4b, m4d, m4e)) print(fitstat(m, ~ ivwald1))


# -----------------------------------------------------------------------------
# 3. Salva os modelos (para montar as tabelas sem reestimar)
# -----------------------------------------------------------------------------
saveRDS(list(m1_20 = m1_20, m2_20 = m2_20, m3_20 = m3_20, m3e = m3e, m4e = m4e,
             m4a = m4a, m4b = m4b, m4d = m4d),
        file.path(pasta, "modelos_secao_4_2.rds"))
