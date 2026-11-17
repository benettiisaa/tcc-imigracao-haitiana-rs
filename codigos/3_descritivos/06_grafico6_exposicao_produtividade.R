# =============================================================================
# Gráfico 6: participação de haitianos no emprego formal em 2013 e variação da
#            produtividade do trabalho entre 2013 e 2020, municípios do RS
#
# Entrada: dados/painel_final_haitiano.rds   (script 2_painel/05)
# Saída:   resultados/grafico6_exposicao_produtividade.png
#
# Inclui os 62 municípios com ao menos um vínculo formal de haitiano em 2013.
# Variação da produtividade: diferença dos logaritmos entre 2013 e 2020, x 100.
# Reta: ajuste linear por MQO, com intervalo de confiança de 95%.
# =============================================================================

source("00_configuracao.R")
source("codigos/funcoes/padrao_graficos.R")

library(dplyr)
library(ggplot2)

exigir("painel_final_haitiano.rds")
painel <- readRDS(file.path(pasta, "painel_final_haitiano.rds"))

disp <- painel %>%
  filter(ano %in% c(2013, 2020)) %>%
  group_by(id_municipio) %>%
  summarise(exposicao = share_hai_2013[ano == 2013] * 100,
            var_prod  = (log_produtividade[ano == 2020] - log_produtividade[ano == 2013]) * 100,
            nome      = municipio_nome[1],
            .groups = "drop") %>%
  filter(exposicao > 0)        # só municípios com haitianos em 2013

nrow(disp)                                       # 62
disp %>% filter(exposicao > 1) %>% arrange(desc(exposicao))   # municípios rotulados

g6 <- ggplot(disp, aes(exposicao, var_prod)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_point(colour = cor_hai, size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey30", linewidth = 0.8) +
  geom_text(data = filter(disp, exposicao > 1), aes(label = nome),
            size = 3, vjust = -0.8) +
  scale_x_continuous(labels = num_br, limits = c(0, 5.4)) +
  scale_y_continuous(labels = num_br) +
  labs(x = "Participação de haitianos no emprego formal em 2013 (%)",
       y = "Variação da produtividade, 2013–2020 (%, log)",
       caption = "Fonte: elaboração própria com base em RAIS (Base dos Dados) e DEE/SPGG.") +
  tema

g6
salvar(g6, "grafico6_exposicao_produtividade.png", w = 8, h = 5.5)
