# =============================================================================
# Apêndice B, Gráfico 2: distribuição setorial dos vínculos de haitianos nos dez
#                        municípios com maior número de vínculos haitianos, 2025
#
# Entrada: dados/dados_rais_rs_2010_2024_filtrado.rds
# Saída:   resultados/apB_grafico2_setores_municipios.png
# =============================================================================

source("00_configuracao.R")
source("codigos/funcoes/padrao_graficos.R")

library(dplyr)
library(tidyr)
library(ggplot2)

dados_rais_rs <- carregar_rais(filtrada = TRUE)

# Dez municípios com mais vínculos de haitianos em 2025 (Tabela 3 do Apêndice B)
top10 <- c("Porto Alegre", "Caxias do Sul", "Lajeado", "Garibaldi", "Bento Gonçalves",
           "Marau", "Encantado", "Passo Fundo", "Erechim", "Gramado")

d <- dados_rais_rs %>%
  filter(ano == 2025, nacionalidade == "Haitiano",
         municipio_nome %in% top10, !is.na(setor_macro)) %>%
  group_by(municipio_nome, setor_macro) %>%
  summarise(v = sum(qtd_vinculos, na.rm = TRUE), .groups = "drop") %>%
  group_by(municipio_nome) %>% mutate(pct = v / sum(v) * 100) %>% ungroup()

# Conferência (texto da seção 4.1): Encantado 97,4% na indústria;
# Porto Alegre 29,9% na indústria e 70,1% nos serviços; Gramado 97,1% nos serviços
d %>%
  select(-v) %>%
  mutate(pct = round(pct, 1)) %>%
  pivot_wider(names_from = setor_macro, values_from = pct, values_fill = 0) %>%
  print(n = Inf)

ordem <- d %>% filter(setor_macro == "Indústria") %>% arrange(pct) %>% pull(municipio_nome)
d <- d %>% mutate(municipio_nome = factor(municipio_nome, levels = c("Gramado", ordem[ordem != "Gramado"])),
                  setor_macro = factor(setor_macro, levels = c("Agropecuária", "Serviços", "Indústria")))

g <- ggplot(d, aes(x = pct, y = municipio_nome, fill = setor_macro)) +
  geom_col(width = 0.7) +
  geom_text(data = filter(d, pct >= 8), aes(label = paste0(format(round(pct, 1), decimal.mark = ","), "%")),
            position = position_stack(vjust = 0.5), color = "white", size = 3.2) +
  scale_fill_manual(values = c("Indústria" = "#0B2E52", "Serviços" = "#8FB4DE", "Agropecuária" = "#BFBFBF"), name = NULL) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  labs(x = "% dos vínculos haitianos do município", y = NULL,
       caption = "Fonte: elaboração própria com base em RAIS (Base dos Dados). Dez municípios com maior número de vínculos haitianos, 2025.") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0, size = 8, color = "grey40"))

g
ggsave(file.path(saida, "apB_grafico2_setores_municipios.png"), g, width = 9, height = 5.5, dpi = 300, bg = "white")
