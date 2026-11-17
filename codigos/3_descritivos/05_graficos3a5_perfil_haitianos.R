# =============================================================================
# Gráfico 3: escolaridade de haitianos e brasileiros no emprego formal, 2013 e 2025
# Gráfico 4: faixa etária de haitianos e brasileiros no emprego formal, 2013 e 2025
# Gráfico 5: remuneração média mensal real de haitianos e brasileiros, total e
#            nos frigoríficos, 2013–2023 (R$ de dezembro de 2023)
#
# Entradas: dados/dados_rais_rs_2010_2024_filtrado.rds
#           dados/painel_final_haitiano.rds (deflator do IPCA; script 2_painel/05)
# Saídas:   resultados/grafico3_escolaridade_2013_2025.png
#           resultados/grafico4_faixa_etaria_2013_2025.png
#           resultados/grafico5_remuneracao_2013_2023.png
# =============================================================================

source("00_configuracao.R")
source("codigos/funcoes/padrao_graficos.R")

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

exigir("painel_final_haitiano.rds")

dados_rais_rs <- carregar_rais(filtrada = TRUE)
painel <- readRDS(file.path(pasta, "painel_final_haitiano.rds"))

anos_perfil <- c(2013, 2025)


# -----------------------------------------------------------------------------
# Gráfico 3. Escolaridade
# -----------------------------------------------------------------------------
escolaridade <- dados_rais_rs %>%
  filter(ano %in% anos_perfil, grupo %in% c("Haitianos", "Brasileiros")) %>%
  mutate(escolaridade = case_when(
    grau_instrucao_apos_2005 %in% c("ANALFABETO", "ATE 5.A INC", "5.A CO FUND", "6. A 9. FUND") ~ "Até fundamental incompleto",
    grau_instrucao_apos_2005 %in% c("FUND COMPL", "MEDIO INCOMP")                              ~ "Fundamental completo ou médio incompleto",
    grau_instrucao_apos_2005 %in% c("MEDIO COMPL", "SUP. INCOMP")                              ~ "Médio completo ou superior incompleto",
    grau_instrucao_apos_2005 %in% c("SUP. COMP", "MESTRADO", "DOUTORADO")                      ~ "Superior completo",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(escolaridade)) %>%
  group_by(ano, grupo, escolaridade) %>%
  summarise(vinculos = sum(qtd_vinculos), .groups = "drop") %>%
  group_by(ano, grupo) %>% mutate(pct = vinculos / sum(vinculos) * 100) %>% ungroup() %>%
  mutate(escolaridade = factor(escolaridade, levels = c("Até fundamental incompleto", "Fundamental completo ou médio incompleto",
                                                        "Médio completo ou superior incompleto", "Superior completo")))
print(escolaridade)

g3 <- ggplot(escolaridade, aes(x = escolaridade, y = pct, fill = grupo)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = paste0(num_br(pct, 1), "%")), position = position_dodge(width = 0.75), vjust = -0.4, size = 2.8) +
  facet_wrap(~ ano) +
  scale_fill_manual(values = c("Brasileiros" = cor_bra, "Haitianos" = cor_hai), name = NULL) +
  scale_x_discrete(labels = label_wrap(14)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Escolaridade", y = "% do grupo", caption = fonte) +
  tema + theme(panel.grid.major.x = element_blank(), strip.text = element_text(size = 12, face = "bold"))
g3
salvar(g3, "grafico3_escolaridade_2013_2025.png", w = 11, h = 6)


# -----------------------------------------------------------------------------
# Gráfico 4. Faixa etária
# -----------------------------------------------------------------------------
niveis_faixa <- c("15 a 17 anos", "18 a 24 anos", "25 a 29 anos", "30 a 39 anos",
                  "40 a 49 anos", "50 a 64 anos", "65 anos ou mais")

faixa_etaria <- dados_rais_rs %>%
  filter(ano %in% anos_perfil, grupo %in% c("Haitianos", "Brasileiros"), faixa_etaria %in% niveis_faixa) %>%
  group_by(ano, grupo, faixa_etaria) %>%
  summarise(vinculos = sum(qtd_vinculos), .groups = "drop") %>%
  group_by(ano, grupo) %>% mutate(pct = vinculos / sum(vinculos) * 100) %>% ungroup() %>%
  mutate(faixa_etaria = factor(faixa_etaria, levels = niveis_faixa))
print(faixa_etaria)

g4 <- ggplot(faixa_etaria, aes(x = faixa_etaria, y = pct, fill = grupo)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = paste0(num_br(pct, 1), "%")), position = position_dodge(width = 0.75), vjust = -0.4, size = 2.8) +
  facet_wrap(~ ano) +
  scale_fill_manual(values = c("Brasileiros" = cor_bra, "Haitianos" = cor_hai), name = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Faixa etária", y = "% do grupo", caption = fonte) +
  tema + theme(panel.grid.major.x = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1),
               strip.text = element_text(size = 12, face = "bold"))
g4
salvar(g4, "grafico4_faixa_etaria_2013_2025.png", w = 11, h = 6)


# -----------------------------------------------------------------------------
# Gráfico 5. Remuneração média mensal, 2013–2023 (R$ de dez./2023)
#   soma_remuneracao_media / qtd_vinculos = remuneração média por vínculo;
#   exclui remunerações nulas; deflator: o mesmo do PIB no painel (IPCA)
# -----------------------------------------------------------------------------
deflator <- painel %>% distinct(ano, fator_deflator)

salarios <- dados_rais_rs %>%
  filter(ano >= 2013, ano <= 2023, grupo %in% c("Haitianos", "Brasileiros"), soma_remuneracao_media > 0) %>%
  left_join(deflator, by = "ano") %>%
  group_by(ano, grupo) %>%
  summarise(`Todos os setores` = sum(soma_remuneracao_media * fator_deflator) / sum(qtd_vinculos),
            `Frigoríficos`     = sum((soma_remuneracao_media * fator_deflator)[frigorifico]) / sum(qtd_vinculos[frigorifico]),
            .groups = "drop") %>%
  pivot_longer(c(`Todos os setores`, `Frigoríficos`), names_to = "setor", values_to = "remuneracao") %>%
  mutate(serie = paste0(grupo, " – ", tolower(setor)))
print(salarios %>% pivot_wider(id_cols = ano, names_from = serie, values_from = remuneracao), n = Inf)

g5 <- ggplot(salarios, aes(x = ano, y = remuneracao, color = grupo, linetype = setor)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Haitianos" = cor_hai, "Brasileiros" = "#9A9CA0"), name = NULL) +
  scale_linetype_manual(values = c("Todos os setores" = "solid", "Frigoríficos" = "dashed"), name = NULL) +
  scale_x_continuous(breaks = 2013:2023) +
  scale_y_continuous(labels = function(x) paste0("R$ ", num_br(x)), limits = c(1500, NA)) +
  labs(x = "Ano", y = "Remuneração média mensal (R$ de dez./2023)",
       caption = paste0(fonte, "\nValores deflacionados pelo IPCA. Os valores de 2022 são atipicamente elevados em todos os grupos.")) +
  guides(color = guide_legend(order = 1), linetype = guide_legend(order = 2)) +
  tema
g5
salvar(g5, "grafico5_remuneracao_2013_2023.png")
