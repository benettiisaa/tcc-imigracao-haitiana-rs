# =============================================================================
# Figura 1 (a e b): participação de haitianos no emprego formal dos municípios
#                   do RS em 2013 e em 2025
# Apêndice B, Figura 1: participação total de imigrantes no emprego formal, 2025
#
# Entradas: dados/dados_rais_rs_2010_2024_filtrado.rds
#           dados/RS_Municipios_2020/RS_Municipios_2020.shp (malha municipal
#             do IBGE, 2020; ver dados/LEIA-ME.md)
# Saídas:   resultados/figura1a_mapa_haitianos_2013.png
#           resultados/figura1b_mapa_haitianos_2025.png
#           resultados/apB_figura1_mapa_imigrantes_2025.png
#           resultados/tabela_municipios_mapas.csv e .xlsx (municípios com
#             participação de 1% ou mais; base das notas das figuras)
#
# Os três mapas usam as mesmas faixas e a mesma escala de cores; 0% em cinza;
# escala sequencial de verde-amarelado a azul-escuro (ColorBrewer YlGnBu).
# =============================================================================

source("00_configuracao.R")
source("codigos/funcoes/padrao_graficos.R")

library(dplyr)
library(ggplot2)
library(sf)
library(writexl)

exigir(file.path("RS_Municipios_2020", "RS_Municipios_2020.shp"))

dados_rais_rs <- carregar_rais(filtrada = TRUE)

faixas  <- c("0%", "Até 0,1%", "0,1% a 0,5%", "0,5% a 1%", "1% a 2%", "2% a 5%", "5% a 10%", "Acima de 10%")
cores_f <- setNames(c("#E5E5E5", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#253494", "#081D58"), faixas)

participacao <- function(ano_ref, grupos) {
  dados_rais_rs %>%
    filter(ano == ano_ref) %>%
    group_by(id_municipio) %>%
    summarise(total = sum(qtd_vinculos), grupo_v = sum(qtd_vinculos[grupo %in% grupos]), .groups = "drop") %>%
    mutate(share = grupo_v / total * 100,
           faixa = cut(share, breaks = c(-Inf, 0, 0.1, 0.5, 1, 2, 5, 10, Inf), labels = faixas))
}

malha <- st_read(file.path(pasta, "RS_Municipios_2020", "RS_Municipios_2020.shp"), quiet = TRUE)

mapa_rs <- function(dados, titulo_legenda, arquivo) {
  m <- malha %>% left_join(dados, by = c("CD_MUN" = "id_municipio"))
  print(table(m$faixa, useNA = "ifany"))
  g <- ggplot(m) +
    geom_sf(aes(fill = factor(faixa, levels = faixas)), color = "white", linewidth = 0.1) +
    scale_fill_manual(values = cores_f, drop = FALSE, name = titulo_legenda) +
    theme_void(base_size = 12) +
    theme(legend.position = "right")
  print(g)
  salvar(g, arquivo, w = 8, h = 6.5)
  invisible(m)
}

hai_2013  <- mapa_rs(participacao(2013, "Haitianos"), "% de haitianos\nno emprego formal",
                     "figura1a_mapa_haitianos_2013.png")
hai_2025  <- mapa_rs(participacao(2025, "Haitianos"), "% de haitianos\nno emprego formal",
                     "figura1b_mapa_haitianos_2025.png")
imig_2025 <- mapa_rs(participacao(2025, c("Haitianos", "Outros imigrantes")),
                     "% de imigrantes\nno emprego formal", "apB_figura1_mapa_imigrantes_2025.png")

# Municípios com maior participação em cada mapa (notas das figuras):
# haitianos 2025: Harmonia (157 de 1.749), Encantado (386 de 8.433), Garibaldi (581 de 17.221)
# imigrantes 2025: Chuí (945 de 2.534), Pinto Bandeira (78 de 529), Tapejara (1.560 de 10.715)
for (m in list(hai_2013, hai_2025, imig_2025))
  print(m %>% st_drop_geometry() %>% arrange(desc(share)) %>% select(NM_MUN, grupo_v, total, share) %>% head(5))


# -----------------------------------------------------------------------------
# Tabela de apoio: municípios com participação de 1% ou mais em cada mapa
# -----------------------------------------------------------------------------
tab_mapas <- bind_rows(
  imig_2025 %>% st_drop_geometry() %>% mutate(mapa = "Imigrantes, 2025"),
  hai_2013  %>% st_drop_geometry() %>% mutate(mapa = "Haitianos, 2013"),
  hai_2025  %>% st_drop_geometry() %>% mutate(mapa = "Haitianos, 2025")) %>%
  filter(share >= 1) %>%
  group_by(mapa) %>% arrange(desc(share), .by_group = TRUE) %>% ungroup() %>%
  transmute(Mapa = mapa,
            `Município` = NM_MUN,
            `Vínculos do grupo` = grupo_v,
            `Vínculos totais` = total,
            `Participação (%)` = round(share, 2),
            Faixa = as.character(faixa))

if (interactive()) View(tab_mapas)

write.csv2(tab_mapas, file.path(saida, "tabela_municipios_mapas.csv"), row.names = FALSE)
write_xlsx(split(select(tab_mapas, -Mapa), tab_mapas$Mapa),
           file.path(saida, "tabela_municipios_mapas.xlsx"))
