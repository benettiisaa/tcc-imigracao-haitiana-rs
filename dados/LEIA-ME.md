# Pasta `dados/`

Guarda as bases de entrada e os arquivos intermediários gerados pelos scripts.
Os nomes dos arquivos e das subpastas devem ser exatamente os listados abaixo.

## Bases de entrada

| Arquivo | Fonte | Como obter |
|---|---|---|
| `dados_rais_rs_2010_2024_bruto.rds` | RAIS, vínculos ativos em 31/12, RS, 2010–2025 (Base dos Dados) | `codigos/1_dados/01_extracao_rais_rs.R` (BigQuery) |
| `dados_shift_nacional.rds` | RAIS, estoque nacional de vínculos por nacionalidade (Base dos Dados) | `codigos/1_dados/02_extracao_rais_brasil_shift.R` (BigQuery) |
| `microrregioes_rs.rds` | API de Localidades do IBGE | `codigos/1_dados/04_microrregioes_ibge.R` |
| `PIB Municipal/19092428-pib-municipios-rs-2002-2023-serie-historica.xlsx` | PIB e VAB municipais, série 2002–2023 (DEE/SPGG) | download em <https://dee.rs.gov.br/pib-municipal> |
| `populacao_municipios_rs_2010_2025.csv` | População municipal (IBGE): Censos 2010 e 2022 e estimativas da tabela 6579 do SIDRA | montado à parte (ver abaixo) |
| `RS_Municipios_2020/` (`.shp`, `.shx`, `.dbf`, `.prj`, `.cpg`) | Malha municipal do IBGE, 2020 | download no portal de malhas territoriais do IBGE |

O arquivo de população tem as colunas `municipio_nome`, `ano`, `populacao` e
`fonte`. A coluna `fonte` indica a origem de cada valor: `censo` (2010 e 2022),
`estimativa` (tabela 6579 do SIDRA: 2011–2021 e 2024–2025) e `proxy_censo_2022`
(2023, que repete o valor do Censo de 2022). Pinto Bandeira, instalado em 2013,
não tem valor para 2010 e 2011; como o painel começa em 2013, isso não afeta as
estimações. O script que montou esse arquivo não faz parte do repositório.

## Arquivos gerados pelos scripts

| Arquivo | Gerado por |
|---|---|
| `dados_rais_rs_2010_2024.rds` | `1_dados/03_preparacao_rais.R` (RAIS com grupo de nacionalidade e macrossetor) |
| `dados_rais_rs_2010_2024_filtrado.rds` | `1_dados/03_preparacao_rais.R` (mesma base, com o filtro de remuneração) |
| `painel_produtividade.rds` | `2_painel/01_produtividade.R` |
| `share_real_haitiano.rds` | `2_painel/02_participacao_haitianos.R` |
| `painel_final_haitiano.rds` | `2_painel/03` a `05` (painel 2013–2023 usado nos modelos) |
| `painel_estendido_haitiano.rds` | `2_painel/06_extensao_2010_2012.R` (painel 2010–2023) |
| `painel_industria.rds` | `2_painel/07_painel_industria.R` |
| `modelos_secao_4_2.rds` | `4_resultados/01_modelos.R` |

## O que vai para o GitHub

As três bases da RAIS (`dados_rais_rs_2010_2024_bruto.rds`,
`dados_rais_rs_2010_2024.rds` e `dados_rais_rs_2010_2024_filtrado.rds`) têm
mais de 100 MB, limite do GitHub, e estão listadas no `.gitignore`. Para
recriá-las, rode `1_dados/01` (requer um projeto no Google Cloud) e `1_dados/03`.

Os demais arquivos são pequenos e podem ser publicados. Com eles, é possível
reproduzir as estimações (seções 4.2 e 4.3) sem acesso à RAIS: basta rodar os
scripts das pastas `4_resultados` e `5_validacao`.
