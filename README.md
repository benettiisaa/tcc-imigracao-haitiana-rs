# Imigração haitiana no Rio Grande do Sul: efeitos sobre a produtividade do trabalho

Códigos do Trabalho de Conclusão de Curso de **Isabelle Benetti** (Ciências
Econômicas, Faculdade de Ciências Econômicas da UFRGS, 2026), sob orientação do
Prof. Dr. Sabino da Silva Porto Júnior.

O trabalho estima o efeito da participação de haitianos no emprego formal sobre
a produtividade do trabalho nos 497 municípios do Rio Grande do Sul, entre 2013
e 2020, por Mínimos Quadrados em Dois Estágios (MQ2E), com uma versão flexível
do instrumento *shift-share*: a participação de haitianos em 2013 interagida
com variáveis binárias de ano.

## Estrutura

```
├── 00_configuracao.R          caminhos (dados/ e resultados/) e funções comuns
├── executar_tudo.R            roda todas as etapas na ordem
├── codigos/
│   ├── funcoes/               tabelas em padrão ABNT, J de Hansen e padrão dos gráficos
│   ├── 1_dados/               extração da RAIS (BigQuery), preparação e microrregiões
│   ├── 2_painel/              produtividade, participação de haitianos, controles e instrumento
│   ├── 3_descritivos/         seção 4.1 e gráficos e tabelas descritivos do Apêndice B
│   ├── 4_resultados/          seção 4.2 (Tabela 2) e Tabela 5 do Apêndice B
│   └── 5_validacao/           seção 4.3 (Tabelas 3 e 4, Gráfico 7) e Tabelas 6 e 7 do Apêndice B
├── dados/                     bases de entrada e arquivos intermediários (ver dados/LEIA-ME.md)
└── resultados/                tabelas (.docx) e gráficos (.png) gerados pelos scripts
```

Os scripts são numerados na ordem de execução. Cada um indica, no cabeçalho, o
elemento do artigo que produz, os arquivos que lê e os que grava.

## Como reproduzir

1. Instale o R (versão 4.2 ou superior) e os pacotes:
   ```r
   install.packages(c("basedosdados", "bit64", "dplyr", "tidyr", "readxl", "jsonlite",
                      "fixest", "plm", "modelsummary", "flextable", "officer",
                      "ggplot2", "scales", "sf", "writexl", "ragg"))
   ```
2. Abra o projeto pelo arquivo `tcc-imigracao-haitiana-rs.Rproj`. Todos os
   scripts supõem que o diretório de trabalho é a raiz do repositório.
3. Coloque as bases de entrada na pasta `dados/`, conforme `dados/LEIA-ME.md`.
4. Rode `executar_tudo.R` ou os scripts, um a um, na ordem das pastas e da
   numeração. As tabelas e os gráficos são gravados em `resultados/`.

As bases da RAIS têm mais de 100 MB e não estão no repositório. Para recriá-las,
é preciso um projeto no Google Cloud com o BigQuery ativo: substitua
`"SEU-PROJETO-GCP"` pelo ID do projeto em `codigos/1_dados/01` e `02` e rode
esses scripts e o `03`. Sem a RAIS, é possível reproduzir as estimações das
seções 4.2 e 4.3 a partir dos painéis já processados em `dados/`.

## Fontes de dados

| Dado | Fonte | Uso |
|---|---|---|
| Vínculos formais ativos em 31/12, por município, nacionalidade e CNAE | RAIS (Ministério do Trabalho e Emprego), via Base dos Dados: tabela `br_me_rais.microdados_vinculos` | participação de haitianos, vínculos, controles e estatísticas descritivas |
| PIB e VAB municipais, 2002–2023 | DEE/SPGG-RS, <https://dee.rs.gov.br/pib-municipal> | produtividade do trabalho |
| IPCA anual | IBGE, SIDRA, tabela 1737 (valores no script `2_painel/01`) | deflator, a preços de dezembro de 2023 |
| População municipal | IBGE: Censos 2010 e 2022 e SIDRA, tabela 6579 | log da população |
| Microrregiões geográficas | IBGE, API de Localidades | efeitos fixos e agrupamento por microrregião |
| Malha municipal de 2020 | IBGE, malhas territoriais | mapas |

## Correspondência entre o artigo e os scripts

| Elemento do artigo | Script (`codigos/`) |
|---|---|
| Gráfico 1 | `3_descritivos/01_grafico1_emprego_imigrante.R` |
| Figura 1 (a e b) e Figura 1 do Apêndice B | `3_descritivos/02_figura1_mapas.R` |
| Gráfico 2 | `3_descritivos/03_grafico2_concentracao_hhi.R` |
| Tabela 1 | `3_descritivos/04_tabela1_haitianos_industria.R` |
| Gráficos 3, 4 e 5 | `3_descritivos/05_graficos3a5_perfil_haitianos.R` |
| Gráfico 6 | `3_descritivos/06_grafico6_exposicao_produtividade.R` |
| Tabela 2 | `4_resultados/01_modelos.R` e `4_resultados/02_tabela2_efeito_produtividade.R` |
| Gráfico 7 | `5_validacao/02_grafico7_trajetoria_expostos.R` |
| Tabela 3 | `5_validacao/01_tabela3_corte_2020.R` |
| Tabela 4 | `5_validacao/03_tabela4_estudo_de_evento.R` |
| Apêndice B, Tabelas 1 e 3 | `3_descritivos/07_apB_tabelas1e3_maiores_municipios.R` |
| Apêndice B, Tabela 2 | `3_descritivos/08_apB_tabela2_atividades_cnae.R` |
| Apêndice B, Tabela 4 | `3_descritivos/09_apB_tabela4_setores_haitianos.R` |
| Apêndice B, Tabela 5 | `4_resultados/03_apB_tabela5_primeira_etapa.R` |
| Apêndice B, Tabela 6 | `5_validacao/04_apB_tabela6_exclusao_municipios.R` |
| Apêndice B, Tabela 7 | `5_validacao/05_apB_tabela7_especificacoes.R` |
| Apêndice B, Gráfico 1 | `3_descritivos/10_apB_grafico1_distribuicao_setorial.R` |
| Apêndice B, Gráfico 2 | `3_descritivos/11_apB_grafico2_setores_municipios.R` |
| Apêndice B, Gráfico 3 | `3_descritivos/12_apB_grafico3_nacionalidades_setor.R` |

## Observações

- **Filtro de remuneração.** A base usada no painel exclui os registros da RAIS
  com remuneração média por vínculo superior a 50 salários mínimos de 2022
  (1.667 registros, 6.489 vínculos, menos de 0,01% do total). Os números do
  total de imigrantes no Gráfico 1 e nas Tabelas 1 e 2 do Apêndice B foram
  calculados com a base sem esse filtro; a diferença é inferior a 0,1% (por
  exemplo, 53.599 vínculos de imigrantes em 2025, contra 53.561 na base
  filtrada). Para os haitianos, as duas bases coincidem.
- **Instrumento na forma clássica** (Tabela 5 do Apêndice B): estimado para
  2013–2023; a forma flexível, usada na especificação principal, para 2013–2020.
- **IPCA de 2022.** O script usa 5,78%; a tabela 1737 do SIDRA informa 5,79%.
  A diferença no deflator é inferior a 0,01% e é absorvida pelos efeitos fixos
  de ano.
- **Conferência.** Os painéis gerados pelos scripts foram comparados com os
  arquivos usados no artigo (diferenças numéricas inferiores a 10⁻¹⁴), e todas
  as tabelas de resultados foram reproduzidas com R 4.3.3, fixest 0.14.2,
  modelsummary 1.4.3 e plm 2.6-3. A Base dos Dados pode atualizar a RAIS; uma
  nova extração pode, portanto, gerar diferenças pequenas.

## Como citar

BENETTI, Isabelle. **Imigração haitiana no Rio Grande do Sul**: efeitos sobre a
produtividade do trabalho. 2026. Trabalho de Conclusão de Curso (Graduação em
Ciências Econômicas) – Faculdade de Ciências Econômicas, Universidade Federal do
Rio Grande do Sul, Porto Alegre, 2026.

## Uso de inteligência artificial

A organização e a documentação dos códigos deste repositório contaram com o
apoio de ferramenta de inteligência artificial generativa (Claude, da
Anthropic), conforme a declaração de uso de IAG do artigo.
