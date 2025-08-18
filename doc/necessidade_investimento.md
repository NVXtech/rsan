# Como a necessidade de investimento é calculada (guia para não técnicos)

Este documento descreve, em linguagem acessível, como o sistema calcula a "Necessidade de Investimento" em saneamento para cada componente (água, esgoto, resíduos sólidos e drenagem/águas pluviais). Também explica quais arquivos CSV o sistema usa e como você pode atualizá‑los sem precisar alterar o código.

## Visão geral

O cálculo da necessidade de investimento combina dados sobre atendimento, infraestrutura, população e parâmetros técnicos (como vida útil e custos unitários). O sistema carrega fontes padronizadas em CSV, valida as colunas essenciais e executa um fluxo de transformações e cálculos que resultam em indicadores e valores de investimento por município.

Pontos-chave:

- Os dados ficam em pastas como `dados/base_calculo` e `dados/brutos`.
- Cada arquivo serve como fonte para um tipo específico de dado (atendimento, população, capacidade instalada, etc.).
- O usuário pode atualizar ou substituir arquivos CSV para alterar os dados de entrada sem mexer no código.

## Onde os arquivos CSV ficam e como são nomeados

Os arquivos de entrada importantes geralmente estão em:

- `dados/base_calculo/` — arquivos de base já prontos para cálculo (ex.: `agua_snis_2022.csv`, `esgoto_sinisa_2023.csv`, `estimativa_2023.csv`, `censo_2022.csv`).
- `dados/brutos/` — dados originais obtidos de fontes (PNADc, SINISA, SNIS). Ex.: `dados/brutos/pnadc/` ou `dados/brutos/sinisa/<ano>/`.

Nomes comuns (exemplos):

- `agua_sinis_2022.csv` — atendimento/variáveis de água (fonte SNIS)
- `esgoto_sinisa_2023.csv` — atendimento/variáveis de esgoto (fonte SINISA)
- `estimativa_2023.csv` — estimativas populacionais (PNADc ou projeções)
- `censo_2010.csv` ou `censo_2022.csv` — dados do Censo do IBGE
- `municipios_criticos.csv` — lista de municípios marcados como críticos (opcional)

Formato esperado

- CSV com separador `;` (padrão usado pelo sistema) e decimais com `,`.
- Cada arquivo deve incluir a coluna `codigo_municipio` (código IBGE) quando aplicável.

## Flujo geral de cálculo (passo a passo simples)

1. O sistema carrega as bases CSV necessárias para o componente (ex.: atendimento, população, capacidade instalada).
2. Os dados são validados — o sistema verifica se as colunas essenciais estão presentes.
3. Os indicadores por município são complementados (por exemplo: converter proporção de atendimento em número absoluto usando a população).
4. Calcula-se a necessidade de expansão (atender população sem rede) e a necessidade de reposição (substituir o que já existe por fim de vida útil).
5. Aplica‑se fatores técnicos e custos unitários para transformar necessidades físicas (metros, m3/ano, toneladas) em valores monetários.
6. Remove‑se registros que não são considerados críticos (se configurado), mantendo o foco nas prioridades.
7. Gera‑se as tabelas de saída com o investimento necessário por município e por ação (expansão, reposição, cadastro, etc.).

## Como interpretar os principais resultados

- `investimento_expansao`: valor necessário para expandir serviços até determinado padrão de cobertura.
- `investimento_reposicao`: valor necessário para repor infraestrutura que atingiu fim de vida útil.
- `investimento_total`: soma das parcelas relevantes (expansão + reposição + cadastro e outros custos).

Os relatórios normalmente apresentam valores por município, por tipo de intervenção e acumulados por região ou estado.

## Cálculo por componente (explicação amigável)

### Água (abastecimento)

O que se usa como entrada:

- Taxas de atendimento em água por município (arquivo `agua_*.csv`).
- Produção/consumo e extensão de rede (quando disponível).
- População (arquivo `estimativa_YYYY.csv` ou `censo_YYYY.csv`).
- Parâmetros técnicos: custo por habitante, vida útil, depreciação, investimento per capita padrão.

Como é calculado (resumo simples):

1. Se tivermos taxa de atendimento (%) e população, calcula‑se número de habitantes atendidos e não atendidos.
2. Necessidade de expansão = população não atendida × custo por habitante (ou cálculo per capita definido).
3. Necessidade de reposição = parcela da infraestrutura que atingiu vida útil × custo de reposição.
4. Soma‑se as parcelas e aplica‑se ajustes (ex.: custos de cadastro, obras complementares).

O que o usuário pode alterar sem programar:

- Atualizar o arquivo `agua_<fonte>_<ano>.csv` com novos números de atendimento.
- Ajustar parâmetros no painel de entrada (vida útil, custo per capita) — quando disponível na interface.

### Esgoto (saneamento sanitário)

Entradas típicas:

- Taxas de atendimento de esgoto (arquivo `esgoto_*.csv`).
- Extensão de rede de esgoto, volume de esgoto tratado.
- População e parâmetros de custo.

Resumo do cálculo:

1. Calcula‑se habitantes atendidos e não atendidos a partir das taxas e população.
2. Expansão: população sem serviço × custo per capita para rede e tratamento.
3. Reposição: infraestrutura com vida útil excedida × custo unitário.
4. Aplica‑se fatores de ajuste (ex.: se o município já possui tratamento parcial, reduz‑se a necessidade).

### Resíduos sólidos

Entradas:

- Dados sobre coleta (indiferenciada, seletiva), existência de pesagem, toneladas coletadas/recuperadas.

Resumo:

1. Identifica‑se municípios sem serviço de coleta adequada.
2. Calcula‑se investimento para implantação/expansão (veículos, pontos de coleta, unidades de tratamento).
3. Reposição/substituição de frota ou infraestruturas com base em vida útil.

### Drenagem e águas pluviais

Entradas:

- Índices de drenagem, precipitação, área urbana, capacidade instalada local.

Resumo:

1. Avalia‑se vulnerabilidade considerando precipitação, densidade urbana e índices de drenagem.
2. Necessidade de expansão ou obras de correção é estimada com base em déficit de capacidade e parâmetros técnicos.
3. Calcula‑se reposição e custo de obras de infraestrutura pluvial.

## Critérios de prioridade: municípios críticos

O sistema pode filtrar os resultados para mostrar apenas municípios "críticos". A lista de críticos é mantida em um CSV (por exemplo `dados/base_calculo/municipios_criticos.csv`). Para alterar quais municípios são considerados críticos, basta editar esse arquivo (adicionar/remover `codigo_municipio`).

## Fontes de dados específicas

- PNAD Contínua (PNADc): dados de atendimento populacional e estimativas.
  - Geralmente armazenados em `dados/brutos/pnadc/` e processados para `dados/base_calculo/estimativa_YYYY.csv`.
- SINISA / SNIS: bases de serviços e infraestrutura.
  - Arquivos brutos do SINISA ficam em `dados/brutos/sinisa/<ano>/` e são processados para CSV em `dados/base_calculo/`.

## Como atualizar os dados (passos práticos)

1. Localize o arquivo CSV apropriado em `dados/base_calculo/` (por exemplo `agua_sinis_2022.csv`).
2. Faça uma cópia de segurança do arquivo atual.
3. Substitua-o pelo CSV novo (mesmo padrão de colunas) ou edite as células necessárias.
4. Rode o fluxo de cálculo novamente (pela interface ou pela função principal) — os resultados usarão os novos dados.

Dicas:

- Sempre preserve a coluna `codigo_municipio` quando o dado for por município.
- Mantenha o mesmo formato de separador e decimal: `;` e `,`.

## Perguntas frequentes (FAQ)

Q: Preciso saber programar para atualizar os dados?
A: Não. Basta substituir os arquivos CSV na pasta `dados/base_calculo` respeitando os nomes e colunas esperadas.

Q: Onde altero os parâmetros técnicos (vida útil, custos)?
A: Esses parâmetros costumam estar disponíveis na interface do sistema (painel de entrada) ou em arquivos de configuração. Consulte o manual de usuário do painel ou o responsável técnico.

Q: Como vejo apenas municípios prioritários?
A: Edite o arquivo `dados/base_calculo/municipios_criticos.csv`. O sistema aplicará o filtro automaticamente se configurado.

## Contatos e suporte

Se precisar de ajuda para formatar um CSV ou interpretar os resultados, contate a equipe técnica responsável pelo projeto (informar e‑mail/telefone interno aqui).

---

Este documento é um guia orientado a não técnicos. Se quiser, posso gerar uma versão em PDF, ou criar um checklist para revisão de arquivos CSV antes de rodar os cálculos.
