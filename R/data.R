#' SNIS 2020
#'
#' Dataset contendo as informações e indicadores do SNIS do ano de 2020.
#'
#' @format A data frame with 5570 rows and 608 variables
#' \describe{
#'   \item{codigo_municipio}{codigo ibge do municipio}
#'   \item{`CODIGO`}{codigo do indicador ou informacao do SNIS ex: AG001}
#'   ... todas variáveis do snis
#' }
#' @source \url{http://www.snis.gov.br/}
"snis2020"

#' SINAPI Dezembro de 2021
#'
#' Dataset contendo preços e índices para a construção civil.
#'
#' @format Um data frame com 12414 items e 31 variáveis
#' \describe{
#'   \item{TIPO}{tipo do item}
#'   \item{CODIGO}{código do item}
#'   \item{DESCRICAO}{descrição do item}
#'   \item{UNIDADE}{unidade de medida (ex: m)}
#'   \item{PRECO_XX}{27 colunas com preço do estado (sigla XX) ex: PRECO_AC é o preço no ACRE.}
#' }
#' @source \url{https://www.caixa.gov.br/poder-publico/modernizacao-gestao/sinapi/}
"sinapi"

#' Municípios do Brasil
#'
#' Dataset contendo classificação de localização dos municípios (região e UF).
#'
#'
#' @format Um data frame com 5570 municípios e as seguintes 6 colunas:
#' \describe{
#'   \item{codigo_municipio}{código IBGE do munícipio}
#'   \item{municipio}{Nome do município}
#'   \item{regiao}{Região do Brasil}
#'   \item{regiao_sigla}{Sigla da região do Brasil}
#'   \item{estado}{Unidade federativa}
#'   \item{estado_sigla}{Sigla da unidade federativa}
#' }
#' @source \url{https://servicodados.ibge.gov.br/api/v1/localidades}
"municipio"

#' Projeto tipo para coleta de esgoto
#'
#' Dataset contendo lista de serviços e materiais para criar infraestrutura de coleta de esgoto.
#'
#'
#' @format Um data frame com 54 items serviços e materiais e 9 colunas, sendo estas:
#' \describe{
#'   \item{codigo}{Código do item no SINAPI}
#'   \item{grupo}{Grupo de custos (ex. serviço ou material)}
#'   \item{tipo}{Tipo de custo (ex. demolição, excavação)}
#'   \item{07a}{quantidade de material para cenário população pequena e de alto custo}
#'   \item{07b}{quantidade de material para cenário população pequena e de baixo custo}
#'   \item{08a}{quantidade de material para cenário população média e de alto custo}
#'   \item{08b}{quantidade de material para cenário população média e de baixo custo}
#'   \item{09a}{quantidade de material para cenário população grande e de alto custo}
#'   \item{09b}{quantidade de material para cenário população grande e de baixo custo}
#' }
"projeto_coleta_esgoto"

#' Projeto tipo para distribuicao de água
#'
#' Dataset contendo lista de serviços e materiais para criar infraestrutura de distribuicao de água.
#'
#'
#' @format Um data frame com 75 items serviços e materiais e 9 colunas, sendo estas:
#' \describe{
#'   \item{codigo}{Código do item no SINAPI}
#'   \item{grupo}{Grupo de custos (ex. serviço ou material)}
#'   \item{tipo}{Tipo de custo (ex. demolição, excavação)}
#'   \item{07a}{quantidade de material para cenário população pequena e de alto custo}
#'   \item{07b}{quantidade de material para cenário população pequena e de baixo custo}
#'   \item{08a}{quantidade de material para cenário população média e de alto custo}
#'   \item{08b}{quantidade de material para cenário população média e de baixo custo}
#'   \item{09a}{quantidade de material para cenário população grande e de alto custo}
#'   \item{09b}{quantidade de material para cenário população grande e de baixo custo}
#' }
"projeto_distribuicao_agua"

#' Projeto tipo para distribuicao de água
#'
#' Dataset contendo lista de serviços e materiais para criar infraestrutura de distribuicao de água.
#'
#'
#' @format Um data frame com 27 estados e 3 colunas, sendo estas:
#' \describe{
#'   \item{estado}{Sigal da unidade federativa}
#'   \item{subterranea}{Predominância em % de extração de água superficial}
#'   \item{superficial}{Predominância em % de extração de água subterrânea}
#' }
"projeto_predominancia_tipo_producao"

#' Áreas territoriais dos municípios brasileiros
#'
#' Dataset contendo as áreas dos muncípios brasileiros em km²
#'
#'
#' @format Um data frame com 1 municipios por linha e 2 colunas, sendo estas:
#' \describe{
#'   \item{codigo_municipio}{Código IBGE do município}
#'   \item{area}{Área do município em km²}
#' }
#' @source \url{https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/areas_territoriais/}
"area_municipio"

#' Áreas urbanas dos municípios brasileiros
#'
#' Dataset contendo as áreas urbanas dos muncípios brasileiros em km²
#'
#' @format Um data frame com 1 municipios por linha e 2 colunas, sendo estas:
#' \describe{
#'   \item{codigo_municipio}{Código IBGE do município}
#'   \item{area_urbana}{Área urbana do município em km²}
#' }
#' @source \url{https://biblioteca.ibge.gov.br/index.php/biblioteca-catalogo?view=detalhes&id=2100639}
"area_urbana_municipio"

#' SNIS - Resíduos sólidos dados por prestador
#'
#' Dataset contendo informaçoes das unidades de resíduos sólidos por prestador
#'
#' @format Uma lista de data frame indexados por ano de referência.
#' \describe{
#'   \item{Código}{Código IBGE do município}
#'   \item{Nome}{nome do munícipio}
#'   \item{UF}{estado}
#'   \item{Ano}{ano de referência}
#'   \item{UndCod}{código do prestador}
#'   \item{YYXXXX}{Indicadores e informações}
#' }
#' @source \url{http://www.snis.gov.br/}
"snis_rs"


#' CPRM - dados de pluviometria por município
#'
#' Dataset contendo informaçoes sobre a precipitação anual por município
#'
#' @format Uma data frame com um muncípio por linhas e as colunas:
#' \describe{
#'   \item{codigo_municipio}{Código IBGE do município}
#'   \item{municipio}{nome do munícipio}
#'   \item{estado}{estado}
#'   \item{precipitacao_media}{precipitação anual média}
#'   \item{precipitacao_moda}{precipitação anual moda}
#' }
#' @source \url{http://www.snis.gov.br/}
"pluviometria"


#' PLanos de drenagem municípios brasileiros
#'
#' Dataset contendo informaçoes sobre os plano de drenagem

#' @format Uma dataframe com um muncípio por linha e as colunas:
#' \describe{
#'   \item{codigo_municipio}{Código IBGE do município}
#'   \item{ano_plano}{ano do plano}
#'   \item{investimento}{valor do investimento total previsto para o ano_plano}
#' }
#' @source \url{http://www.snis.gov.br/}
"plano_drenagem"


#' Índices de Drenagem Urbana
#'
#' Dataset contendo os índices de caracteristícas físicas e infraestrutura

#' @format Uma dataframe com um muncípio por linha e as colunas:
#' \describe{
#'   \item{codigo_municipio}{Código IBGE do município}
#'   \item{caracteristicas_fisicas}{índice de caracteristica físicas}
#'   \item{infraestrutura}{índice de infraestrutura}
#' }
"indices_drenagem"
