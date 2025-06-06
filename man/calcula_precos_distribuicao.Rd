% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agua.R
\name{calcula_precos_distribuicao}
\alias{calcula_precos_distribuicao}
\title{Calcula preços distribuicao de água e coleta de esgoto}
\usage{
calcula_precos_distribuicao(
  df_projeto,
  sinapi,
  taxa_servico = 26,
  taxa_materiais = 18,
  fator_correcao = 1
)
}
\arguments{
\item{df_projeto}{um \code{data.frame} contendo os itens do projeto coluna quantidade e codigo sinapi}

\item{sinapi}{o \code{data.frame} com os preços do SINAPI}

\item{taxa_servico}{fator de correção do preço para serviços}

\item{taxa_materiais}{fator de correção do preço para materiais}

\item{fator_correcao}{fator de correção monetária (1+\% de correção)}
}
\value{
um \code{data.frame} com preços por projeto e por estado
}
\description{
Calcula preços distribuicao de água e coleta de esgoto
}
\examples{
\dontrun{
df <- calcula_precos_distribuicao(projeto, sinapi, 26, 18)
}
}
