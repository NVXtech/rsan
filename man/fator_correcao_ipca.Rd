% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/igp.R
\name{fator_correcao_ipca}
\alias{fator_correcao_ipca}
\title{Fator de Correção de Preços - IPCA}
\usage{
fator_correcao_ipca(data_inicial, data_final)
}
\arguments{
\item{data_inicial}{Um vetor de datas (\code{Date}) contendo as datas iniciais para o cálculo.}

\item{data_final}{Um vetor de datas (\code{Date}) contendo as datas finais para o cálculo.}
}
\value{
Um vetor numérico (\code{double}) com os fatores de correção correspondentes para cada par de datas.
}
\description{
Calcula o fator de correção de preços com base no índice IPCA para um intervalo de datas.
}
\examples{
data_inicial <- as.Date(c("2016-01-01", "2017-01-01"))
data_final <- as.Date(c("2021-01-01", "2022-01-01"))
fator <- fator_correcao_ipca(data_inicial, data_final)
}
