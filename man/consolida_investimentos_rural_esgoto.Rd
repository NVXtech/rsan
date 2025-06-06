% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esgoto_rural.R
\name{consolida_investimentos_rural_esgoto}
\alias{consolida_investimentos_rural_esgoto}
\title{Consolida os dados de investimentos para água situação rural}
\usage{
consolida_investimentos_rural_esgoto(tabela)
}
\arguments{
\item{tabela}{contendo as colunas:
\itemize{
\item{investimento_expansao_individual}
\item{investimento_expansao_coleta_esgoto}
\item{investimento_expansao_tratamento_esgoto}
\item{investimento_reposicao_coleta_esgoto}
\item{investimento_reposicao_tratamento_esgoto}
}}
}
\value{
tabela contendo os campos:
\itemize{
\item{investimento_expansao}
\item{investimento_reposicao}
\item{investimento_total}
}
}
\description{
Totaliza os dados investimento em expansão, reposição e total.
}
