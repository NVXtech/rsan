% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drenagem.R
\name{aplica_regressao_multipla_drenagem}
\alias{aplica_regressao_multipla_drenagem}
\title{Aplica regressao multipla drenagem}
\usage{
aplica_regressao_multipla_drenagem(tabela, parametros)
}
\arguments{
\item{tabela}{um \code{data.frame} contendo as colunas \code{precipitacao}, \code{densidade_urbana}, \code{caracteristicas_fisicas} e \code{infrestrutura}}

\item{parametros}{um \code{list} contendo os parâmetros do modelo de regressão}
}
\value{
tabela com a coluna adicional \code{investimento_expansao}
}
\description{
Aplica regressao multipla drenagem
}
