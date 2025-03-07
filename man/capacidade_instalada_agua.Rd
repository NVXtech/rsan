% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agua.R
\name{capacidade_instalada_agua}
\alias{capacidade_instalada_agua}
\title{Capacidade instalada sistema de abastecimento de água}
\usage{
capacidade_instalada_agua(snis, custo)
}
\arguments{
\item{snis}{um \code{data.frame} contendo as colunas \code{codigo_municipio}, \code{AG005} e \code{AG006}}

\item{custo}{um \code{data.frame} contendo as colunas \code{codigo_municipio}, \code{custo_relativo_producao}, \code{preco_distribuicao_agua}}
}
\value{
um \code{data.frame} contendo as colunas \code{capacidade_instalada_distribuicao} e \code{capacidade_instalada_producao}
}
\description{
Capacidade instalada sistema de abastecimento de água
}
