% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agua_rural.R
\name{filtra_setores_rurais}
\alias{filtra_setores_rurais}
\title{Filtra setores censitarios}
\usage{
filtra_setores_rurais(tabela)
}
\arguments{
\item{tabela}{um \code{data.frame} contendo a coluna \code{codigo_municipio}}

\item{setores}{um \code{list} contendo as situações dos setores que se quer manter.}
}
\value{
um \code{data.frame} contendo a coluna adicional \code{taxa_crescimento}.
}
\description{
Fitra a tabela deixando-a somente com as linhas com o tipo de situação desejada.
}
