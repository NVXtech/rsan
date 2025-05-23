% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esgoto_rural.R
\name{adiciona_deficit_rural_esgoto_pnad}
\alias{adiciona_deficit_rural_esgoto_pnad}
\title{Déficit rural esgoto PNAD}
\usage{
adiciona_deficit_rural_esgoto_pnad(tabela, deficit)
}
\arguments{
\item{tabela}{um \code{data.frame} contendo a coluna \code{estado}}

\item{deficit}{um \code{data.frame} contendo a coluna \code{estado} e \code{deficit_esgoto_relativo_rural}}
}
\value{
um \code{data.frame} contendo a coluna adicional \code{taxa_crescimento}.
}
\description{
Adiciona o déficit rural para sistema de abastecimento de esgoto (PNAD).
}
