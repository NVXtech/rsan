% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agua.R
\name{fill_missing_density}
\alias{fill_missing_density}
\title{Completa os dados faltantes de densidade.}
\usage{
fill_missing_density(density, fields)
}
\arguments{
\item{density}{um \code{data.frame} contendo os dados de densidades}

\item{fields}{lista dos campos para preencher}
}
\value{
um \code{data.frame} com a mesma tabela mas sem dados faltantes
}
\description{
Completa os dados faltantes de densidade.
}
\examples{
\dontrun{
df <- fill_missing_density(density, fields)
}
}
