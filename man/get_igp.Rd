% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/igp.R
\name{get_igp}
\alias{get_igp}
\title{Indice IGP-DI}
\usage{
get_igp()
}
\value{
tabela contendo o indice IGP-DI mais atualizado, contendo os campos:
\itemize{
\item{data}{é a Data}
\item{igp}{é o valor do índice}
}
}
\description{
Retorna o indíce IGP-DI mês a mês até o dia mais atual.
Fonte: http://www.ipeadata.gov.br/
}
\examples{
igp <- get_igp()
}
