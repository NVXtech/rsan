% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{somar_por_campo}
\alias{somar_por_campo}
\title{Soma dados agrupados pelo valor de um determinado campo}
\usage{
somar_por_campo(tabela, campo)
}
\arguments{
\item{tabela}{um \code{data.frame} com as colunas para serem somadas e com o nome do campo}

\item{campo}{um \code{character} contendo o nome do campo para agrupar}
}
\value{
a mesma tabela de entrada mais agrupada pelo campo
}
\description{
Soma todas os campos numéricos, os campos de caracteres são mantidos se únicos por grupo.
}
