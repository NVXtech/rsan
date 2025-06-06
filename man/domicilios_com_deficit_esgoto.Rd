% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esgoto_rural.R
\name{domicilios_com_deficit_esgoto}
\alias{domicilios_com_deficit_esgoto}
\title{Número de domicílios com deficit de esgoto}
\usage{
domicilios_com_deficit_esgoto(tabela)
}
\arguments{
\item{tabela}{um \code{data.frame} contendo as colunas \code{domicilios_projecao} e \code{deficit_esgoto_relativo_rural}.}
}
\value{
um \code{data.frame} contendo a coluna adicional \code{domicilios_deficit_esgoto}.
}
\description{
Calcula o número de domicílios com déficit de sistemas de estação de esgoto.
}
