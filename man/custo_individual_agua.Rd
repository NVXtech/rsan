% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agua_rural.R
\name{custo_individual_agua}
\alias{custo_individual_agua}
\title{Custo individual para sistemas de abastecimento de água}
\usage{
custo_individual_agua(tabela, custo, custo_sem_disponibilidade)
}
\arguments{
\item{tabela}{um \code{data.frame} contendo as colunas \code{seguranca_hidrica} e \code{semiarido}.}
}
\value{
um \code{data.frame} contendo a coluna adicional \code{custo_agua_individual}.
}
\description{
Adiciona coluna com custo individual para sistemas de abastecimento de água.
}
