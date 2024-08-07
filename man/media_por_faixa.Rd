% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{media_por_faixa}
\alias{media_por_faixa}
\title{Média de todas as variáveis numéricas agrupando por faixa populacional}
\usage{
media_por_faixa(tabela, campo_faixa = "faixa")
}
\arguments{
\item{tabela}{tabela contendo as colunas de população, estado}

\item{campo_faixa}{parâmetro opcional para definir o campo
que contém a faixa populaciona (default: "faixa")}
}
\value{
tabela contendo a média de todas variáveis por faixa populacional
}
\description{
Média de todas as variáveis numéricas agrupando por faixa populacional
}
\examples{
faixa <- c(rep(1, 2), rep(3, 2), rep(2, 2), rep(4, 2))
valor <- c(1, 1, 2, 2, 3, 3, 4, 4)
input <- dplyr::tibble(faixa, valor)
output <- media_por_faixa(input)
}
