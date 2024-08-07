% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esgoto.R
\name{consolida_investimentos_esgoto}
\alias{consolida_investimentos_esgoto}
\title{Consolida os dados de investimentos esgoto}
\usage{
consolida_investimentos_esgoto(tabela)
}
\arguments{
\item{tabela}{contendo as colunas:
\itemize{
\item{custo_expansao_coleta_esgoto}
\item{custo_expansao_tratamento_esgoto}
\item{custo_reposicao_tratamento_esgoto}
\item{custo_reposicao_coleta_esgoto}
}}
}
\value{
tabela contendo os campos:
\itemize{
\item{investimento_expansao}
\item{investimento_reposicao}
\item{investimento_total}
}
}
\description{
Totaliza os dados investimento em expansão, reposição e total.
Adiciona coluna com o país para totalização.
}
\examples{
\dontrun{
tabela <- consolida_investimentos_esgoto(tabela)
}
}
