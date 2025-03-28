% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drenagem.R
\name{tbl_longa_investimento_drenagem}
\alias{tbl_longa_investimento_drenagem}
\title{Cria tabela longa de necessidade de investimento do componente drenagem}
\usage{
tbl_longa_investimento_drenagem(tabela)
}
\arguments{
\item{tabela}{contendo as colunas:
\itemize{
\item{investimento_expansao}
\item{investimento_reposicao}
\item{investimento_cadastro}
}}
}
\value{
tabela contendo os campos:
\itemize{
\item{estado}
\item{regiao}
\item{componente}
\item{situacao}
\item{destino}
\item{subsistema}
}
}
\description{
Cria tabela longa de necessidade de investimento do componente drenagem
}
