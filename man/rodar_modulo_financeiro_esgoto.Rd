% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esgoto.R
\name{rodar_modulo_financeiro_esgoto}
\alias{rodar_modulo_financeiro_esgoto}
\title{Módulo financeiro para demanda urbana de esgoto}
\usage{
rodar_modulo_financeiro_esgoto(input, orcamentario)
}
\arguments{
\item{input}{estrutura de dados (\code{reactive}) que guarda os parâmetros da interface gráfica}

\item{orcamentario}{um \code{data.frame} contendo a saida do modulo orcamentário}
}
\value{
um \code{data.frame} contendo as necessidade de investimentos e todos campos utilizados
}
\description{
Esta função organiza a ordem de execução das tarefas necessárias
para o cálculo de necessidades de investimento em sistema de abastecimento de esgoto.
}
