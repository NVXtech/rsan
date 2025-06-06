% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agua.R
\name{verifica_codigos_faltantes}
\alias{verifica_codigos_faltantes}
\title{Verifica códigos faltantes no SINAPI}
\usage{
verifica_codigos_faltantes(df, sinapi)
}
\arguments{
\item{df}{Um \code{data.frame} contendo os códigos do projeto.}

\item{sinapi}{Um \code{data.frame} contendo os códigos e descrições do SINAPI.}
}
\value{
Um valor lógico. Retorna \code{TRUE} se todos os códigos estão presentes no SINAPI, caso contrário, retorna \code{FALSE} e registra os códigos faltantes.
}
\description{
Esta função verifica se há códigos no dataframe de projeto que não estão presentes no dataframe do SINAPI.
}
\examples{

projeto <- data.frame(codigo = c("001", "002", "003"))
sinapi <- data.frame(CODIGO = c("001", "002"), DESCRICAO = c("Desc1", "Desc2"))
verifica_codigos_faltantes(projeto, sinapi)
}
