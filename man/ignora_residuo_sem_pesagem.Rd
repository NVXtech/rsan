% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/residuos.R
\name{ignora_residuo_sem_pesagem}
\alias{ignora_residuo_sem_pesagem}
\title{Ignora quantidade de residuo em municipio sem pesagem}
\usage{
ignora_residuo_sem_pesagem(tabela)
}
\arguments{
\item{tabela}{um \code{data.frame} contendo as colunas residuo_coletado_ton_ano, atendimento_coleta_indiferenciada_hab e tem_pesagem}
}
\value{
um \code{data.frame} onde os municipios litoraneos não tem dados de quantidade de residuos
}
\description{
Ignora quantidade de residuo em municipio sem pesagem
}
\examples{
\dontrun{
tbl <- ignora_residuo_litoraneo(tbl)
}
}
