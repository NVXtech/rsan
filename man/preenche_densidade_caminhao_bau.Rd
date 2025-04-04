% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/residuos.R
\name{preenche_densidade_caminhao_bau}
\alias{preenche_densidade_caminhao_bau}
\title{Preenche caminhao bau}
\usage{
preenche_densidade_caminhao_bau(tabela_por_municipio, estado_faixa)
}
\arguments{
\item{tabela_por_municipio}{um \code{data.frame} com as colunas "codigo_municipio", "faixa", "CS001", "CS050", "CO064", "CO065", "CO066", "CO067", "CO068"}

\item{estado_faixa}{um \code{data.frame} com as colunas "estado", "faixa", "populacao_urbana_corrente", "CS050", "CS001"}
}
\value{
um \code{data.frame} contendo a coluna atendimento_relativo_coleta_seletiva
}
\description{
\enumerate{
\item Filtra os municipios que tem coleta seletiva
\item Calcula a quantidade de caminhões bau por faixa
}
}
