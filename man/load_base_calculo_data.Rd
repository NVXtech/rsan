% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base_calculo.R
\name{load_base_calculo_data}
\alias{load_base_calculo_data}
\title{Carrega a base_calculo salva no pacote}
\usage{
load_base_calculo_data(
  output_dir = file.path("dados", "base_calculo"),
  overwrite = FALSE
)
}
\value{
Lista nomeada de data.frames, cada um correspondente a um arquivo CSV original de dados/base_calculo.
}
\description{
Esta função carrega o arquivo 'base_calculo.rda' do diretório 'data/' e retorna uma lista de data.frames.
}
