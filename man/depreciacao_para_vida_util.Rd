% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reposicao.R
\name{depreciacao_para_vida_util}
\alias{depreciacao_para_vida_util}
\title{Transforma depreciacao em porcentagem para vida_util}
\usage{
depreciacao_para_vida_util(valor)
}
\arguments{
\item{valor}{é o valor da depreciação em porcentagem (ex. 2\% o valor = 2)}
}
\value{
valor em anos representando vida útil
}
\description{
Transforma depreciacao em porcentagem para vida_util
}
\examples{
vida_util <- depreciacao_para_vida_util(2)
}
