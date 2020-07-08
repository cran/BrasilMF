\name{dados_cvm}
\alias{dados_cvm_dfp}
\alias{dados_cvm}
\title{Dados CVM}
\usage{
dados_cvm_dfp(documento, ano, tipo)
dados_cvm(info)
}
\arguments{
\item{documento}{Qual o documentos pesquisado (ex. dre, balanco, dmpl)}
\item{ano}{ano da pesquisa}
\item{tipo}{consolidada ou individual}
\item{info}{Informacao desejada na CVM}
}
\description{
Pegas as demonstracoes financeiras padronizadas das companhias abertas disponibilizadas site http://dados.cvm.gov.br/
}
\examples{
dados_cvm("consultor_pf")
dados_cvm_dfp("dre",2020,"consolidada")
}
