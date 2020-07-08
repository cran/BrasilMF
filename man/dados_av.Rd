\name{dados_av}
\alias{dados_av}
\title{Package Description}
\usage{
dados_av(funcao, simbolo, intervalo, tamanho, de, para, mercado, apikey )
}
\arguments{
\item{funcao}{Informar o tipo de informacao que voce deseja}
\item{simbolo}{Informar o tipo de informacao que voce deseja}
\item{intervalo}{Informar o tipo de informacao que voce deseja}
\item{tamanho}{Informar o tipo de informacao que voce deseja}
\item{de}{Informar o tipo de informacao que voce deseja}
\item{para}{Informar o tipo de informacao que voce deseja}
\item{mercado}{Informar o tipo de informacao que voce deseja}
\item{apikey}{Informar o tipo de informacao que voce deseja}
}
\description{
Procura as principais funcoes do ALPHA VANTAGE site https://www.alphavantage.co/documentation/
}
\examples{
dados_av(funcao = "acao_mensal", simbolo = "IBM", apikey = 'demo')
}
