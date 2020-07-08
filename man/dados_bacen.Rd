\name{dados_bacen}
\alias{dados_bacen}
\title{Package Description}
\usage{
dados_bacen(cod, dt_ini,dt_fim)
}
\arguments{
\item{cod}{codigo utilizado pelo BACEN para pesquisa}
\item{dt_ini}{data de inicio da tabela, deve ser menor que a dt_fim}
\item{dt_fim}{data fim da tabela, deve ser maior que a dt_ini}
}
\description{
Procura todas as informacoes do BACEN disponibilizada no site https://dadosabertos.bcb.gov.br/
}
\examples{
dados_bacen("27749")
}
