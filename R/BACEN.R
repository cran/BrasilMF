dados_bacen <- function(cod, dt_ini,dt_fim) {

  if (missing(cod)) {
    warning("Informe o campo cod")
  } else if (missing(dt_ini) | missing(dt_fim)) {
    url <- paste0("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",cod,"/dados?formato=csv")
    base <- read.table(url, sep = ";", header = TRUE, stringsAsFactors = FALSE)
    return(base)
  } else {
    dt_ini <- format(as.Date(dt_ini), "%d/%m/%Y")
    dt_fim <- format(as.Date(dt_fim), "%d/%m/%Y")
    url <- paste0("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",cod,"/dados?formato=csv&dataInicial=",dt_ini,"&dataFinal=",dt_fim)
    base <- read.table(url, sep = ";", header = TRUE, stringsAsFactors = FALSE)
    return(base)
  }
}
