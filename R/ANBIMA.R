dados_anbima <- function(info) {
  switch (info,
          "estrutura_termo" = {
            url <- "https://www.anbima.com.br/informacoes/est-termo/CZ-down.asp"

            tab1 <- read.table(url,sep =";", nrows = 3, dec = ",", quote = "")
            tab1 <- t(tab1)
            tab1 <- data.frame("Vertices" = tab1[2:7,1], "PREFIXADO" = tab1[2:7,2], "IPCA" = tab1[2:7,3], row.names = 1:6, stringsAsFactors = FALSE)
            tab1$IPCA <- gsub("\\,", ".", tab1$IPCA)
            tab1$PREFIXADO <- gsub("\\,", ".", tab1$PREFIXADO)

            tab2 <- read.table(url,sep =";", skip = 5, nrows = 69, dec = ",", col.names = c("Vertices", "ETTJ.IPCA", "ETTJ.PREF", "Inflacao_Implicita"), quote = "", stringsAsFactors = FALSE)
            tab2$Vertices <- gsub("\\.", "", tab2$Vertices)

            tab3 <- read.table(url,sep =";", skip = 78, nrows = 10, dec = ",", col.names = c("Vertices", "Taxas"), quote = "", stringsAsFactors = FALSE)
            tab3$Vertices <- gsub("\\.", "", tab3$Vertices)

            tab4 <- read.table(url,sep =";", skip = 91, nrows = 31, dec = ",", col.names = c("Titulo", "Selic", "Vencimento", "Erro"), quote = "", stringsAsFactors = FALSE)
            tab4$Vencimento <- as.Date(tab4$Vencimento, format = "%d/%m/%Y")

            lista <- list(tab1, tab2, tab3, tab4)

            return(lista)
          },
          "debenture_secundario" = {
            i <- 0
            while (RCurl::url.exists(url)==FALSE) {
              data <- as.Date(Sys.Date() - i)
              data <- format(data, "%y%m%d")
              url <- paste0("https://www.anbima.com.br/informacoes/merc-sec-debentures/arqs/db",data,".txt")
              i <-i + 1
            }

            tabela <- read.table(url, sep = "@", skip = 2, fill = TRUE, header = TRUE, dec = ",")
            colnames(tabela) <- c("Codigo","Nome","Repactuacao_Vencimento","Indice_Correcao","Taxa_Compra","Taxa_Venda","Taxa_Indicativa","Desvio_Padrao","Intervalo_Indicativo_Minimo","Intervalo_Indicativo_Maximo","PU","Perc_PU_Par","Duration","Perc_Reune","Referencia_NTN_B")
            tabela$Taxa_Compra <- gsub("--", NA, tabela$Taxa_Compra)
            tabela$Taxa_Compra <- gsub("\\,", "\\.", tabela$Taxa_Compra)
            tabela$Taxa_Compra <- as.numeric(tabela$Taxa_Compra)
            tabela$Taxa_Venda <- gsub("--", NA, tabela$Taxa_Venda)
            tabela$Taxa_Venda <- gsub("\\,", "\\.", tabela$Taxa_Venda)
            tabela$Taxa_Venda <- as.numeric(tabela$Taxa_Venda)
            tabela$PU <- gsub("N/D", NA, tabela$PU)
            tabela$PU <- gsub("\\,", "\\.", tabela$PU)
            tabela$PU <- as.numeric(tabela$PU)
            tabela$Perc_PU_Par <- gsub("N/D", NA, tabela$Perc_PU_Par)
            tabela$Perc_PU_Par <- gsub("\\,", "\\.", tabela$Perc_PU_Par)
            tabela$Perc_PU_Par <- as.numeric(tabela$Perc_PU_Par)
            tabela$Duration <- gsub("N/D", NA, tabela$Duration)
            tabela$Duration <- gsub("\\,", "\\.", tabela$Duration)
            tabela$Duration <- as.numeric(tabela$Duration)

            return(tabela)
          }
  )

}


