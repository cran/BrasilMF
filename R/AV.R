#Funcao av
dados_av <- function(funcao, simbolo, intervalo, tamanho, de, para, mercado, apikey) (

  if (missing(apikey)) {
    warning("informe a apikey")
    warning("criar apikey em https://www.alphavantage.co")
  } else if ((funcao %in% c("acao_intraday","acao_diaria","acao_diaria_ajustada","acao_semanal","acao_semanal_ajustada","acao_mensal","acao_mensal_ajustada","acao_ultimo","forex_ultimo",
                            "forex_intraday","forex_diario","forex_semanal","forex_mensal","cripto_ultimo","cripto_rating","cripto_diario","cripto_semanal","cripto_mensal"))==FALSE) {
    warning("Funcao inexistente, utilizar as seguintes funcoes: c(acao_intraday,acao_diaria,acao_diaria_ajustada,acao_semanal,acao_semanal_ajustada,acao_mensal,acao_mensal_ajustada,
             acao_ultimo,forex_ultimo,forex_intraday,forex_diario,forex_semanal,forex_mensal,cripto_ultimo,cripto_rating,cripto_diario,cripto_semanal,cripto_mensal)")
  } else {
    switch (funcao,
            "acao_intraday" = {
              if (exists("simbolo") & exists("intervalo") & exists("tamanho")) {
                url <- paste0("https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=",simbolo,"&interval=",intervalo,"&outputsize=",tamanho,"&apikey=",apikey,"&datatype=csv")
                tabela <- read.table(url, sep = ",", header = TRUE, stringsAsFactors = FALSE)
              } else {
                warning("Para pesquisar acao_intraday informar o seguintes campos: c(simbolo,intervalo,tamanho,apikey)")
              }
            },
            "acao_diaria" = {
              if (exists("simbolo") & exists("tamanho")) {
                url <- paste0("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=",simbolo,"&outputsize=",tamanho,"&apikey=",apikey,"&datatype=csv")
                tabela <- read.table(url, sep = ",", header = TRUE, stringsAsFactors = FALSE)
              } else {
                warning("Para pesquisar acao_diaria informar o seguintes campos: c(simbolo,tamanho,apikey)")
              }
            },
            "acao_diaria_ajustada" = {
              if (exists(simbolo) & exists("tamanho")) {
                url <- paste0("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=",simbolo,"&outputsize=",tamanho,"&apikey=",apikey,"&datatype=csv")
                tabela <- read.table(url, sep = ",", header = TRUE, stringsAsFactors = FALSE)
              } else {
                warning("Para pesquisar acao_diaria_ajustada informar o seguintes campos: c(simbolo,tamanho,apikey)")
              }
            },
            "acao_semanal" = {
              if (exists("simbolo")) {
                url <- paste0("https://www.alphavantage.co/query?function=TIME_SERIES_WEEKLY&symbol=",simbolo,"&apikey=",apikey,"&datatype=csv")
                tabela <- read.table(url, sep = ",", header = TRUE, stringsAsFactors = FALSE)
              } else {
                warning("Para pesquisar acao_semanal informar o seguintes campos: c(simbolo,apikey)")
              }
            },
            "acao_semanal_ajustada" = {
              if (exists("simbolo")) {
                url <- paste0("https://www.alphavantage.co/query?function=TIME_SERIES_WEEKLY_ADJUSTED&symbol=",simbolo,"&apikey=",apikey,"&datatype=csv")
                tabela <- read.table(url, sep = ",", header = TRUE, stringsAsFactors = FALSE)
              } else {
                warning("Para pesquisar acao_semanal_ajustada informar o seguintes campos: c(simbolo,apikey)")
              }
            },
            "acao_mensal" = {
              if (exists("simbolo")) {
                url <- paste0("https://www.alphavantage.co/query?function=TIME_SERIES_MONTHLY&symbol=",simbolo,"&apikey=",apikey,"&datatype=csv")
                tabela <- read.table(url, sep = ",", header = TRUE, stringsAsFactors = FALSE)
              } else {
                warning("Para pesquisar acao_mensal informar o seguintes campos: c(simbolo,apikey)")
              }
            },
            "acao_mensal_ajustada" = {
              if (exists("simbolo")) {
                url <- paste0("https://www.alphavantage.co/query?function=TIME_SERIES_MONTHLY_ADJUSTED&symbol=",simbolo,"&apikey=",apikey,"&datatype=csv")
                tabela <- read.table(url, sep = ",", header = TRUE, stringsAsFactors = FALSE)
              } else {
                warning("Para pesquisar acao_mensal_ajustada informar o seguintes campos: c(simbolo,apikey)")
              }
            },
            "acao_ultimo" = {
              if (exists("simbolo")) {
                url <- paste0("https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=",simbolo,"&apikey=",apikey,"&datatype=csv")
                tabela <- read.table(url, sep = ",", header = TRUE, stringsAsFactors = FALSE)
              } else {
                warning("Para pesquisar acao_ultimo informar o seguintes campos: c(simbolo,apikey)")
              }
            },
            "forex_ultimo" = {
              if (exists("de") & exists("para")) {
                url <- paste0("https://www.alphavantage.co/query?function=CURRENCY_EXCHANGE_RATE&from_currency=",de,"&to_currency=",para,"&apikey=",apikey,"&datatype=csv")
                tabela <- read.table(url, sep = ",", header = TRUE, stringsAsFactors = FALSE)
              } else {
                warning("Para pesquisar forex_ultimo informar o seguintes campos: c(de,para,apikey)")
              }
            },
            "forex_intraday" = {
              if (exists("de") & exists("para") & exists("intervalo") & exists("tamanho")) {
                url <- paste0("https://www.alphavantage.co/query?function=FX_INTRADAY&from_currency=",de,"&to_currency=",para,"interval=",intervalo,"outputsize=",tamanho,"&apikey=",apikey,"&datatype=csv")
                tabela <- read.table(url, sep = ",", header = TRUE, stringsAsFactors = FALSE)
              } else {
                warning("Para pesquisar forex_intraday informar o seguintes campos: c(de,para,intervalo,tamanho,apikey)")
              }
            },
            "forex_diario" = {
              if (exists("de") & exists("para") & exists("tamanho")) {
                url <- paste0("https://www.alphavantage.co/query?function=FX_DAILY&from_currency=",de,"&to_currency=",para,"outputsize=",tamanho,"&apikey=",apikey,"&datatype=csv")
                tabela <- read.table(url, sep = ",", header = TRUE, stringsAsFactors = FALSE)
              } else {
                warning("Para pesquisar forex_diario informar o seguintes campos: c(de,para,tamanho,apikey)")
              }
            },
            "forex_semanal" = {
              if (exists("de") & exists("para")) {
                url <- paste0("https://www.alphavantage.co/query?function=FX_WEEKLY&from_currency=",de,"&to_currency=",para,"&apikey=",apikey,"&datatype=csv")
                tabela <- read.table(url, sep = ",", header = TRUE, stringsAsFactors = FALSE)
              } else {
                warning("Para pesquisar forex_semanal informar o seguintes campos: c(de,para,apikey)")
              }
            },
            "forex_mensal" = {
              if (exists("de") & exists("para")) {
                url <- paste0("https://www.alphavantage.co/query?function=FX_MONTHLY&from_currency=",de,"&to_currency=",para,"&apikey=",apikey,"&datatype=csv")
                tabela <- read.table(url, sep = ",", header = TRUE, stringsAsFactors = FALSE)
              } else {
                warning("Para pesquisar forex_mensal informar o seguintes campos: c(de,para,apikey)")
              }
            },
            "cripto_ultimo" = {
              if (exists("de") & exists("para")) {
                url <- paste0("https://www.alphavantage.co/query?function=CURRENCY_EXCHANGE_RATE&from_currency=",de,"&to_currency=",para,"&apikey=",apikey,"&datatype=csv")
                tabela <- read.table(url, sep = ",", header = TRUE, stringsAsFactors = FALSE)
              } else {
                warning("Para pesquisar cripto_ultimo informar o seguintes campos: c(de,para,apikey)")
              }
            },
            "cripto_rating" = {
              if (exists("simbolo")) {
                url <- paste0("https://www.alphavantage.co/query?function=CRYPTO_RATING&symbol=",simbolo,"&apikey=",apikey,"&datatype=csv")
                tabela <- read.table(url, sep = ",", header = TRUE, stringsAsFactors = FALSE)
              } else {
                warning("Para pesquisar cripto_rating informar o seguintes campos: c(simbolo,apikey)")
              }
            },
            "cripto_diario" = {
              if (exists("simbolo") & exists("mercado")) {
                url <- paste0("https://www.alphavantage.co/query?function=DIGITAL_CURRENCY_DAILY&symbol=",simbolo,"&market=",mercado,"&apikey=",apikey,"&datatype=csv")
                tabela <- read.table(url, sep = ",", header = TRUE, stringsAsFactors = FALSE)
              } else {
                warning("Para pesquisar cripto_diario informar o seguintes campos: c(simbolo,mercado,apikey)")
              }
            },
            "cripto_semanal" = {
              if (exists("simbolo") & exists("mercado")) {
                url <- paste0("https://www.alphavantage.co/query?function=DIGITAL_CURRENCY_WEEKLY&symbol=",simbolo,"&market=",mercado,"&apikey=",apikey,"&datatype=csv")
                tabela <- read.table(url, sep = ",", header = TRUE, stringsAsFactors = FALSE)
              } else {
                warning("Para pesquisar cripto_semanal informar o seguintes campos: c(simbolo,mercado,apikey)")
              }
            },
            "cripto_mensal" = {
              if (exists("simbolo") & exists("mercado")) {
                url <- paste0("https://www.alphavantage.co/query?function=DIGITAL_CURRENCY_MONTHLY&symbol=",simbolo,"&market=",mercado,"&apikey=",apikey,"&datatype=csv")
                tabela <- read.table(url, sep = ",", header = TRUE, stringsAsFactors = FALSE)
              } else {
                warning("Para pesquisar cripto_mensal informar o seguintes campos: c(simbolo,mercado,apikey)")
              }
            }
    )
    return(tabela)
  }
)
