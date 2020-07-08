#Fundos Estruturados: Informacao Cadastral
dados_cvm <- function(info) {
  validos <- c("fie","fie_adiministrador","fie_auditor","fie_gestor","auditor_pj","auditor_pf","fi","participante_intermediario_empresa",
               "participante_intermediario_responsavel","agente_autonomo_pj","agente_autonomo_pf", "cias_incentivadas", "cias_estrangeiras", "cias_abertas",
               "representante_inv_n_residente_pj", "representante_inv_n_residente_pf", "consultor_pj", "consultor_pf", "consultor_diretor", "consultor_socios",
               "administrador_fii", "administrador_carteira_pj", "administrador_carteira_pf", "administrador_carteira_diretor", "administrador_carteira_socios",
               "administrador_carteira_responsaveis")
  if (info %in% validos) {
    switch (info,
            "fie" = {
              url <- 'http://dados.cvm.gov.br/dados/FIE/CAD/DADOS/cad_fie.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_fie <- read.csv(unz(temp, "cad_fie.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_fie)
            },
            "fie_adiministrador" = {
              url <- 'http://dados.cvm.gov.br/dados/FIE/CAD/DADOS/cad_fie.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_fie_admin <- read.csv(unz(temp, "cad_fie_admin.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_fie_admin)
            },
            "fie_auditor" = {
              url <- 'http://dados.cvm.gov.br/dados/FIE/CAD/DADOS/cad_fie.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_fie_auditor <- read.csv(unz(temp, "cad_fie_auditor.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_fie_auditor)
            },
            "fie_gestor" = {
              url <- 'http://dados.cvm.gov.br/dados/FIE/CAD/DADOS/cad_fie.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_fie_gestor <- read.csv(unz(temp, "cad_fie_gestor.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_fie_gestor)
            },
            "auditor_pj" = {
              url <- 'http://dados.cvm.gov.br/dados/AUDITOR/CAD/DADOS/cad_auditor.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_auditor_pj <- read.csv(unz(temp, "cad_auditor_pj.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_auditor_pj)
            },
            "auditor_pf" = {
              url <- 'http://dados.cvm.gov.br/dados/AUDITOR/CAD/DADOS/cad_auditor.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_auditor_pf <- read.csv(unz(temp, "cad_auditor_pf.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_auditor_pf)
            },
            "fi" = {
              data = Sys.Date()-1
              cal <-bizdays::create.calendar("Brazil/ANBIMA", bizdays::holidaysANBIMA, weekdays=c("saturday", "sunday"))
              data <- bizdays::bizseq(data-5,data,cal)
              data <- max(data)
              data <- format(as.Date(data,"1970-01-01"), "%Y%m%d")
              url <- paste0("http://dados.cvm.gov.br/dados/FI/CAD/DADOS/inf_cadastral_fi_",data,".csv")
              fi <- read.table(url, sep = ";", quote = "", stringsAsFactors = FALSE)
              return(fi)
            },
            "participante_intermediario_empresa" = {
              url <- 'http://dados.cvm.gov.br/dados/INTERMED/CAD/DADOS/cad_intermed.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_intermed <- read.csv(unz(temp, "cad_intermed.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_intermed)
            },
            "participante_intermediario_responsavel" = {
              url <- 'http://dados.cvm.gov.br/dados/INTERMED/CAD/DADOS/cad_intermed.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_intermed_resp <- read.csv(unz(temp, "cad_intermed_resp.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_intermed_resp)
            },
            "agente_autonomo_pj" = {
              url <- 'http://dados.cvm.gov.br/dados/AGENTE_AUTON/CAD/DADOS/cad_agente_auton.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_agente_auton_pj <- read.csv(unz(temp, "cad_agente_auton_pj.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_agente_auton_pj)
            },
            "agente_autonomo_pf" = {
              url <- 'http://dados.cvm.gov.br/dados/AGENTE_AUTON/CAD/DADOS/cad_agente_auton.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_agente_auton_pf <- read.csv(unz(temp, "cad_agente_auton_pf.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_agente_auton_pf)
            },
            "cias_incentivadas" = {
              url <- 'http://dados.cvm.gov.br/dados/CIA_INCENT/CAD/DADOS/cad_cia_incent.csv'
              cad_cia_incet <- read.csv(url, sep = ";", quote = "", stringsAsFactors = FALSE)
              return(cad_cia_incet)
            },
            "cias_estrangeiras" = {
              url <- 'http://dados.cvm.gov.br/dados/CIA_ESTRANG/CAD/DADOS/cad_cia_estrang.csv'
              cad_cia_estrang <- read.csv(url, sep = ";", quote = "", stringsAsFactors = FALSE)
              return(cad_cia_estrang)
            },
            "cias_abertas" = {
              url <- 'http://dados.cvm.gov.br/dados/CIA_ABERTA/CAD/DADOS/cad_cia_aberta.csv'
              cad_cia_aberta <- read.csv(url, sep = ";", quote = "", stringsAsFactors = FALSE)
              return(cad_cia_aberta)
            },
            "representante_inv_n_residente_pj" = {
              url <- 'http://dados.cvm.gov.br/dados/INVNR/CAD/DADOS/cad_invnr_repres.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_invnr_repres_pj <- read.csv(unz(temp, "cad_invnr_repres_pj.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_invnr_repres_pj)
            },
            "representante_inv_n_residente_pf" = {
              url <- 'http://dados.cvm.gov.br/dados/INVNR/CAD/DADOS/cad_invnr_repres.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_invnr_repres_pf <- read.csv(unz(temp, "cad_invnr_repres_pf.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_invnr_repres_pf)
            },
            "consultor_pj" = {
              url <- 'http://dados.cvm.gov.br/dados/CONSULTOR_VLMOB/CAD/DADOS/cad_consultor_vlmob.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_consultor_vlmob_pj <- read.csv(unz(temp, "cad_consultor_vlmob_pj.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_consultor_vlmob_pj)
            },
            "consultor_pf" = {
              url <- 'http://dados.cvm.gov.br/dados/CONSULTOR_VLMOB/CAD/DADOS/cad_consultor_vlmob.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_consultor_vlmob_pf <- read.csv(unz(temp, "cad_consultor_vlmob_pf.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_consultor_vlmob_pf)
            },
            "consultor_diretor" = {
              url <- 'http://dados.cvm.gov.br/dados/CONSULTOR_VLMOB/CAD/DADOS/cad_consultor_vlmob.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_consultor_vlmob_diretor <- read.csv(unz(temp, "cad_consultor_vlmob_diretor.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_consultor_vlmob_diretor)
            },
            "consultor_socios" = {
              url <- 'http://dados.cvm.gov.br/dados/CONSULTOR_VLMOB/CAD/DADOS/cad_consultor_vlmob.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_consultor_vlmob_socios <- read.csv(unz(temp, "cad_consultor_vlmob_socios.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_consultor_vlmob_socios)
            },
            "administrador_fii" = {
              url <- 'http://dados.cvm.gov.br/dados/ADM_FII/CAD/DADOS/cad_adm_fii.csv'
              cad_adm_fii <- read.csv(url, sep = ";", quote = "", stringsAsFactors = FALSE)
              return(cad_adm_fii)
            },
            "administrador_carteira_pj" = {
              url <- 'http://dados.cvm.gov.br/dados/ADM_CART/CAD/DADOS/cad_adm_cart.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_adm_cart_pj <- read.csv(unz(temp, "cad_adm_cart_pj.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_adm_cart_pj)
            },
            "administrador_carteira_pf" = {
              url <- 'http://dados.cvm.gov.br/dados/ADM_CART/CAD/DADOS/cad_adm_cart.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_adm_cart_pf <- read.csv(unz(temp, "cad_adm_cart_pf.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_adm_cart_pf)
            },
            "administrador_carteira_diretor" = {
              url <- 'http://dados.cvm.gov.br/dados/ADM_CART/CAD/DADOS/cad_adm_cart.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_adm_cart_diretor <- read.csv(unz(temp, "cad_adm_cart_diretor.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_adm_cart_diretor)
            },
            "administrador_carteira_socios" = {
              url <- 'http://dados.cvm.gov.br/dados/ADM_CART/CAD/DADOS/cad_adm_cart.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_adm_cart_socios <- read.csv(unz(temp, "cad_adm_cart_socios.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_adm_cart_socios)
            },
            "administrador_carteira_responsaveis" = {
              url <- 'http://dados.cvm.gov.br/dados/ADM_CART/CAD/DADOS/cad_adm_cart.zip'
              temp <- tempfile()
              download.file(url,temp)
              cad_adm_cart_resp <- read.csv(unz(temp, "cad_adm_cart_resp.csv"), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              return(cad_adm_cart_resp)
            },
            "fi_estruturado" = {
              data <- Sys.Date()
              data_alt <- format(data, "%Y%m")
              url <- paste0('http://dados.cvm.gov.br/dados/FIE/MEDIDAS/DADOS/medidas_mes_fie_',data_alt,'.csv')
              while(RCurl::url.exists(url) == FALSE) {
                data <- seq(data, length = 2, by = "-1 month")[2]
                data_alt <- format(data, "%Y%m")
                url <- paste0('http://dados.cvm.gov.br/dados/FIE/MEDIDAS/DADOS/medidas_mes_fie_',data_alt,'.csv')
              }
              Tabela <- read.table(url, sep = ";", quote = "", stringsAsFactors = FALSE)
              return(Tabela)
            },
            "fi_icvm_555" = {
              data <- Sys.Date()
              data_alt <- format(data, "%Y%m")
              url <- paste0('http://dados.cvm.gov.br/dados/FI/DOC/LAMINA/DADOS/lamina_fi_',data_alt,'.zip')
              while(RCurl::url.exists(url) == FALSE) {
                data <- seq(data, length = 2, by = "-1 month")[2]
                data_alt <- format(data, "%Y%m")
                url <- paste0('http://dados.cvm.gov.br/dados/FI/DOC/LAMINA/DADOS/lamina_fi_',data_alt,'.zip')
              }

              temp <- tempfile()
              download.file(url,temp)
              Lamina_FI <- read.table(unz(temp, paste0("lamina_fi_",data_alt,".csv")), header = FALSE, sep = ";", quote = "", col.names = paste0("V",seq_len(141)), fill = TRUE, stringsAsFactors = FALSE)
              Lamina_FI <- Lamina_FI[,1:76]
              colnames(Lamina_FI) <- c('CNPJ_FUNDO',	'DENOM_SOCIAL',	'DT_COMPTC',	'NM_FANTASIA',	'ENDER_ELETRONICO',	'PUBLICO_ALVO',	'RESTR_INVEST',	'OBJETIVO',	'POLIT_INVEST',	'PR_PL_ATIVO_EXTERIOR',	'PR_PL_ATIVO_CRED_PRIV',	'PR_PL_ALAVANC',	'PR_ATIVO_EMISSOR',	'DERIV_PROTECAO_CARTEIRA',	'RISCO_PERDA',	'RISCO_PERDA_NEGATIVO',	'PR_PL_APLIC_MAX_FUNDO_UNICO',	'INVEST_INICIAL_MIN',	'INVEST_ADIC',	'RESGATE_MIN',	'HORA_APLIC_RESGATE',	'VL_MIN_PERMAN',	'QT_DIA_CAREN',	'CONDIC_CAREN',	'CONVERSAO_COTA_COMPRA',	'QT_DIA_CONVERSAO_COTA_COMPRA',	'CONVERSAO_COTA_CANC',	'QT_DIA_CONVERSAO_COTA_RESGATE',	'TP_DIA_PAGTO_RESGATE',	'QT_DIA_PAGTO_RESGATE',	'TP_TAXA_ADM',	'TAXA_ADM',	'TAXA_ADM_MIN',	'TAXA_ADM_MAX',	'TAXA_ADM_OBS',	'TAXA_ENTR',	'CONDIC_ENTR',	'QT_DIA_SAIDA',	'TAXA_SAIDA',	'CONDIC_SAIDA',	'TAXA_PERFM',	'PR_PL_DESPESA',	'DT_INI_DESPESA',	'DT_FIM_DESPESA',	'ENDER_ELETRONICO_DESPESA',	'VL_PATRIM_LIQ',	'CLASSE_RISCO_ADMIN',	'PR_RENTAB_FUNDO_5ANO',	'INDICE_REFER',	'PR_VARIACAO_INDICE_REFER_5ANO',	'QT_ANO_PERDA',	'DT_INI_ATIV_5ANO',	'ANO_SEM_RENTAB',	'CALC_RENTAB_FUNDO_GATILHO',	'PR_VARIACAO_PERFM',	'CALC_RENTAB_FUNDO',	'RENTAB_GATILHO',	'DS_RENTAB_GATILHO',	'ANO_EXEMPLO',	'ANO_ANTER_EXEMPLO',	'VL_RESGATE_EXEMPLO',	'VL_IMPOSTO_EXEMPLO',	'VL_TAXA_ENTR_EXEMPLO',	'VL_TAXA_SAIDA_EXEMPLO',	'VL_AJUSTE_PERFM_EXEMPLO',	'VL_DESPESA_EXEMPLO',	'VL_DESPESA_3ANO',	'VL_DESPESA_5ANO',	'VL_RETORNO_3ANO',	'VL_RETORNO_5ANO',	'REMUN_DISTRIB',	'DISTRIB_GESTOR_UNICO',	'CONFLITO_VENDA',	'TEL_SAC',	'ENDER_ELETRONICO_RECLAMACAO',	'INF_SAC')
              Lamina_FI_Carteira <- read.csv(unz(temp,paste0("lamina_fi_carteira_",data_alt,".csv")), sep = ";", stringsAsFactors = FALSE)
              Lamina_FI_Rentab_Ano <- read.csv(unz(temp,paste0("lamina_fi_rentab_ano_",data_alt,".csv")), sep = ";", stringsAsFactors = FALSE)
              Lamina_FI_Rentab_Mes <- read.csv(unz(temp,paste0("lamina_fi_rentab_mes_",data_alt,".csv")), sep = ";", stringsAsFactors = FALSE)
              unlink(temp)
              Lamina_FI <- Lamina_FI[,c("CNPJ_FUNDO",	"DENOM_SOCIAL",	"DT_COMPTC",	"PUBLICO_ALVO",	"OBJETIVO",	"POLIT_INVEST",	"RISCO_PERDA",	"RISCO_PERDA_NEGATIVO",	"INVEST_INICIAL_MIN",	"INVEST_ADIC",	"RESGATE_MIN",	"VL_MIN_PERMAN",	"QT_DIA_CONVERSAO_COTA_RESGATE",	"TP_TAXA_ADM",	"TAXA_ADM",	"TAXA_ADM_MIN",	"TAXA_ADM_MAX",	"TAXA_ADM_OBS",	"TAXA_ENTR",	"TAXA_PERFM",	"VL_PATRIM_LIQ",	"CLASSE_RISCO_ADMIN",	"INDICE_REFER")]
              Lamina_FI_Carteira <- Lamina_FI_Carteira[,c("CNPJ_FUNDO",	"DENOM_SOCIAL",	"DT_COMPTC",	"TP_ATIVO",	"PR_PL_ATIVO")]
              Lamina_FI_Rentab_Ano <- Lamina_FI_Rentab_Ano[,c("CNPJ_FUNDO",	"DENOM_SOCIAL",	"DT_COMPTC",	"ANO_RENTAB",	"PR_RENTAB_ANO", "PR_VARIACAO_INDICE_REFER_ANO")]
              Lamina_FI_Rentab_Mes <- Lamina_FI_Rentab_Mes[,c("CNPJ_FUNDO",	"DENOM_SOCIAL",	"DT_COMPTC",	"MES_RENTAB",	"PR_RENTAB_MES",	"PR_VARIACAO_INDICE_REFER_MES")]
              Lista <- list(Lamina_FI, Lamina_FI_Carteira, Lamina_FI_Rentab_Ano, Lamina_FI_Rentab_Mes)

              return(Lista)
            }
    )
  } else {
    warning("Tipos invalidos")
    warning("Validos apenas: fi_estruturado, fi_icvm555, fie,fie_adiministrador,fie_auditor,fie_gestor,auditor_pj,auditor_pf,fi,participante_intermediario_empresa,
                 participante_intermediario_responsavel,agente_autonomo_pj,agente_autonomo_pf, cias_incentivadas, cias_estrangeiras, cias_abertas,
                 representante_inv_n_residente_pj, representante_inv_n_residente_pf, consultor_pj, consultor_pf, consultor_diretor, consultor_socios,
                 administrador_fii, administrador_carteira_pj, administrador_carteira_pf, administrador_carteira_diretor, administrador_carteira_socios,
                 administrador_carteira_responsaveis")
  }
}

#DFP Anual
dados_cvm_dfp <- function(documento, ano, tipo) {
  url <- paste0('http://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/DFP/',documento,'/DADOS/',documento,"_cia_aberta_",ano,".zip")
  temp <- tempfile()
  download.file(url,temp)

  if (tipo == "consolidada") {
    Tabela <- read.csv(unz(temp, paste0(documento,"_cia_aberta_con_",ano,".csv")), sep = ";", stringsAsFactors = FALSE)
  } else if (tipo == "individual") {
    Tabela <- read.csv(unz(temp, paste0(documento,"_cia_aberta_ind_",ano,".csv")), sep = ";", stringsAsFactors = FALSE)
  } else {
    warning("Tipo nao identificado")
  }
  unlink(temp)
  if (documento == "dmpl") {
    Tabela <- Tabela[,c("CNPJ_CIA","DT_REFER","DENOM_CIA","GRUPO_DFP","MOEDA","ESCALA_MOEDA","COLUNA_DF","CD_CONTA","DS_CONTA","ORDEM_EXERC","VL_CONTA")]
    Temp <- Tabela[,c("CNPJ_CIA","DENOM_CIA","GRUPO_DFP","MOEDA","ESCALA_MOEDA","COLUNA_DF","CD_CONTA","DS_CONTA")]
    Temp <- unique(Temp)
  } else {
    Tabela <- Tabela[,c("CNPJ_CIA","DT_REFER","DENOM_CIA","GRUPO_DFP","MOEDA","ESCALA_MOEDA","CD_CONTA","DS_CONTA","ORDEM_EXERC","VL_CONTA")]
    Temp <- Tabela[,c("CNPJ_CIA","DENOM_CIA","GRUPO_DFP","MOEDA","ESCALA_MOEDA","CD_CONTA","DS_CONTA")]
    Temp <- unique(Temp)
  }
  Temp_antes <- unique(merge(Temp, Tabela[Tabela$ORDEM_EXERC == Tabela$ORDEM_EXERC[1],], all.x = TRUE))
  Temp_depois <- unique(merge(Temp, Tabela[Tabela$ORDEM_EXERC == Tabela$ORDEM_EXERC[2],], all.x = TRUE))
  Tabela <- dplyr::bind_cols(Temp, "ANTERIOR" = Temp_antes[,ncol(Temp_antes)], "ATUAL" = Temp_depois[,ncol(Temp_depois)])

  return(Tabela)
}
