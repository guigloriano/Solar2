library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)



caminhoDadosEstacao <- "D:\\Solar2\\Parte1\\datasets\\aa_originals\\estacao\\"
caminhoCSV <- setwd(caminhoDadosEstacao)
caminhoCSV <- setwd(caminhoDadosEstacao)
listaDadosEstacao <- list.files(pattern = "*.csv")
csvEstacao <- paste(caminhoCSV,  "/", listaDadosEstacao, sep = "")

diaEst <- 1
amostrasEst_Diaria <- c()
amostrasGeralEst <- data.frame(NULL)
for(diaEst in 1:length(listaDadosEstacao)){ 
  #for(dia in 1:2){   
  datasetEstacao <- readr::read_csv(listaDadosEstacao[diaEst], col_types = cols(hora_minuto = col_character()))
  linhasEstacao <- nrow(datasetEstacao)
  amostrasEst_Diaria$dia_estacao <- datasetEstacao$dia_mes_ano[1]
  amostrasEst_Diaria$amostras_estacao <- linhasEstacao
  amostrasGeralEst <- rbind(amostrasGeralEst, amostrasEst_Diaria)
}



caminhoDadosInversor <- "D:\\Solar2\\Parte1\\datasets\\analysis\\01_inversor_hour_ok\\"
caminhoInvCSV <- setwd(caminhoDadosInversor)
caminhoInvCSV <- setwd(caminhoDadosInversor)
listaDadosInversor <- list.files(pattern = "*.csv")
csvInversor <- paste(caminhoInvCSV,  "/", listaDadosInversor, sep = "")



diaInv <- 1
amostrasInv_Diaria <- c()
amostrasGeralInv <- data.frame(NULL)
for(diaInv in 1:length(listaDadosInversor)){ 
  #for(dia in 1:2){   
  datasetInversor <- readr::read_csv(listaDadosInversor[diaInv], col_types = cols(hora_minuto = col_character()))
  linhasInversor <- nrow(datasetInversor)
  amostrasInv_Diaria$dia_inversor <- datasetInversor$dia_mes_ano[1]
  amostrasInv_Diaria$amostras_inversor <- linhasInversor
  amostrasGeralInv <- rbind(amostrasGeralInv, amostrasInv_Diaria)
}



AnaliseAmostras <- cbind(amostrasGeralEst, amostrasGeralInv)


write_csv(AnaliseAmostras,'D:\\Solar2\\Parte1\\datasets\\Qtd_Amostras_Diaria_01.csv')

