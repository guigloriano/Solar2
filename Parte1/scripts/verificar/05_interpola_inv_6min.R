library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)
library(lubridate)
library(chron)
options(scipen = 999)

caminhoDados <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\05_interpola_inv\\"
caminhoCSV <- setwd(caminhoDados)
caminhoCSV <- setwd(caminhoDados)
listaDados <- list.files(pattern = "*.csv")
csvDados <- paste(caminhoCSV,  "/", listaDados, sep = "")

dias <- 1
amostras_geral <- c()

for(dias in 1:length(listaDados)){ 
  # for(dias in 1:1){   
  dataset <- readr::read_csv(listaDados[dias], col_types = cols(hora_minuto = col_number()))
  
  dataset$X1 <- NULL
  dia <- dataset$dia_mes_ano[1]
  
  amostras <- nrow(dataset)
  linha_atual <- 1
  
  
  amostras_faltantes <- c()
  
  subAno <- substring(dataset$dia_mes_ano, 1, 4)
  subMes <- substring(dataset$dia_mes_ano, 5, 6)
  subDia <- substring(dataset$dia_mes_ano, 7, 8)
  
  dataset$hora_minuto <- str_pad(dataset$hora_minuto, 6, pad = "0")
  subHora <- substring(dataset$hora_minuto, 1, 2)
  subMin  <- substring(dataset$hora_minuto, 3, 4)
  subSeg  <- substring(dataset$hora_minuto, 5, 6)
  
  TempoAmostra <- paste(subAno, "-", subMes, "-", subDia, " " ,subHora, ":", subMin, ":", subSeg ,sep = "")
  dataset$Data <- strptime(TempoAmostra, format = "%Y-%m-%d %H:%M:%OS")
  
  # dia_mes_ano 1, hora_minuto 2, P_AC 3, I_AC 4, I_DC 5, V_AC 6, V_DC 7, IRR 8, Data 9
  dataset <- dataset[, c(9, 1, 2, 3, 4, 5, 6, 7, 8)]
  
  if (amostras >= 1){
    while (linha_atual < amostras){
      
      dataset$hora_minuto <- as.numeric(dataset$hora_minuto)
      #dif_hora <- dataset$hora_minuto[linha_atual+1] - dataset$hora_minuto[linha_atual]
      dif_hora2 <- as.numeric(difftime(dataset$Data[linha_atual+1],dataset$Data[linha_atual]))
      amostras_faltantes_aux <- c()
      
      if ( dif_hora2 > 1 ){
        amostras_faltantes_aux$dia <- dataset$dia_mes_ano[linha_atual]
        amostras_faltantes_aux$hora <- dataset$hora_minuto[linha_atual]
        amostras_faltantes_aux$hora_1 <- dataset$hora_minuto[linha_atual+1]
        amostras_faltantes_aux$amostras <- amostras
        amostras_faltantes_aux$linha <- linha_atual
        amostras_faltantes_aux$diferenca <- dif_hora2
      }
      
      #amostras_faltantes <- rbind(amostras_faltantes, amostras_faltantes_aux)
      amostras_faltantes <- bind_rows(amostras_faltantes, amostras_faltantes_aux)
      
      
      if (dif_hora2 == 6){
        #amostras <- amostras+1
        
        Data <- dataset$Data[linha_atual]
        dia_mes_ano = dataset$dia_mes_ano[1]
        hora_minuto = Data+240
        Data = Data + 240
        
        teste3aux <- substring(hora_minuto, 12, 13)
        teste4aux <- substring(hora_minuto, 15, 16)
        teste5aux <- substring(hora_minuto, 18, 19)
        hora_minuto <- paste(teste3aux, teste4aux, teste5aux, sep = "")
        
        # dia_mes_ano 1, hora_minuto 2, P_AC 3, I_AC 4, I_DC 5, V_AC 6, V_DC 7, IRR 8, Data 9
        P_AC = round ((dataset$P_AC[linha_atual]+dataset$P_AC[linha_atual+1])/2, digits = 2)
        I_AC = round ((dataset$I_AC[linha_atual]+dataset$I_AC[linha_atual+1])/2, digits = 2)
        I_DC = round ((dataset$I_DC[linha_atual]+dataset$I_DC[linha_atual+1])/2, digits = 2)
        V_AC = round ((dataset$V_AC[linha_atual]+dataset$V_AC[linha_atual+1])/2, digits = 2)
        V_DC = round ((dataset$V_DC[linha_atual]+dataset$V_DC[linha_atual+1])/2, digits = 2)
        IRR =    round ((dataset$IRR[linha_atual]+dataset$IRR[linha_atual+1])/2, digits = 2)
        
        tempLinha <-  data.frame(Data, dia_mes_ano, hora_minuto, P_AC, I_AC, I_DC, V_AC, V_DC, IRR)
        
        
        dataset <- rbind(dataset[1:linha_atual, ], tempLinha, dataset[linha_atual+1:nrow(dataset), ])
        
        
        linha_atual<-linha_atual+1
        amostras <- amostras+1
        
      }
      
      linha_atual<-linha_atual+1
    }
    amostras_geral <- rbind(amostras_geral, amostras_faltantes)
  }
  
  dataset$Data <- NULL
  dataset <- na.omit(dataset)
  # caminho para salvar os dados interpolados da estação de sujidade  
  caminho_salvar <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\05_interpola_inv\\"
  fileDest <- paste(caminho_salvar,  "/inv_inter_", dia, ".csv", sep = "")
  write.csv(dataset, fileDest)  
}












