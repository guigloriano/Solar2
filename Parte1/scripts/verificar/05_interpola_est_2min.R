library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)
library(lubridate)
library(chron)
options(scipen = 999)

caminhoDados <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\05_interpola_est\\"
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
  dataset <- na.omit(dataset)
  
  
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
  dataset <- dataset[, c(17, 1, 2, 3, 4, 5, 8, 9, 12, 13, 14, 15, 6, 7, 10, 11, 16)]
  
  if (amostras >= 1){
    while (linha_atual < amostras){
      
      dataset <- na.omit(dataset)
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
      
      
      if (dif_hora2 == 2){
        #amostras <- amostras+1
        
        Data <- dataset$Data[linha_atual]
        dia_mes_ano = dataset$dia_mes_ano[1]
        hora_minuto = Data+60
        Data = Data + 60
        
        teste3aux <- substring(hora_minuto, 12, 13)
        teste4aux <- substring(hora_minuto, 15, 16)
        teste5aux <- substring(hora_minuto, 18, 19)
        hora_minuto <- paste(teste3aux, teste4aux, teste5aux, sep = "")
        
        irr=        round ((dataset$irr[linha_atual]+dataset$irr[linha_atual+1])/2, digits = 2)
        massaPM1 =  round ((dataset$massaPM1[linha_atual]+dataset$massaPM1[linha_atual+1])/2, digits = 2)
        massaPM2 =  round ((dataset$massaPM2[linha_atual]+dataset$massaPM2[linha_atual+1])/2, digits = 2)
        massaPM4 =  round ((dataset$massaPM4[linha_atual]+dataset$massaPM4[linha_atual+1])/2, digits = 2)
        massaPM10 = round ((dataset$massaPM10[linha_atual]+dataset$massaPM10[linha_atual+1])/2, digits = 2)
        numPM1 =    round ((dataset$numPM1[linha_atual]+dataset$numPM1[linha_atual+1])/2, digits = 2)
        numPM2 =    round ((dataset$numPM2[linha_atual]+dataset$numPM2[linha_atual+1])/2, digits = 2)
        numPM4 =    round ((dataset$numPM4[linha_atual]+dataset$numPM4[linha_atual+1])/2, digits = 2)
        numPM10 =   round ((dataset$numPM10[linha_atual]+dataset$numPM10[linha_atual+1])/2, digits = 2)
        tamanho_medio = round ((dataset$tamanho_medio[linha_atual]+dataset$tamanho_medio[linha_atual+1])/2, digits = 2)
        temp =      round ((dataset$temp[linha_atual]+dataset$temp[linha_atual+1])/2, digits = 2)
        vento_dir = round ((dataset$vento_dir[linha_atual]+dataset$vento_dir[linha_atual+1])/2, digits = 2)
        vento_vel = round ((dataset$vento_vel[linha_atual]+dataset$vento_vel[linha_atual+1])/2, digits = 2)
        rainfall =  round ((dataset$rainfall[linha_atual]+dataset$rainfall[linha_atual+1])/2, digits = 2)
        
        tempLinha <-  data.frame(Data, dia_mes_ano, hora_minuto, irr, massaPM1, massaPM2, numPM1, numPM2, 
                                 tamanho_medio, temp, vento_dir, vento_vel, 
                                 massaPM4, massaPM10, numPM4, numPM10, rainfall)
        
        #dataset$Data <- NULL
        # dataset <- rbind(dataset[1:linha_atual,],tempLinha,dataset[-(1:linha_atual),])
        
        dataset <- na.omit(dataset)
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
  caminho_salvar <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\05_interpola_est\\"
  fileDest <- paste(caminho_salvar,  "/amb_inter_", dia, ".csv", sep = "")
  write.csv(dataset, fileDest)  
}

#dataset$X1 <- NULL
# salvar o contador de amostras com 8 minutos de diferença
# amostras_geral <- amostras_geral
# write.csv(amostras_geral,'D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\amb\\interpoladas\\AAA.csv')









