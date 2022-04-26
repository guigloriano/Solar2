library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)
library(lubridate)
library(chron)

caminhoDados <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\09_combinacaofinal\\reduzidas\\"

caminhoCSV <- setwd(caminhoDados)
caminhoCSV <- setwd(caminhoDados)
listaDados <- list.files(pattern = "*.csv")
csvDados <- paste(caminhoCSV,  "/", listaDados, sep = "")

dias <- 1
datasets_teste <- 0
# dias <- 98
csv_unificado <- c()
df_Outubro <- c()
df_Marco <- c()
df_SOutubro <- c()
df_SMarco <- c()

if (datasets_teste == 1){
    df_21 <- c()
    df_42 <- c()
    df_63 <- c()
    df_84 <- c()
    df_105 <- c()
}


for(dias in 1:length(listaDados)){ 
  # for(dias in 1:1){   
  dataset <- readr::read_csv(listaDados[dias], col_types = cols(hora_minuto = col_character()))
  
  dataset$X1 <-NULL
  dataset <- subset(dataset, dataset$irr_inv !=0)
  dataset$dia <- dias
  dataset$m <- round(dataset$m, digits=5)
  num_linhas <- nrow(dataset)

  linha <- 1
  for(linha in 1:nrow(dataset)){
    dataset$TIME[linha] <- linha
  }
  
  amostras <- nrow(dataset)
  csv_unificado <- rbind(csv_unificado, dataset)
  
  if (datasets_teste == 1){
      if (dias <= 21){  
        df_21 <- rbind(df_21, dataset)
      }
      if (dias > 21 && dias <= 42){
        df_42 <- rbind(df_42, dataset)
      }
      if (dias > 42 && dias <= 63){
        df_63 <- rbind(df_63, dataset)
      }
      if (dias > 63 && dias <= 84){
        df_84 <- rbind(df_84, dataset)
      }
      if (dias > 84 && dias <= 105){
        df_105 <- rbind(df_105, dataset)
      }
  }
  
  
  if (dias < 13 || dias > 35){
    df_SOutubro <- rbind(df_SOutubro, dataset)
  }
  
  if (dias > 13 && dias < 35){
    df_Outubro <- rbind(df_Outubro, dataset)
  }
  
  
  if (dias < 76 || dias > 98){
    df_SMarco <- rbind(df_SMarco, dataset)
  }
  
  if (dias > 76 && dias < 98){
    df_Marco <- rbind(df_Marco, dataset)
  }
  
}
  if (datasets_teste == 1){
      df_21 <- as.data.frame(df_21)
      df_42 <- as.data.frame(df_42)
      df_63 <- as.data.frame(df_63)
      df_84 <- as.data.frame(df_84)
      df_105 <- as.data.frame(df_105)
      
      
      df_42$DIA <- df_42$DIA - 21
  }

  

  df_Outubro$dia <- df_Outubro$dia - 13
  df_Marco$dia <- df_Marco$dia - 76
  
  df_Outubro = as.data.frame(df_Outubro)
  df_Marco = as.data.frame(df_Marco)
  
  df_SOutubro = as.data.frame(df_SOutubro)
  df_SMarco = as.data.frame(df_SMarco)
  
  
  
  df = as.data.frame(csv_unificado)
  #teste <- df[, c(17, 18, 4, 10, 16, 13, 2, 3, 5, 6, 7, 8, 9, 11, 12, 14, 15)]
  newdf <- df[, c("dia", "TIME", "irr_inv", "temp", "m", "P_DC", "dia_mes_ano", "hora_minuto",
                  "massaPM1", "numPM1", "massaPM2", "numPM2", "tamanho_medio", "vento_vel",
                  "vento_dir", "I_DC", "V_DC")]
  
  # 1X1            2dia_mes_ano         3hora_minuto         4irr_inv	
  # 5massaPM1      6numPM1              7massaPM2            8numPM2	
  # 9tamanho_medio 10temp               11vento_vel          12vento_dir	
  # 13P_DC         14I_DC	              15V_DC               16m	
  # 17dia          18TIME
  
  
  if (datasets_teste == 1){
      df_21 <- df_21[, c("dia", "TIME", "irr_inv", "temp", "m", "P_DC")]
      df_42 <- df_42[, c("dia", "TIME", "irr_inv", "temp", "m", "P_DC")]
      df_63 <- df_63[, c("dia", "TIME", "irr_inv", "temp", "m", "P_DC")]
      df_84 <- df_84[, c("dia", "TIME", "irr_inv", "temp", "m", "P_DC")]
      df_105 <- df_105[, c("dia", "TIME", "irr_inv", "temp", "m", "P_DC")]
  }
  
  
  
  
  df_Outubro <- df_Outubro[, c("dia", "TIME", "irr_inv", "temp", "m", "P_DC", "dia_mes_ano")]
                  #     , "hora_minuto", "massaPM1", "numPM1", "massaPM2", 
                   #     "numPM2", "tamanho_medio", "vento_vel", "vento_dir", "I_DC", "V_DC")]
  df_SOutubro <- df_SOutubro[, c("dia", "TIME", "irr_inv", "temp", "m", "P_DC", "dia_mes_ano")]
  df_SMarco <- df_SMarco[, c("dia", "TIME", "irr_inv", "temp", "m", "P_DC", "dia_mes_ano")]
  df_Marco <- df_Marco[, c("dia", "TIME", "irr_inv", "temp", "m", "P_DC", "dia_mes_ano")]
  
  
  
  if (datasets_teste == 1){
      names(df_21)[names(df_21) == "dia"] <- "DIA"
      names(df_21)[names(df_21) == "irr_inv"] <- "IRR"
      names(df_21)[names(df_21) == "temp"] <- "TEMP"
      names(df_21)[names(df_21) == "m"] <- "MA"
      names(df_21)[names(df_21) == "P_DC"] <- "PDC"
      
      names(df_42)[names(df_42) == "dia"] <- "DIA"
      names(df_42)[names(df_42) == "irr_inv"] <- "IRR"
      names(df_42)[names(df_42) == "temp"] <- "TEMP"
      names(df_42)[names(df_42) == "m"] <- "MA"
      names(df_42)[names(df_42) == "P_DC"] <- "PDC"
      
      names(df_63)[names(df_63) == "dia"] <- "DIA"
      names(df_63)[names(df_63) == "irr_inv"] <- "IRR"
      names(df_63)[names(df_63) == "temp"] <- "TEMP"
      names(df_63)[names(df_63) == "m"] <- "MA"
      names(df_63)[names(df_63) == "P_DC"] <- "PDC"
      
      names(df_84)[names(df_84) == "dia"] <- "DIA"
      names(df_84)[names(df_84) == "irr_inv"] <- "IRR"
      names(df_84)[names(df_84) == "temp"] <- "TEMP"
      names(df_84)[names(df_84) == "m"] <- "MA"
      names(df_84)[names(df_84) == "P_DC"] <- "PDC"
      
      names(df_105)[names(df_105) == "dia"] <- "DIA"
      names(df_105)[names(df_105) == "irr_inv"] <- "IRR"
      names(df_105)[names(df_105) == "temp"] <- "TEMP"
      names(df_105)[names(df_105) == "m"] <- "MA"
      names(df_105)[names(df_105) == "P_DC"] <- "PDC"
  
      df_42$DIA <- df_42$DIA - 21
      df_63$DIA <- df_63$DIA - 42
      df_84$DIA <- df_84$DIA - 63
      df_105$DIA <- df_105$DIA - 84
  } 
  
  names(df_Outubro)[names(df_Outubro) == "dia"] <- "DIA"
  names(df_Outubro)[names(df_Outubro) == "irr_inv"] <- "IRR"
  names(df_Outubro)[names(df_Outubro) == "temp"] <- "TEMP"
  names(df_Outubro)[names(df_Outubro) == "m"] <- "MA"
  names(df_Outubro)[names(df_Outubro) == "P_DC"] <- "PDC"
  names(df_Outubro)[names(df_Outubro) == "dia_mes_ano"] <- "data"
  
  names(df_SOutubro)[names(df_SOutubro) == "dia"] <- "DIA"
  names(df_SOutubro)[names(df_SOutubro) == "irr_inv"] <- "IRR"
  names(df_SOutubro)[names(df_SOutubro) == "temp"] <- "TEMP"
  names(df_SOutubro)[names(df_SOutubro) == "m"] <- "MA"
  names(df_SOutubro)[names(df_SOutubro) == "P_DC"] <- "PDC"
  names(df_SOutubro)[names(df_SOutubro) == "dia_mes_ano"] <- "data"
  
  names(df_Marco)[names(df_Marco) == "dia"] <- "DIA"
  names(df_Marco)[names(df_Marco) == "irr_inv"] <- "IRR"
  names(df_Marco)[names(df_Marco) == "temp"] <- "TEMP"
  names(df_Marco)[names(df_Marco) == "m"] <- "MA"
  names(df_Marco)[names(df_Marco) == "P_DC"] <- "PDC"
  names(df_Marco)[names(df_Marco) == "dia_mes_ano"] <- "data"
  
  
  names(df_SMarco)[names(df_SMarco) == "dia"] <- "DIA"
  names(df_SMarco)[names(df_SMarco) == "irr_inv"] <- "IRR"
  names(df_SMarco)[names(df_SMarco) == "temp"] <- "TEMP"
  names(df_SMarco)[names(df_SMarco) == "m"] <- "MA"
  names(df_SMarco)[names(df_SMarco) == "P_DC"] <- "PDC"
  names(df_SMarco)[names(df_SMarco) == "dia_mes_ano"] <- "data"

  
  
  
  
  
  caminho_salvar <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\"
  
  if (datasets_teste == 1){
      fileDest <- paste(caminho_salvar,  "/21.csv", sep = "")
      write.csv(df_21, fileDest) 
      
      fileDest <- paste(caminho_salvar,  "/42.csv", sep = "")
      write.csv(df_42, fileDest) 
      
      fileDest <- paste(caminho_salvar,  "/63.csv", sep = "")
      write.csv(df_63, fileDest) 
      
      fileDest <- paste(caminho_salvar,  "/84.csv", sep = "")
      write.csv(df_84, fileDest) 
      
      fileDest <- paste(caminho_salvar,  "/105.csv", sep = "")
      write.csv(df_105, fileDest) 
  }   
  
  
  
  fileDest <- paste(caminho_salvar,  "/TodosDias.csv", sep = "")
  write.csv(csv_unificado, fileDest) 

  fileDest <- paste(caminho_salvar,  "/df_Outubro.csv", sep = "")
  write.csv(df_Outubro, fileDest) 
  
  fileDest <- paste(caminho_salvar,  "/df_Marco.csv", sep = "")
  write.csv(df_Marco, fileDest) 
  
  fileDest <- paste(caminho_salvar,  "/df_SOutubro.csv", sep = "")
  write.csv(df_SOutubro, fileDest) 
  
  fileDest <- paste(caminho_salvar,  "/df_SMarco.csv", sep = "")
  write.csv(df_SMarco, fileDest) 
  
  