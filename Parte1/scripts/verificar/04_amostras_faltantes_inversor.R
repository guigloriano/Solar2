library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)

#caminhoDados <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\inv\\"
caminhoDados <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\05_interpola_inv\\"

caminhoCSV <- setwd(caminhoDados)
caminhoCSV <- setwd(caminhoDados)
listaDados <- list.files(pattern = "*.csv")
csvDados <- paste(caminhoCSV,  "/", listaDados, sep = "")
dias <- 1
amostras_geral <- c()


for(dias in 1:length(listaDados)){ 
  
  dataset <- readr::read_csv(listaDados[dias], col_types = cols(hora_minuto = col_character()))
  amostras <- nrow(dataset)
  dia <- dataset$dia_mes_ano[1]
  amostras_faltantes <- c()
  dataset$X1 <- NULL
  linha_atual <- 1
  
  subAno <- substring(dataset$dia_mes_ano, 1, 4)
  subMes <- substring(dataset$dia_mes_ano, 5, 6)
  subDia <- substring(dataset$dia_mes_ano, 7, 8)
  
  dataset$hora_minuto <- str_pad(dataset$hora_minuto, 6, pad = "0")
  subHora <- substring(dataset$hora_minuto, 1, 2)
  subMin  <- substring(dataset$hora_minuto, 3, 4)
  subSeg  <- substring(dataset$hora_minuto, 5, 6)
  
  TempoAmostra <- paste(subAno, "-", subMes, "-", subDia, " " ,subHora, ":", subMin, ":", subSeg ,sep = "")
  dataset$Data <- strptime(TempoAmostra, format = "%Y-%m-%d %H:%M:%OS")
  
  dataset <- dataset[, c(9, 1, 2, 3, 4, 5, 6, 7, 8)]
  
  if (amostras > 2){
    while (linha_atual < amostras){
      
      amostras_faltantes_aux <- c()
      dif_hora <- as.numeric(difftime(dataset$Data[linha_atual+1],dataset$Data[linha_atual]))
      
      # registra se ha diferenca acima de 2 minutos em cada amostra diaria
      if ( dif_hora >= 2 )
      {
        amostras_faltantes_aux$dia <- dataset$dia_mes_ano[linha_atual]
        amostras_faltantes_aux$hora <- dataset$hora_minuto[linha_atual]
        amostras_faltantes_aux$hora_1 <- dataset$hora_minuto[linha_atual+1]
        amostras_faltantes_aux$amostras <- amostras
        amostras_faltantes_aux$linha <- linha_atual
        amostras_faltantes_aux$diferenca <- as.numeric(difftime(dataset$Data[linha_atual+1],dataset$Data[linha_atual]))
      }
      
      amostras_faltantes <- bind_rows(amostras_faltantes, amostras_faltantes_aux)
      linha_atual <- linha_atual+1
    }
    amostras_geral <- rbind(amostras_geral, amostras_faltantes)
  }
}


dataset$X1 <- NULL
# salvar o contador de amostras com +3 minutos de diferença
write.csv(amostras_geral,'D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\Inversor v1.0 (amostras faltantes).csv')



