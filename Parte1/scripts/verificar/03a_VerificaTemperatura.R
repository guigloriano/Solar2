library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)

options(scipen=999)

#este caminho faz referencia aos .csvs na pasta do Node
#caminhoDados <- "D:\\github\\Tabelas_DynamoDB\\nodeJS\\csv_novo\\amb-novo\\"

# CSVs originais para serem testados
#caminhoDados <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\amb\\"

# Verificar se a correção da temperatura deu certo
caminhoDados <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\03a_ambientais\\"

caminhoCSV <- setwd(caminhoDados)
caminhoCSV <- setwd(caminhoDados)
listaDados <- list.files(pattern = "*.csv")
csvDados <- paste(caminhoCSV,  "/", listaDados, sep = "")

dias <- 1
amostras_geral <- c()
relacao_dias_zeros <-c()

for(dias in 1:length(listaDados)){ 
  
  dataset <- readr::read_csv(listaDados[dias], col_types = cols(hora_minuto = col_number()))
  amostras <- nrow(dataset)
  amostras_faltantes <- c()
  dataset$X1 <- NULL
  
  #  if (amostras == 0){
  #    DiaZero <- substring(listaDados[dias], 17, 24)
  #    relacao_dias_zeros <- rbind(relacao_dias_zeros, DiaZero)
  #  }
  
  
  if (amostras > 0){
    dia <- dataset$dia_mes_ano[1]
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
    
    dataset <- dataset[, c(17, 1, 2, 3, 4, 5, 8, 9, 12, 13, 14, 15, 6, 7, 10, 11, 16)]
    
    while (linha_atual < amostras){
      
      amostras_faltantes_aux <- c()
      if(dataset$temp[linha_atual] == -127 && dataset$temp[linha_atual+1] == -127)
      {
        amostras_faltantes_aux$dia <- dataset$dia_mes_ano[linha_atual]
        amostras_faltantes_aux$hora <- dataset$hora_minuto[linha_atual]
        amostras_faltantes_aux$temp <- dataset$temp[linha_atual]
        amostras_faltantes_aux$linha <- linha_atual
      }

      if(dataset$temp[linha_atual] == -127)
      {
        amostras_faltantes_aux$dia <- dataset$dia_mes_ano[linha_atual]
        amostras_faltantes_aux$hora <- dataset$hora_minuto[linha_atual]
        amostras_faltantes_aux$temp <- round((dataset$temp[linha_atual-1]+dataset$temp[linha_atual+1])/2, digits = 2)
        amostras_faltantes_aux$linha <- linha_atual
        
      }
      
      if(dataset$temp[linha_atual] <= -1)
      {
        amostras_faltantes_aux$dia <- dataset$dia_mes_ano[linha_atual]
        amostras_faltantes_aux$hora <- dataset$hora_minuto[linha_atual]
        amostras_faltantes_aux$temp <- dataset$temp[linha_atual]
        amostras_faltantes_aux$linha <- linha_atual
      } 
      
      if(dataset$temp[linha_atual] >= 50)
      {
        amostras_faltantes_aux$dia <- dataset$dia_mes_ano[linha_atual]
        amostras_faltantes_aux$hora <- dataset$hora_minuto[linha_atual]
        amostras_faltantes_aux$temp <- dataset$temp[linha_atual]
        amostras_faltantes_aux$linha <- linha_atual
      } 
        
        
      amostras_faltantes <- bind_rows(amostras_faltantes, amostras_faltantes_aux)
      linha_atual <- linha_atual+1
    }
    amostras_geral <- rbind(amostras_geral, amostras_faltantes)
  }
} 
dataset$X1 <- NULL
# salvar o contador de amostras com +3 minutos de diferença
write.csv(amostras_geral,'D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\z03_VerifTemp-20200502b.csv')

#amostras_nulas <- data.frame(relacao_dias_zeros)
#write.csv(amostras_nulas,'D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\Estacao-AmostrasZeradas.csv')




