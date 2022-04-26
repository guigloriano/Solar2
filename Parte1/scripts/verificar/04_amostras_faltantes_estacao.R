library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)

options(scipen=999)

#caminhoDados <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\amb\\removidos\\"
caminhoDados <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\amb\\"


#caminhoDados <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\05_interpola_est\\"

caminhoCSV <- setwd(caminhoDados)
caminhoCSV <- setwd(caminhoDados)
listaDados <- list.files(pattern = "*.csv")
csvDados <- paste(caminhoCSV,  "/", listaDados, sep = "")

dias <- 1
#dias <- 71
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
    #    linha_atual <- 524
    
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
      dif_hora <- as.numeric(difftime(dataset$Data[linha_atual+1],dataset$Data[linha_atual]), units = "mins")

      # registra se ha diferenca acima de 2 minutos em cada amostra diaria
      if ( dif_hora >= 2 )
      {
        

        amostras_faltantes_aux$dia <- dataset$dia_mes_ano[linha_atual]
        amostras_faltantes_aux$hora <- dataset$hora_minuto[linha_atual]
        amostras_faltantes_aux$hora_1 <- as.numeric(dataset$hora_minuto[linha_atual+1])
        amostras_faltantes_aux$amostras <- amostras
        amostras_faltantes_aux$linha <- linha_atual
        amostras_faltantes_aux$diferenca <- as.numeric(difftime(dataset$Data[linha_atual+1],dataset$Data[linha_atual]), units = "mins")
      }
        
      amostras_faltantes <- bind_rows(amostras_faltantes, amostras_faltantes_aux)
      linha_atual <- linha_atual+1
    }
    amostras_geral <- rbind(amostras_geral, amostras_faltantes)
  }
} 
dataset$X1 <- NULL
# salvar o contador de amostras com +3 minutos de diferença
write.csv(amostras_geral,'D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\z04_AmostrasComFalhas[2]-20200504.csv')

#amostras_nulas <- data.frame(relacao_dias_zeros)
#write.csv(amostras_nulas,'D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\Estacao-AmostrasZeradas.csv')




