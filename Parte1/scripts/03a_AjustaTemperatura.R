library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)
options(scipen=999)

caminhoDados <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\amb\\"

caminhoCSV <- setwd(caminhoDados)
caminhoCSV <- setwd(caminhoDados)
listaDados <- list.files(pattern = "*.csv")
csvDados <- paste(caminhoCSV,  "/", listaDados, sep = "")

dias <- 1
dia <- NULL
amostras_geral <- c()
relacao_dias_zeros <- c()

for(dias in 1:length(listaDados)){ 
  
  dataset <- readr::read_csv(listaDados[dias], col_types = cols(hora_minuto = col_number()))
  amostras <- nrow(dataset)
  dataset$X1 <- NULL
  
  if (amostras > 0){
    dia <- dataset$dia_mes_ano[1]
    linha_atual <- 1
    
    while (linha_atual < amostras){
      
      if(dataset$temp[linha_atual] <= -15 && dataset$temp[linha_atual+1] <= -15 ){
        dataset$temp[linha_atual] = dataset$temp[linha_atual-1]
        
      }
      
      if(dataset$temp[linha_atual] <= -15){
        dataset$temp[linha_atual] = round((dataset$temp[linha_atual-1] + dataset$temp[linha_atual+1])/2, digits = 2)
        
      }
      
      linha_atual <- linha_atual+1
    }
    
    dataset$X1 <- NULL
    fileDest <- paste("D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\03a_ambientais\\ambientais-ufms-", dia, ".csv", sep = "")
    write.csv(dataset, fileDest)
  }
} 



