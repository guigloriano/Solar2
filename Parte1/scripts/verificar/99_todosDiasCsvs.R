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
csv_unificado <- c()

for(dias in 1:length(listaDados)){ 
  # for(dias in 1:1){   
  dataset <- readr::read_csv(listaDados[dias], col_types = cols(hora_minuto = col_character()))
  dataset$X1 <-NULL
  
  amostras <- nrow(dataset)
  
  csv_unificado <- rbind(csv_unificado, dataset)
  
  
  
}

caminho_salvar <- "C:\\Users\\Guilherme\\Desktop\\"
fileDest <- paste(caminho_salvar,  "/todos_dias2.csv", sep = "")
write.csv(csv_unificado, fileDest) 


