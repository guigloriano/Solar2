library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)

tab_INMET <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\preciptacao_inmet\\originais_mes\\"

# bloco para leitura dos arquivos .csvs das tabelas combinadas de estação e inversor
cam_INMET   <- setwd(tab_INMET)
cam_INMET   <- setwd(tab_INMET)
lista_INMET <- list.files(pattern = "*.csv")
arq_INMET   <- paste(cam_INMET,  "/", lista_INMET, sep = "")

lista_dias <- data.frame(NULL)

i = 1

for(i in 1:length(arq_INMET)){ 
  #  i = 1
  df_INMET <- arq_INMET[i]

  x_INMET <- readr::read_csv(df_INMET, col_types = cols(data = col_character()))
  
  
  g <- dplyr::group_by(x_INMET, data)
  
  gg <- dplyr::summarise(g, precipitacao = round(sum(precipitacao, na.rm=TRUE), digits=2))
  
  lista_dias <- rbind(lista_dias, gg)
  
}

write_csv(lista_dias, "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\preciptacao_inmet\\Relacao_por_dia.csv")
