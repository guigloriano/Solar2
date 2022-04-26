
library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)

tab_EstInv <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\07_agrupamento_10min\\"
tab_Macc   <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\08_quebraDataset\\"
dest_comb  <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\09_combinacaofinal\\"

# bloco para leitura dos arquivos .csvs das tabelas combinadas de estação e inversor
cam_EstInv   <- setwd(tab_EstInv)
cam_EstInv   <- setwd(tab_EstInv)
lista_EstInv <- list.files(pattern = "*.csv")
arq_EstInv   <- paste(cam_EstInv,  "/", lista_EstInv, sep = "")

# bloco para leitura dos arquivos .csvs da massa acumulada
cam_Macc   <- setwd(tab_Macc)
cam_Macc   <- setwd(tab_Macc)
lista_Macc <- list.files(pattern = "*.csv")
arq_Macc   <- paste(cam_Macc, "/", lista_Macc, sep = "")

# bloco para leitura do caminho que os arquivos combinados
# serao salvos
cam_Merge <- setwd(dest_comb)
cam_Merge <- setwd(dest_comb)
lista_arq_Merge <- list.files(pattern = "*.csv")
arq_Merge <- paste(cam_Merge,  "/", lista_arq_Merge, sep = "")

# este codigo combina as tabelas do Inversor e Estacao 
# de minuto em minuto em um único arquivo

i = 1
for(i in 1:length(arq_EstInv)){ 
  #  i = 1
  dfEstInv <- arq_EstInv[i]
  dfMacc <- arq_Macc[i]
  
  x_tab <- readr::read_csv(dfEstInv, col_types = cols(hora_minuto = col_character()))
  y_m <- readr::read_csv(dfMacc, col_types = cols(hora_minuto = col_character()))
  y_m$X1 <- NULL
  
  dia <- x_tab$dia_mes_ano[1]
  
  x_tab$hora_minuto <- str_pad(x_tab$hora_minuto, width=6, side="left", pad="0")
  x_tab$hora_minuto <- as.character(x_tab$hora_minuto)
  
  y_m$hora_minuto <- str_pad(y_m$hora_minuto, width=6, side="left", pad="0")
  y_m$hora_minuto <- as.character(y_m$hora_minuto)
  
  z_merge <- merge.data.frame(x = x_tab, y = y_m, all = TRUE)
  
  
  z_merge <- z_merge[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 29, 12, 14, 15, 16, 17, 18,
                         19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 30)]
  
  fileDest <- paste(cam_Merge,  "/completas/tabelas_completas_", dia, ".csv", sep = "")
  write_csv(z_merge, fileDest)
  
  z1_merge <- z_merge[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 17, 18, 29 )]
  
  fileDest <- paste(cam_Merge,  "/reduzidas/tabelas_reduzidas_", dia, ".csv", sep = "")
  write_csv(z1_merge, fileDest)
}

