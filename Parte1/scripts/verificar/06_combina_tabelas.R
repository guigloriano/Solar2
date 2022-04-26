library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)
library(lubridate)
library(chron)

inv_caminho <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\05_interpola_inv\\"
est_caminho <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\05_interpola_est\\"
merge_caminho <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\06_combina_tabelas\\"

# bloco para leitura dos arquivos .csvs do inversor
pathInv <- setwd(inv_caminho)
pathInv <- setwd(inv_caminho)
namesInv <- list.files(pattern = "*.csv")
filesInv <- paste(pathInv,  "/", namesInv, sep = "")


# bloco para leitura dos arquivos .csvs da estacao
pathSta <- setwd(est_caminho)
pathSta <- setwd(est_caminho)
namesSta <- list.files(pattern = "*.csv")
filesSta <- paste(pathSta, "/", namesSta, sep = "")

# este codigo combina as tabelas do Inversor e Estacao 
# de minuto em minuto em um único arquivo



#dias = 1
for(dias in 1:length(filesInv)){ 

  dfestacao <- filesSta[dias]
  dfinversor <- filesInv[dias]
  
  x_est <- readr::read_csv(dfestacao, col_types = cols(hora_minuto = col_character()))
  x_est$X1 <- NULL
  
  y_inv <- readr::read_csv(dfinversor, col_types = cols(hora_minuto = col_character()))
  y_inv$X1 <- NULL
  
  x_est$hora_minuto <- str_pad(x_est$hora_minuto, width=6, side="left", pad="0")
#  x_est$hora_minuto <- as.character(x_est$hora_minuto)
  
  y_inv$hora_minuto <- str_pad(y_inv$hora_minuto, width=6, side="left", pad="0")
#  y_inv$hora_minuto <- as.character(y_inv$hora_minuto)
  
#  z_merge <- merge.data.frame(x = x_est, y = y_inv, all = TRUE)
  z_merge <- merge.data.frame(x = x_est, y = y_inv,   all.x = TRUE)

# num_linhas <- nrow(z_merge)
  
  # calculo da potencia dc
  z_merge$P_DC = round(z_merge$I_DC * z_merge$V_DC, digits = 2)
  dia <- z_merge$dia_mes_ano[1]
  
  
  fileDest <- paste("D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\06_combina_tabelas\\tab_combinadas_", dia, ".csv", sep = "")
  write_csv(z_merge, fileDest)
  
}
