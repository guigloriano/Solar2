library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)

#dados com dias já separados e temperatura corrigida
est_caminho <- "D:\\Solar2\\Parte1\\datasets\\aa_originals\\estacao\\"
inv_caminho <- "D:\\Solar2\\Parte1\\datasets\\analysis\\01_inversor_hour_ok\\"   
comb_caminho <- "D:\\Solar2\\Parte1\\datasets\\analysis\\02_merge_tables\\"



# bloco para leitura dos arquivos .csvs do inversor
cam_Inv <- setwd(inv_caminho)
cam_Inv <- setwd(inv_caminho)
lista_arq_Inv <- list.files(pattern = "*.csv")
arq_Inv <- paste(cam_Inv,  "/", lista_arq_Inv, sep = "")



# bloco para leitura dos arquivos .csvs da estacao
cam_Est <- setwd(est_caminho)
cam_Est <- setwd(est_caminho)
lista_arq_Est <- list.files(pattern = "*.csv")
arq_Est <- paste(cam_Est, "/", lista_arq_Est, sep = "")



# bloco para leitura do caminho que os arquivos combinados
# serao salvos
cam_Merge <- setwd(comb_caminho)
cam_Merge <- setwd(comb_caminho)
lista_arq_Merge <- list.files(pattern = "*.csv")
arq_Merge <- paste(cam_Merge,  "/", lista_arq_Merge, sep = "")


# este codigo combina as tabelas do Inversor e Estacao 
# de minuto em minuto em um único arquivo

for(i in 1:length(arq_Inv)){ 
  #  i = 1
  dfestacao <- arq_Est[i]
  dfinversor <- arq_Inv[i]
  
  x_est <- readr::read_csv(dfestacao, col_types = cols(hora_minuto = col_character()))
  y_inv <- readr::read_csv(dfinversor, col_types = cols(hora_minuto = col_character()))
  
  x_est$hora_minuto <- str_pad(x_est$hora_minuto, width=6, side="left", pad="0")
  x_est$hora_minuto <- as.character(x_est$hora_minuto)
  
  y_inv$hora_minuto <- str_pad(y_inv$hora_minuto, width=6, side="left", pad="0")
  y_inv$hora_minuto <- as.character(y_inv$hora_minuto)
  
  z_merge <- merge.data.frame(x = x_est, y = y_inv, all = TRUE)
  
  # calculo da potencia dc
  z_merge$P_DC = z_merge$I_DC * z_merge$V_DC
  z_merge$P_DC = round (z_merge$P_DC, 2)
  dia <- z_merge$dia_mes_ano[1]
  
  z_merge[is.na(z_merge)] <- 0
  z_merge$X1 <- NULL
  
  fileDest <- paste(cam_Merge,  "/merge_", dia, ".csv", sep = "")
  write_csv(z_merge, fileDest)
  
}

