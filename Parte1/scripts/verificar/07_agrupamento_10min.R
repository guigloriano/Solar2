library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)
library(lubridate)
library(chron)


merge_caminho <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\06_combina_tabelas\\"

# bloco para leitura do caminho que os arquivos combinados
# serao salvos
pathMerge <- setwd(merge_caminho)
pathMerge <- setwd(merge_caminho)
namesMerge <- list.files(pattern = "*.csv")
filesMerge <- paste(pathMerge,  "/", namesMerge, sep = "")


# agrupamento dos arquivos em registros de 1hr
caminho_agrupamento <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\07_agrupamento_10min\\"  

fi = 0 
dias <- 1
# dias <- 78

for(dias in 1:length(filesMerge)){ 
  
  # i = 1 
  
  mergeDataset <- readr::read_csv(filesMerge[dias], col_types = cols(hora_minuto = col_character()))
  
  g <- dplyr::group_by(mergeDataset, dia_mes_ano, h = substr(hora_minuto, 1, 2), 
                       m = floor(as.numeric(substr(hora_minuto, 3, 4))/10))
  
  gg <- dplyr::summarise(g, hora_minuto = dplyr::first(hora_minuto), 
                         
                         irr_inv = round(mean(IRR, na.rm=TRUE), digits = 2),
                         massaPM1 = round(mean(massaPM1, na.rm=TRUE), digits = 2), 
                         numPM1 = round(mean(numPM1, na.rm=TRUE), digits = 2), 
                         massaPM2 = round(mean(massaPM2, na.rm=TRUE), digits = 2), 
                         numPM2 = round(mean(numPM2, na.rm=TRUE), digits = 2), 
                         
                         tamanho_medio = round(mean(tamanho_medio, na.rm=TRUE), digits = 2),
                         temp = round(mean(temp, na.rm=TRUE), digits = 2),
                         vento_vel=round(mean(vento_vel, na.rm=TRUE), digits = 2),
                         vento_dir=round(mean(vento_dir, na.rm=TRUE), digits = 2), 
                         
                         P_AC = round(mean(P_AC, na.rm=TRUE), digits = 2),
                         P_DC = round(mean(P_DC, na.rm=TRUE), digits = 2),
                         
                         I_AC = round(mean(I_AC, na.rm=TRUE), digits = 2),
                         V_AC = round(mean(V_AC, na.rm=TRUE), digits = 2),
                         I_DC = round(mean(I_DC, na.rm=TRUE), digits = 2),
                         V_DC = round(mean(V_DC, na.rm=TRUE), digits = 2),
                         
                         massaPM4 = round(mean(massaPM4, na.rm=TRUE), digits = 2), 
                         numPM4 = round(mean(numPM4, na.rm=TRUE), digits = 2), 
                         massaPM10 = round(mean(massaPM10, na.rm=TRUE), digits = 2), 
                         numPM10 = round(mean(numPM10, na.rm=TRUE), digits = 2),
                         rainfall = max(rainfall, na.rm=TRUE),
                         irr_est = round(mean(irr, na.rm=TRUE), digits = 2), 
                         
                         #DVr1 =  round(atan  (sum( -vento_vel * sind(vento_dir) ) / sum( -vento_vel * cosd(vento_dir) ) ), digits = 6),
                         #DVr2 =  round(atan2 (sum( -vento_vel * sind(vento_dir) ) , sum( -vento_vel * cosd(vento_dir) ) ), digits = 6),
                         #IDV1 =  round(1 + sin(atan  (sum( -vento_vel * sind(vento_dir) ) / sum( -vento_vel * cosd(vento_dir) ) ) - fi ), digits = 6),#-(-0.03313127))   )
                         #IDV2 =  round(1 + sin(atan2 (sum( -vento_vel * sind(vento_dir) ) , sum( -vento_vel * cosd(vento_dir) ) ) - fi ), digits = 6),
                         
                         n = dplyr::n())
  
  y <- gg
  y$h <- NULL
  y$m <- NULL
  y$n <- NULL
  
  # remove os NA e os -Inf dos datasets forcando eles a 0
  is.na(y)<-sapply(y, is.infinite)
  y[is.na(y)] <- 0
  
#  if ( y$hora_minuto[1] > "000000")
#    y$hora_minuto[1] <- "000000"
  
  
  subAno <- substring(y$dia_mes_ano, 1, 4)
  subMes <- substring(y$dia_mes_ano, 5, 6)
  subDia <- substring(y$dia_mes_ano, 7, 8)
  
  y$hora_minuto <- str_pad(y$hora_minuto, 6, pad = "0")
  subHora <- substring(y$hora_minuto, 1, 2)
  subMin  <- substring(y$hora_minuto, 3, 4)
  subSeg  <- substring(y$hora_minuto, 5, 6)
  
  if (as.numeric(subMin[1]) >= 01 && as.numeric(subMin[1]) < 09 ){
      subMin[1] <- "00"
  }
    
  
  TempoAmostra <- paste(subAno, "-", subMes, "-", subDia, " " ,subHora, ":", subMin, ":", subSeg ,sep = "")
  y$Data <- strptime(TempoAmostra, format = "%Y-%m-%d %H:%M:%OS")
  
  aux_temp <- y$Data+540

  aux_hora <- substring(aux_temp, 12, 13)
  aux_min <- substring(aux_temp, 15, 16)
  aux_seg <- substring(aux_temp, 18, 19)
  
  aux_final <- paste(aux_hora, aux_min, aux_seg, sep = "")
  
  y$hora_minuto <- aux_final
  y$Data <- NULL
  
  salvarArq_name <- paste(caminho_agrupamento, "tab_10min_", mergeDataset$dia_mes_ano[1], ".csv", sep = "")
  write_csv(y, salvarArq_name)
  
}

















