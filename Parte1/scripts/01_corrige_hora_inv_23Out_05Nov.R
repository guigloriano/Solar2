# List files
library(dplyr)
library(magrittr)
library(readr)
library("stringr")
library(plyr)  


cam_horas <- setwd("D:\\Solar2\\Parte1\\datasets\\aa_originals\\inversor\\")
arq_horas <- list.files(pattern = "*.csv")

salvarArq_path <- "D:\\Solar2\\Parte1\\datasets\\analysis\\01_inversor_hour_ok\\"


for(i in 1:length(arq_horas)){
  assign(arq_horas[i],read.csv(arq_horas[i],skip=1, header=TRUE))
  dataset_atual <- readr::read_csv(arq_horas[i], col_types = cols(hora_minuto = col_character()))
  salvarArq_path <- "D:\\Solar2\\Parte1\\datasets\\analysis\\01_inversor_hour_ok\\"
  salvarArq_name <- paste(salvarArq_path, arq_horas[i], sep = "")
  write_csv(dataset_atual, salvarArq_name)
}



for(i in 20:33){ 
  assign(arq_horas[i],read.csv(arq_horas[i],skip=1, header=TRUE))
  dataset_atual <- readr::read_csv(arq_horas[i], col_types = cols(hora_minuto = col_character()))
  dataset_atual$hora_minuto <- str_pad(dataset_atual$hora_minuto, width=6, side="left", pad="0")

  t <- strptime(paste(dataset_atual$hora_minuto), "%H%M%S")
  t <- t - lubridate::as.period(1, unit = "hours")
  dataset_atual$hora_minuto <- as.numeric(strftime(t, "%H%M%S"))

  salvarArq_name <- paste(salvarArq_path, arq_horas[i], sep = "")
  write_csv(dataset_atual, salvarArq_name)

}
