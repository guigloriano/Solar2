#### LEITURA DOS DADOS DA ESTA��O #####


library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)
library(lubridate)
library(chron)

# caminhoDados <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\09_combinacaofinal\\reduzidas\\"

caminhoDados <- "D:\\Solar2\\Parte1\\datasets\\tabela_teste\\09_combinacaofinal\\reduzidas\\"

caminhoCSV <- setwd(caminhoDados)
caminhoCSV <- setwd(caminhoDados)
listaDados <- list.files(pattern = "*.csv")
csvDados <- paste(caminhoCSV,  "/", listaDados, sep = "")

dias <- 1
datasets_teste <- 0
csv_unificado <- c()

df_21 <- c()
df_42 <- c()
df_63 <- c()
df_84 <- c()
df_105 <- c()

for(dias in 1:length(listaDados)){ 
  # for(dias in 1:1){   
  dataset <- readr::read_csv(listaDados[dias], col_types = cols(hora_minuto = col_character()))
  
  dataset$X1 <-NULL
  dataset <- subset(dataset, dataset$irr_inv !=0)
  dataset$dia <- dias
  dataset$m <- round(dataset$m, digits=5)
  num_linhas <- nrow(dataset)
  
  linha <- 1
  for(linha in 1:nrow(dataset)){
    dataset$TIME[linha] <- linha
  }
  
  amostras <- nrow(dataset)
  csv_unificado <- rbind(csv_unificado, dataset)
  
  if (dias > 12 && dias <= 33){  
    df_21 <- rbind(df_21, dataset)
  }
  if (dias > 33 && dias <= 54){
    df_42 <- rbind(df_42, dataset)
  }
  if (dias > 54 && dias <= 75){
    df_63 <- rbind(df_63, dataset)
  }
  if (dias > 75 && dias <= 96){
    df_84 <- rbind(df_84, dataset)
  }
  if (dias > 96 && dias <= 117){
    df_105 <- rbind(df_105, dataset)
  }
  
}

df_21 <- as.data.frame(df_21)
df_42 <- as.data.frame(df_42)
df_63 <- as.data.frame(df_63)
df_84 <- as.data.frame(df_84)
df_105 <- as.data.frame(df_105)





##### LEITURA DOS DADOS DO INMET #####


#install.packages("chron")
# load it
library(chron)
# make dummy data

library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)

tab_INMET <- "D:\\Solar2\\Parte1\\datasets\\analysis\\preciptacao_inmet\\"

# bloco para leitura dos arquivos .csvs das tabelas combinadas de esta��o e inversor
cam_INMET   <- setwd(tab_INMET)
cam_INMET   <- setwd(tab_INMET)
lista_INMET <- list.files(pattern = "*.csv")
arq_INMET   <- paste(cam_INMET,  "/", lista_INMET, sep = "")



df_INMET <- readr::read_csv(arq_INMET, col_types = cols(data = col_character()))
df_aux <- df_INMET



library(lubridate)
df_aux$data <- dmy(df_aux$data)


# df$ddate <- format(as.Date(df$ddate), "%d/%m/%Y")
df_aux$data <- format(as.Date(df_aux$data), "%Y%m%d")



###### COMBINANDO AS DUAS INFORMA��ES ####

names(df_aux)[names(df_aux) == "data"] <- "dia_mes_ano"

df_21 = merge(x = df_21, y = df_aux, by = "dia_mes_ano", all.x = TRUE)

df_42 = merge(x = df_42, y = df_aux, by = "dia_mes_ano", all.x = TRUE)
df_63 = merge(x = df_63, y = df_aux, by = "dia_mes_ano", all.x = TRUE)
df_84 = merge(x = df_84, y = df_aux, by = "dia_mes_ano", all.x = TRUE)
df_105 = merge(x = df_105, y = df_aux, by = "dia_mes_ano", all.x = TRUE)






##### GERANDO ARQUIVOS PARA SALVAR ######

df = as.data.frame(csv_unificado)
#teste <- df[, c(17, 18, 4, 10, 16, 13, 2, 3, 5, 6, 7, 8, 9, 11, 12, 14, 15)]
newdf <- df[, c("dia", "TIME", "irr_inv", "temp", "m", "P_DC", "dia_mes_ano", "hora_minuto",
                "massaPM1", "numPM1", "massaPM2", "numPM2", "tamanho_medio", "vento_vel",
                "vento_dir", "I_DC", "V_DC")]

# 1X1            2dia_mes_ano         3hora_minuto         4irr_inv	
# 5massaPM1      6numPM1              7massaPM2            8numPM2	
# 9tamanho_medio 10temp               11vento_vel          12vento_dir	
# 13P_DC         14I_DC	              15V_DC               16m	
# 17dia          18TIME


df_21 <- df_21[, c("dia", "TIME", "irr_inv", "temp", "m", "P_DC", "precipitacao")]
df_42 <- df_42[, c("dia", "TIME", "irr_inv", "temp", "m", "P_DC", "precipitacao")]
df_63 <- df_63[, c("dia", "TIME", "irr_inv", "temp", "m", "P_DC", "precipitacao")]
df_84 <- df_84[, c("dia", "TIME", "irr_inv", "temp", "m", "P_DC", "precipitacao")]
df_105 <- df_105[, c("dia", "TIME", "irr_inv", "temp", "m", "P_DC", "precipitacao")]


names(df_21)[names(df_21) == "dia"] <- "DIA"
names(df_21)[names(df_21) == "irr_inv"] <- "IRR"
names(df_21)[names(df_21) == "temp"] <- "TEMP"
names(df_21)[names(df_21) == "m"] <- "MA"
names(df_21)[names(df_21) == "P_DC"] <- "PDC"
names(df_21)[names(df_21) == "precipitacao"] <- "PRECIP"

names(df_42)[names(df_42) == "dia"] <- "DIA"
names(df_42)[names(df_42) == "irr_inv"] <- "IRR"
names(df_42)[names(df_42) == "temp"] <- "TEMP"
names(df_42)[names(df_42) == "m"] <- "MA"
names(df_42)[names(df_42) == "P_DC"] <- "PDC"
names(df_42)[names(df_42) == "precipitacao"] <- "PRECIP"

names(df_63)[names(df_63) == "dia"] <- "DIA"
names(df_63)[names(df_63) == "irr_inv"] <- "IRR"
names(df_63)[names(df_63) == "temp"] <- "TEMP"
names(df_63)[names(df_63) == "m"] <- "MA"
names(df_63)[names(df_63) == "P_DC"] <- "PDC"
names(df_63)[names(df_63) == "precipitacao"] <- "PRECIP"

names(df_84)[names(df_84) == "dia"] <- "DIA"
names(df_84)[names(df_84) == "irr_inv"] <- "IRR"
names(df_84)[names(df_84) == "temp"] <- "TEMP"
names(df_84)[names(df_84) == "m"] <- "MA"
names(df_84)[names(df_84) == "P_DC"] <- "PDC"
names(df_84)[names(df_84) == "precipitacao"] <- "PRECIP"

names(df_105)[names(df_105) == "dia"] <- "DIA"
names(df_105)[names(df_105) == "irr_inv"] <- "IRR"
names(df_105)[names(df_105) == "temp"] <- "TEMP"
names(df_105)[names(df_105) == "m"] <- "MA"
names(df_105)[names(df_105) == "P_DC"] <- "PDC"
names(df_105)[names(df_105) == "precipitacao"] <- "PRECIP"



##### CORRE��O DA NUMERA��O DOS DIAS #####
df_21$DIA <- df_21$DIA - 12

#df_42$DIA <- df_42$DIA - 33
df_42$DIA <- df_42$DIA - 12

#df_63$DIA <- df_63$DIA - 54
df_63$DIA <- df_63$DIA - 12

#df_84$DIA <- df_84$DIA - 75
df_84$DIA <- df_84$DIA - 12

#df_105$DIA <- df_105$DIA - 96
df_105$DIA <- df_105$DIA - 12


###### VERIFICA��O DA CHUVA POR DIA #####

df_21Aux <- distinct(df_21,DIA, .keep_all= TRUE)
df_42Aux <- distinct(df_42,DIA, .keep_all= TRUE)
df_63Aux <- distinct(df_63,DIA, .keep_all= TRUE)
df_84Aux <- distinct(df_84,DIA, .keep_all= TRUE)
df_105Aux <- distinct(df_105,DIA, .keep_all= TRUE)

max(df_21Aux$PRECIP)
max(df_42Aux$PRECIP)
max(df_63Aux$PRECIP)
max(df_84Aux$PRECIP)
max(df_105Aux$PRECIP)


##### SALVAR OS ARQUIVOS #####

# caminho_salvar <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\"

caminho_salvar <- "D:\\Solar2\\Parte1\\datasets\\tabela_teste\\10_\\"

fileDest <- paste(caminho_salvar,  "/1_21.csv", sep = "")
write.csv(df_21, fileDest) 

fileDest <- paste(caminho_salvar,  "/22_42.csv", sep = "")
write.csv(df_42, fileDest) 

fileDest <- paste(caminho_salvar,  "/43_63.csv", sep = "")
write.csv(df_63, fileDest) 

fileDest <- paste(caminho_salvar,  "/64_84.csv", sep = "")
write.csv(df_84, fileDest) 

fileDest <- paste(caminho_salvar,  "/85_105.csv", sep = "")
write.csv(df_105, fileDest) 


