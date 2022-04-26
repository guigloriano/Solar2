#######################################################################################
##
##    Variavel hora_quebra esta incrementando mais do que deveria.
##    esta operação deveria aumentar a quantidade de segundos por hora (3600)
##    mas está incrementando a quantidade diária (84600)
##    outro problema é que o conjunto de teste é feito a cada 10 minutos, ou seja
##    a variavel deveria incrementar 10 min * 60 seg = 600 
##
##
########################################################################################

library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)

pasta_estacao <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\05_interpola_est\\"

# bloco para leitura dos arquivos .csvs da estacao
caminho_estacao <- setwd(pasta_estacao)
caminho_estacao <- setwd(pasta_estacao)
nomes_arq_estacao <- list.files(pattern = "*.csv")
arquivos_estacao <- paste(caminho_estacao,  "/", nomes_arq_estacao, sep = "")


dataset_aux <- setNames(data.frame(matrix(ncol = 9, nrow = 1)), 
                        c("dia_mes_ano", "hora_minuto", "Vd1", "Pd", "Nloss", 
                          "Vd2", "m", "x_gauss", "SR" ))

dataset_save <- NULL
arq_final_diario <- c()

ListaVento <- NULL
ListaVentoMedia <- NULL
ListaMassa <- NULL
ListaMassaMedia <- NULL
ListaConcentracao <- NULL
ListaConcentracaoMedia <- NULL


UAux <- NULL
RaAux <- NULL
RbAux <- NULL
VsAux <- NULL
VdAux <- NULL


aux_dia <- NULL
hora_inicial <- NULL
z_merge_total <- NULL


dia_atual = 1
dia_inicio = 1

cont_dias <- NULL
hora_quebra <- 1


#dia_atual <- 78
#dia_inicio <- 8
#dia_final = 8 #106

#for (dia_atual in dia_inicio:dia_final){
for (dia_atual in dia_inicio:length(arquivos_estacao)){
#  cont_dias <- (dia_atual - 1)*24
  
  cont_dias <- dia_atual
  
  dataset_temp <- readr::read_csv(nomes_arq_estacao[dia_atual], col_types = cols(hora_minuto = col_character()))
  dataset_temp$X1 <- NULL
  
  amostras <- nrow(dataset_temp)
  linha <- 1
  temp10_minutos <- NULL
  
  dia <- dataset_temp$dia_mes_ano[1]
  
  
  
  for (linha in 1:amostras){
    if (as.numeric(dataset_temp$hora_minuto[linha]) %% 1000 != 0){
      temp10_minutos <- bind_rows(temp10_minutos, dataset_temp[linha,])
    }else{
      source("D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\08b_calculaSerieTemporal.R")
      

      temp10_minutos <- NULL
      temp10_minutos <- bind_rows(temp10_minutos, dataset_temp[linha,])
    }
    if (linha == amostras){
      source("D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\08b_calculaSerieTemporal.R")
    }
    
  
    

    
  }
  dataset_save <- rbind(dataset_save, dataset_aux)
  dataset_save <- na.omit(dataset_save)
  
  arq_final_diario <- filter(dataset_save, dataset_save$dia_mes_ano == dia)

  caminho_salvar <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\08_quebraDataset\\"
#  caminho_salvar <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\"
  fileDest <- paste(caminho_salvar,  "/teste_M_", dia, ".csv", sep = "")
  write.csv(arq_final_diario, fileDest)  
  
  

}






#horaCaminho <- "D:\\github\\Tabelas_DynamoDB\\csv\\teste_lm\\group"

# bloco para leitura dos arquivos .csvs do inversor
#pathHORA <- setwd(horaCaminho)
#pathHORA <- setwd(horaCaminho)
#namesHORA <- list.files(pattern = "*.csv")
#filesHORA <- paste(pathHORA,  "/", namesHORA, sep = "")

#aux_i <- 1
#aux_j <- 1
#aux_k <- length(filesHORA)


#dataset_HORA_aux <- NULL

#for (aux_i in aux_j:aux_k){
#  dataset_HORA <- readr::read_csv(namesHORA[aux_i], col_types = cols(hora_minuto = col_character()))
#  dataset_HORA_aux <- rbind(dataset_HORA_aux, dataset_HORA)
#}

#zy_merge <- merge(x = dataset_HORA_aux, y = dataset_save, by=c("dia_mes_ano", "hora_minuto"), all = TRUE)

#salvarArq_name <- paste("D:\\teste", ".csv", sep = "")
#write_csv(zy_merge, salvarArq_name)

#zy_merge <- na.omit(zy_merge)
#zy_merge[is.na(zy_merge)] <- 0

#modelo <- zy_merge$P_AC ~ zy_merge$irr_est + zy_merge$irr_inv + zy_merge$temp + zy_merge$numPM1 + zy_merge$massaPM1 + 
#  zy_merge$numPM2 + zy_merge$massaPM2 + zy_merge$vento_vel + zy_merge$vento_dir + zy_merge$DVr1 + zy_merge$DVr2 + 
#  zy_merge$IDV1 + zy_merge$IDV2 + zy_merge$Vd1 + zy_merge$Pd + zy_merge$Nloss + zy_merge$Vd2 + zy_merge$m + 
#  zy_merge$x_gauss + zy_merge$SR

#modelo <- zy_merge$P_AC ~ zy_merge$irr_inv + zy_merge$temp + zy_merge$numPM1 + zy_merge$massaPM1 + 
#  zy_merge$vento_vel + zy_merge$Vd1 + zy_merge$Pd + zy_merge$m + zy_merge$x_gauss + zy_merge$SR


#reg_linear <- lm(modelo, data = zy_merge, na.action=na.omit )  


#summary(reg_linear) 

#dfy_cor <- data.frame(zy_merge)
#dfy_cor$dia_mes_ano <- NULL
#dfy_cor$hora_minuto <- NULL
#dfy_cor$rainfall <- NULL
#dfy_cor$irr_est <- NULL
#dfy_cor$numPM2 <- NULL
#dfy_cor$massaPM2 <- NULL
#dfy_cor$vento_dir <- NULL
#dfy_cor$DVr1 <- NULL
#dfy_cor$DVr2 <- NULL
#dfy_cor$IDV1 <- NULL
#dfy_cor$IDV2 <- NULL
#dfy_cor$Nloss <- NULL
#dfy_cor$Vd2 <- NULL
#corr <- cor(dfy_cor[, ])
