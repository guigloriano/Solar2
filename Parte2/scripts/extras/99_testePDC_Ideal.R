
#### Análise de dados - Ricardo #####

## http://sia.webpopix.org/nlme.html

if (1 == 1){
  rm(list=ls(all=TRUE))
  set.seed(19)
  
  library(compiler)
  enableJIT(3)
  
  library(lme4)
  
  #remove.packages("rlang")
  #install.packages("rlang")
  library(tidyverse)  
  
  library(gdata)
  library(nlme)
  library(RVAideMemoire)
  library(readODS)
  library(car)
  require(MASS)
  library(predictmeans)
  library(ggplot2)
  library(ggpubr)
  library(nlraa)
  
}

#### Leitura dos dados ####
#dados <- read_ods("/Users/erlandison/Desktop/Dados_Ricardo/Dados/D121A.ods") 
#dados <- read_ods("C:\\Users\\Guilherme\\Desktop\\Script_Artigos\\D121A.ods") 



#### Ler os Datasets ####
dados1 <- read.csv("D:\\Mestrado\\Em Desenvolvimento\\csvs\\1_21.csv") 
dados2 <- read.csv("D:\\Mestrado\\Em Desenvolvimento\\csvs\\22_42.csv") 
dados3 <- read.csv("D:\\Mestrado\\Em Desenvolvimento\\csvs\\43_63.csv") 
dados4 <- read.csv("D:\\Mestrado\\Em Desenvolvimento\\csvs\\64_84.csv") 
dados5 <- read.csv("D:\\Mestrado\\Em Desenvolvimento\\csvs\\85_105.csv") 


#### Agrupar os Datasets ####
dados2$DIA <- dados2$DIA + 21 
dados3$DIA <- dados3$DIA + 42 
dados4$DIA <- dados4$DIA + 63 
dados5$DIA <- dados5$DIA + 84 
dadosTotal <- rbind(dados1, dados2, dados3, dados4, dados5)
dadosTotal$X <- NULL

#### Selecionar os 50 Primeiros Dias ####
# dados50 <- dadosTotal[!(dadosTotal$DIA > 50),]
# dados50 <- dados1

dados50 <- dadosTotal
dados50$X <- NULL
dados50 <- na.omit(dados50)

#### Remover os PDCs = 0, para não ter problema no modelo Logístico ####
dados50 <- dados50[!(dados50$PDC == 0),]


#### Início da manipulação do Dataset ###
str(dados50)
D2 <- data.frame(dados50)
n2 <- nrow(D2)

#### Calculo da Irradiação, PDC e Temp acumuladas ####

id.fi2 <- cumsum(as.numeric(ftable(D2[,1]))) 
id.in2 <- c(1,id.fi2+1)
AIRR2 <- 0
APDC2 <- 0
ATEMP2 <- 0 


for(d in 1:length(id.fi2)){
  AIRR2 <- c(AIRR2, cumsum(dados50[id.in2[d]:id.fi2[d],3]))
  APDC2 <- c(APDC2, cumsum(dados50[id.in2[d]:id.fi2[d],6]))
  ATEMP2 <- c(ATEMP2, cumsum(dados50[id.in2[d]:id.fi2[d],4]))
}

#### Gerar Dataset auxiliar chamado DA3 ####
DA3 <- D2
DA3[,'AIRR'] <- AIRR2[-1]     
DA3[,'APDC'] <- APDC2[-1]


#### Salvar DA3 no Desktop ####
# write.table(DA3, file = "C:\\Users\\Guilherme\\Desktop\\data3.csv", sep = ",", row.names = F)
# DataFrame com todas variáveis (inclusive AIRR e APDC)
# DA3


#### Criar cópia do DataFrame DA3 apenas com dados de IRR <= 700 ####
dadoscopy <- DA3
dadoscopy2 <- dadoscopy[!(dadoscopy$IRR <= 700),]
dadoscopy2

#### Verificar posições iniciais e finais desse novo dataset ####
id.fi3 <- cumsum(as.numeric(ftable(dadoscopy2[,1]))) 
id.in3 <- c(1,id.fi3+1)


#### Calcular IRR e PDC acumulados para daset com dados de IRR <= 700 ####
IRR_PICO <- 0
PDC_PICO <- 0
for(d in 1:length(id.fi3)){
  IRR_PICO <- c(IRR_PICO, cumsum(dadoscopy2[id.in3[d]:id.fi3[d],3]))
  PDC_PICO <- c(PDC_PICO, cumsum(dadoscopy2[id.in3[d]:id.fi3[d],3])*10.45)
}
dadoscopy2[,"IRR_PICO"] <- IRR_PICO[-1]     
dadoscopy2[,"PDC_PICO"] <- PDC_PICO[-1]



#### Criar Dataset apenas com as últimas medições de IRR <= 700 ####
teste <- c()
for(i in 1:length(id.fi3)){
  teste <- rbind(teste, dadoscopy2[id.fi3[i],])
}


#### Realizar transformação logarítimica no dataset ####
dadoscopy2[,"teste"]<- log(dadoscopy2[,'PDC_PICO'])








teste2 <- c()
for(d in 1:length(id.fi3)){
  teste2 <- c(teste2, sum(dadoscopy2[id.in3[d]:id.fi3[d],"teste"]))
}



aux1 <- c()
aux2 <- c()

for(d in 1:length(id.fi3)){
  aux1 <- dadoscopy2[xi[dia]:xf[dia],6]
  aux2 <- c(aux2, sum(aux1))
}


Dip_list
Dio_list














d <- 1
PP <- 0
qtd_amostras <- 0

for(d in 1:length(id.fi3)){
    PP            <- c(PP, dadoscopy2[,'IRR_PICO'][id.fi3[d]])
    qtd_amostras  <- c(qtd_amostras, (id.fi3[d]-id.fi3[d-1]) )
}








AIRR2 <- c(AIRR2, cumsum(dados50[id.in2[d]:id.fi2[d],3]))
APDC2 <- c(APDC2, cumsum(dados50[id.in2[d]:id.fi2[d],6]))
ATEMP2 <- c(ATEMP2, cumsum(dados50[id.in2[d]:id.fi2[d],4]))







DA2 <- D2
DA2[,3] <- AIRR2[-1]     
DA2[,6] <- APDC2[-1]

##### Transformação Log ####
if ( 1 == 0 ){
  DA2[,1] <- factor(DA2[,1])     
  DA2[,2] <- DA2[,2] - 1
  DA2[,3] <- log(DA2[,3])
  DA2[,4] <- log(DA2[,4])
  DA2[,5] <- log(DA2[,5])
  DA2[,6] <- log(DA2[,6])
}



DA2 <- data.frame(DA2)

