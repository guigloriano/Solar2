
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

dados1 <- read_ods("D:\\Mestrado\\Em Desenvolvimento\\csvs\\1_21.ods") 
dados2 <- read_ods("D:\\Mestrado\\Em Desenvolvimento\\csvs\\22_42.ods") 
dados3 <- read_ods("D:\\Mestrado\\Em Desenvolvimento\\csvs\\43_63.ods") 
dados4 <- read_ods("D:\\Mestrado\\Em Desenvolvimento\\csvs\\64_84.ods") 
dados5 <- read_ods("D:\\Mestrado\\Em Desenvolvimento\\csvs\\85_105.ods") 


#### Agrupar os Datasets ####

dados2$DIA <- dados2$DIA + 21 
dados3$DIA <- dados3$DIA + 42 
dados4$DIA <- dados4$DIA + 63 
dados5$DIA <- dados5$DIA + 84 

dadosTotal <- rbind(dados1, dados2, dados3, dados4, dados5)

dadosTotal$X <- NULL

#### Selecionar os 50 Primeiros Dias ####

dados50 <- dadosTotal[!(dadosTotal$DIA > 50),]
dados50 <- na.omit(dados50)

#### Remover os PDCs = 0, para não ter problema no modelo Logístico ####
dados50 <- dados50[!(dados50$PDC == 0),]


str(dados50)
D2 <- data.frame(dados50)
n2 <- nrow(D2)

##### ACUMULADA ####

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


DA2 <- D2
DA2[,3] <- AIRR2[-1]     
DA2[,6] <- APDC2[-1]

##### Transformação Log ####

DA2[,1] <- factor(DA2[,1])     
DA2[,2] <- DA2[,2] - 1
DA2[,3] <- log(DA2[,3])
DA2[,4] <- log(DA2[,4])
DA2[,5] <- log(DA2[,5])
DA2[,6] <- log(DA2[,6])

DA2 <- data.frame(DA2)

#### GrÃ¡ficos dos dias 1 e 2 ####

plot(DA2[1:74,2], exp(DA2[1:74,6]), type="l", 
     lwd=2, ylab="Power Generated", xlab="Time")
points(DA2[1:74,2], exp(DA2[1:74,6]), pch=19)

plot(DA2[75:149,2], exp(DA2[75:149,6]), type="l", 
     lwd=2, ylab="Power Generated", xlab="Time")
points(DA2[75:149,2], exp(DA2[75:149,6]), pch=19)

plot(DA2[1:74,2], DA2[1:74,6], type="l", lwd=2, 
     ylab="Log-measures", xlab="Time")
points(DA2[1:74,2], DA2[1:74,6], pch=19)

plot(DA2[75:149,2], DA2[75:149,6], type="l", lwd=2, 
     ylab="Log-measures", xlab="Time")
points(DA2[75:149,2], DA2[75:149,6], pch=19)




f.logia <- function(x1, alpha0, beta0, beta1){
  (alpha0) - log(1 + exp(beta0 - beta1*x1))
} 

##### M1 - So Logístico ####
# obs: chamado de M0 no script original

v1a <- c(1,1,0.1)    

M1a <- nls(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
          data = DA2, 
          start=c(alpha0=1, beta0=1, beta1=0.1) #, start = v1,
)    

AIC(M1a)
BIC(M1a)

pred1a <- predict(M1a)
EQM1a <- round(mean((pred1a - DA2[,6])^2), 4)
EQM1a

summary(M1a)




##### M2 - somente alpha0 ####
# obs: chamado de M1 no script original

v2a <- c(1, 1, 0.2)    
M2a <- nlme(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
           data = DA2, 
           fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
           random = list(alpha0 ~ 1),
           start=v2a,
           groups = ~ DIA
)

AIC(M2a)
BIC(M2a)

summary(M2a)

pred2a <- predict(M2a)
EQM2a <- round(mean((pred2a - DA2[,6])^2), 4)
EQM2a



##### M3 - somente beta0 ####
# obs: chamado de M2 no script original

v3a <- c(1, 1, 0.1)    
M3a <- nlme(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
           data = DA2, 
           fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
           random = list(beta0 ~ 1),
           start=v3a, 
           groups = ~ DIA
)

AIC(M3a)           
BIC(M3a)
pred3a <- predict(M3a)
EQM3a <- round(mean((pred3a - DA2[,6])^2), 4)
EQM3a




##### M4 - somente beta1 (ou gama na Tabela 3)
# obs: chamado de M3 no script original

v4a <- c(1, 1, 0.1)    
M4a <- nlme(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
           data = DA2, 
           fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
           random = list(beta1 ~ 1),
           start=v4a, 
           groups = ~ DIA
)

AIC(M4a)     
BIC(M4a)
pred4a <- predict(M4a)
EQM4a <- round(mean((pred4a - DA2[,6])^2), 4)
EQM4a




##### M5 - Efeito em alpha0 e beta0 ####
# obs: chamado de M4 no script original

v5a <- c(1, 1, 0.1)    
M5a <- nlme(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
           data = DA2, 
           fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
           random = list(alpha0 ~ 1, beta0 ~ 1),
           start=v5a, groups = ~ DIA
)

AIC(M5a)     
BIC(M5a)
pred5a <- predict(M5a)
EQM5a <- round(mean((pred5a - DA2[,6])^2), 4)
EQM5a





##### M6 - Efeito em alpha0 e beta1 ####
# obs: chamado de M5 no script original

v6a <- c(1, 1, 0.1)    
M6a <- nlme(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
           data = DA2, 
           fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
           random = list(alpha0 ~ 1, beta1 ~ 1),
           start=v6a, 
           groups = ~ DIA
)

AIC(M6a)     
BIC(M6a)           
pred6a <- predict(M6a)
EQM6a <- round(mean((pred6a - DA2[,6])^2), 4)
EQM6a





##### M7 - Efeito em beta0 e beta1 ####
# obs: chamado de M6 no script original

v7a <- c(1, 1, 0.1)    
M7a <- nlme(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
           data = DA2, 
           fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
           random = list(beta0 ~ 1, beta1 ~ 1),
           start=v7a, groups = ~ DIA
)

AIC(M7a)     
BIC(M7a)           
pred7a <- predict(M7a)
EQM7a <- round(mean((pred7a - DA2[,6])^2), 4)
EQM7a





##### M8 - Efeito em alpha0, beta0 e beta1 ####
# obs: chamado de M7 no script original
v8a <- c(1, 1, 0.1)    
M8a <- nlme(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
           data = DA2, 
           
           
           #### inserção destas linhas para convergência do modelo ####
           control = nlmeControl(maxIter=100000,
                                 opt = c("nlminb"), #nlminb
                                 msMaxIter = 100000,
           ),
           
           
           fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
           random = list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
           start=v8a, 
           groups = ~ DIA
)

AIC(M8a)     
BIC(M8a)  
pred8a <- predict(M8a)
EQM8a <- round(mean((pred8a - DA2[,6])^2), 4)
EQM8a





#### M9 - Efeito em alpha e beta0  ####

v9a <- c(1,1,1,1,1,1,1) 
M9a <- nlme(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
           data = DA2, 
           
           
           #### inserção destas linhas para convergência do modelo ####
           control = nlmeControl(maxIter=100000,
                                 opt = c("nlminb"), #nlminb
                                 msMaxIter = 100000,
                                 minScale = 1e-8
           ),
           
           
           fixed=list(alpha0 ~ IRR + MA, beta0 ~ IRR + MA, beta1 ~ 1),
           random = list(alpha0 ~ 1, beta0 ~ 1),
           correlation = corAR1(0.2, form = ~ TIME|DIA),
           weights = varPower(),   
           start=v9a, groups = ~ DIA)

AIC(M9a)
BIC(M9a)

pred9a <- predict(M9a)
EQM9a <- round(mean((pred9a - DA2[,6])^2), 4)
EQM9a

summary(M9a)







#### M10 - alpha0 e beta0 ####

v10a <- c(1,1,1,1,1) 
M10a <- nlme(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
            data = DA2,            

            fixed=list(alpha0 ~ IRR, beta0 ~ IRR, beta1 ~ 1),            
            random = (list(alpha0 ~ 1)),
#            random = (list(alpha0 ~ 1, beta0 ~ 1)),
            correlation = corAR1(0.2, form = ~ TIME|DIA),
            weights = varPower(),   
            start=v10a, 
            groups = ~ DIA)

AIC(M10a)
BIC(M10a)
pred10a <- predict(M10a)
EQM10a <- round(mean((pred10a - DA2[,6])^2), 4)
EQM10a

summary(M10a)







Criteria2 <- c("AIC", "BIC", "MSE")
MS1a <- c(AIC(M1a), BIC(M1a), EQM1a)
MS2a <- c(AIC(M2a), BIC(M2a), EQM2a)
MS3a <- c(AIC(M3a), BIC(M3a), EQM3a)
MS4a <- c(AIC(M4a), BIC(M4a), EQM4a)
MS5a <- c(AIC(M5a), BIC(M5a), EQM5a)
MS6a <- c(AIC(M6a), BIC(M6a), EQM6a)
MS7a <- c(AIC(M7a), BIC(M7a), EQM7a)
MS8a <- c(AIC(M8a), BIC(M8a), EQM8a)
MS9a <- c(AIC(M9a), BIC(M9a), EQM9a)
MS10a <- c(AIC(M10a), BIC(M10a), EQM10a)

SelectionCriteria2 <- data.frame(Criteria2, MS1a, MS2a, MS3a, MS4a, MS5a, 
                                 MS6a, MS7a, MS8a, MS9a, MS10a)









#### Modelo M11  ####

v11a <- c(1,1,1,1,1,1,1) 
M11a <- nlme(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
            data = DA2, 
            
            #### inserção destas linhas para convergência do modelo ####
            control = nlmeControl(maxIter=100000,
                                  opt = c("nlminb"), #nlminb
                                  msMaxIter = 100000,
                                  minScale = 1e-8
            ),
            
            
            fixed=list(alpha0 ~ IRR + MA, beta0 ~ IRR + MA, beta1 ~ 1),
#            random = list(beta1 ~ 1),
#            random = list(alpha0 ~ 1, beta0 ~ 1),
#            random = list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
            correlation = corAR1(0.2, form = ~ TIME|DIA),
            weights = varPower(),   
            start=v11a, groups = ~ DIA)


AIC(M11a)
BIC(M11a)

pred11a <- predict(M11a)
EQM11a <- round(mean((pred11a - DA2[,6])^2), 4)
EQM11a

summary(M11a)






#### GrÃ¡fico dos valores preditos x resÃduos

par(mai=c(1,1,0.3,0.3))
res <- as.numeric(residuals(M9a))
plot(pred9, res, ylim=c(-1.5,1.5), ylab="Residuals",xlab="Predicted values")

#### GrÃ¡fico dos valores registrados x preditos

plot(DA[,6], pred9, ylab="Predicted values", xlab="Registered values")


##### Graficos dos dias #####

id.fi <- cumsum(as.numeric(ftable(D[,1]))) 
id.in <- c(1,id.fi+1)

dia <- 1

##### tempo x PCD - Acumulados

xi <- id.in[dia] 
xf <- id.fi[dia]
Di <- cumsum(D[xi:xf,6])
plot(D[xi:xf,2], Di, pch=19, lwd=2, ylab="X", xlab="Tempo", xlim=c(0,length(Di)))
points(D[xi:xf,2], Di, col="black", type="l", lwd=2)

##### tempo x PCD - Acumulados

xi <- id.in[dia] 
xf <- id.fi[dia]
Do <- DA[xi:xf,6]
De <- as.numeric(pred9[xi:xf])
nd <- length(Do)

## Log-scale

plot(D[xi:xf,2], Do, col="grey70", pch=19, lwd=2, ylab="Y's values", xlab="Time", xlim=c(0, 77), ylim=c(3,13))
points(D[xi:xf,2], De, col="black", type="l", lwd=2)

## origina;-scale

plot(D[xi:xf,2], exp(Do), pch=19, col="grey70", lwd=2, ylab="X", xlab="Tempo", xlim=c(0,length(Do)))
points(D[xi:xf,2], exp(De), col="black", type="l", lwd=2)

###########################          
#### Percentuais em relaÃ§Ã£o
###########################

perc <- round(((exp(Do[nd]) - exp(De[nd]))/exp(De[nd]))*100, 4)
perc

EQM <- round(mean((Do - De)^2), 4)
EQM

PE <- c(-0.0697, 0.6212, 0.8907, -0.5001, -0.2452, 0.3554, -0.4009, -0.0659, 0.2027, 0.2923, 0.0370, -0.0278)
barplot(PE, ylim=c(-1,1), xlab="Days", ylab="Percentage")

text(0.8, -0.95, "Day 1", cex=0.7)
text(1.9, -0.95, "Day 2", cex=0.7)
text(3.1, -0.95, "Day 3", cex=0.7)
text(4.3, -0.95, "Day 4", cex=0.7)
text(5.5, -0.95, "Day 5", cex=0.7)
text(6.7, -0.95, "Day 6", cex=0.7)
text(7.9, -0.95, "Day 7", cex=0.7)
text(9.1, -0.95, "Day 8", cex=0.7)
text(10.3, -0.95, "Day 9", cex=0.7)
text(11.5, -0.95, "Day 10", cex=0.7)
text(12.7, -0.95, "Day 11", cex=0.7)
text(13.9, -0.95, "Day 12", cex=0.7)

text(0.8, -0.0997, "-0.0697", cex=0.6)
text(1.9, 0.6612, "0.6212", cex=0.6)
text(3.1, 0.9307, "0.8907", cex=0.6)
text(4.3, -0.5401, "-0.5001", cex=0.6)
text(5.5, -0.2852, "-0.2452", cex=0.6)
text(6.7, 0.3954, "0.3554", cex=0.6)
text(7.9, -0.4409, "-0.4009", cex=0.6)
text(9.1, -0.1059, "-0.0659", cex=0.6)
text(10.3, 0.2427, "0.2027", cex=0.6)
text(11.5, 0.3323, "0.2923", cex=0.6)
text(12.7, 0.0770, "0.0370", cex=0.6)
text(13.9, -0.0678, "-0.0278", cex=0.6)
