
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
dados <- read_ods("D:\\Solar2\\Parte2\\datasets\\ods\\1_21.ods") 

str(dados)
D <- data.frame(dados)
n <- nrow(D)


D <- D[!(D$PDC == 0),]

##### ACUMULADA ####

id.fi <- cumsum(as.numeric(ftable(D[,1]))) 
id.in <- c(1,id.fi+1)
AIRR <- 0
APDC <- 0
ATEMP <- 0 

for(d in 1:length(id.fi)){
  AIRR <- c(AIRR, cumsum(dados[id.in[d]:id.fi[d],3]))
  APDC <- c(APDC, cumsum(dados[id.in[d]:id.fi[d],6]))
  ATEMP <- c(ATEMP, cumsum(dados[id.in[d]:id.fi[d],4]))
}

DA <- D
DA[,3] <- AIRR[-1]     
DA[,6] <- APDC[-1]

##### Transformação Log ####

DA[,1] <- factor(DA[,1])     
DA[,2] <- DA[,2] - 1
DA[,3] <- log(DA[,3])
DA[,4] <- log(DA[,4])
DA[,5] <- log(DA[,5])
DA[,6] <- log(DA[,6])

DA <- data.frame(DA)

#### GrÃ¡ficos dos dias 1 e 2 ####

plot(DA[1:74,2], exp(DA[1:74,6]), type="l", 
     lwd=2, ylab="Power Generated", xlab="Time")
points(DA[1:74,2], exp(DA[1:74,6]), pch=19)

plot(DA[75:149,2], exp(DA[75:149,6]), type="l", 
     lwd=2, ylab="Power Generated", xlab="Time")
points(DA[75:149,2], exp(DA[75:149,6]), pch=19)

plot(DA[1:74,2], DA[1:74,6], type="l", lwd=2, 
     ylab="Log-measures", xlab="Time")
points(DA[1:74,2], DA[1:74,6], pch=19)

plot(DA[75:149,2], DA[75:149,6], type="l", lwd=2, 
     ylab="Log-measures", xlab="Time")
points(DA[75:149,2], DA[75:149,6], pch=19)




f.logi <- function(x1, alpha0, beta0, beta1){
  (alpha0) - log(1 + exp(beta0 - beta1*x1))
} 

##### M1 - So Logístico ####
# obs: chamado de M0 no script original

v1 <- c(1,1,0.1)    

M1 <- nls(PDC ~ f.logi(TIME, alpha0, beta0, beta1), 
          data = DA, 
          start=c(alpha0=1, beta0=1, beta1=0.1) #, start = v1,
)    

AIC(M1)
BIC(M1)

pred1 <- predict(M1)
EQM1 <- round(mean((pred1 - DA[,6])^2), 4)
EQM1

##### M2 - somente alpha0 ####
# obs: chamado de M1 no script original

v2 <- c(1, 1, 0.2)    
M2 <- nlme(PDC ~ f.logi(TIME, alpha0, beta0, beta1), 
           data = DA, 
           fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
           random = list(alpha0 ~ 1),
           start=v2,
           groups = ~ DIA
)

AIC(M2)
BIC(M2)

summary(M2)

pred2 <- predict(M2)
EQM2 <- round(mean((pred2 - DA[,6])^2), 4)
EQM2

##### M3 - somente beta0 ####
# obs: chamado de M2 no script original

v3 <- c(1, 1, 0.1)    
M3 <- nlme(PDC ~ f.logi(TIME, alpha0, beta0, beta1), 
           data = DA, 
           fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
           random = list(beta0 ~ 1),
           start=v3, 
           groups = ~ DIA
)

AIC(M3)           
BIC(M3)
pred3 <- predict(M3)
EQM3 <- round(mean((pred3 - DA[,6])^2), 4)
EQM3

##### M4 - somente beta1 (ou gama na Tabela 3)
# obs: chamado de M3 no script original

v4 <- c(1, 1, 0.1)    
M4 <- nlme(PDC ~ f.logi(TIME, alpha0, beta0, beta1), 
           data = DA, 
           fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
           random = list(beta1 ~ 1),
           start=v4, 
           groups = ~ DIA
)

AIC(M4)     
BIC(M4)
pred4 <- predict(M4)
EQM4 <- round(mean((pred4 - DA[,6])^2), 4)
EQM4

##### M5 - Efeito em alpha0 e beta0 ####
# obs: chamado de M4 no script original

v5 <- c(1, 1, 0.1)    
M5 <- nlme(PDC ~ f.logi(TIME, alpha0, beta0, beta1), 
           data = DA, 
           fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
           random = list(alpha0 ~ 1, beta0 ~ 1),
           start=v5, groups = ~ DIA
)

AIC(M5)     
BIC(M5)
pred5 <- predict(M5)
EQM5 <- round(mean((pred5 - DA[,6])^2), 4)
EQM5


##### M6 - Efeito em alpha0 e beta1 ####
# obs: chamado de M5 no script original

v6 <- c(1, 1, 0.1)    
M6 <- nlme(PDC ~ f.logi(TIME, alpha0, beta0, beta1), 
           data = DA, 
           fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
           random = list(alpha0 ~ 1, beta1 ~ 1),
           start=v6, 
           groups = ~ DIA
)

AIC(M6)     
BIC(M6)           
pred6 <- predict(M6)
EQM6 <- round(mean((pred6 - DA[,6])^2), 4)
EQM6


##### M7 - Efeito em beta0 e beta1 ####
# obs: chamado de M6 no script original

v7 <- c(1, 1, 0.1)    
M7 <- nlme(PDC ~ f.logi(TIME, alpha0, beta0, beta1), 
           data = DA, 
           fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
           random = list(beta0 ~ 1, beta1 ~ 1),
           start=v7, groups = ~ DIA
)

AIC(M7)     
BIC(M7)           
pred7 <- predict(M7)
EQM7 <- round(mean((pred7 - DA[,6])^2), 4)
EQM7





##### M8 - Efeito em alpha0, beta0 e beta1 ####
# obs: chamado de M7 no script original
v8 <- c(1, 1, 0.1)    
M8 <- nlme(PDC ~ f.logi(TIME, alpha0, beta0, beta1), 
           data = DA, 
           #### inserção destas linhas para convergência do modelo ####
           control = nlmeControl(maxIter=100000,
                                 opt = c("nlminb"), #nlminb
                                 msMaxIter = 100000,
           ),
           ############################################################
           
           fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
           random = list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
           start=v8, 
           groups = ~ DIA
)

AIC(M8)     
BIC(M8)  
pred8 <- predict(M8)
EQM8 <- round(mean((pred8 - DA[,6])^2), 4)
EQM8





#### Modelo M9  ####

v9 <- c(1,1,1,1,1,1,1) 
M9 <- nlme(PDC ~ f.logi(TIME, alpha0, beta0, beta1), 
           data = DA, 
           fixed=list(alpha0 ~ IRR + MA, beta0 ~ IRR + MA, beta1 ~ 1),
           random = list(alpha0 ~ 1, beta0 ~ 1),
           correlation = corAR1(0.2, form = ~ TIME|DIA),
           weights = varPower(),   
           start=v9, groups = ~ DIA)

AIC(M9)
BIC(M9)

pred9 <- predict(M9)
EQM9 <- round(mean((pred9 - DA[,6])^2), 4)
EQM9

summary(M9)







#### Modelo M10 ####

v10 <- c(1,1,1,1,1) 
M10 <- nlme(PDC ~ f.logi(TIME, alpha0, beta0, beta1), 
            data = DA, 
            fixed=list(alpha0 ~ IRR, beta0 ~ IRR, beta1 ~ 1),
            random = (list(alpha0 ~ 1, beta0 ~ 1)),
            correlation = corAR1(0.2, form = ~ TIME|DIA),
            weights = varPower(),   
            start=v10, groups = ~ DIA)

AIC(M10)
BIC(M10)
pred10 <- predict(M10)
EQM10 <- round(mean((pred10 - DA[,6])^2), 4)
EQM10

summary(M10)





Criteria <- c("AIC", "BIC", "MSE")
MS1 <- c(AIC(M1), BIC(M1), EQM1)
MS2 <- c(AIC(M2), BIC(M2), EQM2)
MS3 <- c(AIC(M3), BIC(M3), EQM3)
MS4 <- c(AIC(M4), BIC(M4), EQM4)
MS5 <- c(AIC(M5), BIC(M5), EQM5)
MS6 <- c(AIC(M6), BIC(M6), EQM6)
MS7 <- c(AIC(M7), BIC(M7), EQM7)
MS8 <- c(AIC(M8), BIC(M8), EQM8)
MS9 <- c(AIC(M9), BIC(M9), EQM9)
MS10 <- c(AIC(M10), BIC(M10), EQM10)

SelectionCriteria <- data.frame(Criteria, MS1, MS2, MS3, MS4, MS5, MS6, 
                                MS7, MS8, MS9, MS10)








#### GrÃ¡fico dos valores preditos x resÃduos

par(mai=c(1,1,0.3,0.3))
res <- as.numeric(residuals(M9))
plot(pred9, res, ylim=c(-1.5,1.5), ylab="Residuals",xlab="Predicted values")

#### GrÃ¡fico dos valores registrados x preditos

plot(DA[,6], pred9, ylab="Predicted values", xlab="Registered values")

######################################
##### Graficos dos dias ##############
######################################

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
