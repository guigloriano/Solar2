
library(dplyr)
library(lubridate)
library(scales)
library(reshape2)
library(ggplot2)
library(hrbrthemes)
library(plotly)

#### 0.0 - Script para Cria��o do Gr�fico ####

##### 0.1 - Defini��o do Caminho "raiz" do script ####
local_pasta <- "D:\\Solar2\\Parte2\\"

graph_extra <- paste(local_pasta, "graphs\\5 - Diversos\\", sep = "")

##### 0.2 - Depend�ncias do Script #####

# Para executar esse script � necess�rio algumas vari�veis precedentes de 
# outros arquivos, como:
#     - Dip_List
#     - Dio List

# Para isso � necess�rio executar antes os scritps:
#     - 001a_nlme_treino  
#     - 001b_nlme_teste   

cod_treino <- paste(local_pasta, "scripts\\004a_nlme_treino50.R", sep = "")
source(cod_treino)

cod_teste <- paste(local_pasta, "scripts\\004b_nlme_teste50.R", sep = "")
source(cod_teste)





#### 1.0 - Inicializa��o de Vari�veis ####
ver_eqm <- numeric()
ver_eqm <- data.frame((EQMi))
mean(EQMi)  

#### 2.0 - Gr�ficos: Dif. Perc. Corrigida e EQM (com erro) ####

##### 2.1 - Grafico do EQM com a Tendencia do erro ######
eqm_png <- paste ("EQMerror50", ".png", sep="")
dest_eqm <- paste(graph_extra, eqm_png, sep = "")
png(filename = dest_eqm, width = 720, height = 480, units = 'px')

    data_eqm <- data.frame(cbind(sm, EQMi))
    
    gp_eqm <- ggplot(data = data_eqm, aes(x = sm)) + theme_bw() +
        geom_line(aes(y= EQMi, group = 1, color = "Prediction Error"), 
                  size = 0.6, lty = "81") +
        geom_point(aes(y = EQMi, color = "Prediction Error"), size= 1.0) + 
        scale_x_continuous(breaks = 5*0:106, expand = c(0.01,0.01)) +
        geom_smooth(aes(y = EQMi, color = "Local Regression Fitting", 
                        method="loess", se = F))+#, linetype = "dashed") +
        xlab('Days') +   ylab('MSE - Mean Square Error') +
        theme(text = element_text(size=16),
              axis.text.x = element_text(angle=45, hjust=1),
              legend.justification=c(1,0), legend.position=c(1,0),
              legend.background = element_rect(fill = "lightgray") ) +
        scale_colour_manual(name=NULL,
                            values=c("Prediction Error" = "blue", 
                                     "Local Regression Fitting" ="red"))

gp_eqm
print(gp_eqm)
dev.off()



##### 2.2- Grafico da Dif. Per. Media Original  ####
dpm_png <- paste ("DifPerc50_original_105.png", sep="")
dest_dpm <- paste(graph_extra, dpm_png, sep = "")
png(filename = dest_dpm, width = 720, height = 480, units = 'px')

new_bar_DPM <- data.frame("dia" = seq(1,100), "DPM" = Dim)

DPM_corrigido1 <- ggplot(data = new_bar_DPM , aes(x=dia, y = DPM, color="gray")) + 
  theme_bw() + geom_bar(stat="identity", position="dodge", color="black",) +
  #  scale_y_continuous(breaks = seq(from = min(Dim), to = max(Dim), by = 1), name = "Percentagem (%)") +
  scale_y_continuous(limits = c(min(Dim),max(Dim)), name = "Percentage (%)") +
  scale_x_continuous(breaks = 5*0:106, expand = c(0.01,0.01), name = "Days") +
  #  geom_smooth(aes(y = DPM, color = "Local Regression Fitting", method="loess", se = F))+
  #, linetype = "dashed") +
  theme(axis.title.y = element_text(hjust = 0.5, color = 'black', size=20),) + 
  # ggtitle("Diferen�a Percentual M�dia Di�ria") +  theme_bw() + 
  #  ggtitle("Percent Difference Average Daily") ++
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45))

print(DPM_corrigido1)

plot(DPM_corrigido1)
dev.off()





##### 2.3 - Grafico da Dif. Per. Media Original  ####
dpm_png <- paste ("DifPerc50_Abs_105.png", sep="")
dest_dpm <- paste(graph_extra, dpm_png, sep = "")
png(filename = dest_dpm, width = 720, height = 480, units = 'px')

new_bar_DPM2 <- data.frame("dia" = seq(1,100), "DPM" = abs(Dim))

DPM_corrigido <- ggplot(data = new_bar_DPM2 , aes(x=dia, y = DPM, color="gray")) + 
  theme_bw() + geom_bar(stat="identity", position="dodge", color="black",) +
  #  scale_y_continuous(breaks = seq(from = min(Dim), to = max(Dim), by = 1), name = "Percentagem (%)") +
  scale_y_continuous(limits = c(0, max(new_bar_DPM2$DPM) ), name = "Percentagem (%)") +
  scale_x_continuous(breaks = 5*0:106, expand = c(0.01,0.01), name = "Dias") +
  #  geom_smooth(aes(y = DPM, color = "Local Regression Fitting", method="loess", se = F))+
  #, linetype = "dashed") +
  theme(axis.title.y = element_text(hjust = 0.5, color = 'black', size=20),) + 
  # ggtitle("Diferen�a Percentual M�dia Di�ria") +  theme_bw() + 
  #  ggtitle("Percent Difference Average Daily") ++
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45))

plot(DPM_corrigido)
dev.off()




##### 2.4 - Grafico: DCPower (Dip x Dio Completo)  ####
dpm_png <- paste ("DCPower50.png", sep="")
dest_dpm <- paste(graph_extra, dpm_png, sep = "")
png(filename = dest_dpm, width = 720, height = 480, units = 'px')
    
    sm <- seq(1,length(Dip_list))
    # dev.off()
    dc_power <- ggplot() + theme_bw() + 
        geom_line(aes(x = sm, y = Dip_list, group=1, color = "Predicted"), size = 0.7) +
        geom_point(aes(x = sm, y = Dip_list, color = "Predicted"), size = 1.5) +
        
        geom_line(aes(x = sm, y = Dio_list, group=2, color = "Observed"), size = 0.7) +
        geom_point(aes(x = sm, y = Dio_list, color = "Observed"), size = 1.5) +
        
        xlab('Days') + ylab('DC Power') + 
        scale_x_continuous(breaks=seq(0, 106, 5))  +
        scale_fill_manual(values=c("#a6cee3","#1f78b4","#b2df8a")) + 
        theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust=1),
              legend.justification=c(1,1), legend.position=c(.99,.99),
              legend.background = element_rect(fill = "lightgray", size=0.5, 
                                               linetype="solid", colour ="black")) +
        
        scale_color_manual(name=NULL,
                           values=c('Predicted' = 'blue', 'Observed' ='red'))

print(dc_power)
plot(dc_power)

dev.off()


Dip_list
Dio_list
