
library(dplyr)
library(lubridate)
library(scales)
library(reshape2)
library(ggplot2)
library(hrbrthemes)
library(plotly)


#### 0.0 - Script para Criação do Gráfico ####

##### 0.2 - Definição do Caminho "raiz" do script ####

local_pasta <- "D:\\Solar2\\Parte2\\"
cam_graph5 <- paste(local_pasta, "graphs\\6 - DIP x DIO (Mensal)\\", sep = "")


##### 0.3 - Dependências do Script #####

# Para executar esse script é necessário algumas variáveis precedentes de 
# outros arquivos, como:
#     - Dip_List
#     - Dio List

# Para isso é necessário executar antes os scritps:
#     - 001a_nlme_treino  
#     - 001b_nlme_teste   

cod_treino <- paste(local_pasta, "scripts\\001a_nlme_treino21.R", sep = "")
source(cod_treino)

cod_teste <- paste(local_pasta, "scripts\\001b_nlme_teste21.R", sep = "")
source(cod_teste)



#### 1.0 - Preparação do Dataset para geração do Gráfico ####

##### 1.1 - Criação do Dataset com Predito e Observado #####
df.PredObs <- numeric()
qtd_dias_pred <- length(Dip_list)
df.PredObs <- data.frame("Dia" = seq(1,qtd_dias_pred), 
                         "Predita" = Dip_list, 
                         "Observada" = Dio_list)


##### 1.2 - Leitura do Dataset contendo Todos os Dias com as datas #####

# Neste dataset está presente TODAS as amostras obtidas no período de 105 dias.
# Onde o valor da IRR > 0. Há também os dias em que ocorreram as medições. 

cam_df.Total <- paste(local_pasta, "datasets\\TodosDias2.csv", sep = "")
df.Total <- read.csv(cam_df.Total, header = TRUE)
df.Total$X <- NULL


##### 1.3 - Fazendo Merge nos Datasets anteriores #####

df.TotalCombinado <- merge(df.PredObs, df.Total, by.x = "Dia",
                           by.y = "dia", all.x = TRUE, all.y = FALSE)


###### 1.3.1 Remoção de variáveis que não serão utilizadas ######
df.TotalCombinado$TIME <- NULL
df.TotalCombinado$irr_inv <- NULL
df.TotalCombinado$temp <- NULL
df.TotalCombinado$m <- NULL
df.TotalCombinado$P_DC <- NULL


###### 1.3.2 - Seleção de Linhas únicas do dataset ######
df.TotalCombinado <- distinct(df.TotalCombinado)


###### 1.3.3 - Geração de uma cópia do dataset ######
df.Ajustado <- df.TotalCombinado
df.Ajustado$Dia <- NULL
df.Ajustado$X <- NULL


###### 1.3.4 - Conversão da variável para o tipo DATE ######
df.Ajustado$dia_mes_ano <- parse_date_time(df.Ajustado$dia_mes_ano, 
                                           orders = c("ymd", "dmy", "mdy"))

df.Ajustado$dia_mes_ano <- as.Date(df.Ajustado$dia_mes_ano)



#### 2.0 Geração dos Gráficos de Barras: DIP x DIO ####

##### 2.1 - Mensais - Outubro/19 #####

nome_png <- paste ("1_Out19_energia_completo.png")
dest_png <- paste(cam_graph5, nome_png, sep = "")
png(filename = dest_png, width = 720, height = 480, units = 'px')


###### Criação da lista com os dias de Outubro
diasMes <- numeric()
diasMes <- data.frame("dia" = seq(as.Date("2019/10/1"), 
                                  as.Date("2019/10/31"), 
                                  "days"))

###### Separação dos dias de Outubro presentes no dataset
diasOutubro <- df.Ajustado[1:8,]

###### Combinando os os dias de outubro do dataset com os dias mensais
diasOutubro <- merge(diasOutubro, diasMes, by.x = "dia_mes_ano", 
                     by.y = "dia", all.x = TRUE, all.y = TRUE)

###### Substituindo os valores NA por 0 
diasOutubro[is.na(diasOutubro)] <- 0

###### Agrupando pelo dia em 2 Fatores: Energia Predita e Observada
grupoOut <- melt(data = diasOutubro, id.vars = "dia_mes_ano", 
                 measure.vars = c("Predita", "Observada"))

###### Renomeando o Fator "variable" para "Energia" 
names(grupoOut)[names(grupoOut) == "variable"] <- "Energia"

###### Plotagem do Gráfico de Outubro 
gf_predOut <- ggplot(data = grupoOut, 
                     aes(x=dia_mes_ano, y = value, fill = Energia)) +  
  theme_ipsum() + geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 50*0:900, name = "Energy") +
  scale_x_date(name="Days", labels = date_format("%m-%d-%Y"),
               breaks = date_breaks("days"), expand = c(0,0)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, color = 'black', size=16),
        axis.text.x = element_text(angle = 90, size=12),
        axis.title.x = element_text(hjust = 0.5, size=16)) + 
  labs(fill = "Energy:") +
  scale_fill_discrete(name = "Energy", labels = c("Predicted", "Observed")) +
  ggtitle("Days - October/19")

plot(gf_predOut)
print(gf_predOut)
dev.off()


##### 2.2 - Mensais - Novembro/19 ####
nome_png <- paste ("2_Nov19_energia_completo.png")
dest_png <- paste(cam_graph5, nome_png, sep = "")
png(filename = dest_png, width = 720, height = 480, units = 'px')

###### Criação da lista com os dias de Novembro
diasMes <- numeric()
diasMes <- data.frame("dia" = seq(as.Date("2019/11/1"), 
                                  as.Date("2019/11/30"), "days"))

###### Separação dos dias de Novembro presentes no dataset
diasNovembro <- df.Ajustado[9:33,]

###### Combinando os os dias de Novembro do dataset com os dias mensais
diasNovembro <- merge(diasNovembro, diasMes, by.x = "dia_mes_ano", 
                      by.y = "dia", all.x = TRUE, all.y = TRUE)

###### Substituindo os valores NA por 0 
diasNovembro[is.na(diasNovembro)] <- 0

###### Agrupando pelo dia em 2 Fatores: Energia Predita e Observada
grupoNov <- melt(data = diasNovembro, id.vars = "dia_mes_ano", 
                 measure.vars = c("Predita", "Observada"))

###### Renomeando o Fator "variable" para "Energia" 
names(grupoNov)[names(grupoNov) == "variable"] <- "Energia"

###### Plotagem do Gráfico de Novembro
gf_predNov <- ggplot(data = grupoNov, 
                     aes(x=dia_mes_ano, y = value, fill = Energia)) + 
  theme_ipsum() + geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 50*0:900, name = "Energy") +
  scale_x_date(name="Days", labels = date_format("%m-%d-%Y"), 
               breaks = date_breaks("days"), expand = c(0,0)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, color = 'black', size=16),
        axis.text.x = element_text(angle = 90, size=12),
        axis.title.x = element_text(hjust = 0.5, size=16) )+ 
  labs(fill = "Energy:") +
  scale_fill_discrete(name = "Energy", labels = c("Predicted", "Observed")) +
  ggtitle("Days - November/19") 

plot(gf_predNov)
dev.off()



##### 2.3 - Mensais - Dezembro/19 ####
nome_png <- paste ("3_Dez19_energia_completo.png")
dest_png <- paste(cam_graph5, nome_png, sep = "")
png(filename = dest_png, width = 720, height = 480, units = 'px')

###### Criação da lista com os dias de Dezembro
diasMes <- numeric()
diasMes <- data.frame("dia" = seq(as.Date("2019/12/1"), 
                                  as.Date("2019/12/31"), "days"))

###### Separação dos dias de Dezembro presentes no dataset
diasDezembro <- df.Ajustado[34:39,]

###### Combinando os os dias de Novembro do dataset com os dias mensais
diasDezembro <- merge(diasDezembro, diasMes, 
                      by.x = "dia_mes_ano", by.y = "dia", 
                      all.x = TRUE, all.y = TRUE)

###### Substituindo os valores NA por 0 
diasDezembro[is.na(diasDezembro)] <- 0

###### Agrupando pelo dia em 2 Fatores: Energia Predita e Observada
grupoDez <- melt(data = diasDezembro, id.vars = "dia_mes_ano", 
                 measure.vars = c("Predita", "Observada"))

###### Renomeando o Fator "variable" para "Energia" 
names(grupoDez)[names(grupoDez) == "variable"] <- "Energia"

###### Plotagem do Gráfico de Dezembro
gf_predDez <- ggplot(data = grupoDez , 
                     aes(x=dia_mes_ano, y = value, fill = Energia)) + 
  theme_ipsum() + geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 50*0:900, name = "Energy") +
  scale_x_date(name="Days", labels = date_format("%m-%d-%Y"), 
               breaks = date_breaks("days"), expand = c(0,0)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, color = 'black', size=16),
        axis.text.x = element_text(angle = 90, size=12),
        axis.title.x = element_text(hjust = 0.5, size=16)) + 
  
  labs(fill = "Energy:") +
  scale_fill_discrete(name = "Energy", labels = c("Predicted", "Observed")) +
  ggtitle("Days - December/19") 

plot(gf_predDez)
dev.off()



##### 2.4 - Mensais - Janeiro/20 ####
nome_png <- paste ("4_Jan20_energia_completo.png")
dest_png <- paste(cam_graph5, nome_png, sep = "")
png(filename = dest_png, width = 720, height = 480, units = 'px')

###### Criação da lista com os dias de Janeiro
diasMes <- numeric()
diasMes <- data.frame("dia" = seq(as.Date("2020/1/1"), 
                                  as.Date("2020/1/31"), "days"))

###### Separação dos dias de Janeiro presentes no dataset
diasJaneiro <- df.Ajustado[40:50,]

###### Combinando os os dias de Janeiro do dataset com os dias mensais
diasJaneiro <- merge(diasJaneiro, diasMes, by.x = "dia_mes_ano", 
                     by.y = "dia", all.x = TRUE, all.y = TRUE)

###### Substituindo os valores NA por 0 
diasJaneiro[is.na(diasJaneiro)] <- 0

###### Agrupando pelo dia em 2 Fatores: Energia Predita e Observada
grupoJan <- melt(data = diasJaneiro, id.vars = "dia_mes_ano", 
                 measure.vars = c("Predita", "Observada"))

###### Renomeando o Fator "variable" para "Energia" 
names(grupoJan)[names(grupoJan) == "variable"] <- "Energia"

###### Plotagem do Gráfico de Janeiro
gf_predJan <- ggplot(data = grupoJan , 
                     aes(x=dia_mes_ano, y = value, fill = Energia)) +  
  theme_ipsum() + geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 50*0:900, name = "Energy") +
  scale_x_date(name="Days", labels = date_format("%m-%d-%Y"), 
               breaks = date_breaks("days"), expand = c(0,0)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, color = 'black', size=16),
        axis.text.x = element_text(angle = 90, size=12),
        axis.title.x = element_text(hjust = 0.5, size=16)  ) + 
  labs(fill = "Energy:") + 
  scale_fill_discrete(name = "Energy", labels = c("Predicted", "Observed")) +
  ggtitle("Days - January/20") 

plot(gf_predJan)
dev.off()



##### 2.5 - Mensais - Fevereiro/20 ####
nome_png <- paste ("5_Fev20_energia_completo.png")
dest_png <- paste(cam_graph5, nome_png, sep = "")
png(filename = dest_png, width = 720, height = 480, units = 'px')

###### Criação da lista com os dias de Fevereiro
diasMes <- numeric()
diasMes <- data.frame("dia" = seq(as.Date("2020/2/1"), 
                                  as.Date("2020/2/29"), "days"))
###### Separação dos dias de Fevereiro presentes no dataset    
diasFevereiro <- df.Ajustado[51:66,]

###### Combinando os os dias de Novembro do dataset com os dias mensais
diasFevereiro <- merge(diasFevereiro, diasMes, by.x = "dia_mes_ano", 
                       by.y = "dia", all.x = TRUE, all.y = TRUE)

###### Substituindo os valores NA por 0 
diasFevereiro[is.na(diasFevereiro)] <- 0

###### Agrupando pelo dia em 2 Fatores: Energia Predita e Observada
grupoFev <- melt(data = diasFevereiro, id.vars = "dia_mes_ano", 
                 measure.vars = c("Predita", "Observada"))

###### Renomeando o Fator "variable" para "Energia" 
names(grupoFev)[names(grupoFev) == "variable"] <- "Energia"

###### Plotagem do Gráfico de Fevereiro
gf_predFev <- ggplot(data = grupoFev , 
                     aes(x=dia_mes_ano, y = value, fill = Energia)) +  
  theme_ipsum() + geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 50*0:900, name = "Energy") +
  scale_x_date(name="Days", labels = date_format("%m-%d-%Y"), 
               breaks = date_breaks("days"), expand = c(0,0)) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(hjust = 0.5, color = 'black', size=16),
        axis.text.x = element_text(angle = 90, size=12),
        axis.title.x = element_text(hjust = 0.5, size=16) ) + 
  labs(fill = "Energy:") + 
  scale_fill_discrete(name = "Energy", labels = c("Predicted", "Observed")) +
  ggtitle("Days - February/20") 

plot(gf_predFev)
dev.off()



##### 2.6 - Mensais - Março/20 ####
nome_png <- paste ("6_Mar20_energia_completo.png")
dest_png <- paste(cam_graph5, nome_png, sep = "")
png(filename = dest_png, width = 720, height = 480, units = 'px') 

###### Criação da lista com os dias de Março
diasMes <- numeric()
diasMes <- data.frame("dia" = seq(as.Date("2020/3/1"), as.Date("2020/3/31"), "days"))

###### Separação dos dias de Março presentes no dataset
diasMarco <- df.Ajustado[67:88,]

###### Combinando os os dias de Março  do dataset com os dias mensais
diasMarco <- merge(diasMarco, diasMes, by.x = "dia_mes_ano", by.y = "dia", all.x = TRUE, all.y = TRUE)

###### Substituindo os valores NA por 0 
diasMarco[is.na(diasMarco)] <- 0

###### Agrupando pelo dia em 2 Fatores: Energia Predita e Observada
grupoMar <- melt(data = diasMarco, id.vars = "dia_mes_ano", measure.vars = c("Predita", "Observada"))


###### Renomeando o Fator "variable" para "Energia" 
names(grupoMar)[names(grupoMar) == "variable"] <- "Energia"

###### Plotagem do Gráfico de Novembro
gf_predMar <- ggplot(data = grupoMar , 
                     aes(x=dia_mes_ano, y = value, fill = Energia) ) +  
  theme_ipsum() + geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 50*0:900, name = "Energy") +
  scale_x_date(name="Days", labels = date_format("%m-%d-%Y"), 
               breaks = date_breaks("days"), expand = c(0,0)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, color = 'black', size=16),
        axis.text.x = element_text(angle = 90, size=12),
        axis.title.x = element_text(hjust = 0.5, size=16)) + 
  labs(fill = "Energy:") + 
  scale_fill_discrete(name = "Energy", labels = c("Predicted", "Observed")) +
  ggtitle("Days - March/20") 

plot(gf_predMar)
dev.off()



##### 2.7 - Mensais - Abril/20 ####
nome_png <- paste ("7_Abr20_energia_completo.png")
dest_png <- paste(cam_graph5, nome_png, sep = "")
png(filename = dest_png, width = 720, height = 480, units = 'px') 

###### Criação da lista com os dias de Abril
diasMes <- numeric()
diasMes <- data.frame("dia" = seq(as.Date("2020/04/01"), 
                                  as.Date("2020/04/30"), "days"))

###### Separação dos dias de Abril presentes no dataset
diasAbril <- df.Ajustado[89:105,]

###### Combinando os os dias de Abril do dataset com os dias mensais
diasAbril <- merge(diasAbril, diasMes, by.x = "dia_mes_ano",
                   by.y = "dia", all.x = TRUE, all.y = TRUE)

###### Substituindo os valores NA por 0 
diasAbril[is.na(diasAbril)] <- 0

###### Agrupando pelo dia em 2 Fatores: Energia Predita e Observada
grupoAbr <- melt(data = diasAbril, id.vars = "dia_mes_ano", 
                 measure.vars = c("Predita", "Observada"))

###### Renomeando o Fator "variable" para "Energia" 
names(grupoAbr)[names(grupoAbr) == "variable"] <- "Energia"

###### Plotagem do Gráfico de Novembro
gf_predAbr <- ggplot(data = grupoAbr , 
                     aes(x=dia_mes_ano, y = value, fill = Energia)) +  
  theme_ipsum() + geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 50*0:900, name = "Energy") +
  scale_x_date(name="Days", labels = date_format("%m-%d-%Y"), 
               breaks = date_breaks("days"), expand = c(0,0)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, color = 'black', size=16),
        axis.text.x = element_text(angle = 90, size=12),
        axis.title.x = element_text(hjust = 0.5, size=16) ) + 
  labs(fill = "Energy:") +
  scale_fill_discrete(name = "Energy", labels = c("Predicted", "Observed")) +
  ggtitle("Days - April/20") 

plot(gf_predAbr)
dev.off()

dev.off()





