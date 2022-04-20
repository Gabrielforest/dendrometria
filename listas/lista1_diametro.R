#lista 1 clean

#A)
lista1 <- data.frame(li=c(3,NA,NA,NA,NA,NA),
                     ls=c(NA,NA,NA,NA,NA,27),
                     vc=NA, fo=c(NA,218,NA,1360,NA,14),
                     fa=c(NA,NA,NA,NA,NA,2749), 
                     fr=c(NA,NA,0.3183,NA,NA,NA),
                     df=c(0.0045,NA,NA,NA,NA,NA),
                     fo.vc=NA, g=NA, fo.g=NA,
                     ht=c(11.99,19.14,23.17,26.09,27.8,29.6),
                     v=NA, fo.v=NA)
nclasses <- 6
intervalos <- diff(range(3:27))/nclasses
lista1$li <- seq(3,23,intervalos)
lista1$ls <- seq(7,27,intervalos)

lista1$vc <- (lista1$ls+lista1$li)/2

lista1$fo[3] <- lista1$fa[6]*lista1$fr[3]
lista1$fo <- round(lista1$fo, digits = 0)

lista1$fr[1] <- intervalos*lista1$df[1]

lista1$fo[1] <- lista1$fr[1]*lista1$fa[6]
lista1$fo <- round(lista1$fo, digits = 0)

x<- sum(lista1$fo[1:4],lista1$fo[6])
lista1$fo[5] <- lista1$fa[6]-x  

lista1$fa[1] <- 49
lista1$fa[2] <- sum(lista1$fo[1:2])
lista1$fa[3] <- sum(lista1$fo[1:3])
lista1$fa[4] <- sum(lista1$fo[1:4])
lista1$fa[5] <- sum(lista1$fo[1:5])

lista1$fr <-  lista1$fo/2749
lista1$fr <- round(lista1$fr,digits = 4)

lista1$df <- lista1$fr/intervalos 
lista1$df <- round(lista1$df,digits = 4)

lista1$fo.vc <- lista1$fo*lista1$vc
lista1$g <- pi*(lista1$vc^2)/40000
lista1$g <- round(lista1$g, digits = 5)

lista1$fo.g <-  lista1$fo*lista1$g
lista1$fo.g <- round(lista1$fo.g, digits = 4)

lista1$v <- lista1$ht*lista1$g*0.45
lista1$fo.v <-  lista1$fo*lista1$v
lista1$fo.v <- round(lista1$fo.v, digits = 4)




#B)
library(ggplot2)

names <- c("classe 1", "classe 2", "classe 3", "classe 4", "classe 5", "classe 6")
lista1 <- cbind(Classes= names, lista1)


barras <- ggplot(data= lista1, aes(x=Classes, y=fo),boundary=0, colour="black",fill="lightblue") 
barras + geom_bar(fill = "lightblue", stat ="identity", colour= "black") + 
  xlab("Classes")+
  ylab("Frequência")+
  scale_y_continuous(limits = c(0,1400), breaks=seq(0,1400,200))+;#mexer nessa linha
  labs(title= "Tabela das Frequências Observadas")+
  theme(plot.title = element_text(hjust = 0.5))

# Para montar o histograma é necessário supor os dados brutos quando não os possui,
#desse modo, iremos supor esses valores com base na variável vc e fo:
dap_hist <- c(rep(lista1$vc,lista1$fo));           #mexer nessa linha
dap_hist1 <- as.data.frame.Date(dap_hist)

names(dap_hist1) <- "DAP"

histograma <- ggplot(dap_hist1,aes(x=DAP)) + 
  geom_histogram(aes(y=..density..),binwidth = 4,boundary=3, colour= "black", fill="lightblue")
histograma +
  xlab("Intervalo de Classes[cm]")+
  ylab("Densidade")+
  labs(title="Histograma da Densidade de Freqûencia")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits= c(0,27), breaks = seq(3,27,4)); #mexer nessa linha

#C)
#Diâmetro médio:
diametro_medio <- sum(lista1$fo.vc)/sum(lista1$fo)

#Diâmetro modal:
lista1[which.max(lista1$fo),]
#vc dessa linha é o diâmetro modal

#Diâmetro médio quadrático:
#(somatório de fo.g / somatório de fo)
gmedio <- sum(lista1$fo.g)/sum(lista1$fo)
dg <- sqrt((gmedio*40000)/pi)


#isso aqui é uma aproximação, pois n tenho os DAPs exatos:
#(somatória de DAP^2/fa)
dg_demonstracao <- sqrt(sum(dap_hist1$DAP^2)/lista1$fa[6])

#ou outra aproximação:
dap_varp <- sum((dap_hist1$DAP-diametro_medio)^2)/length(dap_hist1$DAP)
dg_demonstracao1 <- sqrt(diametro_medio^2+dap_varp)

#D)

#área basal por hectare e do povoamento

#área basal por ha:

#    parcelas*m^2  ------- sum(fo.g)       
#       10 000 m^2 ------- x               x= ____ m^2/ha

soma_fo.g <- sum(lista1$fo.g)
area_basal_ha <- (soma_fo.g*10000)/(39*500)

#área basal por povoamento (área basal total existente na área):

# área basal por ha * tamanho da área em ha = ____ m^2/pop

basal_povoamento <- area_basal_ha * 739  

#E)

#Utilizando um fator de forma de 0,45, qual é o volume de madeira médio por árvore,
#por hectare e total existente na área (Volume do povoamento)?

#volume médio de madeira por ha:

#     parcelas*m^2 -------- sum(fo.v)
#        10 000m^2 -------- x                       x= ____ m^3/ha 

soma_fo.v <- sum(lista1$fo.v)
volume_ha <- (soma_fo.v*10000)/(39*500)

#volume médio de madeira por árvore:

#        N árvores -------- sum(fo.v)
#        1 árvore  -------- x                       x= ____ m^3/árvore 

volume_arvore <- (soma_fo.v*1)/lista1$fa[6]

#volume total existente na área:

#   volume por ha * tamanho da área em ha =               x= ____ m^3/pop

volume_povoamento <- volume_ha * 739


write.table(lista1, file= "df.txt", sep=",", quote = FALSE, row.names = FALSE)




