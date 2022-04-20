#exemplo_prova
tabela <- data.frame(li=c(NA,NA,NA,NA,NA,23),
                     ls=c(NA,NA,NA,NA,NA,NA),
                     vc=NA, fo=c(NA,NA,1438,NA,521,NA),
                     fa=c(91,NA,NA,NA,4708,NA), 
                     fr=c(NA,NA,0.3039,0.4793,NA,NA),
                     df=c(NA,NA,0.0760,NA,NA,NA),
                     fo.vc=NA, g=NA, fo.g=NA,
                     ht=c(11.81,18.53,23.2,26.1,28.58,30.56),
                     v=NA, fo.v=NA)

intervalo_classe <- 4 
tabela$li <- seq(3,23,4)
tabela$ls <- seq(7,27,4)

tabela$vc <- (tabela$ls+tabela$li)/2

#fr = fo/sum(fo)
#sum(fo) = fo/fr
tabela$fa[6] <- tabela$fo[3]/tabela$fr[3]
tabela$fa <- round(tabela$fa, digits=0)

tabela$fo[6] <- tabela$fa[6]-tabela$fa[5]

tabela$fo[4] <- tabela$fr[4]*tabela$fa[6] 
tabela$fo <- round(tabela$fo, digits=0)

tabela$fo[1] <- tabela$fa[1]

tabela$fo[2] <- 0

tabela$fo[2] <- tabela$fa[6]-sum(tabela$fo)

tabela$fa[2] <- sum(tabela$fo[1:2])
tabela$fa[3] <- sum(tabela$fo[1:3])
tabela$fa[4] <- sum(tabela$fo[1:4])

tabela$fr <- tabela$fo/tabela$fa[6]
tabela$fr <- round(tabela$fr, digits=4)

tabela$df <- tabela$fr/intervalo_classe
tabela$df <- round(tabela$df, digits=4)

tabela$fo.vc <- tabela$fo*tabela$vc

tabela$g <- (tabela$vc^2)*pi/40000
tabela$g <- round(tabela$g, digits=5)

tabela$fo.g <- tabela$fo*tabela$g
tabela$fo.g <- round(tabela$fo.g, digits=5)

tabela$v <- tabela$ht*tabela$g*0.45
tabela$v <- round(tabela$v, digits=5)

tabela$fo.v <- tabela$fo*tabela$v
tabela$fo.v <- round(tabela$fo.v, digits=5)


library(ggplot2)

names <- c("classe 1", "classe 2", "classe 3", "classe 4", "classe 5", "classe 6")
tabela <- cbind(Classes= names, tabela)


barras <- ggplot(data= tabela, aes(x=Classes, y=fo),boundary=0, colour="black",fill="lightblue") 
barras + geom_bar(fill = "lightblue", stat ="identity", colour= "black") + 
  xlab("Classes")+
  ylab("Frequência")+
  scale_y_continuous(limits = c(0,2400), breaks=seq(0,2400,200))+ #mexer nessa linha com base na > fo
labs(title= "Tabela das Frequências Observadas")+
  theme(plot.title = element_text(hjust = 0.5))

# Para montar o histograma é necessário supor os dados brutos quando não os possui,
#desse modo, iremos supor esses valores com base na variável vc e fo:
dap_hist <- c(rep(tabela$vc,tabela$fo));           #mexer nessa linha
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
diametro_medio <- sum(tabela$fo.vc)/sum(tabela$fo)

#Diâmetro modal:
tabela[which.max(tabela$fo),]
#vc dessa linha é o diâmetro modal

#Diâmetro médio quadrático:
#(somatório de fo.g / somatório de fo)
gmedio <- sum(tabela$fo.g)/sum(tabela$fo)
dg <- sqrt((gmedio*40000)/pi)


#isso aqui é uma aproximação, pois n tenho os DAPs exatos:
#(somatória de DAP^2/fa)
dg_demonstracao <- sqrt(sum(dap_hist1$DAP^2)/tabela$fa[6])

#ou outra aproximação:
dap_varp <- sum((dap_hist1$DAP-diametro_medio)^2)/length(dap_hist1$DAP)
dg_demonstracao1 <- sqrt(diametro_medio^2+dap_varp)

#D)

#área basal por hectare e do povoamento

#área basal por ha:

#    parcelas*m^2  ------- sum(fo.g)       
#       10 000 m^2 ------- x               x= ____ m^2/ha

soma_fo.g <- sum(tabela$fo.g)
area_basal_ha <- (soma_fo.g*10000)/(65*500)

#área basal por povoamento (área basal total existente na área):

# área basal por ha * tamanho da área em ha = ____ m^2/pop

basal_povoamento <- area_basal_ha * 1005  

#E)

#Utilizando um fator de forma de 0,45, qual é o volume de madeira médio por árvore,
#por hectare e total existente na área (Volume do povoamento)?

#volume médio de madeira por ha:

#     parcelas*m^2 -------- sum(fo.v)
#        10 000m^2 -------- x                       x= ____ m^3/ha 

soma_fo.v <- sum(tabela$fo.v)
volume_ha <- (soma_fo.v*10000)/(65*500)

#volume médio de madeira por árvore:

#        N árvores -------- sum(fo.v)
#        1 árvore  -------- x                       x= ____ m^3/árvore 

volume_arvore <- (soma_fo.v*1)/tabela$fa[6]

#volume total existente na área:

#   volume por ha * tamanho da área em ha =               x= ____ m^3/pop

volume_povoamento <- volume_ha * 1005
