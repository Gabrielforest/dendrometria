#Encontro 4 análise estatístic dos diâmetros coletados de 1 inv. de Eucalyptus sp.
#aos 9 anos de idade no estado de Minas Gerais

library(tidyverse)

fuste <- read.csv2("fustes.csv")
View(fuste)

unique(fuste$matgen) 

#tds dap <- dap
#obs. arv. nulas = árvores mortas:
dap <- fuste$dap
dap <- na.omit(dap)

nclasses <- nclass.Sturges(dap)
#ou (ceiling arredonda pra cima):
nclasses <- ceiling(1+3.3333*log10(length(dap)))

#direita=false é exclusive a direita (aberta), mas o último é inclusive (fechado) 
#por conta do lowest function
#criando classes comando cut: 
cldap <- cut(dap, breaks = nclasses, right = F, include.lowest = T)

#tab freq.:
tabfreq <- table(cldap)

#ddply faaz isso tbm:
tabfreq <- data.frame(classes= row.names(tabfreq),fo=as.vector(tabfreq))

#freq relativa:
tabfreq$fr <- tabfreq$fo/sum(tabfreq$fo)
#soma tem q dar 1:
sum(tabfreq$fr)

#range retorna o menor e o maior valor, 
#e diff retorna a diferença entre 2 valores sequenciais de um vetor
#assim temos 
intervalo_classe <- diff(range(dap,na.rm=T))/nclasses

#densidade de freq:
tabfreq$df <- tabfreq$fr/intervalo_classe

#tirando os NA:
dap <- dap[!is.na(dap)]

#dados brutos:

dap_med <- mean(dap, na.rm = T); #media aritmética
dap_sd <- sd(dap, na.rm =T); #desvio padrão
dap_var <- var(dap, na.rm=T); #variância amostral
dap_median <- median(dap, na.rm = T); #mediana

gmedio <- mean((pi*dap^2)/40000, na.rm=T)
#gmedio m por planta 
dg <- sqrt(40000*gmedi/pi)
#ou
dg <- sqrt(sum(dap^2)/length(dap))
#ou
dap_varp <- sum((dap-dap_med)^2)/length(dap)
dg <- sqrt(dap_med^2+dap_varp);

#dados agrupados:
tabfreq$vc <- NA
tabfreq$vc[1] <- min(dap)+intervalo_classe/2
for(i in 2:nrow(tabfreq)) tabfreq$vc[i] <- tabfreq$vc[i-1]+intervalo_classe


dap_med_agrupado <- with(tabfreq,sum(fo*vc)/sum(fo))

tabfreq$g <- (pi*tabfreq$vc^2)/40000 

gmedio_agrupado <- with(tabfreq,sum(fo*g)/sum(fo))

dg_agrupado <- sqrt(40000*gmedio_agrupado)


par(mfrow=c(1,2))
hist(x=dap, probability = T, main = "", xlab= "DAP[cm]", 
     ylab= "densidade frequência", col= "red")

barplot.default (x=dap, probability = F, main = "", xlab= "DAP[cm]", 
     ylab= "frequência", col= "red")

par(mfrow=c(1,1))
hist(x=dap, probability = T, main = "", xlab= "DAP[cm]", 
     ylab= "densidade frequência", col= "yellow")

x <- dap;
curve(dnorm(x,dap_med,dap_sd),col="red", add="T")


#gráficos de caixa
#objetivos principais: presença de outliers(quanto mais variáveis mias haverá)
#se as vars
boxplot(dap)
points(dap_med, pch="x",col= "red")

#dap variando pra cada material genético:
boxplot(fuste$dap~fuste$matgen, xlab="material genético", ylab="dap(cm)",col=2:5)

#Gráfico quantil quantil (teórico e observado):
#se ondular foge a distribuição normal
#normal igual reta
qqnorm(dap)

library(fBasics)

qqnormPlot(dap)
#só até 5000 pts, por isso fez sorteio para esses 4900 pts
shapiro.test(sample(dap,4900))
ks.test(dap, "pnorm", dap_med, dap_sd); #Kolmogorov Smirnov

##curtose
kurtosis(dap);
#Curtose == 3: Mesocúrtica (semelhante a distribuição normal)
# Curtose < 3: Platicúrtica (mais achatada que a distribuição normal)
#Curtose >3: Leptocúrtica (menos achatada que a distribuição normal)


#Assimetria
skewness(dap)
# 0: Distribuição simétrica
# < 0: Assimetria negativa (Assimetria a esquerda)
# > 0: Assimetria positiva (Assimétrica a direita)












