#Lista 5
#Foi realizado no município Três Lagoas/MS um inventário florestal em 
#um povoamento clonal de Eucalyptus grandis W. Hill ex Maiden x Eucalyptus urophylla 
#S. T. Blake. A idade média deste povoamento era de 2,88 anos e possuía 
#uma área de 1226 ha. As alturas totais foram mensuradas utilizando o suunto e a correção 
#destas deve ser realizada a partir das declidades(%) médias das parcelas. 
#De posse dos dados brutos (fustes.csv) deste inventário, pede-se: (100%)
fustes <- read.csv2("fustes.csv")
fustes <- na.omit(fustes)
#Questão a) Apenas para a variável diâmetro à altura do peito (dap>0 e dap!=NULO) e 
#considerando a união de todas as parcelas, calcule:
dap <- na.omit(fustes$dap)
#1)  DAP mínimo
min(dap)
#2)  DAP 1º quartil
boxplot(dap)
summary(dap)
#ou
quantile(dap, 0.25)
#3)  DAP médio
summary(dap)
#ou
dap_med <- mean(dap)
#4)  DAP médio quadrático
gmedio <- mean((pi*dap^2)/40000)
#gmedio em m por planta 
dg <- sqrt(40000*gmedio/pi)
#ou
dg <- sqrt(sum(dap^2)/length(dap))
#ou
dap_varp <- sum((dap-dap_med)^2)/length(dap)
dg <- sqrt(dap_med^2+dap_varp)
#5  DAP mediano
median(dap)
#6  DAP 3° quartil
summary(dap)
#ou
quantile(dap, 0.75)
#7 diametro médio das dominantes
#selecionando as árvores dominantes:
dominantes <- fustes[fustes$cat=="Dominante",]
mean(dominantes$dap)
#8) dap máximo [cm]
max(dap)
#9) Coeficiente de variação [%]
cvdap <- sd(dap)/mean(dap)*100
#10) Curtose (-1:Platicúrtica, 0:Mesocúrtica, 1:Leptocúrtica)
library(fBasics)
#Curtose == 3: Mesocúrtica (semelhante a distribuição normal)
#Curtose < 3: Platicúrtica (mais achatada que a distribuição normal)
#Curtose >3: Leptocúrtica (menos achatada que a distribuição normal)
kurtosis(dap)
#Platicúrtica, logo, resposta = -1
#11) Assimetria: (-1:Assimetria negativa, 0:Simétrica, 1:Assimetria positiva)
# 0: Distribuição simétrica
# < 0: Assimetria negativa (Assimetria a esquerda)
# > 0: Assimetria positiva (Assimétrica a direita)
skewness(dap)
#assimetria negativa, logo, reposta= -1
#12) Área basal por hectare [m2]    
#    área em m^2*parcelas -----  soma_g
#      10000 m^2          -----   x 
g <- (pi*(dap^2))/40000
soma_g <- sum(g)
parcelas <- length(unique(fustes$parcela))
area_basal_ha <- (soma_g*10000)/(469.9*parcelas)
#13) Área basal do povoamento [m2]
area_basal_ha*1226

#Questão (b) Apenas para a variável altura total corrigida (htc>0 e htc!=NULO) e 
#considerando a união de todas as parcelas, calcule:
#corrigindo as alturas: ## comparar pra ver se o resultado é igual
for (i in 1:length(fustes$decliv_perc)){
  if((fustes$decliv_perc[i] > 7)){
    fustes$ht[i] <- (cos(atan((fustes$decliv_perc[i]/100))))*(fustes$ht[i])
  }
}
ht <- na.omit(fustes$ht)
#ou
x <- fustes[fustes$decliv_perc>7,]
y <- fustes[fustes$decliv_perc<7,]
x$ht <- (cos(atan((x$decliv_perc/100))))*(x$ht)
fustes1 <- rbind(x,y)
ht <- na.omit(fustes1$ht)
#1) htc mínima [m]
min(ht)
#2) htc 1.o quartil [m]
summary(ht)
#ou
quantile(ht, 0.25)
#3) htc média [m]
mean(ht)
#4) htc mediana [m]
median(ht)
#5) htc 3.o quartil [m]
summary(ht)
#ou
quantile(ht, 0.75)
#6) Altura média das dominantes [m]
dominantes <- fustes1[fustes1$cat=="Dominante",]
mean_ht <- mean(dominantes$ht)
#7) htc máxima [m]
max(ht)
#8) Coeficiente de variação [%]
cv_ht <- sd(ht)/mean(ht)*100
#9) Curtose (-1:Platicúrtica, 0:Mesocúrtica, 1:Leptocúrtica)
#Curtose == 3: Mesocúrtica (semelhante a distribuição normal)
#Curtose < 3: Platicúrtica (mais achatada que a distribuição normal)
#Curtose >3: Leptocúrtica (menos achatada que a distribuição normal)
kurtosis(ht)
#0.38 logo, é platicúrtica resposta = -1
#10) Assimetria: (-1:Assimetria negativa, 0:Simétrica, 1:Assimetria positiva)
# 0: Distribuição simétrica
# < 0: Assimetria negativa (Assimetria a esquerda)
# > 0: Assimetria positiva (Assimétrica a direita)
skewness(ht)
#0.14 logo, a assimetria é positiva, assimétrica à esquerda, isso significa que 
# existem muitas árvores abaixo do esperado

#(c) Apenas para a variável volume total com casca (vtcc>0 e vtcc!=NULO) e 
#considerando a união de todas as parcelas, calcule:

#1) vtcc mínimo [m3]
ffcc <- na.omit(fustes1$ffcc)
fustes1$vtcc <- ht*(pi*dap^2/40000)*ffcc 
vtcc <- fustes1$vtcc
#2) vtcc mínimo
min(vtcc)
#3) vtcc 1.o quartil [m3]
summary(vtcc)
#ou
quantile(vtcc, 0.25)
#4) vtcc médio [m3]
mean(vtcc)
#5) vtcc mediano [m3]
median(vtcc)
#6) vtcc 3.o quartil [m3]
summary(vtcc)
#ou
quantile(vtcc, 0.75)
#7) vtcc máximo [m3]
max(vtcc)
#8) Coeficiente de variação [%]
cv_vtcc <- sd(vtcc)/mean(vtcc)*100
#9) Curtose (-1:Platicúrtica, 0:Mesocúrtica, 1:Leptocúrtica)
#Curtose == 3: Mesocúrtica (semelhante a distribuição normal)
#Curtose < 3: Platicúrtica (mais achatada que a distribuição normal)
#Curtose >3: Leptocúrtica (menos achatada que a distribuição normal)
kurtosis(vtcc)
#0.86 logo, é Platicúrtica e a resposta= -1
#10) Assimetria: (-1:Assimetria negativa, 0:Simétrica, 1:Assimetria positiva)
# 0: Distribuição simétrica
# < 0: Assimetria negativa (Assimetria a esquerda)
# > 0: Assimetria positiva (Assimétrica a direita)
skewness(vtcc)
#0.04 logo, é uma assimetria positiva e a resposta= 1
#11) vtcc por hectare [m3]
# 469.9*60 m^2 ----- soma_vtcc
#  10 000  m^2 ----- x
soma_vtcc <- sum(vtcc)
volume_ha <- (10000*soma_vtcc)/(469.9*60)
#12) vtcc do povoamento [m3]
volume_ha*1226
#13) Incremento médio anual [m3/ha.ano]
#= volume total/ idade* área_total
(volume_ha*1226)/(2.88*1226) 
 


library(ggplot2)
#(d) Faça o histograma para as variáveis dap, htc e vtcc
hist_dap <- ggplot(fustes1,aes(x=dap)) + 
  geom_histogram(aes(y=..density..),colour= "black", fill="lightblue")
hist_dap +
  xlab("DAP [cm]")+
  ylab("Densidade")+
  labs(title="Histograma da Densidade de Frequência")+
  theme(plot.title = element_text(hjust = 0.5));

hist_htc <- ggplot(fustes1,aes(x=ht)) + 
  geom_histogram(aes(y=..density..), colour= "black", fill="lightblue")
hist_htc +
  xlab("Altura Total [m]")+
  ylab("Densidade")+
  labs(title="Histograma da Densidade de Frequência")+
  theme(plot.title = element_text(hjust = 0.5))
  
hist_vtcc <- ggplot(fustes1,aes(x=vtcc)) + 
  geom_histogram(aes(y=..density..), colour= "black", fill="lightblue")
hist_vtcc +
  xlab("Volume Total com Casca [m^3]")+
  ylab("Densidade")+
  labs(title="Histograma da Densidade de Frequência")+
  theme(plot.title = element_text(hjust = 0.5))
  
#(e) Faça o gráfico quantil-quantil (Distribuição normal) para as variáveis dap, htc e vtcc.
par(mfrow= c(1,3))
qqnorm(dap, xlab="Quantis Teóricos", ylab= "Quantis de DAP", main = "Q-Q Normal do DAP")
qqline(dap, col = 2)
qqnorm(ht, xlab="Quantis Teóricos", ylab= "Quantis de ht",main = "Q-Q Normal da Altura Total")
qqline(ht, col = 2)
qqnorm(vtcc, xlab = "Quantis Teóricos", ylab= "Quantis de vtcc", main = "Q-Q Normal do Volume Total com Casca")
qqline(vtcc, col = 2)
#(f) Faça o diagrama de caixa (boxplot) para as variáveis dap, htc e vtcc
par(mfrow= c(1,3))
boxplot(dap, ylab= "dap[cm]", main= "Diagrama de caixa do DAP", col= "red")
boxplot(ht, ylab= "ht[m]", main= "Diagrama de caixa da Altura Total", col= "green")
boxplot(vtcc, ylab= "vtcc[m^3]", main= "Diagrama de caixa do Volume Total com Casca", col= "yellow")
