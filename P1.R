#P1

tabela <- data.frame(li=c(NA,NA,NA),
                     ls=c(NA,NA,NA),
                     vc=NA, fo=c(NA,NA,NA),
                     fa=c(3293,6519,6984), 
                     fr=c(NA,NA,NA),
                     df=c(NA,NA,0.0166),
                     fo.vc=c(NA,NA,NA), g=NA, fo.g=c(NA,82.10170,NA),
                     ht=c(24.03,26.76,28.96),
                     v=NA, fo.v=NA)

#(a) Completar a tabela 1 (li e ls devem ser números inteiros e não é necessário indicar os cálculos).
tabela$fo[3] <- tabela$fa[3]-tabela$fa[2]
tabela$fo[2] <- tabela$fa[2]-tabela$fa[1]
tabela$fo[1] <- tabela$fa[1]
sum(tabela$fo)

#fr=fo/sum(fo)
tabela$fr <- tabela$fo/tabela$fa[3]
tabela$fr <- round(tabela$fr, digits=5)

#df=fr/intervalo_classe
nclasses <- 3
intervalo_classe <- tabela$fr[3]/tabela$df[3]
intervalo_classe <- round(intervalo_classe, digits=0)

#g= fo.g/g
tabela$g[2] = tabela$fo.g[2]/tabela$fo[2]
#vc[2] = sqrt(g*40000/pi)
tabela$vc[2] <- sqrt(tabela$g[2]*40000/pi)
tabela$vc[2] <- round(tabela$vc[2],digits = 0)

#vc= li+ls/2
tabela$li[2] <- 16
tabela$ls[2] <- 20

tabela$li[1] <- 12 
tabela$li[3] <- 20 

tabela$ls[1] <- 16
tabela$ls[3] <- 24

#vc = li+ls/2
tabela$vc <- (tabela$li+tabela$ls)/2

#df=fr/intervalo_classe
tabela$df <- tabela$fr/intervalo_classe
tabela$df <- round(tabela$df, digits=4)

#fo.vc
tabela$fo.vc <- tabela$fo*tabela$vc 

#g = pi*vc^2/40000
tabela$g <- (pi*(tabela$vc^2))/40000
tabela$g <- round(tabela$g, digits=5)

#fo.g
tabela$fo.g <- tabela$fo*tabela$g

#v= ht*g*ff
tabela$v <- 0.44*tabela$ht*tabela$g 
tabela$v <- round(tabela$v, digits=5)

#fo.v = 
tabela$fo.v <- tabela$fo*tabela$v

#(b) Diâmetro médio, diâmetro médio quadrático, valor central da classe que contém o diâmetro mediano e valor
#central da classe que contém o diâmetro modal.
diametro_medio <- sum(tabela$fo.vc)/sum(tabela$fo)
round(diametro_medio, digits = 2)
valor_central <- 18

#Diâmetro modal:
tabela[which.max(tabela$fo),]
#vc dessa linha é o diâmetro modal
#modal = 14

gmedio <- sum(tabela$fo.g)/sum(tabela$fo)
dg <- sqrt((gmedio*40000)/pi)


#A idade deste povoamento era de 6,3 anos e possuía
#uma área 1284 ha. A partir das 115 parcelas de 500 m2
#(c) Área basal por hectare.

#área basal por ha:

#    parcelas*m^2  ------- sum(fo.g)       
#       10 000 m^2 ------- x               x= ____ m^2/ha

soma_fo.g <- sum(tabela$fo.g)
area_basal_ha <- (soma_fo.g*10000)/(115*500)

#área basal por povoamento (área basal total existente na área):

# área basal por ha * tamanho da área em ha = ____ m^2/pop

basal_povoamento <- area_basal_ha * 1284  

hc <- ((27.76-3.36)/23)*27.56+(1.46*0.992882604)
round(hc, digits = 2)
#(d) Utilizando um fator de forma de 0, 44, qual é o volume de madeira médio por árvore, por hectare e total existente
#na área (Volume do povoamento)?]

#volume médio de madeira por ha:

#     parcelas*m^2 -------- sum(fo.v)
#        10 000m^2 -------- x                       x= ____ m^3/ha 

soma_fo.v <- sum(tabela$fo.v)
volume_ha <- (soma_fo.v*10000)/(115*500)

#volume médio de madeira por árvore:

#        N árvores -------- sum(fo.v)
#        1 árvore  -------- x                       x= ____ m^3/árvore 

volume_arvore <- (soma_fo.v*1)/tabela$fa[3]

#volume total existente na área:

#   volume por ha * tamanho da área em ha =               x= ____ m^3/pop

volume_povoamento <- volume_ha * 1284


#Num ponto de estação, para medida de área basal com relascópio de Bitterlich, um operador 
#está usando uma constante instrumental igual a 1. Uma árvore próxima ao ponto de estação 
#não está visível, por este motivo, o operador foi até a
#árvore e mediu o seu DAP (24,07 cm) e a distância do ponto de estação (14,16 metros). Responda: (10%)

#(a) Ela será contada? Justifique.
R <- sqrt((0.2407^2)/(1/2500))
#Não será contada, pois está fora da prova de numeração angular máxima,
#ou seja, R < distância do PE (ponto de estação)

#(b) Qual é a constante instrumental (K) que tangencia esta árvore?

K <- 2500*((0.2407/14.16)^2)

#(c) Qual é a área basal por ha considerando que no giro de 3600
#foram contadas 9 árvores?
G_ha <- 1*9
#9 m^2/ha  

#(d) Qual a área da parcela delimitada por esta árvore?
#Essa árvore não delimita parcela

#Na tabela 2 podem ser observadas as medições das 8 árvores mais grossas de uma parcela de 400m2

#(os sinais positivos
# e negativos significam simplesmente leituras em situação de aclive e declive, respectivamente). 
#Qual é a altura média das árvores dominantes para esta parcela? 
#Justifique a sua resposta. (20%)

#Tabela 2: Medições das alturas de algumas árvores em diferentes condições de relevo e escalas/distâncias da árvore

#Árvore DAP    Instrumento               Leitura na base    Leitura no ápice  Escala Distância da árvore Declividade
#1      29,61  Prancheta Dendrométrica * −1,40cm              7, 2cm          cm      30m                0,42°
#2      30,94  Blume-Leiss               −5,42m               +18,50m         25m     30m                8,00°
#3      31,71  Haga                      −1,50m               +25,90m         30m     31m                4,00%
#4      30,10  Suunto                    −11,60°              +39,50°         graus   28m                7,00°
#5      31,64  Haga                      +4,20m               +32,70m         25m     25m                12,00%
#6      31,60  Suunto                    +3,30m               +24,50m         20m     28m                15,00%
#7      29,61  Suunto                    −12,00%              +88,00%         %       29m                2,14%
#8      31,14  Haga                      −52,96m              −0,25m          30m     29m                56,65°

#*      Largura da prancheta = 9cm

#Declividade superior a 4 graus ou 7% deve ser corrigida
#para corrigir é só multiplicar fazendo as devidas alterações

#as alterações necessárias para corrigir a porcentagem e graus elevados são:
#porcentagem: multiplicar por cos(atan(__)) 
#graus: multiplicar por cos(__*pi/180)


arvore1 <- (30*(0.014+0.072))/0.09

arvore2 <- ((5.42+18.5)/25)*(30*cos(8*pi/180))

arvore3 <- ((25.90+1.50)/30)*31

arvore4 <- (tan(11.60*pi/180)+tan(39.5*pi/180))*(28*cos(7*pi/180))

arvore5 <- ((32.70-4.20)/25)*(25*(cos(atan(0.12))))

arvore6 <- ((24.50-3.30)/20)*(28*(cos(atan(0.15))))

arvore7 <- (0.88+0.12)*29

arvore8 <- ((52.96-0.25)/30)*(29*(cos(56.65*pi/180)))

sort()
media <- mean(c(arvore5, arvore3, arvore6, arvore8))
round(media, digits= 2)

#5
#Na tabela 3 são apresentadas as variáveis mensuradas na cubagem rigorosa do fuste de uma árvore da espécie Eucalyp-
# tus sp. (20%)

#Tabela 3: Cubagem do fuste de uma árvore da espécie Eucalyptus sp.
#Árvore dap[cm] ht[m] hi[m] dicc[cm] espcascai[cm] disc[cm] gisc[m2] visc_smalian[m3
#                                                                               ]
#32233  24,10   29,40 0,10  28,40      1,30         XXXXX
#32233  24,10   29,40 0,70  25,80      1,00
#32233  24,10   29,40 1,30  24,10      0,70
#32233  24,10   29,40 4,00  20,20      0,60
#32233  24,10   29,40 8,00  18,10      0,40          17,30   0,02351    0,10372
#32233  24,10   29,40 12,00 15,80      0,40          15,00   0,01767    0,08236
#32233  24,10   29,40 16,00 13,20      0,40          12,40   0,01208    0,0595
#32233  24,10   29,40 20,00 10,50      0,40          9,70    0,00739    0,03894
#32233  24,10   29,40 24,00 7,00       0,40          6,20    0,00302    0,02082

#dap: diâmetro a altura do peito, ht: altura total, hi: altura da iésima seção, dicc: diâmetro da iésima seção com casca,
#disc: diâmetro da iésima seção sem casca, espcascai: espessura da casca da iésima seção,
#gisc: Área seccional sem casca da iésima seção e visc_smalian: volume sem casca da iésima seção utilizando o procedimento de Smalian

tabela1 <- data.frame(Árvore = rep(32233, 9), dap_cm = rep(24.10, 9), ht_m = rep(29.4, 9), 
                       hi_m = c(0.1,0.7,1.3,4,8,12,16,20,24),
                       dicc_cm = c(28.40,25.8,24.10 ,20.20,18.10,15.8,13.2,10.50,7.00),
                       espcascai_cm = c(1.3,1,0.7,0.6,0.4,0.4,0.4,0.4,0.4),
                       disc_cm = c(NA,NA,NA,NA, 17.3,15,12.4,9.7,6.2), 
                       gi_m2 = c(NA,NA,NA,NA,0.02351,0.01767, 0.01208,0.00739,0.00302), 
                       vismalian_m3 = c(NA,NA,NA,NA,0.10372,0.08236,0.0595,0.03894,0.02082))

#Calcule:
#(a) Volume comercial sem casca. Considere o menor diâmetro mensurado como sendo o diâmetro mínimo comercial.
tabela1$disc_cm <- tabela1$dicc_cm-(2*tabela1$espcascai_cm)
tabela1$gi_m2 <- (tabela1$disc_cm^2)*pi/40000; #pq no ex. pediu sem casca...
tabela1$gi_m2 <- round(tabela1$gi_m2, digits= 5)

tabela1$vismalian_m3[2] <- mean(tabela1$gi_m2[1:2])*(tabela1$hi_m[2]-tabela1$hi_m[1]) 
tabela1$vismalian_m3[3] <- mean(tabela1$gi_m2[2:3])*(tabela1$hi_m[3]-tabela1$hi_m[2]) 
tabela1$vismalian_m3[4] <- mean(tabela1$gi_m2[3:4])*(tabela1$hi_m[4]-tabela1$hi_m[3]) 

tabela1$vismalian_m3 <- round(tabela1$vismalian_m3, digits= 5)

volumecomercial_m3 <- sum(tabela1$vismalian_m3[2:9])

#(b) Volume total sem casca.
#vponta <- gponta*hponta/3 
#vponta <- (g_menordiametro *(ht-hi))/3
vponta <- (tabela1$gi_m2[9]*(tabela1$ht_m[9]-tabela1$hi_m[9]))/3 

#vtoco = gtoco*htoco
vtoco <- (tabela1$gi_m2[1]*tabela1$hi_m[1])

#Vtotal = Vcomercial + Vponta + Vtoco
vtotal <- volumecomercial_m3 + vponta + vtoco
round(vtotal,digits=5)
#(c) Fator de forma para volume total sem casca.
#vtotal = g*ht*ff
#v/g*ht = ff
g <- (pi*26.3^2)/40000
ff <- vtotal/(g*29.3)








