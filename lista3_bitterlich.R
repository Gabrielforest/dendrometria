#Lista 3- PROVA DE NUMERAÇÃO ANGULAR DE BITTERLICH
#classroom

#Num ponto de estação, para medida de área basal com relascópio de Bitterlich, um 
#operador está usando uma constante instrumental igual a 1,5. Uma árvore próxima 
#ao ponto de estação não está visível, por este motivo, o operador foi até 
#a árvore e mediu o seu DAP (23,64 cm) e a distância do ponto de estação (13,91 metros). 

#(a) Ela será contada? Justifique.
###K=2500*(a/l)^2         ###DAP=sqrt((K/2500)*R^2)  ###R= sqrt((dap^2)/(K/2500))
#1.5=2500*(0.2364/R)^2
#1.5/2500= (0.2364^2)/(R^2)
#(1.5/2500)*R^2=(0.2364^2)
#R^2=(0.2364^2)/(1.5/2500)
###R= sqrt((dap^2)/(K/2500))
R <- sqrt((0.2364^2)/(1.5/2500))
# 9.651 m
#A árvore não será contada pois está fora da prova de numeração angular máxima,
#ou seja, R < distância do PE (ponto de estação). 

#(b) Qual é a constante instrumental (K) que tangencia esta árvore?
#K=2500(D/R)^2
K <- 2500*((0.2364/13.91)^2)
#  0.7221

#(c) Qual é a área basal por ha considerando que no giro de 360°
#foram contadas 15 árvores?
#G=N*K
G_ha <- 15*1.5
#22.5 m^2/ha

#(d) Qual a área da parcela delimitada por esta árvore?
#a área delimitada por essa árvore é = 0 m^2

#Se a árv. fosse contada seria, a área delimitada seria:
#A=pi*R^2
A <- pi*(9.651^2)
#292.613 m^2
#quantas árvores de 23,64cm de diâmetro existem por ha ?
N <- 10000/292.613 
#ou
N <- (1/(pi*(23.64^2)/40000))*1.5
#Lista 3
#email

#Num ponto de estação, para medida de área basal com relascópio de Bitterlich, 
#um operador está usando uma constante instrumental igual a 2. Uma árvore 
#próxima ao ponto de estação não está visível, por este motivo, o operador foi até a
#árvore e mediu o seu DAP (21,12 cm) e a distância do ponto de estação (12,42 metros). Responda: (100%)

#(a) Ela será contada? Justifique.
#K=2500*(a/l)^2
#R= sqrt((dap^2)/(K/2500))
R1 <- sqrt((0.2112^2)/(2/2500))
#7.467 m
#A árvore não será contada pois está fora da prova de numeração angular máxima,
#ou seja, R < distância do PE (ponto de estação). 


#(b) Qual é a constante instrumental (K) que tangencia esta árvore?
#K=2500(dap/R)^2
K1 <- 2500*((0.2112/12.42)^2)
# 0.7229

#(c) Qual é a área basal por ha considerando que no giro de 360°
#foram contadas 14 árvores?
#G=N*K
G_ha1 <- 14*2
#28 m^2/ha

#(d) Qual a área da parcela delimitada por esta árvore?
#a área delimitada por essa árvore é = 0 m^2

#Se a árv. fosse contada seria:
#A=pi*R^2
A1 <- pi*(7.467^2)

