#Cubagem rigorosa - Smalian
#método de Smalian: faz a secção e mede nos extremos
#lista 4 - Cubagem Rigorosa:
#usar 5 casas p/ volume
Árvore  dap[cm] ht[m]  hi[m] dicc[cm] espcascai[cm] disc[cm] gi[m2] vismalian[m3]
30619   26.30   29.30  0.10   31.40      1.30                          XXXXXXXX
30619   26.30   29.30  0.70   27.60      0.80
30619   26.30   29.30  1.30   26.30      0.70
30619   26.30   29.30  4.00   23.00      0.70
30619   26.30   29.30  8.00   20.80      0.60
30619   26.30   29.30  12.00  18.50      0.50
30619   26.30   29.30  16.00  15.50      0.50
30619   26.30   29.30  20.00  13.00      0.50
30619   26.30   29.30  24.00  9.20       0.40

tabela1 <- data.frame(Árvore = rep(30619, 9), dap_cm = rep(26.30, 9), ht_m = rep(29.3, 9), 
            hi_m = c(0.1,0.7,1.3,4,8,12,16,20,24),
            dicc_cm = c(31.40 ,27.60,26.30,23.00,20.80,18.50,15.50,13.00,9.20),
            espcascai_cm = c(1.3,0.8,0.7,0.7,0.6,0.5,0.5,0.5,0.4),
            disc_cm = rep(NA, 9), gi_m2 = rep(NA, 9), vismalian_m3 = rep(NA, 9))


tabela1$disc_cm <- tabela1$dicc_cm-(2*tabela1$espcascai_cm)
tabela1$gi_m2 <- (tabela1$disc_cm^2)*pi/40000; #pq no ex. pediu sem casca...
tabela1$gi_m2 <- round(tabela1$gi_m2, digits= 5)

tabela1$vismalian_m3[2] <- mean(tabela1$gi_m2[1:2])*(tabela1$hi_m[2]-tabela1$hi_m[1]) 
tabela1$vismalian_m3[3] <- mean(tabela1$gi_m2[2:3])*(tabela1$hi_m[3]-tabela1$hi_m[2]) 
tabela1$vismalian_m3[4] <- mean(tabela1$gi_m2[3:4])*(tabela1$hi_m[4]-tabela1$hi_m[3]) 
tabela1$vismalian_m3[5] <- mean(tabela1$gi_m2[4:5])*(tabela1$hi_m[5]-tabela1$hi_m[4]) 
tabela1$vismalian_m3[6] <- mean(tabela1$gi_m2[5:6])*(tabela1$hi_m[6]-tabela1$hi_m[5]) 
tabela1$vismalian_m3[7] <- mean(tabela1$gi_m2[6:7])*(tabela1$hi_m[7]-tabela1$hi_m[6]) 
tabela1$vismalian_m3[8] <- mean(tabela1$gi_m2[7:8])*(tabela1$hi_m[8]-tabela1$hi_m[7]) 
tabela1$vismalian_m3[9] <- mean(tabela1$gi_m2[8:9])*(tabela1$hi_m[9]-tabela1$hi_m[8]) 

tabela1$vismalian_m3 <- round(tabela1$vismalian_m3, digits= 5)

#Calcule:

#(a) Volume comercial sem casca considerando diâmetro mínimo com casca igual a 9cm.
#dicc[cm]             #hi[m]
#13                    20
#9.2                   24
variacao_casca_cm <- 13-9.2
variacao_altura_m <- 24-20
#variacao_altura_m --- variacao_casca_cm
#         x        --- dicc mínimo existente - dicc mínimo qual quer saber 
# x= __ m
x <- (variacao_altura_m * (9.2-9))/variacao_casca_cm

# a nova altura da última seção = variação da altura + altura da última seção
altura_ultima_secao <- x + 24

#nova linha:
z <- c(0,26.3,29.3,altura_ultima_secao,9,0.4,NA,NA,NA)
tabela1 <- rbind(tabela1, z)

tabela1$disc_cm[10] <- tabela1$dicc_cm[10]-(2*tabela1$espcascai_cm[10])
tabela1$gi_m2[10] <- (tabela1$disc_cm[10]^2)*pi/40000
tabela1$gi_m2[10] <- round(tabela1$gi_m2[10], digits=5)

#((g1+g2)/2)*(hi2-hi1) = vismalian
tabela1$vismalian_m3[10] <- mean(tabela1$gi_m2[9:10])*(tabela1$hi_m[10]-tabela1$hi_m[9])
tabela1$vismalian_m3[10] <- round(tabela1$vismalian_m3[10], digits = 5)
  
#então o volume comercial é a somatória dos volumes
volumecomercial_m3 <- sum(tabela1$vismalian_m3[2:10])

#(b) Volume total sem casca.
#vponta <- gponta*hponta/3 
#vponta <- (g_menordiametro *(ht-hi))/3
vponta <- (tabela1$gi_m2[10]*(tabela1$ht_m[10]-tabela1$hi_m[10]))/3 

#vtoco = gtoco*htoco
vtoco <- (tabela1$gi_m2[1]*tabela1$hi_m[1])

#Vtotal = Vcomercial + Vponta + Vtoco
vtotal <- volumecomercial_m3 + vponta + vtoco

#(c) Fator de forma para volume total sem casca.
#vtotal = g*ht*ff
#v/g*ht = ff
g <- (pi*26.3^2)/40000
ff <- vtotal/(g*29.3)



