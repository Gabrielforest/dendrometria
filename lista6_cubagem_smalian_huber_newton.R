#Lista 6
#Tabela 1: Cubagem do fuste de uma árvore de Eucalyptus sp utilizando a procedimento de Smalian
Árvore hi[m] dicc[cm] gi[m2] vismalian[m3]
35159 0.10   27.2              xxxxx
35159 0.70   xxxxx    xxxxx
35159 1.30   21.4              xxxxx
35159 2.20   xxxxx    xxxxx
35159 3.10   19.05             xxxxx
35159 5.10   xxxxx    xxxxx
35159 7.10   16.95             xxxxx
35159 9.10   xxxxx    xxxxx
35159 11.10  14.45             xxxxx
35159 13.10  xxxxx    xxxxx
35159 15.10  12.45             xxxxx
#hi: altura da iésima seção, dicc: diâmetro da iésima seção com casca
#gi: Área seccional da iésima seção e vismalian: volume da iésima seção utilizando 
#o procedimento de Smalian

tabela_ismalian <- data.frame(Árvore = rep(35159, 11),  
                               hi_m = c(0.1,0.7,1.3,2.2,3.10,5.1,7.1,9.1,11.1,13.1,15.1),
                               dicc_cm = c(27.2,NA,21.4,NA,19.05,NA,16.95,NA,14.45,NA,12.45),
                               gi_m2 = rep(NA, 11), vismalian_m3 = rep(NA, 11))
#pi*dap^2/40000
tabela_ismalian$gi_m2 <- pi*(tabela_ismalian$dicc_cm^2)/40000
#vismalian = mean(c(g1,g2))*(h2-h1)
tabela_ismalian$vismalian_m3[3] <- mean(tabela_ismalian$gi_m2[c(3,1)])*(tabela_ismalian$hi_m[3]-
                                                                          tabela_ismalian$hi_m[1]) 
  
tabela_ismalian$vismalian_m3[5] <- mean(tabela_ismalian$gi_m2[c(5,3)])*(tabela_ismalian$hi_m[5]-
                                                                          tabela_ismalian$hi_m[3])

tabela_ismalian$vismalian_m3[7] <- mean(tabela_ismalian$gi_m2[c(7,5)])*(tabela_ismalian$hi_m[7]-
                                                                     tabela_ismalian$hi_m[5])

tabela_ismalian$vismalian_m3[9] <- mean(tabela_ismalian$gi_m2[c(9,7)])*(tabela_ismalian$hi_m[9]-
                                                                     tabela_ismalian$hi_m[7])

tabela_ismalian$vismalian_m3[11] <- mean(tabela_ismalian$gi_m2[c(11,9)])*(tabela_ismalian$hi_m[11]-
                                                                     tabela_ismalian$hi_m[9])

tabela_ismalian_ <- na.omit(tabela_ismalian)
volume_comercial_ismalian <- sum(tabela_ismalian_$vismalian_m3)
volume_comercial_ismalian <- round(volume_comercial_ismalian, digits=5)  

#Tabela 2: Cubagem do fuste de uma Árvore de Eucalyptus sp utilizando a 
#procedimento de Huber
Árvore hi[m] dicc[cm] gi[m2] vihuber[m3]
35159 0.10    xxxxx   xxxxx   xxxxx
35159 0.70    23.2
35159 1.30    xxxxx   xxxxx   xxxxx
35159 2.20    20.55
35159 3.10    xxxxx   xxxxx   xxxxx
35159 5.10    18.05
35159 7.10    xxxxx   xxxxx   xxxxx
35159 9.10    15.9
35159 11.10   xxxxx   xxxxx   xxxxx
35159 13.10   13.65
35159 15.10   xxxxx   xxxxx   xxxxx
#hi: altura da iésima seção, dicc: diâmetro da iésima seção com casca
#gi: Área seccional da iésima seção e vihuber: volume da iésima seção 
#utilizando o procedimento de Huber

tabela_huber <- data.frame(Árvore = rep(35159, 11),  
                            hi_m = c(0.1,0.7,1.3,2.2,3.10,5.1,7.1,9.1,11.1,13.1,15.1),
                            dicc_cm = c(NA,23.2,NA,20.55,NA,18.05,NA,15.9,NA,13.65,NA),
                            gi_m2 = rep(NA, 11), vhuber_m3 = rep(NA, 11))
#g = pi*dap^2/40000
tabela_huber$gi_m2 <- pi*(tabela_huber$dicc_cm^2)/40000
#vihuber = g*(h2-h1);   h2: superior  h1: inferior
tabela_huber$vhuber_m3[2]<-(tabela_huber$hi_m[3]-tabela_huber$hi_m[1])*(tabela_huber$gi_m2[2])

tabela_huber$vhuber_m3[4]<-(tabela_huber$hi_m[5]-tabela_huber$hi_m[3])*(tabela_huber$gi_m2[4])

tabela_huber$vhuber_m3[6]<-(tabela_huber$hi_m[7]-tabela_huber$hi_m[5])*(tabela_huber$gi_m2[6])

tabela_huber$vhuber_m3[8]<-(tabela_huber$hi_m[9]-tabela_huber$hi_m[7])*(tabela_huber$gi_m2[8])

tabela_huber$vhuber_m3[10]<-(tabela_huber$hi_m[11]-tabela_huber$hi_m[9])*(tabela_huber$gi_m2[10])

tabela_huber_ <- na.omit(tabela_huber)
volume_comercial_huber <- sum(tabela_huber_$vhuber_m3)
volume_comercial_huber <- round(volume_comercial_huber, digits=5)

#Tabela 3: Cubagem do fuste de uma Árvore de Eucalyptus sp 
#utilizando a procedimento de Newton
Árvore hi[m] dicc[cm] gi[m2] vinewton[m3]
35159  0.10    27.20          xxxxx
35159  0.70    23.20
35159  1.30    21.40          xxxxx
35159  2.20    20.55
35159  3.10    19.05          xxxxx
35159  5.10    18.05
35159  7.10    16.95          xxxxx
35159  9.10    15.90
35159  11.10   14.45          xxxxx
35159  13.10   13.65
35159  15.10   12.45          xxxxx
#hi: altura da iésima seção, dicc: diâmetro da iésima seção com casca
#gi: Área seccional da iésima seção e vinewton: volume da iésima seção utilizando
#o procedimento de Newton

tabela_newton <- data.frame(Árvore = rep(35159, 11),  
                             hi_m = c(0.1,0.7,1.3,2.2,3.10,5.1,7.1,9.1,11.1,13.1,15.1),
                             dicc_cm = c(27.2,23.2,21.4,20.55,19.05,18.05,16.95,15.9,14.45,13.65,12.45),
                             gi_m2 = rep(NA, 11), vnewton_m3 = rep(NA, 11))

tabela_newton$gi_m2 <- (pi*tabela_newton$dicc_cm^2)/40000  

#V= ((g1+4*gcentral+g3)/6)*(h2-h1)
# h2 e h1, são as alturas superior e inferior em relação ao gcentral
tabela_newton$vnewton_m3[2] <- (((tabela_newton$gi_m2[1]+(4*tabela_newton$gi_m2[2]) 
                                + tabela_newton$gi_m2[3])/6)*(tabela_newton$hi_m[3]-tabela_newton$hi_m[1]))

tabela_newton$vnewton_m3[4] <- (((tabela_newton$gi_m2[3]+(4*tabela_newton$gi_m2[4])
                                + tabela_newton$gi_m2[5])/6)*(tabela_newton$hi_m[5]-tabela_newton$hi_m[3]))

tabela_newton$vnewton_m3[6] <- (((tabela_newton$gi_m2[5]+ (4*tabela_newton$gi_m2[6]) 
                                + tabela_newton$gi_m2[7])/6)*(tabela_newton$hi_m[7]-tabela_newton$hi_m[5]))

tabela_newton$vnewton_m3[8] <- (((tabela_newton$gi_m2[7]+ (4*tabela_newton$gi_m2[8]) 
                                + tabela_newton$gi_m2[9])/6)*(tabela_newton$hi_m[9]-tabela_newton$hi_m[7]))

tabela_newton$vnewton_m3[10] <- (((tabela_newton$gi_m2[9]+ (4*tabela_newton$gi_m2[10]) 
                                + tabela_newton$gi_m2[11])/6)*(tabela_newton$hi_m[11]-tabela_newton$hi_m[9]))

tabela_newton_ <- na.omit(tabela_newton)
volume_comercial_newton <- sum(tabela_newton_$vnewton_m3)
volume_comercial_newton <- round(volume_comercial_newton, digits=5)
