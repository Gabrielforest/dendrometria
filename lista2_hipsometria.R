#hipsometria
#lista 2

#Um técnico florestal querendo obter a altura média das árvores dominantes em uma 
#parcela de 700m2 mediu as alturas das 7 árvores mais grossas da parcela. 
#Com as informações apresentadas na tabela 1 (os sinais positivos e negativos significam                                                                                           e negativos significam 
#simplesmente leituras em situação de aclive e declive, respectivamente), responda:


#Tabela 1: Medições das alturas de algumas árvores em diferentes condições de relevo e escalas/distâncias da árvore
#Árvore  DAP      Instrumento       Leitura na base    Leitura no ápice  Escala  Distância da árvore   Declividade
#1       31.23    Prancheta Dendrométrica * −1.50cm      8.00cm          cm        30m                  0.38°
#2       33.30    Suunto                    −11.60°      +35.50°         graus     31m                  8.00°
#3       29.88    Suunto                    −8.00%       +90.00%         %         29m                  2.14%
#4       28.26    Suunto                    +3.50m       +24.30m         20m       28m                  15.00%
#5       31.86    Haga                      −5.42m       +18.50m         25m       30m                  7.00°
#6       32.40    Haga                      +4.20m       +32.70m         25m       25m                  10.00%
#7       29.00    Blume-Leiss              −51.96m       +0.25m          30m       29m                  56.65°


#(a) O técnico está correto em medir a altura de apenas 7 árvores? Porquê?
#O correto é medir a altura das 100 árvores mais grossas por ha (indivíduos dominantes)
#, segundo ASSMANN,logo, para uma área de 700m^2 temos:

#          10000m^2 ---- 100 árv.
#            700m^2 ---- x árv.   

x <- (700*100)/10000
#x = 7 árvores, então o técnico está correto em medir de acordo com Assmann  

#(b) Qual é a altura média das árvores dominantes para a referida parcela?

#Declividade superior a 4 graus ou 7% deve ser corrigida
#para corrigir é só multiplicar fazendo as devidas alterações

#as alterações necessárias para corrigir a porcentagem e graus elevados são:
#porcentagem: multiplicar por cos(atan(__)) 
#graus: multiplicar por cos(__*pi/180)


#Lh(l1+l2)/largura da prancheta
arvore1 <- (30*(0.015+0.08))/0.1

#Lh(l1+l2)   a declividade aqui é superior a 4 graus
#Só que aq as leituras estavam em graus, logo, tem q fzr a tan:
arvore2 <- (31*(cos(8*pi/180)))*(tan(11.60*pi/180)+(tan(35.50*pi/180)))

#Lh(l1+l2)  aqui as leituras estavam em porcentagem,por isso dividimos por 100:
arvore3 <- 29*(0.08+0.9)

#Lh(l1+l2)   a declividade aqui é superior a 7%
#nessa nós temos leituras com sinais iguais, então subtraímos 
arvore4 <- ((28*(24.30-3.5))/20)*(cos(atan(0.15)))

#Lh(l1+l2)   a declividade é superior a 4°
arvore5 <- ((30*(5.42+18.5))/25)*cos(7*pi/180)

#Lh(l1+l2)   a declividade é superior a 7%
#nessa nós temos leituras com sinais iguais, portanto subtraímos
arvore6 <- (25*(32.7-4.2)/25)*cos(atan(0.1))

#Lh(l1+l2)   a delividade é superior a 4° 
arvore7 <- ((29*(51.96+0.25))/30)*cos(56.65*pi/180)

#altura média das árvores dominantes:
altura_media <- mean(c(arvore1,arvore2,arvore3,arvore4,arvore5,arvore6,arvore7))
test <- round(altura_media, digits= 4)


