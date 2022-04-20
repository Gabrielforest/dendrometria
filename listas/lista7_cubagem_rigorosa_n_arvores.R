#Na Tabela 1 podem ser observados os resultados da cubagem rigorosa de 9 árvores 
#em um povoamento de Eucalyptus grandis W. Hill ex Maiden no Sul do Estado de São Paulo. 
#Sabendo-se que estas 9 árvores encontram-se na mesma classe diamétrica (15 a 17cm), calcule 
#o número definitivo de árvores a serem cubadas utilizando o estimador da suficiência amostral 
#da amostragem casual simples. Considere um erro amostral aceitável igual a 9% para um nível de 
#significância igual a 1%. Os quantis de ordem p=99,5% da distribuição t de Student para diferentes 
#graus de liberdade podem ser observados na Tabela 2. (100%)

#Tabela 1: Resultado da cubagem rigorosa - Amostra piloto
#ÁRVORE  DAP    HT    VTCC
#20     16.50  25.10  0.241697
#37     16.70  24.20  0.248593
#52     16.40  22.70  0.231245
#83     15.60  22.20  0.191407
#105    16.10  22.50  0.205119
#126    15.70  28.00  0.242437
#129    15.30  24.40  0.170675
#130    15.30  25.30  0.201456
#138    15.10  28.00  0.229366
#onde: DAP=Diâmetro à altura do peito [cm],HT=Altura total [m] e VTCC=Volume total com casca [m3]

tabela1 <- data.frame(arvore= c(20,37,52,83,105,126,129,130,138),
                      dap= c(16.50,16.70,16.40,15.60,16.10,15.70,15.30,15.30,15.10),
                      ht= c(25.10,24.20,22.70,22.20,22.50,28,24.40,25.30,28),
                      vtcc= c(0.241697,0.248593, 0.231245,0.191407,0.205119,0.242437,0.170675,0.201456,
                              0.229366))

y <- tabela1$vtcc
media_y <- mean(y)
somatorio_y <- sum(y)
somatorio_y2 <- sum(y^2)
n <- 9
sy <- sqrt((somatorio_y2-((somatorio_y^2/n)))/(n-1))
cv <- sy/media_y*100
#n_arvs <- (t^2*sx^2)/E^2   com E^2 sendo: (mean(x)* porcentagem de erro amostral dada no exercício)^2 
#erro amostral aceitável 9%, nível de significância igual a 1%
#t = número da tabela t-student correpondente à n-1. 
t <- 3.3554;#ou
t <- abs(qt(0.01/2, 8))
n_arvs1 <- (t^2*sy^2)/((media_y*0.09)^2) 
n_arvs1 <- round(n_arvs1,digits=0)
#se n_arvs1 for igual a n_arvs2, esse será o número de árvores a serem cubadas 
#o t agora é o valor de n_arvs1 - 1 correspondente na tabela t-student: 
t <- 2.8453;#ou
t <- abs(qt(0.01/2, 20))
n_arvs2 <- (t^2*sy^2)/((media_y*0.09)^2) 
n_arvs2 <- round(n_arvs2,digits=0)
#se n_arvs2 for igual a n_arvs2, esse será o número de árvores a serem cubadas 
t <- 2.9768;#ou
t <- abs(qt(0.01/2, 14))
n_arvs3 <- (t^2*sy^2)/((media_y*0.09)^2) 
n_arvs3 <- round(n_arvs3,digits=0)
#se n_arvs3 for igual a n_arvs4, esse será o número de árvores a serem cubadas 
t <- 2.9208;#ou 
t <- abs(qt(0.01/2, 16)) 
n_arvs4 <- (t^2*sy^2)/((media_y*0.09)^2) 
n_arvs4 <- round(n_arvs4,digits=0)
#se n_arvs4 for igual a n_arvs5, esse será o número de árvores a serem cubadas 
t <- 2.9467;#ou
t <- abs(qt(0.01/2, 15))
n_arvs5 <- (t^2*sy^2)/((media_y*0.09)^2) 
n_arvs5 <- round(n_arvs4,digits=0)
#16 árvores devem ser cubadas...

#ou

y <- tabela1$vtcc
n <- 9
cv <- ((sqrt(((sum(y^2)-(sum(y)^2/n)))/(n-1)))/(mean(y)))*100
sy <- sqrt(((sum(y^2)-(sum(y)^2/n)))/(n-1))
#n_arvs <- (t^2*sx^2)/E^2   com E^2 sendo: (erro amostral dado no exercício)^2 
#erro 9%
#t = número da tabela t-student correpondente à n-1. 
t <- 3.3554;#ou
t <- abs(qt(0.01/2, 8))
n_arvs1 <- (t^2*cv^2)/(9^2) 
n_arvs1 <- round(n_arvs1,digits=0)
#se n_arvs1 for igual a n_arvs2, esse será o número de árvores a serem cubadas 
#o t agora é o valor de n_arvs1 - 1 correspondente na tabela t-student: 
t <- 2.8453;#ou
t <- abs(qt(0.01/2, 20))
n_arvs2 <- (t^2*cv^2)/(9^2) 
n_arvs2 <- round(n_arvs2,digits=0)
#se n_arvs1 for igual a n_arvs2, esse será o número de árvores a serem cubadas 
t <- 2.9768;#ou
t <- abs(qt(0.01/2, 14))
n_arvs3 <- (t^2*cv^2)/(9^2) 
n_arvs3 <- round(n_arvs3,digits=0)
#se n_arvs2 for igual a n_arvs3, esse será o número de árvores a serem cubadas 
t <- 2.9208;#ou 
t <- abs(qt(0.01/2, 16))
n_arvs4 <- (t^2*cv^2)/(9^2) 
n_arvs4 <- round(n_arvs4,digits=0)
#se n_arvs4 for igual a n_arvs5, esse será o número de árvores a serem cubadas 
t <- 2.9467;#ou
t <- abs(qt(0.01/2, 15))
n_arvs5 <- (t^2*cv^2)/(9^2) 
n_arvs5 <- round(n_arvs4,digits=0)
#16 árvores devem ser cubadas...
