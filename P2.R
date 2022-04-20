#questão 1


#questão 2







#questao 3
#exercício vismalian:
tabela1 <- data.frame(Árvore = rep(36950, 9), dap_cm = rep(24.60, 9), ht_m = rep(29.5, 9), 
                       hi_m = c(0.1,0.7,1.3,4,8,12,16,20,24),
                       dicc_cm = c(30.70 ,27.40,24.60,21.80,19.00,16.75,14.40,11.60,8.10),
                       espcascai_cm = c(1.7,1.2,0.8,0.5,0.5,0.4,0.4,0.4,0.4),
                       disc_cm = c(NA,NA,NA,NA,0.02545,0.01998,0.01453,0.00916,0.00419),
                       gisc_m2 = c(NA,NA,NA,NA,0.02545,0.01998,0.01453,0.00916,0.00419), 
                       vismalian_m3 = c(NA,NA,NA,NA,0.11886,0.09086,0.06902,0.04738,0.02670))


tabela1$disc_cm <- tabela1$dicc_cm-(2*tabela1$espcascai_cm);   #se for sem casca usa essa linha
tabela1$gisc_m2 <- (tabela1$disc_cm^2)*pi/40000; #pq no ex. pediu sem casca... se n é só trocar
tabela1$gisc_m2 <- round(tabela1$gisc_m2, digits= 5)

tabela1$vismalian_m3[2] <- mean(tabela1$gisc_m2[1:2])*(tabela1$hi_m[2]-tabela1$hi_m[1]) 
tabela1$vismalian_m3[3] <- mean(tabela1$gisc_m2[2:3])*(tabela1$hi_m[3]-tabela1$hi_m[2]) 
tabela1$vismalian_m3[4] <- mean(tabela1$gisc_m2[3:4])*(tabela1$hi_m[4]-tabela1$hi_m[3]) 

#(a) volume comercial sem casca
0 -> tabela1$vismalian_m3[1]
volume_comercial_ismalian <- sum(tabela1$vismalian_m3)
volume_comercial_ismalian <- round(volume_comercial_ismalian, digits=5)  

#(b) Volume total sem casca.
#vponta <- gponta*hponta/3 
#vponta <- (g_menordiametro *(ht-hi))/3
vponta <- (tabela1$gisc_m2[9]*(tabela1$ht_m[9]-tabela1$hi_m[9]))/3 

#vtoco = gtoco*htoco
vtoco <- (tabela1$gisc_m2[1]*tabela1$hi_m[1])

#Vtotal = Vcomercial + Vponta + Vtoco
vtotal <- volume_comercial_ismalian + vponta + vtoco

#(c) fator de forma para volume total sem casca
#vtotal = g*ht*ff
#v/g*ht = ff
g <- (pi*24.6^2)/40000
ff <- vtotal/(g*29.5)

#(d)0,48093; 0,5985; 0,50261; 0,51471.
y <- c(0.51426,0.48093,0.5985,0.50261,0.51471)
media_y <- mean(y)
somatorio_y <- sum(y)
somatorio_y2 <- sum(y^2)
n <- 5
sy <- sqrt((somatorio_y2-((somatorio_y^2/n)))/(n-1))
cv <- sy/media_y

# tentativa de automatizar
#n_arvs <- (t^2*sx^2)/E^2   com E^2 sendo: (erro amostral dado no exercício)^2 
#erro 9%
#t = número da tabela t-student correpondente à n-1. 
t <- abs(qt(0.05/2, 4))
n_arvs1 <- (t^2*cv^2)/(0.0915^2) 
n_arvs1 <- round(n_arvs1,digits=0)
#se n_arvs1 for igual a n_arvs2, esse será o número de árvores a serem cubadas 
#o t agora é o valor de n_arvs1 - 1 correspondente na tabela t-student: 
t <- abs(qt(0.05/2, 6))
n_arvs2 <- (t^2*cv^2)/(0.0915^2) 
n_arvs2 <- round(n_arvs2,digits=0)

#se n_arvs1 for igual a n_arvs2, esse será o número de árvores a serem cubadas 
#o t agora é o valor de n_arvs1 - 1 correspondente na tabela t-student: 
t <- abs(qt(0.05/2, 4))
n_arvs3 <- (t^2*cv^2)/(0.0915^2) 
n_arvs3 <- round(n_arvs3,digits=0)
#7 árvores devem ser cubadas

#questão 4
l <-  3    ; # comprimento da tora
e <-  1.24 ; # espessura da casca
d <-  82   ; # diâmetro maior extremidade
D <-  59   ; # diâmetro menor extremidade
d_no_meio <- 70 
diametro_pecas <- 7 ; # peças presas pela máquina
esp_lamina <- 2 ; # espessura da lâmina

#diâmetro sem casca= diâmetro das extremidades - 2*espessura da casca  
d_sem_casca <-  d - 2*(e) 
D_sem_casca <-  D - 2*(e)
d_no_meio_sem_casca <- d_no_meio - 2*(e)

#(a) Quantos compensados de 3×2m podem ser obtidos, se cada compensado é formado pela colagem de 5 lâminas?
#q = ((pi/40000))*((Diâmetro_menor_sem_casca^2)-(tamanho_das_peças_presas_pela_máquina^2)))/(espessura_da_lâmina/1000)
q <- ((pi/40000)*((D_sem_casca^2)-(diametro_pecas^2)))/(esp_lamina/1000) 

#Superfície da madeira laminada = q * l      
S <- q * l

#n° de lâminas de 6m^2  (3mx2m)
n_laminas <- S/(3*2); # alterar com as dimensões do enunciado da letra a)
#colagem de 5 lâminas restam x compensados:
n_compensados <- n_laminas/5; # alterar o número de lâminas necessário para 1 compensado
#número de lâminas que sobraram: 
sobra_de_laminas <- 5*(n_compensados-(round(n_compensados, digits = 0))); #alterar o número de lâminas
round(sobra_de_laminas, digits=0)
#(b) Se fosse esquadrejada, qual seria o volume da peça em m3?
# volume_esquadrejada <- ((D_sem_casca/100)^2/2)*l
v_esquadrejada <- ((D_sem_casca/100)^2/2)*l 

#(c) Qual é o volume 4° e 5° deduzido?
v_quarto_deduzido <- (((d_no_meio_sem_casca/100)/4*pi)^2)*l
v_quinto_deduzido <- (pi/6.25)*((((d_no_meio_sem_casca/100)^2)/4*pi))*l


