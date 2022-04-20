#Uma tora a ser laminada possui 3m de comprimento, uma espessura de casca de aproximadamente 1.14cm, diâmetros
#com casca nas extremidades de 77cm e 65cm e o diâmetro com casca no meio da tora igual a 71cm. A máquina
#laminadora prende a tora com peças de 5cm de diâmetro e está regulada para desenrolar lâminas com espessura de
#2mm. Pergunta-se: (100%)

l <-  3    ; # comprimento da tora
e <-  1.14 ; # espessura da casca
d <-  77   ; # diâmetro maior extremidade
D <-  65   ; # diâmetro menor extremidade
d_no_meio <- 71 
diametro_pecas <- 5 ; # peças presas pela máquina
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
