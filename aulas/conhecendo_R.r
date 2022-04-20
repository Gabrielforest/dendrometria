#AULA: CONHECENDO O R - CLASSES

#Uso de vetores
a<-1;
b<-c(a,5);

#Tamanho de um vetor
length(b);
b[1];
b[2];

#Criar um vetor contendo uma sequencia de numeros.
v1=1:100
v2=seq(1,100,2); 
length(v2);

#Uso de matrizes
m<-matrix(0,3,2);
dim(m); #Dimensoes da matriz

m2<-matrix(v2,50,2);

tm2<-t(m2); #t: Transpor matriz

m2[2,1];
m2[3,2];

m2[,2]; #Observacoes da coluna 2
m2[2,]; #Observacoes da linha 2

#Uso de data frames
l<-c('a','b','c','d','e','f'); #l<-letters[1:6]

df<-data.frame(letras=l,valores=1:6);

View(df); #Visualizar um objeto
df2=edit(df); #Editar os valores
print(df2);

df$letras;
df$valores;

df$valores[3];
df$valores[3:5];

nobj<-list(vetor=v,matriz=m2,dataframe=df);

nobj$vetor;
nobj$matriz;
nobj$dataframe;
nobj$dataframe$valores;


#Importar arquivo *.csv
dir();

dados<-read.csv2('invparc.csv');

View(dados);

#Exportando arquivo *.csv
write.csv2(dados,'invparcb.csv');

#Exportando arquivo *.xlsx
library(xlsx)
write.xlsx(dados,'resultados.xlsx',sheetName = 'parcelas')

