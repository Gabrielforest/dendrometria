##Condicional
#if 
x<-2;
if(x%%2==0){
  print('Numero par');
}else{
  print('Numero impar')
}

#ifelse
print(
  ifelse(x%%2==0,'Numero par','Numero impar')
)

#switch
turma<-'Dendro';
switch(turma, 
       Dendro = '502510',
       InvFlor = '502626'
)

##Lacos (Devem ser evitados e se utilizar que seja com proposito muito bem definido)

#for
x<-seq(2,20,2);
sx<-0;
for(i in 1:length(x)){
  sx<-sx+x[i]
}
sx

sx<-0;
for(vx in x){
  sx<-sx+vx
}
sx

sx<-0;
for(i in seq(1,length(x),2)){
  sx<-sx+x[i]
}
sx

#while
sx<-0;
i<-1;
while(i<=length(x)){
  sx<-sx+x[i];
  i<-i+1
}

#repeat
sx<-0;
i<-1;
repeat{
  if(i<=length(x)){
    sx<-sx+x[i];
    i<-i+1
  }else{break}
}
sx


##criando uma funcao: exemplo media ponderada
x<-seq(2,20,2);
y<-seq(10,1,-1)

mean(x);
mean(y);
sum(x*y)/sum(y);

med_pond<-function(x,y){
  sxy<-0;
  sy<-0;
  for(i in 1:length(x)){
    sxy<-sxy+x[i]*y[i];
    sy<-sy+y[i];
  }
  return(sxy/sy);
}
med_pond(x,y)
