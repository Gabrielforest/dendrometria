###AULA: REGRESSAO

cub<-read.csv2('cubagem.csv');
View(cub)
names(cub);

#Correlacao
cor(cub);

cor(cub[,c('dap','vol')])
cor(cub$dap,cub$vol);

par(mfrow=c(1,2));
with(cub,plot(dap,vol,pch='*'));
with(cub,plot(ht,vol,pch='*'));

cub$dap2ht<-cub$dap^2*cub$ht;
cor(cub$dap,cub$vol);
cor(cub$dap2ht,cub$vol);

with(cub,plot(dap2ht,vol,pch='*'));

(mls<-lm('vol~I(dap^2*ht)',cub));#variável dependente e (~) "varia em função de", I = combina as 
                                 #variáveis dentro dos parênteses.
sumario <- summary(mls);#(análise por parametro)
             #erro padrão residual, quanto os valores estimados variam de acordo com os valores
             #observados. coeficiente de variação quanto uma variação é explicada por outra entre 
             #variáveis. coeficiente de variação ajustado quando um modelo tem diferentes variáveis
             #independentes, quanto + aumenta o valor de r^2, o ajustado é pra comparar cm outro 
             #modelo com diferentes números de variáveis. DF número de gl n-parâmetros-1 (50-1-1)
             #p-value pra ver se o modelo é significativo
anova(mls);#análise por modelo *** betas diferentes de 0 ao nível de significância de 1% testado

syx <- sumario$sigma; #erro padrão residual em m^3
syxp <- syx/mean(cub$vol)*100; #erro padrão residual em porcentagem (errando pra mais ou pra menos)

#estimando os volumes (prevendo os volumes), pega os betas estimados e aplicam para os
#parâmetros do modelo:
par(mfrow=c(1,2));
#ylim são só pra deixar mais visual
plot(predict(mls),residuals(mls),
     ylim=c(-0.045,0.045),
     xlab='volume predito (m³)',
     ylab='residuos (m³)',
     pch='*');#símbolo para identificar
abline(h=0);#passar uma linha na posição zero horizontalmente

#para ver se são outliers ou não
plot(predict(mls),rstudent(mls),
     ylim=c(-3,3),
     xlab='volume predito (m³)',
     ylab='residuos studentizados',
     pch='*');
abline(h=0);
tinf<-qt(0.025,nrow(cub)-1)
tsup<-qt(0.975,nrow(cub)-1)
abline(h=tinf,lty=2);
abline(h=tsup,lty=2);

res<-as.vector(residuals(mls));

# Normalidade: Anderson-Darling
# nortest::ad.test(res)$p.value;
shapiro.test(res); #distribuição normal é quando é maior q 0,05
graphics.off();
par(mfrow=c(1,2));

hist(res,probability = T, main='',xlab='resíduos(m³)',ylab='densidade');

mres<-mean(res);
sdres<-sd(res);
x<-res;
curve(dnorm(x,mres,sdres),col='darkgreen',add=T);

fBasics::qqnormPlot(res);     

###Regressao linear multipla com transformacao da variavel dependente

(ajlin<-lm('I(log(vol))~I(log(dap))+I(log(ht))',cub)); #Modelo linear multiplo, com transformação
                                                      #da variável independente, vol variando 
                                                     #em função de dap e ht beta0 intercept
                                                    #beta 1dap beta2 ht
(sumario=summary(ajlin));
anova(ajlin);

(ymed<-mean(cub$vol)); #média na unidade de interesse m^3
(syx<-sumario$sigma); #Erro padrao residual da qual eu n quero a uniddade (ln(m^3))
(n<-length(cub$vol)); #número de observações.

D(expression(log(vol)),'vol'); #derivando log(vol) em função de volume
dvol<-1/cub$vol; #aplicando a expressão obtida acima

medgeo<-exp(mean(log(dvol))); #média geométrica

syx_log<-sumario$sigma; #Erro padrao residual (ln m^3);

#Indice de Furnival na escala da variavel de interesse
IF<-1/medgeo*syx_log; #Ind. Furnival (m^3)

#Indice de Furnival em porcentagem
(IFP<-(IF/ymed)*100);#em média os valores variam em % em relação aos volumes observados

###Regressao nao linear
nlinear<-nls(formula = 'vol~b0*dap^b1*ht^b2',data=cub, start=list(b0=pi/40000*0.45,b1=2,b2=1));
#
summary(nlinear)


#volume é uma variável dependente
#dap e ht são variáveis independentes
#quanto vale beta0 e beta1 pra q a soma dos erros aos quadrados seja mínima
#V= b0+b1*dap^2*h+ Ei
#parâmetros aditivos representam um modelo linear
#linear simples se tiver 1 parâmetro além do b0 
#lineas múltiplo se tiver 2 parâmetros além do b0

#R^2 É O QUANTO as variáveis independentes explicam as dependentes, sendo 1 = 100%