#Tabela 1: Resultados da cubagem de diferentes árvores da espécie Eucalyptus sp.
#cai smalian na p2 dnv

tabela1 <-  read.csv2("cubagem_9.csv")

x <- tabela1$dap2ht
y <- tabela1$vtcc
n <- 14
#(a) Utilizando o método dos mínimos quadrados, quais são os valores estimados para os parâmetros do modelo
#vtcc = β0 + β1dap2ht + i (Spurr).
beta1 <- (sum(x*y)-((sum(x)*sum(y))/n))/((sum(x^2)-((sum(x)^2)/n)))
beta0 <- mean(y)-(beta1*mean(x))

#(b) Faça a análise de variância do ajuste. Sabendo-se que o valor de F tabelado para α = 5% é igual a 4,75 o 
#modelo ajustado foi significativo?
cor(tabela1$dap,tabela1$vtcc);
(mls<-lm('vtcc~I(dap2ht)',tabela1));#variável dependente e (~) "varia em função de", I = combina as 
#variáveis dentro dos parênteses (nesse caso não precisaria).
sumario <- summary(mls)
anova(mls)
#O modelo ajustado foi significativo. Pois Fcalc. > Ftab., rejeita-se H0, portanto, 
#os betas são diferentes de 0 e a variável independente explica a dependente. 

#(c) Calcule e defina coeficiente de determinação. Apresente o R2 normal e ajustado na escala percentual.
r2_normal <- sumario$r.squared
r2_normal_percentual <- (sumario$r.squared)*100
r2_ajustado <- sumario$adj.r.squared
r2_ajustado_percentual <- (sumario$adj.r.squared)*100

#(d) Calcule e defina o erro padrão residual. O mesmo deverá ser apresentado na unidade da variável dependente e
#em porcentagem.
syx <- sumario$sigma; #erro padrão residual em m^3
syxp <- syx/mean(tabela1$vtcc)*100; #erro padrão residual em porcentagem (errando pra mais ou pra menos)

#(e) Para um dap de 18.3cm e altura total igual a 27.8m qual é o volume estimado através da função
#de regressão ajustada ?
df <- data.frame(dap2ht=(18.3^2*27.8))
predict(mls, newdata = df)

#(f) Faça um gráfico de dispersão dos resíduos.
par(mfrow=c(1,2))

plot(predict(mls),residuals(mls),
     ylim=c(-0.06,0.06),
     xlab='volume predito (m³)',
     ylab='residuos (m³)',
     pch='*');#símbolo para identificar
abline(h=0)


plot(predict(mls),rstudent(mls),
     ylim=c(-4,4),
     xlab='volume predito (m³)',
     ylab='residuos studentizados',
     pch='*');
abline(h=0);
tinf<-qt(0.025,nrow(tabela1)-1)
tsup<-qt(0.975,nrow(tabela1)-1)
abline(h=tinf,lty=2);
abline(h=tsup,lty=2);
