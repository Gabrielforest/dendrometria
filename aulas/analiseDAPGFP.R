#treinim 

library(plyr)

fuste <-  readcsv2("fustes.csv")

head(fuste)

#dap :
dap <- fuste$dap

#eliminating NAs:
dap <- na.omit(dap)

#to determine the classes number we've got:
nclasses <- nclass.Sturges(dap)

#for the classes:
cldap <- cut(dap, breaks = nclasses, right = F, include.lowest = T)

#frequency table:
tab_freq <- count(cldap)

head(tab_freq)
tab_freq$fr <- tab_freq$freq/(sum(tab_freq$freq))

sum(tab_freq$fr) ==  1; #if it's TRUE that's okay and u can keep going...

names(tab_freq) <- c("classes","fo","fr")

#inside this range divide by the classes number:
class_break <- diff(range(dap))/nclasses

#frequency density:
tab_freq$fd <- tab_freq$fr/class_break

#data analyses:
dap_sd <- sd(dap); #standard deviation
dap_med <- median(dap); #median
dap_mean <- mean(dap); #mean
dap_var <- var(dap); #sample variance

#quadratic mean diameter
mean_g <- mean((pi*(dap^2))/40000)
dg <- sqrt((40000*mean_g)/pi) 

#either


