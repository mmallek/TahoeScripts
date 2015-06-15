###3-paramter logistic function

logistic<-function(x,a,b) exp(a + b*x)/(1 + exp(a + b*x))
logistic3p<-function(x,a,b,c) ((a-1)/(1+((x/c)^b)))+1

curve(logistic3p(x,1,1,1),0,100)
curve(logistic3p(x,.7,1.5,10),0,100)
title('Logistic function')

#2 parameter
legend('top',legend='f(x)= exp(a + b*x)/(1 + exp(a + b*x))',bty='n')
