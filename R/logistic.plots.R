###logistic plots script

par(mar=c(5,5,4,2))

#3 parameter logistic for susceptibility - disturbance interactions
logistic3p<-function(x,intercept,slope,inflexion){
  ((intercept-1)/(1+((x/inflexion)^slope)))+1
}

curve(logistic3p(x,1.2,3,10),0,30,ylim=c(1,1.2),xlab='years',ylab='probability',
      cex.lab=1.5)
mtext('Intercept=1.2, Slope=3, Inflexion=10',side=3,col='red',cex=1.5)

curve(logistic3p(x,.8,3,10),0,30,ylim=c(.8,1),xlab='years',ylab='probability',
      cex.lab=1.5)
mtext('Intercept=0.8, Slope=3, Inflexion=10',side=3,col='red',cex=1.5)


#4 parameter logistic 
logistic4p<-function(x,left,right,slope,inflexion){
  left + ((right-left)/(1+exp(slope*(inflexion-x))))
}

#susceptibility - TPI
curve(logistic4p(x,.8,1.2,1,0),-3,3,ylim=c(.8,1.2),
  xlab='topographic position index',ylab='probability')

#treatment suitability - TPI
curve(logistic4p(x,.4,1,1,0),-3,3,ylim=c(0,1),
  xlab='topographic position index',ylab='probability')

#treatment suitability - slope
curve(logistic4p(x,1,.2,.2,30),0,100,ylim=c(0,1),
    xlab='slope (%)',ylab='probability')

#treatment suitability - road proximity - all mechanical treatments
curve(logistic4p(x,1,0,.015,300),0,1000,ylim=c(0,1),
      xlab='road proximity (m)',ylab='probability')

#treatment suitability - road proximity - prescribe fire
curve(logistic4p(x,1,.1,.001,5000),0,20000,ylim=c(0,1),
      xlab='road proximity (m)',ylab='probability')
