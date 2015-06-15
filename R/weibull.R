###Script for visualizing the Weibull distribution
##K. McGarigal 9/27/2013

###weibull hazard function
hweibull<-function(x,shape,mri){
  scale<-round(mri/gamma(1+(1/shape)),0)
  z<-(shape*(x^(shape-1)))/(scale^shape)
  return(z)  
}
# x is age???

###weibull distribution plots
weibull.plot<-function(shape=1,mri=10){
  old.par<-par(no.readonly=TRUE)
  on.exit(par(old.par))
  par(mfrow=c(2,2))
  
  #define hazard function
  tweibull<-function(x,shape=shape,scale=scale){
    1-pweibull(x,shape=shape,scale=scale)
  }
  
  #define scale as a function of mri and shape
  scale<-round(mri/gamma(1+(1/shape)),0)
  
  #create plots
  curve(pweibull(x,shape,scale),from=0,to=2.5*scale,ylim=c(0,1),
        xlab='Time (between events)',
        ylab='Cumulative probability (quantile)',
        main='Cumulative Probability')
  mtext(paste('(shape=',shape,', mri=',mri,')',sep=''),side=3,col='red')
  curve(dweibull(x,shape,scale),from=0,to=2.5*scale,
        xlab='Time (between events)',
        ylab='Probability density',
        main='Probability Density')
  mtext(paste('(shape=',shape,', mri=',mri,')',sep=''),side=3,col='red')
  curve(tweibull(x,shape,scale),from=0,to=2.5*scale,ylim=c(0,1),
        xlab='Time (since last event)',
        ylab='Prob (not having an event up to time t)',
        main='Time Since Event')
  mtext(paste('(shape=',shape,', mri=',mri,')',sep=''),side=3,col='red')
  curve(hweibull(x,shape,mri),from=0,to=5*scale,
        xlab='Time(between events)',
        ylab='Hazard',
        main='Hazard Function')
  mtext(paste('(shape=',shape,', mri=',mri,')',sep=''),side=3,col='red')
}

###Examples of using hweibull function to compute the hazard probability
hweibull(400,shape=3,mri=20)
hweibull(150,shape=3,mri=100)

###Examples of using weibull.plot function
weibull.plot() #uses default shape=1 and mri=10
weibull.plot(shape=3,mri=20)

# 
wcdf <- function(t, mri, shape=3){
    1 - exp(-(t/mri)^shape)
}
#A(t) time since fire
wtsf <- function(t, mri, shape=3){
    exp(-(t/mri)^shape)
}

f.t = wtsf(50, shape=3, mri=12) * wtsf(50, shape=3, mri=12)

timesteps = c(1:100)
timesteps2 = c(1:60)
plot(wcdf(timesteps, 20))
plot(wcdf(timesteps2, 10))
plot(wtsf(timesteps, mri=50, shape=3)*hweibull(timesteps, mri=50, shape=3))
plot(f.t)

hweibull(50, shape=3, mri=12)
wtsf(50, shape=3, mri=50)
hweibull(50, shape=3, mri=12) * wtsf(50, shape=3, mri=12)

pweibull(20, shape=3, scale=206)
tweibull(20, shape=3, scale=206)
weibull.plot(shape=3,mri=140)


meanfri1 = c(45,30,30,36,46,86,8,16,49,12,8,15,40,30,50,46,42,250,225,17,12,30,20,23,9,18)
meanfri2 = meanfri1
meanfri3 = c(45,7,16,72,46,86,7,16,49,9,8,15,40,32,57,54,110,206,189,14,12,34,28,23,10,28)
minmid = c(20,20,30,10,10,20,20,30,10,20,20,50,30,50,80,10,20,20,10,20,40,80,10,30,20,10)
minlate = c(140,40,60,60,160,140,40,60,30,80,100,130,110,130,160,160,150,80,90,120,200,280,160,100,140,90)

highfri1 = c(225,167,176,124,92,226,80,94,86,80,62,68,200,200,278,92,47,385,304,113,150,600,40,135,180,150)
highfri2 = c(223,65,90,180,92,226,75,90,85,122,55,70,209,300,300,190,124,350,325,150,180,250,92,133,200,38)
highfri3 = highfri2

tweibull1 = pweibull(minmid, shape=3, scale=meanfri1)
tweibull3 = tweibull(minmid, shape=3, scale=meanfri3)
cat(tweibull1,sep="\n")
cat(tweibull3, sep="\n")


tweibull.1L = pweibull(minlate, shape=3, scale=highfri1)
cat(tweibull.1L, sep="\n")
tweibull.2L = pweibull(minlate, shape=3, scale=highfri2)
cat(tweibull.2L, sep="\n")
tweibull.3L = tweibull(minlate, shape=3, scale=highfri2)
cat(tweibull.3L, sep="\n")

======================================================================
#Option 3 Only
minmid = c(20,20,30,10,10,20,20,30,10,20,20,50,30,50,80,10,20,20,10,20,40,80,10,30,20,10)
minlate = c(140,40,60,60,160,140,40,60,30,80,100,130,110,130,160,160,150,80,90,120,200,280,160,100,140,90)

meanfri = c(45,7,16,72,46,86,7,16,49,9,8,15,40,32,57,54,110,206,189,14,12,34,28,23,10,28)
highfri = c(223,65,90,180,92,226,75,90,85,122,55,70,209,300,300,190,124,350,325,150,180,250,92,133,200,38)
lowfri = c(56,8,19,120,91,140,8,19,114,10,9,19,50,36, 70,75,1000,500,450,15,13,40,40,28,10,111)
prophigh = c(0.2,0.11,0.17,0.4,0.5,0.38,0.1,0.17,0.57,0.08,0.14,0.21,0.19,0.11,0.19,0.28,0.89,0.59,0.58,0.09,0.07,0.14,0.3,0.17,0.05,0.74)

#wrong still
prob.mid = tweibull(minmid, shape=3, scale=meanfri) * prophigh
cat(prob.mid, sep="\n")

#prob of reaching mid using overall FRI
prob.mid.O = tweibull(minmid, shape=3, scale=meanfri)
cat(prob.mid.O, sep="\n")

#prob of reaching mid using high FRI
prob.mid.H = tweibull(minmid, shape=3, scale=highfri)
cat(prob.mid.H, sep="\n")

#prob of reaching mid using low FRI
prob.mid.L = tweibull(minmid, shape=3, scale=lowfri)
cat(prob.mid.L, sep="\n")

#prob of reaching late using high FRI
prob.high.H = tweibull(minlate, shape=3, scale=highfri)
cat(prob.high.H, sep="\n")

