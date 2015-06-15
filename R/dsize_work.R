require(ggplot2)
theme_set(theme_bw())
# load table
firehistory = read.csv("/Users//mmallek//Tahoe/FireHistory/firehist.csv", header=T)

# remove NAs
firehist = na.omit(firehistory)
#attach(firehist)


#add new column with log-transformed fire sizes
firehist$logfire = log(firehist$Fsize)
head(firehist)

#find out how many rows turned to Inf from the log
sum(is.infinite(firehist$logfire))

#turn firehistory into a vector
firehistv = as.vector(firehistory$Fsize)
str(firehistv)
head(firehistv)
fh.hist = hist(firehistv, breaks=c(0,1,10,100,1000,10000,100000))

#remove NAs to use vector without log-transforming it
firehistv.c = na.omit(firehistv)

#add 1 to each value in firehistv
firehistv01 = firehistv + 1
head(firehistv01)

#take log of new vector
logfire = log(firehistv01)
head(logfire)

#check for and remove NAs
sum(is.na(logfire))
logfire = na.omit(logfire)
sum(is.na(logfire))

# regular plot and density line
plot(density(logfire))
hist(logfire, breaks=25, freq=FALSE, add=TRUE)
temp = hist(logfire, breaks=c(0,1,10,100,1000,10000,100000), freq=FALSE, add=TRUE)

min(logfire)
hist(logfire, breaks=c(0,1,2,3,4,5,12), freq=FALSE)
lines(density(logfire))
barplot(logfire)

# turn data into proportion like the dsize function uses
logfirehist = hist(logfire, breaks=c(0,1,2,3,4,5), freq=FALSE)
sum(logfirehist[[3]])
logfireprops = logfirehist[[3]] / 0.8709797


# plot of non-log-transformed data
plot(density(firehistv.c))
hist(firehistv.c, breaks=25, freq=FALSE, add=TRUE)
hist(firehistv.c, breaks=c(0,1,10,100,1000,10000,100000), freq=FALSE, add=TRUE)
hist(firehistv.c, breaks=c(0,1,10,100,1000,10000,100000), freq=F)



qplot(logfire, data = firehist, geom='histogram', binwidth=1) 
qplot(logfire, data= firehist, geom='density')
qplot(logfire, binwidth=1) + geom_histogram(aes(y = ..density..), binwidth=1) + geom_density()

#ggplot with density line
qplot(logfire, geom = 'blank') +
    geom_histogram(aes(y = ..density..), alpha = 0.8, binwidth=0.5) + 
    geom_line(aes(y = ..density..,), stat = 'density')


#gives optimum bin width (doesn't seem to really work)
dpih(logfire)

dsize(path,session=NULL,runs=NULL,pool.runs=TRUE,
             start.step=1,stop.step=NULL,cell.size=30,log.size=TRUE,
             breaks=c(0,1,10,100,1000,10000,100000),
             col.bars='blue',col.text='brown',...)