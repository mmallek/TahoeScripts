#### New darea function for future data

#### Main point of this function is to plot a darea from rmlstats copy ####
#### and generate the output statistics ####
require(tidyr)
require(dplyr)
require(ggplot2)
require(grid)

sessions=c(34,35,36,38,39,43,44)
sessionnames = c('CCSM-1','CCSM-2','CCSM-3','CCSM-5','CCSM-4','CCSM-6','ESM2M')
path = '/Users/mmallek/Tahoe/RMLands/results/results20150904/'
start.step = 14


#read darea data
x<-read.csv(paste(path,'darea.csv',sep=''),header=TRUE)

# if cover type specified, subset data frame (x) here
if (!is.null(covtype)){
    x<-x[x$cov.name==covtype,]
}

    #rescale cell counts
    x$mort.high<-x$mort.high*((cell.size^2)/10000)
    x$mort.low<-x$mort.low*((cell.size^2)/10000)
    x$mort.any<-x$mort.any*((cell.size^2)/10000)
    
    # take out default for sessions and runs - these need to be specified in function call
    
    # next what we really want is to grab the last 5 timesteps for each run

for(i in 1:length(sessions)){
    
    # for now let's assume 1 session is specified
    y<-x[x$session.id == sessions[i],]
    
    # we want all the runs, so ignore that
    # also only have 1 type of disturbance, so no need to separate by that
    # now we want to limit by timesteps
    y = y[y$timestep.id >= start.step,]
    
    #y = x
    
    # what we eventually want to create here is a plot that shows the darea
    # for the last 5 timesteps of each run. So it will be the same size as
    # the hrv version but with the 5 timesteps lined up sequentially
    
    # aggregate: The first argument is the column of which the values are going to be grouped, 
    # and then sent to the function (mean in this case). The second argument is a list of which 
    # variables to group by. It can be named to make the output cleaner. 
    
    # count number of cells experience x type fire each timestep, each run
    y.low<-aggregate(y$mort.low,list(timestep=y$timestep.id, run=y$run.id),sum)
    colnames(y.low)[3] = 'mort.low' 
    y.high<-aggregate(y$mort.high,list(timestep=y$timestep.id, run=y$run.id),sum)
    colnames(y.high)[3] = 'mort.high' 
    y.any<-aggregate(y$mort.any,list(timestep=y$timestep.id, run=y$run.id),sum)
    colnames(y.any)[3] = 'mort.any' 
    
    # merge the data frames
    y2 = full_join(y.low, y.high, by=c('timestep','run'))
    #y2 = full_join(y2, y.any, by=c('timestep','run'))
    
    # convert any NA values to 0 (not sure how this could happen)
    y2[is.na.data.frame(y2)]<-0
    #y2<-y2[order(y2$timestep),]
    
    
    #optionally convert darea to percent of eligible
    # percent of eligible always 1942557 in cells
    # but after we rescale it becomes 174830.1
    if(y.scale=='percent'){
        y2[,3:4]<-round((y2[,3:4]/174830.1)*100,3)
    }
    
    #y3 = y2[,1:4]
    y3 = y2
    y3 = gather(y3, burn, percent, mort.high,mort.low)
    y3$step = do.call(paste, c(y3[c('run','timestep')],sep='-'))
    
    pl = ggplot(y3, aes(step, percent, fill=factor(burn, labels=c('High Mortality','Low Mortality'))))
    pl1 = pl + geom_bar(stat="identity") + theme_bw() +
        theme(axis.title.y = element_text(size=24,vjust=2),
              axis.title.x = element_text(size=24,vjust=-1),
              axis.text.x  = element_blank(),#element_text(size=16),
              axis.text.y  = element_text(size=16)) +
        theme(axis.ticks.x = element_blank()) +
        theme(legend.title = element_text(size=16)) +
        theme(legend.text = element_text(size = 16)) +
        theme(legend.position = c(0.1,.92)) +
        theme(plot.title = element_text(size=24,vjust=1)) +
        theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank()) +
        ggtitle(paste("Area Burned by Wildfire", sessionnames[i])) + 
        ylab("Percent of Landscape Burned") +
        xlab("Timesteps 14-18, Runs 1-500") +
        labs(fill='') 
        print(pl1)
    
    
    # get min, max, median, mean for each scenario
    y2_2 = full_join(y2, y.any, by=c('timestep','run'))
    
    if(y.scale=='percent'){
        y2_2[,5]<-round((y2_2[,5]/174830.1)*100,3)
    }
    
    temp<-matrix(0,nrow=4,ncol=4)    	
    colnames(temp)<-c(sessionnames[i],'mort.low','mort.high','mort.any')
    temp[,1]<-c('minimum darea/timestep','maximum darea/timestep',
                'median darea/timestep','mean darea/timestep')
    temp[1,2:4]<-round(apply(y2_2[,3:5],2,min),2)
    temp[2,2:4]<-round(apply(y2_2[,3:5],2,max),2)
    temp[3,2:4]<-round(apply(y2_2[,3:5],2,median),2)
    temp[4,2:4]<-round(apply(y2_2[,3:5],2,mean),2)
    print(temp)
}

        

######## code to build a clustered bar chart ####

asessions=c(30, 34,35,36,38,39,43,44)
fsessions=c(34,35,36,38,39,43,44)
sessionnames = c('HRV', 'CCSM-1','CCSM-2','CCSM-3','CCSM-5','CCSM-4','CCSM-6','ESM2M')
hsession = 30
hstart.step = 40
start.step = 14
covtype = NULL
covtype = 'Sierran Mixed Conifer - Xeric' # set limits are 
covtype = 'Sierran Mixed Conifer - Mesic'
scenario.levels = c("HRV", "CCSM-1", "CCSM-5", "CCSM-4", "CCSM-6", "CCSM-2", "CCSM-3", "ESM2M")
variable = 'median'

#read darea data
x0<-read.csv(paste(path,'darea.csv',sep=''),header=TRUE)

if (!is.null(covtype)){
    x0<-x0[x0$cov.name==covtype,]
}

x = x0
#rescale cell counts
x$mort.high<-x$mort.high*((cell.size^2)/10000)
x$mort.low<-x$mort.low*((cell.size^2)/10000)
x$mort.any<-x$mort.any*((cell.size^2)/10000)

# first build data frame and do calcs for the future sessions ####
df = data.frame(summary_stat=factor(), mort_level = factor(), value = numeric(), session=integer())
for(i in 1:length(fsessions)){
    
    # for now let's assume 1 session is specified
    y<-x[x$session.id == fsessions[i],]
    
    # now we want to limit by timesteps
    y = y[y$timestep.id >= start.step,]
        
    # aggregate: The first argument is the column of which the values are going to be grouped, 
    # and then sent to the function (mean in this case). The second argument is a list of which 
    # variables to group by. It can be named to make the output cleaner. 
    
    # count number of cells experience x type fire each timestep, each run
    y.low<-aggregate(y$mort.low,list(timestep=y$timestep.id, run=y$run.id),sum)
    colnames(y.low)[3] = 'mort.low' 
    y.high<-aggregate(y$mort.high,list(timestep=y$timestep.id, run=y$run.id),sum)
    colnames(y.high)[3] = 'mort.high' 
    y.any<-aggregate(y$mort.any,list(timestep=y$timestep.id, run=y$run.id),sum)
    colnames(y.any)[3] = 'mort.any' 
    
    # merge the data frames
    y2 = full_join(y.low, y.high, by=c('timestep','run'))
    y2 = full_join(y2, y.any, by=c('timestep','run'))
    
    # convert any NA values to 0 (not sure how this could happen)
    y2[is.na.data.frame(y2)]<-0

    #optionally convert darea to percent of eligible
    # percent of eligible always 1942557 in cells
    # but after we rescale it becomes 174830.1
    if(y.scale=='percent'){
        y2[,3:5]<-round((y2[,3:5]/174830.1)*100,3)
    }
    
    temp<-matrix(0,nrow=4,ncol=4)    	
    
    temp[1,2:4]<-round(apply(y2[,3:5],2,min),3)
    temp[2,2:4]<-round(apply(y2[,3:5],2,max),3)
    temp[3,2:4]<-round(apply(y2[,3:5],2,median),3)
    temp[4,2:4]<-round(apply(y2[,3:5],2,mean),3)
    
    temp = as.data.frame(temp)
    colnames(temp)<-c('summary_stat','mort.low','mort.high','mort.any')
    temp[,1]<-c('minimum darea/timestep','maximum darea/timestep',
                'median darea/timestep','mean darea/timestep')
    temp$summary_stat = as.factor(temp$summary_stat)
        
    #isolate row of info you want to plot
    if(variable=='min') temp = temp[1,]
    if(variable=='max') temp = temp[2,]
    if(variable=='median') temp = temp[3,]
    if(variable=='mean') temp = temp[4,]
    
    temp2 = gather(temp, mort_level, value, 2:4)
    temp2$session = fsessions[i]
    temp2$scenario = sessionnames[i+1]
    
    df = bind_rows(df, temp2)
}
    ### end building individual tables

#############
### add hrv values to df ####
# for now let's assume 1 session is specified
y<-x[x$session.id == hsession,]
y = y[y$timestep.id >= hstart.step,]
y.low<-aggregate(y$mort.low,list(timestep=y$timestep.id, run=y$run.id),sum)
colnames(y.low)[3] = 'mort.low' 
y.high<-aggregate(y$mort.high,list(timestep=y$timestep.id, run=y$run.id),sum)
colnames(y.high)[3] = 'mort.high' 
y.any<-aggregate(y$mort.any,list(timestep=y$timestep.id, run=y$run.id),sum)
colnames(y.any)[3] = 'mort.any' 

y2 = full_join(y.low, y.high, by=c('timestep','run'))
y2 = full_join(y2, y.any, by=c('timestep','run'))
y2[is.na.data.frame(y2)]<-0
if(y.scale=='percent'){
    y2[,3:5]<-round((y2[,3:5]/174830.1)*100,3)
}

temp<-matrix(0,nrow=4,ncol=4)        

temp[1,2:4]<-round(apply(y2[,3:5],2,min),3)
temp[2,2:4]<-round(apply(y2[,3:5],2,max),3)
temp[3,2:4]<-round(apply(y2[,3:5],2,median),3)
temp[4,2:4]<-round(apply(y2[,3:5],2,mean),3)

temp = as.data.frame(temp)
colnames(temp)<-c('summary_stat','mort.low','mort.high','mort.any')
temp[,1]<-c('minimum darea/timestep','maximum darea/timestep',
            'median darea/timestep','mean darea/timestep')
temp$summary_stat = as.factor(temp$summary_stat)

#isolate row of info you want to plot
if(variable=='min') temp = temp[1,]
if(variable=='max') temp = temp[2,]
if(variable=='median') temp = temp[3,]
if(variable=='mean') temp = temp[4,]

temp2 = gather(temp, mort_level, value, 2:4)
temp2$session = hsession
temp2$scenario = 'HRV'

df = bind_rows(df, temp2)

df$scenario = factor(df$scenario, levels=scenario.levels, ordered=T)

## calculate grand mean across future scenarios ####

df2 = data.frame(summary_stat=factor(), mort_level = factor(), value = numeric(), session=factor())

# separate future from hrv
x2 = x[x$session.id %in% fsessions,]
# limit to only final timesteps
y = x2[x2$timestep.id >= fstart.step,]

######
y.low<-aggregate(y$mort.low,list(timestep=y$timestep.id, run=y$run.id, session=y$session.id),sum)
colnames(y.low)[4] = 'mort.low' 
y.high<-aggregate(y$mort.high,list(timestep=y$timestep.id, run=y$run.id, session=y$session.id),sum)
colnames(y.high)[4] = 'mort.high' 
y.any<-aggregate(y$mort.any,list(timestep=y$timestep.id, run=y$run.id, session=y$session.id),sum)
colnames(y.any)[4] = 'mort.any' 

# merge the data frames
y2 = full_join(y.low, y.high, by=c('timestep','run','session'))
y2 = full_join(y2, y.any, by=c('timestep','run','session'))

# convert any NA values to 0 (not sure how this could happen)
y2[is.na.data.frame(y2)]<-0

#optionally convert darea to percent of eligible
# percent of eligible always 1942557 in cells
# but after we rescale it becomes 174830.1
if(y.scale=='percent'){
    y2[,4:6]<-round((y2[,4:6]/174830.1)*100,3)
}

temp<-matrix(0,nrow=4,ncol=4)        

temp[1,2:4]<-round(apply(y2[,4:6],2,min),3)
temp[2,2:4]<-round(apply(y2[,4:6],2,max),3)
temp[3,2:4]<-round(apply(y2[,4:6],2,median),3)
temp[4,2:4]<-round(apply(y2[,4:6],2,mean),3)

temp = as.data.frame(temp)
colnames(temp)<-c('summary_stat','mort.low','mort.high','mort.any')
temp[,1]<-c('minimum darea/timestep','maximum darea/timestep',
            'median darea/timestep','mean darea/timestep')
temp$summary_stat = as.factor(temp$summary_stat)

#isolate row of info you want to plot
if(variable=='min') temp = temp[1,]
if(variable=='max') temp = temp[2,]
if(variable=='median') temp = temp[3,]
if(variable=='mean') temp = temp[4,]

# also gather option if that helps
tempg = gather(temp, mort_level, value, 2:4)
tempg$session = 'future'

df$mort_level = factor(df$mort_level, levels=c('mort.high', 'mort.low', 'mort.any'), ordered=T)

###############
# now plot all scenarios together ####
    pl = ggplot(df, aes(scenario, value)) 
    pl + geom_bar(aes(fill = mort_level), position="dodge",stat='identity') +
         theme_bw() + scale_x_discrete() +
        theme(axis.title.y = element_text(size=24,vjust=2),
              axis.title.x = element_text(size=24,vjust=-1),
              axis.text.x  = element_text(size=16),
              axis.text.y  = element_text(size=16)) +
        theme(axis.ticks.x = element_blank()) +
        theme(legend.title = element_text(size=16)) +
        theme(legend.text = element_text(size = 16)) +
        #theme(legend.position = c(0.1,.93)) +
        theme(plot.title = element_text(size=24,vjust=1)) +
        theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
        #theme(panel.grid.minor.x = element_blank(),
        #      panel.grid.major.x = element_blank(),
        #      panel.grid.minor.y = element_blank()) +
        ggtitle(paste("Area Burned by Wildfire")) + 
        ylab("Percent of Landscape Burned") +
        xlab("Climate Model") 

####################################
###################################
# get colors
n <- 3 
hcl(h=seq(15, 375-360/n, length=n)%%360, c=100, l=40) 
# now plot all scenarios together with hrv and gmean as lines ####

pl = ggplot(df, aes(scenario, value)) 
pl + geom_bar(aes(fill = mort_level), position="dodge",stat='identity') +
    theme_bw() + scale_fill_hue(l=40, labels=c('High Mortality','Low Mortality','Any Mortality'),name='Mortality Level') +
    ###
    geom_crossbar(data=tempg[tempg$mort_level=='mort.any',], 
                  aes(x='HRV',y=value, ymin=value, ymax=value), 
                  lty='dotted',lwd=1, color='#005CCC',show_guide=F) +
    geom_crossbar(data=tempg[tempg$mort_level=='mort.high',], 
                  aes(x='HRV',y=value, ymin=value, ymax=value), 
                  lty='dotted',lwd=1, color='#AE3121',show_guide=F) +
    geom_crossbar(data=tempg[tempg$mort_level=='mort.low',], 
                  aes(x='HRV',y=value, ymin=value, ymax=value), 
                  lty='dotted',lwd=1, color='#007700',show_guide=F) +
    ###
    theme(axis.title.y = element_text(size=32,vjust=2),
          axis.title.x = element_text(size=32,vjust=-1),
          axis.text.x  = element_text(size=24),
          axis.text.y  = element_text(size=24)) +
    theme(axis.ticks.x = element_blank()) +
    theme(legend.title = element_text(size=24)) +
    theme(legend.text = element_text(size=24)) +
    #theme(legend.position = c(0.1,.93)) +
    theme(plot.title = element_text(size=40,vjust=1)) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    #theme(panel.grid.minor.x = element_blank(),
    #      panel.grid.major.x = element_blank(),
    #      panel.grid.minor.y = element_blank()) +
    ggtitle(paste("Area Burned by Wildfire")) + 
    ylab("Percent of Landscape Burned") +
    xlab("Climate Model") 

# saved as 1400x700

##############################################
#############################################
# plot individual cover types ####

# get colors
n <- 3 
hcl(h=seq(15, 375-360/n, length=n)%%360, c=100, l=40) 
# now plot all scenarios together with hrv and gmean as lines

pl = ggplot(df, aes(scenario, value)) 
pl + geom_bar(aes(fill = mort_level), position="dodge",stat='identity') +
    theme_bw() + scale_fill_hue(l=40, labels=c('High Mortality','Low Mortality','Any Mortality'),name='Mortality Level') +
    ###
    geom_crossbar(data=tempg[tempg$mort_level=='mort.any',], 
                  aes(x='HRV',y=value, ymin=value, ymax=value), 
                  lty='dotted',lwd=1, color='#005CCC',show_guide=F) +
    geom_crossbar(data=tempg[tempg$mort_level=='mort.high',], 
                  aes(x='HRV',y=value, ymin=value, ymax=value), 
                  lty='dotted',lwd=1, color='#AE3121',show_guide=F) +
    geom_crossbar(data=tempg[tempg$mort_level=='mort.low',], 
                  aes(x='HRV',y=value, ymin=value, ymax=value), 
                  lty='dotted',lwd=1, color='#007700',show_guide=F) +
    ###
    ylim(y=c(0,7)) +
    theme(axis.title.y = element_text(size=32,vjust=2),
          axis.title.x = element_text(size=32,vjust=-1),
          axis.text.x  = element_text(size=24),
          axis.text.y  = element_text(size=24)) +
    theme(axis.ticks.x = element_blank()) +
    theme(legend.title = element_text(size=24)) +
    theme(legend.text = element_text(size=24)) +
    #theme(legend.position = c(0.1,.93)) +
    theme(plot.title = element_text(size=40,vjust=1)) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    #theme(panel.grid.minor.x = element_blank(),
    #      panel.grid.major.x = element_blank(),
    #      panel.grid.minor.y = element_blank()) +
    ggtitle(paste("Area Burned by Wildfire \n Sierran Mixed Conifer - Mesic")) + 
    ylab("Percent of Landscape Burned") +
    xlab("Climate Model") 

# saved as 1400x700
####################################
###################################

### code to show results aggregated across future scenarios

fsessions=c(9,8,10,13,14,20,21)
hsession = 6
fsessionnames = c('CCSM-2','CCSM-1','CCSM-3','CCSM-5','CCSM-4','CCSM-6','ESM2M')
hsessionname = 'HRV'
variable = 'mean'
futurepath = '/Users/mmallek/Tahoe/RMLands/results201507/future/'
histpath = '/Users/mmallek/Tahoe/RMLands/results201507/hrv/'


#read darea data
x<-read.csv(paste(futurepath,'darea.csv',sep=''),header=TRUE)

# no cover type specification for this one

#rescale cell counts
x$mort.high<-x$mort.high*((cell.size^2)/10000)
x$mort.low<-x$mort.low*((cell.size^2)/10000)
x$mort.any<-x$mort.any*((cell.size^2)/10000)


df = data.frame(summary_stat=factor(), mort_level = factor(), value = numeric(), session=factor())

# separate future from hrv
x2 = x[x$session.id %in% fsessions,]
w = x[x$session.id %in% hsession,]

# limit to only final timesteps
x2 = x2[x2$timestep.id >= fstart.step,]
w = w[w$timestep.id >= hstart.step,]


# now x is a data frame with all the cover types, runs, and timesteps
# if we want to aggregate across all of that, we can just do the calculations on what we have now

if(y.scale=='percent'){
    x2[,9:11]<-round((x2[,9:11]/174830.1)*100,3)
    w[,9:11]<-round((w[,9:11]/174830.1)*100,3)
}

temp<-matrix(0,nrow=4,ncol=4)        

temp[1,2:4]<-round(apply(x2[,9:11],2,min),3)
temp[2,2:4]<-round(apply(x2[,9:11],2,max),3)
temp[3,2:4]<-round(apply(x2[,9:11],2,median),3)
temp[4,2:4]<-round(apply(x2[,9:11],2,mean),3)

temp2<-matrix(0,nrow=4,ncol=4)        

temp2[1,2:4]<-round(apply(w[,9:11],2,min),3)
temp2[2,2:4]<-round(apply(w[,9:11],2,max),3)
temp2[3,2:4]<-round(apply(w[,9:11],2,median),3)
temp2[4,2:4]<-round(apply(w[,9:11],2,mean),3)

temp = as.data.frame(temp)
colnames(temp)<-c('summary_stat','mort.low','mort.high','mort.any')
temp[,1]<-c('minimum darea/timestep','maximum darea/timestep',
            'median darea/timestep','mean darea/timestep')
temp$summary_stat = as.factor(temp$summary_stat)

temp2 = as.data.frame(temp2)
colnames(temp2)<-c('summary_stat','mort.low','mort.high','mort.any')
temp2[,1]<-c('minimum darea/timestep','maximum darea/timestep',
            'median darea/timestep','mean darea/timestep')
temp2$summary_stat = as.factor(temp2$summary_stat)

#isolate row of info you want to plot
if(variable=='min') temp = temp[1,]
if(variable=='max') temp = temp[2,]
if(variable=='median') temp = temp[3,]
if(variable=='mean') temp = temp[4,]

#isolate row of info you want to plot
if(variable=='min') temp2 = temp2[1,]
if(variable=='max') temp2 = temp2[2,]
if(variable=='median') temp2 = temp2[3,]
if(variable=='mean') temp2 = temp2[4,]

tempg = gather(temp, mort_level, value, 2:4)
tempg$session = 'future'
temp2g = gather(temp2, mort_level, value, 2:4)
temp2g$session = 'hrv'

# combine the future and hrv data
df = bind_rows(df, tempg)
df = bind_rows(df, temp2g)

# clustered bar plots of future vs. hrv
pl = ggplot(df, aes(as.factor(session), value)) 
pl + geom_bar(aes(fill = mort_level), position="dodge",stat='identity') +
    theme_bw() + scale_x_discrete(labels=c('Future', 'HRV')) +
    theme(axis.title.y = element_text(size=24,vjust=2),
          axis.title.x = element_text(size=24,vjust=-1),
          axis.text.x  = element_text(size=16),
          axis.text.y  = element_text(size=16)) +
    theme(axis.ticks.x = element_blank()) +
    theme(legend.title = element_text(size=16)) +
    theme(legend.text = element_text(size = 16)) +
    #theme(legend.position = c(0.1,.93)) +
    theme(plot.title = element_text(size=24,vjust=1)) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    #theme(panel.grid.minor.x = element_blank(),
    #      panel.grid.major.x = element_blank(),
    #      panel.grid.minor.y = element_blank()) +
    ggtitle(paste("Area Burned by Wildfire")) + 
    ylab("Percent of Landscape Burned") +
    xlab("Climate Model") 
