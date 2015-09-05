#### New darea function for future data
require(tidyr)
require(dplyr)
require(ggplot2)
require(grid)

sessions=c(9,8,10,13,14,20,21)
sessionnames = c('CCSM-1','CCSM-2','CCSM-3','CCSM-4','CCSM-5','CCSM-6','ESM2M')
futurepath = '/Users/mmallek/Tahoe/RMLands/results201507/future/'
start.step = 14

darea.future <-
    function(path,sessions=,var='mean',runs=,start.step=,
            step.length=5, covtype=NULL, cell.size=30,y.scale='percent',
             col.bars=c('blue','light blue','turquoise'),col.sub='brown',
             cex.main=1.5,cex.sub=1.5,cex.legend=1.5,outfile=FALSE,...){
        
        
        #read darea data
        x<-read.csv(paste(futurepath,'darea.csv',sep=''),header=TRUE)
        
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

        

######## code to build a clustered bar chart

sessions=c(9,8,10,13,14,20,21)
sessionnames = c('CCSM-2','CCSM-1','CCSM-3','CCSM-4','CCSM-5','CCSM-6','ESM2M')

#read darea data
x<-read.csv(paste(path,'darea.csv',sep=''),header=TRUE)

# no cover type specification for this one

#rescale cell counts
x$mort.high<-x$mort.high*((cell.size^2)/10000)
x$mort.low<-x$mort.low*((cell.size^2)/10000)
x$mort.any<-x$mort.any*((cell.size^2)/10000)

df = data.frame(summary_stat=factor(), mort_level = factor(), value = numeric(), session=integer())
for(i in 1:length(sessions)){
    
    # for now let's assume 1 session is specified
    y<-x[x$session.id == sessions[i],]
    
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
    #y2<-y2[order(y2$timestep),]
    
    
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
    temp2$session = sessions[i]
    
    df = bind_rows(df, temp2)

    ### end building individual tables
 
# now plot all scenarios together
    pl = ggplot(df, aes(as.factor(session), value)) 
    pl + geom_bar(aes(fill = mort_level), position="dodge",stat='identity') +
         theme_bw() + scale_x_discrete(labels=sessionnames) +
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
}
        





####################################
###################################
### code to show results aggregated across future scenarios

fsessions=c(9,8,10,13,14,20,21)
hsession = 6
fsessionnames = c('CCSM-2','CCSM-1','CCSM-3','CCSM-4','CCSM-5','CCSM-6','ESM2M')
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
temp2g = gather(temp, mort_level, value, 2:4)
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



















#####        ########################
        
        #clustered bar charts for multiple sessions
        if(length(sessions)>1){     
            #create list objects for results
            z<-vector("list",length(dist.levels))
            names(z)<-paste(dist.levels,' darea comparison (',y.scale,')',sep='')
            
            #loop thru disturbance types
            for(i in 1:length(dist.levels)){ 
                
                #create list objects for results
                z[[i]]<-vector("list",length(runs))
                names(z[[i]])<-paste('run number ',runs,sep='')
                
                #loop thru runs
                for(j in 1:length(runs)){
                    
                    #subset data for dist.type and run.id
                    x2<-x[x$dist.type==dist.levels[i] & x$run.id==runs[j],]
                    
                    #sort sesssion.id
                    sessions<-sort(unique(x2$session.id))
                    
                    #create results matrix
                    y2<-matrix(0,nrow=3,ncol=length(sessions))
                    rownames(y2)<-c('low.mort','high.mort','any.mort')
                    colnames(y2)<-sessions
                    
                    #loop thru sessions
                    for(k in 1:length(sessions)){
                        
                        #select records for session
                        y<-x2[x2$session.id==sessions[k],]
                        
                        #summarize darea by dist.type, run and timestep
                        y.low<-aggregate(y$mort.low,list(y$timestep.id),sum)
                        colnames(y.low)<-c('timestep','mort.low')
                        y.high<-aggregate(y$mort.high,list(y$timestep.id),sum)
                        colnames(y.high)<-c('timestep','mort.high')
                        y.any<-aggregate(y$mort.any,list(y$timestep.id),sum)
                        colnames(y.any)<-c('timestep','mort.any')
                        y<-merge(y.low,y.high,by='timestep',sort=FALSE)
                        y<-merge(y,y.any,by='timestep',sort=FALSE)
                        y<-merge(timestep,y,by='timestep',all.x=TRUE,sort=FALSE)
                        y[is.na.data.frame(y)]<-0
                        y<-y[order(y$timestep),]
                        
                        #optionally convert darea to percent of eligible
                        if(y.scale=='percent'){
                            t<-read.csv(paste(path,'eligible.csv',sep=''),header=TRUE)
                            t<-t[t$session.id==sessions[k] & t$dist.type==dist.levels[i],]
                            t<-round(t$cell.count*((cell.size^2)/10000),0)
                            y[,2:4]<-round((y[,2:4]/t)*100,2)
                        }
                        
                        #compute darea summary
                        if(var=='min') y2[,k]<-round(apply(y[,2:4],2,min),2)
                        else if(var=='max') y2[,k]<-round(apply(y[,2:4],2,max),2)
                        else if(var=='median') y2[,k]<-round(apply(y[,2:4],2,median),2)
                        else if(var=='mean') y2[,k]<-round(apply(y[,2:4],2,mean),2)
                        
                    }
                    
                    #print summary to console and list object
                    mortality<-c('low.mort','high.mort','any.mort')
                    z[[i]][[j]]<-as.data.frame(cbind(mortality,y2))
                    print(y2)
                    
                    #create clustered bar chart
                    barplot(y2,beside=TRUE,border=NA,#'black',
                            xaxs='i',yaxs='i',col=col.bars,
                            axis.lty=1,...)
                    
                    #add plot title	- dist.type			
                    if(y.scale=='percent')
                        title(main=paste(dist.levels[i],' (',var,' darea/timestep',')',sep=''),
                              ylab='Percent of Eligible',xlab='Scenario/Session',
                              cex.main=cex.main,...)
                    else
                        title(main=paste(dist.levels[i],' (',var,' darea/timestep',')',sep=''),
                              ylab='Area (ha)',xlab='Scenario/Session',cex.main=cex.main,...)
                    
                    #add subtitle - run number
                    mtext(side=3,col=col.sub,cex=cex.sub,
                          text=paste('Run #',runs[j],sep=''),...)
                    
                    #add legend				
                    legend(x='bottomright',#inset=c(0.05,0.05),
                           legend=c('Low mort','High mort','Any mort'),
                           fill=col.bars,cex=cex.legend)

                    
                }
            }
            
            #output tables to file
            if(outfile==TRUE){
                for(i in 1:length(z)){
                    write.table(z[[i]],file=paste(path,dist.levels[i],'_darea_comparison.csv',sep=''),
                                quote=FALSE,row.names=FALSE,sep=',',append=TRUE)
                }
            }
        }
        
        #stacked bar chart trajectory for single sesssion	
        else{
            
            #subset data based on session.id
            x<-x[x$session.id==sessions,]
            
            #verify valid run.ids
            if(is.null(runs)) runs<-unique(x$run.id)
            else{
                all.runs<-unique(x$run.id)
                if(!runs %in% all.runs) stop('Invalid run ids')
            }
            
            #subset data based on runs
            x<-x[x$run.id %in% runs,]
            
            #set start.step and stop.step parameters
            if(start.step>max(x$timestep.id)) stop('Start.step exceeds maximum timestep')
            if(is.null(stop.step)) stop.step<-max(x$timestep.id)
            else{
                if(stop.step>max(x$timestep.id)) warning('Stop.step exceeds maximum timestep and was set to the maximum')
                stop.step<-min(stop.step,max(x$timestep.id))
            }
            
            #subset data based on start.step and stop.step
            x<-x[x$timestep.id>=start.step & x$timestep.id<=stop.step,]
            timestep<-seq(start.step,stop.step,1)
            timestep<-as.data.frame(timestep)
            
            #create dist.level vector for loops
            dist.levels<-as.vector(unique(x$dist.type))
            
            #create list objects for results
            z1<-vector("list",length(dist.levels))
            names(z1)<-paste(dist.levels,' disturbance trajectory (',y.scale,')',sep='')
            z2<-vector("list",length(dist.levels))
            names(z2)<-paste(dist.levels,' disturbance summary (',y.scale,')',sep='')
            
            #loop thru disturbance types
            for(i in 1:length(dist.levels)){
                
                #create list objects for results
                z1[[i]]<-vector("list",length(runs))
                names(z1[[i]])<-paste('run number ',runs,sep='')
                z2[[i]]<-vector("list",length(runs))
                names(z2[[i]])<-paste('run number ',runs,sep='')
                
                #loop thru runs 
                for(j in 1:length(runs)){
                    
                    # here would be a place to subset by cover type
                    
                    #summarize darea by dist.type, run and timestep
                    y<-x[x$dist.type==dist.levels[i] & x$run.id==runs[j],]
                    y.low<-aggregate(y$mort.low,list(y$timestep.id),sum)
                    colnames(y.low)<-c('timestep','mort.low')
                    y.high<-aggregate(y$mort.high,list(y$timestep.id),sum)
                    colnames(y.high)<-c('timestep','mort.high')
                    y.any<-aggregate(y$mort.any,list(y$timestep.id),sum)
                    colnames(y.any)<-c('timestep','mort.any')
                    y<-merge(y.low,y.high,by='timestep',sort=FALSE)
                    y<-merge(y,y.any,by='timestep',sort=FALSE)
                    y<-merge(timestep,y,by='timestep',all.x=TRUE,sort=FALSE)
                    y[is.na.data.frame(y)]<-0
                    y<-y[order(y$timestep),]
                    
                    #optionally convert darea to percent of eligible
                    # this eligible bit is what ruins everything
                    # have to put a dummy string in for y in the function call
                    if(y.scale=='percent'){
                        t<-read.csv(paste(path,'eligible.csv',sep=''),header=TRUE)
                        t<-t[t$session.id==sessions & t$dist.type==dist.levels[i],]
                        t<-round(t$cell.count*((cell.size^2)/10000),0)
                        y[,2:4]<-round((y[,2:4]/t)*100,2)
                    }
                    
                    
                    #print results to console and list object		
                    z1[[i]][[j]]<-y
                    print(format(z1[[i]][[j]],big.mark=','))
                    
                    #compute darea summary
                    temp<-matrix(0,nrow=4,ncol=4)		
                    colnames(temp)<-c('summary statistic','mort.low','mort.high','mort.any')
                    temp[,1]<-c('minimum darea/timestep','maximum darea/timestep',
                                'median darea/timestep','mean darea/timestep')
                    temp[1,2:4]<-round(apply(y[,2:4],2,min),2)
                    temp[2,2:4]<-round(apply(y[,2:4],2,max),2)
                    temp[3,2:4]<-round(apply(y[,2:4],2,median),2)
                    temp[4,2:4]<-round(apply(y[,2:4],2,mean),2)
                    
                    #print summary to console and list object
                    z2[[i]][[j]]<-as.data.frame(temp)
                    print(format(z2[[i]][[j]],big.mark=','))
                    
                    #plot disturbance area trajectory
                    ############## ADDED BORDER=NA ####################
                    barplot(t(y[,c(3,2)]),space=0, border=NA,
                            #    barplot(y2,beside=TRUE,border=NA,#'black',
                            
                            xaxs='i',yaxs='i',col=c('dark blue', 'dark green'),#col.bars,
                            axis.lty=1,names=y$timestep,...)
                    
                    # to add a line at the equilibration period
                    #abline(v=40,lty=2,lwd=2,col="black")
                    
                    #add plot title	- dist.type	
                    if(is.null(step.length)) xlab='Timestep'
                    else xlab=paste('Timestep (x',step.length,' yrs)')
                    if(y.scale=='percent')
                        title(main=paste(dist.levels[i],'Disturbance Trajectory',sep=' '),
                              ylab='Percent of Eligible',xlab=xlab,cex.main=cex.main,...)
                    else
                        title(main=paste(dist.levels[i],'Disturbance Trajectory',sep=' '),
                              ylab='Area (ha)',xlab=xlab,cex.main=cex.main,...)
                    
                    #add subtitle - run number
                    mtext(side=3,col=col.sub,cex=cex.sub,
                          text=paste('Run #',runs[j],sep=''),...)
                    
                    #add legend		
                    #####################################################
                    ### I CHANGED A LINE HERE
                    #######################################################
                    legend(x='topright',#inset=c(0.05,0.05),
                           #legend=c('High mort','Low mort'),fill=col.bars,cex=cex.legend)
                           legend=c('High mort','Low mort'),fill=c('dark blue','dark green'),cex=cex.legend)
                    if(!i==length(dist.levels) || !j==length(runs)) 
                        readline("Press return for next plot ")
                }
            }
            
            #create list object
            z<-list(z1,z2)
            names(z)<-c('Disturbance Area Trajectory','Disturbance Area Summary')
            
            #output tables to file
            if(outfile==TRUE){
                for(i in 1:length(z1)){
                    write.table(z1[[i]],file=paste(path,dist.levels[i],'_darea_trajectory.csv',sep=''),
                                quote=FALSE,row.names=FALSE,sep=',',append=TRUE)
                    write.table(z2[[i]],file=paste(path,dist.levels[i],'_darea_summary.csv',sep=''),
                                quote=FALSE,row.names=FALSE,sep=',',append=TRUE)
                }
            }
        }
        
        par(old.par)
        invisible(z)
    }