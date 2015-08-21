#fragpath should be path to fragstats.land file
#scenario should be name of model and run value, e.g. ccsm2_run1
#nrun is the number of runs completed for the scenario
#stop.run is the number of runs to analyze
#LID.path is path up to tif file
#covcondlist is path to csv with list of covcond files

fragland.plot.future <-
    function(fragpath, infile,path,LID.path,nrun=NULL,scenarios=NULL,
             covcondlist='/Users/mmallek/Tahoe/RMLands/upload_20150529/covcondlist_500ts.csv',
             metrics=NULL,start.step=0,stop.run=NULL,
             quantiles=c(0.05,0.95),col.line='dark blue',col.sub='brown',
             cex.main=1.5,cex.sub=1.5,cex.legend=1.5,cex.lab=1.5,
             save.figs=FALSE,...){
        
        #set defaults
        options(warn=0)
        old.par<-par(no.readonly=TRUE)
        
        #read fragstats data
        y<-read.csv(fragpath,strip.white=TRUE,header=TRUE)
        
        # read covcond list
        z = read.csv(covcondlist, header=F)
        #create runs variable so that each final timestep is associated with a run
        z$run = seq(0,nrun,1)
        colnames(z)[1:2] = c("file","run")
        
        y$LID = NULL
        
        y = cbind(z, y)
        
        # set metrics parameter
        # this allows you to include only a subset of the metrics
        all.metrics<-colnames(y)[-c(1:2)]
        if(is.null(metrics)) metrics<-all.metrics
        if(any(!metrics %in% all.metrics)) stop('Invalid metrics selected')
        
        #select columns matching metrics from previous steps
        y.metrics<-subset(y,select=metrics)
        y.head<-y[,1:2]
        y<-cbind(y.head,y.metrics)
        
        #set file-dependent defaults
        # may not need these
        if(is.null(stop.run)){
            stoprun<-max(y$run)
        }
        else{
            if(stop.run>max(y$run)) 
                warning('Stop.run exceeds maximum timestep and will be set to the maximum')
            stoprun<-min(stop.run,max(y$run))
        }
        if(start.run>=stoprun) 
            stop('Start.run must be less than maximum timestep')
        
        # make new dataframe that only includes the runs specified
        q1<-y[y$run>=start.step & y$run<=stoprun,]
        
        #reestablish runs
        # can't determine purpose of this step
        runs<-sort(as.vector(unique(q1$run)))
        
        #loop thru metrics
        for(i in 3:ncol(q1)){ # the first number is whichever corresponds to PD
            #create plot limits
            ymin<-min(min(q1[,i]),y[y$run==0,i])
            ymax<-max(max(q1[,i]),y[y$run==0,i])
            yrange<-ymax-ymin
            
            #set x label
            xlab=paste('Final timesteps, individual runs')
            
            #plot to file
            if(save.figs==TRUE){
                bitmap(file=paste(names(q2)[i],runs[j],'.png',sep=''),
                       height=6,width=8,res=300,...) 
            }
            #create plot
            plot(q2[,4],q2[,i],type='l',lwd=2,col=col.line,
                 ylim=c(ymin-(yrange*0.01),ymax+(yrange*0.1)),
                 #xaxs='i',xlab=xlab,ylab=names(q2[i]),...)
                 xaxs='r',xlab=xlab,ylab=names(q2[i]),...)
            plot(q1[,2],q1[,i],type='l',lwd=2,col=col.line,ylim=c(ymin-(yrange*0.01),
                                                                  ymax+(yrange*0.1)),xaxs='r',xlab=xlab,ylab=names(q1[i]))
            
            #add current condition and %SRV lines
            # current condition
            abline(h=y[y$run==0,i],lwd=2,lty=1,col='red')
            abline(h=quantile(q1[,i],0.5,na.rm=TRUE),lwd=2,lty=2,col='darkgrey')
            abline(h=quantile(q1[,i],quantiles[1],na.rm=TRUE),lwd=2,lty=3,col='darkgrey')
            abline(h=quantile(q1[,i],quantiles[2],na.rm=TRUE),lwd=2,lty=3,col='darkgrey')
            
            #add plot title				
            title(main=paste('Landscape Metric Trajectory',
                             ' (',names(q1)[i],')',sep=''),line=2.5,cex.main=cex.main,...)
            #add subtitle
            mtext(side=3,line=1,col=col.sub,cex=cex.sub,
                  text=scenarios)
            
            #add legend
            legend(x='top',horiz=TRUE, legend=c('current',paste('q',quantiles[1],sep=''),
                                                'q0.5',paste('q',quantiles[2],sep='')),
                   lty=c(1,3,2,3),bty='n',lwd=2,
                   col=c('red','darkgrey','darkgrey','darkgrey'),cex=cex.legend)
            if(save.figs==TRUE) dev.off()
            
            if(save.figs==FALSE & !j==length(runs))
                readline('Press return for next plot')
        }
            if(save.figs==FALSE & !i==ncol(q1))
                readline('Press return for next plot')
        
        par(old.par)
    }


#fragpath should be path to fragstats.land file
#scenario should be name of model and run value, e.g. ccsm2_run1
#nrun is the number of runs completed for the scenario
#stop.run is the number of runs to analyze
#LID.path is path up to tif file
#covcondlist is path to csv with list of covcond files

require(tidyr)
require(dplyr)
require(ggplot2)

fragland.boxplot <-
    function(fragpath='/Users/mmallek/Tahoe/RMLands/results201507/future/fragresults/',
             infile,
             scenarios=NULL,
             nrun=NULL, 
             covcondlist='/Users/mmallek/Tahoe/RMLands/upload_20150529/covcondlist_500ts.csv',
             metrics=NULL,
             landfiles = c('classland_pastclimate_20150723.land', 'classland_ccsm1_20150723.land',
                           'classland_ccsm2_20150723.land','classland_ccsm3_20150723.land',
                           'classland_ccsm4_20150723.land','classland_ccsm5_20150723.land',
                           'classland_ccsm6_20150723.land','classland_esm2m_20150723.land'),
             scenarios = c(landfiles = c('pastclimate', 'ccsm1','ccsm2','ccsm3',
                                         'ccsm4','ccsm5','ccsm6','esm2m'))
             outfile=FALSE){
        
        #set defaults
        options(warn=0)
        
        z<-read.csv(paste(fragpath,landfiles[1],sep=''),strip.white=TRUE,header=TRUE)
        z$scenario = scenarios[1]
    
        #read fragstats data if there's more than the initial dataframe
        if(length(landfiles>1)){
            for(i in 1:length(landfiles)){
                w<-read.csv(paste(fragpath,landfiles[i],sep=''),strip.white=TRUE,header=TRUE)
                w$scenario = scenarios[i]
                z = bind_rows(z,w)
            }
        }
        
        z$scenario = as.factor(z$scenario)
        z$scenario = relevel(z$scenario, "pastclimate")
        
        # read covcond list
        w = read.csv(covcondlist, header=F)
        #create runs variable so that each final timestep is associated with a run
        w$run = seq(0,nrun,1)
        colnames(w)[1:2] = c("file","run")
        
        z$LID = NULL
        y = cbind(w,z)
        
        # set metrics parameter
        # this allows you to include only a subset of the metrics
        all.metrics<-colnames(y)[-c(1:2,25)]
        if(is.null(metrics)) metrics<-all.metrics
        if(any(!metrics %in% all.metrics)) stop('Invalid metrics selected')
        
        #select columns matching metrics from previous steps
        y.metrics<-subset(y,select=metrics)
        y.head<-y[,c(1:2,25)]
        y1<-cbind(y.head,y.metrics)
   
        # make a box and whisker plot

        # use gather on y1 data frame to get a column with the metric name, 
        # a column with that metric's value, for all of the fragland metrics
        # data_long = gather(y1, metric, value, PD:AI)
        
        # define the summary function
        f <- function(x) {
            r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
            names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
            r
        }
        # summary function for outliers
        o <- function(x) {
            subset(x, x < quantile(x, probs = c(0.05, 0.95))[1] | quantile(x, probs = c(0.05, 0.95))[2] < x)
        }

        for(i in 1:length(metrics)){     
            p = ggplot(data=y1[y1$run!=0,], aes(x=factor(scenario), y=y1[y1$run!=0,metrics[i]] )) 
            p1 = p + 
                stat_summary(fun.data = f, geom="boxplot", ,fill="#339900", width=0.5) +
                stat_summary(fun.y = o, geom="point", col="#CC3300") +
                geom_hline(aes(yintercept=y1[y1$run==0,metrics[i]]), lwd=3, col="#333333") +
                theme_bw() +
                theme(axis.title.y = element_text(size=24,vjust=1),
                      axis.title.x = element_text(size=24,vjust=-1),
                      axis.text.x  = element_text(size=16),
                      axis.text.y  = element_text(size=16)) +
                theme(legend.title=element_text(size=16)) +
                theme(legend.text = element_text(size = 16)) +
                theme(plot.title = element_text(size=24,vjust=1)) +
                theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
                ggtitle(paste("Landscape Metric: ", metrics[i], sep='')) + 
                xlab("Climate Scenario") +
                ylab("Metric Value") 
            print(p1)
            ggsave(paste(metrics[i], "-boxplots",".png",sep=""), 
                   path="/Users/mmallek/Tahoe/RMLands/results201507/future/images/",
                   width=15, height=5, units='in',limitsize=FALSE
            )    
        }

    }


###############################################
###############################################
### Moved from covcond.future.R

fragland.boxplot <-
    function(fragpath='/Users/mmallek/Tahoe/RMLands/results201507/future/fragresults/',
             infile,
             nrun=NULL, 
             covcondlist='/Users/mmallek/Tahoe/RMLands/upload_20150529/covcondlist_500ts.csv',
             metrics=NULL,
             landfiles = c('classland_pastclimate_20150723.land', 'classland_ccsm1_20150723.land',
                           'classland_ccsm2_20150723.land','classland_ccsm3_20150723.land',
                           'classland_ccsm4_20150723.land','classland_ccsm5_20150723.land',
                           'classland_ccsm6_20150723.land','classland_esm2m_20150723.land'),
             scenarios = c('pastclimate', 'ccsm1','ccsm2','ccsm3',
                           'ccsm4','ccsm5','ccsm6','esm2m'),
             outfile=FALSE){
        
        #set defaults
        options(warn=0)
        
        z<-read.csv(paste(fragpath,landfiles[1],sep=''),strip.white=TRUE,header=TRUE)
        z$scenario = scenarios[1]
        
        #read fragstats data if there's more than the initial dataframe
        if(length(landfiles>1)){
            for(i in 1:length(landfiles)){
                w<-read.csv(paste(fragpath,landfiles[i],sep=''),strip.white=TRUE,header=TRUE)
                w$scenario = scenarios[i]
                z = bind_rows(z,w)
            }
        }
        
        z$scenario = as.factor(z$scenario)
        z$scenario = relevel(z$scenario, "pastclimate")
        
        # read covcond list
        w = read.csv(covcondlist, header=F)
        #create runs variable so that each final timestep is associated with a run
        w$run = seq(0,nrun,1)
        colnames(w)[1:2] = c("file","run")
        
        z$LID = NULL
        y = cbind(w,z)
        
        # set metrics parameter
        # this allows you to include only a subset of the metrics
        all.metrics<-colnames(y)[-c(1:2,25)]
        if(is.null(metrics)) metrics<-all.metrics
        if(any(!metrics %in% all.metrics)) stop('Invalid metrics selected')
        
        #select columns matching metrics from previous steps
        y.metrics<-subset(y,select=metrics)
        y.head<-y[,c(1:2,25)]
        y1<-cbind(y.head,y.metrics)
        
        # make a box and whisker plot
        
        # use gather on y1 data frame to get a column with the metric name, 
        # a column with that metric's value, for all of the fragland metrics
        # data_long = gather(y1, metric, value, PD:AI)
        
        # define the summary function
        f <- function(x) {
            r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
            names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
            r
        }
        # summary function for outliers
        o <- function(x) {
            subset(x, x < quantile(x, probs = c(0.05, 0.95))[1] | quantile(x, probs = c(0.05, 0.95))[2] < x)
        }
        
        for(i in 1:length(metrics)){     
            p = ggplot(data=y1[y1$run!=0,], aes(x=factor(scenario), y=y1[y1$run!=0,metrics[i]] )) 
            p1 = p + 
                stat_summary(fun.data = f, geom="boxplot", ,fill="#339900") +
                stat_summary(fun.y = o, geom="point", col="#CC3300") +
                geom_hline(aes(yintercept=y1[y1$run==0,metrics[i]]), lwd=3, col="#333333") +
                theme_bw() +
                theme(axis.title.y = element_text(size=24,vjust=1),
                      axis.title.x = element_text(size=24,vjust=-1),
                      axis.text.x  = element_text(size=16),
                      axis.text.y  = element_text(size=16)) +
                theme(legend.title=element_text(size=16)) +
                theme(legend.text = element_text(size = 16)) +
                theme(plot.title = element_text(size=24,vjust=1)) +
                theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
                ggtitle(paste("Landscape Metric: ", metrics[i], sep='')) + 
                xlab("Climate Scenario") +
                ylab("Metric Value") 
            print(p1)
            ggsave(paste(metrics[i], "-boxplots",".png",sep=""), 
                   path="/Users/mmallek/Tahoe/RMLands/results201507/future/images/",
                   width=15, height=5, units='in',limitsize=FALSE)    
        }
    }
