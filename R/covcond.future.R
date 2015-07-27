### barplots showing covcond distribution across all scenarios
### so each individual plot should show one cover type
### based on fragland.barplot.future

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
                           'classland_ccsm4_20150723.land','classland_ccsm4_20150723.land',
                           'classland_ccsm6_20150723.land','classland_esm2m_20150723.land'),
             scenarios = c(landfiles = c('pastclimate', 'ccsm1','ccsm2','ccsm3',
                                         'ccsm4','ccsm4','ccsm6','esm2m'))
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
        
        
        #put the final table together
        z1<-z1[,c(1,2,7,27,52,77,97,102,105,103,104,106)]
        row.names(z1)<-NULL
        colnames(z1)<-c('landscape.metric','srv0%','srv5%','srv25%',
                        'srv50%','srv75%','srv95%','srv100%','srv.cv',
                        'current.value','current.%SRV','departure.index')
        
        
        
        #create list object
        z<-list(infile,scenarios,z1,z2)
        names(z)<-c('infile','scenario','landscape SRV',
                    'landscape departure index (%)')
        
        #output tables to file
        if(outfile==TRUE){
            write.table(z[[3]],file=paste(paste(fragpath,sep=''),'srv.csv',sep='.'),
                        quote=FALSE,row.names=FALSE,append=TRUE,sep=',')
            write.table(z[[4]],file=paste(paste(fragpath,sep=''),'ldi.csv',sep='.'),
                        quote=FALSE,row.names=FALSE,append=TRUE,sep=',')
        }
        
        return(z)
    }

#######################################
###################################
#########################################
path = '/Users/mmallek/Tahoe/RMLands/results201507/future/'
sessions=c(6,9,8,10,13,14,20,21)
scenario_name = c(landfiles = c('pastclimate', 'ccsm1','ccsm2','ccsm3',
                                'ccsm4','ccsm4','ccsm6','esm2m'))
cover.names='Oak-Conifer Forest and Woodland'
runs=18
    
covcondout<-covcond(path=futurepath,
                    sessions=21,
                    var='srv50%',
                    runs=c(1:100), #can pool runs
                    start.step=1,
                    stop.step=NULL,
                    cell.size=30,
                    #cover.names=c('Oak-Conifer Forest and Woodland'),
                    cover.min.ha=1000,
                    outfile=T)

scenarios = c(landfiles = c('pastclimate', 'ccsm1','ccsm2','ccsm3',
                            'ccsm4','ccsm4','ccsm6','esm2m'))

covcond.cover.barplot <-
    function(path,sessions=NULL,var='srv50%',runs=NULL,start.step=1,
             stop.step=NULL,cell.size=30,cover.names=NULL,cover.min.ha=0,outfile=FALSE){
        
        #set defaults
        options(warn=0)
        
        #read covcond data
        y0<-read.csv(paste(path,'covcond.csv',sep=''),header=TRUE)
        # switch timestep and run IDs
        # only want final run ID (18) for RV of future
        # each timestep actually a run, but we want all of them
        colnames(y0)[2:3] = c("timestep.id","run.id") # now run.id gives current
        y = y0
        
        #set session parameter
        if(is.null(sessions)) sessions<-unique(y$session.id)
        
        #verify valid session ids
        all.sessions<-unique(y$session.id)
        if(any(!sessions %in% all.sessions)) stop('Invalid session ids')
        
        #select subset of cover types
        if(!is.null(cover.names)){
            cov.levels<-levels(y$cov.name)
            if(any(!cover.names %in% cov.levels)) stop('Invalid cover names')
            y<-y[y$cov.name %in% cover.names,] 
        }
        
        # get rid of non-seral types
        y = y[y$cond.name != 'Non-seral',]
        #get cover type area and select cover types with min area
        # calculate area in each cover type
        # compare cover type areas to min cover type area specified in arguments
        # only keep cover types above the minimum
        t1<-y[y$session.id==sessions[1] & y$run.id==0 & y$timestep.id==1,]
        t2<-aggregate(t1$cell.count,list(t1$cov.name),sum)
        colnames(t2)<-c('cover.type','cov.count')
        t2$area.ha<-round(t2$cov.count*cell.size^2/10000,0)
        t2<-t2[t2$area.ha>cover.min.ha,]
        t2.cover.type<-t2$cover.type
        y<-y[y$cov.name %in% t2.cover.type,]

        
        #multiple sessions
        if(length(sessions)>1){
            
            #subset data based on session and dynamic cover types
            y<-y[y$session.id %in% sessions & y$cond.name != 'Non-seral' & 
                     y$cond.name != 'Non-seral cover types',]
            
            #set runs parameter
            if(is.null(runs)) runs<-unique(y$run.id)
            
            #verify valid run ids
            all.runs<-unique(y$run.id)
            if(any(!runs %in% all.runs)) stop('Invalid run ids')
            
            # extract only the final timesteps
            #y = y[y$timestep.id==18,]
            #y = y[y$session.id==6,]
            
            #set start.step and stop.step parameters
            #if(start.step>max(y$timestep.id)) stop('Start.step exceeds maximum timestep')
            #if(is.null(stop.step)) stop.step<-max(y$timestep.id)
            #else{
            #    if(stop.step>max(y$timestep.id)) warning('Stop.step exceeds maximum timestep and was set to the maximum')
            #    stop.step<-min(stop.step,max(y$timestep.id))
            #}
            
            #subset data based on runs, start.step, stop.step and min area
            #y<-y[y$run.id %in% runs & y$timestep.id>=start.step & y$timestep.id<=stop.step,]
            # this gets you the data frame with only the final timestep (18, as run.id)
            y<-y[y$run.id %in% runs,]
            
            #extra set of unique covcond classes
            y1<-y[order(y$cov.cond.id),]
            y1<-y1[(y1$cov.cond.id != y1$cov.cond.id[c((1:dim(y1)[1])[-1],1)]),
                   c('cov.cond.id','cov.name','cond.name')]
            
            #get current cell.count for each covcond class
            s1<-merge(y1,t1,by='cov.cond.id',all.x=TRUE)
            s2<-s1[,c(1,9)]
            s2[is.na(s2)]<-0
            
            
            #create dataframe for covcond stats results
            zz1<-y1
            colnames(zz1)<-c('cov.cond.id','cover.type','condition.class')
            
            #create dataframe for cover departure results
            d1<-as.data.frame(unique(y1$cov.name)) 
            colnames(d1)<-c('cover.type')
            d2<-t2[,c(1,3)]
            # this is just the cover type and its total area
            zz2<-merge(d1,d2,by='cover.type',sort=FALSE)
            colnames(zz2)<-c('cover.type','area.ha')
            
            # calculate current proportions
############# need to fix this            
            for(i in 1:nrow(zz2)){
                s2$proportion = s2$cell.count*.09/zz2[1,2]
            }
            s2$proportion = s2$cell.count*.09/zz2[1,2]
            current = cbind(zz1, s2)
            levels(current$condition.class) = c('Early-All Structures', 'Early-Aspen', 
                                     'Mid-Closed', 'Mid-Moderate', 'Mid-Open',
                                     'Mid-Aspen', "Mid-Aspen and Conifer", "Late-Conifer and Aspen", 
                                     'Late-Closed','Late-Moderate', 'Late-Open', 'Non-seral')
            
            #loop thru selected sessions
            for(j in 1:length(sessions)){
                
                #calculate SRV quantiles by covcond class
                # extract covcond codes
                cov.cond<-levels(as.factor(y$cov.cond.id))
                q1<-matrix(0,nrow=length(cov.cond),ncol=100)
                for(i in 1:length(cov.cond)){ 
                    # q2 isolates a single session, all the runs (aka timesteps), 
                    # and all the final timesteps (aka runs)
                    q2<-y[y$session.id==sessions[j] & y$cov.cond.id==cov.cond[i],]
                    # don't need to calculate quantiles for boxplots
                    #q1[i,1:101]<-quantile(q2$cell.count,probs=seq(0,1,0.01))
                    q1[i,1:nrow(q2)] = q2$cell.count
                }
                
                z1 = cbind(y1, q1)

                for(i in 1:length(unique(z1$cov.name))){
                    z1[z1$cov.name==unique(z1$cov.name)[i],4:103] = 
                           apply(z1[z1$cov.name==unique(z1$cov.name)[i],4:103], MARGIN=2, 
                                 FUN=function(x) x*.09/zz2[i,2])
                }
                #z2 = as.data.frame(z2)
                #z2 = cbind(y1, z1)
                z2 = merge(y1, z1)
                z2$cond.name = factor(z2$cond.name, levels=c('Early-All Structures', 'Early-Aspen', 
                                         'Mid-Closed', 'Mid-Moderate', 'Mid-Open',
                                         'Mid-Aspen', "Mid-Aspen and Conifer", "Late-Conifer and Aspen", 
                                         'Late-Closed','Late-Moderate', 'Late-Open', 'Non-seral'))
                
                # use gather on z2 data frame to get a column with the metric name, 
                # a column with that metric's value, for all of the fragland metrics
                data_long = gather(z2, run, proportion, -cov.cond.id, -cov.name, -cond.name)
                
                for(i in 1:length(unique(z1$cov.name))){
                p = ggplot(data=data_long[data_long$cov.name==unique(z1$cov.name)[i],], aes(x=cond.name, y=proportion )) 
                p1 = p +
###################### current has bad values for proportions                    
                    geom_crossbar(data=current[current$cover.type==unique(z1$cov.name)[i],], aes(x=condition.class,y=proportion, ymin=proportion, 
                                                    ymax=proportion), lwd=2,col="#333333") +
                    stat_summary(fun.data = f, geom="boxplot", ,fill="#339900") +
                    stat_summary(fun.y = o, geom="point", col="#CC3300") +
                    theme_bw() +
                    theme(axis.title.y = element_text(size=24,vjust=2),
                          axis.title.x = element_text(size=24,vjust=-1),
                          axis.text.x  = element_text(size=16),
                          axis.text.y  = element_text(size=16)) +
                    theme(legend.title=element_text(size=16)) +
                    theme(legend.text = element_text(size = 16)) +
                    theme(plot.title = element_text(size=24,vjust=1)) +
                    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
##################### need to put scenario into data frame
                    ggtitle(paste(unique(z1$cov.name)[i], scenario_name[1], sep=' ')) + 
                    xlab("Seral Stage") +
                    ylab("Proportion of Cover Type") 
                    print(p1)
                }
  ##################

### need to make another set of plots where it's across scenarios and for each seral stage
                
                
               
            }
            
            #create list object and print tables
            z<-list(sessions,runs,start.step,stop.step,zz1,zz2)
            names(z)<-c('sessions','maximum number of runs','start.step','maximum stop.step',
                        'cover-condition class statistics','cover type SRV departure index (sdi)')
            
        }
        
        #single session
        else{
            
            #subset data based on session and dynamic cover types
            y<-y[y$session.id==sessions & y$cond.name != 'Non-seral' & 
                     y$cond.name != 'Non-seral cover types',]
            
            #set runs parameter
            if(is.null(runs)) runs<-unique(y$run.id)
            
            #verify valid run ids
            all.runs<-unique(y$run.id)
            if(any(!runs %in% all.runs)) stop('Invalid run ids')
            
            #set start.step and stop.step parameters
            if(start.step>max(y$timestep.id)) stop('Start.step exceeds maximum timestep')
            if(is.null(stop.step)) stop.step<-max(y$timestep.id)
            else{
                if(stop.step>max(y$timestep.id)) warning('Stop.step exceeds maximum timestep and was set to the maximum')
                stop.step<-min(stop.step,max(y$timestep.id))
            }
            
            #subset data based on runs, start.step, stop.step and min area
            y<-y[y$run.id %in% runs & y$timestep.id>=start.step & y$timestep.id<=stop.step,]
            
            #get unique covcond class
            y1<-y[order(y$cov.cond.id),]
            y1<-y1[(y1$cov.cond.id != y1$cov.cond.id[c((1:dim(y1)[1])[-1],1)]),
                   c('cov.cond.id','cov.name','cond.name')]
            
            #get current cell.count for each covcond class
            s1<-merge(y1,t1,by='cov.cond.id',all.x=TRUE)
            s2<-s1[,c(1,9)]
            s2[is.na(s2)]<-0
            
            #create dataframe for cover departure results
            d1<-as.data.frame(unique(y1$cov.name)) 
            colnames(d1)<-c('cover.type')
            d2<-t2[,c(1,3)]
            z2<-merge(d1,d2,by='cover.type',sort=FALSE)
            colnames(z2)<-c('cover.type','area.ha')
            
            #calculate SRV quantiles by covcond class
            cov.cond<-levels(as.factor(y$cov.cond.id))
            q1<-matrix(0,nrow=length(cov.cond),ncol=101)
            for(i in 1:length(cov.cond)){ 
                q2<-y[y$session.id==sessions & y$cov.cond.id==cov.cond[i],]
                q1[i,1:101]<-quantile(q2$cell.count,probs=seq(0,1,0.01))
            }
            
            #put most of it together
            z1<-matrix(0,nrow=length(cov.cond),ncol=14)
            z1<-as.data.frame(z1)
            z1[,1:3]<-y1[,1:3]
            z1[,4:10]<-q1[,c(1,6,26,51,76,96,101)]
            z1[,12]<-s2[,2]
            colnames(z1)<-c('covcond.id','cover.type','condition.class',
                            'srv0%','srv5%','srv25%','srv50%','srv75%','srv95%','srv100%','srv.cv',
                            'current.%cover','current.%srv','departure.index')
            z1<-merge(z1,t2,by='cover.type',sort=FALSE)
            
            #calculate srv.cv and current.%SRV for each covcond class
            for(i in 1:length(cov.cond)){
                if(is.na(z1[i,4])) z1[i,c(11,13)]<-'NA'
                else if(z1[i,10]-z1[i,4]==0) z1[i,c(11,13)]<-'NA'
                else{
                    z1[i,11]<-round(((z1[i,9]-z1[i,5])/z1[i,7])*100,0)
                    z1[i,13]<-max(0,which(q1[i,]<z1[i,12]))  	
                    if(z1[i,13]==101) z1[i,13]<-100
                }
            }
            
            #calculate SRV departure index for each class
            z1[,13]<-as.numeric(z1[,13])
            for(i in 1:nrow(z1)){ 		
                if(is.na(z1[i,13])) z1[i,14]<-'NA'
                else{
                    if(z1[i,13]==50) z1[i,14]<-0
                    else if(z1[i,13]<50) z1[i,14]<-round((z1[i,13]-50)/50*100,0)
                    else if(z1[i,13]>50) z1[i,14]<-round((z1[i,13]-50)/50*100,0)
                }
            }
            z1[,14]<-as.numeric(z1[,14])
            format(z1,big.mark=',')
            
            #rescale cell counts; convert SRV percentiles to percentages
            z1[,c(4:10,12)]<-round((z1[,c(4:10,12)]/z1$cov.count)*100,2)
            z1<-z1[,-15] #drop cov.count column
            z1<-z1[,c(2,1,3:14)] #reorder columns
            
            #calculate SRV departure index for each cover type
            d2<-aggregate(abs(z1[,14]),list(z1[,2]),mean,na.rm=TRUE)
            colnames(d2)<-c('cover.type','sdi')
            z2<-merge(z2,d2,by='cover.type',sort=FALSE)
            z2[,3]<-round(z2[,3],0)
            format(z2,big.mark=',')
            
            #create list object and print tables
            z<-list(sessions,runs,start.step,stop.step,z1,z2)
            names(z)<-c('sessions','runs','start.step','stop.step',
                        'cover-condition class statistics','cover type SRV departure index (sdi)')
            
        }
        
        #output tables to file
        if(outfile==TRUE){
            write.table(z[[5]],file=paste(path,'covcond.stats.csv',sep=''),
                        quote=FALSE,append=TRUE,row.names=FALSE,sep=',')
            write.table(z[[6]],file=paste(path,'cov.departure.csv',sep=''),
                        quote=FALSE,append=TRUE,row.names=FALSE,sep=',')  
        }
        
        return(z)
    }

#####################################
#######################################
#################################

covcond.plot <-
    function(path,session=NULL,var='srv50%',runs=1,start.step=0,
             stop.step=NULL,step.length=NULL,type='stack',cell.size=30,
             cover.names=NULL,cover.min.ha=0,
             col.bars=c('black','tan','brown','orange','seagreen','salmon',
                        'green','cyan','yellow','magenta','coral','wheat',
                        'gray','tan2','brown2','orange2','seagreen2','salmon2',
                        'green2','cyan2','yellow2','magenta2','coral2','wheat2'),
             col.sub='brown',cex.main=1.5,cex.sub=1.5,cex.legend=1.5,cex.lab=1.5,
             outfile=FALSE,save.figs=FALSE,...){
        
        #set defaults
        options(warn=0)
        old.par<-par(no.readonly=TRUE)
        
        #read covcond data
        y<-read.csv(paste(path,'covcond.csv',sep=''),header=TRUE)
        
        #set session parameter
        if(is.null(session)) session<-unique(y$session.id)
        
        #verify valid session ids
        all.sessions<-unique(y$session.id)
        if(any(!session %in% all.sessions)) stop('Invalid session ids')
        
        #assign colors to master list of condition classes
        temp<-subset(y,select=c(cov.cond.id,cond.name))
        temp<-unique(temp)
        all.conds<-as.vector(unique(y$cond.name))
        if(length(col.bars)>length(all.conds)) col.bars<-col.bars[1:length(all.conds)]
        cond.colors<-as.data.frame(cbind(cond.name=all.conds,colors=col.bars))
        cond.colors<-merge(temp,cond.colors)
        
        #clustered bar charts for multiple sessions
        if(length(session)>1){
            
            #create covcond stats for plot
            y<-covcond(path=path,sessions=session,var=var,runs=runs,start.step=start.step,
                       stop.step=stop.step,cell.size=cell.size,cover.names=cover.names,
                       cover.min.ha=cover.min.ha)[[5]]
            
            #create list object
            cov.levels<-unique(y$cover.type)
            z<-vector("list", length(cov.levels))
            names(z)<-cov.levels
            
            #loop thru cover types
            for(i in 1:length(cov.levels)){ 
                
                #subset data based on cov.id
                x<-y[y$cover.type==cov.levels[i],]
                
                #get condition classes
                cond.levels<-x[,c(1,3)]
                names(cond.levels)<-c('cov.cond.id','cond.name')
                cond.levels<-cond.levels[order(cond.levels$cov.cond.id),]
                n<-nrow(cond.levels)
                
                #assign colors
                col.bars<-merge(cond.levels,cond.colors)
                col.bars<-as.character(col.bars$colors)
                
                #save results to list object
                z[[i]]<-x
                print(z[[i]])
                x<-as.matrix(x[,-c(1:3)])
                
                #plot to file
                if(save.figs==TRUE){
                    bitmap(file=paste(cov.levels[i],'.png',sep=''),
                           height=6,width=8,res=300,...) 
                }
                
                #create clustered bar chart
                #barplot(x,beside=TRUE,border='dark gray',
                barplot(x,beside=TRUE,
                        xaxs='i',yaxs='i',col=col.bars,cex.lab=cex.lab,
                        axis.lty=1,...)
                
                #add legend
                cond.levels<-as.vector(cond.levels[,2])				
                legend(x='topright',inset=c(0.02,0.02),
                       legend=cond.levels,fill=col.bars,cex=cex.legend)
                
                #add plot title				
                if(var=='srv.cv'){
                    title(main=paste('Cover-Condition Summary (',var,')',sep=''),line=2.5,
                          ylab='Coefficient of Variation',xlab='Scenario/Session',
                          cex.main=cex.main,...)
                }
                else{title(main=paste('Cover-Condition Summary (',var,')',sep=''),line=2.5,
                           ylab='Percentage of Cover Type',xlab='Scenario/Session',
                           cex.main=cex.main,...)
                }
                
                #add subtitle: dist.type and cov.type
                mtext(side=3,col=col.sub,cex=cex.sub,text=cov.levels[i],line=1,...)
                
                if(save.figs==TRUE) dev.off()
                
                if(save.figs==FALSE & !i==length(cov.levels))
                    readline("Press return for next plot ")
            }
            
            #output tables to file
            if(outfile==TRUE){
                for(i in 1:length(z)){
                    write.table(z[[i]],file=paste(path,cov.levels[i],' Summary.csv',sep=''),
                                quote=FALSE,row.names=FALSE,sep=',',append=FALSE)
                }
            }
            
        }
        
        #covcond trajectory plots for single session	
        else{
            
            #subset data based on session and dynamic cover types
            y<-y[y$session.id==session & y$cond.name != 'Non-seral' &
                     y$cond.name != 'Non-seral cover types',]
            
            #verify valid run.ids
            if(is.null(runs)) runs<-unique(y$run.id)
            else{
                all.runs<-unique(y$run.id)
                if(!runs %in% all.runs) stop('Invalid run ids')
            }
            
            #set start.step and stop.step
            if(start.step>max(y$timestep.id)) stop('Start.step exceeds maximum timestep')
            if(is.null(stop.step)) stop.step<-max(y$timestep.id)
            else{
                if(stop.step>max(y$timestep.id)) warning('Stop.step exceeds maximum timestep and was set to the maximum')
                stop.step<-min(stop.step,max(y$timestep.id))
            }
            
            #select subset of cover types
            if(!is.null(cover.names)){
                cov.levels<-levels(y$cov.name)
                if(any(!cover.names %in% cov.levels)) stop('Invalid cover names')
                y<-y[y$cov.name %in% cover.names,] 
            }
            
            #calculate total area by cover type for min area cover types
            t1<-y[y$run.id==1 & y$timestep.id==0,]
            t2<-aggregate(t1$cell.count,list(t1$cov.name),sum)
            colnames(t2)<-c('cover.type','cov.count')
            t2$area.ha<-round(t2$cov.count*cell.size^2/10000,0)
            t2<-t2[t2$area.ha>cover.min.ha,]
            t2.cover.type<-t2$cover.type
            
            #subset data based on run, start.step, stop.step and min area
            y<-y[y$run.id %in% runs & y$timestep.id>=start.step & y$timestep.id<=stop.step & 
                     y$cov.name %in% t2.cover.type,]
            
            #create timestep variable
            timestep<-seq(start.step,stop.step,1)
            timestep<-as.data.frame(timestep)
            
            #create list object
            cov.levels<-unique(y$cov.name)
            z<-vector("list", length(cov.levels))
            names(z)<-cov.levels
            
            #loop thru cover types
            for(i in 1:length(cov.levels)){ 
                
                #subset data based on cov.id
                x<-y[y$cov.name==cov.levels[i],c(2,3,4,6,7)]
                names(x)<-c('run.id','timestep','cov.cond.id','cond.name','cell.count')
                
                #create list objects
                z.run<-vector("list",length(runs))
                names(z.run)<-runs
                
                #loop thru runs
                for(j in 1:length(runs)){
                    
                    #subset data based on run.id
                    q<-x[x$run.id==runs[j],-1]
                    
                    #get condition classes
                    cond.levels<-as.data.frame(unique(q[,2:3]))
                    cond.levels<-cond.levels[order(cond.levels$cov.cond.id),]
                    n<-nrow(cond.levels)
                    
                    #assign colors
                    col.bars<-merge(cond.levels,cond.colors)
                    col.bars<-as.character(col.bars$colors)
                    
                    #fill out complete dataset
                    x2<-merge(timestep,cond.levels)
                    x2<-merge(x2,q,by=c('timestep','cov.cond.id','cond.name'),
                              all.x=TRUE,sort=TRUE)
                    x2[is.na.data.frame(x2)]<-0
                    
                    #restructure dataset for plotting
                    t1<-timestep
                    for(k in 1:n){
                        x3<-x2[x2$cov.cond.id==cond.levels[k,1],c(1,4)]
                        names(x3)[2]<-as.vector(cond.levels[k,2])
                        t1<-merge(t1,x3,by='timestep',all.x=TRUE)
                    }
                    
                    #convert cell.count to proportion of cover
                    t1[,2:(n+1)]<-t1[,2:(n+1)]/apply(t1[,-1],1,sum)
                    
                    #save results to list object		
                    z.run[[j]]<-round(t1,3)
                    print(z.run[[j]])
                    
                    #plot to file
                    if(save.figs==TRUE){
                        bitmap(file=paste(cov.levels[i],runs[j],'.png',sep=''),
                               height=6,width=8,res=300,...) 
                    }      
                    
                    #plot disturbance area trajectory as 100% stacked bar chart
                    if(type=='stack'){
                        
                        #create 100% stacked bar chart
                        #print(round(t1,2))
                        barplot(t(t1[,-1]),space=0,border=NA,
                                axs='s',yaxs='i',col=col.bars,
                                #xaxs='i',yaxs='i',col=col.bars,
                                axis.lty=1,names=t1$timestep,cex.lab=cex.lab,...)
                        
                        #add legend
                        cond.levels<-as.vector(cond.levels[,2])				
                        legend(x='topright',inset=c(0.02,0.02),
                               legend=cond.levels,fill=col.bars,cex=cex.legend)
                    }
                    
                    #plot disturbance area trajectory as lines
                    else{
                        
                        #create line chart
                        #print(round(t1,2))
                        matplot(t1[,1]*10,t1[,2:ncol(t1)],type='l',
                                ylab='',xlab='',lty=1,col=col.bars,cex.lab=cex.lab,...)
                        
                        #add legend
                        cond.levels<-as.vector(cond.levels[,2])				
                        legend(x='topright',inset=c(0.02,0.02),
                               legend=cond.levels,lty=1,col=col.bars,cex=cex.legend,lwd=1.5)
                    }
                    abline(v=40)
                    
                    #add plot title	
                    if(is.null(step.length)) xlab='Timestep'
                    else xlab=paste('Timestep (x',step.length,' yrs)')
                    title(main='Cover-Condition Dynamics',line=2.5,
                          ylab='Proportion of Cover Type',xlab=xlab,cex.main=cex.main,...)
                    
                    #add subtitle: dist.type and cov.type
                    mtext(side=3,col=col.sub,cex=cex.sub,
                          text=paste(cov.levels[i],': Run #',runs[j],sep=''),line=1,...)
                    
                    if(save.figs==TRUE) dev.off()
                    
                    if(save.figs==FALSE & (!i==length(cov.levels) | !j==length(runs)))
                        readline("Press return for next plot ")
                    
                } #end loop thru runs
                
                z[[i]]<-z.run
                
                
            } #end loop thru cover types
            
            #output tables to file
            if(outfile==TRUE){
                for(i in 1:length(z)){
                    write.table(z[[i]],file=paste(path,cov.levels[i],' dynamics.csv',sep=''),
                                quote=FALSE,row.names=FALSE,sep=',',append=FALSE)
                }
            }
            
        }
        
        par(old.par)
        invisible(z)
    }