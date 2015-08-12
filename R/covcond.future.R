# this script contains 3 functions
# fragstats plots for landscape metrics
# covcond plots by each cover type, for each scenario
# covcond plots by each cov-cond type across scenarios



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

#######################################
###################################
#########################################
path = '/Users/mmallek/Tahoe/RMLands/results201507/future/'
sessions=c(6,9,8,10,13,14,20,21)
scenario_name = c('pastclimate', 'ccsm1','ccsm2','ccsm3',
                'ccsm4','ccsm5','ccsm6','esm2m'))
#cover.names='Oak-Conifer Forest and Woodland'
runs=18
cover.min.ha=1000

scenarios = c('pastclimate', 'ccsm1','ccsm2','ccsm3',
            'ccsm4','ccsm5','ccsm6','esm2m')

covcond.cover.boxplot <-
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
            
            # this gets you the data frame with only the final timestep (18, as run.id)
            y<-y[y$run.id %in% runs,]
            
            #extra set of unique covcond classes
            y1<-y[order(y$cov.cond.id),]
            y1<-y1[(y1$cov.cond.id != y1$cov.cond.id[c((1:dim(y1)[1])[-1],1)]),
                   c('cov.cond.id','cov.name','cond.name')]
            
            #get current cell.count for each covcond class
            s1<-merge(y1,t1,by='cov.cond.id',all.x=TRUE)
            s2<-s1[,c(1,2,9)]
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
            s2$proportion = s2$cell.count
            for(i in 1:length(unique(s2$cov.name))){
                s2[s2$cov.name==unique(s2$cov.name)[i],4] = s2[s2$cov.name==unique(s2$cov.name)[i],3] *.09/zz2[i,2]
            }
            current = merge(zz1, s2)
            current$cov.name.x=NULL
            #levels(current$condition.class) = c('Early-All Structures', 'Early-Aspen', 
            #                         'Mid-Closed', 'Mid-Moderate', 'Mid-Open',
            #                         'Mid-Aspen', "Mid-Aspen and Conifer", "Late-Conifer and Aspen", 
            #                         'Late-Closed','Late-Moderate', 'Late-Open', 'Non-seral')
            
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
                #data_long$ = relevel(z$scenario, "pastclimate")
                
                for(i in 1:length(unique(z1$cov.name))){
                    p = ggplot(data=data_long[data_long$cov.name==unique(z1$cov.name)[i],], aes(x=cond.name, y=proportion )) 
                    p1 = p +                
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
                        ggtitle(paste(unique(z1$cov.name)[i], "\n", scenario_name[j])) + 
                        xlab("Seral Stage") +
                        ylab("Proportion of Cover Type") 
                        print(p1)
                        if(saveimage==TRUE){
                            ggsave(paste(unique(z1$cov.name)[i],scenario_name[j],"boxplots",".png",sep="-"), 
                                   path="/Users/mmallek/Tahoe/RMLands/results201507/future/images/",
                                   width=15, height=5, units='in',limitsize=FALSE)                
                    }  
                }   
               
            }
}
##################
##################### need to put scenario into data frame
### it is, it's the session id
### need to make another set of plots where it's across scenarios and for each seral stage

path = '/Users/mmallek/Tahoe/RMLands/results201507/future/'
sessions=c(6,9,8,10,13,14,20,21)
scenario_name = c('pastclimate', 'ccsm1','ccsm2','ccsm3','ccsm4','ccsm5','ccsm6','esm2m')
#cover.names='Oak-Conifer Forest and Woodland'
runs=18

covcond.scenario.boxplot(
    path = '/Users/mmallek/Tahoe/RMLands/results201507/future/',
    sessions=c(6,9,8,10,13,14,20,21),
    scenario_name = c('pastclimate', 'ccsm1','ccsm2','ccsm3','ccsm4','ccsm5','ccsm6','esm2m'),
    runs=18,
    cell.size=30,
    cover.min.ha=1000,
    saveimage=TRUE
    )

covcond.scenario.boxplot <-
    function(path,sessions=NULL,scenario_name=NULL,runs=NULL,cell.size=30,
             cover.names=NULL,cover.min.ha=0,saveimage=FALSE){
        
        #set defaults
        options(warn=0)
        
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
        
        #read covcond data
        y0<-read.csv(paste(path,'covcond.csv',sep=''),header=TRUE)
        # switch timestep and run IDs
        # only want final run ID (18) for RV of future
        # each timestep actually a run, but we want all of them
        colnames(y0)[2:3] = c("timestep.id","run.id") # now run.id gives current
        # careful with this line - it is basically hard coded so sessions and scenario_name object
        # better be in the right order and of the same length
        for(i in 1:length(sessions)){
            y0$scenario[y0$session.id==sessions[i]] = scenario_name[i]
        }
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
            
        #set runs parameter
        if(is.null(runs)) runs<-unique(y$run.id)
        
        #verify valid run ids
        all.runs<-unique(y$run.id)
        if(any(!runs %in% all.runs)) stop('Invalid run ids')
        
        # this gets you the data frame with only the final timestep (18, as run.id)
        y<-y[y$run.id %in% runs,]
        
        # delete columns not of interest
        y1 = y
        y1$timestep.id = NULL
        y1$run.id = NULL
        y1$cell.count = NULL
        y1 = distinct(y1) # set of unique cov-cond-scenario combinations
    
        #new dataframe that's a set of unique covcond classes
        y2<-y[order(y$cov.cond.id),]
        y2<-y2[(y2$cov.cond.id != y2$cov.cond.id[c((1:dim(y2)[1])[-1],1)]),
               c('cov.cond.id','cov.name','cond.name')]
        
        #get current cell.count for each covcond class
        s1<-merge(y2,t1,by='cov.cond.id',all.x=TRUE)
        s2<-s1[,c(1,2,9)]
        s2[is.na(s2)]<-0
        
        #create dataframe for covcond stats results
        #zz1<-y1
        # not needed 
        # colnames(zz1)<-c('cov.cond.id','cover.type','condition.class')
        
        #create dataframe for cover departure results
        d1<-as.data.frame(unique(y1$cov.name)) 
        colnames(d1)<-c('cover.type')
        d2<-t2[,c(1,3)]
        # this is just the cover type and its total area
        zz2<-merge(d1,d2,by='cover.type',sort=FALSE)
        colnames(zz2)<-c('cover.type','area.ha')
    
        # data frame to hold current info
        zz3 = y2
    
        # calculate current proportions
        s2$proportion = s2$cell.count
        for(i in 1:length(unique(s2$cov.name))){
            s2[s2$cov.name==unique(s2$cov.name)[i],4] = s2[s2$cov.name==unique(s2$cov.name)[i],3] *.09/zz2[i,2]
        }
        # maybe 
        current = merge(zz3, s2)
        ####### possible hard coding
        current = current[,-4]
        
        # maybe it will work by using the covcond id, instead of 2 loops?
        covcond.code = sort(unique(current$cov.cond.id))
        # remember scenarios called by sessions or scenario_name
        
        # do all the rest of the work in these loops?
        
        # create the empty matrix to hold the data
        q1<-matrix(0,nrow=length(sessions),ncol=100)
        y3 = matrix(0, nrow=1, ncol=ncol(y1)+ncol(q1))
        for(j in 1:length(covcond.code)){
            # q2 is a subsetted matrix for one cover-condition combo
            # and all the scenarios
            q2<-y[y$cov.cond.id==covcond.code[j],]
            for(k in 1:length(sessions)){
                rownames(q1) = scenario_name #gives rows meaningful names
                # next line stores in q1 the cell counts extracted from q2 for each covcond type
                q1[k,1:length(q2$cell.count[q2$session.id==sessions[k]])] = 
                    q2$cell.count[q2$session.id==sessions[k]]
                y3 = bind_cols(y1[y1$cov.cond.id==covcond.code[j],], as.data.frame(q1))
            }

            name = unique(q2[q2$cov.cond.id==covcond.code[j],5])
            area = zz2$area.ha[zz2$cover.type==name]
            y3[,6:105] = 
                apply(y3[,6:105], MARGIN=2, FUN=function(x) x*.09/area)
            
            # give real names to columns
            colnames(y3)[6:105] = paste("Run",seq(1,100, 1))
            # put scenarios in order
            ########y3$scenario = factor(y3$scenario, levels=c('pastclimate', 'ccsm1','ccsm2','ccsm3','ccsm4','ccsm5','ccsm6','esm2m'))
            # gather all the proportions for plotting
            data_long = gather(y3, run.timestep, proportion, -session.id,-cov.cond.id, -cov.name, -cond.name, -scenario)
            
            # make the plot
            p = ggplot(data_long, aes(x=scenario, y=proportion )) 
            currentvalue = current[current$cov.cond.id==covcond.code[j],]
            p1 = p +         
                stat_summary(fun.data = f, geom="boxplot", ,fill="#339900") +
                stat_summary(fun.y = o, geom="point", col="#CC3300") +
                geom_hline(data=currentvalue, aes(yintercept=proportion), lwd=3, col="#333333") +                        
                theme_bw() +
                theme(axis.title.y = element_text(size=24,vjust=2),
                      axis.title.x = element_text(size=24,vjust=-1),
                      axis.text.x  = element_text(size=16),
                      axis.text.y  = element_text(size=16)) +
                theme(legend.title=element_text(size=16)) +
                theme(legend.text = element_text(size = 16)) +
                theme(plot.title = element_text(size=24,vjust=1)) +
                theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
                ggtitle(paste(data_long$cov.name[1], "\n", data_long$cond.name[1])) + 
                xlab("Climate Model") +
                ylab("Proportion of Cover Type") 
                print(p1)
            if(saveimage==TRUE){
                ggsave(paste(covcond.code[j],"-boxplots",".png",sep=""), 
                       path="/Users/mmallek/Tahoe/RMLands/results201507/future/images/",
                       width=10, height=5, units='in',limitsize=FALSE)                
            }          
        }
    }


            

#####################################
#######################################
#################################
