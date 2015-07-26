
#fragpath should be path to fragstats.land file
#scenario should be name of model and run value, e.g. ccsm2_run1
#nrun is the number of runs completed for the scenario
#stop.run is the number of runs to analyze
#LID.path is path up to tif file
#covcondlist is path to csv with list of covcond files

        
fragland.future <-
    function(fragpath,infile,LID.path,scenarios=NULL,
             nrun=NULL, 
             covcondlist='/Users/mmallek/Tahoe/RMLands/upload_20150529/covcondlist_500ts.csv',
             metrics=NULL,outfile=FALSE){
        
        #set defaults
        options(warn=0)
        
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
        y1<-cbind(y.head,y.metrics)
        
        
        #calculate landscape metric SRV quantiles
        q1<-matrix(0,ncol(y1)-2,101)
        for(i in 1:nrow(q1)){ 
            q1[i,1:101]<-quantile(y1[,i+2],probs=seq(0,1,0.01),na.rm=TRUE)
        }
            
            #get current landscape metric (timestep 0)
            current.value<-round(t(y1[y1$run==0,-c(1:2)][1,]),3)
            
            #put most of it together
            z1<-round(as.data.frame(q1),3)
            landscape.metric<-colnames(y1)[-c(1:2)]
            z1<-cbind(landscape.metric,z1)
            z1<-cbind(z1,current.value)
            temp<-matrix(0,nrow(z1),2)
            z1<-cbind(z1,temp)
            
            #calculate srv.cv and current landscape metric %SRV
            for(i in 1:nrow(z1)){
                if(z1[i,102]-z1[i,2]==0) z1[i,c(104,105)]<-'NA'
                else{
                    z1[i,105]<-round(((z1[i,97]-z1[i,7])/z1[i,52])*100,0)
                    z1[i,104]<-max(0,which(q1[i,]<z1[i,103]))		
                    if(z1[i,104]==101) z1[i,104]<-100
                }
            }
            
            #compute departure index
            #	t1<-rep(0,nrow(z1))
            #	z1<-cbind(z1,t1)
            #	z1[,104]<-as.numeric(z1[,104])
            #	for(i in 1:nrow(z1)){ 		
            #		if(is.na(z1[i,104])) z1[i,106]<-'NA'
            #		else{
            #		    if(z1[i,104]<50) z1[i,106]<-round((z1[i,104]-50)/50*100,0)
            #			else if(z1[i,104]>50) z1[i,106]<-round((z1[i,104]-50)/50*100,0)
            #			else z1[i,106]<-0
            #			}
            #		}
            #	z1[,106]<-as.numeric(z1[,106])
            #	z2<-mean(abs(z1[,106]),na.rm=TRUE)
            #	z2<-round(z2,0)
            
            # my departure index
            # if the percentile value is less than 50
            t1<-rep(0,nrow(z1))
            z1<-cbind(z1,t1)
            z1[,104]<-as.numeric(z1[,104])
            for(i in 1:nrow(z1)){ 		
                if(is.na(z1[i,104])) z1[i,106]<-'NA'
                else{
                    if(z1[i,104]<50) z1[i,106]<-round( 
                        (z1[i,103] - z1[i,52])/ # actual minus the median
                            (z1[i,52] - z1[i,2]) * 100, # median minus 0
                        0)
                    else if(z1[i,104]>50)
                        z1[i,106]<-round( 
                            (z1[i,103] - z1[i,52])/ # actual minus the median
                                (z1[i,102] - z1[i,52]) * 100, # 100th minus median 
                            0)
                    else z1[i,106]<-0
                }
            }
            z1[,106]<-as.numeric(z1[,106])
            z2<-mean(abs(z1[,106]),na.rm=TRUE)
            z2<-round(z2,0)
            
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