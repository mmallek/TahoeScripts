darea<-function(path,session=NULL,var='mean',runs=NULL,start.step=1,
	stop.step=NULL,step.length,cell.size=30,y.scale='percent',
	col.bar=c('blue','light blue','turquoise'),
	col.text='brown',outfile=FALSE,...){

##things to do:
#add plot by cover type? see preturn for possible approach

options(warn=0)
old.par<-par(no.readonly=TRUE)
	
#read darea data
x<-read.csv(paste(path,'darea.csv',sep=''),header=TRUE)

#rescale cell counts
x$mort.high<-round(x$mort.high*((cell.size^2)/10000),0)
x$mort.low<-round(x$mort.low*((cell.size^2)/10000),0)
x$mort.any<-round(x$mort.any*((cell.size^2)/10000),0)

#set global session and runs parameters
if(is.null(session)) session<-levels(as.factor(x$session.id))

#clustered bar charts for multiple sessions
if(length(session)>1){

  #subset data based on session.id
  x<-x[x$session.id %in% session,]
  
  #set runs parameters based on max run.id
  if(is.null(runs)) runs<-max(x$run.id)
  else{
    if(runs>max(x$run.id)) warning('Runs exceeds maximum run id and was set to the maximum')
    runs<-min(runs,max(x$run.id))
    }
  
  #subset data based on runs
  x<-x[x$run.id<=runs,]
  run.levels<-as.vector(unique(x$run.id))
  
  #create list objects for results
  dist.levels<-as.vector(unique(x$dist.type))
  z<-vector("list",length(dist.levels))
  names(z)<-paste(dist.levels,' darea comparison (',y.scale,')',sep='')
  z[[i]]<-vector("list",length(run.levels))
  names(z[[i]])<-paste('run number ',run.levels,sep='')
  
	#loop thru disturbance types
	for(i in 1:length(dist.levels)){ 

		#loop thru runs
		for(j in 1:length(run.levels)){

      #subset data for dist.type and run.id
		  x2<-x[x$dist.type==dist.levels[i] & x$run.id==run.levels[j],]
		  
		  #set start.step and stop.step parameters
		  if(start.step>max(x2$timestep.id)) stop('Start.step exceeds maximum timestep')
		  if(is.null(stop.step)) stop.step<-max(x2$timestep.id)
		  else{
		    if(stop.step>max(x2$timestep.id)) warning('Stop.step exceeds maximum timestep and was set to the maximum')
		    stop.step<-min(stop.step,max(x2$timestep.id))
		    }
		  
		  #subset data based on start.step and stop.step
		  x2<-x2[x2$timestep.id>=start.step & x2$timestep.id<=stop.step,]
		  timestep<-seq(start.step,stop.step,1)
		  timestep<-as.data.frame(timestep)
		  
		  session<-sort(unique(x2$session.id))
		  
      #create results matrix
			y2<-matrix(0,nrow=3,ncol=length(session))
			rownames(y2)<-c('low.mort','high.mort','any.mort')
			colnames(y2)<-session

			#loop thru sessions
			for(k in 1:length(session)){
	
				#summarize darea by dist.type, run and timestep
			  y<-x2[x2$session.id==session[k],]
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
					t<-t[t$session.id==session[k] & t$dist.type==dist.levels[i],]
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
			barplot(y2,beside=TRUE,border='black',
				xaxs='i',yaxs='i',col=col.bar,
				axis.lty=1,...)
	
			#add plot title	- dist.type			
			if(y.scale=='percent')
				title(main=paste(dist.levels[i],' (',var,' darea/timestep',')',sep=''),
					ylab='Percent of Eligible',xlab='Scenario/Session')
			else
				title(main=paste(dist.levels[i],' (',var,' darea/timestep',')',sep=''),
					ylab='Area (ha)',xlab='Scenario/Session')
	
			#add subtitle - run number
			mtext(side=3,col=col.text,text=paste('Run #',run.levels[j],sep=''))
	
			#add legend				
			legend(x='topright',inset=c(0.05,0.05),
				legend=c('Low mort','High mort','Any mort'),fill=col.bar)
		
			if(!i==length(dist.levels) || !j==runs) 
				readline("Press return for next plot ")

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
  x<-x[x$session.id %in% session,]
  
  #set runs parameters
  if(is.null(runs)) runs<-max(x$run.id)
  else{
    if(runs>max(x$run.id)) warning('Runs exceeds maximum run id and was set to the maximum')
    runs<-min(runs,max(x$run.id))
    }
  
  #subset data based on run.id
  x<-x[x$run.id<=runs,]
  run.levels<-as.vector(unique(x$run.id))
  
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
  
	#create list objects for results
  dist.levels<-as.vector(unique(x$dist.type))
  z1<-vector("list",length(dist.levels))
	names(z1)<-paste(dist.levels,' disturbance trajectory (',y.scale,')',sep='')
  z2<-vector("list",length(dist.levels))
  names(z2)<-paste(dist.levels,' disturbance summary (',y.scale,')',sep='')
  z1[[i]]<-vector("list",length(run.levels))
  names(z1[[i]])<-paste('run number ',run.levels,sep='')
  z2[[i]]<-vector("list",length(run.levels))
  names(z2[[i]])<-paste('run number ',run.levels,sep='')
  
	#loop thru disturbance types
	for(i in 1:length(dist.levels)){
    
		#loop thru runs 
		for(j in 1:length(run.levels)){

			#summarize darea by dist.type, run and timestep
			y<-x[x$dist.type==dist.levels[i] & x$run.id==run.levels[j],]
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
				t<-t[t$session.id==session & t$dist.type==dist.levels[i],]
				t<-round(t$cell.count*((cell.size^2)/10000),0)
				y[,2:4]<-round((y[,2:4]/t)*100,2)
				}
	
			#print results to console and list object		
			z1[[i]][[j]]<-y
			print(names(z1)[[i]][[j]])
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
			print(names(z2)[[i]][[j]])
			print(format(z2[[i]][[j]],big.mark=','))
	
			#plot disturbance area trajectory
			barplot(t(y[,c(3,2)]),space=0,
				xaxs='i',yaxs='i',col=col.bar,
				axis.lty=1,names=y$timestep,...)
	
			#add plot title	- dist.type			
			if(y.scale=='percent')
				title(main=paste(dist.levels[i],'Disturbance Trajectory',sep=' '),
					ylab='Percent of Eligible',
          xlab=paste('Timestep (x',step.length,' yrs)'))
			else
				title(main=paste(dist.levels[i],'Disturbance Trajectory',sep=' '),
					ylab='Area (ha)',
				  xlab=paste('Timestep (x',step.length,' yrs)'))
	
			#add subtitle - run number
			mtext(side=3,col=col.text,text=paste('Run #',run.levels[j],sep=''))
	
			#add legend				
			legend(x='topright',inset=c(0.05,0.05),
				legend=c('High mort','Low mort'),fill=col.bar)
		
			if(!i==length(dist.levels) || !j==runs) 
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