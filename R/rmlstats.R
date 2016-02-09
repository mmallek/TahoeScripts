
calibrate <-
function(path,session,runs=NULL,var='any.mort',
  start.step=1,stop.step=NULL,step.length,cell.size=30,
  disturb=c('fb','pb','pd','sb','sbw','wfire'),new=FALSE,outfile=FALSE){

z<-vector("list",length(disturb))
names(z)<-disturb

if(new==TRUE){
	for(i in 1:length(disturb)){
    z[[i]]<-read.csv(paste(path,disturb[i],'.targets.csv',sep=''),
      header=TRUE)
    }
	}

else{                                       
	for(i in 1:length(disturb)){
    z[[i]]<-read.csv(paste(path,disturb[i],'.calibrate','.csv',sep=''),
      header=TRUE)
    }
	}

for(i in 1:length(disturb)){
	t<-rotation(path=path,session=session,runs=runs,var=var,
	start.step=start.step,stop.step=stop.step,step.length=step.length,
  cell.size=cell.size)[[i+5]]
  if(var=='low.mort') t<-t[,3]
  else if(var=='high.mort') t<-t[,4]
  else t<-t[,5]
	t<-as.data.frame(t)
	colnames(t)<-paste('s',session,sep='')
	z[[i]]<-cbind(z[[i]],t)
	}

#output tables to file
if(outfile==TRUE){
	for(i in 1:length(disturb)){
		write.table(z[[i]],file=paste(path,disturb[i],'.calibrate','.csv',sep=''),
		quote=FALSE,row.names=FALSE,sep=',',append=FALSE)
		}
	}
return(z)
}
contrast <-
function(path='D:/landeco/exercises/rmlands/fragstats/',
  w1='contrast.floristics.csv', w2='contrast.structure.csv', 
  w3='contrast.canopy.cover.csv', w4='contrast.development.csv', 
  rules= 'contrast.rules.csv', out='contrast.fsq'){

#contrast.R - Create edge contrast matrix for Fragstats
#Usage: contrast(path, floristics.csv, structure.csv, canopy.cover.csv, development.csv, rules.csv, results.csv)
#Hard wired to expect four weights files
#Input weight files have classes in sequential order
#Input weight files are assumed to be symmetrical with missing values in the lower half
#Must have function available to run >source('.../contrast.R')
#K. McGarigal
#April 28, 2006
#modified June 12, 2007
#modified Oct 2, 2014 to reflect update to fragstats4.2

#create weights matrices
read.contrast<-function(path,file){						#define read function
	z<-read.csv(paste(path,file,sep=''), header=TRUE)	#assign weights file
	z<-as.matrix(z[,-1])								#coerce to matrix and remove first column of row numbers
	if(all(is.na(z[lower.tri(z)]))) 					#determine if lower half of matrix is all N/A (missing data)
	{			
		 z[lower.tri(z)]<-t(z)[lower.tri(z)]			#replace N/A lower half of matrix with upper half values
	}
	z													#return results to z
}
w1<-read.contrast(path,w1)								#assign to w1 the first weights file
w2<-read.contrast(path,w2)								#assign to w2 the second weights file
w3<-read.contrast(path,w3)								#assign to w3 the third weights file
w4<-read.contrast(path,w4)								#assign to w4 the third weights file

#create rules dataframe
rules<-read.csv(paste(path,rules,sep=''), row.names=1, header=TRUE)

#sum weights for each pairwise combination of cover classes
temp1<-w1[rules[,1],rules[,1]]+w2[rules[,2],rules[,2]]+w3[rules[,3],rules[,3]]
temp2<-w4[rules[,4],rules[,4]]
temp3<-pmax(temp1,temp2)

#write results to csv file
outfile<-paste(path,out,sep='')
write.table('FSQ_TABLE',file=outfile,quote=FALSE,col.names=FALSE,row.names=FALSE)
a<-paste('CLASS_LIST_NUMERIC(',paste(rownames(rules),collapse=','),')\n',sep='')
cat(a,file=outfile,append=TRUE)
write.table(temp3,file=outfile,quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',',append=TRUE)

}
covcond <-
function(path,sessions=NULL,var='srv50%',runs=NULL,start.step=1,
	stop.step=NULL,cell.size=30,cover.names=NULL,cover.min.ha=0,outfile=FALSE){

#set defaults
options(warn=0)
  
#read covcond data
y<-read.csv(paste(path,'covcond.csv',sep=''),header=TRUE)

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

#get cover type area and select cover types with min area
t1<-y[y$session.id==sessions[1] & y$run.id==1 & y$timestep.id==0,]
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
  
  #create dataframe for covcond stats results
  zz1<-y1
  colnames(zz1)<-c('covcond.id','cover.type','condition.class')
  
  #create dataframe for cover departure results
  d1<-as.data.frame(unique(y1$cov.name)) 
  colnames(d1)<-c('cover.type')
  d2<-t2[,c(1,3)]
  zz2<-merge(d1,d2,by='cover.type',sort=FALSE)
  colnames(zz2)<-c('cover.type','area.ha')

  #loop thru selected sessions
  for(j in 1:length(sessions)){
    
    #calculate SRV quantiles by covcond class
    cov.cond<-levels(as.factor(y$cov.cond.id))
    q1<-matrix(0,nrow=length(cov.cond),ncol=101)
    for(i in 1:length(cov.cond)){ 
      q2<-y[y$session.id==sessions[j] & y$cov.cond.id==cov.cond[i],]
      q1[i,1:101]<-quantile(q2$cell.count,probs=seq(0,1,0.01))
      }
    
    #put most of it together
    z1<-matrix(0,nrow=length(cov.cond),ncol=14)
    z1<-as.data.frame(z1)
    z1[,1:3]<-y1[,1:3]
    z1[,4:10]<-q1[,c(1,6,26,51,76,96,101)]
    z1[,12]<-s2[,2]
    colnames(z1)<-c('covcond.id','cover.type','condition.class',
      'srv0%','srv5%','srv25%','srv50%','srv75%','srv95%','srv100%',
      'srv.cv','current.%cover','current.%srv','departure.index')
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
    
    #merge selected covcond stat to final results table
    temp<-subset(z1,select=var)
    names(temp)<-paste(var,'.',sessions[j],sep='')
    zz1<-cbind(zz1,temp)
    
    #calculate SRV departure index for each cover type
    d2<-aggregate(abs(z1[,14]),list(z1[,2]),mean,na.rm=TRUE)
    names(d2)<-c('cover.type',paste('sdi','.',sessions[j],sep=''))    
    zz2<-merge(zz2,d2,by='cover.type',sort=FALSE)
    zz2[,-1]<-round(zz2[,-1],0)
    format(zz2,big.mark=',')
    
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
  # original index
      z1[,13]<-as.numeric(z1[,13])
      for(i in 1:nrow(z1)){ 		
        if(is.na(z1[i,13])) z1[i,14]<-'NA'
        else{
          if(z1[i,13]==50) z1[i,14]<-0
          else if(z1[i,13]<50) z1[i,14]<-round((z1[i,13]-50)/50*100,0) # 13 is current %srv
          #else if(z1[i,13]>50) z1[i,14]<-round((z1[i,13]-50)/50*100,0)
          }
        }
      z1[,14]<-as.numeric(z1[,14])
      format(z1,big.mark=',')
  
##### my modified index
  # column 5 is 5th percentile
  # column 7 is 50th
  # column 9 is 95th
  # column 12 is current
#  for(i in 1:nrow(z1)){     	
#      #if(is.na(z1[i,104])) z1[i,106]<-'NA'  # may not need this line
#      #else{
#          if(z1[i,12]<50) z1[i,14]<-round( 
#              (z1[i,12] - z1[i,7])/ # actual minus the median
#                  #(z1[i,52] - z1[i,2]) * 100, # median minus 0
#                  (z1[i,7] - z1[i,5]) * 100, # median minus 5 - we want 5th as the range right?
#              0)
#          else if(z1[i,12]>50)
#              z1[i,14]<-round( 
#                  (z1[i,12] - z1[i,7])/ # actual minus the median
#                      #(z1[i,102] - z1[i,52]) * 100, # 100th minus median 
#                      (z1[i,9] - z1[i,7]) * 100, # 95th minus median 
#                  0)
#          else z1[i,106]<-0
#      #}
#  }
  format(z1,big.mark=',')
  
  ##### end of inserted code
    
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
		  bitmap(file=paste(cov.levels[i],'.pdf',sep=''),
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
#		if(var=='srv.cv'){
#      title(main=paste('Cover-Condition Summary (',var,')',sep=''),line=2.5,
#				ylab='Coefficient of Variation',xlab='Scenario/Session',
#        cex.main=cex.main,...)
#		}
#		else{title(main=paste('Cover-Condition Summary (',var,')',sep=''),line=2.5,
#		      ylab='Percentage of Cover Type',xlab='Scenario/Session',
#		      cex.main=cex.main,...)
#		}
	
		#add subtitle: dist.type and cov.type
#		mtext(side=3,col=col.sub,cex=cex.sub,text=cov.levels[i],line=1,...)
    
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
  		  bitmap(file=paste(cov.levels[i],runs[j],'.pdf',sep=''),
  		         height=7,width=10,res=300,...) 
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
  				legend=cond.levels,fill=col.bars,cex=cex.legend,bg='white')
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
      # title(main='Cover-Condition Dynamics',line=2.5,
        title(ylab='Proportion of Cover Type',xlab=xlab,cex.main=cex.main,...)
  	
  		#add subtitle: dist.type and cov.type
 # 		mtext(side=3,col=col.sub,cex=cex.sub,
  		#text=paste(cov.levels[i],': Run #',runs[j],sep=''),line=1,...)
 #       text=cov.levels[i],line=1,...)

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


edgedepth <-
function(path='d:/landeco/exercises/rmlands/fragstats/', 
  w1='edgedepth.canopy.cover.csv', 
  w2='edgedepth.structure.csv', 
  w3='edgedepth.development.csv', 
  rules='edgedepth.rules.csv', 
  out='edgedepth.fsq'){

#edgedepth.R - Create edge depth matrix for Fragstats
#Usage: contrast(path, canopy.cover.csv, structure.csv, development.csv, rules.csv, results.csv)
#Hard wired to expect three weights files
#Input weight files have classes in sequential order
#Must have function available to run >source('d:/R/edgedepth.R')
#K. McGarigal
#April 28, 2006
#modified June 12, 2007
#modified Oct 2, 2014 to reflect update to fragstats4.2
  
#create weights matrices
read.edgedepth<-function(path,file){					#define read function
	z<-read.csv(paste(path,file,sep=''), header=TRUE)	#assign weights file
	z<-as.matrix(z[,-1])								#coerce to matrix and remove first column of row numbers
	if(all(is.na(z[lower.tri(z)]))) 					#determine if lower half of matrix is all N/A (missing data)
	{			
		 z[lower.tri(z)]<-t(z)[lower.tri(z)]		 	#replace N/A lower half of matrix with upper half values
	}
	z													#return results to z
}
w1<-read.edgedepth(path,w1)								#assign to w1 the first weights file
w2<-read.edgedepth(path,w2)								#assign to w2 the second weights file
w3<-read.edgedepth(path,w3)								#assign to w3 the second weights file

#create rules dataframe
rules<-read.csv(paste(path,rules,sep=''), row.names=1, header=TRUE)

#max edge depths for each pairwise combination of cover classes
temp1<-pmax(w1[rules[,1],rules[,1]],w2[rules[,2],rules[,2]],w3[rules[,3],rules[,3]])

#write results to csv file
outfile<-paste(path,out,sep='')
write.table('FSQ_TABLE',file=outfile,quote=FALSE,col.names=FALSE,row.names=FALSE)
a<-paste('CLASS_LIST_NUMERIC(',paste(rownames(rules),collapse=','),')\n',sep='')
cat(a,file=outfile,append=TRUE)
write.table(temp1,file=outfile,quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',',append=TRUE)

}
rotation <-
function(path,sessions=NULL,runs=NULL,pool.runs=TRUE,
  var='any.mort',start.step=1,stop.step=NULL,step.length,
  cell.size=30,outfile=FALSE){
  
  #set defaults
  options(warn=0)
  
  #set start.step default
  if(is.null(start.step)) start.step<-1
  
  #read covcond and darea data
  y<-read.csv(paste(path,'covcond.csv',sep=''),header=TRUE)
  x<-read.csv(paste(path,'darea.csv',sep=''),header=TRUE)
  
  # remove water and barren!
  y = y[y$cov.name!='Barren' & y$cov.name!='Water',]
  
  #create covcond list
  covcond.list<-as.data.frame(unique(subset(y,select=c('cov.name','cond.name'))))
  covcond.list<-covcond.list[order(covcond.list$cov.name,covcond.list$cond.name),]
  
  #set sessions parameter and verify valid session ids
  y.sessions<-unique(y$session.id)
  x.sessions<-unique(x$session.id)
  if(is.null(sessions)){
    if(any(!y.sessions %in% x.sessions)) stop('Inconsistent session ids in covcond.csv and darea.csv')
    if(any(!x.sessions %in% y.sessions)) stop('Inconsistent session ids in covcond.csv and darea.csv')
    sessions<-y.sessions
  }
  else{
    if(any(!sessions %in% y.sessions)) stop('Invalid session ids in covcond.csv')
    if(any(!sessions %in% x.sessions)) stop('Invalid session ids in darea.csv')
  }
  
  #subset based on session.id
  x<-x[x$session.id %in% sessions,]
  y<-y[y$session.id %in% sessions,]
  
  #calculate total area by cover type
  y1<-y[y$session.id==sessions[1] & y$run.id==1 & y$timestep.id==0,]
  y2<-aggregate(y1$cell.count,list(y1$cov.name),sum)
  colnames(y2)<-c('cov.name','cov.count')

  #multiple sessions
  if(length(sessions)>1){
    
    #check consistency in arguments
    if(pool.runs==FALSE) warning('Runs are always pooled with multiple sessions')
    
    #set global runs parameter and verify valid run ids
    y.runs<-unique(y$run.id)
    x.runs<-unique(x$run.id)
    if(is.null(runs)){
      if(any(!y.runs %in% x.runs)) stop('Inconsistent run ids in covcond.csv and darea.csv')
      if(any(!x.runs %in% y.runs)) stop('Inconsistent run ids in covcond.csv and darea.csv')
      runs<-y.runs
    }
    else{
      if(any(!runs %in% y.runs)) stop('Invalid run ids in covcond.csv')
      if(any(!runs %in% x.runs)) stop('Invalid run ids in darea.csv')
    }
    
    #subset based on run.id
    x<-x[x$run.id %in% runs,]
    y<-y[y$run.id %in% runs,]
    
    #set global start.step and stop.step parameters
    if(start.step>min(max(x$timestep.id),max(y$timestep.id)))
      stop('Start.step exceeds maximum timestep')
    if(is.null(stop.step)) stop.step<-min(max(x$timestep.id),max(y$timestep.id))
    else{
      if(stop.step>min(max(x$timestep.id),max(y$timestep.id))) 
        warning('Stop.step exceeds maximum timestep and was set to the maximum')
      stop.step<-min(stop.step,max(x$timestep.id),max(y$timestep))
    }
    
    #create list object
    dist.levels<-levels(x$dist.type)
    z1<-list(sessions,runs,start.step,stop.step,step.length)
    names(z1)<-c('sessions','maximum runs','start.step','stop.step','step.length')
    z2<-vector("list", length(dist.levels)) #create empty list
    names(z2)<-paste(dist.levels,'Rotation Periods (yrs)',sep=' ')
    z<-c(z1,z2)
    
    #loop thru disturbance types
    for(i in 1:length(dist.levels)){ 
      
      #loop thru selected sessions
      for(j in 1:length(sessions)){
        
        #subset based on dist.type and session.id
        t1<-x[x$dist.type==dist.levels[i] & x$session.id==sessions[j] &
          x$run.id %in% runs & x$timestep.id>=start.step & x$timestep.id<=stop.step,]
        if(nrow(t1)==0) stop('There are no observations meeting the specified criteria for one of the sessions')
        s1<-y[y$session.id==sessions[j] &
          y$run.id %in% runs & y$timestep.id>=start.step-1 & y$timestep.id<stop.step,]
        if(nrow(s1)==0) stop('There are no observations meeting the specified criteria for one of the sessions')
        
        #compute number of runs for session
        s.runs<-unique(t1$run.id)
        
        #set period parameter for session
        if(start.step==0) period<-(stop.step-start.step)*step.length*length(s.runs)
        else period<-(stop.step-start.step+1)*step.length*length(s.runs)

        #set number of timesteps parameter for session
        if(start.step==0) tsteps<-(stop.step-start.step)*length(s.runs)
        else tsteps<-(stop.step-start.step+1)*length(s.runs)

        #calculate total area by covcond type
        m1<-aggregate(s1$cell.count,list(s1$cov.name,s1$cond.name),sum)
        colnames(m1)<-c('cov.name','cond.name','covcond.count')
        m1<-m1[order(m1$cov.name,m1$cond.name),]
        m1<-merge(covcond.list,m1,by=c('cov.name','cond.name'),sort=TRUE,all.x=TRUE)
        
        #compute rotation period by cover type
        if(var=='low.mort') t2<-aggregate(t1$mort.low,list(t1$cov.name),sum)
        else if(var=='high.mort') t2<-aggregate(t1$mort.high,list(t1$cov.name),sum)
        else t2<-aggregate(t1$mort.any,list(t1$cov.name),sum)
        colnames(t2)<-c('cov.name',paste(var,'.',sessions[j],sep=''))
        t2<-merge(y2,t2,by='cov.name',sort=TRUE,all.x=TRUE)
        t2[is.na.data.frame(t2)]<-0
        t2[,1]<-as.character(t2[,1])
        sums<-as.vector(apply(t2[,2:3],2,sum))
        t2[nrow(t2)+1,2:3]<-sums
        t2[nrow(t2),1]<-'Total'
        t2[,3]<-round(period/(t2[,3]/t2$cov.count),0)
        t2$cov.count<-round(t2$cov.count*((cell.size^2)/10000),0)
        colnames(t2)<-c('cov.name','area.ha',paste(var,'.',sessions[j],sep=''))
        if(j==1) t3<-t2
        else t3<-merge(t3,t2,by=c('cov.name','area.ha'),sort=FALSE)
        
        #compute rotation period by covcond class
        if(var=='low.mort') t2<-aggregate(t1$mort.low,list(t1$cov.name,t1$cond.name),sum)
        else if(var=='high.mort') t2<-aggregate(t1$mort.high,list(t1$cov.name,t1$cond.name),sum)
        else t2<-aggregate(t1$mort.any,list(t1$cov.name,t1$cond.name),sum)
        colnames(t2)<-c('cov.name','cond.name',paste(var,'.',sessions[j],sep=''))
        t2<-merge(m1,t2,by=c('cov.name','cond.name'),sort=TRUE,all.x=TRUE)
        t2[,1]<-as.character(t2[,1])
        t2[,2]<-as.character(t2[,2]) 
        sums<-as.vector(apply(t2[,3:4],2,sum,na.rm=TRUE))
        t2[nrow(t2)+1,3:4]<-sums
        t2[nrow(t2),1:2]<-c('Total','Total')
        t2[,4]<-round(step.length/(t2[,4]/t2$covcond.count),0)
        t2r<-t2[,-3] #rotation periods for each session
        t2x<-t2[,-4] #covcond area for each session
        if(j==1){
          t3r<-t2r
          t3x<-t2x
        }
        else{
          t3r<-merge(t3r,t2r,by=c('cov.name','cond.name'),sort=FALSE,all=TRUE)
          t3x<-merge(t3x,t2x,by=c('cov.name','cond.name'),sort=FALSE,all=TRUE)
        }
      }
      
      #merge output across runs
      t3x[,-c(1:2)][is.na(t3x[,-c(1:2)])]<-0
      t3x$mean.covcond.count<-apply(t3x[,-c(1:2)],1,mean)
      t3x$area.ha<-round((t3x$mean.covcond.count/tsteps)*((cell.size^2)/10000),0)
      t3x<-subset(t3x,select=c('cov.name','cond.name','area.ha'))  
      out2<-merge(t3x,t3r,by=c('cov.name','cond.name'),sort=FALSE)
      
      z[[i+5]]<-list('By Cover Type'=t3,'By Cover-Condition Class'=out2)
        
    }
  }


  #single session
  else{
    
    #set runs parameter and verify valid run ids
    y.runs<-unique(y$run.id)
    x.runs<-unique(x$run.id)
    if(is.null(runs)){
      if(any(!y.runs %in% x.runs)) stop('Inconsistent run ids in covcond.csv and darea.csv')
      if(any(!x.runs %in% y.runs)) stop('Inconsistent run ids in covcond.csv and darea.csv')
      runs<-y.runs
    }
    else{
      if(any(!runs %in% y.runs)) stop('Invalid run ids in covcond.csv')
      if(any(!runs %in% x.runs)) stop('Invalid run ids in darea.csv')
    }

    #check for invalid parameterization
    if(length(runs)==1 & pool.runs==FALSE)
      stop('Must have multiple runs when pool.runs=FALSE')
    
    #subset based on run.id
    x<-x[x$run.id %in% runs,]
    y<-y[y$run.id %in% runs,]
    
    #set start.step and stop.step parameters
    if(start.step>max(x$timestep.id))
      stop('Start.step exceeds maximum timestep')
    if(is.null(stop.step)) stop.step<-max(x$timestep.id)
    else{
      if(stop.step>max(x$timestep.id)) 
        warning('Stop.step exceeds maximum timestep and was set to the maximum')
      stop.step<-min(stop.step,max(x$timestep.id))
    }
    
    #subset based on start.step and stop.step
    x<-x[x$timestep.id>=start.step & x$timestep.id<=stop.step,]
    y<-y[y$timestep.id>=start.step-1 & y$timestep.id<stop.step,]
    
    #create list object
    dist.levels<-levels(x$dist.type)
    z1<-list(sessions,runs,start.step,stop.step,step.length)
    names(z1)<-c('sessions','runs','start.step','stop.step','step.length')
    z2<-vector("list", length(dist.levels)) #create empty list
    names(z2)<-paste(dist.levels,'Rotation Periods (yrs)',sep=' ')
    z<-c(z1,z2)
    
    #loop thru disturbance types
    for(i in 1:length(dist.levels)){ 
      
      #subset based on dist.type
      t1<-x[x$dist.type==dist.levels[i],]

      #for pooled runs
      if(pool.runs==TRUE){

        #calculate total area by covcond type
        m1<-aggregate(y$cell.count,list(y$cov.name,y$cond.name),sum)
        colnames(m1)<-c('cov.name','cond.name','covcond.count')
        m1<-merge(covcond.list,m1,by=c('cov.name','cond.name'),sort=TRUE,all.x=TRUE)
        
        #set period parameter
        if(start.step==0) period<-(stop.step-start.step)*step.length*length(runs)
        else period<-(stop.step-start.step+1)*step.length*length(runs)
        
        #set number of timesteps parameter
        if(start.step==0) tsteps<-(stop.step-start.step)*length(runs)
        else tsteps<-(stop.step-start.step+1)*length(runs)

        #compute rotation period by cover type    
        t.low<-aggregate(t1$mort.low,list(t1$cov.name),sum)
        colnames(t.low)<-c('cov.name','low.count')
        t.high<-aggregate(t1$mort.high,list(t1$cov.name),sum)
        colnames(t.high)<-c('cov.name','high.count')
        t.any<-aggregate(t1$mort.any,list(t1$cov.name),sum)
        colnames(t.any)<-c('cov.name','any.count')
        t2<-merge(t.low,t.high,by='cov.name',sort=FALSE)
        t2<-merge(t2,t.any,by='cov.name',sort=FALSE)
        t2<-merge(y2,t2,by='cov.name',sort=TRUE,all.x=TRUE)
        t2[is.na.data.frame(t2)]<-0
        t2[,1]<-as.character(t2[,1])
        sums<-as.vector(apply(t2[,2:5],2,sum))
        t2[nrow(t2)+1,2:5]<-sums
        t2[nrow(t2),1]<-'Total'
        t2$low.count<-round(period/(t2$low.count/t2$cov.count),0)
        t2$high.count<-round(period/(t2$high.count/t2$cov.count),0)
        t2$any.count<-round(period/(t2$any.count/t2$cov.count),0)
        t2$cov.count<-round(t2$cov.count*((cell.size^2)/10000),0)
        colnames(t2)<-c('cov.name','area.ha','low.mort',
                        'high.mort','any.mort')
        out1<-t2        

        #compute rotation period by covcond class    
        t.low<-aggregate(t1$mort.low,list(t1$cov.name,t1$cond.name),sum)
        colnames(t.low)<-c('cov.name','cond.name','low.count')
        t.high<-aggregate(t1$mort.high,list(t1$cov.name,t1$cond.name),sum)
        colnames(t.high)<-c('cov.name','cond.name','high.count')
        t.any<-aggregate(t1$mort.any,list(t1$cov.name,t1$cond.name),sum)
        colnames(t.any)<-c('cov.name','cond.name','any.count')
        t2<-merge(t.low,t.high,by=c('cov.name','cond.name'),sort=FALSE)
        t2<-merge(t2,t.any,by=c('cov.name','cond.name'),sort=FALSE)
        t2<-merge(m1,t2,by=c('cov.name','cond.name'),sort=TRUE,all.x=TRUE)
        t2[,4:6][is.na(t2[4:6])]<-0
        t2[,1]<-as.character(t2[,1])
        t2[,2]<-as.character(t2[,2]) 
        sums<-as.vector(apply(t2[,3:6],2,sum,na.rm=TRUE))
        t2[nrow(t2)+1,3:6]<-sums
        t2[nrow(t2),1:2]<-c('Total','Total')
        t2$low.count<-round(step.length/(t2$low.count/t2$covcond.count),0)
        t2$high.count<-round(step.length/(t2$high.count/t2$covcond.count),0)
        t2$any.count<-round(step.length/(t2$any.count/t2$covcond.count),0)
        t2$covcond.count<-round((t2$covcond.count/tsteps)*((cell.size^2)/10000),0)
        colnames(t2)<-c('cov.name','cond.name','area.ha','low.mort',
                        'high.mort','any.mort')
        out2<-t2

        z[[i+5]]<-list('By Cover Type'=out1,'By Cover-Condition Class'=out2)

      }
      
      #for separate runs
      else{
  
        #set period parameter
        if(start.step==0) period<-(stop.step-start.step)*step.length
        else period<-(stop.step-start.step+1)*step.length

        #set number of timesteps parameter
        if(start.step==0) tsteps<-(stop.step-start.step)
        else tsteps<-(stop.step-start.step+1)
        
        #loop thru selected runs
        for(j in 1:length(runs)){
          
          #subset based on dist.type
          t1r<-t1[t1$run.id==runs[j],]
          y1r<-y[y$run.id==runs[j],]

          #calculate total area by covcond type
          m1r<-aggregate(y1r$cell.count,list(y1r$cov.name,y1r$cond.name),sum)
          colnames(m1r)<-c('cov.name','cond.name','covcond.count')
          m1r<-merge(covcond.list,m1r,by=c('cov.name','cond.name'),sort=TRUE,all.x=TRUE)
          
          #compute rotation periods by cover type  
          if(var=='low.mort') t2<-aggregate(t1r$mort.low,list(t1r$cov.name),sum)
          else if(var=='high.mort') t2<-aggregate(t1r$mort.high,list(t1r$cov.name),sum)
          else t2<-aggregate(t1r$mort.any,list(t1r$cov.name),sum)
          colnames(t2)<-c('cov.name',paste(var,'.',runs[j],sep=''))
          t2<-merge(y2,t2,by='cov.name',sort=TRUE,all.x=TRUE)
          t2[,1]<-as.character(t2[,1])
          sums<-as.vector(apply(t2[,2:3],2,sum,na.rm=TRUE))
          t2[nrow(t2)+1,2:3]<-sums
          t2[nrow(t2),1]<-'Total'
          t2[,3]<-round(period/(t2[,3]/t2$cov.count),0)
          t2$cov.count<-round(t2$cov.count*((cell.size^2)/10000),0)
          colnames(t2)<-c('cov.name','area.ha',paste(var,'.',runs[j],sep=''))
          if(j==1) t3<-t2
          else t3<-merge(t3,t2,by=c('cov.name','area.ha'),sort=FALSE,all.x=TRUE)
          
          #compute rotation period by covcond type    
          if(var=='low.mort') t2<-aggregate(t1r$mort.low,list(t1r$cov.name,t1r$cond.name),sum)
          else if(var=='high.mort') t2<-aggregate(t1r$mort.high,list(t1r$cov.name,t1r$cond.name),sum)
          else t2<-aggregate(t1r$mort.any,list(t1r$cov.name,t1r$cond.name),sum)
          colnames(t2)<-c('cov.name','cond.name',paste(var,'.',runs[j],sep=''))
          t2<-merge(m1r,t2,by=c('cov.name','cond.name'),sort=TRUE,all.x=TRUE)
          t2[,1]<-as.character(t2[,1])
          t2[,2]<-as.character(t2[,2])
          sums<-as.vector(apply(t2[,3:4],2,sum,na.rm=TRUE))
          t2[nrow(t2)+1,3:4]<-sums
          t2[nrow(t2),1:2]<-c('Total','Total')
          t2[,4]<-round(step.length/(t2[,4]/t2$covcond.count),0)
          t2r<-t2[,-3] #rotation period for run
          t2x<-t2[,-4] #covcond area for run
          if(j==1){
            t3r<-t2r
            t3x<-t2x
            }
          else{
            t3r<-merge(t3r,t2r,by=c('cov.name','cond.name'),sort=FALSE,all=TRUE)
            t3x<-merge(t3x,t2x,by=c('cov.name','cond.name'),sort=FALSE,all=TRUE)
            }
          }

          #merge covcond area output across runs
          t3x[,-c(1:2)][is.na(t3x[,-c(1:2)])]<-0
          t3x$mean.covcond.count<-apply(t3x[,-c(1:2)],1,mean)
          t3x$area.ha<-round((t3x$mean.covcond.count/tsteps)*((cell.size^2)/10000),0)
          t3x<-subset(t3x,select=c('cov.name','cond.name','area.ha'))  
          out2<-merge(t3x,t3r,by=c('cov.name','cond.name'),sort=FALSE)
        
          z[[i+5]]<-list('By Cover Type'=t3,'By Cover-Condition Class'=out2)
          
        }
      }
    }

    #output tables to file
    if(outfile==TRUE){
      for(i in 6:length(z)){
        write.table(z[[i]][[1]],file=paste(path,dist.levels[i-5],'_rotation.cov.csv',sep=''),
                    quote=FALSE,row.names=FALSE,sep=',',append=TRUE)
        write.table(z[[i]][[2]],file=paste(path,dist.levels[i-5],'_rotation.covcond.csv',sep=''),
                    quote=FALSE,row.names=FALSE,sep=',',append=TRUE)
      }
    }
  
  return(z)
}
preturn <-
function(path,session,runs=NULL,pool.runs=TRUE,
  stop.step=NULL,step.length,cell.size=30,cover.min.ha=0,
  cover.names=NULL,y.scale='percent',
  col.bars=c('yellow','green','blue'),col.sub='brown',
  cex.main=1.5,cex.sub=1.5,cex.legend=1.5,legendlocale='topleft',outfile=FALSE,...){

##things to do:
#need to figure out a way to exclude the equilibration period

require(Hmisc)

#set defaults
options(warn=0)
old.par<-par(no.readonly=TRUE)

#read preturn data
x<-read.csv(paste(path,'preturn.csv',sep=''),header=TRUE)

#set sessions parameter
if(is.null(session)) session<-unique(x$session.id)
if(length(session)>1) stop('Enter single session id')

#verify valid session ids
all.sessions<-unique(x$session.id)
if(any(!session %in% all.sessions)) stop('Invalid session id')

#subset data based on session.id and reorder columns
x<-x[x$session.id==session,c(1:7,9,8,10)]

#set runs parameter
if(is.null(runs)) runs<-unique(x$run.id)

#verify valid run ids
all.runs<-unique(x$run.id)
if(any(!runs %in% all.runs)) stop('Invalid run ids')

#subset data based on run ids
x<-x[x$run.id %in% runs,]

#set stop.step
if(is.null(stop.step)) stop.step<-max(x$timestep.id)
else{
  if(stop.step>max(x$timestep.id)) 
    warning('Stop.step exceeds maximum timestep and was set to the maximum')
  stop.step<-min(stop.step,max(x$timestep.id))
  }

#subset data based on stop.step
x<-x[x$timestep.id==stop.step,]

#select subset of cover types
if(!is.null(cover.names)){
  cov.levels<-levels(x$cov.name)
  if(any(!cover.names %in% cov.levels)) stop('Invalid cover names')
  x<-x[x$cov.name %in% cover.names,] 
  }
    
#select cover types with min area
if(cover.min.ha>0){
  x2<-read.csv(paste(path,'covcond.csv',sep=''),header=TRUE)
  t1<-x2[x2$run.id==runs[1] & x2$timestep.id==0,]
  t2<-aggregate(t1$cell.count,list(t1$cov.name),sum)
  colnames(t2)<-c('cover.type','cov.count')
  t2$area.ha<-t2$cov.count*cell.size^2/10000
  t2<-t2[t2$area.ha>cover.min.ha,]
  t2.cover.type<-t2$cover.type
  x<-x[x$cov.name %in% t2.cover.type,]
  }

#create list object
dist.levels<-levels(x$dist.type)
z1<-list(session,runs,stop.step)
names(z1)<-c('session','runs','stop.step')
z2<-vector("list", length(dist.levels))
names(z2)<-paste(dist.levels,'Return Interval Distribution',sep=' ')
z<-c(z1,z2)

#set period parameter
period<-stop.step*step.length

#plot return interval distribution by dist.type, cov.name and run
for(i in 1:length(dist.levels)){ #loop thru dist.types

  #subset data based on dist.type
  y1<-x[x$dist.type==dist.levels[i],]
  
  #rescale freq and cell counts, and compute mean return interval
  y1$mort.low<-y1$mort.low*((cell.size^2)/10000)
  y1$mort.high<-y1$mort.high*((cell.size^2)/10000)
  y1$mort.any<-y1$mort.any*((cell.size^2)/10000)
  y1<-y1[order(y1$freq,decreasing=TRUE),]
  y1$mri<-round(period/(y1$freq),0)
  y1$mri[y1$mri==Inf]<-period+step.length
  y1<-y1[,c(2,4,6,11,8:10)]
  y1<-aggregate(y1[,5:7],list(y1$run.id,y1$dist.type,y1$cov.name,y1$mri),sum)
  names(y1)<-c('run.id','dist.type','cov.name','mri','mort.low','mort.high','mort.any')
  
  #create 'total eligible' records
  y1.temp<-aggregate(y1[,5:7],list(y1$run.id,y1$dist.type,y1$mri),sum)
  colnames(y1.temp)<-c('run.id','dist.type','mri','mort.low','mort.high','mort.any')
  y1.temp$cov.name<-'Total Eligible'
  y1.temp<-y1.temp[,c(1,2,7,3:6)]
  y1<-rbind(y1,y1.temp)
  y1$mri<-as.numeric(y1$mri)
 
 	#create list object
  cov.levels<-sort(as.vector(unique(y1$cov.name)))
  z.cov<-vector("list",length(cov.levels))
	names(z.cov)<-cov.levels

  #loop thru cov.types
	for(j in 1:length(cov.levels)){ 

	  #subset data based on cov.name
	  y<-y1[y1$cov.name==cov.levels[j],c(1,4:7)]
	  
    #for pooled runs
    if(pool.runs==TRUE){
 
      #expand dataset
      mri.levels<-unique(y$mri)
      temp<-merge(as.data.frame(runs),mri.levels)
      names(temp)<-c('run.id','mri')
      y<-merge(temp,y,all=TRUE)
      y[is.na(y)]<-0
      
      #summarize data
			y.low<-aggregate(y$mort.low,list(y$mri),mean)
			  colnames(y.low)<-c('mri','mort.low')
			y.high<-aggregate(y$mort.high,list(y$mri),mean)
		    colnames(y.high)<-c('mri','mort.high')
			y.any<-aggregate(y$mort.any,list(y$mri),mean)
		    colnames(y.any)<-c('mri','mort.any')
			y<-merge(y.low,y.high,by='mri',sort=FALSE)
			y<-merge(y,y.any,by='mri',sort=FALSE)
			#y$mri<-as.character(y$mri)
			
			#only plot if >1 interval
	    if(!nrow(y)<=1){ 

			#optionally convert to percentage
			if(y.scale=='percent'){ 
				y$mort.low<-round((y$mort.low/sum(y$mort.low))*100,2)
				y$mort.high<-round((y$mort.high/sum(y$mort.high))*100,2)
				y$mort.any<-round((y$mort.any/sum(y$mort.any))*100,2)
				}

			#compute median return intervals
      q.l<-round(wtd.quantile(y$mri,weights=y$mort.low,probs=.5),0)
      #q.l[q.l==period]<-paste('>=',period,sep='')
      q.h<-round(wtd.quantile(y$mri,weights=y$mort.high,probs=.5),0)
      #q.h[q.h==period]<-paste('>=',period,sep='')
      q.a<-round(wtd.quantile(y$mri,weights=y$mort.any,probs=.5),0)
      #q.a[q.a==period]<-paste('>=',period,sep='')

			#compute mean return intervals
			#q.l<-round(wtd.mean(y$mri,weights=y$mort.low),0)
			#q.l[q.l>=period]<-paste('>=',period,sep='')
			#q.h<-round(wtd.mean(y$mri,weights=y$mort.high),0)
			#q.h[q.h>=period]<-paste('>=',period,sep='')
			#q.a<-round(wtd.mean(y$mri,weights=y$mort.any),0)
			#q.a[q.a>=period]<-paste('>=',period,sep='')

      #define unknown return interval
			y$mri[y$mri>period]<-paste('>=',period,sep='')

			#print(dist.levels[i])
			#print(cov.levels[j])
			#print(format(y,big.mark=','))
			z.cov[[j]]<-y
			
			#plot barplot
			barplot(t(y[,2:4]),beside=TRUE,
				xaxs='i',yaxs='i',col=col.bars,
				ylim=c(0,max(y$mort.high,y$mort.low,y$mort.any)),
				axis.lty=1,names=y$mri)

			#add plot title				
			if(y.scale=='percent')
				title(main='Population Return Interval Distribution',
					ylab='Percent of Eligible',xlab='Return Interval (yrs)',
          cex.main=cex.main,...)
			else if(y.scale=='ha')
				title(main='Population Return Interval Distribution',
					ylab='Area (ha)',xlab='Return Interval (yrs)',
				  cex.main=cex.main,...)

			#add subtitle: dist.type and cov.type
			mtext(side=3,col=col.sub,cex=cex.sub,
				text=paste(dist.levels[i],': ',cov.levels[j],sep=' '),...)

			#add legend				
			legend(x=legendlocale,inset=c(0.05,0.05),bty='n',
				legend=c(paste('Low mort (median RI =',q.l,'yrs)',sep=' '),
				paste('High mort (median RI =',q.h,'yrs)',sep=' '),
				paste('Any mort (median RI =',q.a,'yrs)',sep=' ')),
				fill=col.bars,cex=cex.legend)

			if(!i==length(dist.levels) || !j==length(cov.levels)) 	
				readline("Press return for next plot ")			

			} #end if >1 interval
  
    } #end if pool.runs=TRUE
 
		#for individual runs
		else{

      #create list objects
		  z.run<-vector("list",length(runs))
		  names(z.run)<-runs

      #loop thru runs
      for(k in 1:length(runs)){
        
  		  #subset data based on run.id
  		  q<-y[y$run.id==runs[k],]

  		  #expand dataset
  		  mri.levels<-unique(y$mri)
  		  temp<-merge(as.data.frame(runs),mri.levels)
  		  names(temp)<-c('run.id','mri')
  		  q<-merge(temp,q,all=TRUE)
  		  q[is.na(q)]<-0
  		  
  		  #summarize data
  		  q.low<-aggregate(q$mort.low,list(q$mri),mean)
  		  colnames(q.low)<-c('mri','mort.low')
  		  q.high<-aggregate(q$mort.high,list(q$mri),mean)
  		  colnames(q.high)<-c('mri','mort.high')
  		  q.any<-aggregate(q$mort.any,list(q$mri),mean)
  		  colnames(q.any)<-c('mri','mort.any')
  		  q<-merge(q.low,q.high,by='mri',sort=FALSE)
  		  q<-merge(q,q.any,by='mri',sort=FALSE)
  		  #q$mri<-as.character(q$mri)
  		  
  		  #only plot if >1 interval
  		  if(!nrow(q)<=1){ 
  		    
  		    #optionally convert to percentage
  		    if(y.scale=='percent'){ 
  		      q$mort.low<-round((q$mort.low/sum(q$mort.low))*100,4)
  		      q$mort.high<-round((q$mort.high/sum(q$mort.high))*100,4)
  		      q$mort.any<-round((q$mort.any/sum(q$mort.any))*100,4)
  		      }
  		    
  		    #compute median return intervals
  		    q.l<-round(wtd.quantile(q$mri,weights=q$mort.low,probs=.5),0)
  		    #q.l[q.l==period]<-paste('>=',period,sep='')
  		    q.h<-round(wtd.quantile(q$mri,weights=q$mort.high,probs=.5),0)
  		    #q.h[q.h==period]<-paste('>=',period,sep='')
  		    q.a<-round(wtd.quantile(q$mri,weights=q$mort.any,probs=.5),0)
  		    #q.a[q.a==period]<-paste('>=',period,sep='')

  		    #compute mean return intervals
  		    #q.l<-round(wtd.mean(q$mri,weights=q$mort.low),0)
  		    #q.l[q.l==period]<-paste('>=',period,sep='')
  		    #q.h<-round(wtd.mean(q$mri,weights=q$mort.high),0)
  		    #q.h[q.h==period]<-paste('>=',period,sep='')
  		    #q.a<-round(wtd.mean(q$mri,weights=q$mort.any),0)
  		    #q.a[q.a==period]<-paste('>=',period,sep='')
          
  		    #define unknown return interval
  		    q$mri[q$mri>period]<-paste('>=',period,sep='')
  		    
  		    #print(dist.levels[i])
  		    #print(cov.levels[j])
  		    #print(format(q,big.mark=','))
  		    z.run[[k]]<-q
  		    
  		    #plot barplot
  		    barplot(t(q[,2:4]),beside=TRUE,
            xaxs='i',yaxs='i',col=col.bars,
            ylim=c(0,max(q$mort.high,q$mort.low,q$mort.any)),
            axis.lty=1,names=q$mri)
  		    
  		    #add plot title				
  		    if(y.scale=='percent')
  		      title(main='Population Return Interval Distribution',
              ylab='Percent of Eligible',xlab='Return Interval (yrs)',
              cex.main=cex.main,...)
  		    else if(y.scale=='ha')
  		      title(main='Population Return Interval Distribution',
              ylab='Area (ha)',xlab='Return Interval (yrs)',
              cex.main=cex.main,...)
  		    
  		    #add subtitle: dist.type and cov.type
  		    mtext(side=3,col=col.sub,cex=cex.sub,
            text=paste(dist.levels[i],': ',cov.levels[j],': Run #',runs[k],sep=' '),...)
  		    
  		    #add legend				
  		    legend(x=legendlocale,inset=c(0.05,0.05),bty='n',
             legend=c(paste('Low mort (median RI =',q.l,'yrs)',sep=' '),
                paste('High mort (median RI =',q.h,'yrs)',sep=' '),
                paste('Any mort (median RI =',q.a,'yrs)',sep=' ')),
                fill=col.bars,cex=cex.legend)
  		    
  		    if(!i==length(dist.levels) || !j==length(cov.levels) || !k==length(runs)) 	
  		      readline("Press return for next plot ")			
  		    
  		    } #end if >1 interval
		  
        } #end loop thru runs

      z.cov[[j]]<-z.run
		  
	  	} #end if pool.runs=FALSE
      
	  } #end cov.type loop
  
  z[[i+3]]<-z.cov
  
  } #end dist.type loop
	
#output tables to file
if(outfile==TRUE){
	for(i in 4:length(z)){
		write('',file=paste(path,dist.levels[i-3],' preturn.csv',sep='')) #empty file if it exists
		for(j in 1:length(cov.levels)){
			write.table(z[[i]][j],file=paste(path,dist.levels[i-3],' preturn.csv',sep=''),
			quote=FALSE,row.names=FALSE,sep=',',append=TRUE)
			}
		}
	}
	
par(old.par)
return(z)
}
departure <-
function(ref,alt){

	#calculate landscape metric SRV quantiles
	q1<-quantile(ref,probs=seq(0,1,0.01),na.rm=TRUE)

	#get median alternative scenario value
	t1<-median(alt)

	#calculate alternative scenario %SRV
	if(q1[101]-q1[1]==0) z1<-'NA'
	else{
		z1<-max(0,which(q1<t1))
		if(z1==101) z1<-100
		}

	#compute departure index
	if(is.na(z1)) z<-'NA'
	else{
		if(z1<25) z<-(25-z1)/25*100
		else if(z1>75) z<-(z1-75)/25*100
		else z<-0
		}
		
return(z)
}
dsize <-
function(path,session=NULL,runs=NULL,pool.runs=TRUE,
  start.step=1,stop.step=NULL,cell.size=30,log.size=TRUE,
  breaks=c(0,1,10,100,1000,10000,100000),target=NULL,
  col.bars='blue',col.sub='brown',cex.main=1.5,cex.sub=1.5,
  cex.legend=1.5,...){

#set defaults
options(warn=0,scipen=999)
old.par<-par(no.readonly=TRUE)
	
#read dsize data
x<-read.csv(paste(path,'dsize.csv',sep=''),header=TRUE)

#read target dsize data
if(!is.null(target)){
  x2<-read.csv(paste(path,target,sep=''),header=FALSE)
  x2<-as.vector(x2[,1])
  x2<-na.omit(x2)
  }

#set session parameter
if(is.null(session)) session<-unique(x$session.id)
if(length(session)>1) stop('Enter single session id')

#verify valid session ids
all.sessions<-unique(x$session.id)
if(any(!session %in% all.sessions)) stop('Invalid session id')

#set runs parameter
if(is.null(runs)) runs<-unique(x$run.id)

#verify valid run ids
all.runs<-unique(x$run.id)
if(any(!runs %in% all.runs)) stop('Invalid run ids')

#set start.step and stop.step parameters
if(is.null(stop.step)) stop.step<-max(x$timestep.id)
else{
  if(stop.step>max(x$timestep.id)) 
  warning('Stop.step exceeds maximum timestep and was set to the maximum')
	stop.step<-min(stop.step,max(x$timestep.id))
	}

#subset data based on session.id, runs, start.step and stop.step
x<-x[x$session.id==session & x$run.id %in% runs & 
	x$timestep.id>=start.step & x$timestep.id<=stop.step,]

#rescale dist size
if(log.size==TRUE) x$size.obs<-log((x$size.obs*((cell.size^2)/10000))+1)
else x$size.obs<-x$size.obs*((cell.size^2)/10000)

#set breaks for barplot
if(max(breaks)<max(x$size.obs)) breaks<-ceiling(c(breaks,max(x$size.obs)))

#create looping vector for dist.type
dist.levels<-levels(x$dist.type)

#create empty list
z<-vector("list", length(dist.levels)) 
names(z)<-paste(dist.levels,' disturbance size',sep='')

#loop thru dist.types
for(i in 1:length(dist.levels)){ 
	y<-x[x$dist.type==dist.levels[i],]

	#create list objects for results
	z[[i]]<-vector("list",length(runs))
	names(z[[i]])<-paste('run number ',runs,sep='')
	
  #for pooled runs
  if(pool.runs==TRUE){

    #create data for barplot
    bins<-as.data.frame(as.factor(seq(1,length(breaks)-1)))
    names(bins)<-'bin.id'
    bin.id<-cut(y$size.obs,breaks=breaks,labels=FALSE) 
    temp<-as.data.frame(table(bin.id))
    temp<-merge(temp,bins,all=TRUE)
    temp$Freq[is.na(temp$Freq)]<-0
    temp$bin.id<-breaks[-1]
    temp$Proportion<-temp$Freq/sum(temp$Freq)
    names(temp)<-c('bin','obs.freq','obs.proportion')
    
    if(is.null(target)) z[[i]]<-temp
    
    #summarize target data 
    else{
      bin.id<-cut(x2,breaks=breaks,labels=FALSE) 
      temp2<-as.data.frame(table(bin.id))
      temp2<-merge(temp2,bins,all=TRUE)
      temp2$Freq[is.na(temp2$Freq)]<-0
      temp2$bin.id<-breaks[-1]
      temp2$proportion<-temp2$Freq/sum(temp2$Freq)    
      names(temp2)<-c('bin','target.freq','target.proportion')
      temp<-as.matrix(merge(temp,temp2))
      z[[i]]<-temp
      }
    
    #plot barplot
    if(is.null(target)){
      barplot(temp$obs.proportion,names.arg=temp$bin,col=col.bars,...)
      }
    else{
      barplot(t(temp[,c(3,5)]),beside=TRUE,names.arg=temp[,1],
        col=col.bars,...)
      }
    
    if(log.size==TRUE){
      title(main='Disturbance Size Distribution',cex.main=cex.main,
        ylab='Proportion',xlab='Log(Disturbance Size (ha) + 1)',...)
      }
    else{
      title(main='Disturbance Size Distribution',cex.main=cex.main,
        ylab='Proportion',xlab='Disturbance Size (ha)',...)
      }
    #####################################################
    # comment out below items to remove "Wildfire Run #" from plot
    #mtext(side=3,col=col.sub,cex=cex.sub,
    #  text=dist.levels[i],...)
    
    if(!is.null(target)){
      legend('topright',inset=c(0.1,0.1),legend=c('Observed','Target'),
        fill=col.bars,cex=cex.legend,bty='n')
      }
    
    if(!i==length(dist.levels)) 
      readline("Press return for next plot ")
  }

  #for separate runs
  else{
 
    #create target summary table
    if(!is.null(target)){
      bins<-as.data.frame(as.factor(seq(1,length(breaks)-1)))
      names(bins)<-'bin.id'
      bin.id<-cut(x2,breaks=breaks,labels=FALSE) 
      target.temp<-as.data.frame(table(bin.id))
      target.temp<-merge(target.temp,bins,all=TRUE)
      target.temp$Freq[is.na(target.temp$Freq)]<-0
      target.temp$bin.id<-breaks[-1]
      target.temp$proportion<-target.temp$Freq/sum(target.temp$Freq)    
      names(target.temp)<-c('bin','target.freq','target.proportion')
      }
    
    #loop thru runs
    for(j in 1:length(runs)){
      
      #select data for run
      q<-x[x$run.id==runs[j],]
      
      #create data for barplot
      bin.id<-cut(q$size.obs,breaks=breaks,labels=FALSE) 
      temp<-as.data.frame(table(bin.id))
      temp<-merge(temp,bins,all=TRUE)
      temp$Freq[is.na(temp$Freq)]<-0
      temp$bin.id<-breaks[-1]
      temp$Proportion<-temp$Freq/sum(temp$Freq)
      names(temp)<-c('bin','obs.freq','obs.proportion')

      if(is.null(target)) z[[i]][[j]]<-temp

      else{
        temp<-as.matrix(merge(temp,target.temp))
        z[[i]][[j]]<-temp
        }
      
      #plot barplot
      if(is.null(target)){
        barplot(temp$obs.proportion,names.arg=temp$bin,col=col.bars,...)
      }
      else{
        barplot(t(temp[,c(3,5)]),beside=TRUE,names.arg=temp[,1],
          col=col.bars,...)
      }
      
    	if(log.size==TRUE){
	      title(main='Disturbance Size Distribution',cex.main=cex.main,
		      ylab='Proportion',xlab='Log(Disturbance Size (ha) + 1)',...)
        }
  	  else{
	      title(main='Disturbance Size Distribution',cex.main=cex.main,
		      ylab='Proportion',xlab='Disturbance Size (ha)',...)
        }

      #####################################################
      # comment out below items to remove "Wildfire Run #" from plot
      #mtext(side=3,col=col.sub,cex=cex.sub,
		#    text=paste(dist.levels[i],' Run #',runs[j],sep=''),...)

      if(!is.null(target)){
        legend('topright',inset=c(0.1,0.1),legend=c('Observed','Target'),
          fill=col.bars,cex=cex.legend,bty='n')
        }
      
    	if(!i==length(dist.levels) || !j==length(runs)) 
	  	readline("Press return for next plot ")
      }
    }
  }
  
par(old.par)
return(z)
}
darea <-
function(path,sessions=NULL,var='mean',runs=NULL,start.step=1, legendloc=NULL,
	stop.step=NULL,step.length=NULL, covtype=NULL, cell.size=30,y.scale='percent',
	col.bars=c('dark green','dark blue','brown'),col.sub='brown', save.figs=FALSE,
  cex.main=1.5,cex.sub=1.5,cex.legend=1.5,outfile=FALSE,...){

##things to do:
#add plot by cover type? see preturn for possible approach
    
# add argument that specifies whether to do it by cover type
# can be true/false or provide vector of cover types

#set defaults
options(warn=0)
old.par<-par(no.readonly=TRUE)
	
#read darea data
x<-read.csv(paste(path,'darea.csv',sep=''),header=TRUE)


if (!is.null(covtype)){
    x<-x[x$cov.name==covtype,]
}

#rescale cell counts
# DON'T BE A DUMBASS!!!
#x$mort.high<-round(x$mort.high*((cell.size^2)/10000),0)
#x$mort.low<-round(x$mort.low*((cell.size^2)/10000),0)
#x$mort.any<-round(x$mort.any*((cell.size^2)/10000),0)
x$mort.high<-x$mort.high*((cell.size^2)/10000)
x$mort.low<-x$mort.low*((cell.size^2)/10000)
x$mort.any<-x$mort.any*((cell.size^2)/10000)

#set global session and runs parameters
if(is.null(sessions)) sessions<-unique(x$session.id)

#verify valid session ids
all.sessions<-unique(x$session.id)
if(any(!sessions %in% all.sessions)) stop('Invalid session ids')

#clustered bar charts for multiple sessions
if(length(sessions)>1){

  #subset data based on session.id
  x<-x[x$session.id %in% sessions,]

  #verify valid run.ids
  if(is.null(runs)) runs<-unique(x$run.id)
  else{
    all.runs<-unique(x$run.id)
    if(!runs %in% all.runs) stop('Invalid run ids')
  }
  
  #subset data based on runs
  x<-x[x$run.id %in% runs,]
  
  #create dist.levels vector for loop
  dist.levels<-as.vector(unique(x$dist.type))
  
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
			legend(x=legendloc,#inset=c(0.05,0.05),
				legend=c('Low mort','High mort','Any mort'),
        fill=col.bars,cex=cex.legend)
		
			#if(!i==length(dist.levels) || !j==length(runs)) 
			#	readline("Press return for next plot ")

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
			                
				xaxs='i',yaxs='i',col=col.bars,
				axis.lty=1,names=y$timestep,...)
            
            # to add a line at the equilibration period
            #abline(v=40,lty=2,lwd=2,col="black")

            
            #add plot title	- dist.type	
			if(is.null(step.length)) xlab='Timestep'
#			else xlab=paste('Timestep (x',step.length,' yrs)')
			if(y.scale=='percent')
				title(main=paste(dist.levels[i],'Disturbance Trajectory',sep=' '),
					ylab='Percent of Eligible',xlab=xlab,cex.main=cex.main,...)
			else
				title(main=paste(dist.levels[i],'Disturbance Trajectory',sep=' '),
					ylab='Area (ha)',xlab=xlab,cex.main=cex.main,...)
	
			#add subtitle - run number
			#mtext(side=3,col=col.sub,cex=cex.sub,
            #text=paste('Run #',runs[j],sep=''),...)


	
			#add legend		
            #####################################################
            ### I CHANGED A LINE HERE
            #######################################################
			legend(legendloc, #inset=c(0.05,0.05),
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


dinit <-
function(path,sessions=NULL,var='mean',runs=NULL,start.step=1,
	stop.step=NULL,step.length=NULL,col.line='blue',
  col.sub='brown',cex.main=1.5,cex.sub=1.5,cex.legend=1.5,outfile=FALSE,...){

##things to do:
#add plot by cover type? see preturn for possible approach

#set defaults
options(warn=0)
old.par<-par(no.readonly=TRUE)
	
#read darea data
x<-read.csv(paste(path,'dinit.csv',sep=''),header=TRUE)

#set global session and runs parameters
if(is.null(sessions)) sessions<-sort(unique(x$session.id))

#verify valid session ids
all.sessions<-unique(x$session.id)
if(any(!sessions %in% all.sessions)) stop('Invalid session ids')

#clustered bar charts for multiple sessions
if(length(sessions)>1){
  
  #subset data based on session.id
  x<-x[x$session.id %in% sessions,]
  
  #verify valid run.ids
  if(is.null(runs)) runs<-unique(x$run.id)
  else{
    all.runs<-unique(x$run.id)
    if(!runs %in% all.runs) stop('Invalid run ids')
  }
  
  #subset data based on runs
  x<-x[x$run.id %in% runs,]
  
  #create dist.levels vector for loop
  dist.levels<-as.vector(unique(x$dist.type))
  
  #create list objects for results
  z<-vector("list",length(dist.levels))
  names(z)<-paste(dist.levels,' dinit comparison (',var,')',sep='')
  
  #loop thru disturbance types
  for(i in 1:length(dist.levels)){ 
    
    #create list objects for results
    out<-matrix(NA,nrow=length(runs),ncol=length(sessions))
    colnames(out)<-paste('session',sessions,'')
    run<-runs
    out<-cbind(run,out)
    
    #loop thru runs
    for(j in 1:length(runs)){
      
      #subset data for dist.type and run.id
      x2<-x[x$dist.type==dist.levels[i] & x$run.id==runs[j],]
      
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

      #sort sesssion.id
      sessions<-sort(unique(x2$session.id))
      
      #loop thru sessions
      for(k in 1:length(sessions)){
        
        #select records for session
        y<-x2[x2$session.id==sessions[k],]

        #summarize counts by timestep
        y.sum<-aggregate(y$count,list(y$timestep.id),sum)
        colnames(y.sum)<-c('timestep','count')
        y<-merge(timestep,y.sum,by='timestep',all.x=TRUE,sort=FALSE)
        y[is.na.data.frame(y)]<-0
        y<-y[order(y$timestep),]
        
        #compute darea summary
        if(var=='min') out[j,k+1]<-min(y$count)
        else if(var=='max') out[j,k+1]<-max(y$count)
        else if(var=='median') out[j,k+1]<-round(median(y$count),0)
        else if(var=='mean') out[j,k+1]<-round(mean(y$count),0)
        
      }
      
      #create clustered bar chart
      barplot(out[j,-1],border='black',names.arg=sessions,
              xaxs='i',yaxs='i',col=seq(1,length(sessions)),
              axis.lty=1)
      
      #add plot title	- dist.type			
      title(main=paste(dist.levels[i],' (',var,' dinit/timestep',')',sep=''),
        ylab='Count',xlab='Scenario/Session',cex.main=cex.main,...)
      
      #add subtitle - run number
      mtext(side=3,col=col.sub,cex=cex.sub,
        text=paste('Run #',runs[j],sep=''),...)
      
      if(!i==length(dist.levels) || !j==length(runs)) 
        readline("Press return for next plot ")
      
    }
  }

  #print summary to console and list object
  z[i]<-as.data.frame(out)
  print(out)
  
  #output tables to file
  if(outfile==TRUE){
    for(i in 1:length(z)){
      write.table(z[i],file=paste(path,dist.levels[i],'_darea_comparison.csv',sep=''),
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
  names(z1)<-paste(dist.levels,' disturbance trajectory',sep='')
  z2<-vector("list",length(dist.levels))
  names(z2)<-paste(dist.levels,' disturbance summary',sep='')
  
  #loop thru disturbance types
  for(i in 1:length(dist.levels)){
    
    #create list objects for results
    z1[[i]]<-vector("list",length(runs))
    names(z1[[i]])<-paste('run number ',runs,sep='')
    z2[[i]]<-vector("list",length(runs))
    names(z2[[i]])<-paste('run number ',runs,sep='')
    
    #loop thru runs 
    for(j in 1:length(runs)){
      
      #summarize counts by cov.name
      y<-x[x$dist.type==dist.levels[i] & x$run.id==runs[j],]
      y.sum<-aggregate(y$count,list(y$timestep.id),sum)
      colnames(y.sum)<-c('timestep','count')
      y<-merge(timestep,y.sum,by='timestep',all.x=TRUE,sort=FALSE)
      y[is.na.data.frame(y)]<-0
      y<-y[order(y$timestep),]
      
      #print results to console and list object		
      z1[[i]][[j]]<-y
      print(format(z1[[i]][[j]],big.mark=','))
      
      #compute dinit summary
      temp<-matrix(0,nrow=4,ncol=2)		
      colnames(temp)<-c('summary statistic','count')
      temp[,1]<-c('minimum count/timestep','maximum count/timestep',
                  'median count/timestep','mean count/timestep')
      temp[1,2]<-min(y$count)
      temp[2,2]<-max(y$count)
      temp[3,2]<-round(median(y$count),0)
      temp[4,2]<-round(mean(y$count),0)
      
      #print summary to console and list object
      z2[[i]][[j]]<-as.data.frame(temp)
      print(format(z2[[i]][[j]],big.mark=','))
      
      #plot disturbance area trajectory
      plot(y$timestep,y$count,type='l',xlab='',ylab='',
           col=col.line,lwd=2)
      
      #add plot title	- dist.type	
      if(is.null(step.length)) xlab='Timestep'
      else xlab=paste('Timestep (x',step.length,' yrs)')
      title(main=paste(dist.levels[i],'Disturbance Trajectory',sep=' '),
        ylab='Count',xlab=xlab,cex.main=cex.main,...)
      
      #add subtitle - run number
      mtext(side=3,col=col.sub,cex=cex.sub,
            text=paste('Run #',runs[j],sep=''),...)
      
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
tarea <-
function(path,session=NULL,var='mean',runs=NULL,start.step=1,
	stop.step=NULL,cell.size=30,step.length=NULL,y.scale='ha',
	col.bar=c('yellow','pink','blue','green','orange','red'),
	col.text='brown',outfile=FALSE,...){

##things to do:
#add plot by cover type? see preturn for possible approach
#need to implement y.scale=percent option

#set defaults
options(warn=0)
old.par<-par(no.readonly=TRUE)
	
#read tarea data
x<-read.csv(paste(path,'tarea.csv',sep=''),header=TRUE)

#rescale cell counts
x$cell.count<-round(x$cell.count*((cell.size^2)/10000),2)

#set global session and runs parameters
if(is.null(session)) session<-unique(x$session.id)

#verify valid session ids
all.sessions<-unique(x$session.id)
if(any(!session %in% all.sessions)) stop('Invalid session ids')

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
  
	#create mtype.level variable for loops
	mtype.levels<-as.vector(unique(x$management.type))

	#create list object for results
	z<-vector("list", length(mtype.levels))
	names(z)<-paste(mtype.levels,' Treated Area Comparison (',y.scale,')',sep='')
  
	#loop thru mtypes
	for(i in 1:length(mtype.levels)){
    
    #create list object for results
	  z[[i]]<-vector("list",length(run.levels))
	  names(z[[i]])<-paste('run number ',run.levels,sep='')
	  
		#loop thru runs 
		for(j in 1:length(run.levels)){

			#select data for mtype and run
			y2<-x[x$management.type==mtype.levels[i] & x$run.id==run.levels[j],]
			dtype.levels<-sort(as.vector(unique(y2$dist.type)))

			#set start.step and stop.step parameters
			if(start.step>max(y2$timestep.id)) stop('Start.step exceeds maximum timestep')
			if(is.null(stop.step)) stop.step<-max(y2$timestep.id)
			else{
			  if(stop.step>max(y2$timestep.id)) warning('Stop.step exceeds maximum timestep and was set to the maximum')
			  stop.step<-min(stop.step,max(y2$timestep.id))
			}
      
			#subset data based on start.step and stop.step
			y2<-y2[y2$timestep.id>=start.step & y2$timestep.id<=stop.step,]
			timestep<-seq(start.step,stop.step,1)
			timestep<-as.data.frame(timestep)
      
			#sort sesssion.id
			session<-sort(unique(y2$session.id))
			
			#create results matrix
			zz1<-matrix(0,nrow=length(dtype.levels),ncol=length(session))
			rownames(zz1)<-dtype.levels
			colnames(zz1)<-session

			#loop thru sessions
			for(k in 1:length(session)){

				#select records for session
				y3<-y2[y2$session.id==session[k],]

				#loop thru treatment types
				for(m in 1:length(dtype.levels)){
				
					#summarize darea by timestep
					y4<-y3[y3$dist.type==dtype.levels[m],]
					temp<-aggregate(y4$cell.count,list(y4$timestep.id),sum)
					colnames(temp)<-c('timestep',dtype.levels[m])
					y4<-merge(timestep,temp,by='timestep',all.x=TRUE,sort=FALSE)
					y4[is.na.data.frame(y4)]<-0
					y4<-y4[order(y4$timestep),]
					if(var=='median') temp<-median(y4[,2])
					else if(var=='min') temp<-min(y4[,2])
					else if(var=='max') temp<-max(y4[,2])
					else temp<-mean(y4[,2])
					if(y.scale=='percent') zz1[m,k]<-round(temp,2)
					else zz1[m,k]<-round(temp)
					}

				#optionally convert tarea to percent of eligible
#				if(y.scale=='percent'){
#					t<-read.csv(paste(path,'eligible.csv',sep=''),header=TRUE)
#					t<-t[t$session.id==session & t$dist.type==dist.levels[i],]
#					t<-round(t$cell.count*((cell.size^2)/10000),0)
#					y[,2:4]<-round((y[,2:4]/t)*100,2)
#					}
		
				}

			#print summary to console and list object
			Treatment.type<-dtype.levels
			z[[i]][[j]]<-as.data.frame(cbind(Treatment.type,zz1))
			print(zz1)
			
			#create clustered bar chart
			barplot(zz1,beside=TRUE,border='black',
				xaxs='i',yaxs='i',col=col.bar,
				axis.lty=1,...)
	
			#add plot title	- dist.type			
			if(y.scale=='percent')
				title(main=paste(mtype.levels[i],' (',var,' treatment area/timestep',')',sep=''),
					ylab='Percent of Eligible',xlab='Scenario/Session',...)
			else if(y.scale=='ha')
				title(main=paste(mtype.levels[i],' (',var,' treatment area/timestep',')',sep=''),
					ylab='Area (ha)',xlab='Scenario/Session',...)
	
			#add subtitle - run number
			mtext(side=3,col=col.text,text=paste('Run #',run.levels[j],sep=''))
	
			#add legend				
			legend(x='topright',inset=c(0.05,0.05),
				legend=dtype.levels,fill=col.bar)
		
			if(!i==length(mtype.levels) || !j==runs) 
				readline("Press return for next plot ")

			}
		}

  #output tables to file
  if(outfile==TRUE){
    write.table(z[[i]],file=paste(path,mtype.levels[i],'_Tarea_Comparison.csv',sep=''),
      quote=FALSE,row.names=FALSE,sep=',',append=TRUE)
    }
	}
	
#stacked bar chart trajectory for single sesssion	
else{

  #subset data based on session.id
  x<-x[x$session.id==session,]
  
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

  #create mtype.level vector for loops
  mtype.levels<-as.vector(unique(x$management.type))
  
  #create list objects for results
  z1<-vector("list", length(mtype.levels))
  names(z1)<-paste(mtype.levels,' Treated Area (',y.scale,')',sep='')
  z2<-vector("list", length(mtype.levels))
  names(z2)<-paste(mtype.levels,' Treated Area Summary (',y.scale,')',sep='')
  
	#loop thru mtypes
	for(i in 1:length(mtype.levels)){

    #create list objects for results
	  z1[[i]]<-vector("list",length(run.levels))
	  names(z1[[i]])<-paste('run number ',run.levels,sep='')
	  z2[[i]]<-vector("list",length(run.levels))
	  names(z2[[i]])<-paste('run number ',run.levels,sep='')
    
		#select mtype records	
		y1<-x[x$management.type==mtype.levels[i],]

		#loop thru runs 
		for(j in 1:length(run.levels)){

			#select run records
			y2<-y1[y1$run.id==run.levels[j],]
      
      #create dtype.level vector for loop
			dtype.levels<-sort(as.vector(unique(y2$dist.type)))

			#create results file
			zz1<-as.data.frame(matrix(0,nrow(timestep),length(dtype.levels)+1))
			zz1<-cbind(timestep,zz1)
			names(zz1)<-c('timestep',dtype.levels,'total')

			#loop thru treatment types
			for(k in 1:length(dtype.levels)){
			
				#summarize darea by dist.type, run and timestep
				y3<-y2[y2$dist.type==dtype.levels[k],]

				temp<-aggregate(y3$cell.count,list(y3$timestep.id),sum)
				colnames(temp)<-c('timestep',dtype.levels[k])
				y3<-merge(timestep,temp,by='timestep',all.x=TRUE,sort=FALSE)
				y3[is.na.data.frame(y3)]<-0
				y3<-y3[order(y3$timestep),]
				zz1[,k+1]<-round(y3[,2])
				
				}

			#calculate total treated area
			y3<-y2[y2$run.id==run.levels[j],]
			temp<-aggregate(y3$cell.count,list(y3$timestep.id),sum)
			colnames(temp)<-c('timestep','total')
			y3<-merge(timestep,temp,by='timestep',all.x=TRUE,sort=FALSE)
			y3[is.na.data.frame(y3)]<-0
			y3<-y3[order(y3$timestep),]
			zz1[,ncol(zz1)]<-round(y3[,2])

			#optionally convert tarea to percent of potential treatment area
#			if(y.scale=='percent'){
#				t<-read.csv(paste(path,'eligible.csv',sep=''),header=TRUE)
#				t<-t[t$session.id==session & t$dist.type==dist.levels[i],]
#				t<-round(t$cell.count*((cell.size^2)/10000),0)
#				y[,2:4]<-round((y[,2:4]/t)*100,2)
#				}
	
			#print results to console and list object		
			z1[[i]][[j]]<-zz1
			print(format(z1[[i]][[j]],big.mark=','))
			
			#compute tarea summary
			zz2<-matrix(0,4,ncol(zz1))		
			colnames(zz2)<-c('summary statistic',dtype.levels,'total')
			zz2[,1]<-c('minimum treated area/timestep','maximum treated area/timestep',
				'median treated area/timestep','mean treated area/timestep')
			if(y.scale=='percent'){
				zz2[1,-1]<-round(apply(zz1[,-1],2,min),2)
				zz2[2,-1]<-round(apply(zz1[,-1],2,max),2)
				zz2[3,-1]<-round(apply(zz1[,-1],2,median),2)
				zz2[4,-1]<-round(apply(zz1[,-1],2,mean),2)
				}
			else{
				zz2[1,-1]<-round(apply(zz1[,-1],2,min),0)
				zz2[2,-1]<-round(apply(zz1[,-1],2,max),0)
				zz2[3,-1]<-round(apply(zz1[,-1],2,median),0)
				zz2[4,-1]<-round(apply(zz1[,-1],2,mean),0)
				}
	
			#print summary to console and list object
			z2[[i]][[j]]<-as.data.frame(zz2)
			print(format(z2[[i]][[j]],big.mark=','))
			
			#plot disturbance area trajectory
			if(length(dtype.levels)>1){
			  n<-length(dtype.levels)+1
			  barplot(t(zz1[,2:n]),space=0,
				  xaxs='i',yaxs='i',col=col.bar,
				  axis.lty=1,names=zz1$timestep)
				}
			else{
			  barplot(zz1[,2],space=0,
				  xaxs='i',yaxs='i',col='blue',
				  axis.lty=1,names=zz1$timestep,...)
				}
	
			#add plot title	- dist.type	
			if(is.null(step.length)) xlab='Timestep'
			else xlab=paste('Timestep (x',step.length,' yrs)')
			if(y.scale=='percent')
				title(main=paste(mtype.levels[i],'Treated Area Trajectory',sep=' '),
					ylab='Percent of Eligible',xlab=xlab)
			else if(y.scale=='ha')
				title(main=paste(mtype.levels[i],'Treated Area Trajectory',sep=' '),
					ylab='Area (ha)',xlab=xlab)
	
			#add subtitle - run number
			mtext(side=3,col=col.text,text=paste('Run #',run.levels[j],sep=''))
	
			#add legend	
      if(length(dtype.levels)>1){
			  legend(x='topright',inset=c(0.05,0.05),
				  legend=dtype.levels,fill=col.bar)
      }
      else{
        legend(x='topright',inset=c(0.05,0.05),
          legend=dtype.levels,fill='blue')
      }
         
			if(!i==length(mtype.levels) || !j==runs) 
				readline("Press return for next plot ")
			}
		}

	#create list object
	z<-list(z1,z2)
	names(z)<-c('Treated Area Trajectory','Treated Area Summary')

	#output tables to file
	if(outfile==TRUE){
		for(i in 1:length(z1)){
			write.table(z1[[i]],file=paste(path,mtype.levels[i],'_tarea_trajectory.csv',sep=''),
			quote=FALSE,row.names=FALSE,sep=',',append=TRUE)
			write.table(z2[[i]],file=paste(path,mtype.levels[i],'_tarea_summary.csv',sep=''),
			quote=FALSE,row.names=FALSE,sep=',',append=TRUE)
			}
		}
	}

par(old.par)
invisible(z)
}
fragland <-
function(path,infile,LID.path,scenarios=NULL,
  sessions=NULL,sessions.name='session',runs=NULL,runs.name='run',
  metrics=NULL,var='srv50%',start.step=0,stop.step=NULL,outfile=FALSE){

#set defaults
options(warn=0)

#read fragstats data
y<-read.csv(paste(path,infile,sep=''),strip.white=TRUE,header=TRUE)

#parse LID name
temp<-gsub('\\',',',LID.path,fixed=TRUE)
write.table(temp,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
temp<-read.csv('temp.txt',header=FALSE)
t0<-length(temp)-1
temp<-gsub('\\',',',y$LID,fixed=TRUE)
write.table(temp,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
temp<-read.csv('temp.txt',header=FALSE)
temp<-temp[,-c(1:t0)]
temp<-temp[,c(1:3)]
names(temp)<-c('scenario','session.id','run.id')
temp$session.id<-sub(sessions.name,'',temp$session.id,fixed=TRUE)
temp$session.id<-as.numeric(temp$session.id)
temp$run.id<-sub(runs.name,'',temp$run.id,fixed=TRUE)
temp$run.id<-as.numeric(temp$run.id)
y<-cbind(temp,y[,-1])

#set scenarios parameter
if(is.null(scenarios)) scenarios<-as.vector(unique(y$scenario))

#verify valid scenarios
all.scenarios<-unique(y$scenario)
if(any(!scenarios %in% all.scenarios)) stop('Invalid scenarios')

#set sessions parameter
if(is.null(sessions)) sessions<-as.vector(unique(y$session.id))

#verify valid session ids
all.sessions<-unique(y$session.id)
if(any(!sessions %in% all.sessions)) stop('Invalid session ids')

#set runs parameter
if(is.null(runs)) runs<-unique(y$run.id)

#verify valid run ids
all.runs<-unique(y$run.id)
if(any(!runs %in% all.runs)) stop('Invalid run ids')

#select data
y<-y[y$scenario %in% scenarios & y$session.id %in% sessions & y$run.id %in% runs,]
if(nrow(y)==0){
  stop('No observations given specified scenarios, sessions and runs')
  }

#create data frame with stats for scenarios, sesssions, runs, etc.
y$timestep<-0
t0<-unique(y[,1:2])
row.names(t0)<-NULL
t0$runs<-0
t0$start.step<-0
t0$stop.step<-0

#loop thru scenarios and sessions
for(j in 1:nrow(t0)){

	#select records for current scenario and session
	y1<-y[y$scenario==t0[j,1] & y$session.id==t0[j,2],1:3]

  #add timestep variable by run
  truns<-as.vector(unique(y1$run.id))
  t0[j,3]<-length(truns)
  tstep<-NULL
  for(k in truns){
	  t1<-sum(y1$run.id==k)
    tstep<-c(tstep,seq(0,t1-1)) 
    }
  y$timestep[y$scenario==t0[j,1] & y$session.id==t0[j,2]]<-tstep  
  }

#rearrange dataframe 
y<-y[,c(1:3,ncol(y),4:c(ncol(y)-1))]

#set metrics parameter
all.metrics<-colnames(y)[-c(1:4)]
if(is.null(metrics)) metrics<-all.metrics
if(any(!metrics %in% all.metrics)) stop('Invalid metrics selected')

#select columns
y.metrics<-subset(y,select=metrics)
y.head<-y[,1:4]
y<-cbind(y.head,y.metrics)

#create results files for multiple scenarios and/or sessions
if(nrow(t0)>1){
	landscape.metric<-colnames(y)[-c(1:4)]
	zz1<-as.data.frame(landscape.metric)
	row.names(zz1)<-NULL
	colnames(zz1)<-'landscape.metric'
	zz2<-t0[,1:2]
	row.names(zz2)<-NULL	
	zz2$ldi<-NA
	}

#loop thru scenarios and sessions
for(j in 1:nrow(t0)){

	#select records for current scenario and session
	y1<-y[y$scenario==t0[j,1] & y$session.id==t0[j,2],-c(1:3)]

	#set file-dependent defaults
	if(is.null(stop.step)){
		stopstep<-max(y1$timestep)
		}
  else{
		if(stop.step>max(y1$timestep)) 
			warning('Stop.step exceeds maximum timestep and will be set to the maximum')
		stopstep<-min(stop.step,max(y1$timestep))
		  }
	if(start.step>=stopstep) 
		stop('Start.step must be less than maximum timestep')
	t0[j,4]<-start.step
	t0[j,5]<-stopstep

	#calculate landscape metric SRV quantiles
	q1<-matrix(0,ncol(y1)-1,101)
	q2<-y1[y1$timestep>=start.step & y1$timestep<=stopstep,]
	for(i in 1:nrow(q1)){ 
		q1[i,1:101]<-quantile(q2[,i+1],probs=seq(0,1,0.01),na.rm=TRUE)
		}
	
	#get current landscape metric (timestep 0)
	current.value<-round(t(y1[y1$timestep==0,-1][1,]),3)
	
	#put most of it together
	z1<-round(as.data.frame(q1),3)
	landscape.metric<-colnames(y1)[-1]
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
	t1<-rep(0,nrow(z1))
	z1<-cbind(z1,t1)
	z1[,104]<-as.numeric(z1[,104])
	for(i in 1:nrow(z1)){ 		
		if(is.na(z1[i,104])) z1[i,106]<-'NA'
		else{
		    if     (z1[i,104]!=50) z1[i,106]<-round((z1[i,104]-50)/50*100,0) # column 104 is current value, column 106 is departure index
			#else if(z1[i,104]>50) z1[i,106]<-round((z1[i,104]-50)/50*100,0)
			else z1[i,106]<-0
			}
		}
	z1[,106]<-as.numeric(z1[,106])
	z2<-mean(abs(z1[,106]),na.rm=TRUE)
	z2<-round(z2,0)
    
    # my departure index
    # if the percentile value is less than 50
    # column 2 is 0th percentile; column 102 is 100th percentile
    # column 52 is 50th percentile
    # column 7 is 5th, column 97 is 95th
#  t1<-rep(0,nrow(z1))
#  z1<-cbind(z1,t1)
#  z1[,104]<-as.numeric(z1[,104])
#  for(i in 1:nrow(z1)){ 		
#      if(is.na(z1[i,104])) z1[i,106]<-'NA'
#      else{
#        if(z1[i,104]<50) z1[i,106]<-round( 
#        (z1[i,103] - z1[i,52])/ # actual minus the median
#         #(z1[i,52] - z1[i,2]) * 100, # median minus 0
#         (z1[i,52] - z1[i,7]) * 100, # median minus 5 - we want 5th as the range right?
#          0)
#      else if(z1[i,104]>50)
#        z1[i,106]<-round( 
#            (z1[i,103] - z1[i,52])/ # actual minus the median
#                #(z1[i,102] - z1[i,52]) * 100, # 100th minus median 
#                (z1[i,97] - z1[i,52]) * 100, # 95th minus median 
#            0)
#      else z1[i,106]<-0
#            }
#        }
    z1[,106]<-as.numeric(z1[,106])
	z2<-mean(abs(z1[,106]),na.rm=TRUE)
	z2<-round(z2,0)
  
	#put the final table together
	z1<-z1[,c(1,2,7,27,52,77,97,102,105,103,104,106)]
	row.names(z1)<-NULL
	colnames(z1)<-c('landscape.metric','srv0%','srv5%','srv25%',
		'srv50%','srv75%','srv95%','srv100%','srv.cv',
		'current.value','current.%SRV','departure.index')

	#merge selected covcond stat to final results table
	if(nrow(t0)>1){
		temp<-subset(z1,select=var)
		names(temp)<-paste(var,'.',t0[j,1],'.',t0[j,2],sep='')
		zz1<-cbind(zz1,temp)
		}
	
	#add departure index result to final results table
	if(nrow(t0)>1) zz2[j,3]<-z2

  }

#create list object
if(nrow(t0)>1) z<-list(infile,t0,zz1,zz2)
else z<-list(infile,t0,z1,z2)
names(z)<-c('infile','run stats','landscape SRV',
  'landscape departure index (%)')

#output tables to file
if(outfile==TRUE){
	write.table(z[[3]],file=paste(paste(path,infile,sep=''),'srv.csv',sep='.'),
	quote=FALSE,row.names=FALSE,append=TRUE,sep=',')
	write.table(z[[4]],file=paste(paste(path,infile,sep=''),'ldi.csv',sep='.'),
	quote=FALSE,row.names=FALSE,append=TRUE,sep=',')
	}
	
return(z)
}
fragland.pdf.plot <-
function(infile,path,LID.path,scenarios=NULL,
  sessions=NULL,sessions.name='session',runs=NULL,runs.name='run',
  pool.runs=TRUE,metrics=NULL,current=TRUE,start.step=0,stop.step=NULL,
  ref.scenario=NULL,ref.session=NULL,ref.tstep=NULL,ref.include=TRUE,
  quantiles=c(0.05,0.95),col.line='dark blue',col.sub='brown',
  cex.main=1.5,cex.sub=1.25,cex.legend=1.25,cex.lab=1.25,
  outfile=FALSE,save.figs=FALSE,...){

#set defaults
options(warn=0)
old.par<-par(no.readonly=TRUE)

#read fragstats data
y<-read.csv(paste(path,infile,sep=''),strip.white=TRUE,header=TRUE)

#parse LID name
temp<-gsub('\\',',',LID.path,fixed=TRUE)
write.table(temp,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
temp<-read.csv('temp.txt',header=FALSE)
t0<-length(temp)-1
temp<-gsub('\\',',',y$LID,fixed=TRUE)
write.table(temp,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
temp<-read.csv('temp.txt',header=FALSE)
temp<-temp[,-c(1:t0)]
temp<-temp[,c(1:3)]
names(temp)<-c('scenario','session.id','run.id')
temp$session.id<-sub(sessions.name,'',temp$session.id,fixed=TRUE)
temp$session.id<-as.numeric(temp$session.id)
temp$run.id<-sub(runs.name,'',temp$run.id,fixed=TRUE)
temp$run.id<-as.numeric(temp$run.id)
y<-cbind(temp,y[,-1])

#set scenarios parameter
if(is.null(scenarios)) scenarios<-as.vector(unique(y$scenario))

#verify valid scenarios
all.scenarios<-unique(y$scenario)
if(any(!scenarios %in% all.scenarios)) stop('Invalid scenarios')

#set sessions parameter
if(is.null(sessions)) sessions<-as.vector(unique(y$session.id))

#verify valid session ids
all.sessions<-unique(y$session.id)
if(any(!sessions %in% all.sessions)) stop('Invalid session ids')

#set runs parameter
if(is.null(runs)) runs<-unique(y$run.id)

#verify valid run ids
all.runs<-unique(y$run.id)
if(any(!runs %in% all.runs)) stop('Invalid run ids')

#select data
y<-y[y$scenario %in% scenarios & y$session.id %in% sessions & y$run.id %in% runs,]
if(nrow(y)==0){
  stop('No observations given specified scenarios, sessions and runs')
}

#create data frame with stats for scenarios, sesssions, runs, etc.
t0<-unique(y[,1:2])

#create timestep variable
y$timestep<-0

#loop thru scenarios and sessions
for(j in 1:nrow(t0)){
  
  #select records for current scenario and session
  y1<-y[y$scenario==t0[j,1] & y$session.id==t0[j,2],1:3]
  
  #add timestep variable by run
  truns<-as.vector(unique(y1$run.id))
  tstep<-NULL
  for(k in truns){
    t1<-sum(y1$run.id==k)
    tstep<-c(tstep,seq(0,t1-1)) 
  }
  y$timestep[y$scenario==t0[j,1] & y$session.id==t0[j,2]]<-tstep  
}

#rearrange dataframe 
y<-y[,c(1:3,ncol(y),4:c(ncol(y)-1))]

#set metrics parameter
all.metrics<-colnames(y)[-c(1:4)]
if(is.null(metrics)) metrics<-all.metrics
if(any(!metrics %in% all.metrics)) stop('Invalid metrics selected')

#select columns
y.metrics<-subset(y,select=metrics)
y.head<-y[,1:4]
y<-cbind(y.head,y.metrics)

#set file-dependent defaults
if(is.null(stop.step)){
  stopstep<-max(y$timestep)
}
else{
  if(stop.step>max(y$timestep)) 
    warning('Stop.step exceeds maximum timestep and will be set to the maximum')
  stopstep<-min(stop.step,max(y$timestep))
}
if(start.step>=stopstep) 
  stop('Start.step must be less than maximum timestep')


####for reference scenario and session comparison
if(!is.null(ref.scenario) & !is.null(ref.session)){
  
  if(is.null(ref.tstep)){
    stop('Missing timestep (tstep) for alternative scenarios and sessions')
  }
  
  #copy data for consistency with script below
  q1<-y
  
  #create reference and alternative scenarios and sessions
  t1<-t0[t0$scenario==ref.scenario & t0$session==ref.session,]
  t2<-t0[!t0$scenario==ref.scenario | !t0$session==ref.session,]    
  
  #create results table for departure index
  if(ref.include==TRUE & current==TRUE){
    z<-as.data.frame(matrix(NA,nrow=ncol(q1)-3,ncol=nrow(t0)+2))
    z[,1]<-c(names(q1[,-c(1:4)]),'mean')
    names(z)<-c('metric','current',paste(t0[,1],t0[,2],sep='.'))
  }
  else if(ref.include==TRUE & current==FALSE){
    z<-as.data.frame(matrix(NA,nrow=ncol(q1)-3,ncol=nrow(t0)+1))
    z[,1]<-c(names(q1[,-c(1:4)]),'mean')
    names(z)<-c('metric',paste(t0[,1],t0[,2],sep='.'))
  }    
  else if(ref.include==FALSE & current==TRUE){
    z<-as.data.frame(matrix(NA,nrow=ncol(q1)-3,ncol=nrow(t2)+2))
    z[,1]<-c(names(q1[,-c(1:4)]),'mean')
    names(z)<-c('metric','current',paste(t2[,1],t2[,2],sep='.'))  
  }
  else if(ref.include==FALSE & current==FALSE){
    z<-as.data.frame(matrix(NA,nrow=ncol(q1)-3,ncol=nrow(t2)+1))
    z[,1]<-c(names(q1[,-c(1:4)]),'mean')
    names(z)<-c('metric',paste(t2[,1],t2[,2],sep='.'))  
  }
  
  #loop thru metrics
  for(i in 5:ncol(q1)){
    
    #order data
    q2<-q1[order(q1$timestep),]
    
    #establish plot limits
    if(current==TRUE){
      xmin<-min(y[,i],na.rm=TRUE)
      xmax<-max(y[,i],na.rm=TRUE)
    }
    else{
      xmin<-min(y[y$timestep>=start.step & y$timestep<=stopstep,i],
        y[y$timestep==ref.tstep,i],na.rm=TRUE)
      xmax<-max(y[y$timestep>=start.step & y$timestep<=stopstep,i],
        y[y$timestep==ref.tstep,i],na.rm=TRUE)
    }
    xrange<-xmax-xmin
    
    #plot to file
    if(save.figs==TRUE){
      bitmap(file=paste(names(q2)[i],'.png',sep=''),
        height=6,width=8,res=300,...) 
    }
    
    #create blank plot
    plot(1,1,type='n',xlim=c(xmin-(xrange*0.03),xmax+(xrange*0.03)),
      ylim=c(0,1.3),xaxs='i',xlab=names(q1[i]),ylab='Density',main='',
      cex.lab=cex.lab,...)
    
    #plot reference scenario and session
    q3<-q2[q2$scenario==t1[1,1] & q2$session==t1[1,2] & 
      q2$timestep>=start.step & q2$timestep<=stopstep,]
    
    if(nrow(q3)==0){
      stop('No data given specified arguments')
    }
    else if(nrow(q3)==1){
      segments(q3[,i],0,q3[,i],1,lwd=2,lty=1,col=1)     
    }
    else{
      temp<-density(q3[,i],from=xmin,to=xmax,na.rm=TRUE)
      lines(temp$x,temp$y/max(temp$y),lwd=2,lty=1,col=1)  
    }
    
    #compute landscape departure index for current
    if(current==TRUE){
      z[i-4,2]<-departure(ref=q3[,i],alt=q2[q2$timestep==0,i][1])
    }
    
    #plot reference scenario and session at specified ref.tstep
    if(ref.include==TRUE){
      q4<-q2[q2$scenario==t1[1,1] & q2$session==t1[1,2] & 
        q2$timestep==ref.tstep,]
      
      if(nrow(q4)==0){
        stop('No reference data at specified ref.tstep')
      }
      else if(nrow(q4)==1){
        segments(q4[,i],0,q4[,i],1,lwd=2,lty=2,col=1)     
      }
      else{
        temp<-density(q4[,i],from=xmin,to=xmax,na.rm=TRUE)
        lines(temp$x,temp$y/max(temp$y),lwd=2,lty=2,col=1)  
      }
      
      #compute landscape departure index
      if(current==TRUE){
        z[i-4,3]<-departure(ref=q3[,i],alt=q4[,i])
      }
      else{
        z[i-4,2]<-departure(ref=q3[,i],alt=q4[,i])
      }
    }
    
    #loop thru alternative scenarios and sessions
    for(k in 1:nrow(t2)){
      
      #plot scenarios and sessions
      q5<-q2[q2$scenario==t2[k,1] & q2$session==t2[k,2] & 
               q2$timestep==ref.tstep,]
      if(nrow(q5)==0){
        stop('No alternatives given specified arguments')
      }
      else if(nrow(q5)==1){
        segments(q5[,i],0,q5[,i],1,lwd=2,lty=k+2,col=k+1)  
      }
      else{
        temp<-density(q5[,i],from=xmin,to=xmax,na.rm=TRUE)
        lines(temp$x,temp$y/max(temp$y),lwd=2,lty=k+2,col=k+1)  
      }
      
      #compute landscape departure index
      if(ref.include==TRUE & current==TRUE){
        z[i-4,k+3]<-departure(ref=q3[,i],alt=q5[,i])
      }
      else if(ref.include==TRUE & current==FALSE){
        z[i-4,k+2]<-departure(ref=q3[,i],alt=q5[,i])
      }
      else if(ref.include==FALSE & current==TRUE){
        z[i-4,k+2]<-departure(ref=q3[,i],alt=q5[,i])
      }
      else if(ref.include==FALSE & current==FALSE){
        z[i-4,k+1]<-departure(ref=q3[,i],alt=q5[,i])
      }
      
      #add current condition
      if(current==TRUE){
        segments(y[y$scenario==t0[k,1] & y$session.id==t0[k,2] & y$timestep==0,i],
           0,y[y$scenario==t0[k,1] & y$session.id==t0[k,2] & y$timestep==0,i],1,
           lwd=2,lty=1,col=k)
      }
    }
    
    #add legend
    if(current==TRUE){
      legend(x='topleft',ncol=1,legend='current',bty='n',
        lty=1,lwd=2,col='darkgrey',cex=cex.legend,title='Current')
    }
    #reference scenario
    legend(x='top',ncol=1,legend=c(paste(t1[,1],t1[,2],sep='.')),bty='n',
        lty=1,lwd=2,col=1,cex=cex.legend,title='Reference')

    #alternative scenarios  
    if(ref.include==TRUE){
      legend(x='topright',ncol=1,legend=c(paste(t0[,1],t0[,2],sep='.')),bty='n',
        lty=seq(2,nrow(t0)+1),lwd=2,col=seq(1,nrow(t0)),cex=cex.legend,
        title='Alternatives')
    }
    else{
      legend(x='topright',ncol=1,legend=c(paste(t2[,1],t2[,2],sep='.')),bty='n',
        lty=seq(3,nrow(t2)+3),lwd=2,col=seq(2,nrow(t2)+2),cex=cex.legend,
        title='Alternatives')
    }    
    #add plot title				
    title(main=paste('Landscape Metric Density',
      ' (',names(q2)[i],')',sep=''),line=2.5,cex.main=cex.main,...)
    
    #add subtitle
    mtext(side=3,line=1,col=col.sub,cex=cex.sub,
      text=paste('Comparison of reference scenario to timestep ',
         ref.tstep,' of the alternatives',sep=''))
    
    #add horizontal line at zero
    abline(h=0,col='gray')
    
    if(save.figs==TRUE) dev.off()
    
    if(save.figs==FALSE & !i==ncol(q1))
      readline('Press return for next plot')
    
  } #end loop thru metrics
  
  #compute total landscape departure index
  z[ncol(q1)-3,-1]<-apply(as.data.frame(z[,-1]),2,mean,na.rm=TRUE)
  
  #print landscape departure index table
  print('Landscape departure index table')
  z[,-1]<-round(z[,-1],0)
  print(z)  
  
  #output tables to file
  if(outfile==TRUE){
    write.table(z,file=paste(paste(path,infile,sep=''),'ref','ldi.csv',sep='.'),
      quote=FALSE,row.names=FALSE,append=TRUE,sep=',')
  }
  
} #end for reference scenario and session

###for multiple scenarios and sessions without a reference
else if(nrow(t0)>1){
  
  #select data
  q1<-y[y$timestep>=start.step & y$timestep<=stopstep,]
  
  #reestablish runs
  runs<-as.vector(unique(q1$run.id))
  
  #loop thru metrics
  for(i in 5:ncol(q1)){
    
    #establish plot limits
    if(current==TRUE){
      xmin<-min(q1[,i],y[y$timestep==0,i],na.rm=TRUE)
      xmax<-max(q1[,i],y[y$timestep==0,i],na.rm=TRUE)
    }
    else{
      xmin<-min(q1[,i],na.rm=TRUE)
      xmax<-max(q1[,i],na.rm=TRUE)
    }
    xrange<-xmax-xmin
    
    #for pooled across runs
    if(pool.runs==TRUE){
      
      #order data
      q2<-q1[order(q1$timestep),]
      
      #plot to file
      if(save.figs==TRUE){
        bitmap(file=paste(names(q2)[i],'.png',sep=''),
               height=6,width=8,res=300,...) 
      }
            
      #create blank plot
      plot(1,1,type='n',xlim=c(xmin-(xrange*0.03),xmax+(xrange*0.03)),
        ylim=c(0,1.1),xaxs='i',xlab=names(q2[i]),ylab='Density',
        cex.lab=cex.lab,main='',...)
      
      #loop thru scenarios and sessions
      for(k in 1:nrow(t0)){
        
        #plot scenarios and sessions
        q3<-q2[q2$scenario==t0[k,1] & q2$session==t0[k,2],]
        if(!nrow(q3)==0){
          temp<-density(q3[,i],from=xmin,to=xmax,na.rm=TRUE)
          lines(temp$x,temp$y/max(temp$y),lwd=2,lty=k,col=k)  
        }
        #add current condition
        if(current==TRUE){
          segments(y[y$scenario==t0[k,1] & y$session.id==t0[k,2] & y$timestep==0,i],
            0,y[y$scenario==t0[k,1] & y$session.id==t0[k,2] & y$timestep==0,i],1,
            lwd=2,lty=1,col='darkgrey')
        }
           
      }
      
      #add legend
      if(current==TRUE){
        legend(x='topleft',ncol=3,
          legend=c('current',paste(t0[,1],t0[,2],sep='.')),
          lty=c(1,seq(1,nrow(t0))),bty='n',lwd=2,
          col=c('darkgrey',seq(1,nrow(t0))),cex=cex.legend)
      }
      else{  
        legend(x='topleft',ncol=3,
          legend=paste(t0[,1],t0[,2],sep='.'),
          lty=seq(1,nrow(t0)),bty='n',lwd=2,
          col=c(seq(1,nrow(t0))),cex=cex.legend)
      }
      
      #add plot title				
      title(main=paste('Landscape Metric Density',
        ' (',names(q2)[i],')',sep=''),line=2.5,cex.main=cex.main,...)
      
      #add subtitle
      mtext(side=3,line=1,col=col.sub,cex=cex.sub,text='Pooled across runs',...)
      
      #add horizontal line at zero
      abline(h=0,col='gray')
      
      if(save.figs==TRUE) dev.off()
      
    } #end if pooled across runs
    
    #for separate plot per run
    else{
      
      #loop thru runs
      for(j in 1:length(runs)){
        
        #select data for run
        q2<-q1[q1$run.id==runs[j],]
        q2<-q2[order(q2$timestep),]
        
        #plot to file
        if(save.figs==TRUE){
          bitmap(file=paste(names(q2)[i],runs[j],'.png',sep=''),
                 height=6,width=8,res=300,...) 
        }
                
        #create blank plot
        plot(1,1,type='n',xlim=c(xmin-(xrange*0.03),xmax+(xrange*0.03)),
          ylim=c(0,1.1),xaxs='i',xlab=names(q1[i]),ylab='Density',
          cex.lab=cex.lab,main='',...)
        
        #loop thru scenarios and sessions
        for(k in 1:nrow(t0)){
          
          #plot scenarios and sessions
          q3<-q2[q2$scenario==t0[k,1] & q2$session==t0[k,2],]
          if(!nrow(q3)==0){
            temp<-density(q3[,i],from=xmin,to=xmax,na.rm=TRUE)
            lines(temp$x,temp$y/max(temp$y),lwd=2,lty=k,col=k)  
          }
          
          #add current condition
          if(current==TRUE){
            segments(y[y$scenario==t0[k,1] & y$session.id==t0[k,2] & y$timestep==0,i],
              0,y[y$scenario==t0[k,1] & y$session.id==t0[k,2] & y$timestep==0,i],1,
              lwd=2,lty=1,col='darkgrey')
          }
        }
                
        #add legend
        if(current==TRUE){
          legend(x='topleft',ncol=3,
            legend=c('current',paste(t0[,1],t0[,2],sep='.')),
            lty=c(1,seq(1,nrow(t0))),bty='n',lwd=2,
            col=c('darkgrey',seq(1,nrow(t0))),cex=cex.legend)
        }
        else{  
          legend(x='topleft',ncol=3,
            legend=paste(t0[,1],t0[,2],sep='.'),
            lty=seq(1,nrow(t0)),bty='n',lwd=2,
            col=c(seq(1,nrow(t0))),cex=cex.legend)
        }
        
        #add plot title				
        title(main=paste('Landscape Metric Density',
          ' (',names(q2)[i],')',sep=''),line=2.5,cex.main=cex.main,...)
        
        #add subtitle
        mtext(side=3,line=1,col=col.sub,cex=cex.sub,
          text=paste('Run #',runs[j],sep=''),...)
        
        #add horizontal line at zero
        abline(h=0,col='gray')
        
        if(save.figs==TRUE) dev.off()
        
        if(save.figs==FALSE & !j==length(runs))
          readline('Press return for next plot')
        
      } #end loop thru runs
      
    } #end separate plot per run
    
    if(save.figs==FALSE & !i==ncol(q1))
      readline('Press return for next plot')
    
  } #end loop thru metrics
  
} #end for multiple scenarios and sessions without a reference


###for single scenario and session
else{
  
  #select data
  q1<-y[y$timestep>=start.step & y$timestep<=stopstep,]

  #reestablish runs
  runs<-as.vector(unique(q1$run.id))
  
  #loop thru metrics
  for(i in 5:ncol(q1)){
    
    #establish plot limits
    if(current==TRUE){
      xmin<-min(q1[,i],y[y$timestep==0,i],na.rm=TRUE)
      xmax<-max(q1[,i],y[y$timestep==0,i],na.rm=TRUE)
    }
    else{
      xmin<-min(q1[,i],na.rm=TRUE)
      xmax<-max(q1[,i],na.rm=TRUE)
    }
    xrange<-xmax-xmin
    
    #for pooled runs
    if(pool.runs==TRUE){
      
      #select data for run
      q2<-q1[order(q1$timestep),]
      
      #plot to file
      if(save.figs==TRUE){
        bitmap(file=paste(names(q2)[i],'.png',sep=''),
               height=6,width=8,res=300,...) 
      }
            
      #plot density
      z1<-density(q2[,i],from=xmin,to=xmax,na.rm=TRUE)
      plot(z1$x,z1$y/max(z1$y),type='l',
        xlim=c(xmin-(xrange*0.03),xmax+(xrange*0.03)),
        ylim=c(0,1.1),xaxs='i',xlab=names(q2[i]),
        ylab='Density',main='',lwd=2,col=col.line,
        cex.lab=cex.lab,...)
      
      #add current condition and %SRV lines	
      if(current==TRUE){
        segments(y[y$timestep==0,i],0,y[y$timestep==0,i],1,
          lwd=2,lty=1,col='red')
        }
      segments(quantile(q2[,i],.5),0,
        quantile(q2[,i],.5),1,
        lwd=2,lty=2,col='darkgray')
      segments(quantile(q2[,i],quantiles[1]),0,
        quantile(q2[,i],quantiles[1]),1,
        lwd=2,lty=3,col='darkgray')
      segments(quantile(q2[,i],quantiles[2]),0,
        quantile(q2[,i],quantiles[2]),1,
        lwd=2,lty=3,col='darkgray')
      
      #add legend
      if(current==TRUE){
        legend(x='top',ncol=4,
          legend=c('current',paste('q',quantiles[1],sep=''),
          'q0.5',paste('q',quantiles[2],sep='')),
          lty=c(1,3,2,3),bty='n',lwd=2,
          col=c('red','darkgray','darkgray','darkgray'),cex=cex.legend)
        }
      else{  
        legend(x='top',ncol=3,
          legend=c(paste('q',quantiles[1],sep=''),
          'q0.5',paste('q',quantiles[2],sep='')),
          lty=c(3,2,3),bty='n',lwd=2,col='darkgrey',cex=cex.legend)
        }
      
      #add plot title				
      title(main=paste('Landscape Metric Density',
        ' (',names(q2)[i],')',sep=''),line=2.5,cex.main=cex.main,...)
      
      #add subtitle
      mtext(side=3,line=1,col=col.sub,cex=cex.sub,
        text='Pooled across runs',...)
      
      #add horizontal line at zero
      abline(h=0,col='gray')
      
      if(save.figs==TRUE) dev.off()

      if(save.figs==FALSE & !i==ncol(q1))
        readline('Press return for next plot')
      
    } #end pooled across runs
    
    #for separate plots per run
    else{
      
      #loop thru runs
      for(j in 1:length(runs)){
        
        #select data for run
        q2<-q1[q1$run.id==runs[j],]
        q2<-q2[order(q2$timestep),]
        
        #plot to file
        if(save.figs==TRUE){
          bitmap(file=paste(names(q2)[i],runs[j],'.png',sep=''),
                 height=6,width=8,res=300,...) 
        }
                
        #plot density
        z1<-density(q2[,i],from=xmin,to=xmax,na.rm=TRUE)
        plot(z1$x,z1$y/max(z1$y),type='l',
          xlim=c(xmin-(xrange*0.03),xmax+(xrange*0.03)),
          ylim=c(0,1.1),xaxs='i',xlab=names(q2[i]),
          ylab='Density',main='',lwd=2,col=col.line,
          cex.lab=cex.lab,...)
        
        #add current condition and %SRV lines	
        if(current==TRUE){
          segments(y[y$timestep==0,i],0,y[y$timestep==0,i],1,
            lwd=2,lty=1,col='red')
          }
        segments(quantile(q2[,i],.5),0,
          quantile(q2[,i],.5),1,
          lwd=2,lty=2,col='darkgray')
        segments(quantile(q2[,i],quantiles[1]),0,
          quantile(q2[,i],quantiles[1]),1,
          lwd=2,lty=3,col='darkgray')
        segments(quantile(q2[,i],quantiles[2]),0,
          quantile(q2[,i],quantiles[2]),1,
          lwd=2,lty=3,col='darkgray')
        
        #add legend
        if(current==TRUE){
          legend(x='topleft',ncol=3,
            legend=c('current',paste('q',quantiles[1],sep=''),
            'q0.5',paste('q',quantiles[2],sep='')),
            lty=c(1,3,2,3),bty='n',lwd=2,
            col=c('red','darkgray','darkgrey','darkgrey'),cex=cex.legend)
          }
        else{  
          legend(x='topleft',ncol=3,
            legend=c(paste('q',quantiles[1],sep=''),
            'q0.5',paste('q',quantiles[2],sep='')),
            lty=c(3,2,3),bty='n',lwd=2,col='darkgrey',cex=cex.legend)
         }
        
        #add plot title				
        title(main=paste('Landscape Metric Density',
          ' (',names(q2)[i],')',sep=''),line=2.5,cex.main=cex.main,...)
        
        #add subtitle
        mtext(side=3,line=1,col=col.sub,cex=cex.sub,
          text=paste('Run #',runs[j],sep=''),...)
        
        #add horizontal line at zero
        abline(h=0,col='gray')
        
        if(save.figs==TRUE) dev.off()
        
        if(save.figs==FALSE & !j==length(runs) | !i==ncol(q1))
          readline('Press return for next plot')
        
      } #end loop thru runs
      
    } #end separate plot per run
    
  } #end loop thru metrics
  
} #end single scenario and session

par(old.par)
}
fragland.plot <-
function(infile,path,LID.path,scenarios=NULL,
  sessions=NULL,sessions.name='session',runs=NULL,runs.name='run',
  metrics=NULL,start.step=0,stop.step=NULL,step.length=NULL,
  quantiles=c(0.05,0.95),col.line='dark blue',col.sub='brown',
  cex.main=1.5,cex.sub=1.5,cex.legend=1.5,cex.lab=1.5,
  save.figs=FALSE,...){

#set defaults
options(warn=0)
old.par<-par(no.readonly=TRUE)

#read fragstats data
y<-read.csv(paste(path,infile,sep=''),strip.white=TRUE,header=TRUE)

#parse LID name
temp<-gsub('\\',',',LID.path,fixed=TRUE)
write.table(temp,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
temp<-read.csv('temp.txt',header=FALSE)
t0<-length(temp)-1
temp<-gsub('\\',',',y$LID,fixed=TRUE)
write.table(temp,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
temp<-read.csv('temp.txt',header=FALSE)
temp<-temp[,-c(1:t0)]
temp<-temp[,c(1:3)]
names(temp)<-c('scenario','session.id','run.id')
temp$session.id<-sub(sessions.name,'',temp$session.id,fixed=TRUE)
temp$session.id<-as.numeric(temp$session.id)
temp$run.id<-sub(runs.name,'',temp$run.id,fixed=TRUE)
temp$run.id<-as.numeric(temp$run.id)
y<-cbind(temp,y[,-1])

#set scenarios parameter
if(is.null(scenarios)) scenarios<-as.vector(unique(y$scenario))

#verify valid scenarios
all.scenarios<-unique(y$scenario)
if(any(!scenarios %in% all.scenarios)) stop('Invalid scenarios')

#set sessions parameter
if(is.null(sessions)) sessions<-as.vector(unique(y$session.id))

#verify valid session ids
all.sessions<-unique(y$session.id)
if(any(!sessions %in% all.sessions)) stop('Invalid session ids')

#set runs parameter
if(is.null(runs)) runs<-unique(y$run.id)

#verify valid run ids
all.runs<-unique(y$run.id)
if(any(!runs %in% all.runs)) stop('Invalid run ids')

#select data
y<-y[y$scenario %in% scenarios & y$session.id %in% sessions & y$run.id %in% runs,]
if(nrow(y)==0){
  stop('No observations given specified scenarios, sessions and runs')
  }

#create data frame with stats for scenarios, sesssions, runs, etc.
t0<-unique(y[,1:2])

#create timestep variable
y$timestep<-0

#loop thru scenarios and sessions
for(j in 1:nrow(t0)){

	#select records for current scenario and session
	y1<-y[y$scenario==t0[j,1] & y$session.id==t0[j,2],1:3]

  #add timestep variable by run
  truns<-as.vector(unique(y1$run.id))
  tstep<-NULL
  for(k in truns){
	  t1<-sum(y1$run.id==k)
    tstep<-c(tstep,seq(0,t1-1)) 
    }
  y$timestep[y$scenario==t0[j,1] & y$session.id==t0[j,2]]<-tstep  
  }

#rearrange dataframe 
y<-y[,c(1:3,ncol(y),4:c(ncol(y)-1))]

#set metrics parameter
all.metrics<-colnames(y)[-c(1:4)]
if(is.null(metrics)) metrics<-all.metrics
if(any(!metrics %in% all.metrics)) stop('Invalid metrics selected')

#select columns
y.metrics<-subset(y,select=metrics)
y.head<-y[,1:4]
y<-cbind(y.head,y.metrics)

#set file-dependent defaults
if(is.null(stop.step)){
  stopstep<-max(y$timestep)
  }
else{
	if(stop.step>max(y$timestep)) 
		warning('Stop.step exceeds maximum timestep and will be set to the maximum')
	stopstep<-min(stop.step,max(y$timestep))
	  }
if(start.step>=stopstep) 
	stop('Start.step must be less than maximum timestep')

#select records
q1<-y[y$timestep>=start.step & y$timestep<=stopstep,]

#reestablish runs
runs<-sort(as.vector(unique(q1$run.id)))

#for multiple scenarios and sessions
if(nrow(t0)>1){

	#loop thru metrics
	for(i in 5:ncol(q1)){

    #create plot limits
    ymin<-min(q1[,i])
    ymax<-max(q1[,i])
    yrange<-ymax-ymin

    #loop thru runs
    for(j in 1:length(runs)){

      #select data for run
      q2<-q1[q1$run.id==runs[j],]
 			q2<-q2[order(q2$timestep),]

      #create blank plot
      if(is.null(step.length)) xlab='Timestep'
      else xlab=paste('Timestep (x',step.length,' yrs)')
       
 			#plot to file
 			if(save.figs==TRUE){
 			  bitmap(file=paste(names(q2)[i],runs[j],'.png',sep=''),
 			         height=6,width=8,res=300,...) 
 			}
       
      #create plot 
      plot(1,1,type='n',xlim=c(start.step,stopstep),xaxs='i',
        ylim=c(ymin-(yrange*0.01),ymax+(yrange*0.1)),
        xlab=xlab,ylab=names(q1[i]),main='',...)
  
  		#loop thru scenarios and sessions
  		for(k in 1:nrow(t0)){
  	
  			#plot scenarios and sessions
  			q3<-q2[q2$scenario==t0[k,1] & q2$session.id==t0[k,2],]
        lines(q3[,4],q3[,i],lwd=2,lty=k,col=k)
  
  			}
 	
  		#add legend
       legend(x='top',ncol=3,
  			legend=paste(t0[,1],t0[,2],sep='.'),
          lty=seq(1,nrow(t0)),bty='n',lwd=2,
          col=c(seq(1,nrow(t0))),cex=cex.legend)
  		
  		#add plot title				
  		title(main=paste('Landscape Metric Trajectory',
        ' (',names(q2)[i],')',sep=''),line=2.5,cex.main=cex.main,...)

 			#add subtitle
			mtext(side=3,line=1,col=col.sub,cex=cex.sub,
        text=paste('Run #',runs[j],sep=' '),...)
      
			if(save.figs==TRUE) dev.off()
			  	
  		if(save.figs==FALSE & !j==length(runs))
  			readline('Press return for next plot')
    	}

 		if(save.figs==FALSE & !i==ncol(q1))
 			readline('Press return for next plot')
    }
  }
    
#for single scenario and session
else{

	#loop thru metrics
	for(i in 5:ncol(q1)){

		#create plot limits
    ymin<-min(min(q1[,i]),y[y$timestep==0,i])
    ymax<-max(max(q1[,i]),y[y$timestep==0,i])
    yrange<-ymax-ymin

    #loop thru runs
    for(j in 1:length(runs)){

      #select data for run
      q2<-q1[q1$run.id==runs[j],]
 			q2<-q2[order(q2$timestep),]
      	
  		#plot metric trajectory
      if(is.null(step.length)) xlab='Timestep'
      else xlab=paste('Timestep (x',step.length,' yrs)')
 			
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
      
  
  		#add current condition and %SRV lines	
  		abline(h=y[y$timestep==0 & y$run.id==runs[j],i],lwd=2,lty=1,
        col='red')
  		abline(h=quantile(q2[,i],0.5,na.rm=TRUE),lwd=2,lty=2,col='darkgrey')
  		abline(h=quantile(q2[,i],quantiles[1],na.rm=TRUE),lwd=2,lty=3,col='darkgrey')
  		abline(h=quantile(q2[,i],quantiles[2],na.rm=TRUE),lwd=2,lty=3,col='darkgrey')
  	
  		#add plot title				
  		title(main=paste('Landscape Metric Trajectory',
        ' (',names(q2)[i],')',sep=''),line=2.5,cex.main=cex.main,...)

 			#add subtitle
			mtext(side=3,line=1,col=col.sub,cex=cex.sub,
        text=paste('Run #',runs[j],sep=' '))
  
  		#add legend
  		legend(x='top',horiz=TRUE,
  			legend=c('current',paste('q',quantiles[1],sep=''),
          'q0.5',paste('q',quantiles[2],sep='')),
          lty=c(1,3,2,3),bty='n',lwd=2,
  		    col=c('red','darkgrey','darkgrey','darkgrey'),cex=cex.legend)

			if(save.figs==TRUE) dev.off()
      
  		if(save.figs==FALSE & !j==length(runs))
  			readline('Press return for next plot')
  		}

		if(save.figs==FALSE & !i==ncol(q1))
			readline('Press return for next plot')
		}
	}

par(old.par)
}
fragclass <-
function(path,inland,inclass,LID.path,scenarios=NULL,
  sessions=NULL,sessions.name='session',runs=NULL,runs.name='run',
  gridname,classes=NULL,metrics=NULL,var='srv50%',start.step=0,stop.step=NULL,
  outfile=FALSE){

#add option to select classes and metrics

#set defaults
options(warn=0)
	
#read fragstats data
y.land<-read.csv(paste(path,inland,sep=''),strip.white=TRUE,header=TRUE)
y.class<-read.csv(paste(path,inclass,sep=''),strip.white=TRUE,header=TRUE)

#parse LID name
t1<-gsub('\\',',',LID.path,fixed=TRUE)
write.table(t1,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
t1<-read.csv('temp.txt',header=FALSE)
t0<-length(t1)-1
t1<-gsub('\\',',',y.land$LID,fixed=TRUE)
write.table(t1,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
t1<-read.csv('temp.txt',header=FALSE)
t1<-t1[,-c(1:t0)]
t1<-t1[,c(1:3)]
names(t1)<-c('scenario','session.id','run.id')
t1$session.id<-sub(sessions.name,'',t1$session.id,fixed=TRUE)
t1$session.id<-as.numeric(t1$session.id)
t1$run.id<-sub(runs.name,'',t1$run.id,fixed=TRUE)
t1$run.id<-as.numeric(t1$run.id)

#parse grid names
t2<-sub(gridname,',grid',y.land$LID,fixed=TRUE)
write.table(t2,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
t2<-read.csv('temp.txt',header=FALSE)
names(t2)<-c('junk','grid')

#combine results
y.land<-cbind(t1,t2,y.land[,-1])
y.land<-y.land[,-4]

#parse y.class LID name
t1<-gsub('\\',',',y.class$LID,fixed=TRUE)
write.table(t1,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
t1<-read.csv('temp.txt',header=FALSE)
t1<-t1[,-c(1:t0)]
t1<-t1[,c(1:3)]
names(t1)<-c('scenario','session.id','run.id')
t1$session.id<-sub(sessions.name,'',t1$session.id,fixed=TRUE)
t1$session.id<-as.numeric(t1$session.id)
t1$run.id<-sub(runs.name,'',t1$run.id,fixed=TRUE)
t1$run.id<-as.numeric(t1$run.id)

#parse grid names
t2<-sub(gridname,',grid',y.class$LID,fixed=TRUE)
write.table(t2,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
t2<-read.csv('temp.txt',header=FALSE)
names(t2)<-c('junk','grid')

#combine results
y.class<-cbind(t1,t2,y.class[,-1])
y.class<-y.class[,-4]

#set scenarios parameter
if(is.null(scenarios)) scenarios<-as.vector(unique(y.land$scenario))

#verify valid scenarios
all.scenarios<-unique(y.land$scenario)
if(any(!scenarios %in% all.scenarios)) stop('Invalid scenarios')

#set sessions parameter
if(is.null(sessions)) sessions<-as.vector(unique(y.land$session.id))

#verify valid session ids
all.sessions<-unique(y.land$session.id)
if(any(!sessions %in% all.sessions)) stop('Invalid session ids')

#set runs parameter
if(is.null(runs)) runs<-unique(y.land$run.id)

#verify valid run ids
all.runs<-unique(y.land$run.id)
if(any(!runs %in% all.runs)) stop('Invalid run ids')

#select data
y.land<-y.land[y.land$scenario %in% scenarios & 
  y.land$session.id %in% sessions & y.land$run.id %in% runs,]
if(nrow(y.land)==0){
  stop('No observations given specified scenarios, sessions and runs')
  }
y.class<-y.class[y.class$scenario %in% scenarios & 
  y.class$session.id %in% sessions & y.class$run.id %in% runs,]
if(nrow(y.class)==0){
  stop('No observations given specified scenarios, sessions and runs')
  }

###create timestep variable in y.land
y.land$timestep<-0
t0<-unique(y.land[,1:2])
row.names(t0)<-NULL
t0$runs<-0
t0$start.step<-0
t0$stop.step<-0

#loop thru scenarios and sessions
for(j in 1:nrow(t0)){

	#select records for current scenario and session
	y1<-y.land[y.land$scenario==t0[j,1] & y.land$session.id==t0[j,2],1:3]

  #add timestep variable by run
  truns<-as.vector(unique(y1$run.id))
  t0[j,3]<-length(truns)
  tstep<-NULL
  for(k in truns){
	  t1<-sum(y1$run.id==k)
    tstep<-c(tstep,seq(0,t1-1)) 
    }
  y.land$timestep[y.land$scenario==t0[j,1] & y.land$session.id==t0[j,2]]<-tstep  
  }

#rearrange dataframe and drop variables 
y.land<-y.land[,c(1:4,ncol(y.land))]
  
#get unique covcond types
if(is.null(classes)) s1<-sort(as.vector(unique(y.class$TYPE)))
else s1<-classes

#merge fragstats class data with landscape timestep
t1<-merge(y.land,s1)
names(t1)<-c('scenario','session.id','run.id','grid','timestep','TYPE')
y<-merge(t1,y.class,by=c('scenario','session.id','run.id','grid','TYPE'),
  all.x=TRUE)

#ensure numeric variables
for(i in 7:ncol(y)){
  y[,i]<-as.numeric(as.character(y[,i]))
  }

#set metrics parameter
class.metrics<-colnames(y)[-c(1:6)]
if(is.null(metrics)) metrics<-class.metrics
if(any(!metrics %in% class.metrics)) stop('Invalid metrics selected')

#select columns
y.metrics<-subset(y,select=metrics)
y.head<-y[,1:6]
y<-cbind(y.head,y.metrics)

#create list objects for class results
r<-vector('list',length(s1))
names(r)<-s1

#loop thru class types
for(i in 1:length(s1)){ 

	#get class name
	class.name<-as.character(s1[i])

	#select class records
	q0<-y[y$TYPE==s1[i],]

	#create results files for multiple scenarios
	if(nrow(t0)>1){
		zz1<-as.data.frame(metrics)
  	zz2<-t0[,1:2]
  	row.names(zz2)<-NULL
    zz2$ldi<-NA
		}

	#loop thru scenarios and sessions
	for(j in 1:nrow(t0)){

	  #select records for current scenario and session
	  q1<-q0[q0$scenario==t0[j,1] & q0$session.id==t0[j,2],]
	
		#set file-dependent defaults
	  if(is.null(stop.step)){
			stopstep<-max(q1$timestep)
			}
		else{
	  	if(stop.step>max(q1$timestep)) 
	  		warning('Stop.step exceeds maximum timestep and will be set to the maximum')
	  	stopstep<-min(stop.step,max(q1$timestep))
	  	}
	  if(start.step>=stopstep) 
	  	stop('Start.step must be less than maximum timestep')
	  t0[j,4]<-start.step
	  t0[j,5]<-stopstep
	
		#calculate class metric SRV quantiles
		q1<-q1[,-c(1:5)]
		q2<-matrix(0,ncol(q1)-1,101)
		q3<-q1[q1$timestep>=start.step & q1$timestep<=stopstep,]
		for(k in 1:nrow(q2)){
			q3[,k+1]<-as.numeric(as.character(q3[,k+1]))
			q2[k,1:101]<-quantile(q3[,k+1],probs=seq(0,1,0.01),na.rm=TRUE)
			}
		
		#get current class metric (timestep 0)
		current.value<-t(q1[q1$timestep==0,-1])
		current.value<-current.value[,1]
		current.value<-round(as.numeric(current.value),3)
		
		#put most of it together
		z1<-round(as.data.frame(q2),3)
		z1<-cbind(metrics,z1)
		z1<-cbind(z1,current.value)
		temp<-matrix(0,nrow(z1),2)
		z1<-cbind(z1,temp)
		
		#calculate srv.cv and current class metric %SRV
		for(k in 1:nrow(z1)){
			if(z1[k,102]-z1[k,2]==0 || is.na(z1[k,103])) z1[k,104]<-'NA'
			else{
				z1[k,105]<-round(((z1[k,97]-z1[k,7])/z1[k,52])*100,0)			
				z1[k,104]<-max(0,which(q2[k,]<z1[k,103]))		
				if(z1[k,104]==101) z1[k,104]<-100
				}
			if(z1[k,102]-z1[k,2]==0) z1[k,105]<-'NA'
			else{
				z1[k,105]<-round(((z1[k,97]-z1[k,7])/z1[k,52])*100,0)			
				}
			}

		#compute departure index
		t1<-rep(0,nrow(z1))
		z1<-cbind(z1,t1)
		z1[,104]<-as.numeric(z1[,104])
		for(k in 1:nrow(z1)){ 		
			if(is.na(z1[k,104])) z1[k,106]<-'NA'
			else{
				if(z1[k,104]!=50) z1[k,106]<-round((z1[k,104]-50)/50*100,0) # column 104 is current value, column 106 is departure index				
				#else if(z1[k,104]>50) z1[k,106]<-round((z1[k,104]-50)/50*100,0) # I removed this
				else z1[k,106]<-0
				}
			}
		z1[,106]<-as.numeric(z1[,106])
		z2<-mean(abs(z1[,106]),na.rm=TRUE)
		z2<-round(z2,0)
		
		
		#put the final table together
		z1<-z1[,c(1,2,7,27,52,77,97,102,105,103,104,106)]
		colnames(z1)<-c('class.metric','srv0%','srv5%','srv25%',
			'srv50%','srv75%','srv95%','srv100%','srv.cv',
			'current.value','current.%SRV','departure.index')
	
  	#merge selected covcond stat to final results table
  	if(nrow(t0)>1){
  		temp<-subset(z1,select=var)
  		names(temp)<-paste(var,'.',t0[j,1],'.',t0[j,2],sep='')
  		zz1<-cbind(zz1,temp)
   		}
		
		#add departure index result to final results table
		if(nrow(t0)>1) zz2[j,3]<-z2
	
		} #end loop thru scenarios and sessions
		
	#add results to final list object
	if(nrow(t0)>1) r[[i]]<-list(zz1,zz2)
	else r[[i]]<-list(z1,z2)
	names(r[[i]])<-c('class SRV','class departure index (%)')

	#print results to console
	print(class.name)
	print(r[[i]])

	#output tables to file
	if(outfile==TRUE){
		write(class.name,file=paste(paste(path,inclass,sep=''),'srv.csv',sep='.'),
		  append=TRUE)
		write.table(r[[i]][[1]],file=paste(paste(path,inclass,sep=''),
		  'srv.csv',sep='.'),quote=FALSE,sep=',',row.names=FALSE,append=TRUE)
		write(class.name,file=paste(paste(path,inclass,sep=''),'cdi.csv',sep='.'),
		  append=TRUE)
		write.table(r[[i]][[2]],file=paste(paste(path,inclass,sep=''),
		  'cdi.csv',sep='.'),quote=FALSE,sep=',',row.names=FALSE,append=TRUE)
		}

	if(!i==length(s1))
		readline('Press return for next table')

	}

#create list object
z<-list(inland,inclass,t0,r)
names(z)<-c('inland','inclass','run stats','class statistics')
invisible(z)
}
fragclass.plot <-
function(path,inland,inclass,LID.path,scenarios=NULL,
  sessions=NULL,sessions.name='session',runs=NULL,runs.name='run',
  classes=NULL,metrics=NULL,gridname,start.step=0,stop.step=NULL,
  step.length=NULL,quantiles=c(0.05,0.95),col.line='dark blue',
  col.sub='brown',cex.main=1.5,cex.sub=1.5,cex.legend=1.5,cex.lab=1.5,
  save.figs=FALSE,...){

#set defaults
options(warn=0)
old.par<-par(no.readonly=TRUE)

#read fragstats data
y.land<-read.csv(paste(path,inland,sep=''),strip.white=TRUE,header=TRUE)
y.class<-read.csv(paste(path,inclass,sep=''),strip.white=TRUE,header=TRUE)

#parse LID name
t1<-gsub('\\',',',LID.path,fixed=TRUE)
write.table(t1,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
t1<-read.csv('temp.txt',header=FALSE)
t0<-length(t1)-1
t1<-gsub('\\',',',y.land$LID,fixed=TRUE)
write.table(t1,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
t1<-read.csv('temp.txt',header=FALSE)
t1<-t1[,-c(1:t0)]
t1<-t1[,c(1:3)]
names(t1)<-c('scenario','session.id','run.id')
t1$session.id<-sub(sessions.name,'',t1$session.id,fixed=TRUE)
t1$session.id<-as.numeric(t1$session.id)
t1$run.id<-sub(runs.name,'',t1$run.id,fixed=TRUE)
t1$run.id<-as.numeric(t1$run.id)

#parse grid names
t2<-sub(gridname,',grid',y.land$LID,fixed=TRUE)
write.table(t2,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
t2<-read.csv('temp.txt',header=FALSE)
names(t2)<-c('junk','grid')

#combine results
y.land<-cbind(t1,t2,y.land[,-1])
y.land<-y.land[,-4]

#parse y.class LID name
t1<-gsub('\\',',',y.class$LID,fixed=TRUE)
write.table(t1,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
t1<-read.csv('temp.txt',header=FALSE)
t1<-t1[,-c(1:t0)]
t1<-t1[,c(1:3)]
names(t1)<-c('scenario','session.id','run.id')
t1$session.id<-sub(sessions.name,'',t1$session.id,fixed=TRUE)
t1$session.id<-as.numeric(t1$session.id)
t1$run.id<-sub(runs.name,'',t1$run.id,fixed=TRUE)
t1$run.id<-as.numeric(t1$run.id)

#parse grid names
t2<-sub(gridname,',grid',y.class$LID,fixed=TRUE)
write.table(t2,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
t2<-read.csv('temp.txt',header=FALSE)
names(t2)<-c('junk','grid')

#combine results
y.class<-cbind(t1,t2,y.class[,-1])
y.class<-y.class[,-4]

#set scenarios parameter
if(is.null(scenarios)) scenarios<-as.vector(unique(y.land$scenario))

#verify valid scenarios
all.scenarios<-unique(y.land$scenario)
if(any(!scenarios %in% all.scenarios)) stop('Invalid scenarios')

#set sessions parameter
if(is.null(sessions)) sessions<-as.vector(unique(y.land$session.id))

#verify valid session ids
all.sessions<-unique(y.land$session.id)
if(any(!sessions %in% all.sessions)) stop('Invalid session ids')

#set runs parameter
if(is.null(runs)) runs<-unique(y.land$run.id)

#verify valid run ids
all.runs<-unique(y.land$run.id)
if(any(!runs %in% all.runs)) stop('Invalid run ids')

#select data
y.land<-y.land[y.land$scenario %in% scenarios & 
  y.land$session.id %in% sessions & y.land$run.id %in% runs,]
if(nrow(y.land)==0){
  stop('No observations given specified scenarios, sessions and runs')
}
y.class<-y.class[y.class$scenario %in% scenarios & 
  y.class$session.id %in% sessions & y.class$run.id %in% runs,]
if(nrow(y.class)==0){
  stop('No observations given specified scenarios, sessions and runs')
}

###create timestep variable in y.land
y.land$timestep<-0
t0<-unique(y.land[,1:2])
row.names(t0)<-NULL
t0$runs<-0
t0$start.step<-0
t0$stop.step<-0

#loop thru scenarios and sessions
for(j in 1:nrow(t0)){
  
  #select records for current scenario and session
  y1<-y.land[y.land$scenario==t0[j,1] & y.land$session.id==t0[j,2],1:3]
  
  #add timestep variable by run
  truns<-as.vector(unique(y1$run.id))
  t0[j,3]<-length(truns)
  tstep<-NULL
  for(k in truns){
    t1<-sum(y1$run.id==k)
    tstep<-c(tstep,seq(0,t1-1)) 
  }
  y.land$timestep[y.land$scenario==t0[j,1] & y.land$session.id==t0[j,2]]<-tstep  
}

#rearrange dataframe and drop variables 
y.land<-y.land[,c(1:4,ncol(y.land))]

#get unique covcond types
if(is.null(classes)) s1<-sort(as.vector(unique(y.class$TYPE)))
else s1<-classes

#merge fragstats class data with landscape timestep
t1<-merge(y.land,s1)
names(t1)<-c('scenario','session.id','run.id','grid','timestep','TYPE')
y<-merge(t1,y.class,by=c('scenario','session.id','run.id','grid','TYPE'),
         all.x=TRUE)

#ensure numeric variables
for(i in 7:ncol(y)){
  y[,i]<-as.numeric(as.character(y[,i]))
}

#set metrics parameter
class.metrics<-colnames(y)[-c(1:6)]
if(is.null(metrics)) metrics<-class.metrics
if(any(!metrics %in% class.metrics)) stop('Invalid metrics selected')

#select columns
y.metrics<-subset(y,select=metrics)
y.head<-y[,1:6]
y<-cbind(y.head,y.metrics)

#set file-dependent defaults
if(is.null(stop.step)){
  stopstep<-max(y$timestep)
  }
else{
	if(stop.step>max(y$timestep)) 
		warning('Stop.step exceeds maximum timestep and will be set to the maximum')
	stopstep<-min(stop.step,max(y$timestep))
	}
if(start.step>=stopstep) 
	stop('Start.step must be less than maximum timestep')

#select records
q1<-y[y$timestep>=start.step & y$timestep<=stopstep,]

#reestablish runs
runs<-sort(as.vector(unique(q1$run.id)))

###for multiple scenarios and sessions
if(nrow(t0)>1){

  #loop thru covcond classes (type)
  for(m in 1:length(s1)){

    #select data for covcond class
    q2<-q1[q1$TYPE==s1[m],]

  	#loop thru class metrics
  	for(i in 7:ncol(q2)){ 

      #establish plot limits
      ymin<-min(q2[,i],na.rm=TRUE)
      ymax<-max(q2[,i],na.rm=TRUE)
      yrange<-ymax-ymin
	
      #loop thru runs
      for(j in 1:length(runs)){

        #select data for run
        q3<-q2[q2$run.id==runs[j],]
  			q3<-q3[order(q3$timestep),]
			
        #create blank plot
        if(is.null(step.length)) xlab='Timestep'
        else xlab=paste('Timestep (x',step.length,' yrs)')
        
  			#plot to file
  			if(save.figs==TRUE){
  			  bitmap(file=paste(names(q3)[i],runs[j],'.',s1[m],'.png',sep=''),
  			         height=6,width=8,res=300,...) 
  			}
        
        #create plot
        plot(1,1,type='n',xlim=c(start.step,stopstep),xaxs='i',
          ylim=c(ymin-(yrange*0.01),ymax+(yrange*0.1)),
          xlab=xlab,ylab=names(q3[i]),main='',cex.lab=cex.lab,...)

    		#loop thru scenarios and sessions
    		for(k in 1:nrow(t0)){
    	
    			#plot scenarios and sessions
    			q4<-q3[q3$scenario==t0[k,1] & q3$session.id==t0[k,2],]
          if(!nrow(q4)==0) lines(q4[,6],q4[,i],lty=k,lwd=2,col=k)
    
    			}
   	
    		#add legend
    		legend(x='top',ncol=3,
    			legend=paste(t0[,1],t0[,2],sep='.'),
            lty=seq(1,nrow(t0)),bty='n',lwd=2,
            col=c(seq(1,nrow(t0))),cex=cex.legend)
    		
    		#add plot title				
    		title(main=paste('Class Metric Trajectory',
          ' (',names(q4)[i],')',sep=''),line=2.5,cex.main=cex.main,...)
  
   			#add subtitle
  			mtext(side=3,line=1,col=col.sub,cex=cex.sub,
          text=paste(s1[m],': Run #',runs[j],sep=''))
        
  			if(save.figs==TRUE) dev.off()
  			    	
    		if(save.figs==FALSE & !j==length(runs))
    			readline('Press return for next plot')
      	}
  
   		if(save.figs==FALSE & !i==ncol(q2))
   			readline('Press return for next plot')
      }

 		if(save.figs==FALSE & !m==length(s1))
 			readline('Press return for next plot')
    }
  }


###for single scenario and session
else{

  #loop thru covcond classes (type)
  for(m in 1:length(s1)){

    #select data for covcond class
    q2<-q1[q1$TYPE==s1[m],]

  	#loop thru class metrics
  	for(i in 7:ncol(q2)){ 

      #establish plot limits
      ymin<-min(min(q2[,i],na.rm=TRUE),
        y[y$timestep==0 & y$TYPE==s1[m],i],na.rm=TRUE)
      ymax<-max(max(q2[,i],na.rm=TRUE),
        y[y$timestep==0 & y$TYPE==s1[m],i],na.rm=TRUE)
      yrange<-ymax-ymin	
      
      #loop thru runs
      for(j in 1:length(runs)){

        #select data for run
        q3<-q2[q2$run.id==runs[j],]
  			q3<-q3[order(q3$timestep),]

  			#plot metric trajectory
        if(is.null(step.length)) xlab='Timestep'
        else xlab=paste('Timestep (x',step.length,' yrs)')
        
  			#plot to file
  			if(save.figs==TRUE){
  			  bitmap(file=paste(names(q3)[i],runs[j],'.',s1[m],'.png',sep=''),
  			         height=6,width=8,res=300,...) 
  			}
  			
  			#create plot 
    		plot(q3[,6],q3[,i],type='l',lwd=2,col=col.line,
  				ylim=c(ymin-(yrange*0.01),ymax+(yrange*0.1)),
  				xaxs='i',xlab=xlab,ylab=names(q3[i]),cex.lab=cex.lab,...)

    		#add current condition and %SRV lines	
    		abline(h=y[y$timestep==0 & y$TYPE==s1[m] & y$run.id==runs[j],i],
          lty=1,lwd=2,col='red')
    		abline(h=quantile(q3[,i],0.5,na.rm=TRUE),lwd=2,lty=2,col='darkgrey')
    		abline(h=quantile(q3[,i],quantiles[1],na.rm=TRUE),lwd=2,lty=3,
          col='darkgrey')
    		abline(h=quantile(q3[,i],quantiles[2],na.rm=TRUE),lwd=2,lty=3,
          col='darkgrey')
 
    		#add legend
    		legend(x='top',horiz=TRUE,
    			legend=c('current',paste('q',quantiles[1],sep=''),
            'q0.5',paste('q',quantiles[2],sep='')),
            lty=c(1,3,2,3),bty='n',lwd=2,
            col=c('red','darkgrey','darkgrey','darkgrey'),cex=cex.legend)
 			
  			#add plot title				
  			title(main=paste('Class Metric Trajectory',' (',names(q3[i]),')',
          sep=''),line=2.5,cex.main=cex.main)
  
   			#add subtitle
  			mtext(side=3,line=1,col=col.sub,cex=cex.sub,
          text=paste(s1[m],': Run #',runs[j],sep=''))

  			if(save.figs==TRUE) dev.off()
  			
    		if(save.figs==FALSE & !j==length(runs))
    			readline('Press return for next plot')
      	}
  
   		if(save.figs==FALSE & !i==ncol(q2))
   			readline('Press return for next plot')
      }

 		if(save.figs==FALSE & !m==length(s1))
 			readline('Press return for next plot')
    }
  }
par(old.par)
}
fragclass.pdf.plot <-
function(path,inland,inclass,LID.path,scenarios=NULL,
  sessions=NULL,sessions.name='session',runs=NULL,runs.name='run',
  pool.runs=TRUE,gridname,classes=NULL,metrics=NULL,current=TRUE,
  start.step=0,stop.step=NULL,
  ref.scenario=NULL,ref.session=NULL,ref.tstep=NULL,ref.include=TRUE,
  quantiles=c(0.05,0.95),col.line='dark blue',col.sub='brown',
  cex.main=1.5,cex.sub=1.25,cex.legend=1.25,cex.lab=1.5,
  outfile=FALSE,save.figs=FALSE,...){

#set defaults
options(warn=0)
old.par<-par(no.readonly=TRUE)

#read fragstats data
y.land<-read.csv(paste(path,inland,sep=''),strip.white=TRUE,header=TRUE)
y.class<-read.csv(paste(path,inclass,sep=''),strip.white=TRUE,header=TRUE)

#parse LID name
t1<-gsub('\\',',',LID.path,fixed=TRUE)
write.table(t1,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
t1<-read.csv('temp.txt',header=FALSE)
t0<-length(t1)-1
t1<-gsub('\\',',',y.land$LID,fixed=TRUE)
write.table(t1,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
t1<-read.csv('temp.txt',header=FALSE)
t1<-t1[,-c(1:t0)]
t1<-t1[,c(1:3)]
names(t1)<-c('scenario','session.id','run.id')
t1$session.id<-sub(sessions.name,'',t1$session.id,fixed=TRUE)
t1$session.id<-as.numeric(t1$session.id)
t1$run.id<-sub(runs.name,'',t1$run.id,fixed=TRUE)
t1$run.id<-as.numeric(t1$run.id)

#parse grid names
t2<-sub(gridname,',grid',y.land$LID,fixed=TRUE)
write.table(t2,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
t2<-read.csv('temp.txt',header=FALSE)
names(t2)<-c('junk','grid')

#combine results
y.land<-cbind(t1,t2,y.land[,-1])
y.land<-y.land[,-4]

#parse y.class LID name
t1<-gsub('\\',',',y.class$LID,fixed=TRUE)
write.table(t1,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
t1<-read.csv('temp.txt',header=FALSE)
t1<-t1[,-c(1:t0)]
t1<-t1[,c(1:3)]
names(t1)<-c('scenario','session.id','run.id')
t1$session.id<-sub(sessions.name,'',t1$session.id,fixed=TRUE)
t1$session.id<-as.numeric(t1$session.id)
t1$run.id<-sub(runs.name,'',t1$run.id,fixed=TRUE)
t1$run.id<-as.numeric(t1$run.id)

#parse grid names
t2<-sub(gridname,',grid',y.class$LID,fixed=TRUE)
write.table(t2,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
t2<-read.csv('temp.txt',header=FALSE)
names(t2)<-c('junk','grid')

#combine results
y.class<-cbind(t1,t2,y.class[,-1])
y.class<-y.class[,-4]

#set scenarios parameter
if(is.null(scenarios)) scenarios<-as.vector(unique(y.land$scenario))

#verify valid scenarios
all.scenarios<-unique(y.land$scenario)
if(any(!scenarios %in% all.scenarios)) stop('Invalid scenarios')

#set sessions parameter
if(is.null(sessions)) sessions<-as.vector(unique(y.land$session.id))

#verify valid session ids
all.sessions<-unique(y.land$session.id)
if(any(!sessions %in% all.sessions)) stop('Invalid session ids')

#set runs parameter
if(is.null(runs)) runs<-unique(y.land$run.id)

#verify valid run ids
all.runs<-unique(y.land$run.id)
if(any(!runs %in% all.runs)) stop('Invalid run ids')

#select data
y.land<-y.land[y.land$scenario %in% scenarios & 
                 y.land$session.id %in% sessions & y.land$run.id %in% runs,]
if(nrow(y.land)==0){
  stop('No observations given specified scenarios, sessions and runs')
}
y.class<-y.class[y.class$scenario %in% scenarios & 
                   y.class$session.id %in% sessions & y.class$run.id %in% runs,]
if(nrow(y.class)==0){
  stop('No observations given specified scenarios, sessions and runs')
}

###create timestep variable in y.land
y.land$timestep<-0
t0<-unique(y.land[,1:2])
row.names(t0)<-NULL
t0$runs<-0
t0$start.step<-0
t0$stop.step<-0

#loop thru scenarios and sessions
for(j in 1:nrow(t0)){
  
  #select records for current scenario and session
  y1<-y.land[y.land$scenario==t0[j,1] & y.land$session.id==t0[j,2],1:3]
  
  #add timestep variable by run
  truns<-as.vector(unique(y1$run.id))
  t0[j,3]<-length(truns)
  tstep<-NULL
  for(k in truns){
    t1<-sum(y1$run.id==k)
    tstep<-c(tstep,seq(0,t1-1)) 
  }
  y.land$timestep[y.land$scenario==t0[j,1] & y.land$session.id==t0[j,2]]<-tstep  
}

#rearrange dataframe and drop variables 
y.land<-y.land[,c(1:4,ncol(y.land))]

#get unique covcond types
if(is.null(classes)) s1<-sort(as.vector(unique(y.class$TYPE)))
else s1<-classes

#merge fragstats class data with landscape timestep
t1<-merge(y.land,s1)
names(t1)<-c('scenario','session.id','run.id','grid','timestep','TYPE')
y<-merge(t1,y.class,by=c('scenario','session.id','run.id','grid','TYPE'),
  all.x=TRUE)

#ensure numeric variables
for(i in 7:ncol(y)){
  y[,i]<-as.numeric(as.character(y[,i]))
}

#set metrics parameter
class.metrics<-colnames(y)[-c(1:6)]
if(is.null(metrics)) metrics<-class.metrics
if(any(!metrics %in% class.metrics)) stop('Invalid metrics selected')

#select columns
y.metrics<-subset(y,select=metrics)
y.head<-y[,1:6]
y<-cbind(y.head,y.metrics)

#set file-dependent defaults
if(is.null(stop.step)){
  stopstep<-max(y$timestep)
}
else{
  if(stop.step>max(y$timestep)) 
    warning('Stop.step exceeds maximum timestep and will be set to the maximum')
  stopstep<-min(stop.step,max(y$timestep))
}
if(start.step>=stopstep) 
  stop('Start.step must be less than maximum timestep')

####for reference scenario and session comparison
if(!is.null(ref.scenario) & !is.null(ref.session)){

  if(is.null(ref.tstep)){
    stop('Missing timestep (tstep) for alternative scenarios and sessions')
    }

  #copy data for consistency with script below
  q1<-y

  #create reference and alternative scenarios and sessions
  t1<-t0[t0$scenario==ref.scenario & t0$session.id==ref.session,]
  t2<-t0[!t0$scenario==ref.scenario | !t0$session.id==ref.session,]    
 
  #loop thru covcond classes (type)
  for(m in 1:length(s1)){

    #get class name
    class.name<-as.character(s1[m])

    #select data for covcond class
    q2<-q1[q1$TYPE==s1[m],]

    #order data
		q3<-q2[order(q2$timestep),]

    #create results table for departure index
    if(ref.include==TRUE & current==TRUE){
      z<-as.data.frame(matrix(NA,nrow=ncol(q3)-5,ncol=nrow(t0)+2))
      z[,1]<-c(names(q3[,-c(1:6)]),'mean')
      names(z)<-c('metric','current',paste(t0[,1],t0[,2],sep='.'))
      }
    else if(ref.include==TRUE & current==FALSE){
      z<-as.data.frame(matrix(NA,nrow=ncol(q3)-5,ncol=nrow(t0)+1))
      z[,1]<-c(names(q3[,-c(1:6)]),'mean')
      names(z)<-c('metric',paste(t0[,1],t0[,2],sep='.'))
      }    
    else if(ref.include==FALSE & current==TRUE){
      z<-as.data.frame(matrix(NA,nrow=ncol(q3)-5,ncol=nrow(t2)+2))
      z[,1]<-c(names(q3[,-c(1:6)]),'mean')
      names(z)<-c('metric','current',paste(t2[,1],t2[,2],sep='.'))  
      }
    else if(ref.include==FALSE & current==FALSE){
      z<-as.data.frame(matrix(NA,nrow=ncol(q3)-5,ncol=nrow(t2)+1))
      z[,1]<-c(names(q3[,-c(1:6)]),'mean')
      names(z)<-c('metric',paste(t2[,1],t2[,2],sep='.'))  
  		}
 
  	#loop thru class metrics
  	for(i in 7:ncol(q3)){ 

      #establish plot limits
      if(current==TRUE){
        xmin<-min(q3[,i],na.rm=TRUE)
        xmax<-max(q3[,i],na.rm=TRUE)
        }
      else{
        xmin<-min(q3[q3$timestep>=start.step & q3$timestep<=stopstep,i],
          q3[q2$timestep==ref.tstep,i],na.rm=TRUE)
        xmax<-max(q3[q3$timestep>=start.step & q3$timestep<=stopstep,i],
          q3[q3$timestep==ref.tstep,i],na.rm=TRUE)
        }      
      xrange<-xmax-xmin
   
      #plot to file
      if(save.figs==TRUE){
        bitmap(file=paste(names(q3)[i],'.',s1[m],'.png',sep=''),
               height=6,width=8,res=300,...) 
      }
      
      #create blank plot
      plot(1,1,type='n',xlim=c(xmin-(xrange*0.03),xmax+(xrange*0.03)),
        ylim=c(0,1.3),xaxs='i',xlab=names(q3[i]),ylab='Density',main='',
        cex.lab=cex.lab,...)

  		#plot reference scenario and session
      q4<-q3[q3$scenario==t1[1,1] & q3$session.id==t1[1,2] & 
        q3$timestep>=start.step & q3$timestep<=stopstep,]

      if(nrow(q4)==0){
        stop('No data given specified arguments')
        }
      else if(nrow(q4)==1){
        segments(q4[,i],0,q4[,i],1,lwd=2,lty=1,col=1)     
        }
      else{
        temp<-density(q4[,i],from=xmin,to=xmax,na.rm=TRUE)
        lines(temp$x,temp$y/max(temp$y),lwd=2,lty=1,col=1)  
        }
      
      #compute landscape departure index for current
      if(current==TRUE){
        z[i-6,2]<-departure(ref=q4[,i],alt=q3[q3$timestep==0,i][1])
        }
        
      #plot reference scenario and session at specified ref.tstep
      if(ref.include==TRUE){
        q5<-q3[q3$scenario==t1[1,1] & q3$session.id==t1[1,2] & 
          q3$timestep==ref.tstep,]

        if(nrow(q5)==0){
          stop('No reference data at specified ref.tstep')
          }
        else if(nrow(q5)==1){
          segments(q5[,i],0,q5[,i],1,lwd=2,lty=2,col=1)     
          }
        else{
          temp<-density(q5[,i],from=xmin,to=xmax,na.rm=TRUE)
          lines(temp$x,temp$y/max(temp$y),lwd=2,lty=2,col=1)  
          }
        
        #compute landscape departure index
        if(current==TRUE){
          z[i-6,3]<-departure(ref=q4[,i],alt=q5[,i])
          }
        else{
          z[i-6,2]<-departure(ref=q4[,i],alt=q5[,i])
          }
        }
        
      #loop thru alternative scenarios and sessions
  		for(k in 1:nrow(t2)){
  	
  			#plot scenarios and sessions
  			q6<-q3[q3$scenario==t2[k,1] & q3$session.id==t2[k,2] & 
          q3$timestep==ref.tstep,]

  			if(nrow(q6)==0){
  			  stop('No alternatives given specified arguments')
  			  }
  			else if(nrow(q6)==1){
  			  segments(q6[,i],0,q6[,i],1,lwd=2,lty=k+2,col=k+1)  
  			  }
  			else{
  			  temp<-density(q6[,i],from=xmin,to=xmax,na.rm=TRUE)
  			  lines(temp$x,temp$y/max(temp$y),lwd=2,lty=k+2,col=k+1)  
  			  }
        
        #compute landscape departure index
        if(ref.include==TRUE & current==TRUE){
          z[i-6,k+3]<-departure(ref=q4[,i],alt=q6[,i])
          }
        else if(ref.include==TRUE & current==FALSE){
          z[i-6,k+2]<-departure(ref=q4[,i],alt=q6[,i])
          }
        else if(ref.include==FALSE & current==TRUE){
          z[i-6,k+2]<-departure(ref=q4[,i],alt=q6[,i])
          }
        else if(ref.include==FALSE & current==FALSE){
          z[i-6,k+1]<-departure(ref=q4[,i],alt=q6[,i])
          }
  			}
  
  		#add current condition	
  		if(current==TRUE){
        segments(q3[q3$timestep==0,i][1],0,q3[q3$timestep==0,i][1],1,
          lwd=2,lty=1,col='red')
        }
        
  		#add legend
      if(current==TRUE){
    		legend(x='topleft',ncol=1,legend='current',bty='n',
          lty=1,lwd=2,col='red',cex=cex.legend,title='Current')
        }
      #reference scenario
    	legend(x='top',ncol=1,legend=c(paste(t1[,1],t1[,2],sep='.')),bty='n',
        lty=1,lwd=2,col=1,cex=cex.legend,title='Reference')
      
      #alternative scenarios  
      if(ref.include==TRUE){
    		legend(x='topright',ncol=1,legend=c(paste(t0[,1],t0[,2],sep='.')),bty='n',
          lty=seq(2,nrow(t0)+1),lwd=2,col=seq(1,nrow(t0)),cex=cex.legend,
          title='Alternatives')
  			}
      else{
    		legend(x='topright',ncol=1,legend=c(paste(t2[,1],t2[,2],sep='.')),bty='n',
          lty=seq(3,nrow(t2)+3),lwd=2,col=seq(2,nrow(t2)+2),cex=cex.legend,
          title='Alternatives')
        }    
  		
      #add plot title				
  		title(main=paste('Landscape Metric Density',
        ' (',names(q3)[i],')',sep=''),line=2.5,cex.main=cex.main,...)
  
  		#add subtitle
			mtext(side=3,line=1.25,col=col.sub,cex=cex.sub,text=s1[m],...)
  		mtext(side=3,line=.2,col=col.sub,cex=cex.sub,
        text=paste('Comparison of reference scenario to timestep ',
          ref.tstep,' of the alternatives',sep=''),...)
  
      #add horizontal line at zero
      abline(h=0,col='gray')
      
			if(save.figs==TRUE) dev.off()

      if(save.figs==FALSE & !i==ncol(q3))
   			readline('Press return for next plot')

      } #end loop thru metrics

    #compute total class departure index
    z[ncol(q3)-5,-1]<-apply(as.data.frame(z[,-1]),2,mean,na.rm=TRUE)
      
    #print class departure index table
    print(paste('Class departure index table: ',s1[m],sep=''))
    z[,-1]<-round(z[,-1],0)
    print(z)  
  
    #output tables to file
    if(outfile==TRUE){
  		write(class.name,file=paste(paste(path,inclass,sep=''),'ref',
        'cdi.csv',sep='.'),append=TRUE)
      write.table(z,file=paste(paste(path,inclass,sep=''),'ref',
        'cdi.csv',sep='.'),quote=FALSE,row.names=FALSE,append=TRUE,sep=',')
    	}

 		if(save.figs==FALSE & !m==length(s1))
 			readline('Press return for next plot')

    } #end loop thru covcond types

  } #end for multiple scenarios and sessions with a reference


###for multiple scenarios and sessions without a reference
else if(nrow(t0)>1){

  #select data
  q1<-y[y$timestep>=start.step & y$timestep<=stopstep,]

  #reestablish runs
  runs<-sort(as.vector(unique(q1$run.id)))

  #loop thru covcond classes (type)
  for(m in 1:length(s1)){

    #select data for covcond class
    q2<-q1[q1$TYPE==s1[m],]

  	#loop thru class metrics
  	for(i in 7:ncol(q2)){ 

      #establish plot limits
      if(current==TRUE){
        xmin<-min(q2[,i],y[y$timestep==0 & y$TYPE==s1[m],i],na.rm=TRUE)
        xmax<-max(q2[,i],y[y$timestep==0 & y$TYPE==s1[m],i],na.rm=TRUE)
        }
      else{
        xmin<-min(q2[,i],na.rm=TRUE)
        xmax<-max(q2[,i],na.rm=TRUE)
        }      
      xrange<-xmax-xmin

      #for pooled across runs
      if(pool.runs==TRUE){
          
        #order data
  			q3<-q2[order(q2$timestep),]
        
  			#plot to file
  			if(save.figs==TRUE){
  			  bitmap(file=paste(names(q3)[i],'.',s1[m],'.png',sep=''),
  			         height=6,width=8,res=300,...) 
  			}
  			
        #create blank plot
        plot(1,1,type='n',xlim=c(xmin-(xrange*0.03),xmax+(xrange*0.03)),
        ylim=c(0,1.1),xaxs='i',xlab=names(q3[i]),ylab='Density',main='',
        cex.lab=cex.lab,...)
  
  			#loop thru scenarios
    		for(k in 1:nrow(t0)){
    	
    			#plot scenarios and sessions
    			q4<-q3[q3$scenario==t0[k,1] & q3$session.id==t0[k,2],]
          if(!nrow(q4)==0){ 
            temp<-density(q4[,i],from=xmin,to=xmax,na.rm=TRUE)
            lines(temp$x,temp$y/max(temp$y),lty=k,lwd=2,col=k)
            }
    			
          #add current condition
    			if(current==TRUE){
    			  segments(y[y$scenario==t0[k,1] & y$session.id==t0[k,2] &
              y$timestep==0 & y$TYPE==s1[m],i],0,
    			    y[y$scenario==t0[k,1] & y$session.id==t0[k,2] &
              y$timestep==0 & y$TYPE==s1[m],i],1,
    			    lwd=2,lty=1,col='darkgrey')
    			}
    			
    		}
	
        #add legend
        if(current==TRUE){
      		legend(x='topleft',ncol=3,
      			legend=c('current',paste(t0[,1],t0[,2],sep='.')),
            lty=c(1,seq(1,nrow(t0))),bty='n',lwd=2,
            col=c('darkgrey',seq(1,nrow(t0))),cex=cex.legend)
          }
        else{  
      		legend(x='topleft',ncol=3,
      			legend=paste(t0[,1],t0[,2],sep='.'),
            lty=seq(1,nrow(t0)),bty='n',lwd=2,
            col=c(seq(1,nrow(t0))),cex=cex.legend)
    		  }
    		  
    		#add plot title				
    		title(main=paste('Class Metric Trajectory',
          ' (',names(q3)[i],')',sep=''),line=2.5,cex.main=cex.main,...)
  
   			#add subtitle
  			mtext(side=3,line=1,col=col.sub,cex=cex.sub,
          text=paste(s1[m],': Pooled across runs',sep=''))
  
        #add horizontal line at zero
        abline(h=0,col='gray')
        
  			if(save.figs==TRUE) dev.off()
    		
        } #end for pooled across runs

      #for separate plot per run
      else{

        #loop thru runs
        for(j in 1:length(runs)){
  
          #select data for run
          q3<-q2[q2$run.id==runs[j],]
    			q3<-q3[order(q3$timestep),]
          
    			#plot to file
    			if(save.figs==TRUE){
    			  bitmap(file=paste(names(q3)[i],runs[j],'.',s1[m],'.png',sep=''),
    			         height=6,width=8,res=300,...) 
    			}
    			        
          #create blank plot
          plot(1,1,type='n',xlim=c(xmin-(xrange*0.03),xmax+(xrange*0.03)),
          ylim=c(0,1.1),xaxs='i',xlab=names(q3[i]),ylab='Density',main='',
          cex.lab=cex.lab,...)
    
    			#loop thru scenarios
      		for(k in 1:nrow(t0)){
      	
      			#plot scenarios and sessions
      			q4<-q3[q3$scenario==t0[k,1] & q3$session.id==t0[k,2],]
            if(!nrow(q4)==0){ 
              temp<-density(q4[,i],from=xmin,to=xmax,na.rm=TRUE)
              lines(temp$x,temp$y/max(temp$y),lty=k,lwd=2,col=k)
              }
            
      			#add current condition
      			if(current==TRUE){
      			  segments(y[y$scenario==t0[k,1] & y$session.id==t0[k,2] & 
                y$timestep==0 & y$TYPE==s1[m],i],0,
		            y[y$scenario==t0[k,1] & y$session.id==t0[k,2] & 
                y$timestep==0 & y$TYPE==s1[m],i],1,
		            lwd=2,lty=1,col='darkgrey')
      			}
      			
      		}
      		
          #add legend
          if(current==TRUE){
        		legend(x='topleft',ncol=3,
        			legend=c('current',paste(t0[,1],t0[,2],sep='.')),
              lty=c(1,seq(1,nrow(t0))),bty='n',lwd=2,
              col=c('darkgrey',seq(1,nrow(t0))),cex=cex.legend)
            }
          else{  
        		legend(x='topleft',ncol=3,
        			legend=paste(t0[,1],t0[,2],sep='.'),
              lty=seq(1,nrow(t0)),bty='n',lwd=2,
              col=c(seq(1,nrow(t0))),cex=cex.legend)
      		  }
      		  
      		#add plot title				
      		title(main=paste('Class Metric Trajectory',
            ' (',names(q4)[i],')',sep=''),line=2.5,cex.main=cex.main,...)
    
     			#add subtitle
    			mtext(side=3,line=1,col=col.sub,cex=cex.sub,
            text=paste(s1[m],': Run #',runs[j],sep=''))
    
          #add horizontal line at zero
          abline(h=0,col='gray')
          
    			if(save.figs==TRUE) dev.off()
    			    		
       		if(save.figs==FALSE & !j==length(runs))
      			readline('Press return for next plot')

          } #end loop thru runs
          
        } #end for separate plot per run 
    
   		if(save.figs==FALSE & !i==ncol(q2))
   			readline('Press return for next plot')

      } #end loop thru metrics

 		if(save.figs==FALSE & !m==length(s1))
 			readline('Press return for next plot')

    } #end loop thru covcond types

  } #end for multiple scenarios and sessions without a reference


###for single scenario and session
else{

  #select data
  q1<-y[y$timestep>=start.step & y$timestep<=stopstep,]

  #reestablish runs
  runs<-sort(as.vector(unique(q1$run.id)))
  
  #loop thru covcond classes (type)
  for(m in 1:length(s1)){

    #select data for covcond class
    q2<-q1[q1$TYPE==s1[m],]
		
  	#loop thru class metrics
  	for(i in 7:ncol(q2)){ 

      #establish plot limits
      if(current==TRUE){
        xmin<-min(q2[,i],y[y$timestep==0 & y$TYPE==s1[m],i],na.rm=TRUE)
        xmax<-max(q2[,i],y[y$timestep==0 & y$TYPE==s1[m],i],na.rm=TRUE)
        }
      else{
        xmin<-min(q2[,i],na.rm=TRUE)
        xmax<-max(q2[,i],na.rm=TRUE)      
        }
      xrange<-xmax-xmin

      #for pooled runs
      if(pool.runs==TRUE){

        #select data for run
        q3<-q2[order(q2$timestep),]
        
        #plot to file
        if(save.figs==TRUE){
          bitmap(file=paste(names(q3)[i],'.',s1[m],'.png',sep=''),
                 height=6,width=8,res=300,...) 
        }
        
        #plot density
        z1<-density(q3[,i],from=xmin,to=xmax,na.rm=TRUE)
        plot(z1$x,z1$y/max(z1$y),type='l',
          xlim=c(xmin-(xrange*0.03),xmax+(xrange*0.03)),
          ylim=c(0,1.1),xaxs='i',xlab=names(q3[i]),
          ylab='Density',lwd=2,main='',col=col.line,
          cex.lab=cex.lab,...)
        
        #add current condition and %SRV lines	
        if(current==TRUE){
          segments(y[y$timestep==0 & y$TYPE==s1[m] & y$run.id==runs[j],i],0,
            y[y$timestep==0 & y$TYPE==s1[m] & y$run.id==runs[j],i],1,
            lty=1,lwd=2,col='red')
          }
        segments(quantile(q3[,i],.5,na.rm=TRUE),0,
           quantile(q3[,i],.5,na.rm=TRUE),1,
           lty=2,lwd=2,col='darkgrey')
        segments(quantile(q3[,i],quantiles[1],na.rm=TRUE),0,
           quantile(q3[,i],quantiles[1],na.rm=TRUE),1,
           lty=3,lwd=2,col='darkgrey')
        segments(quantile(q3[,i],quantiles[2],na.rm=TRUE),0,
           quantile(q3[,i],quantiles[2],na.rm=TRUE),1,
           lty=3,lwd=2,col='darkgrey')
        
        #add legend
        if(current==TRUE){
          legend(x='topleft',ncol=4,
            legend=c('current',paste('q',quantiles[1],sep=''),
            'q0.5',paste('q',quantiles[2],sep='')),
            lty=c(1,3,2,3),bty='n',lwd=2,
            col=c('red','darkgrey','darkgrey','darkgrey'),cex=cex.legend)
          }
        else{
          legend(x='topleft',ncol=3,
            legend=c(paste('q',quantiles[1],sep=''),
            'q0.5',paste('q',quantiles[2],sep='')),
            lty=c(3,2,3),bty='n',lwd=2,col='darkgrey',cex=cex.legend)
          }
          
        #add plot title				
        title(main=paste('Class Metric Density',
          ' (',names(q3)[i],')',sep=''),line=2.5,cex.main=cex.main,...)
        
        #add subtitle
        mtext(side=3,line=1,col=col.sub,cex=cex.sub,
          text=paste(s1[m],': Pooled across runs',sep=''),...)
        
        #add horizontal line at zero
        abline(h=0,col='gray')
        
        if(save.figs==TRUE) dev.off()
        
        if(save.figs==FALSE & (!i==ncol(q2) | !m==length(s1)))
          readline('Press return for next plot')
        }
      
      #for separate plots per run
      else{
        
        #loop thru runs
        for(j in 1:length(runs)){
  
          #select data for run
          q3<-q2[q2$run.id==runs[j],]
    			q3<-q3[order(q3$timestep),]
          
    			#plot to file
    			if(save.figs==TRUE){
    			  bitmap(file=paste(names(q3)[i],runs[j],'.',s1[m],'.png',sep=''),
    			         height=6,width=8,res=300,...) 
    			}
    			  
          #plot density
          z1<-density(q3[,i],from=xmin,to=xmax,na.rm=TRUE)
          plot(z1$x,z1$y/max(z1$y),type='l',
            xlim=c(xmin-(xrange*0.03),xmax+(xrange*0.03)),
            ylim=c(0,1.1),xaxs='i',xlab=names(q3[i]),
            ylab='Density',lwd=2,main='',col=col.line,
            cex.lab=cex.lab,...)
    		
      		#add current condition and %SRV lines	
      		if(current==TRUE){
            segments(y[y$timestep==0 & y$TYPE==s1[m] & y$run.id==runs[j],i],0,
              y[y$timestep==0 & y$TYPE==s1[m] & y$run.id==runs[j],i],1,
              lty=1,lwd=2,col='red')
            }
      		segments(quantile(q3[,i],.5,na.rm=TRUE),0,
            quantile(q3[,i],.5,na.rm=TRUE),1,
            lty=2,lwd=2,col='darkgrey')
      		segments(quantile(q3[,i],quantiles[1],na.rm=TRUE),0,
            quantile(q3[,i],quantiles[1],na.rm=TRUE),1,
            lty=3,lwd=2,col='darkgrey')
      		segments(quantile(q3[,i],quantiles[2],na.rm=TRUE),0,
      		  quantile(q3[,i],quantiles[2],na.rm=TRUE),1,
            lty=3,lwd=2,col='darkgrey')
  
      		#add legend
          if(current==TRUE){
            legend(x='topleft',ncol=4,
        			legend=c('current',paste('q',quantiles[1],sep=''),
              'q0.5',paste('q',quantiles[2],sep='')),
              lty=c(1,3,2,3),bty='n',lwd=2,
              col=c('red','darkgrey','darkgrey','darkgrey'),cex=cex.legend)
            }
          else{
        		legend(x='topleft',ncol=3,
        			legend=c(paste('q',quantiles[1],sep=''),
              'q0.5',paste('q',quantiles[2],sep='')),
              lty=c(3,2,3),bty='n',lwd=2,col='darkgrey',cex=cex.legend)
            }
    
          #add plot title				
      		title(main=paste('Class Metric Density',
            ' (',names(q3)[i],')',sep=''),line=2.5,cex.main=cex.main,...)
  
     			#add subtitle
    			mtext(side=3,line=1,col=col.sub,cex=cex.sub,
            text=paste(s1[m],': Run #',runs[j],sep=''),...)
  
          #add horizontal line at zero
          abline(h=0,col='gray')
          
    			if(save.figs==TRUE) dev.off()
                
       		if(save.figs==FALSE & (!j==length(runs) | !i==ncol(q2) | !m==length(s1)))
      			readline('Press return for next plot')

          } #end loop thru runs

        } #end separate plot per run

      } #end loop thru metrics

    } #end loop thru classes

  } #end single scenario and session
	
par(old.par)
}
lc.table <-
function(infile,scenario=NULL,var='srv50%',start.step=1,
	stop.step=NULL,outfile=FALSE){
	
#read habit@ data
y<-read.csv(infile,header=TRUE)

if(is.null(scenario)) scenario<-as.vector(unique(y$LID))

#create results files for multiple sessions
if(length(scenario)>1){
	zz1<-matrix(0,length(scenario),3)
	zz1<-as.data.frame(zz1)
	colnames(zz1)<-c('scenario',paste(var,'.','LChrc',sep=''),paste(var,'.','LCpc',sep=''))
	zz1[,1]<-scenario
	}

#loop thru sessions
for(j in 1:length(scenario)){

	#select records for current session
	y1<-y[y$LID==scenario[j],]
	y1<-y1[,-1] #drop LID 
	
	#set file-dependent defaults
	if(start.step>max(y1$timestep)) 
		stop('Start.step exceeds maximum timestep')
	if(is.null(stop.step)) stop.step<-max(y1$timestep)
	else{
		if(stop.step>max(y1$timestep)) 
			warning('Stop.step exceeds maximum timestep and will be set to the maximum')
		stop.step<-min(stop.step,max(y1$timestep))
		}
	
	#calculate LC metric SRV quantiles
	q1<-matrix(0,2,101)
	q2<-y1[y1$timestep>=start.step & y1$timestep<=stop.step,]
	for(i in 1:nrow(q1)){ 
		q1[i,1:101]<-quantile(q2[,i+1],probs=seq(0,1,0.01))
		}
	
	#get current LC metric (timestep 0)
	current.value<-round(t(y1[y1$timestep==0,-1]),3)
	
	#put most of it together
	z1<-round(as.data.frame(q1),2)
	LC.metric<-colnames(y1)[-1]
	z1<-cbind(LC.metric,z1)
	z1<-cbind(z1,current.value)
	temp<-matrix(0,nrow(z1),2)
	z1<-cbind(z1,temp)
	
	#calculate srv.cv and current LC metric %SRV
	for(i in 1:nrow(z1)){
		if(z1[i,102]-z1[i,2]==0) z1[i,c(104,105)]<-'NA'
		else{
			z1[i,105]<-round(((z1[i,97]-z1[i,7])/z1[i,52])*100,0)
			z1[i,104]<-max(0,which(q1[i,]<z1[i,103]))		
			if(z1[i,104]==101) z1[i,104]<-100
			}
		}
	
	#put the final table together
	z1<-z1[,c(1,2,7,27,52,77,97,102,105,103,104)]
	colnames(z1)<-c('LC.metric','srv0%','srv5%','srv25%',
		'srv50%','srv75%','srv95%','srv100%','srv.cv',
		'current.value','current.%SRV')

	#merge selected LC stat to final results table
	if(length(scenario)>1){
		temp<-subset(z1,select=var)
		zz1[j,2:3]<-t(temp)
		}
	}

#create list object
if(length(scenario)>1) z<-list(infile,scenario,start.step,stop.step,zz1)
else z<-list(infile,scenario,start.step,stop.step,z1)
names(z)<-c('infile','scenario','start.step','stop.step',
	'landscape habitat capability SRV')

#output tables to file
if(outfile==TRUE){
	write.table(z[[5]],file=paste(infile,'srv.csv',sep='.'),
	quote=FALSE,row.names=FALSE,append=TRUE,sep=',')
	}
	
return(z)
}
lc.plot <-
function(infile,scenario=NULL,start.step=1,stop.step=NULL,
	type='trajectory',col.line='dark blue',col.text='brown',outfile=FALSE,...){

old.par<-par(no.readonly=TRUE)
	
#read fragstats data
y<-read.csv(infile,header=TRUE)

if(is.null(scenario)) scenario<-as.vector(unique(y$LID))

#set file-dependent defaults
if(start.step>max(y$timestep)) 
	stop('Start.step exceeds maximum timestep')
if(is.null(stop.step)) stop.step<-max(y$timestep)
else{
	if(stop.step>max(y$timestep)) 
		warning('Stop.step exceeds maximum timestep and will be set to the maximum')
	stop.step<-min(stop.step,max(y$timestep))
	}
		
#select records based on start and stop step	
q1<-y[y$timestep>=start.step & y$timestep<=stop.step,]

#create timestep variable based on start and stop step
timestep<-seq(start.step,stop.step,1)

#for multiple scenarios
if(length(scenario)>1){

	#loop thru metrics
	for(i in 3:ncol(q1)){
	
		#select LC metric
		q2<-q1[,c(1,2,i)]
		q2[,3]<-as.numeric(q2[,3])

		#create results file
		z1<-matrix(0,length(timestep),length(scenario))
		z1<-as.data.frame(z1)
		z1<-cbind(timestep,z1)

		#loop thru scenarios
		for(j in 1:length(scenario)){
	
			#select records for current scenario
			z1[,j+1]<-q2[q2$LID==scenario[j],3]
			colnames(z1)[j+1]<-scenario[j]

			}

		#plot ecdf plot: landscape capability response curve
		if(type=='ecdf'){	

			#calculate LC metric SRV quantiles
			t1<-matrix(0,101,length(scenario))
			for(k in 1:ncol(t1)){ 
				t1[,k]<-quantile(z1[,k+1],probs=seq(0,1,0.01))
				}
			t1<-as.data.frame(t1)
			quantile<-seq(0,1,0.01)
			z2<-cbind(quantile,t1)
			z3<-z2[order(z2$quantile,decreasing=TRUE),]
			names(z3)<-c('quantile',scenario)

			#plot LC trajectories
			print(round(z3,2),2)
			matplot(z3[,-1],z3[,1],type='l',
				xlim=c(max(z3[,-1])*1.01,min(z3[,-1])*.99),
				yaxs='i',xaxs='i',
				xlab='Landscape Capability Index',
				ylab='Cumulative Percentile',
				lty=1,col=rainbow(length(scenario)),...)
		
			#add legend
			legend(x='topright',inset=c(0.02,0.02),
				legend=scenario,lty=1,col=rainbow(length(scenario)),lwd=1.5)
			
			#add plot title				
			title(main='Landscape Habitat Capability Response Curve')

			#add subtitle: LC index
			if(names(q2)[3]=='LChrc')
				mtext(side=3,col=col.text,text='(based on home range capability)')
			else if(names(q2)[3]=='LCpc')
				mtext(side=3,col=col.text,text='(based on population capability)')
				
			}

		#plot trajectory of LC as a line
		else{	

			#plot LC trajectories
			print(round(z1,2),2)
			matplot(z1[,1],z1[,-1],type='l',
				ylim=c(min(z1[,-1])*.99,max(z1[,-1])*1.01),
				yaxs='i',xaxs='i',
				xlab='Timestep (x10 yrs)',ylab=names(q2[3]),
				lty=1,col=rainbow(length(scenario)),...)
		
			#add legend
			legend(x='topright',inset=c(0.02,0.02),
				legend=scenario,lty=1,col=rainbow(length(scenario)),lwd=1.5)
			
			#add plot title				
			title(main='Landscape Habitat Capability Trajectory')

			#add subtitle: LC index
			if(names(q2)[3]=='LChrc')
				mtext(side=3,col=col.text,text='(based on home range capability)')
			else if(names(q2)[3]=='LCpc')
				mtext(side=3,col=col.text,text='(based on population capability)')

			}
		
		if(!i==ncol(q1))
			readline('Press return for next plot')
		}
	}

#for single scenario
else{

	#select scenario records
	q1<-q1[q1$LID==scenario,]

	#loop thru metrics
	for(i in 3:ncol(q1)){
	
		#plot LC trajectory
		plot(q1[,2],q1[,i],type='l',col=col.line,
			ylim=c(min(q1[,i])*.99,max(q1[,i])*1.01),
			yaxs='i',xaxs='i',
			xlab='Timestep (x10 yrs)',ylab=names(q1[i]),...)
		
		#add current condition and %SRV lines	
		abline(h=y[y$LID==scenario & y$timestep==0,i],lty=1,col='yellow',...)
		abline(h=quantile(q1[,i],.5),lty=1,col='red',...)
		abline(h=quantile(q1[,i],.05),lty=2,col='red',...)
		abline(h=quantile(q1[,i],.95),lty=2,col='red',...)
	
		#add plot title				
		title(main=paste('Landscape Habitat Capability Trajectory',' (',names(q1)[i],')',sep=''))
	
		#add subtitle: LC metric
		mtext(side=3,col=col.text,text='(current landscape, 5th, 50th and 95th percentiles)')
	
		if(!i==ncol(q1))
			readline('Press return for next plot')
		}
	}

par(old.par)
}
similarity <-
function(path="D:/FragProtocol/Files/", 
  contrast="contrast.fsq",
  rules='contrast.rules.csv', 
  out="similarity.fsq"){

#similar.R - Create similarity matrix for Fragstats
#Usage: similar(path, contrast.csv, results.csv)
#Must have contrast weights file before running (contrast.R)
#Must have function available to run >source("d:/R/similar.R")
#K. McGarigal
#April 28, 2006
#modified Oct 2, 2014 to reflect update to fragstats4.2

#create rules dataframe
rules<-read.csv(paste(path,rules,sep=''),row.names=1,header=TRUE)

#create weights matrices
z<-read.csv(paste(path,contrast,sep=''),skip=2,header=FALSE)	#assign weights file

#take complement of contrast weights
z<-1-z

#write results to csv file
outfile<-paste(path,out,sep='')
write.table('FSQ_TABLE',file=outfile,quote=FALSE,col.names=FALSE,row.names=FALSE)
a<-paste('CLASS_LIST_NUMERIC(',paste(rownames(rules),collapse=','),')\n',sep='')
cat(a,file=outfile,append=TRUE)
write.table(z,file=outfile,quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',',append=TRUE)

}

dsize.new <-
    function(path,session=NULL,runs=NULL,pool.runs=TRUE,
             start.step=1,stop.step=NULL,cell.size=30,log.size=TRUE,
             breaks=c(0,1,10,100,1000,10000,100000),target=NULL,
             col.bars='blue',col.sub='brown',cex.main=1.5,cex.sub=1.5,
             cex.legend=1.5,...){
        
        #set defaults
        options(warn=0,scipen=999)
        old.par<-par(no.readonly=TRUE)
        
        #read dsize data
        x<-read.csv(paste(path,'dsize.csv',sep=''),header=TRUE)
        
        #read target dsize data
        if(!is.null(target)){
            x2<-read.csv(paste(path,target,sep=''),header=FALSE)
            x2<-as.vector(x2[,1])
            x2<-na.omit(x2)
        }
        
        #set session parameter
        if(is.null(session)) session<-unique(x$session.id)
        if(length(session)>1) stop('Enter single session id')
        
        #verify valid session ids
        all.sessions<-unique(x$session.id)
        if(any(!session %in% all.sessions)) stop('Invalid session id')
        
        #set runs parameter
        if(is.null(runs)) runs<-unique(x$run.id)
        
        #verify valid run ids
        all.runs<-unique(x$run.id)
        if(any(!runs %in% all.runs)) stop('Invalid run ids')
        
        #set start.step and stop.step parameters
        if(is.null(stop.step)) stop.step<-max(x$timestep.id)
        else{
            if(stop.step>max(x$timestep.id)) 
                warning('Stop.step exceeds maximum timestep and was set to the maximum')
            stop.step<-min(stop.step,max(x$timestep.id))
        }
        
        #subset data based on session.id, runs, start.step and stop.step
        x<-x[x$session.id==session & x$run.id %in% runs & 
                 x$timestep.id>=start.step & x$timestep.id<=stop.step,]
        
        #rescale dist size
        if(log.size==TRUE) x$size.obs<-log((x$size.obs*((cell.size^2)/10000))+1)
        else x$size.obs<-x$size.obs*((cell.size^2)/10000)
        
        #set breaks for barplot
        if(max(breaks)<max(x$size.obs)) breaks<-ceiling(c(breaks,max(x$size.obs)))
        
        #create looping vector for dist.type
        dist.levels<-levels(x$dist.type)
        
        #create empty list
        z<-vector("list", length(dist.levels)) 
        names(z)<-paste(dist.levels,' disturbance size',sep='')
        
        #loop thru dist.types
        for(i in 1:length(dist.levels)){ 
            y<-x[x$dist.type==dist.levels[i],]
            
            #create list objects for results
            z[[i]]<-vector("list",length(runs))
            names(z[[i]])<-paste('run number ',runs,sep='')
            
            #for pooled runs
            if(pool.runs==TRUE){
                
                #create data for barplot
                bins<-as.data.frame(as.factor(seq(1,length(breaks)-1)))
                names(bins)<-'bin.id'
                bin.id<-cut(y$size.obs,breaks=breaks,labels=FALSE) 
                temp<-as.data.frame(table(bin.id))
                temp<-merge(temp,bins,all=TRUE)
                temp$Freq[is.na(temp$Freq)]<-0
                temp$bin.id<-breaks[-1]
                temp$Proportion<-temp$Freq/sum(temp$Freq)
                names(temp)<-c('bin','obs.freq','obs.proportion')
                
                if(is.null(target)) z[[i]]<-temp
                
                #summarize target data 
                else{
                    bin.id<-cut(x2,breaks=breaks,labels=FALSE) 
                    temp2<-as.data.frame(table(bin.id))
                    temp2<-merge(temp2,bins,all=TRUE)
                    temp2$Freq[is.na(temp2$Freq)]<-0
                    temp2$bin.id<-breaks[-1]
                    temp2$proportion<-temp2$Freq/sum(temp2$Freq)    
                    names(temp2)<-c('bin','target.freq','target.proportion')
                    temp<-as.matrix(merge(temp,temp2))
                    z[[i]]<-temp
                    temp2 = as.data.frame(temp[,1:3])
                    temp2$type = as.factor('Observed')
                    names(temp2)[2:3] = c('Frequency', 'Proportion')
                    temp3 = as.data.frame(temp[,c(1,4,5)])
                    temp3$type = as.factor('Target')
                    names(temp3)[2:3] = c('Frequency', 'Proportion')
                    temp4 = rbind(temp2, temp3)
                }
                
                #plot barplot
                if(is.null(target)){
                    barplot(temp$obs.proportion,names.arg=temp$bin,col=col.bars,...)
                }
                else{
                    ggplot(temp4, aes(x=factor(bin), y=Proportion, fill=type)) + 
                        geom_bar(stat="identity", position = 'dodge') +
                        theme_bw() + 
                        theme(axis.title.y = element_text(size=32,vjust=2),
                              axis.title.x = element_text(size=32,vjust=-1),
                              axis.text.x  = element_text(size=24),
                              axis.text.y  = element_text(size=24)) +
                        theme(legend.title=element_blank()) +
                        theme(legend.text = element_text(size = 24)) +
                        theme(plot.title = element_text(size=40,vjust=1)) +
                        theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
                        ggtitle('Disturbance Size Distribution') + 
                        xlab("Disturbance Size (ha)") +
                        ylab("Proportion") 
                    #barplot(t(temp[,c(3,5)]),beside=TRUE,names.arg=temp[,1],
                    #        col=col.bars,...)
                }
                
                if(log.size==TRUE){
                    title(main='Disturbance Size Distribution',cex.main=cex.main,
                          ylab='Proportion',xlab='Log(Disturbance Size (ha) + 1)',...)
                }
                else{
                    title(main='Disturbance Size Distribution',cex.main=cex.main,
                          ylab='Proportion',xlab='Disturbance Size (ha)',...)
                }
                #####################################################
                # comment out below items to remove "Wildfire Run #" from plot
                #mtext(side=3,col=col.sub,cex=cex.sub,
                #  text=dist.levels[i],...)
                
                if(!is.null(target)){
                    legend('topright',inset=c(0.1,0.1),legend=c('Observed','Target'),
                           fill=col.bars,cex=cex.legend,bty='n')
                }
                
                if(!i==length(dist.levels)) 
                    readline("Press return for next plot ")
            }
            
            #for separate runs
            else{
                
                #create target summary table
                if(!is.null(target)){
                    bins<-as.data.frame(as.factor(seq(1,length(breaks)-1)))
                    names(bins)<-'bin.id'
                    bin.id<-cut(x2,breaks=breaks,labels=FALSE) 
                    target.temp<-as.data.frame(table(bin.id))
                    target.temp<-merge(target.temp,bins,all=TRUE)
                    target.temp$Freq[is.na(target.temp$Freq)]<-0
                    target.temp$bin.id<-breaks[-1]
                    target.temp$proportion<-target.temp$Freq/sum(target.temp$Freq)    
                    names(target.temp)<-c('bin','target.freq','target.proportion')
                }
                
                #loop thru runs
                for(j in 1:length(runs)){
                    
                    #select data for run
                    q<-x[x$run.id==runs[j],]
                    
                    #create data for barplot
                    bin.id<-cut(q$size.obs,breaks=breaks,labels=FALSE) 
                    temp<-as.data.frame(table(bin.id))
                    temp<-merge(temp,bins,all=TRUE)
                    temp$Freq[is.na(temp$Freq)]<-0
                    temp$bin.id<-breaks[-1]
                    temp$Proportion<-temp$Freq/sum(temp$Freq)
                    names(temp)<-c('bin','obs.freq','obs.proportion')
                    
                    if(is.null(target)) z[[i]][[j]]<-temp
                    
                    else{
                        temp<-as.matrix(merge(temp,target.temp))
                        z[[i]][[j]]<-temp
                    }
                    
                    #plot barplot
                    if(is.null(target)){
                        barplot(temp$obs.proportion,names.arg=temp$bin,col=col.bars,...)
                    }
                    else{
                        barplot(t(temp[,c(3,5)]),beside=TRUE,names.arg=temp[,1],
                                col=col.bars,...)
                    }
                    
                    if(log.size==TRUE){
                        title(main='Disturbance Size Distribution',cex.main=cex.main,
                              ylab='Proportion',xlab='Log(Disturbance Size (ha) + 1)',...)
                    }
                    else{
                        title(main='Disturbance Size Distribution',cex.main=cex.main,
                              ylab='Proportion',xlab='Disturbance Size (ha)',...)
                    }
                    
                    #####################################################
                    # comment out below items to remove "Wildfire Run #" from plot
                    #mtext(side=3,col=col.sub,cex=cex.sub,
                    #    text=paste(dist.levels[i],' Run #',runs[j],sep=''),...)
                    
                    if(!is.null(target)){
                        legend('topright',inset=c(0.1,0.1),legend=c('Observed','Target'),
                               fill=col.bars,cex=cex.legend,bty='n')
                    }
                    
                    if(!i==length(dist.levels) || !j==length(runs)) 
                        readline("Press return for next plot ")
                }
            }
        }
        
        par(old.par)
        return(z)
    }