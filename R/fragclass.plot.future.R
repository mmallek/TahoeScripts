# FRAGCLASS FUNCTIONS

# First code creates boxplots by class metric ####
# with separate boxplot for each seral stage

#fragpath should be path to fragstats.land file
#classpath should be path to fragstats.land file
#scenario should be name of model and run value, e.g. ccsm2_run1
#nrun is the number of runs completed for the scenario
#stop.run is the number of runs to analyze
#LID.path is path up to tif file
#covcondlist is path to csv with list of covcond files

require(tidyr)
require(dplyr)
require(ggplot2)
require(grid)

### set up only to calcuate for the hrv
### have to first manipulate the output .class and .land files so that kevin's weird parsing works

path='/Users/mmallek/Tahoe/RMLands/results/results20150904/fragstats20150901/'
inland='fragresults_hrv_20150831.land'
inclass='fragresults_hrv_20150831.class'
LID.path='Z:\\Working\\maritza\\'
scenarios='hrv'
sessions=1
sessions.name='covcond'
runs=1
runs.name='run'
gridname='covcond'
#gridname='wfmort',
classes=c('MEG_M_EARLY_ALL','MEG_M_MID_CL','MEG_M_MID_MOD','MEG_M_MID_OP','MEG_M_LATE_CL','MEG_M_LATE_MOD','MEG_M_LATE_OP')
classes=c('MEG_X_EARLY_ALL','MEG_X_MID_CL','MEG_X_MID_MOD','MEG_X_MID_OP','MEG_X_LATE_CL','MEG_X_LATE_MOD','MEG_X_LATE_OP')
classes=c('OCFW_EARLY_ALL','OCFW_MID_CL','OCFW_MID_MOD','OCFW_MID_OP','OCFW_LATE_CL','OCFW_LATE_MOD','OCFW_LATE_OP')
classes=c('OCFW_U_EARLY_ALL','OCFW_U_MID_CL','OCFW_U_MID_MOD','OCFW_U_MID_OP','OCFW_U_LATE_CL','OCFW_U_LATE_MOD','OCFW_U_LATE_OP')
classes=c('RFR_M_EARLY_ALL','RFR_M_MID_CL','RFR_M_MID_MOD','RFR_M_MID_OP','RFR_M_LATE_CL','RFR_M_LATE_MOD','RFR_M_LATE_OP')
classes=c('RFR_X_EARLY_ALL','RFR_X_MID_CL','RFR_X_MID_MOD','RFR_X_MID_OP','RFR_X_LATE_CL','RFR_X_LATE_MOD','RFR_X_LATE_OP')
classes=c('SMC_M_EARLY_ALL','SMC_M_MID_CL','SMC_M_MID_MOD','SMC_M_MID_OP','SMC_M_LATE_CL','SMC_M_LATE_MOD','SMC_M_LATE_OP')
classes=c('SMC_X_EARLY_ALL','SMC_X_MID_CL','SMC_X_MID_MOD','SMC_X_MID_OP','SMC_X_LATE_CL','SMC_X_LATE_MOD','SMC_X_LATE_OP')
classes=c('SMC_U_EARLY_ALL','SMC_U_MID_CL','SMC_U_MID_MOD','SMC_U_MID_OP','SMC_U_LATE_CL','SMC_U_LATE_MOD','SMC_U_LATE_OP')
#metrics=c('AREA_AM','SHAPE_AM','CORE_AM','CLUMPY','GYRATE_AM')
var='srv50%'
start.step=40
stop.step=500
covlabel = 'MEG_M'
covlabel = 'MEG_X'
covlabel = 'OCFW'
covlabel = 'OCFW_U'
covlabel = 'RFR_M'
covlabel = 'RFR_X'
covlabel = 'SMC_M'
covlabel = 'SMC_U'
covlabel = 'SMC_X'

imagepath = "/Users/mmallek/Documents/Thesis/Plots/fragclass-bymetrics"


# default for kevin's fragclass script
path,inland,inclass,LID.path,scenarios=NULL,
sessions=NULL,sessions.name='session',runs=NULL,runs.name='run',
gridname,classes=NULL,metrics=NULL,var='srv50%',start.step=0,stop.step=NULL,
outfile=FALSE){
        
# define the summary function
f <- function(x) {
    r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    r
}


#read fragstats data
y.land<-read.csv(paste(path,inland,sep=''),strip.white=TRUE,header=TRUE)
y.class<-read.csv(paste(path,inclass,sep=''),strip.white=TRUE,header=TRUE)

# first set of code just works on land file ####
#parse LID name
# this gets you a 3-column data frame (t1) with a scenario name and then numerical session and run values
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
# this gets you a 2-column data frame (t2) which has the second half of the path to the class/land files
    # with 'grid' in the front of the path for some reason
t2<-sub(gridname,',grid',y.land$LID,fixed=TRUE)
write.table(t2,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
t2<-read.csv('temp.txt',header=FALSE)
names(t2)<-c('junk','grid')

#combine results
# this gets you your y.land file with better initial columns, I guess, than the LID
y.land<-cbind(t1,t2,y.land[,-1])
y.land<-y.land[,-4]

# next set of code modifies the class file ####
#parse y.class LID name
# this gets you the same t1 data frame as earlier, only with more rows
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
# same as above
t2<-sub(gridname,',grid',y.class$LID,fixed=TRUE)
write.table(t2,'temp.txt',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=',')
t2<-read.csv('temp.txt',header=FALSE)
names(t2)<-c('junk','grid')

#combine results
# same as above, only for y.class
y.class<-cbind(t1,t2,y.class[,-1])
y.class<-y.class[,-4]

# all the warning code; don't need if you've carefully set all your variables
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
# kind of unnecessary
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

#loop thru scenarios and sessions (don't need this, only 1 of each)
#for(j in 1:nrow(t0)){   
    
    #select records for current scenario and session
    #y1<-y.land[y.land$scenario==t0[j,1] & y.land$session.id==t0[j,2],1:3]
    y1 = y.land[,1:3]

    #add timestep variable by run
    truns<-as.vector(unique(y1$run.id))
    #t0[j,3]<-length(truns)
    t0[1,3] = length(truns)
    tstep<-NULL
    for(k in truns){
        t1<-sum(y1$run.id==k)
        tstep<-c(tstep,seq(0,t1-1)) 
    }
    #y.land$timestep[y.land$scenario==t0[j,1] & y.land$session.id==t0[j,2]]<-tstep   [for loop]
    y.land$timestep<-tstep 
#}

# I think everything up to this point was making new columns at the front and making a timestep column sequence

#rearrange dataframe and drop variables 
# this erases all of the landscape results
y.land<-y.land[,c(1:4,ncol(y.land))]

### start here for new classes ####

#get unique covcond types
if(is.null(classes)) s1<-sort(as.vector(unique(y.class$TYPE)))
else s1<-classes

#merge fragstats class data with landscape timestep
# creates a data frame with the right timestep for each row in the class metric
t1<-merge(y.land,s1)
names(t1)<-c('scenario','session.id','run.id','grid','timestep','TYPE')
y<-merge(t1,y.class,by=c('scenario','session.id','run.id','grid','TYPE'),
         all.x=TRUE)

#ensure numeric variables (some get put as factor by default)
for(i in 7:ncol(y)){
    y[,i]<-as.numeric(as.character(y[,i]))
}

#set metrics parameter
class.metrics<-colnames(y)[-c(1:6)]
if(is.null(metrics)) metrics<-class.metrics
if(any(!metrics %in% class.metrics)) stop('Invalid metrics selected')

#select columns
# reduce data frame to only selected metrics, plus front part
y.metrics<-subset(y,select=metrics)
y.head<-y[,1:6]
y<-cbind(y.head,y.metrics)

# end of part 1 ####
# now there is a working data frame for calculating next stuff 


q0 = y

# set aside current value
current.value = q0[q0$timestep==0,]

q1<-q0
# remove early timesteps
q1 = q1[q1$timestep>=start.step,]
q1$TYPE = gsub(paste(covlabel,'(.*)',sep='_'), '\\1', q1$TYPE)
current.value$TYPE = gsub(paste(covlabel,'(.*)',sep='_'), '\\1', current.value$TYPE)
q1$TYPE = factor(q1$TYPE, levels = c("EARLY_ALL", "MID_CL", "MID_MOD", "MID_OP", "LATE_CL", "LATE_MOD", "LATE_OP"))
current.value$TYPE = factor(current.value$TYPE, levels = c("EARLY_ALL", "MID_CL", "MID_MOD", "MID_OP", "LATE_CL", "LATE_MOD", "LATE_OP"))

for(j in 1:length(metrics)){     
    p = ggplot(data=q1, aes(x=TYPE, y=q1[,metrics[j]]))
    p1 = p + 
        stat_summary(fun.data = f, geom="boxplot", ,fill=c("#339900")) +
        geom_crossbar(data=current.value, 
            aes(x=TYPE,y=current.value[,6+j], ymin=current.value[,6+j], ymax=current.value[,6+j]), lwd=2,col="#333333") +
        theme_bw() +
        theme(axis.title.y = element_text(size=24,vjust=1),
              axis.title.x = element_blank(), # element_text(size=24,vjust=-1),
              axis.text.x  = element_text(size=16),
              axis.text.y  = element_text(size=16)) +
        theme(legend.title=element_text(size=16)) +
        theme(legend.text = element_text(size = 16)) +
        theme(plot.title = element_text(size=24,vjust=1)) +
        theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
        ggtitle(paste(covlabel, " Class Metric: ", metrics[j], sep='')) + 
       # xlab(covlabel) +
        ylab("Metric Value") 
    print(p1)
    ggsave(paste(covlabel, metrics[j], "boxplots.png",sep="-"), 
           path=imagepath,
           width=13, height=5, units='in',limitsize=FALSE
    )    
}


#    } #end loop thru scenarios and sessions



#####################################
#####################################