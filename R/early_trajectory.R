## trajectories across scenarios, one figure per cover-seral stage combo ####
# version with no hrv or grand mean ####
require(tidyr)
require(dplyr)
require(ggplot2)
require(grid)
require(RColorBrewer)


fsessions = c(34,35,36,38,39,43,44)
fscenarionames = c('CCSM-1','CCSM-2','CCSM-3','CCSM-5','CCSM-4','CCSM-6','ESM2M')
path = '/Users/mmallek/Tahoe/RMLands/results/results20150904/'
fstart.step=1
fstop.step=18
hstart.step = 40
hstop.step = 500
cover.min.ha=1000
imagepath = "/Users/mmallek/Documents/Thesis/Plots/covcond-byscenario"
covlabel = c('MEGM','MEGX','OCFW','OCFWU','RFRM','RFRX','SMCM','SMCU','SMCX')
scenario.levels = c( "CCSM-1", "CCSM-5", "CCSM-4", "CCSM-6", "CCSM-2", "CCSM-3", "ESM2M")
cover.names = c("Sierran Mixed Conifer - Mesic", "Sierran Mixed Conifer - Xeric")


#read covcond data
y0<-read.csv(paste(path,'covcond.csv',sep=''),header=TRUE)

# limit y0 to sessions of interest
y0 = y0[y0$session.id %in% fsessions,]

# careful with this line - it is basically hard coded so sessions and scenario_name object
# better be in the right order and of the same length
for(i in 1:length(fsessions)){
    y0$scenario[y0$session.id==fsessions[i]] = fscenarionames[i]
}


y  = y0

#select subset of cover types
if(!is.null(cover.names)){
    cov.levels<-levels(y$cov.name)
    if(any(!cover.names %in% cov.levels)) stop('Invalid cover names')
    y<-y[y$cov.name %in% cover.names,] 
}

# get rid of non-seral types
y = y[y$cond.name != 'Non-seral',]

# if you only want one condition
y = y[y$cond.name == 'Early-All Structures',]


#get cover type area and select cover types with min area
# calculate area in each cover type
# compare cover type areas to min cover type area specified in arguments
# only keep cover types above the minimum
# only need to do this if you didn't specify cover types above
t1<-y[y$session.id==fsessions[1] & y$run.id==1 & y$timestep.id ==0,]
t2<-aggregate(t1$cell.count,list(t1$cov.name),sum)
colnames(t2)<-c('cover.type','cov.count')
t2$area.ha<-round(t2$cov.count*cell.size^2/10000,0)
t2<-t2[t2$area.ha>cover.min.ha,]
t2.cover.type<-t2$cover.type

y<-y[y$cov.name %in% t2.cover.type,]

# this gets you the data frame with only the timesteps of interest [1-18]
y = y[y$session.id %in% fsessions & y$timestep.id %in% c(fstart.step:fstop.step),]

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

# calculate current proportions
s2$proportion = s2$cell.count
for(i in 1:length(unique(s2$cov.name))){
    s2[s2$cov.name==unique(s2$cov.name)[i],4] = s2[s2$cov.name==unique(s2$cov.name)[i],3] *.09/zz2[i,2]
}
current = merge(zz1, s2)
current$cov.name.x=NULL


#create dataframe for cover departure results
d1<-as.data.frame(unique(y1$cov.name)) 
colnames(d1)<-c('cover.type')
d2<-t2[,c(1,3)]
# this is just the cover type and its total area
zz2<-merge(d1,d2,by='cover.type',sort=FALSE)
colnames(zz2)<-c('cover.type','area.ha')


# maybe it will work by using the covcond id, instead of 2 loops?
covcond.code = sort(unique(current$cov.cond.id))
#df$scenario = factor(df$scenario, levels=scenario.levels, ordered=T)


q = y

# calculate proportion
q$proportion = 0
q[,9] = q[,7] *.09/t2[t2$cover.type==q$cov.name[1],3]
q$scenario = factor(q$scenario, levels=scenario.levels, ordered=T)

#eliminate unneeded columns
q$session.id = NULL
q$cell.count = NULL


for(j in 1:length(covcond.code)){
    # q2 is a subsetted matrix for one cover-condition combo
    # and all the scenarios
    q2<-q[q$cov.cond.id==covcond.code[j],]
    
    for(k in 1:length(scenario.levels)){
        q3 = q2[q2$scenario==scenario.levels[k],]
        # make the plot
        p = ggplot(q3, aes(x=timestep.id, y=proportion, group=run.id)) 
        p1 = p +
            geom_line(alpha=0.75) + 
            theme_bw() +
            theme(legend.position="top", legend.title = element_blank()) +
            theme(axis.title.y = element_text(size=32,vjust=2),
                  axis.title.x = element_text(size=32,vjust=-1),
                  axis.text.x  = element_text(size=20),
                  axis.text.y  = element_text(size=24)) +
            theme(legend.text = element_text(size = 24)) +
            theme(plot.title = element_text(size=40,vjust=1.5)) +
            theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
            ggtitle(paste(q3$cov.name[1],  '\n', q3$cond.name[1], '\n', q3$scenario[1])) + 
            xlab("Timestep") +
            ylab("Proportion of Cover Type") 
        print(p1)    
        
        if(saveimage==TRUE){
            ggsave(paste(covcond.code[j], '-', scenario.levels[k],"-trajectory",".png",sep=""), 
                   path="/Users/mmallek/Documents/Thesis/Plots/seralstage-trajectory/",
                   width=10, height=10, units='in',limitsize=FALSE)      
        }
    }
}    
   

# same trajectory plot but only with means, and with all scenarios for a given cover type ####

# make a new data frame with means for plotting
b = q
b$cov.name=NULL
b$cond.name=NULL
b = aggregate(q['proportion'], by=q[c('timestep.id','cov.cond.id','scenario')], FUN=mean)

# plot the result

for(j in 1:length(covcond.code)){
    # q2 is a subsetted matrix for one cover-condition combo
    # and all the scenarios
    b2<-b[b$cov.cond.id==covcond.code[j],]
    
    #for(k in 1:length(scenario.levels)){
    #    q3 = q2[q2$scenario==scenario.levels[k],]
        # make the plot
    p = ggplot(b2, aes(x=timestep.id, y=proportion, group=scenario)) 
    p1 = p +
        geom_line(aes(col=scenario),size=1.5) +
        scale_colour_manual(values=brewer.pal(7,'Dark2'), name="Climate Model", 
                            guide = guide_legend(reverse=TRUE)) +
        theme_bw() +
        theme(legend.position="right",
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 28)) +
        guides(color = guide_legend(nrow=4, byrow=TRUE)) +
        theme(axis.title.y = element_text(size=32,vjust=2),
              axis.title.x = element_text(size=32,vjust=-1),
              axis.text.x  = element_text(size=20),
              axis.text.y  = element_text(size=24)) +
        theme(plot.title = element_text(size=40,vjust=1.5)) +
        theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
        #ggtitle(paste(cover.names[j],  '\n Early Successional Stage')) + 
        theme(legend.position='none') +
        xlab("Timestep") +
        ylab("Proportion of Cover Type") 
    print(p1)    
    
    if(saveimage==TRUE){
        ggsave(paste(covcond.code[j], '-earlytraj-mean.png',sep=""), 
               path="/Users/mmallek/Documents/Thesis/Plots/early-trajectory-means/",
               width=7, height=6, units='in',limitsize=FALSE)      
    }
}
    
# plot medians of trajectory ####
c = aggregate(q['proportion'], by=q[c('timestep.id','cov.cond.id','scenario')], FUN=median)
conds = unique(y1$cond.name)
conds = rep(conds, 2)
# plot the result

for(j in 1:length(covcond.code)){
    # c2 is a subsetted matrix for one cover-condition combo
    # and all the scenarios
    c2<-c[c$cov.cond.id==covcond.code[j],]
    
    # make the plot
    p = ggplot(c2, aes(x=timestep.id, y=proportion, group=scenario)) 
    p1 = p +
        geom_line(aes(col=scenario),size=1.5) +
        scale_colour_manual(values=brewer.pal(7,'Dark2'), name="Climate Model", 
                            guide = guide_legend(reverse=TRUE)) +
        theme_bw() +
        theme(
            legend.position="right",
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 28)) +
            #legend.position=c(.75, .22), # for mid-closed
            #legend.position=c(.75, .75),
            #legend.text = element_text(size = 18),
            #legend.title = element_text(size = 24)) +
        guides(color = guide_legend(nrow=4, byrow=TRUE)) +
        theme(axis.title.y = element_text(size=32,vjust=2),
              axis.title.x = element_text(size=32,vjust=-1),
              axis.text.x  = element_text(size=20),
              axis.text.y  = element_text(size=24)) +
        theme(plot.title = element_text(size=40,vjust=1.5)) +
        theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
        #ggtitle(paste(cover.names[j],  '\n Early Successional Stage')) + 
        ggtitle(paste(conds[j])) + 
        #theme(legend.position='none') + # uncomment to include legend
        xlab("Timestep") +
        ylab("Proportion of Cover Type") 
    print(p1)    
    
    if(saveimage==TRUE){
        ggsave(paste(covcond.code[j], 
                     #'-trajectory-median.png',sep=""), 
                     #'-trajectory-median-title.png',sep=""), 
                     '-trajectory-median-legend.png',sep=""), 
            path="/Users/mmallek/Documents/Thesis/Plots/seralstage-trajectory-medians/",
            #width=7, height=6, units='in',limitsize=FALSE) # no legend     
            #width=7, height=7, units='in',limitsize=FALSE) # title, no legend     
            width=10, height=7, units='in',limitsize=FALSE) # title, legend     
    }
}
#####################################
#######################################
#################################
