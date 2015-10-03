# FRAGCLASS FUNCTIONS
## New functions to look at SMC M/X Early
# First code creates boxplots by class metric ####
# with separate boxplot for each seral stage


path='/Users/mmallek/Tahoe/Fragstats/fragstats20151002/'
#inland='fragresults_hrv_20150831.land'
inclass='fragresults_ccsm1_20150831.class'
inclasses = c(#'fragresults_ccsm1_20151002.class','fragresults_ccsm2_20151002.class','fragresults_ccsm3_20151002.class',
              'fragresults_ccsm4_20151002.class',#'fragresults_ccsm5_20151002.class',
              'fragresults_ccsm6_20151002.class',
              'fragresults_esm2m_20151002.class')
LID.path='Z:\\Working\\maritza\\'
#scenarios='hrv'
sessions=1
sessions.name='covcond'
runs=1
runs.name='run'
gridname='covcond'
classes=c('SMC_M_EARLY_ALL','SMC_M_MID_CL','SMC_M_MID_MOD','SMC_M_MID_OP','SMC_M_LATE_CL','SMC_M_LATE_MOD','SMC_M_LATE_OP')
classes=c('SMC_X_EARLY_ALL','SMC_X_MID_CL','SMC_X_MID_MOD','SMC_X_MID_OP','SMC_X_LATE_CL','SMC_X_LATE_MOD','SMC_X_LATE_OP')
classes = c('SMC_M_EARLY_ALL', 'SMC_X_EARLY_ALL')
#metrics=c('AREA_AM','SHAPE_AM','CORE_AM','CLUMPY','GYRATE_AM')
var='srv50%'
start.step=40
stop.step=500

covlabel = 'SMC_M'
covlabel = 'SMC_X'

imagepath = "/Users/mmallek/Documents/Thesis/Plots/fragclass-bymetrics"

y.class<-read.csv(paste(path,inclasses[1],sep=''),strip.white=TRUE,header=TRUE)

for(i in 2:length(inclasses)){
    y0 = read.csv(paste(path,inclasses[i],sep=''),strip.white=TRUE,header=TRUE)
    y.class = bind_rows(y.class, y0)
}

y1 = y.class[y.class$TYPE %in% classes,]
y1$scenario = gsub('.*?future\\\\(.*)\\\\cov.*.tif', '\\1', y1$LID)
y1$scenario[1:2] = 'current'


y2 = y1[,c(1,length(y1),3:length(y1)-1)]

metrics = names(y2)[4:length(names(y2))]

y2$CLUMPY = as.numeric(y2$CLUMPY)
y2$AI = as.numeric(y2$AI)

scenario.levels = c("current","ccsm-1", "ccsm-5", "ccsm-4", "ccsm-6", "ccsm-2", "ccsm-3", "esm2m")
y2$scenario = factor(y2$scenario, levels=scenario.levels, ordered=T)
y3 = y2[complete.cases(y2),]
y3 = as.data.frame(y3)

# define the summary function
f <- function(x) {
    r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    r
}


for(i in 1:length(classes)){
    for(j in 1:length(metrics)){  
    data = y3[y3$TYPE==classes[i],]
    p = ggplot(data=data[data$scenario!='current',], aes(x=scenario, y=data[data$scenario!='current',metrics[j]] )) 
    p1 = p +
        stat_summary(fun.data = f, geom="boxplot", ,fill=c("#339900","#339900","#339900")) +
        geom_hline(aes(yintercept=data[1,metrics[i]]), lwd=3, col="#333333") +
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
           path=imagepath,
           width=15, height=5, units='in',limitsize=FALSE)    
    } 
}

for(i in 1:length(metrics)){     
    #p = ggplot(data=y1[y1$LID!='covcond000',], aes(x=factor(scenario), y=y1[y1$LID!='covcond000',metrics[i]] )) 
    #p = ggplot(data=y1, aes(x=factor(scenario), y=y1[,metrics[i]])) 
    p = ggplot(data=v[v$LID!='covcond000',], aes(x=scenario, y=v[v$LID!='covcond000',metrics[i]] )) 
    p1 = p + 
        #stat_summary(fun.data = f, geom="boxplot", ,fill=c("#0099CC","#339900","#339900","#339900","#339900",
        #                                                   "#339900","#339900","#339900")) +
        stat_summary(fun.data = f, geom="boxplot", ,fill=c("#0099CC","#339900")) +
        # comment out funy.y = o to now show 0-5% and 95-100%
        #stat_summary(fun.y = o, geom="point", col="#CC3300") +
        #geom_hline(aes(yintercept=y1[y1$LID == 'covcond000',metrics[i]]), lwd=3, col="#333333") +
        #geom_hline(aes(yintercept=y1[1,metrics[i]]), lwd=3, col="#333333") +
        geom_hline(aes(yintercept=v[1,metrics[i]]), lwd=3, col="#333333") +
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
           path=imagepath,
           width=15, height=5, units='in',limitsize=FALSE
    )    
}