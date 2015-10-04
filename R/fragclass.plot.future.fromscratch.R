# FRAGCLASS FUNCTIONS
## New functions to look at SMC M/X Early
# First code creates boxplots by class metric ####
# with separate boxplot for each seral stage


path='/Users/mmallek/Tahoe/Fragstats/fragstats20151002/'
path = '/Users/mmallek/Tahoe/RMLands/results/results20150904/fragstats20151002/'
#inland='fragresults_hrv_20150831.land'
inclass='fragresults_ccsm1_20150831.class'
inclasses = c('fragresults_ccsm1_20151003-redo.class','fragresults_ccsm2_20151003.class','fragresults_ccsm3_20151003.class',
              'fragresults_ccsm4_20151003.class','fragresults_ccsm5_20151003.class',
              'fragresults_ccsm6_20151003.class',
              'fragresults_esm2m_20151003-redo.class')
LID.path='Z:\\Working\\maritza\\'
#scenarios='hrv'
sessions=1
sessions.name='covcond'
runs=1
runs.name='run'
gridname='covcond'
classes=c('SMC_M_EARLY_ALL','SMC_M_MID_CL','SMC_M_MID_MOD','SMC_M_MID_OP','SMC_M_LATE_CL','SMC_M_LATE_MOD','SMC_M_LATE_OP')
classes=c('SMC_X_EARLY_ALL','SMC_X_MID_CL','SMC_X_MID_MOD','SMC_X_MID_OP','SMC_X_LATE_CL','SMC_X_LATE_MOD','SMC_X_LATE_OP')
classes = c('SMC_M_EARLY_ALL', 'SMC_X_EARLY_ALL','RFR_M_EARLY_ALL','RFR_X_EARLY_ALL','OCFW_EARLY_ALL')
classes = c('SMC_M_EARLY_ALL', 'SMC_X_EARLY_ALL')
#metrics=c('AREA_AM','SHAPE_AM','CORE_AM','CLUMPY','GYRATE_AM')
var='srv50%'
start.step=40
stop.step=500

covlabel = 'SMC_M'
covlabel = 'SMC_X'

imagepath = "/Users/mmallek/Documents/Thesis/Plots/smc-classmetrics/"

## beginning of real code ####
y.class<-read.csv(paste(path,inclasses[1],sep=''),strip.white=TRUE,header=TRUE)


for(i in 2:length(inclasses)){
    y0 = read.csv(paste(path,inclasses[i],sep=''),strip.white=TRUE,header=TRUE)
    y.class = rbind(y.class, y0)
}

y1 = y.class[y.class$TYPE %in% classes,]
y1$scenario = gsub('.*?future\\\\(.*)\\\\covcond\\\\.*.tif', '\\1', y1$LID)
y1$scenario[1:2] = 'current'
y1 = y1[y1$scenario!="Z:\\Working\\maritza\\hrv\\covcond\\covcond000res_clip.tif",] 

y2 = y1[,c(1,length(y1),3:length(y1)-1)]

metrics = names(y2)[4:length(names(y2))]

y2$CLUMPY = as.numeric(y2$CLUMPY)
y2$AI = as.numeric(y2$AI)

scenario.levels = c("current","ccsm-1", "ccsm-5", "ccsm-4", "ccsm-6", "ccsm-2", "ccsm-3", "esm2m")
y2$scenario = factor(y2$scenario, levels=scenario.levels, ordered=T)
y3 = y2[complete.cases(y2),]

# define the summary function
f <- function(x) {
    r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    r
}

titles = c()
metricnames = c('Percent of Landscape','Patch Density','Total Edge','Edge Density','Area-weighted Mean Patch Size','Area-weighted Mean Radius of Gyration',
                'Area-weighted Mean Shape Index','Area-weighted Mean Core Area Size','Contrast-Weighted Edge Density','Area-weighted Edge Contrast',
                'Clumpiness Index','Aggregation Index')

for(i in 1:length(classes)){
    data = y3[y3$TYPE==classes[i],]
    for(j in 1:length(metrics)){  
    p = ggplot(data=data[data$scenario!='current',], aes(x=scenario, y=data[data$scenario!='current',metrics[j]] )) 
    p1 = p +
        stat_summary(fun.data = f, geom="boxplot", ,fill="#339900") +
        geom_hline(aes(yintercept=data[1,metrics[j]]), lwd=3, lty='longdash', col="#333333") +
        #scale_x_discrete(labels=c("CCSM-1", "CCSM-5", "CCSM-4", "CCSM-6", "CCSM-2", "CCSM-3", "ESM2M")) +
        theme_bw() +
        theme(axis.title.y = element_text(size=32,vjust=1),
              axis.title.x = element_text(size=32,vjust=-1),
              axis.text.x  = element_text(size=24),
              axis.text.y  = element_text(size=24)) +
        theme(plot.title = element_text(size=40,vjust=1)) +
        theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
        ggtitle(metricnames[j]) + 
        xlab("Climate Scenario") +
        ylab("Metric Value") 
    print(p1)
    ggsave(paste(classes[i],"-",metrics[j], "-boxplots",".png",sep=""), 
           path=imagepath,
           width=15, height=5, units='in',limitsize=FALSE)    
    } 
}



