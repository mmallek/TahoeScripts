#### PDSI Plots
### need to show pdsi curves for all future scenarios ####
require(ggplot2)
require(tidyr)
require(dplyr)
require(RColorBrewer)
require(grid)

fscenarionames = c('CCSM-1','CCSM-2','CCSM-3','CCSM-4','CCSM-5','CCSM-6','ESM2M')
allscenarionames= c('HRV','CCSM-1','CCSM-2','CCSM-3','CCSM-4','CCSM-5','CCSM-6','ESM2M')
fscenario.levels = c("CCSM-1", "CCSM-5", "CCSM-4", "CCSM-6", "CCSM-2", "CCSM-3", "ESM2M")


# 18 timesteps long climate parameter file
pdsi = read.csv("/Users/mmallek/Documents/Thesis/GISData/pdsivalues_allruns.csv")
pdsi = pdsi[,-9]
colnames(pdsi) = c('Timestep','CCSM-1','CCSM-2','CCSM-3','CCSM-4','CCSM-5','CCSM-6','ESM2M')
pdsi2 = gather(pdsi, model, pdsival, -Timestep) # future only
gmean = median(pdsi2$pdsival)

# plot all models plus hrv
p1 = ggplot(pdsi2, aes(x=Timestep, y=pdsival))
p1 + geom_line(aes(color=model))

# remove hrv from future climate params
#pdsi3 = pdsi2[pdsi2$model!='hrv',]

# 500 timesteps long climate parameter file (hrv)
hrvpdsi = read.csv('/Users/mmallek/Tahoe/RMLands/parameters/climate_mean_5yr.csv')
hrvpdsi$model = 'hrv'
hrvpdsi$pdsival = hrvpdsi$Climate.Modifier
hrvpdsi$Climate.Modifier = NULL
colnames(hrvpdsi)[1] = 'Timestep'
hrvpdsi = hrvpdsi[1:500,]
med.hrv = median(hrvpdsi$pdsival)

# add hrv to future pdsi thing
pdsi2.1 = bind_rows(pdsi2,hrvpdsi)

# calculate average pdsi value for each model run 
    avg = apply(pdsi[,2:8], 2, mean)
    avg
    mean(hrvpdsi$pdsival)
    order(avg)
    pdsi_ordered = pdsi[,order(avg)+1]

# calculate median pdsi value for each model run
    med = apply(pdsi[,2:8], 2, median)
    med
    order(med)
    reorder = c(names(med)[order(med)[1]], names(med)[order(med)[2]],names(med)[order(med)[3]],names(med)[order(med)[4]],names(med)[order(med)[5]],
                names(med)[order(med)[6]],names(med)[order(med)[7]])
    f.scenario.levels = reorder


# create the plot
p2 = ggplot(pdsi4, aes(x=Timestep, y=pdsival))
p2 + geom_line(aes(color=model))

# generate color palette for this plot

mycols = brewer.pal(7, "Dark2")

# make factor and set in order by medians
pdsi4 = pdsi2
pdsi4$model = factor(pdsi4$model, levels=fscenario.levels, ordered=T)

# plot of full 18 timesteps ####
# not used in FRV article
p2 = ggplot(pdsi4, aes(x=Timestep, y=pdsival))
p2 + geom_line(aes(col=pdsi4$model),size=1.5) + 
    scale_colour_manual(values=mycols, name="Climate Model") +
    theme_bw() +
    theme(axis.title.y = element_text(size=32,vjust=1),
          axis.title.x = element_text(size=32,vjust=-1),
          axis.text.x  = element_text(size=24),
          axis.text.y  = element_text(size=24)) +
    theme(legend.title=element_text(size=24)) +
    theme(legend.text = element_text(size = 24)) +
    theme(plot.title = element_text(size=40,vjust=1)) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    ggtitle("Climate Parameter Trajectory") + 
    xlab("Timestep") +
    ylab("Climate Parameter Value") 


### only look at final 5 timesteps ####
# with line for hrv mean (~1)
pdsi4_end = pdsi4[pdsi4$Timestep > 13,]
p4 = ggplot(pdsi4_end, aes(x=Timestep, y=pdsival))
p4 + geom_line(aes(col=pdsi4_end$model),size=1.5) + 
    #geom_hline(aes(yintercept=1)) +
    scale_colour_manual(values=brewer.pal(7,'Dark2'), name="Climate Model") +
    theme_bw() +
    theme(axis.title.y = element_text(size=32,vjust=1),
          axis.title.x = element_text(size=32,vjust=-1),
          axis.text.x  = element_text(size=24),
          axis.text.y  = element_text(size=24)) +
    #theme(legend.title=element_text(size=24)) +
    #theme(legend.text = element_text(size = 24)) +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size=40,vjust=1)) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    ggtitle("Final Five Timesteps") + 
    xlab("Timestep") +
    ylab("Climate Parameter Value") 


imagepath = "/Users/mmallek/Documents/Thesis/Plots/pdsi"
ggsave("future_last5timesteps.png", 
       path=imagepath,
       width=9, height=7, units='in',limitsize=FALSE)    

# boxplots of climate variable values ####
p3 = ggplot(pdsi4, aes(x=model, y=pdsival))
p3 + geom_boxplot(fill=c("#339900","#339900","#339900","#339900","#339900","#339900","#339900")) + #,"#E69F00")) + 
    theme_bw() +
    theme(axis.title.y = element_text(size=24,vjust=1),
          axis.title.x = element_text(size=24,vjust=-1),
          axis.text.x  = element_text(size=16),
          axis.text.y  = element_text(size=16)) +
    theme(legend.title=element_text(size=16)) +
    theme(legend.text = element_text(size = 16)) +
    theme(plot.title = element_text(size=24,vjust=1)) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    ggtitle("Differences in Climate Parameter Value across Climate Scenarios and Models") + 
    xlab("Climate Scenario") +
    ylab("Climate Parameter Value") 

# boxplots of climate variable values using full hrv parameter range ####
allscenarionames= c('HRV','CCSM-1','CCSM-2','CCSM-3','CCSM-4','CCSM-5','CCSM-6','ESM2M')

pdsi4.2 = pdsi2.1 # pdsi4.1
pdsi4.2$model[pdsi4.2$model=='hrv'] = "HRV"
pdsi4.2$model = factor(pdsi4.2$model, levels=c("HRV", "CCSM-1", "CCSM-5", "CCSM-4",
                                  "CCSM-6", "CCSM-2", "CCSM-3", "ESM2M"),
                                  ordered=T)
p5 = ggplot(pdsi4.2, aes(x=model, y=pdsival))
p5 + geom_boxplot(fill=c("#0099CC","#339900","#339900","#339900","#339900","#339900","#339900","#339900")) + 
    geom_hline(aes(yintercept=gmean), lty='dotted',lwd=3, col="#333333") +
    theme_bw() +
    theme(axis.title.y = element_text(size=32,vjust=1),
          axis.title.x = element_text(size=32,vjust=-1),
          axis.text.x  = element_text(size=24),
          axis.text.y  = element_text(size=24)) +
    theme(legend.title=element_text(size=16)) +
    theme(legend.text = element_text(size = 16)) +
    theme(plot.title = element_text(size=40,vjust=1)) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    #ggtitle("Differences in Climate Parameter Value across Climate Scenarios and Models") + 
    xlab("Climate Scenario") +
    ylab("Climate Parameter Value") 

# boxplots of full hrv vs. all future ####
pdsi4.3 = pdsi4.1
pdsi4.3[pdsi4.3$model!='hrv',2] = 'future'
p7 = ggplot(pdsi4.3, aes(x=model, y=pdsival))
p7 + geom_boxplot(fill=c("#0099CC","#339900")) + 
    theme_bw() +
    theme(axis.title.y = element_text(size=24,vjust=1),
          axis.title.x = element_text(size=24,vjust=-1),
          axis.text.x  = element_text(size=16),
          axis.text.y  = element_text(size=16)) +
    theme(legend.title=element_text(size=16)) +
    theme(legend.text = element_text(size = 16)) +
    theme(plot.title = element_text(size=24,vjust=1)) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    ggtitle("Differences in Climate Parameter Value Between Historical and Future Scenarios") + 
    xlab("Climate Scenario") +
    ylab("Climate Parameter Value") 


# Trend line for PDSI ####
# this plot used in FRV article
p6 = ggplot(pdsi4, aes(x=Timestep, y=pdsival,col=model))
p6 + geom_line(size=1, alpha=0.8) + 
    geom_smooth(method= 'lm', se=F, size=1.5, linetype=2) +
    scale_colour_manual(values=mycols, name="Climate Model") +
    expand_limits(y=0) +
    theme_bw() +
    theme(axis.title.y = element_text(size=32,vjust=1),
          axis.title.x = element_text(size=32,vjust=-1),
          axis.text.x  = element_text(size=24),
          axis.text.y  = element_text(size=24)) +
    theme(legend.title=element_text(size=24)) +
    theme(legend.text = element_text(size = 24)) +
    theme(plot.title = element_text(size=40,vjust=1)) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    theme(legend.key.height=unit(2,"line")) + 
    ggtitle("Climate Parameter Trajectory") + 
    xlab("Timestep") +
    ylab("Climate Parameter Value") 

imagepath = "/Users/mmallek/Documents/Thesis/Plots/pdsi"
ggsave("futureclimate_wlm.png", 
       path=imagepath,
       width=12, height=7, units='in',limitsize=FALSE)    

##############################################################
##############################################################
# boxplots of PDSI without HRV or any median lines ####
pdsi2$model = factor(pdsi2$model, levels=fscenario.levels, ordered=T)

p8 = ggplot(pdsi2, aes(x=model, y=pdsival))
p8 + geom_boxplot(fill=c("#339900","#339900","#339900","#339900","#339900","#339900","#339900")) + #,"#E69F00")) + 
    theme_bw() +
    theme(axis.title.y = element_text(size=32,vjust=1),
          axis.title.x = element_text(size=32,vjust=-1),
          axis.text.x  = element_text(size=24),
          axis.text.y  = element_text(size=24)) +
    theme(plot.title = element_text(size=40,vjust=1)) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    ggtitle("Climate Parameter Distribution") + 
    xlab("Climate Scenario") +
    ylab("Climate Parameter Value") 

imagepath = "/Users/mmallek/Documents/Thesis/Plots/pdsi"
ggsave("pdsiboxplots_ordered_nohrv.png", 
       path=imagepath,
       width=12, height=6, units='in',limitsize=FALSE)    

##############################################################
##############################################################
#### Stats on PDSI ####

(avg = apply(pdsi[,2:8], 2, mean))
(median = apply(pdsi[,2:8], 2, median))

hrvmean = mean(hrvpdsi$pdsival)
hrvmedian = median(hrvpdsi$pdsival)

# proportional difference of mean ####
(propdiff = avg/hrvmean)
#propdiff2 = propdiff - 1
propdiff2 = propdiff

#  ccsm4_pdsi1  ccsm4_pdsi2  ccsm4_pdsi3  ccsm4_pdsi4  ccsm4_pdsi5  ccsm4_pdsi6  esm2m_pdsi1 
# -0.007038995  0.269075444  0.338506693  0.094716869  0.130904185  0.272385767  0.604192545 

# proportional difference of median ####
(propdiff = avg/hrvmedian)
#propdiff3 = propdiff - 1
propdiff3 = propdiff

# historical darea data #### 
hrvrun = dareaout[[2]]$`Wildfire disturbance summary (percent)`$`run number 1`
hrvrun$mort.low = as.numeric(as.character(hrvrun$mort.low))
hrvrun$mort.high = as.numeric(as.character(hrvrun$mort.high))
hrvrun$mort.any = as.numeric(as.character(hrvrun$mort.any))
#summary statistic mort.low mort.high mort.any
#1 minimum darea/timestep     0.09      0.01      0.1
#2 maximum darea/timestep    49.33     25.98    70.09
#3  median darea/timestep     9.54       4.4    13.72
#4    mean darea/timestep    12.31       5.7    18.01

# expected mean darea/timestep ####
expmean = data.frame(model = character(), mort.low = numeric(), mort.high = numeric(), mort.any = numeric(), stringsAsFactors=F)
for(i in 1:length(propdiff2)){
    expmean[i,1] = names(propdiff2)[i]
    expmean[i,2:4] = hrvrun[4,2:4] * (propdiff2[i])
    
}
expmean

# expected median darea/timestep ####
expmedian = data.frame(model = character(), mort.low = numeric(), mort.high = numeric(), mort.any = numeric(), stringsAsFactors=F)
for(i in 1:length(propdiff3)){
    expmedian[i,1] = names(propdiff3)[i]
    expmedian[i,2:4] = hrvrun[3,2:4] * (propdiff3[i])    
}
expmedian

##############################

# pooled output data for each scenario
as.data.frame(df)
# mean darea/timestep
mort_level  value session
mort.low 10.582       9
mort.high  3.524       9
mort.any 14.106       9
mort.low 12.894       8
mort.high  6.653       8
mort.any 19.548       8
mort.low 12.365      10
mort.high  6.775      10
mort.any 19.140      10
mort.low 11.756      13
mort.high  5.377      13
mort.any 17.132      13
mort.low 10.373      14
mort.high  3.760      14
mort.any 14.133      14
mort.low 12.523      20
mort.high  6.174      20
mort.any 18.697      20
mort.low 12.559      21
mort.high  8.995      21
mort.any 21.554      21

df2 = spread(df, mort_level, value)
df2 = as.data.frame(df2)


model       mort.low mort.high mort.any
1 ccsm4_pdsi1 12.22335  5.659878 17.88323
2 ccsm4_pdsi2 15.62232  7.233730 22.85605
3 ccsm4_pdsi3 16.47702  7.629488 24.10651
4 ccsm4_pdsi4 13.47596  6.239886 19.71585
5 ccsm4_pdsi5 13.92143  6.446154 20.36758
6 ccsm4_pdsi6 15.66307  7.252599 22.91567
7 esm2m_pdsi1 19.74761  9.143898 28.89151

summary_stat            session mort.any mort.high mort.low
1 mean darea/timestep       8   19.548     6.653   12.894
2 mean darea/timestep       9   14.106     3.524   10.582
3 mean darea/timestep      10   19.140     6.775   12.365
4 mean darea/timestep      13   17.132     5.377   11.756
5 mean darea/timestep      14   14.133     3.760   10.373
6 mean darea/timestep      20   18.697     6.174   12.523
7 mean darea/timestep      21   21.554     8.995   12.559


# need a line plot to show increase in darea

pl = ggplot(expmean, aes(factor(model), mort.any))
pl + geom_point(col='red') + geom_point(propdiff, aes(factor(names(propdiff)),as.vector(propdiff)))


#### Plot HRV PDSI Values ####
# no more excel!

require(ggplot2)
require(tidyr)
require(dplyr)
require(RColorBrewer)
require(grid)

pdsi = read.csv('/Users/mmallek/Tahoe/RMLands/parameters/climate_mean_5yr.csv')
pdsi = pdsi[1:60,]
pdsi$year = seq(1550,1845,5)

ggplot(pdsi, aes(x=year, y=Climate.Modifier)) + geom_line(col='blue', size=1.2) +
    theme_bw() +
    theme(axis.title.y = element_text(size=36,vjust=1),
          axis.title.x = element_text(size=36,vjust=-1),
          axis.text.x  = element_text(size=24),
          axis.text.y  = element_text(size=24)) +
    #theme(legend.title=element_text(size=16)) +
    #theme(legend.text = element_text(size = 16)) +
    theme(plot.title = element_text(size=40,vjust=1)) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    ggtitle("Climate Parameter Trajectory") + 
    xlab("Year") +
    ylab("Climate Parameter Value") 

ggsave('hrv-bigtext.png', 
    path='/Users/mmallek/Documents/Thesis/Plots/pdsi',
    width=10, height=7, units='in',limitsize=FALSE)  
