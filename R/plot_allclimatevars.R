#### PDSI Plots
### need to show pdsi curves for all future scenarios
require(ggplot2)
require(tidyr)
require(dplyr)
require(RColorBrewer)
require(grid)

# 18 timesteps long climate parameter file
pdsi = read.csv("/Users/mmallek/Documents/Thesis/GISData/pdsivalues_allruns.csv")
pdsi2 = gather(pdsi, model, pdsival, -timestep)

# plot all models plus hrv
p1 = ggplot(pdsi2, aes(x=timestep, y=pdsival))
p1 + geom_line(aes(color=factor(pdsi2$model)))

# remove hrv from future climate params
pdsi3 = pdsi2[pdsi2$model!='hrv',]

# 500 timesteps long climate parameter file (hrv)
hrvpdsi = read.csv('/Users/mmallek/Tahoe/RMLands/parameters/climate_mean_5yr.csv')
hrvpdsi$model = 'hrv'
hrvpdsi$pdsival = hrvpdsi$Climate.Modifier
hrvpdsi$Climate.Modifier = NULL
colnames(hrvpdsi)[1] = 'timestep'
hrvpdsi = hrvpdsi[1:500,]

# add hrv to future pdsi thing
pdsi3.1 = bind_rows(pdsi3,hrvpdsi)



# calculate average pdsi value for each model run
avg = apply(pdsi[,2:8], 2, mean)
avg
mean(hrvpdsi$pdsival)
order(avg)

pdsi_ordered = pdsi[,order(avg)+1]

# make data frame for reordered columns
pdsi4 = cbind(pdsi[,1],pdsi_ordered)
colnames(pdsi4)[1] = "timestep"

# gather for plotting
pdsi4 = gather(pdsi4, model, pdsival, -timestep)

# add hrv to future pdsi thing
pdsi4.1 = bind_rows(pdsi4,hrvpdsi)

# create the plot
p2 = ggplot(pdsi4, aes(x=timestep, y=pdsival))
p2 + geom_line(aes(color=factor(pdsi4$model)))

# generate color palette for this plot
#mycols = brewer.pal(8, "RdYlGn")
#mycols = rev(mycols)
mycols = brewer.pal(7, "Dark2")
#mycols = append(mycols,"#000000", after=1)

# make hrv black
#mycols[2] = "#000000"
#colnames(pdsi3)[3] = "pdsival"

# plot of full 18 timesteps
p2 = ggplot(pdsi4, aes(x=timestep, y=pdsival))
p2 + geom_line(aes(col=pdsi4$model),size=1.5) + 
    scale_colour_manual(values=mycols, name="Climate Model") +
    theme_bw() +
    theme(axis.title.y = element_text(size=24,vjust=1),
          axis.title.x = element_text(size=24,vjust=-1),
          axis.text.x  = element_text(size=16),
          axis.text.y  = element_text(size=16)) +
    theme(legend.title=element_text(size=16)) +
    theme(legend.text = element_text(size = 16)) +
    theme(plot.title = element_text(size=24,vjust=1)) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    ggtitle("Climate Parameter Trajectory") + 
    xlab("Timestep") +
    ylab("Climate Parameter Value") 

### only look at final 5 timesteps
# with line for hrv mean (~1)
pdsi4_end = pdsi4[pdsi4$timestep > 13 & pdsi4$model!='hrv',]
p4 = ggplot(pdsi4_end, aes(x=timestep, y=pdsival))
p4 + geom_line(aes(col=pdsi3_end$model),size=1.5) + 
    geom_hline(aes(yintercept=1)) +
    scale_colour_manual(values=brewer.pal(7,'Dark2'), name="Climate Model") +
    theme_bw() +
    theme(axis.title.y = element_text(size=24,vjust=1),
          axis.title.x = element_text(size=24,vjust=-1),
          axis.text.x  = element_text(size=16),
          axis.text.y  = element_text(size=16)) +
    theme(legend.title=element_text(size=16)) +
    theme(legend.text = element_text(size = 16)) +
    theme(plot.title = element_text(size=24,vjust=1)) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    ggtitle("Climate Parameter Trajectory") + 
    xlab("Timestep") +
    ylab("Climate Parameter Value") 



# boxplots of climate variable values
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

# boxplots of climate variable values using full hrv parameter range
pdsi4.2 = pdsi4.1
pdsi4.2$model = factor(pdsi4.2$model, levels=c("hrv", "ccsm4_pdsi1", "ccsm4_pdsi4", "ccsm4_pdsi5",
                                  "ccsm4_pdsi2", "ccsm4_pdsi6", "ccsm4_pdsi3", "esm2m_pdsi1"),
                                  ordered=T)
p5 = ggplot(pdsi4.2, aes(x=model, y=pdsival))
p5 + geom_boxplot(fill=c("#E69F00","#339900","#339900","#339900","#339900","#339900","#339900","#339900")) + 
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



##############################################################
##############################################################
#### Stats on PDSI

(avg = apply(pdsi[,2:8], 2, mean))
(median = apply(pdsi[,2:8], 2, median))

hrvmean = mean(hrvpdsi$pdsival)
hrvmedian = median(hrvpdsi$pdsival)

(propdiff = avg/hrvmean)
propdiff2 = propdiff - 1
propdiff2

#  ccsm4_pdsi1  ccsm4_pdsi2  ccsm4_pdsi3  ccsm4_pdsi4  ccsm4_pdsi5  ccsm4_pdsi6  esm2m_pdsi1 
# -0.007038995  0.269075444  0.338506693  0.094716869  0.130904185  0.272385767  0.604192545 

dareaout[1]
#           ccsm4_pdsi2     ccsm4_pdsi1    ccsm4_pdsi3    ccsm4_pdsi4    ccsm4_pdsi5    ccsm4_pdsi6    esm2m_pdsi1
#low.mort   7.58            12.88           14.46           10.79           8.53         10.08          7.92
#high.mort  4.87            3.92            12.48           6.53            2.90         5.01           6.52
#any.mort  12.45            16.80           26.94           17.33           11.44        15.09          14.44