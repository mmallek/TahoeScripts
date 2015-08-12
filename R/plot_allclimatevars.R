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

p1 = ggplot(pdsi2, aes(x=timestep, y=pdsival))
p1 + geom_line(aes(color=factor(pdsi2$model)))

# 500 timesteps long climate parameter file (hrv)
hrvpdsi = read.csv('/Users/mmallek/Tahoe/RMLands/parameters/climate_mean_5yr.csv')
hrvpdsi$model = 'hrv'
hrvpdsi$pdsival = hrvpdsi$Climate.Modifier
hrvpdsi$Climate.Modifier = NULL
colnames(hrvpdsi)[1] = 'timestep'
hrvpdsi = hrvpdsi[1:500,]

# add hrv to future pdsi thing
pdsi2.1 = pdsi2[pdsi2$model!='hrv',]
pdsi2.1 = bind_rows(pdsi2.1,hrvpdsi)



# calculate average pdsi value for each model run
avg = apply(pdsi[,-1], 2, mean)
avg
mean(hrvpdsi$pdsival)
order(avg)

pdsi_ordered = pdsi[,order(avg)+1]

# make data frame for reordered columns
pdsi3 = cbind(pdsi[,1],pdsi_ordered)
colnames(pdsi3)[1] = "timestep"

# gather for plotting
pdsi3 = gather(pdsi3, model, pdsival, -timestep)

# create the plot
p2 = ggplot(pdsi3, aes(x=timestep, y=pdsival))
p2 + geom_line(aes(color=factor(pdsi3$model)))

# generate color palette for this plot
mycols = brewer.pal(8, "RdYlGn")
mycols = rev(mycols)
mycols = brewer.pal(7, "Dark2")
mycols = append(mycols,"#000000", after=1)

# make hrv black
mycols[2] = "#000000"
colnames(pdsi3)[3] = "pdsival"

p2 = ggplot(pdsi3, aes(x=timestep, y=pdsival))
p2 + geom_line(aes(col=pdsi3$model),size=1.5) + 
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
pdsi3_end = pdsi3[pdsi3$timestep > 13 & pdsi3$model!='hrv',]
p4 = ggplot(pdsi3_end, aes(x=timestep, y=pdsival))
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
p3 = ggplot(pdsi2, aes(x=model, y=pdsival))
p3 + geom_boxplot(fill=c("#339900","#339900","#339900","#339900","#339900","#339900","#339900","#E69F00")) + 
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
p5 = ggplot(pdsi2.1, aes(x=model, y=pdsival))
p5 + geom_boxplot(fill=c("#339900","#339900","#339900","#339900","#339900","#339900","#339900","#E69F00")) + 
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
