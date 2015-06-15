# code to build a horizontal bar plot out of the range of variability results

df = temp[[3]]
names(df)[c(1, 10)] = c('Landscape.Metric','Current Value')
df = df[,c(1:8,10)]
dfnosimi = df[-11,]



require(tidyr)
require(ggplot2)
# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
df_pd = df[1,]
data_long <- gather(df_pd, key, value, 2:9)
data_long <- gather(df, key, value, 2:9)
data_long <- gather(dfnosimi, key, value, 2:9)


str(data_long)
data_long$diff = c(data_long$value[1],diff(data_long$value))

theme_set(theme_bw())
myPalette = c('#FF3300','#FF6600','#009900','#006633','#009900','#FF6600','#FF3300','#333333')
scale_colour_manual(values=myPalette)

ggplot(data=data_long, aes(x=landscape.metric, y=diff, fill=key)) +
    geom_bar(stat="identity", position='fill') + coord_flip() #+
    geom_point(aes(x=value))

ggplot(data=data_long, aes(x=landscape.metric, y=value)) +
    geom_point(aes(col=key),size=5) + scale_colour_manual(values=myPalette) +
    coord_flip() + ylab("Value") + xlab("Landscape Metric")  + 
    theme(axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          axis.text.x  = element_text(size=16),
          axis.text.y  = element_text(size=16))

ggplot(data=data_long[1:7,], aes(x=landscape.metric[1:7], y=value[1:7])) +
    geom_boxplot() + geom_point(aes(col=key))

# try a loop
for(i in 1:nrow(df)){
    df_metric = df[i,]
    df_metric <- gather(df_metric, key, value, 2:9)
    pl = ggplot(data=df_metric, aes(x=Landscape.Metric, y=value)) +
        geom_point(aes(col=key),size=5) + 
        scale_colour_manual(values=myPalette, name='Simulated Range of \nVariability Percentile',
                            labels=c("0th", "5th", "25th","50th", "75th", "95th", "100th", "Current Value")) +
        coord_flip()  + ylab("Value") +
        theme(axis.title.y = element_blank(),
              axis.title.x = element_text(size=24),
              axis.text.x  = element_text(size=16),
              axis.text.y  = element_text(size=16)) +
        theme(legend.title=element_text(size=16)) +
        theme(legend.text = element_text(size = 16))
    print(pl)
    #ggsave(paste(df_metric[1,1],"srvplot","pdf",sep=".")) 
}

# working code
ggplot(data=data_long, aes(x=landscape.metric, y=value)) +
    geom_point(aes(col=key),size=5) + scale_colour_manual(values=myPalette) +
    coord_flip() + ylab("Value") + xlab("Landscape Metric")  + 
    theme(axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          axis.text.x  = element_text(size=16),
          axis.text.y  = element_text(size=16)) + facet_wrap(~landscape.metric)

# For fancy double plot of climate and darea

climdarea$timestep = seq(40,500,1)
rect1<- data.frame (xmin=260, xmax=265, ymin=-Inf, ymax=Inf)
rect2 <- data.frame (xmin=290, xmax=295, ymin=-Inf, ymax=Inf)
require(gridExtra)
p3 = ggplot() + theme_gray() + geom_line(data=climdarea, aes(timestep, climate)) + 
    scale_x_continuous(limits = c(250,311)) +
    #scale_y_continuous(limits=c(0.6,1.6)) +
    labs(x="Timestep") + ylab("Climate Parameter") +
    geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax), alpha=0.1,fill="blue")+
    geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")+
    theme(plot.margin = unit(c(-1,0.5,0.5,0.5), "lines"))
p4 = ggplot() + theme_gray() + geom_line(data=climdarea, aes(timestep, darea)) + 
    ggtitle("Disturbed Area, Climate Parameter by Simulation Timestep") +
    scale_x_continuous(limits = c(250,311)) +
    scale_y_continuous(limits=c(0,60)) +
    geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue") +
    geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          plot.title=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = unit(c(0.5,0.5,-1,0.5), "lines")) +
    ylab("Disturbed area % of landscape")

gp1 = ggplot_gtable(ggplot_build(p3))
gp2 = ggplot_gtable(ggplot_build(p4))
maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth
grid.arrange(gp2, gp1) 

