# code to build a horizontal bar plot out of the range of variability results


# first set: fragLAND results
df = fraglandout[[3]]
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
df_pd = df[1,] # only PD, for testing
#data_long <- gather(df_pd, key, value, 2:9)
data_long <- gather(df, key, value, 2:9)
data_long <- gather(dfnosimi, key, value, 2:9)


str(data_long)
data_long$diff = c(data_long$value[1],diff(data_long$value))

# color settings should be for all plots
theme_set(theme_bw())
myPalette = c('#FF3300','#FF6600','#009900','#006633','#009900','#FF6600','#FF3300','#333333')
scale_colour_manual(values=myPalette)

# barplot didn't really work for me
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

# doesn't work when data are already percentiled
ggplot(data=data_long[1:7,], aes(x=landscape.metric[1:7], y=value[1:7])) +
    geom_boxplot() + geom_point(aes(col=key))
##########################
# use this code for fragland

# all the plots - DONE
df = fraglandout[[3]]
df = df[,c(1:8,10)]
myPalette = c('#FF3300','#FF9933','#009900','#0066CC','#009900','#FF9933','#FF3300','#333333')
theme_set(theme_grey()) 
    
for(i in 1:nrow(df)){
    df_metric = df[i,]
    df_metric <- gather(df_metric, key, value, 2:9)
    pl = ggplot(data=df_metric, aes(x=landscape.metric, y=value)) +
        geom_point(aes(col=key),size=25, shape=124) + 
        scale_colour_manual(values=myPalette, name='Simulated Range of \nVariability Percentile',
                            labels=c("0th", "5th", "25th","50th", "75th", "95th", "100th", "Current Value")) +
        coord_flip()  + ylab("Value") +
        theme(axis.title.y = element_blank(),
              axis.title.x = element_text(size=24),
              axis.text.x  = element_text(size=24),
              axis.text.y  = element_text(size=24)) +
        scale_x_discrete(breaks=NULL) +
        theme(legend.title=element_text(size=16)) +
        theme(legend.text = element_text(size = 16)) +
        # if no legend uncomment line below
        #theme(legend.position = "none") +
        theme(legend.position = "top") +
        ggtitle(paste("Landscape Metric: ", df_metric[1,1],sep='')) + 
        theme(plot.title = element_text(size=32,vjust=2))
    print(pl)
    ggsave(filename=paste(df_metric[1,1],"srvplot","pdf",sep="."),
           path="/Users/mmallek/Tahoe/Report2/images/LandscapeFragPlots_wlegend/",
           width=16, height=3.7, units='in',limitsize=FALSE)
           # if no legend uncomment line below
           #path="/Users/mmallek/Tahoe/Report2/images/LandscapeFragPlots_nolegend/",
           #width=16, height=2.5, units='in',limitsize=FALSE)
}


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
        theme(legend.text = element_text(size = 16)) +
        ggtitle("Landscape Metric") + 
        theme(plot.title = element_text(size=24,vjust=1))
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

############################################
# code from another set of owrk
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

###############################################
#second set: fragCLASS results

# IMPORTANT - right now this works with making one fragstats output for each cover uniquely: no minimum ha argument exists for the class metrics
theme_set(theme_grey())
myPalette = c('#FF3300','#FF9933','#009900','#0066CC','#009900','#FF9933','#FF3300','#333333')
#myPalette = c('#00000000','#FF3300','#FF9933','#009900','#009900','#FF9933','#FF3300','#0066CC','#333333')


# testing code to extract one set of results at a time and get rid of extra columns
#temp = df[[1]]$`class SRV` # early all
#temp[,c(1:8,10)]

df = fragclassout[[4]]#$SMC_X_LATE_OP$`class SRV`

for (i in 1:7){
    cond = df[[i]]$`class SRV`[,c(1:8,10)]
    # try a loop
    for(j in 1:nrow(cond)){
        df_metric = cond[j,]
        df_metric <- gather(df_metric, key, value, 2:9)
        pl = ggplot(data=df_metric, aes(x=class.metric, y=value)) +
            geom_point(aes(col=key),size=25, shape=124) + 
            scale_colour_manual(values=myPalette, name='Simulated Range of \nVariability Percentile',
                                labels=c("0th", "5th", "25th","50th", "75th", "95th", "100th", "Current Value")) +
            coord_flip()  + ylab("Value") +
            theme(axis.title.y = element_blank(),
                  axis.title.x = element_text(size=24),
                  axis.text.x  = element_text(size=24),
                  axis.text.y  = element_text(size=24)) +
            theme(legend.title=element_text(size=24)) +
            theme(legend.text = element_text(size = 24)) +
            scale_x_discrete(breaks=NULL) +
            ggtitle(paste(names(df[i]), ": ",df_metric[1,1], sep='')) + 
            #theme(legend.position = "top") +
            # for no legend uncomment next line
            theme(legend.position = "none") + 
            theme(plot.title = element_text(size=24,vjust=2))
        print(pl)
        ggsave(filename=paste(df_metric[1,1],names(df)[i], "srvplot",".pdf",sep="-"), 
               #path="/Users/mmallek/Tahoe/Report2/images/ClassFragPlots_wlegend/",
               #width=20, height=3.9, units='in',limitsize=FALSE)    
               # for no legend uncomment next lines
               path="/Users/mmallek/Tahoe/Report2/images/ClassFragPlots_nolegend/",
               width=20, height=2.5, units='in',limitsize=FALSE)    
    }  
}



###############################################
#third set: composition results (covcond)
################################################
require(ggplot2)
require(tidyr)
require(dplyr)
myPalette = c('#FF3300','#FF9933','#009900','#0066CC','#009900','#FF9933','#FF3300','#333333')
myPalette = c('#FF330000','#FF3300','#FF9933','#009900','#009900','#FF9933','#FF3300')#,'#0066CC','#333333')
savenames = c('megm','megx','ocfw','ocfwu','rfrm','rfrx','smcm','smcu','smcx')

theme_set(theme_bw())
df = covcondout[[5]]
df = df[,c(2:10,12)]

#df$diff1 = diff(df[4])

                                      
df_metric <- gather(df, key, measurement, 3:10)


temp = df_metric[df_metric$cover.type=='Mixed Evergreen - Mesic' & 
        df_metric$condition.class=='Early-All Structures' & df_metric$key!='current.%cover',]

current = df_metric[df_metric$cover.type=='Mixed Evergreen - Mesic' & 
            df_metric$condition.class=='Early-All Structures' & df_metric$key=='current.%cover',]


# loop to make all the figures - DONE
for (i in 1:length(unique(df_metric$cover.type))){
    for (j in 1:7){
        hrv = df_metric[df_metric$cover.type==unique(df_metric$cover.type)[i] & 
            df_metric$condition.class==unique(df_metric$condition.class)[j] & df_metric$key!='current.%cover',] 
        hrv$diff = c(hrv$measurement[1],diff(hrv$measurement))
        current = df_metric[df_metric$cover.type==unique(df_metric$cover.type)[i] & 
            df_metric$condition.class==unique(df_metric$condition.class)[j] & df_metric$key=='current.%cover',]
        per50 = df_metric[df_metric$cover.type==unique(df_metric$cover.type)[i] & 
            df_metric$condition.class==unique(df_metric$condition.class)[j] & df_metric$key=='srv50%',]
        temp = bind_rows(per50, current)
        #names(per50)[4] = "meas50"
        pl = ggplot(data=df_metric, aes(x=condition.class)) +
            geom_bar(data=hrv, stat="identity", aes(y=diff, fill=key)) + coord_flip() +
            ##geom_point(data=per50, shape=124, size=15,aes(x=condition.class,y=measurement),show_guide=FALSE, color="#0066CC") +
            ##geom_point(data=current, shape=124, size=10,aes(x=condition.class,y=measurement),show_guide=FALSE, color="black") +
            geom_point(data=temp, shape=124, size=15, aes(y=measurement, colour=key)) +
            scale_colour_manual(values=c("#0066CC","#333333"), labels=c("50th", "Current"),name="") +
            scale_fill_manual(values=myPalette,
                              name='Simulated Range of \nVariability Percentile',
                             labels=c("","0th-5th", "5th-25th", "25th-50th","50th-75th", "75th-95th", "95th-100th"),drop=T) +
            ylab("Percent of Cover Extent") +
            theme(axis.title.y = element_blank(),
                  axis.title.x = element_text(size=16),
                  axis.text.x  = element_text(size=16),
                  axis.text.y  = element_text(size=16)) +
            theme(legend.title=element_text(size=16)) +
            theme(legend.text = element_text(size = 16)) +
            #theme(legend.position = "top") +
            ###scale_x_discrete(breaks=NULL) +
            # for no legend uncomment next line
            theme(legend.position = "none") 
            # for no title comment next lines
            #ggtitle(unique(df_metric$cover.type)[i]) + 
            #theme(plot.title = element_text(size=24,vjust=1))
            
            print(pl)
            ggsave(filename=paste(savenames[i],unique(df_metric$condition.class)[j],"srvplot",".pdf",sep="_"), 
                #path="/Users/mmallek/Tahoe/Report2/images/CovcondHRVBarplots/",
                # for no legend uncomment next line
                path="/Users/mmallek/Tahoe/Report2/images/CovcondHRVBarplots_nolegend/",
                #width=13,height=3.1, units='in',limitsize=FALSE)
                 #for no legend/title uncomment next line
                width=13,height=1.2, units='in',limitsize=FALSE)

        }
    }


# testing code to extract one set of results at a time and get rid of extra columns
#temp = df[[1]]$`class SRV` # early all
#temp[,c(1:8,10)]


