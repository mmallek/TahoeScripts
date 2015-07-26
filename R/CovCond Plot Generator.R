###############################################
#composition results (covcond)
################################################

# first, save the results of covcond to an object. I called it covcondout and this is hardwired.
# if you are doing something different from the 9 focal cover types, this script may have to be tweaked

require(ggplot2)
require(tidyr)
require(dplyr)
myPalette = c('#FF330000','#FF3300','#FF9933','#009900','#009900','#FF9933','#FF3300')
savenames = c('megm','megx','ocfw','ocfwu','rfrm','rfrx','smcm','smcu','smcx')

theme_set(theme_bw())
df = covcondout[[5]]
df = df[,c(2:10,12)]

df_metric <- gather(df, key, measurement, 3:10)


# loop to make all the figures 
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
        pl = ggplot(data=df_metric, aes(x=condition.class)) +
            geom_bar(data=hrv, stat="identity", aes(y=diff, fill=key)) + coord_flip() +
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
