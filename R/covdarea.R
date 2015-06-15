### Function to calculate percent of a cover area disturbed every time step

path = '/Users/mmallek/Tahoe/ClusterBackup/november2014/csvs_upto_s20/'
session = 20
darea = read.csv(paste(path, "darea.csv", sep=''))
covarea <-read.csv('/Users/mmallek/Tahoe/R/cover_area.csv',header=TRUE, stringsAsFactors=F)

for(i in 1:max(darea$timestep.id)){
    for(j in sorted(unique(darea$cov.id))){
        
    }
}
i = 1
j = 5
newdf = subset(darea, subset=c(darea$session.id==session & darea$timestep.id==i & darea$cov.id==j), select=c(mort.high:mort.any))
newdf2 = data.frame(timestep=numeric(), cover=character(), mort.high=numeric(), mort.low=numeric(), mort.any=numeric(), stringsAsFactors=F)
newdf2[i,1] = i
newdf2[i,2] = covarea$cov.name[j]
newdf2[i,3] = sum(newdf$mort.high)
newdf2[i,4] = sum(newdf$mort.low)
newdf2[i,5] = sum(newdf$mort.any)
