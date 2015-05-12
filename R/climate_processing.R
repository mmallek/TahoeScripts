setwd("/Users/mmallek/Documents/Thesis/GISData")
# load data, downloaded from the CMIP5 website
ccsm4_r1 = read.csv("CCSM4_r1i1p1_pdsi-all.tsv", header=TRUE, sep = "\t", skip = 1)
ccsm4_r2 = read.csv("CCSM4_r2i1p1_pdsi-all.tsv", header=TRUE, sep = "\t", skip = 1)
ccsm4_r3 = read.csv("CCSM4_r3i1p1_pdsi-all.tsv", header=TRUE, sep = "\t", skip = 1)
ccsm4_r4 = read.csv("CCSM4_r4i1p1_pdsi-all.tsv", header=TRUE, sep = "\t", skip = 1)
ccsm4_r5 = read.csv("CCSM4_r5i1p1_pdsi-all.tsv", header=TRUE, sep = "\t", skip = 1)
ccsm4_r6 = read.csv("CCSM4_r6i1p1_pdsi-all.tsv", header=TRUE, sep = "\t", skip = 1)

require(plyr)
# aggregating all the different data into a single data frame
# these data should already only include June, July, and August for each year on record
ccsm4 = join(ccsm4_r1, ccsm4_r2, by=c('degree_east','degree_north','months.since.1900.01.01'))
ccsm4 = join(ccsm4, ccsm4_r3, by=c('degree_east','degree_north','months.since.1900.01.01'))
ccsm4 = join(ccsm4, ccsm4_r4, by=c('degree_east','degree_north','months.since.1900.01.01'))
ccsm4 = join(ccsm4, ccsm4_r5, by=c('degree_east','degree_north','months.since.1900.01.01'))
ccsm4 = join(ccsm4, ccsm4_r6, by=c('degree_east','degree_north','months.since.1900.01.01'))

# be sure to identify columns correctly
ccsm4 = rename(ccsm4, replace=c('unitless' = 'pdsi1'))
names(ccsm4)
# define all names to be shorter and easier to understand
colnames(ccsm4) <- c("degree_east","degree_north", "months.since.1900.01.01", "pdsi1", "pdsi2","pdsi3","pdsi4","pdsi5","pdsi6")

# keep only those records from 2010 and later, in accordance with assumptions made earlier in the project
ccsm4 = ccsm4[ccsm4$months.since.1900.01.01 >= 1325.5,]
# convert months to years, but round down so that 2010.6 just becomes 2010
# so each year now has 3 entries with the same year
ccsm4$year = floor((ccsm4$months.since.1900.01.01 / 12) + 1900)

# since we relabeled each June-July-Aug to, e.g., 2015,2015,2015
# we can aggregate by the year and take the average value
temp = aggregate(ccsm4[,4:9], by=list(ccsm4$degree_east,ccsm4$degree_north,ccsm4$year),mean, na.rm=TRUE)
# aggregate messes up the column names so fix it by redefining them
names(temp)[1:3] = c('Long','Lat','year')
# this command is just sorting the data so it's by location instead of year
temp = temp[order(temp$Long,temp$Lat),]

# now we're preparing to aggregate into the five-year timesteps
# we sorted the data into sequentially increasing order by location
# so now we just add a timestep field. (2099-2010)/5 = 18   we have 21 data points.
# this produces 1 1 1 1 1 2 2 2 2 2 etc. So each year is getting assigned to a timestep.
temp$timestep = rep(rep(1:18, each=5), times=21)

# now we can aggregate by timestep, keeping locations separate. taking the average here.
# note we are aggregating each pdsi set (column) separately)
temp = aggregate(temp[,4:9], by=list(temp$Long,temp$Lat,temp$timestep),mean, na.rm=TRUE)
# aggregate messes up the column names so we fix them here.
names(temp)[1:3] = c('Long','Lat','timestep')

# check the range so we can pick one legit lat/long to test
range(temp$Lat) 
range(temp$Long)
### need to replace with UTMs!!!
centroid = c(-121,40)

# this should give every set of unique locations
pdsi_pts = unique(temp[,c('Long','Lat')])

# to calculate the inverse distance weighting, we made our own euclidean distance function
eucdist = function(x) sqrt(sum((centroid[1] - x[1])^2,(centroid[2] - x[2])^2))

# checking that this works
# apply the distance function column-wise
test = apply(pdsi_pts,1,eucdist)
### what does this do?
pdsi_pts = cbind(pdsi_pts, test)
# apply also doesn't come up with good names; fix it
names(pdsi_pts)[3] = 'eucdist'
### ??? now we square and take the inverse so the influence of farther away points is much smaller
pdsi_pts$dweight = 1 / (pdsi_pts$eucdist ^ 2)
### normalizing???
pdsi_pts$dweight = pdsi_pts$dweight / sum(pdsi_pts$dweight)

# now we have the relative weight of each location, so we want to merge that with our pdsi columns
test2 = merge(temp, pdsi_pts)
test2 = test2[order(test2$timestep,test2$Long,test2$Lat),]

# set up an empty matrix to hold the weight for each location
pdsi_wtd = matrix(NA,length(unique(test2$timestep)),6)
# had to make another function to do the inverse distance weighting for us
idw = function(x) sum(x*temp2$dweight)
### what does the loop do?
for (i in 1:length(unique(test2$timestep))) {
    temp2 = test2[test2$timestep==i,]
    pdsi_wtd[i,] = apply(temp2[4:9], 2, FUN=idw)
}
# convert to data frame
pdsi_wtd = as.data.frame(pdsi_wtd)
# fix names
names(pdsi_wtd) = names(temp2[4:9])
# make a timestep column for this data fram
pdsi_wtd$timestep = unique(test2$timestep)

# calculate mean and sd for each 5-year averaged pdsi value and store as objects
# for historic data
hist_mean = apply(pdsi_wtd[,1:6], 2, mean) # change numbers to column names (everywhere!)
hist_sd = apply(pdsi_wtd[,1:6], 2, sd)

## this is what the excel sheet formulas looked like
# =(F3-AVERAGE($F$2:$F$61))/2*STDEVP($F$2:$F$61)+1
# =2*AVERAGE($G$2:$G$61)-G2
#
# =2*AVERAGE($G$2:$G$61)-(F3-AVERAGE($F$2:$F$61))/2*STDEVP($F$2:$F$61)+1

### another function to do the rescaling stuff (get more details from excel sheet)
make_clim_mod = function(x){
    mu = (x-hist_mean[i])/2*hist_sd[i] + 1
    2 * mean(mu) - mu # the 2 * maintains that mean at whatever it is
}

# make empty matrix to hold this
clim_mods = matrix(NA,length(unique(test2$timestep)),6)
# this makes the climate modifiers for each pdsi column ready to input into rmlands
for (i in 1:length(hist_mean)){
    clim_mods[,i] = make_clim_mod(pdsi_wtd[,i])
}

# verify: should all have mean of 1 (shows they used the right mean, not the same mean many times)
apply(clim_mods,2,mean)

