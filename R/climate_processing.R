setwd("/Users/mmallek/Documents/Thesis/GISData")
# load data, downloaded from the CMIP5 website
ccsm4_r1 = read.csv("CCSM4_r1i1p1_pdsi-all.tsv", header=TRUE, sep = "\t", skip = 1)
ccsm4_r2 = read.csv("CCSM4_r2i1p1_pdsi-all.tsv", header=TRUE, sep = "\t", skip = 1)
ccsm4_r3 = read.csv("CCSM4_r3i1p1_pdsi-all.tsv", header=TRUE, sep = "\t", skip = 1)
ccsm4_r4 = read.csv("CCSM4_r4i1p1_pdsi-all.tsv", header=TRUE, sep = "\t", skip = 1)
ccsm4_r5 = read.csv("CCSM4_r5i1p1_pdsi-all.tsv", header=TRUE, sep = "\t", skip = 1)
ccsm4_r6 = read.csv("CCSM4_r6i1p1_pdsi-all.tsv", header=TRUE, sep = "\t", skip = 1)
esm2m = read.csv("GFDL_ESM2M_pdsi-all.tsv", header=TRUE, sep = "\t", skip = 1)
test = read.csv("past_climate_5yr.csv", header=TRUE)


require(plyr)
# aggregating all the different data into a single data frame
# these data should already only include June, July, and August for each year on record
ccsm4 = join(ccsm4_r1, ccsm4_r2, by=c('degree_east','degree_north','months.since.1900.01.01'))
ccsm4 = join(ccsm4, ccsm4_r3, by=c('degree_east','degree_north','months.since.1900.01.01'))
ccsm4 = join(ccsm4, ccsm4_r4, by=c('degree_east','degree_north','months.since.1900.01.01'))
ccsm4 = join(ccsm4, ccsm4_r5, by=c('degree_east','degree_north','months.since.1900.01.01'))
ccsm4 = join(ccsm4, ccsm4_r6, by=c('degree_east','degree_north','months.since.1900.01.01'))

# ESM2M data in different coordinates, so we can't merge this yet

# be sure to identify columns correctly
names(ccsm4)
names(esm2m)
# define all names to be shorter and easier to understand
# NOTE: PDSI 1-6 are all the CCSM4 model, which has 6 different runs. PDSI 7 is the ESM2M model, which only had 1 run
colnames(ccsm4) <- c("degree_east","degree_north", "months", "ccsm4_pdsi1", "ccsm4_pdsi2","ccsm4_pdsi3","ccsm4_pdsi4","ccsm4_pdsi5","ccsm4_pdsi6")
colnames(esm2m) <- c("degree_east","degree_north", "months", "esm2m_pdsi1")

# keep only those records from 2010 and later, in accordance with assumptions made earlier in the project
ccsm4 = ccsm4[ccsm4$months >= 1325.5,]
esm2m = esm2m[esm2m$months >= 1325.5,]

# convert months to years, but round down so that 2010.6 just becomes 2010
# so each year is now represented by a new column that has 3 entries with the same year
ccsm4$year = floor((ccsm4$months / 12) + 1900)
esm2m$year = floor((esm2m$months / 12) + 1900)

# since we relabeled each June-July-Aug to, e.g., 2015,2015,2015
# we can aggregate by the year and take the average value
# Kevin said to replace column number with variable name, but this doesn't appear to work
# instead, here confirm that correct column numbers are being used. there will never be that many columns
# to turn into script, can start at 4 and go to length of number of input datasets or something
ccsm4 = aggregate(ccsm4[,4:9], by=list(ccsm4$degree_east,ccsm4$degree_north,ccsm4$year),mean, na.rm=TRUE)
esm2m = aggregate(esm2m[,4], by=list(esm2m$degree_east,esm2m$degree_north,esm2m$year),mean, na.rm=TRUE)

# aggregate messes up the column names so fix it by redefining them
names(ccsm4)[1:3] = c('long','lat','year')
names(esm2m)[1:4] = c('long','lat','year','esm2m_pdsi1')
# this command is just sorting the data so it's by location instead of year
ccsm4 = ccsm4[order(ccsm4$long,ccsm4$lat),]
esm2m = esm2m[order(esm2m$long,esm2m$lat),]


# now we're preparing to aggregate into the five-year timesteps
# we sorted the data into sequentially increasing order by location
# so now we just add a timestep field. (2099-2010)/5 = 18   
# we have 21 data points. can also use unique(DATAFRAME[,c('long','lat')]) to get total
# this produces 1 1 1 1 1 2 2 2 2 2 etc. So each year is getting assigned to a timestep.
ccsm4$timestep = rep(rep(1:18, each=5), times=21)
esm2m$timestep = rep(rep(1:18, each=5), times=10)

# now we can aggregate by timestep, keeping locations separate. taking the average here.
# note we are aggregating each pdsi set (column) separately)
ccsm4 = aggregate(ccsm4[,4:9], by=list(ccsm4$long,ccsm4$lat,ccsm4$timestep),mean, na.rm=TRUE)
esm2m = aggregate(esm2m[,4], by=list(esm2m$long,esm2m$lat,esm2m$timestep),mean, na.rm=TRUE)

# aggregate messes up the column names so we fix them here.
names(ccsm4)[1:3] = c('long','lat','timestep')
names(esm2m)[1:4] = c('long','lat','timestep', 'esm2m_pdsi1')

# for testing in arc
write.csv(ccsm4, "ccsm4_pdsi.csv")

# convert lat/long to UTM
require(rgdal)
# UTM Zone 10N String Proj4js.defs["EPSG:32610"] = "+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# World WGS 84 (GSP satellite; 2007) Proj4js.defs["EPSG:4979"] = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# World WGS84 Projected Proj4js.defs["EPSG:4326"] = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Thanks to Kathy for this code (overwritten somewhat for this dataset)
xysp<-SpatialPointsDataFrame(ccsm4[,c("long","lat")], ccsm4, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), bbox=NULL)
xysputm<-spTransform(xysp,CRS("+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
ccsm4 = as.data.frame(xysputm)
names(ccsm4)[c(1:2,10:11)] = c('GCSlong','GCSlat','UTMlong','UTMlat')

xysp<-SpatialPointsDataFrame(esm2m[,c("long","lat")], esm2m, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), bbox=NULL)
xysputm<-spTransform(xysp,CRS("+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
esm2m = as.data.frame(xysputm)
names(esm2m)[c(1:2,5:6)] = c('GCSlong','GCSlat','UTMlong','UTMlat')

### get centroid location from ArcMap
centroid = c(686001.2301, 4379628.373)

# this should give every set of unique locations
pdsi_pts_ccsm4 = unique(ccsm4[,c('UTMlong','UTMlat')])
pdsi_pts_esm2m = unique(esm2m[,c('UTMlong','UTMlat')])

# to calculate the inverse distance weighting, we made our own euclidean distance function
# this isn't inversed or anything; it's the euclidean distance to the centroid
# basically the pythagorean theorem
eucdist = function(x) sqrt(sum((centroid[1] - x[1])^2,(centroid[2] - x[2])^2))

# checking that this works
# apply the distance function column-wise; creates distances for each unique lat/long
distances_ccsm4 = apply(pdsi_pts_ccsm4,1,eucdist)
distances_esm2m = apply(pdsi_pts_esm2m,1,eucdist)

# append the distances to the data frame with the lat/longs for each data point
pdsi_pts_ccsm4 = cbind(pdsi_pts_ccsm4, distances_ccsm4)
pdsi_pts_esm2m = cbind(pdsi_pts_esm2m, distances_esm2m)

# apply also doesn't come up with good names; fix it
names(pdsi_pts_ccsm4)[3] = 'distance'
names(pdsi_pts_esm2m)[3] = 'distance'
# now we square and take the inverse so the influence of farther away points is much smaller
# 1/d^2
# this step gives us the weight
pdsi_pts_ccsm4$dweight = 1 / (pdsi_pts_ccsm4$distance ^ 2)
pdsi_pts_esm2m$dweight = 1 / (pdsi_pts_esm2m$distance ^ 2)

# now we relativize by the sum of all the weights
### I don't know why - maybe because otherwise they are crazy small? clearly we need to relativize
# but why by sum? I thought I did by largest last time...hmm maybe I did do it by sum
pdsi_pts_ccsm4$dweight = pdsi_pts_ccsm4$dweight / sum(pdsi_pts_ccsm4$dweight)
pdsi_pts_esm2m$dweight = pdsi_pts_esm2m$dweight / sum(pdsi_pts_esm2m$dweight)


# now we have the relative weight of each location, so we want to merge that with our pdsi columns
ccsm4 = merge(ccsm4, pdsi_pts_ccsm4)
esm2m = merge(esm2m, pdsi_pts_esm2m)
# sort it by timestep, then by location
ccsm4 = ccsm4[order(ccsm4$timestep,ccsm4$UTMlong,ccsm4$UTMlat),]
esm2m = esm2m[order(esm2m$timestep,esm2m$UTMlong,esm2m$UTMlat),]

# set up an empty matrix to hold the weight for each location
pdsi_wtd_ccsm4 = matrix(NA,length(unique(ccsm4$timestep)),6)
pdsi_wtd_esm2m = matrix(NA,length(unique(esm2m$timestep)),1)

# had to make another function to do the inverse distance weighting for us across all the locations
# multiplies pdsi value for each location by the distance weight and... what does the sum do? don't we want the average?
idw = function(x) sum(x*temp$dweight)
### what does the loop do?
# we come out of the loop with a matrix consisting of the input values for RMLands?
for (i in 1:length(unique(ccsm4$timestep))) {
    temp = ccsm4[ccsm4$timestep==i,]
    pdsi_wtd_ccsm4[i,] = apply(temp[6:11], 2, FUN=idw)
}

for (i in 1:length(unique(esm2m$timestep))) {
    temp = esm2m[esm2m$timestep==i,]
    pdsi_wtd_esm2m[i,] = apply(temp[6], 2, FUN=idw)
}

# convert to data frame
pdsi_wtd_ccsm4 = as.data.frame(pdsi_wtd_ccsm4)
pdsi_wtd_esm2m = as.data.frame(pdsi_wtd_esm2m)

# fix names
names(pdsi_wtd_ccsm4) = names(ccsm4[6:11])
names(pdsi_wtd_esm2m) = names(esm2m[6])

# make a timestep column for this data frame
pdsi_wtd_ccsm4$timestep = unique(ccsm4$timestep)
pdsi_wtd_esm2m$timestep = unique(esm2m$timestep)

# maybe it is now time to put these together?
pdsi_wtd = merge(pdsi_wtd_ccsm4,pdsi_wtd_esm2m)
pdsi_wtd_vals = pdsi_wtd[,-1]

write.csv(pdsi_wtd_vals, "pdsi_wtd_vals.csv")

# calculate mean and sd for each 5-year averaged pdsi value and store as objects
### is this a to-do? need to repeat previous steps for this? or just need to grab these from previous datasheets maybe
# for historic data
# below is dummy data so that we could test the code
hist_mean = apply(pdsi_wtd[,1:6], 2, mean) # change numbers to column names (everywhere!)
hist_sd = apply(pdsi_wtd[,1:6], 2, sd)

# actual historic mean from original set of PDSI data (1550-1850)
hist_mean = -0.009781612
hist_sd = 0.664115798

## this is what the excel sheet formulas looked like
# =(F3-AVERAGE($F$2:$F$61))/2*STDEVP($F$2:$F$61)+1
# =2*AVERAGE($G$2:$G$61)-G2
#
# =2*AVERAGE($G$2:$G$61)-(F3-AVERAGE($F$2:$F$61))/2*STDEVP($F$2:$F$61)+1

### another function to do the rescaling stuff (get more details from excel sheet)
### we used this version of the function to test the initial code
make_clim_mod = function(x){
    mu = (x-hist_mean[i])/2*hist_sd[i] + 1
    2 * mean(mu) - mu # the 2 * maintains that mean at whatever it is
}

## but there is only one historic mean...
test = pdsi_wtd_vals[,2]
make_clim_mod = function(x){
    mu = (x-hist_mean)/2*hist_sd + 1
    return(2 * mean(mu) - mu) # the 2 * maintains that mean at whatever it is
}

# working out whether problem is the climate modifier function
# in Excel:
# I believe we started in Excel for the mean value for each of the five years
# next equation is (ts - average(all ts))/2*sd(all ts) + 1
# then following equation is 2*(mean(all_rescaled) - rescaled)

# make empty matrix to hold this
clim_mods = matrix(NA,length(unique(ccsm4$timestep)),7)

# this makes the climate modifiers for each pdsi column ready to input into rmlands
for (i in 1:ncol(clim_mods)){
    clim_mods[,i] = make_clim_mod(pdsi_wtd_vals[,i])
}

fclimate = as.data.frame(clim_mods)
names(fclimate)[1:6] = names(ccsm4[6:11])
names(fclimate)[7] = names(esm2m[6])

write.csv(fclimate, "future_climate_params.csv")

# verify: should all have mean of 1 (shows they used the right mean, not the same mean many times)
# this won't be true when we use the historic mean and sd
apply(clim_mods,2,mean)

run a regression on the trend to extract the trend line
capture all the residuals
do the rescale and inversion on the residuals 

data = pdsi_wtd[,c(1,8)]
test = lm(timestep~esm2m_pdsi1, data)
summary(test)
residuals = as.numeric(resid(test))

# make empty matrix to hold this
clim_mods_test = matrix(NA,18,1)
clim_mods_test = make_clim_mod(residuals)

# so it seems that the problem is that adding one and multiplying by 2 
# are tricks used to get the result desired, but are not necessary
# and under HRV circumstances these tricks cancel one another
# so we reduced the equation and test it out
make_clim_mod2 = function(x){
    mu = (x-hist_mean)/2*hist_sd
    1 - mu
}
clim_mods_test = matrix(NA,18,1)
clim_mods_test = make_clim_mod2(pdsi_wtd_vals[,7])

clim_mods2 = matrix(NA,length(unique(ccsm4$timestep)),7)

for (i in 1:ncol(clim_mods2)){
    clim_mods2[,i] = make_clim_mod2(pdsi_wtd_vals[,i])
}

fclimate2 = as.data.frame(clim_mods2)
names(fclimate2)[1:6] = names(ccsm4[6:11])
names(fclimate2)[7] = names(esm2m[6])

# one more test
# this doesn't get it totally right (values below 0 become values below 1)
make_clim_mod3 = function(x){
    mu = (x-hist_mean)/2*hist_sd + 1
    1 + mean(mu) - mu
}

clim_mods_test = matrix(NA,18,1)
clim_mods_test = make_clim_mod3(pdsi_wtd_vals[,7])
