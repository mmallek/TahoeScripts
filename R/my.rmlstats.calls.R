
###R function calls for Tahoe project

#set working directory
#setwd('/Users/mmallek/Tahoe/RMLands/results201507/hrv')
#setwd('/Users/mmallek/Tahoe/R/Rplots/November2014')
#hrvpath='/Users/mmallek/Tahoe/RMLands/results201507/hrv/'
#futurepath='/Users/mmallek/Tahoe/RMLands/results201507/future/'
path = '/Users/mmallek/Tahoe/RMLands/results/results20150904/'
fragpath = "/Users/mmallek/Tahoe/RMLands/results/results20150904/fragstats20150901/"
# sessions = 30 for the hrv
# for future:
# ID34 = ccsm1
# ID35 = ccsm2
# ID36 = ccsm3
# ID38 = ccsm4
# ID39 = ccsm5
# ID43 = ccsm6
# ID44 = esm2m

#must source the rmlstats library
source('/Users/mmallek/Tahoe/Scripts/R/rmlstats.R')

covtypes = c("SMC_M", "SMC_X")

for(i in 1:length(covtypes)){
    
}

# use covtype argument if you want results for a single cover type, or set of cover types
# darea ####
dareaout = darea( 
  path=path,
  session=30,
  # all future sessions
  #sessions=c(34,35,36,38,39,43,44),
  var='mean',
  #runs=c(1:100),
  #start.step=14,
  runs = 1,
  start.step=40,
  stop.step=NULL,
  step.length=5,
  legendloc='topleft',
  #covtype="Mixed Evergreen - Mesic",
  #covtype="Mixed Evergreen - Xeric",
  #covtype="Oak-Conifer Forest and Woodland",
  #covtype="Oak-Conifer Forest and Woodland - Ultramafic",
  #covtype="Red Fir - Mesic",
  #covtype="Red Fir - Xeric",
  #covtype="Sierran Mixed Conifer - Mesic",
  #covtype="Sierran Mixed Conifer - Ultramafic",
  #covtype="Sierran Mixed Conifer - Xeric",
  cell.size=30,
  y.scale='percent',
  col.bars=c('dark green','dark blue','brown'),
  col.sub='brown', 
  cex.main=2.5,cex.sub=1.25,cex.legend=1.5,cex.lab=1.25,
  outfile=F)

#sessions=c(34,35,36,38,39,43,44),
# dsize ####
out<-dsize(path=path,
  session=30, # only takes a single session
  #runs=c(1:100),
  pool.runs=T,
  start.step=40,
  stop.step=NULL,
  cell.size=30,
  log.size=FALSE,
  breaks=c(0,5,25,125,625,3125,15625,78125),
  target='firehist_ha.csv',
  col.bars=c('blue','red'),
  col.sub='brown',
  cex.main=3,cex.sub=1.25,cex.legend=3,cex.lab=3)

dsize.new(path=path,
      session=30, # only takes a single session
      #runs=c(1:100),
      pool.runs=T,
      start.step=40,
      stop.step=NULL,
      cell.size=30,
      log.size=FALSE,
      breaks=c(0,5,25,125,625,3125,15625,78125),
      target='firehist_ha.csv',
      #col.bars=c('blue','red'),
      #col.sub='brown',
      #cex.main=3,cex.sub=1.25,cex.legend=3,cex.lab=3
      )

#sessions=c(34,35,36,38,39,43,44),
# rotation ####
out<-rotation(path=path,
  #session=34,
  #runs=c(1:100),
  session=30,
  runs=1,
  pool.runs=TRUE,
  var='any.mort',
  start.step=14,
  #start.step=40,
  stop.step=NULL,
  step.length=5,
  cell.size=30,
  outfile=F)
    
# preturn ####
out<-preturn(path=path,
  session=30,
  #runs=c(1:100),
  pool.runs=TRUE,
  stop.step=NULL,
  step.length=5,
  cell.size=30,
  cover.names=c('Sierran Mixed Conifer - Mesic'),
  cover.min.ha=1000,
  y.scale='percent',
  col.bars=c('yellow','green','blue'),
  col.sub='brown',
  cex.main=1.5,cex.sub=1.25,cex.legend=1.25,cex.lab=1.25,
  legendlocale='top',
  outfile=F)
  
# covcond ####
covcondout<-covcond(path='/Users/mmallek/Tahoe/RMLands/results/results20150904/',
  sessions=44,
  var='srv50%',
  runs=c(1:100), #can pool runs
  #runs = 1,
  start.step=14,
  stop.step=18,
  cell.size=30,
  #cover.names=c('Oak-Conifer Forest and Woodland'),
  cover.min.ha=1000,
  outfile=F)

covcondtab = read.csv('/Users/mmallek/Tahoe/ClusterBackup/november2014/csvs_upto_s20/covcond.csv')
covcondtab = subset(covcondtab, session.id==20)
stage.name = list()
for(i in 1:nrow(covcondtab)){
    if(grepl("Late", covcondtab$cond.name[i])){
        #cat("if 1 works")
        stage.name[i] = 3}
    if(grepl("Mid", covcondtab$cond.name[i])){
        stage.name[i] = 2}
    if(grepl("Early", covcondtab$cond.name[i])){
        stage.name[i] = 1}
}
newcovcondtab = aggregate(covcondtab, by=stage.name, FUN=sum)

# covcond plot ####                       
covcondout<-covcond.plot(path=path,
  session=30,
  var='srv50%',
  #runs=c(1:10),
  runs = 1,
  start.step=0,
  stop.step=NULL,
  step.length=5,
  type='stack',
  cell.size=30,
  #cover.names=c('Mixed Evergreen - Xeric'),
  cover.min.ha=1000,
  col.bars=c('black','tan','green','orange','brown','lightsalmon',
    'darkgreen','lightgreen','yellow','yellow3','yellow4','wheat'),
  col.sub='brown',
  cex.main=2,cex.sub=1.25,cex.legend=1,cex.lab=4,
  outfile=F,
  save.figs=T)

hrvpath = '/Users/mmallek/Tahoe/RMLands/results201507/hrv/'
futurepath = '/Users/mmallek/Tahoe/RMLands/results201507/future/fragresults/'
# LID Z:\Working\maritza\hrv\session000\run001\ts_grp00\covcond000res_clip.tif 
# LID Z:\Working\maritza\future\ccsm-2_s002_finalgrids\covcond090(1)_res_clip.tif

# fragland ####
fraglandout = fragland(path='/Users/mmallek/Tahoe/RMLands/results/results20150904/fragstats20150901/',
  #infile='classland_session016_res.land',
  infile='fragresults_hrv_20150831.land',
  #Z:\Working\maritza\hrv\session000\run001\ts_grp00\covcond000res_clip.tif 
  LID.path='Z:\\Working\\maritza\\',
  scenarios='hrv',
  sessions=1,
  sessions.name='covcond',
  runs=1,
  runs.name='run',
  #metrics=c('PD','AI'),
  #var='srv50%',
  start.step=40,
  stop.step=NULL,
  outfile=F)

# fragland plot ####
fragland.plot(path='/Users/mmallek/Tahoe/RMLands/results/results20150904/fragstats20150901/', #/Users/mmallek/Tahoe/Fragstats/',
  infile='fragresults_hrv_20150831.land', #classland_session016_res.land',
  LID.path='Z:\\Working\\maritza\\',
  scenarios='hrv',#'maritza',
  sessions=1,
  sessions.name='covcond',#'ccsm2_run1_finalgrids',
  runs=1,
  runs.name='run',#'classland_session016_res.land',
  #metrics=c('PD','AI'),
  start.step=40,
  stop.step=NULL,
  step.length=5,
  quantiles=c(0.05,0.95),
  col.line='dark blue',col.sub='brown',
  cex.main=1.5,cex.sub=1.25,cex.legend=1.25,cex.lab=1.25,
  save.figs=T)

# fragland pdf ####
fragland.pdf.plot(path='/Users/mmallek/Tahoe/ClusterBackup/november2014/csvs_upto_s20/',
  infile='classland_session014_res.land',
  LID.path='E:\\mallek\\results\\',
  scenarios='new',
  sessions=014,
  sessions.name='session',
  runs=001,
  runs.name='run',
  pool.runs=F,
  #metrics=c('PD','AI'),
  current=TRUE,
  start.step=40,
  stop.step=NULL,
  ref.scenario=NULL,
  ref.session=NULL,
  ref.tstep=NULL,
  ref.include=FALSE,
  quantiles=c(0.05,0.95),
  outfile=TRUE,
  col.line='dark blue',col.sub='brown',
  cex.main=1.5,cex.sub=1.25,cex.legend=0.75,cex.lab=1.25,
  save.figs=T)

# frag class ####
fragclassout = fragclass(path='/Users/mmallek/Tahoe/RMLands/results/results20150904/fragstats20150901/',
      inland='fragresults_hrv_20150831.land',
      inclass='fragresults_hrv_20150831.class',
      LID.path='Z:\\Working\\maritza\\',
      scenarios='hrv',
      sessions=1,
      sessions.name='covcond',
      runs=1,
      runs.name='run',
      gridname='covcond',
      #gridname='wfmort',
      #classes=c('MEG_M_EARLY_ALL','MEG_M_MID_CL','MEG_M_MID_MOD','MEG_M_MID_OP','MEG_M_LATE_CL','MEG_M_LATE_MOD','MEG_M_LATE_OP'),
      #classes=c('MEG_X_EARLY_ALL','MEG_X_MID_CL','MEG_X_MID_MOD','MEG_X_MID_OP','MEG_X_LATE_CL','MEG_X_LATE_MOD','MEG_X_LATE_OP'),
      #classes=c('OCFW_EARLY_ALL','OCFW_MID_CL','OCFW_MID_MOD','OCFW_MID_OP','OCFW_LATE_CL','OCFW_LATE_MOD','OCFW_LATE_OP'),
      #classes=c('OCFW_U_EARLY_ALL','OCFW_U_MID_CL','OCFW_U_MID_MOD','OCFW_U_MID_OP','OCFW_U_LATE_CL','OCFW_U_LATE_MOD','OCFW_U_LATE_OP'),
      #classes=c('RFR_M_EARLY_ALL','RFR_M_MID_CL','RFR_M_MID_MOD','RFR_M_MID_OP','RFR_M_LATE_CL','RFR_M_LATE_MOD','RFR_M_LATE_OP'),
      #classes=c('RFR_X_EARLY_ALL','RFR_X_MID_CL','RFR_X_MID_MOD','RFR_X_MID_OP','RFR_X_LATE_CL','RFR_X_LATE_MOD','RFR_X_LATE_OP'),
      #classes=c('SMC_M_EARLY_ALL','SMC_M_MID_CL','SMC_M_MID_MOD','SMC_M_MID_OP','SMC_M_LATE_CL','SMC_M_LATE_MOD','SMC_M_LATE_OP'),
      #classes=c('SMC_X_EARLY_ALL','SMC_X_MID_CL','SMC_X_MID_MOD','SMC_X_MID_OP','SMC_X_LATE_CL','SMC_X_LATE_MOD','SMC_X_LATE_OP'),
      classes=c('SMC_U_EARLY_ALL','SMC_U_MID_CL','SMC_U_MID_MOD','SMC_U_MID_OP','SMC_U_LATE_CL','SMC_U_LATE_MOD','SMC_U_LATE_OP'),
      #metrics=c('AREA_AM','SHAPE_AM','CORE_AM','CLUMPY','GYRATE_AM'),
      var='srv50%',
      start.step=40,
      stop.step=NULL,
      outfile=T)

fragclass.plot(path='/Users/mmallek/Tahoe/Fragstats/',
               inland='classland_session016_res.land',
               inclass='classland_session016_res.class',
               LID.path='E:\\mallek\\results\\',
               scenarios='new',
               sessions=016,
               sessions.name='session',
               runs=001,
               runs.name='run',
               gridname='covcond',
               classes=c("SMC_X_MID_CL", 'SMC_X_MID_MOD', 'SMC_X_LATE_CL', 
                         'SMC_X_LATE_MOD', 'SMC_X_LATE_OP'),
               #classes=c('SMC_M_LATE_OP'),
               #classes=c("MEG_M_EARLY_ALL","MEG_M_MID_CL","MEG_M_MID_MOD",
               #          "MEG_M_MID_OP","MEG_M_LATE_CL","MEG_M_LATE_MOD",
               #          "MEG_M_LATE_OP","MEG_X_EARLY_ALL","MEG_X_MID_CL",
               #          "MEG_X_MID_MOD","MEG_X_MID_OP","MEG_X_LATE_CL",
               #          "MEG_X_LATE_MOD","MEG_X_LATE_OP","OCFW_EARLY_ALL",
               #          "OCFW_MID_CL","OCFW_MID_MOD","OCFW_MID_OP",
               #          "OCFW_LATE_CL","OCFW_LATE_MOD","OCFW_LATE_OP",
               #          "RFR_M_EARLY_ALL","RFR_M_MID_CL","RFR_M_MID_MOD",
               #          "RFR_M_MID_OP","RFR_M_LATE_CL","RFR_M_LATE_MOD",
               #          "RFR_M_LATE_OP","RFR_X_EARLY_ALL","RFR_X_MID_CL",
               #          "RFR_X_MID_MOD","RFR_X_MID_OP","RFR_X_LATE_CL",
               #          "RFR_X_LATE_MOD","RFR_X_LATE_OP","SMC_M_EARLY_ALL",
               #          "SMC_M_MID_CL","SMC_M_MID_MOD","SMC_M_MID_OP",
               #          "SMC_M_LATE_CL","SMC_M_LATE_MOD","SMC_M_LATE_OP",
               #          "SMC_U_EARLY_ALL","SMC_U_MID_CL","SMC_U_MID_MOD",
               #          "SMC_U_MID_OP","SMC_U_LATE_CL","SMC_U_LATE_MOD",
               #          "SMC_U_LATE_OP","SMC_X_EARLY_ALL","SMC_X_MID_CL",
               #          "SMC_X_MID_MOD","SMC_X_MID_OP","SMC_X_LATE_CL",
               #          "SMC_X_LATE_MOD","SMC_X_LATE_OP"),
               metrics=c("AREA_AM", "SHAPE_AM", "CORE_AM", "CLUMPY", "AI"),
               start.step=40,
               stop.step=NULL,
               step.length=5,
               quantiles=c(0.05,0.95),
               col.line='blue',col.sub='brown',
               cex.main=1.5,cex.sub=1.25,cex.legend=0.75,cex.lab=1.25,
               save.figs=T)

fragclass.pdf.plot(path='/Users/mmallek/Tahoe/ClusterBackup/november2014/csvs_upto_s20/',
  inland='fragout.land',
  inclass='fragout.class',
  LID.path='C:\\Users\\Landeco\\Desktop\\mallek\\results\\',
  scenarios='new',
  sessions=19,
  sessions.name='session',
  runs=001,
  runs.name='run',
  pool.runs=TRUE,
  gridname='covcond',
  classes=c('SMC_M_LATE_OP'),
  #metrics=c('PLAND','CLUMPY','AI'),
  current=TRUE,
  start.step=40,
  stop.step=NULL,
  ref.scenario=NULL,
  ref.session=NULL,
  ref.tstep=NULL,
  ref.include=FALSE,
  quantiles=c(0.05,0.95),
  outfile=FALSE,
  col.line='blue',
  col.sub='brown',
  cex.main=1.5,cex.sub=1.25,cex.legend=1.25,cex.lab=1.25,
  save.figs=FALSE)

calibrate(path='/Users/mmallek/Tahoe/ClusterBackup/october2014/',
  session=19,
  runs=NULL,
  var='any.mort',
  start.step=40,
  stop.step=NULL,
  step.length=5,
  cell.size=30,
  disturb=c('fb','pb','pd','sb','sbw','wfire'),
  new=FALSE,
  outfile=FALSE)

hrvpath = '/Users/mmallek/Tahoe/RMLands/results201507/hrv/'
futurepath = '/Users/mmallek/Tahoe/RMLands/results201507/future/fragresults/'
# LID Z:\Working\maritza\hrv\session000\run001\ts_grp00\covcond000res_clip.tif 
# LID Z:\Working\maritza\future\ccsm-2_s002_finalgrids\covcond090(1)_res_clip.tif

#fragpath should be path to fragstats.land file
#scenario should be name of model and run value, e.g. ccsm2_run1
#nrun is the number of runs completed for the scenario
#stop.run is the number of runs to analyze
#LID.path is path up to tif file
#covcondlist is path to csv with list of covcond files
setwd('/Users/mmallek/Tahoe/RMLands/results201507/future/ccsm1')
fragland.plot.future(
    fragpath= '/Users/mmallek/Tahoe/RMLands/results201507/future/fragresults/classland_ccsm1_20150723.land',
    #fragpath = '/Users/mmallek/Tahoe/Fragstats/Fragoutput_201505/classland_ccsm2_r1_20150529.land', 
    #LID.path='Z:\\Working\\maritza\\ccsm2_run1_finalgrids\\',
    #LID.path='E:\\mallek\\results\\new\\session016\\run001\\',
    LID.path='Z:\\Working\\maritza\\future',
    scenarios='ccsm1',
    covcondlist='/Users/mmallek/Tahoe/RMLands/results201507/future/fragresults/covcondlist_100ts.csv',
    nrun=100,
    #metrics=c('PD','ED','AREA_AM','GYRATE_AM','SHAPE_AM','CORE_AM','CWED','CONTAG','SIDI','SIEI','AI'),
    #metrics=c('PD','ED','AREA_AM'),
    start.step=1,
    stop.run=NULL,
    quantiles=c(0.05,0.95),
    col.line='dark blue',col.sub='brown',
    cex.main=1.5,cex.sub=1.25,cex.legend=1,cex.lab=1.25,
    save.figs=F)

fragland,future(
         fragpath = '/Users/mmallek/Tahoe/RMLands/results201507/future/fragresults/classland_pastclimate_20150723.land',
         infile = 'classland_pastclimate_20150723.land',
         LID.path = 'Z:\\Working\\maritza\\future',
         scenarios = 'pastclimate',
         nrun=100, 
         covcondlist = '/Users/mmallek/Tahoe/RMLands/results201507/future/fragresults/covcondlist_100ts.csv',
         metrics = c('PD','ED','AREA_AM','GYRATE_AM','SHAPE_AM','CORE_AM','CWED','CONTAG','SIDI','SIEI','AI'),
         outfile=T)

out<-dinit(path=futurepath,
           sessions=6,
           var='median',
           runs=c(1:100),
           start.step=1,
           stop.step=NULL,
           step.length=5,
           col.line='blue',
           col.sub='brown',
           cex.main=1.5,cex.sub=1.25,cex.legend=0.75,cex.lab=1.25,
           outfile=FALSE)

#not ready for this one
out<-tarea(path='/Users/mmallek/Tahoe/ClusterBackup/november2014/csvs_upto_s20/',
           session=19,
           var='mean',
           runs=001,
           start.step=40,
           stop.step=NULL,
           step.length=5,
           cell.size=30,
           y.scale='ha',
           col.bar=c('yellow','pink','blue','green','orange','red'),
           col.text='brown',
           cex.main=1.5,cex.sub=1.25,cex.legend=0.75,cex.lab=1.25,
           outfile=FALSE)

# calculate number of fires per timestep
numfire = read.csv('/Users/mmallek/Tahoe/ClusterBackup/november2014/csvs_upto_s20/darea.csv', header=TRUE)
numfire = filter(numfire, session.id==20)
numfire1 = table(numfire$timestep.id)
numfire2 = as.data.frame(c(rep(0, 1), table(numfire$timestep.id)))

tahoedata = cbind(tahoedata, numfire2)
tahoedata = rename(tahoedata,c('c(rep(0, 1), table(numfire$timestep.id))'='num_fires'))
tahoedata = rename(tahoedata,c('num_fires'='numfires'))
