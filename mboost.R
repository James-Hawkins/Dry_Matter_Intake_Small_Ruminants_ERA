

source('functions.R') ; source('m.boost.params.R')

# to DO
# create keyword for ue.id and ut.id and use consistently
# replace colnames in d.gbr with global character objects
# harmonize the handling of lists in and out

# add functionality for greater formula configurations

{ 
  

  
source('m.boost.params.R')
  
  s.rums.gbr <- s.rums[]   
  
s.rums$ut.id <- NA
s.rums$ue.id <- NA  

experiment.records <- c()

ue.count <- 0

s.rums[1, 'ue.id'] <- 1

for (r in 1:nrow(s.rums)){
  
 # r <- 1
  
  s.rums[r, 'ut.id'] <- r
  
  ue.id <-  s.rums[r, 'B.Code'] 
  
  
  if (  !(ue.id %in% experiment.records)   ){
    
    experiment.records <- c(experiment.records , ue.id )
    ue.count <- ue.count +1
    
  } 
  
  
  if (r > 1) { s.rums[r, 'ue.id'] <- ue.count  }
  
}
  

ids.omit.ind <- c( 187 , 185 , 186)

ids.omit.var.ol <- s.rums[s.rums$ol.status.var.ranges , 'ut.id']

ids.rep.fem <- s.rums[ (s.rums$is_lactating ==1 | s.rums$is_gestating ==1 ) & s.rums$Species == species.goat , 'ue.id']

ids.omit <- ids.rep.fem
ids.omit <- c(ids.omit , ids.omit.ind , ids.rep.fem , ids.omit.var.ol)

s.rums.min.vars <-  ( !is.na(s.rums$feed_intake_g_d) & !is.na(s.rums$bw_kg) &  !is.na(s.rums$adg_g_day)  )

if ( is.na(age.status) ) { s.rums.age.subset <- TRUE } else if (age.status == 'growing') { 
  s.rums.age.subset <- ( s.rums$bw_kg < 20 & !is.na(s.rums$bw_kg )) }


s.rums.dont.exclude <- (!(s.rums$ut.id %in% ids.omit) & s.rums.age.subset  )

# handle outliers
cnd.sp.lo.ndf.pre.ol <- (s.rums.min.vars & s.rums$Species == species.sheep  & s.rums$ndf.level == ndf.lev.lo & s.rums.dont.exclude)
cnd.sp.hi.ndf.pre.ol <- (s.rums.min.vars & s.rums$Species == species.sheep & s.rums$ndf.level == ndf.lev.hi & s.rums.dont.exclude)
cnd.gt.lo.ndf.pre.ol <- (s.rums.min.vars & s.rums$Species == species.goat & s.rums$ndf.level == ndf.lev.lo & s.rums.dont.exclude)
cnd.gt.hi.ndf.pre.ol <- (s.rums.min.vars & s.rums$Species == species.goat & s.rums$ndf.level == ndf.lev.hi & s.rums.dont.exclude)


s.rums[,ol.status.var.name] <- 999

s.rums <- ols.status( cnd.sp.lo.ndf.pre.ol )
s.rums <- ols.status( cnd.sp.hi.ndf.pre.ol )
s.rums <- ols.status( cnd.gt.lo.ndf.pre.ol )
s.rums <- ols.status( cnd.gt.hi.ndf.pre.ol )

s.rums.variable.redfd <<- ( s.rums$NDF_digest.redf | s.rums$NDF_nutrition.redf ) 


cnd.sp.lo.ndf <- (cnd.sp.lo.ndf.pre.ol & s.rums$ol.status < cutoff.ol.s  )
cnd.sp.hi.ndf <- (cnd.sp.hi.ndf.pre.ol & s.rums$ol.status < cutoff.ol.s  & !s.rums.variable.redfd)
cnd.gt.lo.ndf <- (cnd.gt.lo.ndf.pre.ol & s.rums$ol.status < cutoff.ol.s  & !s.rums.variable.redfd)
cnd.gt.hi.ndf <- (cnd.gt.hi.ndf.pre.ol & s.rums$ol.status < cutoff.ol.s  & !s.rums.variable.redfd)

if ( include.redf ){
  
  cnd.sp.lo.ndf <- (cnd.sp.lo.ndf  & !s.rums.variable.redfd)
  cnd.sp.hi.ndf <- (cnd.sp.lo.ndf  & !s.rums.variable.redfd)
  cnd.gt.lo.ndf <- (cnd.gt.lo.ndf  & !s.rums.variable.redfd)
  cnd.gt.hi.ndf <- (cnd.gt.hi.ndf  & !s.rums.variable.redfd)
  
}


#summary(s.rums[cnd.sp.lo.ndf , 'ol.status'])
#summary(s.rums[cnd.sp.hi.ndf , 'ol.status'])
#summary(s.rums[cnd.gt.lo.ndf , 'ol.status'])
#summary(s.rums[cnd.gt.hi.ndf , 'ol.status'])


{


  n.mod.vers <-  n.mod.v.family * n.mod.v.boost.control.mstop * n.mod.v.boost.control.nu 
  
  n.mod.v.ids <- c()
  
  count <- 1
  family.count <<- 1
  mstop.count <<- 1
  nu.count <<- 1
  
  str.filler <- '-'
  
  # Model version ID: family-mstop-nu
  
for (f in 1  :  n.mod.v.family) { 
for (m in 1  :  n.mod.v.boost.control.mstop) { 
for (n in 1 :   n.mod.v.boost.control.nu) {
    

n.mod.v.ids[ count   ] <- str_c( f,  str.filler , m  , str.filler , n)

count <- count + 1

family.count <- family.count + 1 }
mstop.count <- mstop.count + 1}
nu.count <- nu.count + 1}

} # Define iterations for hyper parameter tuning


n.rows <- n.ndf * n.species * p.k * n.mod.vers * n.mod.form
  

col.k <- c(  rep ( seq(1,p.k,by=1), n.ndf * n.species * n.mod.vers * n.mod.form  ))
col.ndf <-c(  rep ( c( rep(ndf.levs[1] , p.k  )  , rep(ndf.levs[2], p.k  )) , n.species * n.mod.vers  * n.mod.form ))


col.mf <-  rep(1,   p.k * n.ndf * n.mod.vers ) 

if (n.mod.form >1){ col.mf <- c(col.mf, rep(2,   p.k * n.ndf * n.mod.vers  )  ) }
if (n.mod.form >2){ col.mf <- c(col.mf, rep(3,   p.k * n.ndf * n.mod.vers )  ) }
if (n.mod.form >3){ col.mf <- c(col.mf, rep(4,   p.k * n.ndf * n.mod.vers )  ) }

col.mf <- c( rep(col.mf ,n.species) )



col.species <- c( rep(species.sheep , n.rows/n.species )  )

if (n.species == 2){ col.species <- c( col.species , rep(species.goat, n.rows/n.species) ) } 
  
                  

length(  col.k  )
length(  col.ndf  )
length(  col.mf  )
length(  col.species  )

# Define column of model version IDs
col.mod.vers <- c()
seq.start.indx <- 1

for (mv in 1:length(n.mod.v.ids)){
  
#  mv <- 1
  
  sequence.2.add <- rep(n.mod.v.ids[mv], times = (p.k * n.ndf ) )
  
  seq.end.indx <-  seq.start.indx + length( sequence.2.add  ) - 1
  
  col.mod.vers[ seq(from = seq.start.indx , to = (seq.end.indx ) )  ] <- sequence.2.add
  
  seq.start.indx <- seq.end.indx +1 
  
}

col.mod.vers <- c( rep (col.mod.vers , n.mod.form  * n.species ) )
  
length(col.species)
#length(col.ndf )
#length(col.k)
length(col.mod.vers)
  

# comprehensive list of var names

vn.w.R2.mean <<- 'w.R2.mean'

vn.best.w.R2 <<- 'best.w.R2'
vn.best.w.nRMSE <<- 'best.w.nRMSE'
vn.best.w.CCC <<- 'best.w.CCC'


vn.best.global.w.R2 <<- 'best.global.w.R2'
vn.best.global.w.nRMSE <<- 'best.global.w.nRMSE'
vn.best.global.w.CCC <<- 'best.global.w.CCC'


vn.w.R2.mean <<- 'w.R2.mean'
vn.w.nRMSE.mean <<- 'w.nRMSE.mean'
vn.w.CCC.mean <<- 'w.CCC.mean'

vn.w.R2.sd <<- 'w.R2.sd'
vn.w.nRMSE.sd <<- 'w.nRMSE.sd'
vn.w.CCC.sd<<- 'w.CCC.sd'

d.gbr <- data.frame( matrix(NA, nrow = n.rows, ncol = 1) )




# Sample and model descriptors
d.gbr[, 'species' ] <- col.species
d.gbr[, 'ndf' ] <- col.ndf
d.gbr[, 'mod.form' ] <- col.mf
d.gbr[, 'mod.vers' ] <- col.mod.vers
d.gbr[, 'k' ] <- col.k

d.gbr[, 'mv.m.stop' ] <- NA
d.gbr[, 'mv.nu' ] <- NA


# Global metrics - optimal model
d.gbr[ , vn.best.w.R2 ] <- NA
d.gbr[ , vn.best.w.nRMSE ] <- NA
d.gbr[ , vn.best.w.CCC ] <- NA


d.gbr[ ,  'is.best.model'] <- FALSE

# Global metrics = all formulae - optimal model
d.gbr[ , vn.best.global.w.R2 ] <- NA
d.gbr[ , vn.best.global.w.nRMSE ] <- NA
d.gbr[ , vn.best.global.w.CCC ] <- NA

d.gbr[ ,  'is.best.global.model'] <- FALSE


# Current iteration
d.gbr[ , vn.w.R2.mean ] <- NA
d.gbr[ , vn.w.nRMSE.mean ] <- NA
d.gbr[ , vn.w.CCC.mean ] <- NA


d.gbr[ , vn.w.R2.sd ] <- NA
d.gbr[ , vn.w.nRMSE.sd ] <- NA
d.gbr[ , vn.w.CCC.sd ] <- NA


d.gbr[ , 'mean.coef.int'] <- NA
d.gbr[ , 'mean.coef.BW'] <- NA
d.gbr[ , 'mean.coef.ADG'] <- NA
d.gbr[ , 'mean.coef.NDF'] <- NA
d.gbr[ , 'mean.coef.CP'] <- NA
d.gbr[ , 'mean.coef.NDF_digest'] <- NA



# Current fold
d.gbr[ , 'test.r2'] <- NA
d.gbr[ , 'test.w.r2'] <- NA
d.gbr[ , 'test.nrmse'] <- NA
d.gbr[ , 'test.w.nrmse'] <- NA


d.gbr[ , 'coef.int'] <- NA
d.gbr[ , 'coef.BW'] <- NA
d.gbr[ , 'coef.ADG'] <- NA
d.gbr[ , 'coef.NDF'] <- NA
d.gbr[ , 'coef.CP'] <- NA
d.gbr[ , 'coef.NDF_digest'] <- NA



# Whole sample
d.gbr[ , 'observed.Y'] <- NA
d.gbr[ , ' mod.Y '] <- NA

d.gbr[ , 'gbr.form'] <- NA
d.gbr[ , 'gbr.model'] <- NA

# Sample sub-setting
d.gbr[ , 'total.sample'] <- NA
d.gbr[ , 'k.IDs'] <- NA

d.gbr[ , 'r.cond'] <- NA
d.gbr[ , 'all.data'] <- NA
d.gbr[ , 'sample.size'] <- NA

d.gbr[ , 'treatment.IDs.CCs'] <- NA
d.gbr[ , 'all.treatment.IDs'] <- NA


d.gbr[ , 'treatment.IDs'] <- NA

d.gbr[ , 'experiment.IDs'] <- NA
d.gbr[ , 'train.IDs'] <- NA
d.gbr[ , 'k.IDs.length '] <- NA
d.gbr[ , 'train.IDs.length'] <- NA

old.df <- function(){
d.gbr <- data.frame(
  
  
  
  # Sample and model descriptors
       species = col.species
     , ndf = col.ndf 
     , mod.form = col.mf 
     , mod.vers = col.mod.vers
     , k =  col.k 
     
     # Model hyperparameters
     , mv.m.stop = rep(NA, times = n.rows )
     , mv.nu = rep(NA, times = n.rows )
     
    # GBR model summary stats
    
    # Global metrics
    # Optimal model
    , is.best.model  = rep( FALSE , times = n.rows )
    
    
    , vn.best.w.R2 = rep(NA, times = n.rows )
    , vn.best.w.nRMSE = rep(NA, times = n.rows )
    , vn.best.w.CCC = rep(NA, times = n.rows )
    
    
    # Mean of current iteration
    , vn.w.R2.mean = rep( NA , times = n.rows )
    , vn.w.nRMSE.mean= rep( NA , times = n.rows )

    , vn.w.R2.sd = rep( NA , times = n.rows )
    , vn.w.nRMSE.sd = rep( NA , times = n.rows )
    
    
    # Fold specific metrics
    # Test performance metrics
    , test.r2 = rep(NA, times = n.rows )
    , test.w.r2 = rep(NA, times = n.rows )
    
    , test.nrmse = rep(NA, times = n.rows )
    , test.w.nrmse = rep(NA, times = n.rows )
    
    


    # Training performance metrics
    , train.r2 = rep(NA, times = n.rows )
    , train.w.r2 = rep(NA, times = n.rows )
    
    , train.nrmse = rep(NA, times = n.rows )
    , train.w.nrmse = rep(NA, times = n.rows )
    
    , observed.Y = rep(NA, times = n.rows )
    , mod.Y = rep(NA, times = n.rows )
    
    , gbr.form = rep(NA, times = n.rows )
    
    , gbr.model = rep(NA, times = n.rows )
    
    , gbr.model.m.stop = rep(NA, times = n.rows )
    
    , gbr.model.nu = rep(NA, times = n.rows )
    
  
    
    # Sampling criteria
    , total.sample = rep(NA, times= n.rows ) 
    , k.IDs = rep(NA, times= n.rows ) 
     
    , r.cond = rep(NA, times= n.rows )                                       
    , all.data = rep(NA, times = n.rows )   
    , sample.size = rep(NA, times = n.rows)

  
    , treatment.IDs.CCs =  rep(NA,times = n.rows )
    , all.treatment.IDs =  rep(NA,times = n.rows )
    , treatment.IDs =  rep(NA,times = n.rows )
    , experiment.IDs =  rep(NA,times = n.rows )
    , IDs.remaining= rep(NA,times = n.rows )
    , train.IDs = rep(NA,times = n.rows )
  
  
    , k.IDs.length =  rep(NA,times = n.rows )
    , train.IDs.length = rep(NA,times = n.rows  )

)
}

colnames(d.gbr)

# nrow(d.gbr)



gbm.cond.shp.lo.ndf <- (d.gbr$species == species.sheep & d.gbr$ndf == ndf.lev.lo)
gbm.cond.shp.hi.ndf <- (d.gbr$species == species.sheep & d.gbr$ndf == ndf.lev.hi)
gbm.cond.gt.lo.ndf <- (d.gbr$species == species.goat & d.gbr$ndf == ndf.lev.lo)
gbm.cond.gt.hi.ndf <- (d.gbr$species == species.goat & d.gbr$ndf == ndf.lev.hi)



d.gbr[ gbm.cond.shp.lo.ndf , 'r.cond'] <- list(list(cnd.sp.lo.ndf))
d.gbr[ gbm.cond.shp.hi.ndf , 'r.cond'] <- list(list(cnd.sp.hi.ndf ))

d.gbr[  gbm.cond.gt.lo.ndf , 'r.cond'] <- list(list(cnd.gt.lo.ndf ))
d.gbr[  gbm.cond.gt.hi.ndf  , 'r.cond'] <- list(list(cnd.gt.hi.ndf ))


d.gbr[ gbm.cond.shp.lo.ndf , 'species.ndf'] <- sheep.lo.ndf
d.gbr[ gbm.cond.shp.hi.ndf , 'species.ndf'] <- sheep.hi.ndf

d.gbr[  gbm.cond.gt.lo.ndf , 'species.ndf'] <- goat.lo.ndf
d.gbr[  gbm.cond.gt.hi.ndf  , 'species.ndf'] <- goat.hi.ndf


ue.ids.sp.lo.ndf <- unique(s.rums[ cnd.sp.lo.ndf , 'ue.id'])
ue.ids.sp.hi.ndf <- unique(s.rums[ cnd.sp.hi.ndf , 'ue.id'])
ue.ids.gt.lo.ndf <- unique(s.rums[ cnd.gt.lo.ndf , 'ue.id'])
ue.ids.gt.hi.ndf <- unique(s.rums[ cnd.gt.hi.ndf , 'ue.id'])

# Treatment Id lists
d.gbr[gbm.cond.shp.lo.ndf  , 'treatment.IDs'] <- list(list(  unique(s.rums[ cnd.sp.lo.ndf , 'ut.id'])  ))
d.gbr[gbm.cond.shp.hi.ndf  , 'treatment.IDs'] <- list(list(unique(s.rums[ cnd.sp.hi.ndf, 'ut.id'])))

d.gbr[gbm.cond.gt.lo.ndf , 'treatment.IDs'] <- list(list(unique(s.rums[ cnd.gt.lo.ndf , 'ut.id'])))
d.gbr[gbm.cond.gt.hi.ndf  , 'treatment.IDs'] <- list(list(unique(s.rums[ cnd.gt.hi.ndf, 'ut.id'])))

# Experiment Id lists
d.gbr[gbm.cond.shp.lo.ndf  , 'experiment.IDs'] <- list(list(  s.rums[ cnd.sp.lo.ndf , 'ue.id'])  )
d.gbr[gbm.cond.shp.hi.ndf  , 'experiment.IDs'] <- list(list(s.rums[ cnd.sp.hi.ndf, 'ue.id']))

d.gbr[gbm.cond.gt.lo.ndf , 'experiment.IDs'] <- list(list(s.rums[ cnd.gt.lo.ndf , 'ue.id']))
d.gbr[gbm.cond.gt.hi.ndf  , 'experiment.IDs'] <- list(list(s.rums[ cnd.gt.hi.ndf, 'ue.id']))


# Sample sizes per species-ndf level

d.gbr[ gbm.cond.shp.lo.ndf , 'total.sample.size'] <- length(unique(s.rums[ cnd.sp.lo.ndf , 'ut.id']))
d.gbr[ gbm.cond.shp.hi.ndf , 'total.sample.size'] <- length(unique(s.rums[ cnd.sp.hi.ndf , 'ut.id']))
d.gbr[ gbm.cond.gt.lo.ndf , 'total.sample.size'] <- length(unique(s.rums[ cnd.gt.lo.ndf , 'ut.id']))
d.gbr[ gbm.cond.gt.hi.ndf , 'total.sample.size'] <- length(unique(s.rums[ cnd.gt.hi.ndf , 'ut.id']))


# Sample sizes
for ( r in 1:nrow(d.gbr)) {
  
  ids <- de.listify( d.gbr[r, 'experiment.IDs'] ) 
  
  weight.list <- 0
  
  for (id in ids) { 
    
    weight <- unique(s.rums[ s.rums$ue.id == id, 'Sample.size'])

    weight.list <- append (weight.list , weight )

  }
  
  weight.list <- weight.list[-c(1)]
  
  d.gbr[ r, 'weight.list'] <- listify(weight.list)
  
  
  
}

for (r in 1:nrow(d.gbr)) { d.gbr[ r, 'IDs.remaining'] <- list(list( d.gbr[ r, 'treatment.IDs'])) }


} # Data pre processing


{ 
  
scoping <- function(){
  
# current.ds.ids <- ue.ids.sp.lo.ndf
# current.ds.ids <- ue.ids.sp.hi.ndf
# current.ds.ids <- ue.ids.gt.lo.ndf
# current.ds.ids <- ue.ids.gt.hi.ndf


separator <- ' + '

# + is_gestating
#  + milk_kg_day

rhs <- paste( 
  x.vars[1] 
 , x.vars[2] 
 , x.vars[3] 
 , x.vars[4] 
 , x.vars[5] 
 , x.vars[6] 
 , x.vars[7] 
 , x.vars[8] 
 , x.vars[9] 
 , x.vars[10] 
 , x.vars[11] 
 , x.vars[12] 
 , x.vars[13] 
 , x.vars[14] 
 , x.vars[15] 
 , x.vars[16] 
 , x.vars[17] 
 , x.vars[18] 
 , x.vars[19] 
 , x.vars[20] 
 , x.vars[21] 
 , x.vars[22] 
 , x.vars[23] 
 , x.vars[24] 
 , x.vars[25] 
 , x.vars[26] 
 , x.vars[27] 
 , x.vars[28] 
 
 , x.vars[29] 
 , x.vars[30] 
 , x.vars[31]
 , x.vars[33] 

 
  , sep = " + ")


Xs.plus.intercept <- c('Intercept' , x.vars)

formula <- as.formula( 
  paste(
  "feed_intake_g_d  ~ "
    #" drym_intake_g_d  ~ "
  , rhs) )

s.rums$ue.id  <- factor(s.rums$ue.id , unique(s.rums$ue.id ))

scope.model <- glmboost( 
  
  
  formula
  
  , data =  s.rums[ s.rums$ue.id %in% current.ds.ids    ,]
  , family = Gaussian() 
  ,  control = boost_control(mstop = 1000)
  , center = FALSE
)

names <- Xs.plus.intercept 

var.imps <- as.numeric(  varimp(scope.model )  )
var.imps.ordered <- sort(var.imps, decreasing = TRUE)



names[which( var.imps == var.imps.ordered[1] ) ]
names[which( var.imps == var.imps.ordered[2] ) ]
names[which( var.imps == var.imps.ordered[3] ) ]
names[which( var.imps == var.imps.ordered[4] ) ]
names[which( var.imps == var.imps.ordered[5] ) ]
names[which( var.imps == var.imps.ordered[6] ) ]
names[which( var.imps == var.imps.ordered[7] ) ]
names[which( var.imps == var.imps.ordered[8] ) ]

}
  

  
mod.1.sp.lo.ndf <- as.formula(   feed_intake_g_d ~  bw_kg.e75 + adg_g_day + NDF_nutrition  )
mod.2.sp.lo.ndf <- as.formula(   feed_intake_g_d ~  bw_kg.e75 + adg_g_day + CP_nutrition  )
mod.3.sp.lo.ndf <- as.formula(  feed_intake_g_d  ~  bw_kg.e75 + adg_g_day +  NDF_nutrition + CP_nutrition  )
mod.4.sp.lo.ndf <- as.formula(   feed_intake_g_d  ~ bw_kg.e75 + adg_g_day + NDF_nutrition + NDF_digest )

mod.1.sp.hi.ndf <- mod.1.sp.lo.ndf
mod.2.sp.hi.ndf <- mod.2.sp.lo.ndf
mod.3.sp.hi.ndf <- as.formula(   feed_intake_g_d  ~ bw_kg.cbd+ adg_g_day.sqd + NDF_nutrition + CP_nutrition.cbd)
mod.4.sp.hi.ndf <- mod.4.sp.lo.ndf

mod.1.gt.lo.ndf <- as.formula(   feed_intake_g_d  ~ bw_kg.cbd + adg_g_day.cbd + NDF_nutrition  )
mod.2.gt.lo.ndf <- as.formula(   feed_intake_g_d  ~ bw_kg + bw_kg.cbd  + adg_g_day + NDF_nutrition  )

mod.1.gt.hi.ndf <- as.formula(  feed_intake_g_d  ~ bw_kg + adg_g_day + NDF_nutrition )
mod.2.gt.hi.ndf <- as.formula(   feed_intake_g_d   ~ bw_kg + bw_kg.cbd  + adg_g_day + NDF_nutrition  )

d.gbr[gbm.cond.shp.lo.ndf & d.gbr$mod.form == 1, 'gbr.form'] <- listify(  mod.1.sp.lo.ndf )
d.gbr[gbm.cond.shp.lo.ndf & d.gbr$mod.form == 2 , 'gbr.form'] <- listify( mod.2.sp.lo.ndf )
d.gbr[gbm.cond.shp.lo.ndf & d.gbr$mod.form == 3 , 'gbr.form'] <- listify( mod.3.sp.lo.ndf )
d.gbr[gbm.cond.shp.lo.ndf & d.gbr$mod.form == 4 , 'gbr.form'] <- listify( mod.4.sp.lo.ndf )

d.gbr[gbm.cond.shp.hi.ndf & d.gbr$mod.form == 1 , 'gbr.form'] <- listify( mod.1.sp.lo.ndf )
d.gbr[gbm.cond.shp.hi.ndf  & d.gbr$mod.form == 2, 'gbr.form'] <-  listify( mod.2.sp.lo.ndf )
d.gbr[gbm.cond.shp.hi.ndf  & d.gbr$mod.form == 3, 'gbr.form'] <-  listify( mod.3.sp.lo.ndf )
d.gbr[gbm.cond.shp.hi.ndf  & d.gbr$mod.form == 4, 'gbr.form'] <-  listify( mod.4.sp.lo.ndf )

d.gbr[ gbm.cond.gt.lo.ndf & d.gbr$mod.form == 1, 'gbr.form'] <- listify( mod.1.sp.lo.ndf )
d.gbr[ gbm.cond.gt.lo.ndf & d.gbr$mod.form == 2, 'gbr.form'] <-  listify( mod.2.sp.lo.ndf )
d.gbr[ gbm.cond.gt.lo.ndf & d.gbr$mod.form == 3, 'gbr.form'] <-  listify( mod.3.sp.lo.ndf )
d.gbr[ gbm.cond.gt.lo.ndf & d.gbr$mod.form == 4, 'gbr.form'] <-  listify( mod.4.sp.lo.ndf )


d.gbr[ gbm.cond.gt.hi.ndf & d.gbr$mod.form == 1 , 'gbr.form'] <- listify( mod.1.sp.lo.ndf )
d.gbr[ gbm.cond.gt.hi.ndf & d.gbr$mod.form == 2 , 'gbr.form'] <-  listify( mod.2.sp.lo.ndf )
d.gbr[ gbm.cond.gt.hi.ndf & d.gbr$mod.form == 3 , 'gbr.form'] <-  listify( mod.3.sp.lo.ndf )
d.gbr[ gbm.cond.gt.hi.ndf & d.gbr$mod.form == 4 , 'gbr.form'] <-  listify( mod.4.sp.lo.ndf )


if ( random.exp.int){ 
for (r in 1:nrow(d.gbr)){
  
form <- de.listify( d.gbr[r , 'gbr.form'])

form.with.ue.id <- listify( update(  form , . ~ . + ue.id) )

d.gbr[r , 'gbr.form'] <- form.with.ue.id 
  
}} # If including random intercepts, add 'ue.id' to all formulae
  


d.gbr.null <- d.gbr

} # Formula initialisations


colnames(d.gbr)

for ( r in 1 : (  nrow(d.gbr.null)  )  ){
  
if (r == 1) { d.gbr <- d.gbr.null ; start.time <- Sys.time()}

print(paste('Running iteration ' , r , 'of ', nrow(d.gbr)))
  
#  r <- 1
#if (r == 11){ break }

{
  

{
  
species <-   d.gbr[r, 'species'] 
ndf  <-   d.gbr[r, 'ndf'] 
k <-  d.gbr[r,'k']
mv <- d.gbr[ r, 'mod.vers']
mf <-  d.gbr[ r, 'mod.form']  

r.cnd.species.ndf.mf <-  ( d.gbr$species == species & d.gbr$ndf == ndf & d.gbr$mod.form == mf )
r.cnd.species.ndf.mf.mv <-  ( d.gbr$species == species & d.gbr$ndf == ndf & d.gbr$mod.form == mf & d.gbr$mod.vers == mv)


# Extract complete id cases
# condition: if first iteration of formula
if (    any(  is.na(  d.gbr[ r.cnd.species.ndf.mf , 'treatment.IDs.CCs'] ) )  ){
  
cur.form <- d.gbr[r, 'gbr.form'][[1]] 
cur.id.list <- de.listify( d.gbr[r, 'treatment.IDs'] )

cc.id.list <- listify( gen.complete.cases(    cur.id.list , cur.form    )) 

cc.id.list.f1 <- cc.id.list 

d.gbr[ r.cnd.species.ndf.mf , 'treatment.IDs.CCs']  <- cc.id.list
d.gbr[, 'IDs.remaining'] <- cc.id.list

d.gbr[r.cnd.species.ndf.mf , 'total.sample.size'] <-  length( de.listify(cc.id.list)[[1]])

}


mod.vers <- n.mod.v.ids[  (which(mv == n.mod.v.ids)-1)  ]

fold.k.condition <- ( 
  d.gbr$species == species 
  & d.gbr$ndf == ndf 
  & d.gbr$k == k 
  & d.gbr$mod.vers == mod.vers 
  & d.gbr$mod.form == mf )



if ( any(fold.k.condition)  ){

if (       !is.na(( d.gbr[ fold.k.condition , 'k.IDs']  ) )   ) {  
  
  row <- which(fold.k.condition)
  
  ids.remaining <- de.listify( d.gbr[row, 'IDs.remaining']  )
  sampled.ids <- d.gbr[[ row , 'k.IDs']]
  
  d.gbr[ r , 'IDs.remaining'] <- listify(ids.remaining )
  
}} else {
  
  # Fold conditioning
  ids.remaining <- de.listify( d.gbr[r, 'IDs.remaining']  )

  q.total.unique.ids <-  length(unique( d.gbr[[r, 'treatment.IDs.CCs']]  ))
  
  
  q.ids.to.sample <- ceiling( ( 1 / p.k ) * q.total.unique.ids)
  
  try.attempt  <- try ( sampled.ids <<- sample(  ids.remaining ,  q.ids.to.sample)   , silent = TRUE )
  
if ( inherits(  try.attempt , "try-error") ) { 
ids.to.sample <-   length(ids.remaining)  #q.ids.to.sample - 1 

try.attempt  <- try ( sampled.ids <<- sample(  ids.remaining ,  ids.to.sample )   , silent = TRUE )

if ( inherits(  try.attempt , "try-error") ) { 

ids.to.sample <-   q.ids.to.sample - 2

sampled.ids <<- sample( ids.remaining ,    ids.to.sample)  
}}

  ids.remaining <-  ids.remaining[  (which( !(ids.remaining %in% sampled.ids))) ]
  
  next.iter <- (d.gbr$species == species & d.gbr$ndf == ndf &  d.gbr$mod.vers == mv & d.gbr$k == (k + 1) & d.gbr$mod.form == mf )
  
  d.gbr[next.iter , 'IDs.remaining'] <-   listify( ids.remaining )
  
}


} # Iteration traits


d.gbr[r, 'k.IDs'] <-  listify( sampled.ids)
d.gbr[r, 'k.IDs.length'] <-  listify( sampled.ids)


all.ids <-   d.gbr[[r, 'treatment.IDs.CCs']][  which(( d.gbr[[r, 'treatment.IDs.CCs']]  %in% d.gbr[[r, 'treatment.IDs.CCs']] ))  ]
train.ids <-   d.gbr[[r, 'treatment.IDs.CCs']][  which(!( d.gbr[[r, 'treatment.IDs.CCs']]  %in% d.gbr[[r, 'k.IDs']] ))  ]
test.ids <-   d.gbr[[r, 'treatment.IDs.CCs']][  which(( d.gbr[[r, 'treatment.IDs.CCs']]  %in% d.gbr[[r, 'k.IDs']] ))  ]

# Ids for experiment
train.ids.exmt <- unique( s.rums[s.rums$ut.id %in% train.ids , 'ue.id'])
test.ids.exmt <- unique( s.rums[s.rums$ut.id %in% test.ids , 'ue.id'])

d.gbr[r, 'train.IDs'] <-  listify( train.ids )
d.gbr[r, 'train.IDs.length'] <-   length(train.ids)

main.data.r.cond <- d.gbr[r, 'r.cond'][[1]]

all.data <- s.rums[ s.rums$ut.id %in% all.ids   , ]  #main.data.r.cond

d.gbr[r, 'all.data'] <- listify( all.data ) 

# Set observed values
train.data <- all.data[  all.data$ut.id %in%  train.ids, ]
test.data <- all.data[  all.data$ut.id %in%  test.ids, ]

{
  

formula <- de.listify( d.gbr[  r , 'gbr.form'] )

model.return <- gen.glm.model(  train.data ,  formula , d.gbr[r, 'mod.vers'] )


model <- de.listify( model.return )[[1]]

m.stop <- de.listify( model.return[2] )

nu <- de.listify( model.return[3] ) 

model.offset <- as.numeric(model$offset)

# store in dataframe
d.gbr[r,  'gbr.model'] <- listify ( model )  
d.gbr[r,   'm.stop'] <- m.stop
d.gbr[r, 'nu']  <-   nu

ordered.var.names <- extract.ordered.vars(model)


d.gbr[r, 'gbr.model.train.coef.int']  <- as.numeric(coef(model  )[ordered.var.names[1]] ) 
d.gbr[r, 'gbr.model.train.coef.BW']  <- as.numeric(coef(model  )[  ordered.var.names[2]] ) 
d.gbr[r, 'gbr.model.train.coef.ADG']  <- as.numeric(coef(model  )[   ordered.var.names[3]] ) 
d.gbr[r, 'gbr.model.train.coef.NDF']  <- as.numeric(coef(model  )[ ordered.var.names[4]] ) 
d.gbr[r, 'gbr.model.train.coef.NDF*Digest']  <- as.numeric(coef(model  )[ "NDF_nutrition"] ) 

d.gbr[r, 'gbr.model.train.coef.offset.plus.intercept']  <- as.numeric(coef(model  )['(Intercept)'] ) + model.offset 
if ( is.na( as.numeric(coef(model  )['(Intercept)'] ))){ d.gbr[r, 'gbr.model.train.coef.offset.plus.intercept']  <- model.offset  }


} # -- MODEL TRAINING --


trai.eval <- function(){
  
d.gbr[r, 'train.r2'] <- de.listify( gen.eval.metrics( model  ,  formula , train.data , train.data[,'feed_intake_g_d']  , train.ids , train.ids.exmt , model.offset , 'train' , r)[[1]] )   
d.gbr[r, 'train.nrmse'] <-  de.listify( gen.eval.metrics( model  , formula , train.data , train.data[,'feed_intake_g_d']  , train.ids , train.ids.exmt , model.offset , 'train', r )[[2]] )  
d.gbr[r, 'train.w.r2'] <-  de.listify( gen.eval.metrics( model  ,  formula ,train.data , train.data[,'feed_intake_g_d']  , train.ids , train.ids.exmt , model.offset, 'train' , r)[[3]] )   
d.gbr[r, 'train.w.nrmse'] <- de.listify( gen.eval.metrics( model  ,  formula ,train.data , train.data[,'feed_intake_g_d']  , train.ids , train.ids.exmt , model.offset, 'train', r)[[4]] )  


} # -- TRAINING MODEL EVALUATION - train data


{
  
d.gbr[r, 'observed.Y'] <- listify( all.data[,'feed_intake_g_d'] )

mean.bw <- mean( all.data$bw_kg ) 
mean.adg <- mean( all.data$adg_g_day ) 
mean.ndf <- mean( all.data$NDF_nutrition) 
mean.milk <- mean( all.data$milk_kg_day) 

# Re estimate model
model.return.ws <- gen.glm.model(  all.data,  formula , d.gbr[r, 'mod.vers'] )

ws.model <- de.listify( model.return.ws )[[1]]

ws.m.stop <- de.listify( model.return.ws[2] )

ws.nu <- de.listify( model.return.ws[3] ) 

ws.model.offset <- as.numeric(ws.model$offset)

d.gbr[r, 'gbr.whole.sample'] <- listify ( ws.model )  
d.gbr[r, 'ws.ut.ids'] <- listify ( all.data$ut.id )  



if (  substr(mv,1,1)  == 1 )  {  # Specific to paramettric models
  
 
# other method coef(  ws.model , off2int = TRUE)
  
ordered.var.names <- extract.ordered.vars(ws.model)
  
  
ws.intercept.coef <- as.numeric(coef(ws.model )[ ordered.var.names[1] ]) 
ws.bw_kg.coef <- as.numeric(coef(ws.model)[  ordered.var.names[2]  ] )
ws.adg_g_day.coef <- as.numeric(coef(ws.model )[ ordered.var.names[3]  ] )
ws.NDF_nutrition.coef <- as.numeric(coef(ws.model )[ ordered.var.names[4]  ] )
ws.CP_nutrition.coef <- as.numeric(coef(ws.model )[ ordered.var.names[5]  ] )
ws.NDF_digest.coef <- as.numeric(coef(ws.model )[ ordered.var.names[6]  ] )

ws.offset.coef <- as.numeric(model$offset)

#intercept.scaled <-  ws.intercept.coef + mean.bw *  ws.bw_kg.coef  + mean.adg *  ws.adg_g_day.coef + mean.ndf *  ws.NDF_nutrition.coef


d.gbr[r, 'ws.coef.intercept'] <-  ws.intercept.coef 
d.gbr[r, 'ws.coef.bw_kg'] <- ws.bw_kg.coef
d.gbr[r, 'ws.coef.adg_g_day'] <- ws.adg_g_day.coef
d.gbr[r, 'ws.coef.NDF_nutrition'] <- ws.NDF_nutrition.coef
d.gbr[r, 'ws.coef.CP_nutrition'] <- ws.CP_nutrition.coef
d.gbr[r, 'ws.coef.NDF_digest'] <- ws.NDF_digest.coef
d.gbr[r, 'ws.coef.offset'] <- ws.offset.coef
d.gbr[r, 'ws.coef.offset.plus.intercept'] <- (1) * ws.offset.coef + ws.intercept.coef 

if ( is.na(ws.intercept.coef )) { d.gbr[r, 'ws.coef.offset.plus.intercept'] <- (1) * ws.offset.coef }

if ( random.exp.int ){  ws.predicted <- predict.manual( formula , all.data ,  ws.offset.coef , 'whole' , r )              } 
if ( !random.exp.int ){  ws.predicted <- predict(ws.model , data = all.data )            } 




d.gbr[r, 'ws.predicted'] <- listify( ws.predicted )

ws.residuals  <- ws.predicted - all.data[,'feed_intake_g_d']
d.gbr[r, 'ws.residuals'] <- listify(ws.residuals )
d.gbr[r, 'ws.rms.residuals'] <- listify( sqrt(ws.residuals^2) )
#d.gbr[r, 'ws.bw_kg.measrs'] <- listify( ws.bw_kg )


}

} # -- MODEL PREDICTION and evaluation -- Whole sample


{



#d.gbr[r, 'predicted.mod'] <-   listify( as.numeric(test.predicted) )

  
d.gbr[r, 'coef.int']  <- as.numeric(coef(model  )[ ordered.var.names[1] ] ) 
d.gbr[r, 'coef.BW']  <- as.numeric(coef(model  )[  ordered.var.names[2]  ] ) 
d.gbr[r, 'coef.ADG']  <- as.numeric(coef(model  )[   ordered.var.names[3]  ] ) 
d.gbr[r, 'coef.NDF']  <- as.numeric(coef(model  )[ ordered.var.names[4] ] ) 
d.gbr[r, 'coef.CP']  <- as.numeric(coef(model  )[ ordered.var.names[5] ] ) 
d.gbr[r, 'coef.NDF_Digest']  <- as.numeric(coef(model  )[ ordered.var.names[6]]) 

d.gbr[r, 'coef.offset.plus.intercept']  <- as.numeric(coef(model  )[ordered.var.names[1]] ) + model.offset 
if ( is.na( as.numeric(coef(model  )['(Intercept)'] ))){ d.gbr[r, 'coef.offset.plus.intercept']  <- model.offset  }


if ( !random.exp.int ){   test.predicted <- predict( model , newdata = test.data)  }
if ( random.exp.int ){   test.predicted <- predict.manual(  formula  , test.data , model.offset , 'test' , r )  }



# Evaluation
d.gbr[r, 'test.r2'] <- de.listify( gen.eval.metrics( model  , formula ,  test.data , test.data[,'feed_intake_g_d']  , test.ids , test.ids.exmt , model.offset , 'test' , r )[[1]] )   
d.gbr[r, 'test.nrmse'] <-  de.listify( gen.eval.metrics( model  , formula ,   test.data , test.data[,'feed_intake_g_d']  , test.ids , test.ids.exmt , model.offset , 'test' , r)[[2]] )  
d.gbr[r, 'test.w.r2'] <-  de.listify( gen.eval.metrics( model  ,  formula ,  test.data , test.data[,'feed_intake_g_d']  , test.ids , test.ids.exmt , model.offset , 'test' , r)[[3]] )   
d.gbr[r, 'test.w.nrmse'] <- de.listify( gen.eval.metrics( model  ,  formula ,  test.data , test.data[,'feed_intake_g_d']  , test.ids , test.ids.exmt , model.offset , 'test' , r)[[4]] )  

#d.gbr[r, 'test.aic'] <- AIC(  model )


ccc <- CCC(  as.numeric(test.predicted)  ,  as.numeric(test.data[,'feed_intake_g_d'] )  , ci = "z-transform", conf.level = 0.95, na.rm = FALSE)
  
d.gbr[r, 'test.ccc'] <- ccc$rho.c$est  


} # MODEL TESTING


if ( k == p.k) {
  
 

  d.gbr[ r.cnd.species.ndf.mf.mv ,  vn.w.R2.mean ] <- mean(na.omit(d.gbr[ d.gbr$species ==  species & d.gbr$ndf == ndf   , 'test.r2']))
  d.gbr[r.cnd.species.ndf.mf.mv,  vn.w.nRMSE.mean ] <- mean(na.omit(d.gbr[ d.gbr$species ==  species & d.gbr$ndf == ndf , 'test.nrmse']))
  d.gbr[r.cnd.species.ndf.mf.mv, vn.w.CCC.mean] <- mean(na.omit(d.gbr[ d.gbr$species ==  species & d.gbr$ndf == ndf , 'test.ccc']))  
  
 
  d.gbr[r.cnd.species.ndf.mf.mv, vn.w.R2.sd ] <- sd(na.omit(d.gbr[ d.gbr$species ==  species & d.gbr$ndf == ndf , 'test.r2']))
  d.gbr[r.cnd.species.ndf.mf.mv, vn.w.nRMSE.sd ] <- sd(na.omit(d.gbr[ d.gbr$species ==  species & d.gbr$ndf == ndf , 'test.nrmse']))
  
  d.gbr[r.cnd.species.ndf.mf.mv, 'w.AIC.mean'] <- mean(na.omit(d.gbr[ d.gbr$species ==  species & d.gbr$ndf == ndf , 'test.aic']))

  # Coefficients - means and standard deviations
  d.gbr[r.cnd.species.ndf.mf.mv, 'test.w.coef.bw.sd'] <- 1 # sd(na.omit(d.gbr[ d.gbr$species ==  species & d.gbr$ndf == ndf , 'test.r2']))
  
  # Average regression coefficients
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.coef.int'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'coef.int']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.coef.BW'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'coef.BW']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.coef.ADG'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'coef.ADG']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.coef.NDF'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'coef.NDF']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.coef.CP'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'coef.CP']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.coef.NDF_digest'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'coef.NDF_digest']))
  
 # d.gbr[r, 'coef.int']  <- as.numeric(coef(model  )[ ordered.var.names[1] ] ) 
#  d.gbr[r, 'coef.BW']  <- as.numeric(coef(model  )[  ordered.var.names[2]  ] ) 
 # d.gbr[r, 'coef.ADG']  <- as.numeric(coef(model  )[   ordered.var.names[3]  ] ) 
 # d.gbr[r, 'coef.NDF']  <- as.numeric(coef(model  )[ ordered.var.names[4] ] ) 
  #d.gbr[r, 'coef.NDF*Digest']  <- as.numeric(coef(model  )[ "NDF_nutrition"] ) 
  
  
  
  
} # MODEL METRICS ACROSS FOLDS
  
# Model identification 1 -- best performing among given formula (i.e. across hyperparameters)
if ( k == p.k &  mv == col.mod.vers[  length(col.mod.vers )  ]   ) {
  
  species.ndf.form.cond <- (d.gbr$species == species & d.gbr$ndf == ndf & d.gbr$mod.form == mf  )
  
  cond.pmetric <- (substr(d.gbr$mod.vers , 1 ,1 ) == 1)
  cond.n.pmetric <- (substr(d.gbr$mod.vers , 1 ,1 ) == 2)
  
  species.ndf.form.cond.pmetric <- species.ndf.form.cond & cond.pmetric
  species.ndf.form.cond.n.pmetric <- species.ndf.form.cond & cond.n.pmetric
  
  # Parametric model

  d.gbr <- assign.best.model(species.ndf.form.cond.pmetric , d.gbr )
  d.gbr <- assign.best.model(species.ndf.form.cond.n.pmetric , d.gbr)

}
  
# Model identification 2 -- best performing among all formula (i.e. across hyperparameters)
if ( k == p.k & mv == col.mod.vers[  length(col.mod.vers )  ] & mf == n.mod.form  ) {
  
  species.ndf.best.mod.cond <- (d.gbr$species == species & d.gbr$ndf == ndf & d.gbr$is.best.model  )
  
  cond.pmetric <- (substr(d.gbr$mod.vers , 1 ,1 ) == 1)
  cond.n.pmetric <- (substr(d.gbr$mod.vers , 1 ,1 ) == 2)
  
  species.ndf.form.cond.pmetric <- species.ndf.best.mod.cond  & cond.pmetric
  species.ndf.form.cond.n.pmetric <- species.ndf.best.mod.cond  & cond.n.pmetric
  
  
  d.gbr <- assign.best.model.all.forms(species.ndf.form.cond.pmetric , d.gbr )
  d.gbr <- assign.best.model.all.forms(species.ndf.form.cond.n.pmetric , d.gbr)
  
}


if ( r == nrow(d.gbr)){
  
time.elapsed <-  Sys.time() - start.time
print(paste('Evaluation completed in ' , round(time.elapsed ,1), ' minutes.'))

gbr.out()
} # Report simulation time
  
  
} # Run all

} # k-fold CV



View(d.gbr)

gbr.out()



View(d.gbr)

# ---- PLOTS -------
gg.dat.m1 <- gen.gg.df(1 , 'pmetric') 
gg.dat.m2 <- gen.gg.df(2 , 'pmetric') 
gg.dat.m3 <- gen.gg.df(3 , 'pmetric') 
gg.dat.m4 <- gen.gg.df(4 , 'pmetric') 


gg.dat.m1 <- gen.gg.df(1 , 'n.pmetric') 
gg.dat.m2 <- gen.gg.df(2 , 'n.pmetric') 
gg.dat.m3 <- gen.gg.df(3 , 'pmetric') 
gg.dat.m4 <- gen.gg.df(4 , 'pmetric') 



gg.dat <- rbind(
  gg.dat.m1
  ,  gg.dat.m2 
 #, gg.dat.m3
# , gg.dat.m4
  )

gg.d.sp.lon <- gg.dat[gg.dat$species.ndf   == sheep.lo.ndf   , ]
gg.d.sp.hin <- gg.dat[gg.dat$species.ndf   == sheep.hi.ndf , ]
gg.d.gt.lon <- gg.dat[gg.dat$species.ndf   == goat.lo.ndf , ]
gg.d.gt.hin <- gg.dat[gg.dat$species.ndf   == goat.hi.ndf  , ]


{
  gg.valid.y.tit <<- 'Predicted Intake (g/d)'  
  gg.valid.x.tit <<- 'Measured Intake (g/d)'  
  
  
  gg.resid.y.tit <<- 'Residuals (g/d)'
  gg.resid.x.tit <<- 'Fitted intake (g/d)'
  
  gg.valid.form.lab.x.crd.lev.1  <- 650
  gg.valid.form.lab.x.crd.lev.2  <-  gg.valid.form.lab.x.crd.lev.1 
  gg.valid.form.lab.x.crd.lev.3  <-  gg.valid.form.lab.x.crd.lev.1 
  
  # Y coordinates of formula labels
  inc <- 0.078
  row.2.sclr <- 1 - inc 
  row.3.sclr <- 1 - inc * 2
  row.4.sclr <- 1 - inc * 3
  
  sp.lon.max.y <- 1500 #max( s.rums[cond.sheep.hi.ndf , ]) 
  gg.valid.form.lab.y.crd.sp.lon.lev.1  <- sp.lon.max.y * row.2.sclr
  gg.valid.form.lab.y.crd.sp.lon.lev.2  <- sp.lon.max.y * row.3.sclr
  gg.valid.form.lab.y.crd.sp.lon.lev.3  <- sp.lon.max.y * row.4.sclr
  
  
  sp.hin.max.y <- 1300 #max( s.rums[cond.sheep.hi.ndf , ]) 
  gg.valid.form.lab.y.crd.sp.hin.lev.1  <- sp.hin.max.y * row.2.sclr
  gg.valid.form.lab.y.crd.sp.hin.lev.2  <- sp.hin.max.y * row.3.sclr
  gg.valid.form.lab.y.crd.sp.hin.lev.3  <- sp.hin.max.y * row.4.sclr
  
  
  y.lab.inc <- 90
  gg.valid.lab.y.crd.lev.1  <- 600
  gg.valid.lab.y.crd.lev.2  <- gg.valid.lab.y.crd.lev.1 - y.lab.inc
  gg.valid.lab.y.crd.lev.3  <- gg.valid.lab.y.crd.lev.2 - y.lab.inc
  
  gg.valid.lab.x.crd.sp.lon   <- 1300
  gg.valid.lab.x.crd.sp.hin   <- 1150
  
  gg.valid.lab.fs <- 2.4
  
  
  gg.dat[gg.dat$species == species.sheep & gg.dat$ndf == ndf.lev.lo , 'species.ndf' ] <- 'Sheep - Low NDF'
  gg.dat[gg.dat$species == species.sheep & gg.dat$ndf == ndf.lev.hi , 'species.ndf' ] <- 'Sheep - High NDF'
  gg.dat[gg.dat$species == species.goat & gg.dat$ndf == ndf.lev.lo , 'species.ndf' ] <- 'Goat - Low NDF'
  gg.dat[gg.dat$species == species.goat & gg.dat$ndf == ndf.lev.hi , 'species.ndf' ] <- 'Goat - High NDF'
  

  
  unique.species.ndf <- unique( gg.dat$species.ndf )
  gg.dat$species.ndf <- factor( gg.dat$species.ndf  , levels = unique.species.ndf)
  
  
  gg.y.valid.theme <- ggplot(  ) +
    theme(
      panel.grid.major = element_blank()
      ,panel.background = element_blank()
      ,panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
      
      ,strip.background = element_rect(color='black', fill='white', size=1, linetype="solid")
      ,strip.text.x = element_text(size =  7.75 , color = 'black'  )
      
      ,strip.text.y = element_text(size =  9.75 , color = 'black' , face = "bold" )
      
      , axis.text.x = element_text( angle = 90 , hjust = 1 , vjust = 0.5 , size = 10.5)
      , axis.text.y = element_text( hjust = 1 , vjust = 0.5 , size = 10.5)
    ) 
  
  gg.valid.pnt.size <<- 1
  gg.valid.pnt.colr <<- 'black'
  
} # Plot params



# View(d.gbr)

gen.gg.valid <- function(  dat , xc ,  yr.1 , yr.2 , yr.3 ){
  
  # test:
  test <- function(){
    dat <- gg.d.sp.lon 
    xc <- gg.valid.lab.x.crd.sp.lon
    yr.1 <- gg.valid.lab.y.crd.lev.1
    yr.2 <- gg.valid.lab.y.crd.lev.2
    yr.3 <- gg.valid.lab.y.crd.lev.3
  }
  
  
plot <- gg.y.valid.theme   %>% +
  #  ggplot() + 
    geom_point( data = dat  ,  aes(x = observed , y = modelled), size = gg.valid.pnt.size ,  color = gg.valid.pnt.colr ) +
    
  
     ggh4x::facet_nested( 
      species.ndf ~ col.mod.form.label.r1 + col.mod.form.label.r2
      , strip = strip_nested(size = "variable")
      
    ) +
  geom_line(data = data.frame(x = c(-Inf, Inf), y = c(-Inf, Inf)), 
            aes(x = x, y = y   , color = "y = x")) + 
 # geom_abline( aes(  x = y , y = x ,  color = 'Predicted = Observed') , linetype = "solid") +
  geom_smooth(data = dat  ,  aes(x = observed , y = modelled , color = 'Best fit' ), method="lm", se=FALSE , size = .6 , linetype = 'solid') +
    # LABELS
    geom_label( 
      data = dat ,
      aes( x = xc  
           , y =   yr.1
           , label =  label.ccc )
      , label.size = NA
      , parse = FALSE
      , size = gg.valid.lab.fs 
    ) +
    geom_label( 
      data = dat  ,
      aes( x = xc 
           , y = yr.2
           , label =  label.r2 )
      , label.size = NA
      , parse = FALSE
      , size = gg.valid.lab.fs 
    ) +
    geom_label( 
      data = dat  ,
      aes( x = xc    
           , y = yr.3
           , label =  label.nrmse )
      , label.size = NA
      , parse = FALSE
      , size = gg.valid.lab.fs 
    )  + ylab(gg.valid.y.tit) + 
    xlab(gg.valid.x.tit) +
    theme(
      # Render HTML in the strips
      #  strip.text = element_markdown(hjust = 0.5)
      # strip.x ==  strip_nested(size = "variable")
      legend.position = 'inside'
      , legend.position.inside = c(0.15, 0.875)
      , legend.title = element_blank()
    ) + # + scale_color_manual(values = c( "LOBF" = "red")) +
  scale_color_manual(
    name = ''
    , values =   c( 
      "Best fit" = "#8EC5FF"
      , "y = x"  = 'black'
    ) 
    , breaks = c(
      "Best fit"
     ,  'y = x'
    )) +
  guides(color = guide_legend(override.aes = list( ncol = 2 , linetype = c(1, 1))))+
  guides(color = guide_legend(ncol = 2))


return (   plot  ) 
}

gg.y.valid.sp.lon.0 <- gen.gg.valid(gg.d.sp.lon ,gg.valid.lab.x.crd.sp.lon  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3  )
  
gg.y.valid.sp.hin.0 <- gen.gg.valid(gg.d.sp.hin ,gg.valid.lab.x.crd.sp.hin  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3  )

gg.y.valid.sp.lon.0 

gg.y.valid.sp.hin.0


# GOATS
gg.y.valid.gt.lon.0 <- gen.gg.valid(gg.d.gt.lon , 500  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3  )

gg.y.valid.gt.lon.0

gg.y.valid.gt.hin.0 <- gen.gg.valid(gg.d.gt.hin ,600 , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3  )

gg.y.valid.gt.hin.0

# MERGED PLOT OBJECTS

gg.y.valid.sp.lon.p <- gg.y.valid.sp.lon.0 %>% + 
  theme(
    axis.text.x = element_blank()
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
  )

gg.y.valid.sp.hin.p <- gg.y.valid.sp.hin.0 %>% + 
  theme(
 axis.title.y = element_blank()
    , axis.title.x = element_blank()
    
    ,  strip.background = element_rect(color='black', fill='white', size=1, linetype="solid")
    
  )


heights.sp <- c( 0.9 , 1 )

gg.valid.sp.all <- ggarrange(
  
  gg.y.valid.sp.lon.p
  , gg.y.valid.sp.hin.p
  
  , nrow = 2
  , heights =  heights.sp
)


gg.valid.sp.all <- annotate_figure(  gg.valid.sp.all  , 
                                bottom = text_grob(
                                  gg.valid.x.tit
                                  , color = "black"
                                  , hjust = 0.5
                                  , vjust = 0.5
                                  , x = 0.5
                                  , size = 12
                                ),
                                left= text_grob(
                                  gg.valid.y.tit
                                  , color = "black"
                                  , hjust = 0.5
                                  , vjust = 0.5
                                  , x = 0.5
                                  , rot = 90
                                  , size = 12
                                )
)

gg.valid.sp.all



plot.dpi  <-  1000

p.glob.scalar <- 0.625
p.width.sp <- 11 * p.glob.scalar
p.height.sp  <- 7.75 * p.glob.scalar

filename.sp = 'Figures.out/sheep.jpeg'


ggsave(filename =  filename.sp ,  gg.valid.sp.all , width = p.width.sp, height = p.height.sp  , dpi = plot.dpi )





gg.y.valid.gt.hin.0 <- gg.y.valid.theme   %>% +
  
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "solid") + 
  geom_point( data = gg.d.gt.lon  ,  aes(x = observed , y = modelled) , size = gg.valid.pnt.size ,  color = gg.valid.pnt.colr) +
  # geom_text( data = gg.dat ,  aes(x = observed , y = modelled , label = label.ut.id) , color = 'black' , size = 2.5 , nudge_x = 1 , nudge_y =1  )  + 
  ggh4x::facet_nested( 
    species.ndf ~  col.mod.form.label.r2
    , strip = strip_nested(size = "variable")
    
  ) +
  # LABELS
  geom_label( 
    data = gg.d.gt.lon  ,
    aes( x = gg.valid.lab.x.crd.sp.hin  
         , y = gg.valid.lab.y.crd.lev.1 
         , label =  label.ccc )
    , label.size = NA
    , parse = FALSE
    , size = gg.valid.lab.fs 
  ) +
  geom_label( 
    data = gg.d.gt.lon  ,
    aes( x = gg.valid.lab.x.crd.sp.hin  
         , y = gg.valid.lab.y.crd.lev.2 
         , label =  label.r2 )
    , label.size = NA
    , parse = FALSE
    , size = gg.valid.lab.fs 
  ) +
  geom_label( 
    data = gg.d.gt.lon,
    aes( x = gg.valid.lab.x.crd.sp.hin  
         , y = gg.valid.lab.y.crd.lev.3  
         , label =  label.nrmse )
    , label.size = NA
    , parse = FALSE
    , size = gg.valid.lab.fs 
  )  + ylab(gg.valid.y.tit) + 
  xlab(gg.valid.x.tit) 

gg.y.valid.gt.lon.0




gg.resids <- gg.y.valid.theme   %>% +  
 geom_point( data = gg.dat ,  aes(x = modelled, y = residual.1) , color = 'darkgrey') +
 # stat_summary(data = gg.dat ,  aes(x = modelled, y = residual.1) , fun = mean, geom = "line", color = "blue", size = 1) +
  geom_abline(intercept = 0, slope = 0, color = "black", linetype = "solid") + 
  
  geom_smooth(data = gg.dat , aes(x = modelled , y = residual.1)  , method = 'loess' , se = FALSE , color = 'black' , span = 0.9, size = .75)+
  #geom_text( data = gg.dat ,  aes(x = observed , y = modelled , label = label.ut.id) , color = 'black' , size = 2.5 , nudge_x = 1 , nudge_y =1  )  + 
  facet_grid( . ~ species.ndf ) + 
  ylab(gg.resid.y.tit) + 
  xlab(gg.resid.x.tit) 
   
gg.resids




gg.rmsr <- gg.y.valid.theme   %>% +  
  geom_point( data = gg.dat ,  aes(x = modelled, y = rms.residual.1) , color = 'darkgrey') +
  # stat_summary(data = gg.dat ,  aes(x = modelled, y = residual.1) , fun = mean, geom = "line", color = "blue", size = 1) +
 # geom_abline(intercept = 0, slope = 0, color = "black", linetype = "solid") + 
  
  geom_smooth(data = gg.dat , aes(x = modelled , y = rms.residual.1)  , method = 'loess' , se = FALSE , color = 'black' , span = 4, size = .75) +
  #geom_text( data = gg.dat ,  aes(x = observed , y = modelled , label = label.ut.id) , color = 'black' , size = 2.5 , nudge_x = 1 , nudge_y =1  )  + 
  facet_grid( . ~ species.ndf ) 

gg.resids
   

gg.y.bw <- gg.y.valid.theme   %>% +  
  geom_point( data = gg.dat ,  aes(x = bw_kg , y = modelled) , color = 'darkgrey')  + 
  
  geom_abline( data = gg.dat , aes( intercept = coef.intercept , slope = coef.bw_kg ) , color = "black", linetype = "solid")  +
facet_grid( . ~ species.ndf ) 









