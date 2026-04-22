

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


"""
Next steps
- sub/superscripts in labels
- weights
- layers and complete cases


"""



rm(list = ls())


save.image('dmi.estm8.RData')
load('dmi.estm8.RData')

source('libraries.R') ; source('functions.R') ; source('parameters.R') ; source('DMI.data.prep.R') 


# Data prep 
{

s.rums <- d[  d$Species %in% species.sheep | d$Species %in% species.goat , ]


s.rums <- s.rums[   !is.na(s.rums$NDF_nutrition)  , ] 


s.rums <- s.rums[   , c(gbr.reg.variables ) ] 


s.rums$dummy <- 1

pred.vars <- c(
  'bw_kg'
  ,  'adg_g_day'
  
  ,'NDF_nutrition'
  ,'CP_nutrition'
  , 'Ash_nutrition'
  , 'EE_nutrition'
  
  , 'NDF_digest'
  
)
dep.vars <- (  y.var.reg  )


# Variable bounds checks

s.rums[ , 'NDF_digest.redf'] <- FALSE ;  s.rums[ , 'NDF_nutrition.redf'] <- FALSE ; s.rums[ , 'outlier.adg'] <- FALSE

for (r in 1:nrow(s.rums)){
  
  s.rums[r,'drym_intake_g_d'] <- s.rums[r,'feed_intake_g_d'] * s.rums[r,'DM_nutrition'] / 1000

  if ( !is.na(s.rums[r , 'NDF_nutrition']) & s.rums[r , 'NDF_nutrition'] < 75) {s.rums[r , 'NDF_nutrition'] <- 10 * s.rums[r , 'NDF_nutrition'] ; s.rums[r , 'NDF_nutrition.redf'] <- TRUE }
  if ( !is.na(s.rums[r , 'NDF_digest']) & s.rums[r , 'NDF_digest'] <= 1) {s.rums[r , 'NDF_digest'] <- 100 * s.rums[r , 'NDF_digest'] ; s.rums[r , 'NDF_digest.redf'] <- TRUE}
  if ( !is.na(s.rums[r , 'NDF_digest']) & s.rums[r , 'NDF_digest'] >= 100) {s.rums[r , 'NDF_digest'] <- s.rums[r , 'NDF_digest'] / 10 ; s.rums[r , 'NDF_digest.redf'] <- TRUE }
 
  if ( !is.na(s.rums[r , 'adg_g_day']) & s.rums[r , 'adg_g_day'] >= max.adg.g.day) {s.rums[r , 'outlier.adg'] <- TRUE }
  if ( !is.na(s.rums[r , 'adg_g_day']) & s.rums[r , 'adg_g_day'] <= min.adg.g.day) {s.rums[r , 'outlier.adg'] <- TRUE }
}


s.rums$ol.status.var.ranges <- FALSE
s.rums[  (s.rums$outlier.adg) , 'ol.status.var.ranges'] <- TRUE



# Variable interaction declarations
s.rums$NDF_x_NDF_digest <- s.rums$NDF_digest * s.rums$NDF_nutrition


s.rums[ s.rums$Species == species.sheep , 'BW_frac_Mat_BW'] <- s.rums[ s.rums$Species == species.sheep , 'bw_kg']  / 50
s.rums[ s.rums$Species == species.goat, 'BW_frac_Mat_BW'] <- s.rums[ s.rums$Species == species.goat, 'bw_kg']  / 35
  
  


# Variable standardization
{

min.max.stdz.vars <- c(dep.vars  , pred.vars )
mean.stdz.vars <- min.max.stdz.vars

min.max.stdz.labels <-  c()
mean.stdz.labels <-  c()



for (v in 1:length(min.max.stdz.vars)){
  
  min.max.stdz.labels[v] <- str_c( min.max.stdz.vars[v] , suffx.mm.s )
  mean.stdz.labels[v] <- str_c( mean.stdz.vars[v] , suffx.mean.s )
}


all.species.ndfs <<- c( sheep.lo.ndf , sheep.hi.ndf , goat.lo.ndf , goat.hi.ndf)

for (  sn in all.species.ndfs ){
for (v in 1:length(min.max.stdz.vars)) {
  
  # sn <- sheep.lo.ndf ; v <- 8
  
  r.cond <-  ( s.rums$species.ndf == sn )
  
  var.mm.s <- min.max.stdz.vars[v]
  var.mean.s <- mean.stdz.vars[v]
  
  mm.s.min <- min( na.omit(s.rums[  r.cond, var.mm.s] ) )
  mm.s.max <- max( na.omit(s.rums[ r.cond , var.mm.s]  ))
  
  mean <- mean( na.omit(s.rums[ r.cond , var.mean.s]  ))
  sd <- sd( na.omit(s.rums[ r.cond , var.mean.s]  ))
  
  range.mm.s <-  mm.s.max - mm.s.min
 # range.mean.s <-  max( na.omit(s.rums[s.rums$species.ndf == sheep.lo.ndf  ,'bw_kg']  ))  - min( na.omit(s.rums[ s.rums$species.ndf == sheep.lo.ndf ,'bw_kg'] ) )
  
  label.mm.s <-   min.max.stdz.labels[v]
  label.mean.s <-   mean.stdz.labels[v]
  
  s.rums[   r.cond  ,  label.mm.s] <-  ( s.rums[   r.cond ,   var.mm.s ] -  mm.s.min) /   range.mm.s 
  
  
  s.rums[   r.cond  ,   label.mean.s] <-  ( s.rums[   r.cond ,   var.mm.s ] -   mean) /  sd
  
  
}}

}



all.vars <- c( dep.vars,  pred.vars)

suffixes <- c(
  
   '.e25'
  , '.sqt' 
  , '.e75'
  , '.sqd'
  , '.cbd' 
  , '.log'
  , '.recip'
  
  , '.mean.s'
  , '.min.max.s'
)
  

for (v in all.vars){
  
  v.e25 <- str_c( v , '.e25')
  v.sqt <- str_c( v , '.sqt')
  v.e75 <- str_c( v , '.e75')
  v.sqd <- str_c( v , '.sqd')
  v.cbd <- str_c( v , '.cbd')
  v.log <- str_c( v , '.log')
  v.recip <- str_c( v , '.recip')
  
  s.rums[ , v.e25 ] <-  s.rums[,v]^0.25
  s.rums[ , v.sqt ] <-  s.rums[,v]^0.5
  s.rums[ , v.e75 ] <-  s.rums[,v]^0.75
  s.rums[,v.sqd] <-  s.rums[,v]^2
  s.rums[,v.cbd] <-  s.rums[,v]^3
  s.rums[,v.log] <-  log(s.rums[,v])
  s.rums[,v.recip] <-  1 / s.rums[,v]

  
}


v.list.DV <- c( y.var.reg )
v.list.BW <- c('dummy' , 'bw_kg' )   
v.list.DG <- c('dummy' , 'adg_g_day' ) 
v.list.NDF <- c('dummy' , 'NDF_nutrition' ) 
v.list.EE <- c('dummy' , 'EE_nutrition' )  
v.list.ASH <- c('dummy' , 'Ash_nutrition' )  
v.list.NDF.dig <- c('dummy' , 'NDF_digest' )  

for (s in suffixes){ 
  
  new.var.DV <- str_c( v.list.DV[1] ,s )
  v.list.DV <- append( v.list.DV , new.var.DV )
  
  new.var.BW <- str_c( v.list.BW[2] ,s )
  v.list.BW <- append(v.list.BW , new.var.BW )
  
  new.var.DG <- str_c( v.list.DG[2] ,s )
  v.list.DG <- append(v.list.DG , new.var.DG )
  
  new.var.NDF <- str_c( v.list.NDF[2] ,s )
  v.list.NDF <- append(v.list.NDF , new.var.NDF )
  
  new.var.EE <- str_c( v.list.EE[2] ,s )
  v.list.EE <- append(v.list.EE , new.var.EE )
  
  new.var.ASH <- str_c( v.list.ASH[2] ,s )
  v.list.ASH <- append(v.list.ASH , new.var.ASH )
  
  new.var.NDF.dig <- str_c( v.list.NDF.dig[2] ,s )
  v.list.NDF.dig <- append(v.list.NDF.dig , new.var.NDF.dig  )


}

v.list.all <<- c( v.list.DV , v.list.BW , v.list.DG , v.list.NDF , v.list.EE , v.list.ASH , v.list.NDF.dig)


# Total number of studies
print(paste('Studies total:' , length(   unique(s.rums[,  'B.Code'])  )))

print(paste('Studies  sheep - low NDF:' , length(   unique(s.rums[s.rums$NDF.level == ndf.lev.lo & s.rums$Species == species.sheep,  'B.Code'])  )))
print(paste('Studies  sheep - high NDF:' , length(   unique(s.rums[s.rums$NDF.level == ndf.lev.hi & s.rums$Species == species.sheep,  'B.Code'])  )))
print(paste('Studies  goats - low NDF:' , length(   unique(s.rums[s.rums$NDF.level == ndf.lev.lo & s.rums$Species == species.goat,  'B.Code'])  )))
print(paste('Studies   goats - high NDF:' , length(   unique(s.rums[s.rums$NDF.level == ndf.lev.hi & s.rums$Species == species.goat,  'B.Code'])  )))



# Total number of experimental units
print(paste('Experimental units total:' , nrow(s.rums[,  ])  ))

print(paste('Experimental units sheep - low NDF:' , nrow(s.rums[s.rums$NDF.level == ndf.lev.lo & s.rums$Species == species.sheep,  ])  ))
print(paste('Experimental units  - high NDF:' , nrow(s.rums[s.rums$NDF.level == ndf.lev.hi & s.rums$Species == species.sheep,  ])  ))
print(paste('Experimental units - low NDF:' , nrow(s.rums[s.rums$NDF.level == ndf.lev.lo & s.rums$Species == species.goat,  ])  ))
print(paste('Experimental units  - high NDF:' , nrow(s.rums[s.rums$NDF.level == ndf.lev.hi & s.rums$Species == species.goat,  ])  ))


# Total number of animals
print(paste('Sample size total:' , sum(s.rums[, 'T.Animals' ])  ))

print(paste('Sample size (animal units) sheep - low NDF:' , sum(s.rums[s.rums$NDF.level == ndf.lev.lo & s.rums$Species == species.sheep, 'T.Animals' ])  ))
print(paste('Sample size (animal units) sheep - high NDF:' , sum(s.rums[s.rums$NDF.level == ndf.lev.hi & s.rums$Species == species.sheep, 'T.Animals' ]) ))
print(paste('Sample size (animal units) goats - low NDF:' , sum(s.rums[s.rums$NDF.level == ndf.lev.lo & s.rums$Species == species.goat, 'T.Animals' ]) ))
print(paste('Sample size (animal units) goats - high NDF:' , sum(s.rums[s.rums$NDF.level == ndf.lev.hi & s.rums$Species == species.goat, 'T.Animals' ])  ))



# Define sub-samples

length(   unique(s.rums[ s.rums$Stage != na.value ,  'B.Code'])  ) / length(   unique(s.rums[  ,  'B.Code'])  )

s.rums$species.stage <- NULL

species.stage.goat.growing <- 'growing.goat'
species.stage.goat.adult <- 'adult.goat'
species.stage.sheep.growing <- 'growing.sheep'
species.stage.sheep.adult  <- 'adult.sheep'

# Goats
unique.stages.goat <- unique(s.rums[s.rums$Species == species.goat  , 'Stage'])
goat.stages.growing <- unique.stages.goat[c(2,3,5,8,9,10)]
goat.stages.adult <- unique.stages.goat[c(4,6,7)]
goat.stages.non.reprod <- unique.stages.goat[ c(2,3,5,6,8,9,10) ]

s.rums[  s.rums$Species == species.goat & s.rums$Stage %in% goat.stages.growing , 'species.stage'] <- species.stage.goat.growing
s.rums[  s.rums$Species == species.goat & s.rums$Stage %in% goat.stages.adult , 'species.stage'] <- species.stage.goat.adult

s.rums[ s.rums$Species == species.goat , 'stage.non.reprod'] <- FALSE
s.rums[  s.rums$Species == species.goat & s.rums$Stage %in% goat.stages.non.reprod , 'stage.non.reprod'] <- TRUE



length(unique(s.rums[s.rums$species.stage == species.stage.goat.growing , 'B.Code']))
length(unique(s.rums[s.rums$species.stage == species.stage.goat.adult , 'B.Code']))


# Sheep
unique.stages.sheep <- unique(s.rums[s.rums$Species == species.sheep  , 'Stage'])
sheep.stages.growing <- unique.stages.sheep[c(5 , 7)]
sheep.stages.adult <- unique.stages.sheep[c(1 , 3 , 4 , 6, 8 , 9 )]

sheep.stages.non.reprod <- unique.stages.sheep[ c(1,3,5,9) ]

s.rums[  s.rums$Species == species.sheep & s.rums$Stage %in% sheep.stages.growing , 'species.stage'] <- species.stage.sheep.growing
s.rums[  s.rums$Species == species.sheep & s.rums$Stage %in% sheep.stages.adult , 'species.stage'] <- species.stage.sheep.adult


s.rums[ s.rums$Species == species.sheep , 'stage.non.reprod'] <- FALSE
s.rums[  s.rums$Species == species.goat & s.rums$Stage %in% sheep.stages.non.reprod , 'stage.non.reprod'] <- TRUE


length(unique(s.rums[s.rums$species.stage == species.stage.sheep.growing , 'B.Code']))
length(unique(s.rums[s.rums$species.stage == species.stage.sheep.adult , 'B.Code']))

s.rums[  is.na(s.rums$species.stage)  , 'species.stage'] <- 'NA'

# Identify variable transformations which best approximate linearity
optim.tform.dat <- data.frame(
  variable = character()
  , species = character()
  , ndf.level = character()
  , optim.tform = character()
  , var.name = character()
  
  )


r.cnt <- 1

for (s in c(species.sheep , species.goat)){
for (n in c(ndf.lev.lo  , ndf.lev.hi )){
for (v in all.vars){
  
    
optim.tform.dat[r.cnt,'variable'] <- v
optim.tform.dat[r.cnt,'species'] <- s
optim.tform.dat[r.cnt,'ndf.level'] <- n

parms <- optim.tform(s.rums , v, s , n )

optim.tform.dat[optim.tform.dat$species == s & optim.tform.dat$ndf.level == n & optim.tform.dat$variable == v, 'optim.tform']  <- parms[1]
optim.tform.dat[optim.tform.dat$species == s & optim.tform.dat$ndf.level == n & optim.tform.dat$variable == v , 'var.name']  <- parms[2]

r.cnt <- r.cnt + 1
}
}
}


# Check study quantities across gradients of variables
cond.sheep.lo.ndf <- (  s.rums$Species == species.sheep & s.rums$NDF.level == ndf.lev.lo ) 
cond.sheep.hi.ndf <- (  s.rums$Species == species.sheep & s.rums$NDF.level == ndf.lev.hi ) 
cond.goats.lo.ndf <- (  s.rums$Species == species.goat & s.rums$NDF.level == ndf.lev.lo ) 
cond.goats.hi.ndf <- (  s.rums$Species == species.goat & s.rums$NDF.level == ndf.lev.hi ) 


cond.grad.1.sheep.lo.ndf <- ( cond.sheep.lo.ndf & !is.na(s.rums$bw_kg) & !is.na(s.rums$adg_g_day)  & !is.na(s.rums$NDF_nutrition)   )
cond.grad.1.sheep.hi.ndf <- ( cond.sheep.hi.ndf & !is.na(s.rums$bw_kg) & !is.na(s.rums$adg_g_day)  & !is.na(s.rums$NDF_nutrition)   )
cond.grad.1.goats.lo.ndf <- ( cond.goats.lo.ndf & !is.na(s.rums$bw_kg) & !is.na(s.rums$adg_g_day)  & !is.na(s.rums$NDF_nutrition)   )
cond.grad.1.goats.hi.ndf <- ( cond.goats.hi.ndf & !is.na(s.rums$bw_kg) & !is.na(s.rums$adg_g_day)  & !is.na(s.rums$NDF_nutrition)   )



# cond ASH
cond.ash <- (!is.na(s.rums$Ash_nutrition) )
cond.ee <- (!is.na(s.rums$EE_nutrition) )


frac.cond.1.sheep.lo.ndf <- unique(length( s.rums[ cond.grad.1.sheep.lo.ndf ,  'B.Code'  ])) / unique(length( s.rums[ cond.sheep.lo.ndf     ,  'B.Code'  ])) 
frac.cond.1.sheep.hi.ndf <- unique(length( s.rums[ cond.grad.1.sheep.hi.ndf ,  'B.Code'  ])) / unique(length( s.rums[ cond.sheep.hi.ndf     ,  'B.Code'  ])) 
frac.cond.1.goats.lo.ndf <- unique(length( s.rums[ cond.grad.1.goats.lo.ndf ,  'B.Code'  ])) / unique(length( s.rums[ cond.goats.lo.ndf     ,  'B.Code'  ])) 
frac.cond.1.goats.hi.ndf <- unique(length( s.rums[ cond.grad.1.goats.hi.ndf ,  'B.Code'  ])) / unique(length( s.rums[ cond.goats.hi.ndf     ,  'B.Code'  ])) 


# Frac lost when going to ee extract equation 
frac.cond.1.sheep.lo.ndf - unique(length( s.rums[ cond.grad.1.sheep.lo.ndf  & cond.ee ,  'B.Code'  ])) / unique(length( s.rums[ cond.sheep.lo.ndf     ,  'B.Code'  ])) 
frac.cond.1.sheep.hi.ndf - unique(length( s.rums[ cond.grad.1.sheep.hi.ndf  & cond.ee  ,  'B.Code'  ])) / unique(length( s.rums[ cond.sheep.hi.ndf     ,  'B.Code'  ])) 
frac.cond.1.goats.lo.ndf - unique(length( s.rums[ cond.grad.1.goats.lo.ndf  & cond.ee   ,  'B.Code'  ])) / unique(length( s.rums[ cond.goats.lo.ndf     ,  'B.Code'  ])) 
frac.cond.1.goats.hi.ndf - unique(length( s.rums[ cond.grad.1.goats.hi.ndf  & cond.ee   ,  'B.Code'  ])) / unique(length( s.rums[ cond.goats.hi.ndf     ,  'B.Code'  ])) 

# Frac lost when going to ash 
frac.cond.1.sheep.lo.ndf - unique(length( s.rums[ cond.grad.1.sheep.lo.ndf  & cond.ash ,  'B.Code'  ])) / unique(length( s.rums[ cond.sheep.lo.ndf     ,  'B.Code'  ])) 
frac.cond.1.sheep.hi.ndf - unique(length( s.rums[ cond.grad.1.sheep.hi.ndf  & cond.ash  ,  'B.Code'  ])) / unique(length( s.rums[ cond.sheep.hi.ndf     ,  'B.Code'  ])) 
frac.cond.1.goats.lo.ndf - unique(length( s.rums[ cond.grad.1.goats.lo.ndf  & cond.ash   ,  'B.Code'  ])) / unique(length( s.rums[ cond.goats.lo.ndf     ,  'B.Code'  ])) 
frac.cond.1.goats.hi.ndf - unique(length( s.rums[ cond.grad.1.goats.hi.ndf  & cond.ash  ,  'B.Code'  ])) / unique(length( s.rums[ cond.goats.hi.ndf     ,  'B.Code'  ])) 

# Frac lost when going to both ee and ash  extract equation 
frac.cond.1.sheep.lo.ndf - unique(length( s.rums[ cond.grad.1.sheep.lo.ndf  & cond.ee & cond.ash,  'B.Code'  ])) / unique(length( s.rums[ cond.sheep.lo.ndf     ,  'B.Code'  ])) 
frac.cond.1.sheep.hi.ndf - unique(length( s.rums[ cond.grad.1.sheep.hi.ndf  & cond.ee  & cond.ash,  'B.Code'  ])) / unique(length( s.rums[ cond.sheep.hi.ndf     ,  'B.Code'  ])) 
frac.cond.1.goats.lo.ndf - unique(length( s.rums[ cond.grad.1.goats.lo.ndf  & cond.ee  & cond.ash ,  'B.Code'  ])) / unique(length( s.rums[ cond.goats.lo.ndf     ,  'B.Code'  ])) 
frac.cond.1.goats.hi.ndf - unique(length( s.rums[ cond.grad.1.goats.hi.ndf  & cond.ee  & cond.ash ,  'B.Code'  ])) / unique(length( s.rums[ cond.goats.hi.ndf     ,  'B.Code'  ])) 



}


normzd.DVs <- unique( optim.tform.dat[ optim.tform.dat$variable == y.var.reg  , 'var.name'])
normzd.DGs <- unique( optim.tform.dat[ optim.tform.dat$variable == v.list.DG[2]  , 'var.name'])
normzd.NDFs <- unique( optim.tform.dat[ optim.tform.dat$variable == v.list.NDF[2]  , 'var.name'])
normzd.BWs <- unique( optim.tform.dat[ optim.tform.dat$variable == v.list.BW[2]  , 'var.name'])
normzd.ASHs <- unique( optim.tform.dat[ optim.tform.dat$variable == v.list.ASH[2]  , 'var.name'])
normzd.EEs <- unique( optim.tform.dat[ optim.tform.dat$variable == v.list.EE[2]  , 'var.name'])


v.list.DV <- normzd.DVs[2]

v.list.BW <- normzd.BWs
v.list.ADG <- normzd.DGs
v.list.NDF <- normzd.NDFs

v.list.ASH <- normzd.ASHs
v.list.EE <- normzd.EEs



# Regression parameters
{
  weight.scalar <- 1  
  

  
  selc.aic <- 'aic'
  selc.bic <- 'bic'
  selc.rmse <- 'rmse'
  selc.r2.ffx <- 'r2.ffx'
  
  
 selc.crit.1 <<- selc.aic
# selc.crit.1 <-  selc.bic
# selc.crit.1 <- selc.r2.ffx
# selc.crit.1 <- selc.r2.ffx

# selc.crit.2 <- selc.r2.ffx
# selc.crit.2 <-  selc.bic
# selc.crit.2 <- selc.r2.ffx
selc.crit.2 <<- selc.r2.ffx
selc.crit <<- selc.crit.2
  
  # Formula version (gradient) for current model run
  form.grad.0 <<- 0 # BW only 
  form.grad.1 <<- 1 # BW + ADG + NDF
  form.grad.2 <<- 2 # EE 
  form.grad.3 <<- 3 # ASH
  form.grad.4 <<- 4 # ASH + EE ? 
  
  form.grad <- form.grad.1
  #form.grad <- form.grad.2
  #form.grad <- form.grad.3
  #form.grad <- form.grad.4
  

  
  
}




# Stepwise selection 
{

  
# Loop 
{
  
start.time <- Sys.time()
  
  
# Formula settings
tilde <- '~'
#y.var.tilde <-  paste(y.var.reg , tilde , separator = ' ' )   
#plus.sign <- ' + '

rd.intercept <- '+ (1'


rd.unit <- '| B.Code)'


# Data frame to store output variables
stepw.out <- data.frame(
  
   formula = list()
  , observations = list()
  , model = list()
  , inc.cond = list()
)

stepw.out.gr.sheep.lo.NDF <- stepw.out
stepw.out.gr.sheep.hi.NDF <- stepw.out
stepw.out.gr.goats.lo.NDF <- stepw.out
stepw.out.gr.goats.hi.NDF <- stepw.out

stepw.out.adt.sheep.lo.NDF <- stepw.out
stepw.out.adt.sheep.hi.NDF <- stepw.out
stepw.out.adt.goats.lo.NDF <- stepw.out
stepw.out.adt.goats.hi.NDF <- stepw.out

# Nested for loops
r <- 1
v1 <- 0 ; v2 <- NA ;  v3 <-  NA ; v4 <-  NA ; v5 <-  NA


for ( dv in v.list.DV ) {
  

for ( v1 in v.list.BW ) {
  

for ( v2 in v.list.ADG ){   if (form.grad < form.grad.1) {  if (v2 != "dummy") { v2 <- NA ; next}} #{ v2 <- NA ; if (v2 != "dummy") { next} }
  
  
for ( v3 in v.list.NDF ){   if (form.grad < form.grad.1 ) {  if (v3 != "dummy") { v3 <- NA ; next}}

 
#for ( v4 in v.list.EE ){   if (form.grad != form.grad.2 ) { print('...') ; if (v4 != "dummy" ) { print('....') ; v4_4 <- NA ; next   }}  
  
  
#for ( v5 in v.list.ASH ){  if (form.grad != form.grad.3 ) {  if (v5 != "dummy" & !is.na(v5) ) { v5 <- NA ; next}} #{ v5 <- NA ; if (v5 != "dummy") { next}}
  

#for ( v4 in v.list.NDF.dig ) {
  

form.vars <- c(
  
  v1 
  , v2 
  , v3  
  , v4 
  , v5 
  
  , 'is_gestating'
  , 'milk_prod_kg_d'
  
  )


form.vars <- form.vars[   which( !(is.na(form.vars) == form.vars))  ] # Omit variables with NA
form.vars <- form.vars[  !form.vars == ( "dummy")] # Omit dummy variables

form.vars.cnd <- form.vars[-c( length(form.vars) ,length(form.vars) -1)]

rhs <- paste(form.vars[!is.na(form.vars)], collapse = " + ")

lhs <- paste( dv , tilde , collapse = ' ')

formula <- as.formula(
  paste(
   lhs
    
    # fixed effects variables
    # , v1
    
    # , plus.sign 
    
    # , v2
    
    #  , plus.sign 
    
    # , v3
    
    ,  rhs
    
    # random
    
    , rd.intercept
    
    , rd.unit
  )
)

formula.fe <- as.formula(
  paste(
    lhs
    
    # fixed effects variables
    # , v1
    
    # , plus.sign 
    
    # , v2
    
    #  , plus.sign 
    
    # , v3
    
    ,  rhs
    
  )
)

s.rums.cc <- s.rums[complete.cases(s.rums[ !is.na(s.rums$Sample.size) ,form.vars]),]

dat.cond.gr.sheep.lo.NDF <- ( s.rums.cc$Species == species.sheep
                           & s.rums.cc$ndf.level == ndf.lev.lo
                          # & s.rums.cc$is.growing == FALSE
                        #   & s.rums.cc$is.growing == TRUE
                        #   & s.rums.cc$species.stage == species.stage.sheep.growing
                        #  & (s.rums.cc$stage.non.reprod )
                          )

dat.cond.gr.sheep.hi.NDF <- ( s.rums.cc$Species == species.sheep 
                           & s.rums.cc$ndf.level == ndf.lev.hi  
                         #  & !is.na(s.rums.cc$is.growing)
                         #  & s.rums.cc$is.growing ==  TRUE
                        #  & s.rums.cc$is.growing == FALSE
                         #  & s.rums.cc$species.stage == species.stage.sheep.growing
                       #  & (s.rums.cc$stage.non.reprod )
                         )

dat.cond.gr.goats.lo.NDF <- ( s.rums.cc$Species == species.goat
                           & s.rums.cc$ndf.level == ndf.lev.lo 
                        #   & !is.na(s.rums.cc$is.growing)
                         #  & s.rums.cc$is.growing == FALSE
                       #    & s.rums.cc$is.growing ==  TRUE
                          # & s.rums.cc$species.stage == species.stage.goat.growing
                         # & (s.rums.cc$stage.non.reprod )
                          )

dat.cond.gr.goats.hi.NDF <- ( s.rums.cc$Species == species.goat
                          & s.rums.cc$ndf.level == ndf.lev.hi
                         # & !is.na(s.rums.cc$is.growing)
                        #  & s.rums.cc$is.growing == FALSE
                        #  & s.rums.cc$is.growing ==  TRUE
                          #& s.rums.cc$is.growing == TRUE
                        #   & s.rums.cc$species.stage == species.stage.goat.growing
                      #  & (s.rums.cc$stage.non.reprod )
                         )


dat.cond.adt.sheep.lo.NDF <- ( s.rums.cc$Species == species.sheep
                              & s.rums.cc$ndf.level == ndf.lev.lo  
                              & !is.na(s.rums.cc$is.growing)
                              & s.rums.cc$is.adult == TRUE
                              #   & s.rums.cc$species.stage == species.stage.sheep.growing
                              #  & (s.rums.cc$stage.non.reprod )
)

dat.cond.adt.sheep.hi.NDF <- ( s.rums.cc$Species == species.sheep 
                              & s.rums.cc$ndf.level == ndf.lev.hi  
                              & !is.na(s.rums.cc$is.growing)
                              &  s.rums.cc$is.adult == TRUE
                              #  & s.rums.cc$species.stage == species.stage.sheep.growing
                              #  & (s.rums.cc$stage.non.reprod )
)

dat.cond.adt.goats.lo.NDF <- ( s.rums.cc$Species == species.goat
                              & s.rums.cc$ndf.level == ndf.lev.lo 
                              & !is.na(s.rums.cc$is.growing)
                              & s.rums.cc$is.adult == TRUE
                              # & s.rums.cc$species.stage == species.stage.goat.growing
                              # & (s.rums.cc$stage.non.reprod )
)

dat.cond.adt.goats.hi.NDF <- ( s.rums.cc$Species == species.goat
                              & s.rums.cc$ndf.level == ndf.lev.hi
                              & !is.na(s.rums.cc$is.growing)
                              & s.rums.cc$is.adult == TRUE
                              #& s.rums.cc$is.growing == TRUE
                              #   & s.rums.cc$species.stage == species.stage.goat.growing
                              #  & (s.rums.cc$stage.non.reprod )
)

obsvs.gr.sheep.lo.NDF <- na.omit(s.rums.cc[dat.cond.gr.sheep.lo.NDF ,'id'])
obsvs.gr.sheep.hi.NDF <- na.omit(s.rums.cc[dat.cond.gr.sheep.hi.NDF ,'id'])
obsvs.gr.goats.lo.NDF <- na.omit(s.rums.cc[dat.cond.gr.goats.lo.NDF ,'id'])
obsvs.gr.goats.hi.NDF <- na.omit(s.rums.cc[dat.cond.gr.goats.hi.NDF ,'id'])

obsvs.adt.sheep.lo.NDF <- na.omit(s.rums.cc[dat.cond.adt.sheep.lo.NDF ,'id'])
obsvs.adt.sheep.hi.NDF <- na.omit(s.rums.cc[dat.cond.adt.sheep.hi.NDF ,'id'])
obsvs.adt.goats.lo.NDF <- na.omit(s.rums.cc[dat.cond.adt.goats.lo.NDF ,'id'])
obsvs.adt.goats.hi.NDF <- na.omit(s.rums.cc[dat.cond.adt.goats.hi.NDF ,'id'])




# RUN ME MODELS
{
  
try.gr.sheep.lo.ndf  <- try(

model.gr.sheep.lo.ndf <<- lmer( 
  formula
, weights = sample.coef.weight *  s.rums.cc[dat.cond.gr.sheep.lo.NDF , sample.weight.var ] / (mean(na.omit(s.rums.cc[dat.cond.gr.sheep.lo.NDF , 'T.Animals'])) )
  , data = s.rums.cc[dat.cond.gr.sheep.lo.NDF , ]
)

, silent = TRUE

)
  


  try.gr.sheep.hi.ndf  <- try(
  
  
model.gr.sheep.hi.ndf <<- lmer( 
  formula
  , weights = sample.coef.weight * s.rums.cc[dat.cond.gr.sheep.hi.NDF , sample.weight.var ] / (mean(na.omit(s.rums.cc[dat.cond.gr.sheep.hi.NDF , 'T.Animals'])) )
  , data = s.rums.cc[dat.cond.gr.sheep.hi.NDF , ]
)

, silent = TRUE

)


  try.gr.goats.lo.ndf  <- try(
  

model.gr.goats.lo.ndf <<- lmer( 
  formula
  , weights = sample.coef.weight * s.rums.cc[dat.cond.gr.goats.lo.NDF  , sample.weight.var] / (mean(na.omit(s.rums.cc[dat.cond.gr.goats.lo.NDF  , 'T.Animals'])) )
  , data = s.rums.cc[dat.cond.gr.goats.lo.NDF , ]
)

, silent = TRUE

)


  try.gr.goats.hi.ndf  <- try(
  

model.gr.goats.hi.ndf <<- lmer( 
  formula
  , weights = sample.coef.weight * s.rums.cc[dat.cond.gr.goats.hi.NDF , sample.weight.var] / (mean(na.omit(s.rums.cc[dat.cond.gr.goats.hi.NDF , 'T.Animals'])) )
  , data = s.rums.cc[ dat.cond.gr.goats.hi.NDF , ]
)

, silent = TRUE

)
  
  
 
  
} # Growing

{
  
try.adt.sheep.lo.ndf  <- try(
    
    model.adt.sheep.lo.ndf <<- lmer( 
      formula
      , weights = sample.coef.weight * s.rums.cc[dat.cond.adt.sheep.lo.NDF , sample.weight.var] / (mean(na.omit(s.rums.cc[dat.cond.adt.sheep.lo.NDF , 'T.Animals'])) )
      , data = s.rums.cc[dat.cond.adt.sheep.lo.NDF , ]
    )
    
    , silent = TRUE
    
  )
  
  
  try.adt.sheep.hi.ndf  <-try(
    
    
    model.adt.sheep.hi.ndf <<- lmer( 
      formula
      , weights = sample.coef.weight * s.rums.cc[dat.cond.adt.sheep.hi.NDF , sample.weight.var ] / (mean(na.omit(s.rums.cc[dat.cond.adt.sheep.hi.NDF , 'T.Animals'])) )
      , data = s.rums.cc[dat.cond.adt.sheep.hi.NDF , ]
    )
    
    , silent = TRUE
    
  )
  
  
  try.adt.goats.lo.ndf  <- try(
    
    
    model.adt.goats.lo.ndf <<- lmer( 
      formula
      , weights = sample.coef.weight * s.rums.cc[dat.cond.adt.goats.lo.NDF  , sample.weight.var ] / (mean(na.omit(s.rums.cc[dat.cond.adt.goats.lo.NDF  , 'T.Animals'])) )
      , data = s.rums.cc[dat.cond.adt.goats.lo.NDF , ]
    )
    
    , silent = TRUE
    
  )
  
  
  try.adt.goats.hi.ndf  <-try(
    
    
    model.adt.goats.hi.ndf <<- lmer( 
      formula
      , weights = sample.coef.weight * s.rums.cc[dat.cond.adt.goats.hi.NDF , sample.weight.var ] / (mean(na.omit(s.rums.cc[dat.cond.adt.goats.hi.NDF , 'T.Animals'])) )
      , data = s.rums.cc[ dat.cond.adt.goats.hi.NDF , ]
    )
    
    , silent = TRUE
    
  )
  
} # Adult


# RUN GBM MODELS


  
gbr.adt.sheep.lo.ndf <<-  glmboost( 
  formula.fe
  , data = s.rums.cc[dat.cond.gr.sheep.lo.NDF , ] 
  , family = Gaussian() 
  ,  control = boost_control(mstop = 2000)
  , center = FALSE)
  




# SUMMARIZE DATA
{

{
  
if (  !inherits( try.gr.sheep.lo.ndf , "try-error")  )  {
  
# Summarize sheep and lo NDF data
stepw.out.gr.sheep.lo.NDF[ r , 'observations'] <- list(list(obsvs.gr.sheep.lo.NDF))
stepw.out.gr.sheep.lo.NDF[ r , 'formula'] <- list(list(formula))
stepw.out.gr.sheep.lo.NDF[ r , 'y.var'] <- dv
stepw.out.gr.sheep.lo.NDF[ r , 'model'] <- list(list(model.gr.sheep.lo.ndf))
stepw.out.gr.sheep.lo.NDF[ r , 'inc.cond'] <- list(list(dat.cond.gr.sheep.lo.NDF))


norm.vars <- optim.tform.dat[optim.tform.dat$species == species.sheep & optim.tform.dat$ndf.level == ndf.lev.lo, 'var.name']

check.all <- (  form.vars.cnd %in% norm.vars )
check.all <-    all( check.all )          

try( stepw.out.gr.sheep.lo.NDF[ r , 'r2.mrg'] <- r.squaredGLMM(model.gr.sheep.lo.ndf)[1] )
try( stepw.out.gr.sheep.lo.NDF[ r , 'r2.cnd'] <- r.squaredGLMM(model.gr.sheep.lo.ndf)[2] )
try( stepw.out.gr.sheep.lo.NDF[ r , 'aic'] <- AIC(model.gr.sheep.lo.ndf) )
try( stepw.out.gr.sheep.lo.NDF[ r , 'bic'] <- BIC(model.gr.sheep.lo.ndf)   )
try( stepw.out.gr.sheep.lo.NDF[ r  , 'rmse'] <- rmse(model.gr.sheep.lo.ndf)  )
try( stepw.out.gr.sheep.lo.NDF[ r  , 'normalized.vars'] <- check.all , silent = TRUE)

}
  
  
# Summarize sheep and hi NDF data
if (  !inherits( try.gr.sheep.hi.ndf , "try-error")  )  {
stepw.out.gr.sheep.hi.NDF[ r , 'observations'] <- list(list(obsvs.gr.sheep.hi.NDF))
stepw.out.gr.sheep.hi.NDF[ r , 'formula'] <- list(list(formula))
stepw.out.gr.sheep.hi.NDF[ r , 'y.var'] <- dv
stepw.out.gr.sheep.hi.NDF[ r , 'model'] <- list(list(model.gr.sheep.hi.ndf))
stepw.out.gr.sheep.hi.NDF[ r , 'inc.cond'] <- list(list(dat.cond.gr.sheep.hi.NDF))


norm.vars <- optim.tform.dat[optim.tform.dat$species == species.sheep & optim.tform.dat$ndf.level == ndf.lev.hi, 'var.name']


check.all <- (  form.vars.cnd  %in% norm.vars )
check.all <-    all( check.all )  

try( stepw.out.gr.sheep.hi.NDF[ r , 'r2.mrg'] <- r.squaredGLMM(model.gr.sheep.hi.ndf)[1] )
try( stepw.out.gr.sheep.hi.NDF[ r , 'r2.cnd'] <- r.squaredGLMM(model.gr.sheep.hi.ndf)[2] )
try( stepw.out.gr.sheep.hi.NDF[ r, 'aic'] <- AIC(model.gr.sheep.hi.ndf) )
try( stepw.out.gr.sheep.hi.NDF[ r  , 'bic'] <- BIC(model.gr.sheep.hi.ndf)   )
try( stepw.out.gr.sheep.hi.NDF[ r , 'rmse'] <- rmse(model.gr.sheep.hi.ndf)  )
try( stepw.out.gr.sheep.hi.NDF[ r  , 'normalized.vars'] <- check.all , silent = TRUE)


}
  
# Summarize goats and lo NDF data
if (  !inherits( try.gr.goats.lo.ndf , "try-error")  )  { 
  
  
stepw.out.gr.goats.lo.NDF[ r , 'observations'] <- list(list(obsvs.gr.goats.lo.NDF))
stepw.out.gr.goats.lo.NDF[ r , 'formula'] <- list(list(formula))
stepw.out.gr.goats.lo.NDF[ r , 'y.var'] <- dv
stepw.out.gr.goats.lo.NDF[ r , 'model'] <- list(list(model.gr.goats.lo.ndf))
stepw.out.gr.goats.lo.NDF[ r , 'inc.cond'] <- list(list(dat.cond.gr.goats.lo.NDF))


norm.vars <- optim.tform.dat[optim.tform.dat$species == species.goat & optim.tform.dat$ndf.level == ndf.lev.lo , 'var.name']


check.all <- (  form.vars.cnd  %in% norm.vars )
check.all <-    all( check.all )  

try( stepw.out.gr.goats.lo.NDF[ r , 'r2.mrg'] <- r.squaredGLMM(model.gr.goats.lo.ndf)[1] )
try( stepw.out.gr.goats.lo.NDF[ r , 'r2.cnd'] <- r.squaredGLMM(model.gr.goats.lo.ndf)[2] )
try( stepw.out.gr.goats.lo.NDF[ r , 'aic'] <- AIC(model.gr.goats.lo.ndf) )
try( stepw.out.gr.goats.lo.NDF[ r , 'bic'] <- BIC(model.gr.goats.lo.ndf)   )
try( stepw.out.gr.goats.lo.NDF[ r  , 'rmse'] <- rmse(model.gr.goats.lo.ndf)  )
try( stepw.out.gr.goats.lo.NDF[ r  , 'normalized.vars'] <- check.all , silent = TRUE)


}
  
  
# Summarize goats and hi NDF data
if (  !inherits( try.gr.goats.hi.ndf , "try-error")  )  {
stepw.out.gr.goats.hi.NDF[ r , 'observations'] <- list(list(obsvs.gr.goats.hi.NDF))
stepw.out.gr.goats.hi.NDF[ r , 'formula'] <- list(list(formula))
stepw.out.gr.goats.hi.NDF[ r , 'y.var'] <- dv
stepw.out.gr.goats.hi.NDF[ r , 'model'] <- list(list(model.gr.goats.hi.ndf))
stepw.out.gr.goats.hi.NDF[ r , 'inc.cond'] <- list(list(dat.cond.gr.goats.hi.NDF))


norm.vars <- optim.tform.dat[optim.tform.dat$species == species.goat & optim.tform.dat$ndf.level == ndf.lev.hi , 'var.name']


check.all <- (  form.vars.cnd  %in% norm.vars )
check.all <-    all( check.all )  

try( stepw.out.gr.goats.hi.NDF[ r , 'r2.mrg'] <- r.squaredGLMM(model.gr.goats.hi.ndf)[1] )
try( stepw.out.gr.goats.hi.NDF[ r , 'r2.cnd'] <- r.squaredGLMM(model.gr.goats.hi.ndf)[2] )
try( stepw.out.gr.goats.hi.NDF[ r , 'aic'] <- AIC(model.gr.goats.hi.ndf) )
try( stepw.out.gr.goats.hi.NDF[ r , 'bic'] <- BIC(model.gr.goats.hi.ndf)   )
try( stepw.out.gr.goats.hi.NDF[ r  , 'rmse'] <- rmse(model.gr.goats.hi.ndf)  )
try( stepw.out.gr.goats.hi.NDF[ r  , 'normalized.vars'] <- check.all , silent = TRUE)

}
  
} # Growing
  
{
if (  !inherits( try.adt.sheep.lo.ndf , "try-error")  )  {
  
    # Summarize sheep and lo NDF data
    stepw.out.adt.sheep.lo.NDF[ r , 'observations'] <- list(list(obsvs.adt.sheep.lo.NDF))
    stepw.out.adt.sheep.lo.NDF[ r , 'formula'] <- list(list(formula))
    stepw.out.adt.sheep.lo.NDF[ r , 'y.var'] <- dv
    stepw.out.adt.sheep.lo.NDF[ r , 'model'] <- list(list(model.adt.sheep.lo.ndf))
    stepw.out.adt.sheep.lo.NDF[ r , 'inc.cond'] <- list(list(dat.cond.adt.sheep.lo.NDF))
    
    
    norm.vars <- optim.tform.dat[optim.tform.dat$species == species.sheep & optim.tform.dat$ndf.level == ndf.lev.lo, 'var.name']
    
    check.all <- (  form.vars.cnd  %in% norm.vars )
    check.all <-    all( check.all )          
    
    try( stepw.out.adt.sheep.lo.NDF[ r , 'r2.mrg'] <- r.squaredGLMM(model.adt.sheep.lo.ndf)[1] )
    try( stepw.out.adt.sheep.lo.NDF[ r , 'r2.cnd'] <- r.squaredGLMM(model.adt.sheep.lo.ndf)[2] )
    try( stepw.out.adt.sheep.lo.NDF[ r , 'aic'] <- AIC(model.adt.sheep.lo.ndf) )
    try( stepw.out.adt.sheep.lo.NDF[ r , 'bic'] <- BIC(model.adt.sheep.lo.ndf)   )
    try( stepw.out.adt.sheep.lo.NDF[ r  , 'rmse'] <- rmse(model.adt.sheep.lo.ndf)  )
    try( stepw.out.adt.sheep.lo.NDF[ r  , 'normalized.vars'] <- check.all , silent = TRUE)
    
}
if (  !inherits( try.adt.sheep.hi.ndf , "try-error")  )  {

    # Summarize sheep and hi NDF data
    stepw.out.adt.sheep.hi.NDF[ r , 'observations'] <- list(list(obsvs.adt.sheep.hi.NDF))
    stepw.out.adt.sheep.hi.NDF[ r , 'formula'] <- list(list(formula))
    stepw.out.adt.sheep.hi.NDF[ r , 'y.var'] <- dv
    stepw.out.adt.sheep.hi.NDF[ r , 'model'] <- list(list(model.adt.sheep.hi.ndf))
    stepw.out.adt.sheep.hi.NDF[ r , 'inc.cond'] <- list(list(dat.cond.adt.sheep.hi.NDF))
    
    
    norm.vars <- optim.tform.dat[optim.tform.dat$species == species.sheep & optim.tform.dat$ndf.level == ndf.lev.hi, 'var.name']
    
    
    check.all <- (  form.vars.cnd %in% norm.vars )
    check.all <-    all( check.all )  
    
    try( stepw.out.adt.sheep.hi.NDF[ r , 'r2.mrg'] <- r.squaredGLMM(model.adt.sheep.hi.ndf)[1] )
    try( stepw.out.adt.sheep.hi.NDF[ r , 'r2.cnd'] <- r.squaredGLMM(model.adt.sheep.hi.ndf)[2] )
    try( stepw.out.adt.sheep.hi.NDF[ r, 'aic'] <- AIC(model.adt.sheep.hi.ndf) )
    try( stepw.out.adt.sheep.hi.NDF[ r  , 'bic'] <- BIC(model.adt.sheep.hi.ndf)   )
    try( stepw.out.adt.sheep.hi.NDF[ r , 'rmse'] <- rmse(model.adt.sheep.hi.ndf)  )
    try( stepw.out.adt.sheep.hi.NDF[ r  , 'normalized.vars'] <- check.all , silent = TRUE)
}
    # Summarize goats and lo NDF data
  if (  !inherits( try.adt.goats.lo.ndf , "try-error")  )  {
    stepw.out.adt.goats.lo.NDF[ r , 'observations'] <- list(list(obsvs.adt.goats.lo.NDF))
    stepw.out.adt.goats.lo.NDF[ r , 'formula'] <- list(list(formula))
    stepw.out.adt.goats.lo.NDF[ r , 'y.var'] <- dv
    stepw.out.adt.goats.lo.NDF[ r , 'model'] <- list(list(model.adt.goats.lo.ndf))
    stepw.out.adt.goats.lo.NDF[ r , 'inc.cond'] <- list(list(dat.cond.adt.goats.lo.NDF))
    
    
    norm.vars <- optim.tform.dat[optim.tform.dat$species == species.goat & optim.tform.dat$ndf.level == ndf.lev.lo , 'var.name']
    
    
    check.all <- (  form.vars.cnd  %in% norm.vars )
    check.all <-    all( check.all )  
    
    try( stepw.out.adt.goats.lo.NDF[ r , 'r2.mrg'] <- r.squaredGLMM(model.adt.goats.lo.ndf)[1] )
    try( stepw.out.adt.goats.lo.NDF[ r , 'r2.cnd'] <- r.squaredGLMM(model.adt.goats.lo.ndf)[2] )
    try( stepw.out.adt.goats.lo.NDF[ r , 'aic'] <- AIC(model.adt.goats.lo.ndf) )
    try( stepw.out.adt.goats.lo.NDF[ r , 'bic'] <- BIC(model.adt.goats.lo.ndf)   )
    try( stepw.out.adt.goats.lo.NDF[ r  , 'rmse'] <- rmse(model.adt.goats.lo.ndf)  )
    try( stepw.out.adt.goats.lo.NDF[ r  , 'normalized.vars'] <- check.all , silent = TRUE)
  }
  
    # Summarize goats and hi NDF data
  if (  !inherits( try.adt.goats.hi.ndf , "try-error")  )  {
    stepw.out.adt.goats.hi.NDF[ r , 'observations'] <- list(list(obsvs.adt.goats.hi.NDF))
    stepw.out.adt.goats.hi.NDF[ r , 'formula'] <- list(list(formula))
    stepw.out.adt.goats.hi.NDF[ r , 'y.var'] <- dv
    stepw.out.adt.goats.hi.NDF[ r , 'model'] <- list(list(model.adt.goats.hi.ndf))
    stepw.out.adt.goats.hi.NDF[ r , 'inc.cond'] <- list(list(dat.cond.adt.goats.hi.NDF))
    
    
    norm.vars <- optim.tform.dat[optim.tform.dat$species == species.goat & optim.tform.dat$ndf.level == ndf.lev.hi , 'var.name']
    
    
    check.all <- (  form.vars.cnd %in% norm.vars )
    check.all <-    all( check.all )  
    
    try( stepw.out.adt.goats.hi.NDF[ r , 'r2.mrg'] <- r.squaredGLMM(model.adt.goats.hi.ndf)[1] )
    try( stepw.out.adt.goats.hi.NDF[ r , 'r2.cnd'] <- r.squaredGLMM(model.adt.goats.hi.ndf)[2] )
    try( stepw.out.adt.goats.hi.NDF[ r , 'aic'] <- AIC(model.adt.goats.hi.ndf) )
    try( stepw.out.adt.goats.hi.NDF[ r , 'bic'] <- BIC(model.adt.goats.hi.ndf)   )
    try( stepw.out.adt.goats.hi.NDF[ r  , 'rmse'] <- rmse(model.adt.goats.hi.ndf)  )
    try( stepw.out.adt.goats.hi.NDF[ r  , 'normalized.vars'] <- check.all , silent = TRUE)
  }
    
     } # Adult
  
}


print(  paste('Finishing iteration'  , r ))

r <- r + 1

}
}
}
}
#}
#}
  
}

end.time <- Sys.time()
elapsed.seconds <- (end.time - start.time)

print(paste('Successfully completed stepwise selection in ' , elapsed.seconds/60 , ' minutes.'))


# Define models based on selection criteria
try ( model.sum(  stepw.out.gr.sheep.lo.NDF , species.sheep , ndf.lev.lo , stage.growing.all ) , silent = TRUE ) 
try ( model.sum(  stepw.out.gr.sheep.hi.NDF , species.sheep , ndf.lev.hi , stage.growing.all ) , silent = TRUE )
try ( model.sum(  stepw.out.gr.goats.lo.NDF , species.goat , ndf.lev.lo , stage.growing.all ) , silent = TRUE )
try ( model.sum(  stepw.out.gr.goats.hi.NDF , species.goat , ndf.lev.hi , stage.growing.all ) , silent = TRUE )

try ( model.sum(stepw.out.adt.sheep.lo.NDF , species.sheep , ndf.lev.lo , stage.adult.all  ) , silent = TRUE )
try ( model.sum(stepw.out.adt.sheep.hi.NDF , species.sheep , ndf.lev.hi , stage.adult.all) , silent = TRUE )
try ( model.sum(stepw.out.adt.goats.lo.NDF , species.goat , ndf.lev.lo , stage.adult.all) , silent = TRUE )
try ( model.sum(stepw.out.adt.goats.hi.NDF , species.goat , ndf.lev.hi , stage.adult.all )  , silent = TRUE )


}


# Export summary table
{
  
  

# paste( 'Max ' , "R\u00B2" , sep = "")


#  https://www.rdocumentation.org/packages/sjPlot/versions/2.8.17/topics/tab_model

# Define (predictor labels, dependent variable labels) list
{
  
  dv.labels.list <- c( 
    
    'Normzd' 
    , 'Max R2'
    , 'Best AIC' 
    
  )  
  
  
abbv.bw <- 'BW'
abbv.adg <- 'ADG'
abbv.ndf <- 'NDF'
abbv.ash <- 'Ash'
abbv.ee <- 'Ether extract'

abbv.gest <- 'Is gestating'
abbv.milk.yd <- 'Milk yield (kg/d)'

abbv.bw.unt <- '(kg)'
abbv.adg.unt <- '(g/d)'
abbv.ndf.unt <- '(g/kg)'
abbv.ash.unt <- '(g/kg)'
abbv.ee.unt <- '(g/kg)'


pred.labels.list.intercept = c("Intercept" = "Intercept")
    
# BW
{
  
pred.labels.list.bw <- c(
# v.list.BW[2]
 "bw_kg" =  paste(abbv.bw )

# v.list.BW[3]
, "bw_kg.e25" = paste( "\u221c",abbv.bw , " ", abbv.bw.unt )

# v.list.BW[4]
, "bw_kg.sqt" = paste( "\u221a",abbv.bw , " ", abbv.bw.unt )

# v.list.BW[5]
, "bw_kg.e75" = paste("\u00BE",  "\u221a",abbv.bw , " ", abbv.bw.unt )

# v.list.BW[6]
,  "bw_kg.sqd" = paste( abbv.bw , "\u00B2", " ", abbv.bw.unt )

# v.list.BW[7]
, "bw_kg.cbd" = paste( abbv.bw , "\u00B3", " ", abbv.bw.unt )

# v.list.BW[8]
, "bw_kg.log"= paste( "ln(" , abbv.bw , ")" , sep = "")

# v.list.BW[9]
, "bw_kg.recip"= paste( "1 / (" , abbv.bw , ")" , sep = "")
)


  
}

# ADG
{
  
pred.labels.list.adg <- c(

#v.list.DG[2]
"adg_g_day" =  paste(abbv.adg )

# v.list.DG[3]
, "adg_g_day.e25" = paste( "\u221c",abbv.adg , sep = "")

# v.list.DG[4]
, "adg_g_day.sqt" = paste( "\u221a",abbv.adg , " ", abbv.adg.unt) 

# v.list.DG[5]
, "adg_g_day.e75" = paste( abbv.adg,"\u00B2" , sep = "")

# v.list.DG[6]
,  "adg_g_day.sqd" = paste( abbv.adg, "\u00B2", " ", abbv.adg.unt , sep = '') 

# v.list.DG[7]
, "adg_g_day.cbd" = paste( abbv.adg , "\u00B3" , sep = "")

# v.list.BW[8]
, "adg_g_day.log"= paste( "ln(" , abbv.adg , ")" , sep = "")

# v.list.BW[9]
, "adg_g_day.recip"= paste( "1 / (" , abbv.adg , ")" , sep = "")
)
  
}

# NDF
{
  
  pred.labels.list.ndf <- c(
    
    #v.list.NDF[2]
    "NDF_nutrition" =  paste(abbv.ndf )
    
    # v.list.NDF[3]
    , "NDF_nutrition.e25" = paste( "\u221c",abbv.ndf , sep = "")
    
    # v.list.NDF[4]
    , "NDF_nutrition.sqt" = paste( abbv.ndf,"\u00B2" , sep = "")
    
    # v.list.NDF[5]
    , "NDF_nutrition.e75" = paste( abbv.ndf,"\u00B2" , sep = "")
    
    # v.list.NDF[6]
    #,  "NDF_nutrition.sqd" = paste( abbv.ndf , "\u00B2" , sep = "")
    
    ,  "NDF_nutrition.sqd"  = paste( abbv.ndf, "\u00B2", " ", abbv.ndf.unt , sep = '')
    
    
    # v.list.NDF[7]
    , "NDF_nutrition.cbd" = paste( abbv.ndf , "\u00B3" , sep = "")
    
    # v.list.NDF[8]
    , "NDF_nutrition.log"= paste( "ln(" , abbv.ndf , ")" , sep = "")
    
    # v.list.NDF[9]
    , "NDF_nutrition.recip"= paste( "1 / (" , abbv.ndf , ")" , sep = "")
    
    
  )
}

# ASH
{
  
  pred.labels.list.ash <- c(
    
    #v.list.NDF[2]
    "ASH_nutrition" =  paste(abbv.ash )
    
    # v.list.NDF[3]
    , "ASH_nutrition.e25" = paste( "\u221c",abbv.ash , sep = "")
    
    # v.list.NDF[4]
    , "ASH_nutrition.sqt" = paste( abbv.ash,"\u00B2" , sep = "")
    
    # v.list.NDF[5]
    , "ASH_nutrition.e75" = paste( abbv.ash,"\u00B2" , sep = "")
    
    # v.list.NDF[6]
    ,  "ASH_nutrition.sqd" = paste( abbv.ash , "\u00B2" , sep = "")
    
    # v.list.NDF[7]
    , "ASH_nutrition.cbd" = paste( abbv.ash , "\u00B3" , sep = "")
    
    # v.list.NDF[8]
    , "ASH_nutrition.log"= paste( "ln(" , abbv.ash , ")" , sep = "")
    
    # v.list.BW[9]
    , "ASH_nutrition.recip"= paste( "1 / (" , abbv.ash , ")" , sep = "")
  )
}

# EE
{
  
  pred.labels.list.ee <- c(
    
    #v.list.NDF[2]
    "EE_nutrition" =  paste(abbv.ee )
    
    # v.list.NDF[3]
    , "EE_nutrition.e25" = paste( "\u221c",abbv.ee , sep = "")
    
    # v.list.NDF[4]
    , "EE_nutrition.sqt" = paste( abbv.ee,"\u00B2" , sep = "")
    
    # v.list.NDF[5]
    , "EE_nutrition.e75" = paste( abbv.ee,"\u00B2" , sep = "")
    
    # v.list.NDF[6]
    ,  "EE_nutrition.sqd" = paste( abbv.ee , "\u00B2" , sep = "")
    
    # v.list.NDF[7]
    , "EE_nutrition.cbd" = paste( abbv.ee , "\u00B3" , sep = "")
    
    # v.list.NDF[8]
    , "EE_nutrition.log" = paste( "ln(" , abbv.ee , ")" , sep = "")
    
    # v.list.EE[9]
    , "EE_nutrition.recip" = paste( "1 / (" , abbv.ee , ")" , sep = "")
  )
}



pred.labels.list.my.gest <- c(
  
  "milk_prod_kg_d" =  paste(abbv.milk.yd)
  
  , "is_gestating" =  paste(abbv.gest)
  
)

pred.labels.list.my <- c(abbv.milk.yd)

pred.labels.list.gest <- c(abbv.gest)





pred.labels.list <- c(
  pred.labels.list.intercept
  , pred.labels.list.bw
  , pred.labels.list.adg
  , pred.labels.list.ndf
  , pred.labels.list.ash
  , pred.labels.list.ee
  
  , pred.labels.list.my.gest
  )


}

  
if (  form.grad == form.grad.1) { dmi.estim8.gr.sheep.html.file.name <- 'results/dmi.estim8.GROWING.SHEEP.grad.1.html' ; dmi.estim8.gr.goats.html.file.name <- 'results/dmi.estim8.GROWING.GOATS.grad.1.html' ; dmi.estim8.adt.sheep.html.file.name <- 'results/dmi.estim8.ADULT.SHEEP.grad.1.html' ; dmi.estim8.adt.goats.html.file.name <- 'results/dmi.estim8.ADULT.GOATS.grad.1.html'}
if (  form.grad == form.grad.2) { dmi.estim8.gr.sheep.html.file.name <- 'results/dmi.estim8.sheep.grad.2.html' ; dmi.estim8.gr.goats.html.file.name <- 'results/dmi.estim8.goats.grad.2.html'}
if (  form.grad == form.grad.3) { dmi.estim8.gr.sheep.html.file.name <- 'results/dmi.estim8.sheep.grad.3.html' ; dmi.estim8.gr.goats.html.file.name <- 'results/dmi.estim8.goats.grad.3.html'}
  


tab_model(   # GROWING SHEEP 
  
  
  best.model.cr.1.gr.sheep.lo.ndf.n.cond
, best.model.cr.1.gr.sheep.lo.ndf.glob
, best.model.cr.1.gr.sheep.lo.ndf.glob.2

, best.model.cr.1.gr.sheep.hi.ndf.n.cond
, best.model.cr.1.gr.sheep.hi.ndf.glob
, best.model.cr.1.gr.sheep.hi.ndf.glob.2 


, pred.labels = factor(pred.labels.list) 
,  dv.labels = c(dv.labels.list , dv.labels.list)   #dv.labels.list[c(1:4)]  

# , string.pred = "Coefficient"
, collapse.ci = TRUE
#, string.p = "P-Value"
, p.style="stars"
, show.icc = FALSE
, show.re.var = FALSE
, show.obs = TRUE
, show.r2 =  TRUE
, show.ngroups = FALSE
, file = dmi.estim8.gr.sheep.html.file.name

)


tab_model(  # GROWING GOATS
  
  best.model.cr.1.gr.goats.lo.ndf.n.cond
  , best.model.cr.1.gr.goats.lo.ndf.glob
  , best.model.cr.1.gr.goats.lo.ndf.glob.2
  
  , best.model.cr.1.gr.goats.hi.ndf.n.cond
  , best.model.cr.1.gr.goats.hi.ndf.glob
  , best.model.cr.1.gr.goats.hi.ndf.glob.2
  
  
  , pred.labels = pred.labels.list
  ,  dv.labels = c(dv.labels.list , dv.labels.list)
  
  # , string.pred = "Coefficient"
  , collapse.ci = TRUE
  #, string.p = "P-Value"
  , p.style="stars"
  , show.icc = FALSE
  , show.re.var = FALSE
  , show.obs = TRUE
  , show.r2 =  TRUE
  , show.ngroups = FALSE
  , file = dmi.estim8.gr.goats.html.file.name
  
)

tab_model(
  
  
  best.model.cr.1.adt.sheep.lo.ndf.n.cond
  , best.model.cr.1.adt.sheep.lo.ndf.glob
  , best.model.cr.1.adt.sheep.lo.ndf.glob.2
  
  , best.model.cr.1.adt.sheep.hi.ndf.n.cond
  , best.model.cr.1.adt.sheep.hi.ndf.glob
  , best.model.cr.1.adt.sheep.hi.ndf.glob.2
  
  
  , pred.labels = factor(pred.labels.list) 
  ,  dv.labels = c(dv.labels.list , dv.labels.list)   #dv.labels.list[c(1:4)]  
  
  # , string.pred = "Coefficient"
  , collapse.ci = TRUE
  #, string.p = "P-Value"
  , p.style="stars"
  , show.icc = FALSE
  , show.re.var = FALSE
  , show.obs = TRUE
  , show.r2 =  TRUE
  , show.ngroups = FALSE
  , file = dmi.estim8.adt.sheep.html.file.name
  
)

tab_model(
  
  best.model.cr.1.adt.goats.lo.ndf.n.cond
  , best.model.cr.1.adt.goats.lo.ndf.glob
  , best.model.cr.1.adt.goats.lo.ndf.glob.2
  
  , best.model.cr.1.adt.goats.hi.ndf.n.cond
  , best.model.cr.1.adt.goats.hi.ndf.glob
   , best.model.cr.1.adt.goats.hi.ndf.glob.2
  
  
  , pred.labels = pred.labels.list
  ,  dv.labels = c(dv.labels.list , dv.labels.list)
  
  # , string.pred = "Coefficient"
  , collapse.ci = TRUE
  #, string.p = "P-Value"
  , p.style="stars"
  , show.icc = FALSE
  , show.re.var = FALSE
  , show.obs = TRUE
  , show.r2 =  TRUE
  , show.ngroups = FALSE
  , file = dmi.estim8.adt.goats.html.file.name
  
)

}



# Model validation plots
{
  
  
model.tform.n.cond <- 'n.cond'  
model.tform.glob.optm <- 'glob.optm'  


{
#
# Sheep, low NDF
c.model <- best.model.cr.1.gr.sheep.lo.ndf.n.cond
    
    
sheep.hi.ndf.norm.cnd <- data.frame(
  
actual = s.rums.cc[ reg.inc.cond.sheep.hi.ndf.selc.3 ,  y.var.reg  ]
, predicted = predict(  c.model  )
#, residuals = residuals(  c.model  )
#, species.NDF = rep('Sheep - High NDF ', length(predict(c.model))  )
#, fit.tform = rep(model.tform.n.cond , length(predict(c.model)) )
#, exp.vr.1 =    s.rums.cc[ reg.inc.cond.sheep.hi.ndf  , form.cr.1.sheep.hi.ndf.expv.1  ]   
#, exp.vr.2 =    s.rums.cc[ reg.inc.cond.sheep.hi.ndf  , form.cr.1.sheep.hi.ndf.expv.2  ]
)

c.model <- best.model.sheep.hi.ndf.selc.2


sheep.hi.ndf.glob.optm <- data.frame(
  
  actual = s.rums.cc[ reg.inc.cond.sheep.hi.ndf.selc.2 , y.var.reg   ]
  , predicted = predict(  c.model  )
 # , residuals = residuals(  c.model  )
 # , species.NDF = rep('Sheep - High NDF ', length(predict(c.model)))
 # , fit.tform = rep(model.tform.glob.optm , length(predict(c.model)) )
  #, exp.vr.1 =    s.rums.cc[ reg.inc.cond.sheep.hi.ndf  , form.cr.1.sheep.hi.ndf.expv.1  ]   
  #, exp.vr.2 =    s.rums.cc[ reg.inc.cond.sheep.hi.ndf  , form.cr.1.sheep.hi.ndf.expv.2  ]
)

sheep.hi.ndf <- rbind( sheep.hi.ndf.norm.cnd, sheep.hi.ndf.glob.optm )


# Sheep, low NDF
c.model <- best.model.cr.1.sheep.lo.ndf.n.cond

sheep.lo.ndf.norm.cnd <- data.frame(
  
actual = s.rums.cc[ reg.inc.cond.sheep.lo.ndf.n.cond ,  y.var.reg  ]
, predicted = predict(  c.model  )
, residuals = residuals(  c.model  )
, species.NDF = rep('Sheep - Low NDF ', length(predict(   c.model   )))
, fit.tform = rep(model.tform.n.cond , length(predict(c.model)) )
)

c.model <- best.model.cr.1.sheep.lo.ndf.glob

sheep.lo.ndf.glob.optm <- data.frame(
  
  actual = s.rums.cc[ reg.inc.cond.sheep.lo.ndf.glob.otpm ,  y.var.reg  ]
  , predicted = predict(  c.model  )
  , residuals = residuals(  c.model  )
  , species.NDF = rep('Sheep - Low NDF ', length(predict(   c.model   )))
  , fit.tform = rep(model.tform.glob.optm, length(predict(c.model)) )
)

sheep.lo.ndf <- rbind( sheep.lo.ndf.norm.cnd, sheep.lo.ndf.glob.optm )


# Goats, high NDF
c.model <- best.model.cr.1.goats.hi.ndf.n.cond

  
goats.hi.ndf.norm.cnd <- data.frame(
  
actual = s.rums.cc[  reg.inc.cond.goats.hi.ndf.n.cond , y.var.reg  ]
, predicted = predict(  c.model  )
, residuals = residuals(  c.model  )
, species.NDF = rep('Goats - High NDF ', length(predict(c.model)))
, fit.tform = rep(model.tform.n.cond, length(predict(c.model)) )

)

c.model <- best.model.cr.1.goats.hi.ndf.glob


goats.hi.ndf.glob.optm <- data.frame(
  
  actual = s.rums.cc[  reg.inc.cond.goats.hi.ndf.glob.otpm , y.var.reg  ]
  , predicted = predict(  c.model  )
  , residuals = residuals(  c.model  )
  , species.NDF = rep('Goats - High NDF ', length(predict(c.model)))
  , fit.tform = rep(model.tform.glob.optm , length(predict(c.model)) )
  
)

goats.hi.ndf <- rbind( goats.hi.ndf.norm.cnd, goats.hi.ndf.glob.optm )

# Goats, high NDF
c.model <- best.model.cr.1.goats.lo.ndf.n.cond


goats.lo.ndf.norm.cnd <- data.frame(
  
actual = s.rums.cc[  reg.inc.cond.goats.lo.ndf.n.cond , y.var.reg  ]
, predicted = predict(c.model )
, residuals = residuals(  c.model  )
, species.NDF = rep('Goats - Low NDF ', length(predict(c.model )))
, fit.tform = rep(model.tform.n.cond, length(predict(c.model)) )

)

c.model <- best.model.cr.1.goats.lo.ndf.glob


goats.lo.ndf.glob.optm <- data.frame(
  
  actual = s.rums.cc[  reg.inc.cond.goats.lo.ndf.glob.otpm , y.var.reg  ]
  , predicted = predict(c.model )
  , residuals = residuals(  c.model  )
  , species.NDF = rep('Goats - Low NDF ', length(predict(c.model )))
  , fit.tform = rep(model.tform.glob.optm, length(predict(c.model)) )
  
)

goats.lo.ndf <- rbind( goats.lo.ndf.norm.cnd, goats.lo.ndf.glob.optm )



errors.all <- rbind( sheep.hi.ndf  , sheep.lo.ndf , goats.hi.ndf , goats.lo.ndf )

# Factorize species NDF
unq.species.ndf <- unique(errors.all$species.NDF )
ordered.species.ndf <- unq.species.ndf

errors.all$species.NDF <- factor ( errors.all$species.NDF  , levels =ordered.species.ndf )

# Factorize fit transform 
unq.fit.tform <- unique(errors.all$fit.tform )
ordered.fit.tform  <- unq.fit.tform

errors.all$fit.tform <- factor ( errors.all$fit.tform , levels = ordered.fit.tform )



}




gen.theme <- ggplot(errors.all ) + 
  theme(
    panel.grid.major = element_blank()
    ,panel.background = element_blank()
    ,panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
    
    ,strip.background = element_rect(color='black', fill='white', size=.5, linetype="solid")
    ,strip.text.x = element_text(size =  11, color = 'black'  , face = 'italic')
    
    
    
    
  )

gg.error <- gen.theme %>% +
  geom_point( aes(x = actual , y = predicted)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  facet_grid( ~ species.NDF + fit.tform
              #, scales = 'free_x'
              , space = 'free_x'
              )

gg.error

gg.residuals <- gen.theme %>% +  
geom_point(aes( x = predicted , y = residuals)  )  +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed", color = "red") +
  facet_grid( ~ species.NDF + fit.tform  , scales = 'fixed' , space = 'free_x') +
  geom_smooth(aes( x = predicted , y = residuals) , method = 'loess' , span = 0.99)

gg.residuals



gg.qq <- gen.theme %>% +  
  geom_qq(aes( sample = residuals   )  ) +
  geom_qq_line(aes( sample = residuals   ) ) +
  facet_grid( ~ species.NDF + fit.tform  , scales = 'free_x' , space = 'free_x') 

gg.qq.exp.vrs.sheep.hi.ndf <- gen.theme %>% + 
  geom_qq(aes( sample = exp.vr.1   )  ) +
  geom_qq_line(aes( sample = exp.vr.1   ) ) 


}






# Frequency figure
{
  

gen.freq.df <- function(){ return(data.frame( ))}


for (s in species.srs ){ 
for (n in ndf.levs ){ 
for (p in unique.stages.cond ){

if (s == species.srs[1] & n == ndf.levs[1] & p == unique.stages.cond[1]) { row <- 1} 
if (row == 1) { freq.data <- gen.freq.df()  }  
  
freq.data[ row , 'species' ] <- s
freq.data[ row , 'ndf.level' ] <- n


species.lab <- species.lc.plural.labs[ which( s == species.srs ) ]
ndf.lab <- ndf.labs[ which( n == ndf.levs ) ]


d.cond <- (!is.na(d$Species)  &  d$Species == s & !is.na(d$ndf.level) & d$ndf.level == n & !is.na(d$stage.cond) & d$stage.cond == p)
d.cond.all <- (!is.na(d$Species)  & !is.na(d$ndf.level) & !is.na(d$stage.cond)  &  d$Species == s)

quantity <- nrow(d[d.cond , ])  
animals <- sum( d[ d.cond , 'T.Animals' ])   

freq.data[ row , 'freq' ] <-  nrow(d[d.cond , ])  
freq.data[ row , 'freq.frac' ] <- freq.data[ row , 'freq' ] / nrow(d[d.cond.all , ])  


freq.data[ row , 'label' ] <- str_c(p , " ", species.lab , " -\n ",  ndf.lab , " -\n n = ",quantity , " -\n animals = ",animals)


row <- row + 1

}}}

sum(freq.data[ ,'freq.frac' ])


frq.sp <- ggplot( freq.data[freq.data$species == species.sheep, ], aes(area = freq.frac,  fill = freq.frac, label = label)) +
  geom_treemap() +
  geom_treemap_text(colour = gg.freq.group.fs.color ,
                    place = "centre"
                    , size =    8
  ) +
  scale_fill_gradient(
    low = color.grad.freq.lo ,
    high =  color.grad.freq.hi,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  coord_cartesian(ylim = c(0, 1),
                  xlim = c(0, 1),
                  clip="off") + theme(
legend.position = 'none'
) +
  geom_text(
    label = "a", 
    x= gg.freq.label.xc  ,
    y=  gg.freq.label.yc , 
  #  label.padding = unit(p.freq.lab.padding, "lines") # Rectangle size around label
    , size = gg.freq.label.sz 
   # , fill = "grey"
    , color = gg.freq.label.clr 
  #  ,  label.size = 12
  ) 

frq.gt <-ggplot( freq.data[freq.data$species == species.goat, ], aes(area = freq.frac,  fill = freq.frac, label = label)) +
  geom_treemap() +
  geom_treemap_text(colour = gg.freq.group.fs.color,
                    place = "centre"
                    , size =    8
  ) +
  coord_cartesian(ylim = c(0, 1),
                  xlim = c(0, 1),
                  clip="off") + theme(
                    
                      legend.position = 'none')+
  scale_fill_gradient(
    low = color.grad.freq.lo ,
    high =  color.grad.freq.hi,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  geom_text(
    label = "b", 
    x= gg.freq.label.xc  ,
    y=  gg.freq.label.yc , 
    #  label.padding = unit(p.freq.lab.padding, "lines") # Rectangle size around label
    , size = gg.freq.label.sz 
    # , fill = "grey"
    , color = gg.freq.label.clr 
    #  ,  label.size = 12
  ) 



fig.freq.n  <<- grid.arrange(
  
  frq.sp 
  , frq.gt
  
  , widths = c( 1, 1 )
  , heights = c( 1, 1 )
  , layout_matrix = rbind(
    c(1, 1 ),
    c(2 , 2)
  )
  
)

