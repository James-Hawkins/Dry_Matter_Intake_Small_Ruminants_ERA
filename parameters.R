

# File paths
# Main folders
parent.dir <<- '../'
helpers.dir <<- '/Helpers'
results.dir <<- 'Results/'
data.dir <<- 'Data.prep/'


# Sub-folders
results.out.dir <<- 'Results/Figures.out/'



ndf.lev.lo <<- 'low'
ndf.lev.hi <<- 'hi'


ndf.thresh.lo <<- 400
ndf.thresh.hi <<- 400

ndf.thresh.sheep <<- 385
ndf.thresh.goat <<- 420



# Outlier variable ranges 
max.adg.g.day  <<- 2000
min.adg.g.day  <<- -2000

max.ndf.g.kg <<- 0
min.ndf.g.kg <<- 0





suffx.mm.s <<- 'min.max.s'
suffx.mean.s <<- 'mean.s'

#length(unique(  d[d$Species == species.sheep & d$NDF_nutrition < 385 & !is.na(d$NDF_nutrition)  , 'B.Code' ]))  / length(unique(  d[d$Species == species.sheep & !is.na(d$NDF_nutrition) , 'B.Code' ])) 
#length(unique(  d[d$Species == species.goat & d$NDF_nutrition < 420 & !is.na(d$NDF_nutrition)  , 'B.Code' ]))  / length(unique(  d[d$Species == species.goat & !is.na(d$NDF_nutrition) , 'B.Code' ])) 



ndf.levs <- c( ndf.lev.lo ,ndf.lev.hi )
ndf.labs <- c( 'low NDF' ,'high NDF')




species.lc.plural.labs <- c('sheep' , 'goats')

   
gbr.reg.variables <<- c(
  
  'Species'
  , 'ndf.level'
  , 'species.ndf'
  , "T.Animals"  
  , 'Sample.size'
  
  , "B.Code" 
  ,   "A.Level.Name" 
  
  
  , 'feed_intake_g_d'
  , 'DM_nutrition'
  
  
  , "NDF_nutrition" 
  , "CP_nutrition" 
  , "bw_kg" 
  ,  "adg_g_day"
  ,  "NDF_digest"
  
  , "Ash_nutrition"
 , "EE_nutrition" 
  
)



# Specify which Y variable to use
y.var <<- 0
y.var[1] <- 'feed_intake_g_d'
y.var[2] <- 'feed_intake_kg_d'
y.var[3] <- 'feeding.level.g.d.g.bw'
y.var[4] <- 'feeding.level.g.d.kg.bw'

y.var.reg <<- y.var[1]


gg.freq.label.clr <<- "white"
gg.freq.label.sz  <<- 5.5
gg.freq.label.xc <<- 0.00725
gg.freq.label.yc <<- 0.975


color.grad.freq.hi <<- '#2F5F8A'
color.grad.freq.lo <<-  '#75A2BF'


gg.freq.group.fs.color <- gg.freq.label.clr

# Sample size equation to weight formulae

#sample.weight.var <- 'T.Animals'
sample.weight.var <<- 'Sample.size'


sample.coef.weight <<- 0.75


# Outlier handling

ol.rm.gr.sheep.lo.ndf <<- FALSE
ol.rm.gr.sheep.hi.ndf <<- FALSE
ol.rm.gr.goat.lo.ndf <<- FALSE
ol.rm.gr.goat.hi.ndf <<- FALSE


# Scaling parameter used in derivation of weights for GBM




rd.decs.R2 <<- 2
rd.decs.nRMSE <<- 1
