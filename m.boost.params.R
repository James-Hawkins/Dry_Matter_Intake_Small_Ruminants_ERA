

# source('m.boost.params.R')

# GLOBAL SETTINGS

random.exp.int <<- FALSE

fold.by.exp <<- FALSE

# age.status <<- 'growing'
age.status <<- NA


y.var <<- 'feed_intake_g_d'
#y.var <<- 'drym_intake_g_d'




#

# comprehensive list of var names

vn.w.R2.mean <<- 'w.R2.mean'

vn.best.w.R2 <<-'best.w.R2'
vn.best.w.nRMSE <<-'best.w.nRMSE'
vn.best.w.CCC <<-'best.w.CCC'
vn.best.w.AIC <<-'best.w.AIC'

vn.best.global.w.R2 <<-'best.global.w.R2'
vn.best.global.w.nRMSE <<-'best.global.w.nRMSE'
vn.best.global.w.CCC <<-'best.global.w.CCC'
vn.best.global.w.AIC <<-'best.global.w.AIC'

vn.w.R2.mean <<-'w.R2.mean'
vn.w.nRMSE.mean <<-'w.nRMSE.mean'
vn.w.CCC.mean <<-'w.CCC.mean'
vn.w.AIC.mean <<-'w.AIC.mean'

vn.w.R2.sd <<-'w.R2.sd'
vn.w.nRMSE.sd <<-'w.nRMSE.sd'
vn.w.CCC.sd <<-'w.CCC.sd'
vn.w.AIC.sd <<- 'w.AIC.sd'

# Outlier control
ol.status.var.name <<- 'ol.status'



# Fold conditioning
vn.train.t.IDS  <<- 'train.t.IDs' 
vn.train.t.IDS.length   <<- 'train.t.IDs.length' 

vn.test.t.IDS <<- 'test.t.IDs' 
vn.test.t.IDS.length <<- 'train.t.IDs.length'

vn.t.IDS.CCs <<- 't.ID.CCs'
vn.t.IDS.CCs.remaining <<- 't.IDS.CCs.remaining'


fold.cond.variables.all <<- c(
  vn.train.t.IDS
  ,vn.train.t.IDS.length
  ,vn.test.t.IDS
  ,vn.test.t.IDS.length
  , vn.t.IDS.CCs
  ,vn.t.IDS.CCs.remaining
  
  
  , 'all.treatment.IDs'
  ,'all.experiment.IDs'
  ,'total.sample.size'
  
)




cutoff.ol.s <<- 3

include.redf <<- FALSE


# coefficients and constants

base.nat.log.e <<- 2.71828


SE.scalar.90.pci <<- 1.65 ; SE.scalar.95.pci <<- 1.95 ; SE.scalar.99.pci <<- 2.65




# Ranges of hyperparameters per species, ndf level

min.nus <<- c( 0.01 , 0.01 , 0.01 , 0.01)
max.nus <<- c( 0.15 , 0.15 ,0.15 ,0.15 )

min.m.stops <<- c( 400 , 400 , 400 , 400)
max.m.stops <<- c( 1500 , 1500 , 1500 , 1500)

# Parameters for model generation

p.k <- 4
n.species <<- 1
n.ndf <<- 2
n.mod.form <<- 4

# Hyper-parameters
m.stop.cv <<- FALSE
cv.risk.min.grid <<- 1000
cv.risk.max.grid <<- 1100


n.mod.types <<- 1
n.mod.v.family <<- 1
n.mod.v.boost.control.mstop <<- 1
n.mod.v.boost.control.nu <<- 1

ml.boost.families <<- c( Gaussian() , Laplace() , Huber())


mstop.max <<- 3000; mstop.min <<- 1000 ; m.stop.range <<- (mstop.max - mstop.min)
nu.max <<- 0.1 ; nu.min <<- 0.1 ; nu.range <<- (nu.max - nu.min)




optimization.metric.var.name.1 <<- vn.w.R2.mean


all.x.vars <<- c(  
    
    "bw_kg" 
    , 'BW_frac_Mat_BW'
    , 'Met_bw_kg'
    , "bw_kg.e25" 
    , "bw_kg.sqt" 
    , "bw_kg.e75"  
    , "bw_kg.cbd" 
    , "bw_kg.sqd"  
    
    , "bw_kg.log" 
    
    , "adg_g_day"
    , "adg_g_day.e25"
    , "adg_g_day.sqt"
    , "adg_g_day.e75"
    , "adg_g_day.cbd"
    , "adg_g_day.sqd"
    , "adg_g_day.log"
    
    , "NDF_nutrition"
    , "NDF_nutrition.e25"
    , "NDF_nutrition.sqt"
    , "NDF_nutrition.e75"
    , "NDF_nutrition.cbd"
    , "NDF_nutrition.sqd"
    , "NDF_nutrition.log"
    
    , "CP_nutrition"
    , "CP_nutrition.e25"
    , "CP_nutrition.sqt"
    , "CP_nutrition.e75"
    , "CP_nutrition.cbd"
    , "CP_nutrition.sqd"
    , "CP_nutrition.log"
    
    , "NDF_digest"
    
    , 'EE_nutrition'
    
    , 'Ash_nutrition'
    
    
    # Interaction terms
    
    , 'NDF_x_NDF_digest'
    
    , 'is.adult'
    
    ,   'ue.id'
  )  



form.aliases <<- rep(NA, length(all.x.vars) )

label.alias.BW <<- 'BW'
label.alias.MBW <<- 'Met_BW'
label.alias.BWFM <<- 'BW_Frac_MBW'
label.alias.ADG <<-'ADG'
label.alias.NDF <<- 'NDF'
label.alias.CP <<- 'CP'
label.alias.NDFD <<- 'NDFD'
label.alias.NDFxNDFD <<- 'NDF*NDFD'

for (e in 1:length(all.x.vars)){
  
  # test c.var <- x.vars[1]
  
  c.var <- all.x.vars[e]
  
if ( str_detect( c.var , 'bw_kg' )   ){  lab.2.add <- label.alias.BW }
if ( str_detect( c.var , 'bw_kg.e75' )   ){  lab.2.add <- label.alias.MBW }
if ( str_detect( c.var , 'BW_frac_Mat_BW' )   ){  lab.2.add <- label.alias.BWFM  }
if ( str_detect( c.var , 'adg' )   ){  lab.2.add <- label.alias.ADG  } 
if ( str_detect( c.var , 'CP' )   ){  lab.2.add <- label.alias.CP  } 
if ( str_detect( c.var , 'NDF' )   ){  lab.2.add <- label.alias.NDF } 
if ( str_detect( c.var , 'NDF_x_NDF_digest' )   ){  lab.2.add <- label.alias.NDFxNDFD } 

  form.aliases[e] <- lab.2.add
  
}  

form.aliases <<-   form.aliases

suffixes.labs <<-  c(  
  supsc("1/4") 
  , supsc("1/2") 
  , supsc("3/4") 
  , supsc("2") 
  , supsc("3") 
  , subsc("MS") 
  , subsc("MMS") 
  )



