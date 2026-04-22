

# GLOBAL SETTINGS

random.exp.int <<- FALSE


y.var <<- 'feed_intake_g_d'
#y.var <<- 'drym_intake_g_d'


# Outlier control
ol.status.var.name <<- 'ol.status'

cutoff.ol.s <<- 3

include.redf <<- FALSE







# Ranges of hyperparameters per species, ndf level

min.nus <<- c( 0.01 , 0.01 , 0.01 , 0.01)
max.nus <<- c( 0.15 , 0.15 ,0.15 ,0.15 )

min.m.stops <<- c( 400 , 400 , 400 , 400)
max.m.stops <<- c( 1500 , 1500 , 1500 , 1500)

# Parameters for model generation

n.species <<- 2
n.ndf <<- 2

# Hyper-parameters
p.k <- 5
n.mod.form <<- 4
n.mod.v.family <<- 1
n.mod.v.boost.control.mstop <<- 2
n.mod.v.boost.control.nu <<- 2


optimization.metric.var.name.1 <<- vn.w.R2.mean


x.vars <<- c(  
    
    "bw_kg" 
    , 'BW_frac_Mat_BW'
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
    
    
    # Interaction terms
    
    , 'NDF_x_NDF_digest'
    
    , 'is.adult'
    
    ,   'ue.id'
  )  



form.aliases <<- rep(NA, length(x.vars) )

label.alias.BW <<- 'BW'
label.alias.MBW <<- 'Met_BW'
label.alias.BWFM <<- 'BW_Frac_MBW'
label.alias.ADG <<-'ADG'
label.alias.NDF <<- 'NDF'
label.alias.CP <<- 'CP'
label.alias.NDFD <<- 'NDFD'
label.alias.NDFxNDFD <<- 'NDF*NDFD'

for (e in 1:length(x.vars)){
  
  # test c.var <- x.vars[1]
  
  c.var <- x.vars[e]
  
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
  , supsc("x") 
  , supsc("y") 
  )



