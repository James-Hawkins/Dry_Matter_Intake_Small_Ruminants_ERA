

# source('formulae.R)



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
      all.x.vars[1] 
      , all.x.vars[2] 
      , all.x.vars[3] 
      , all.x.vars[4] 
      , all.x.vars[5] 
      , all.x.vars[6] 
      , all.x.vars[7] 
      , all.x.vars[8] 
      , all.x.vars[9] 
      , all.x.vars[10] 
      , all.x.vars[11] 
      , all.x.vars[12] 
      , all.x.vars[13] 
      , all.x.vars[14] 
      , all.x.vars[15] 
      , all.x.vars[16] 
      , all.x.vars[17] 
      , all.x.vars[18] 
      , all.x.vars[19] 
      , all.x.vars[20] 
      , all.x.vars[21] 
      , all.x.vars[22] 
      , all.x.vars[23] 
      , all.x.vars[24] 
      , all.x.vars[25] 
      , all.x.vars[26] 
      , all.x.vars[27] 
      , all.x.vars[28] 
      
      , all.x.vars[29] 
      , all.x.vars[30] 
      , all.x.vars[31]
      , all.x.vars[33] 
      
      
      , sep = " + ")
    
    
    Xs.plus.intercept <- c('Intercept' , all.x.vars  )
    
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
  
  
  
  mod.1.sp.lo.ndf <- as.formula(   feed_intake_g_d ~  bw_kg + adg_g_day  )
  mod.2.sp.lo.ndf <- as.formula(   feed_intake_g_d ~  bw_kg + adg_g_day + NDF_nutrition )
  mod.3.sp.lo.ndf <- as.formula(  feed_intake_g_d  ~  bw_kg + adg_g_day +  NDF_nutrition   )
  mod.4.sp.lo.ndf <- as.formula(   feed_intake_g_d  ~ bw_kg + adg_g_day + NDF_nutrition + CP_nutrition + NDF_digest  )
  
  mod.1.sp.hi.ndf <- mod.1.sp.lo.ndf
  mod.2.sp.hi.ndf <- mod.2.sp.lo.ndf
  mod.3.sp.hi.ndf <- as.formula(   feed_intake_g_d  ~ bw_kg.cbd+ adg_g_day.sqd + NDF_nutrition + CP_nutrition.cbd)
  mod.4.sp.hi.ndf <- mod.4.sp.lo.ndf
  
  mod.1.gt.lo.ndf <- as.formula(   feed_intake_g_d  ~ bw_kg.cbd + adg_g_day.cbd + NDF_nutrition  )
  mod.2.gt.lo.ndf <- as.formula(   feed_intake_g_d  ~ bw_kg + bw_kg.cbd  + adg_g_day + NDF_nutrition  )
  
  mod.1.gt.hi.ndf <- as.formula(  feed_intake_g_d  ~ bw_kg + adg_g_day + NDF_nutrition )
  mod.2.gt.hi.ndf <- as.formula(   feed_intake_g_d   ~ bw_kg + bw_kg.cbd  + adg_g_day + NDF_nutrition  )
  
  d.gbr[gbm.cond.shp.lo.ndf & d.gbr$mod.form == 1, 'form'] <- listify(  mod.1.sp.lo.ndf )
  d.gbr[gbm.cond.shp.lo.ndf & d.gbr$mod.form == 2 , 'form'] <- listify( mod.2.sp.lo.ndf )
  d.gbr[gbm.cond.shp.lo.ndf & d.gbr$mod.form == 3 , 'form'] <- listify( mod.3.sp.lo.ndf )
  d.gbr[gbm.cond.shp.lo.ndf & d.gbr$mod.form == 4 , 'form'] <- listify( mod.4.sp.lo.ndf )
  
  d.gbr[gbm.cond.shp.hi.ndf & d.gbr$mod.form == 1 , 'form'] <- listify( mod.1.sp.lo.ndf )
  d.gbr[gbm.cond.shp.hi.ndf  & d.gbr$mod.form == 2, 'form'] <-  listify( mod.2.sp.lo.ndf )
  d.gbr[gbm.cond.shp.hi.ndf  & d.gbr$mod.form == 3, 'form'] <-  listify( mod.3.sp.lo.ndf )
  d.gbr[gbm.cond.shp.hi.ndf  & d.gbr$mod.form == 4, 'form'] <-  listify( mod.4.sp.lo.ndf )
  
  d.gbr[ gbm.cond.gt.lo.ndf & d.gbr$mod.form == 1, 'form'] <- listify( mod.1.sp.lo.ndf )
  d.gbr[ gbm.cond.gt.lo.ndf & d.gbr$mod.form == 2, 'form'] <-  listify( mod.2.sp.lo.ndf )
  d.gbr[ gbm.cond.gt.lo.ndf & d.gbr$mod.form == 3, 'form'] <-  listify( mod.3.sp.lo.ndf )
  d.gbr[ gbm.cond.gt.lo.ndf & d.gbr$mod.form == 4, 'form'] <-  listify( mod.4.sp.lo.ndf )
  
  
  d.gbr[ gbm.cond.gt.hi.ndf & d.gbr$mod.form == 1 , 'form'] <- listify( mod.1.sp.lo.ndf )
  d.gbr[ gbm.cond.gt.hi.ndf & d.gbr$mod.form == 2 , 'form'] <-  listify( mod.2.sp.lo.ndf )
  d.gbr[ gbm.cond.gt.hi.ndf & d.gbr$mod.form == 3 , 'form'] <-  listify( mod.3.sp.lo.ndf )
  d.gbr[ gbm.cond.gt.hi.ndf & d.gbr$mod.form == 4 , 'form'] <-  listify( mod.4.sp.lo.ndf )
  
  
  if ( random.exp.int){ 
    for (r in 1:nrow(d.gbr)){
      
      form <- de.listify( d.gbr[r , 'form'])
      
      form.with.ue.id <- listify( update(  form , . ~ . + ue.id) )
      
      d.gbr[r , 'form'] <- form.with.ue.id 
      
    }} # If including random intercepts, add 'ue.id' to all formulae
  
  
  
  d.gbr.null <- d.gbr
  
} # Formula initialisations
