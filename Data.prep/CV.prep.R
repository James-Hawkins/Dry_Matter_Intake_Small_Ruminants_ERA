

# source('CV.prep.R')

source('m.boost.params.R') ; source('functions.R') 

{ 
  
  
  
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
    
    
    n.mod.vers <-  n.mod.types  * n.mod.v.family * n.mod.v.boost.control.mstop * n.mod.v.boost.control.nu 
    
    n.mod.v.ids <- c()
    
    count <- 1
    type.count <<- 1
    family.count <<- 1
    mstop.count <<- 1
    nu.count <<- 1
    
    str.filler <- '-'
    
    # Model version ID: type-family-mstop-nu
    
    for (t in 1  :  n.mod.types) { 
    for (f in 1  :  n.mod.v.family) { 
      for (m in 1  :  n.mod.v.boost.control.mstop) { 
        for (n in 1 :   n.mod.v.boost.control.nu) {
          
          
          n.mod.v.ids[ count   ] <- str_c( t ,str.filler , f,  str.filler , m  , str.filler , n)
          
          count <- count + 1
          
          
          nu.count <- nu.count + 1}
          mstop.count <- mstop.count + 1}
          family.count <- family.count + 1 }
     }
    
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
  
  
  d.gbr <- data.frame( matrix(NA, nrow = n.rows, ncol = 1) )
  
  
  d.gbr[, 'loop.iter' ] <- NA
  
  # Sample and model descriptors
  d.gbr[, 'species' ] <- col.species
  d.gbr[, 'ndf' ] <- col.ndf
  d.gbr[, 'mod.form' ] <- col.mf
  d.gbr[, 'mod.vers' ] <- col.mod.vers
  d.gbr[, 'k' ] <- col.k
  
  d.gbr[, 'm.stop' ] <- NA
  d.gbr[, 'nu' ] <- NA
  
  
  # Global metrics - optimal model
  d.gbr[ , vn.best.w.R2 ] <- NA
  d.gbr[ , vn.best.w.nRMSE ] <- NA
  d.gbr[ , vn.best.w.CCC ] <- NA
  d.gbr[ , vn.best.w.AIC ] <- NA
  
  
  d.gbr[ ,  'is.best.model'] <- FALSE
  
  # Global metrics = all formulae - optimal model
  d.gbr[ , vn.best.global.w.R2 ] <- NA
  d.gbr[ , vn.best.global.w.nRMSE ] <- NA
  d.gbr[ , vn.best.global.w.CCC ] <- NA
  d.gbr[ , vn.best.global.w.AIC ] <- NA
  
  d.gbr[ ,  'is.best.global.model'] <- FALSE
  
  
  # Current iteration
  d.gbr[ , vn.w.R2.mean ] <- NA
  d.gbr[ , vn.w.nRMSE.mean ] <- NA
  d.gbr[ , vn.w.CCC.mean ] <- NA
  d.gbr[ , vn.w.AIC.mean ] <- NA
  
  d.gbr[ , vn.w.R2.sd ] <- NA
  d.gbr[ , vn.w.nRMSE.sd ] <- NA
  d.gbr[ , vn.w.CCC.sd ] <- NA
  d.gbr[ , vn.w.AIC.sd ] <- NA
  
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
  
  d.gbr[ , 'form'] <- NA
  d.gbr[ , 'model'] <- NA
  
  # FOLD CONDITIONNING
  
  # Whole sample IDs
  d.gbr[ , fold.cond.variables.all] <- NA
  

  
  # Fold sub-setting
  # d.gbr[ , 'fold.t.IDs'] <- NA
  # d.gbr[ , 'fold.t.IDs.length'] <- NA
  d.gbr[ , vn.t.IDS.CCs.remaining ] <- NA
  
  # Test - train split
  d.gbr[ , vn.train.t.IDS ] <- NA
  d.gbr[ , vn.train.t.IDS.length ] <- NA
  d.gbr[ ,  vn.test.t.IDS ] <- NA
  d.gbr[ , vn.test.t.IDS.length ] <- NA
  
  
  d.gbr[ , 'r.cond'] <- NA
  d.gbr[ , 'all.data'] <- NA
  
  

  


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
  d.gbr[gbm.cond.shp.lo.ndf  , 'all.treatment.IDs'] <- list(list(  unique(s.rums[ cnd.sp.lo.ndf , 'ut.id'])  ))
  d.gbr[gbm.cond.shp.hi.ndf  , 'all.treatment.IDs'] <- list(list(unique(s.rums[ cnd.sp.hi.ndf, 'ut.id'])))
  
  d.gbr[gbm.cond.gt.lo.ndf , 'all.treatment.IDs'] <- list(list(unique(s.rums[ cnd.gt.lo.ndf , 'ut.id'])))
  d.gbr[gbm.cond.gt.hi.ndf  , 'all.treatment.IDs'] <- list(list(unique(s.rums[ cnd.gt.hi.ndf, 'ut.id'])))
  
  # Experiment Id lists
  d.gbr[gbm.cond.shp.lo.ndf  , 'all.experiment.IDs'] <- list(list(  s.rums[ cnd.sp.lo.ndf , 'ue.id'])  )
  d.gbr[gbm.cond.shp.hi.ndf  , 'all.experiment.IDs'] <- list(list(s.rums[ cnd.sp.hi.ndf, 'ue.id']))
  
  d.gbr[gbm.cond.gt.lo.ndf , 'all.experiment.IDs'] <- list(list(s.rums[ cnd.gt.lo.ndf , 'ue.id']))
  d.gbr[gbm.cond.gt.hi.ndf  , 'all.experiment.IDs'] <- list(list(s.rums[ cnd.gt.hi.ndf, 'ue.id']))
  
  
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
  
 # for (r in 1:nrow(d.gbr)) { d.gbr[ r, 'IDs.remaining'] <- list(list( d.gbr[ r, 'treatment.IDs'])) }
  
  
} # Data pre processing

