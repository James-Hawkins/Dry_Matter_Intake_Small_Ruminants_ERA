

# Table out

print('Running table coefs out')

dat.out <-  data.frame(
  species = NA
)

r.count <- 1

for (s in species.srs[1]){
  for (n in ndf.levs ){
    for (mf in 1:n.mod.form){
      
      
      # Test: s <-  species.srs[1] ; n <- ndf.levs[1] ; mf <- 1  
      
      dat.out[ r.count  , 'species'  ] <- s
      dat.out[ r.count  , 'ndf'  ] <- n
      dat.out[ r.count  , 'mod.form'  ] <- mf
      
      d.gbr.row <- d.gbr[ 
        
        (d.gbr$species ==  s 
         &  d.gbr$ndf ==  n  
         & d.gbr$mod.form == mf
         & d.gbr$is.best.model
         & d.gbr$k == 1
        )
        
        ,    ]
      
      # Mean coefficients
      coef.BW.mean <- round( d.gbr.row$mean.coef.BW , 2)
      coef.ADG.mean <- round( d.gbr.row$mean.coef.ADG , 2)
      coef.NDF.mean <- round( d.gbr.row$mean.coef.NDF , 2)
      coef.CP.mean <- round( d.gbr.row$mean.coef.CP , 2)
      
      
      # Standard deviations of coefficient
      coef.BW.se <- round( d.gbr.row$sd.coef.BW , 2)
      coef.ADG.se <- round( d.gbr.row$sd.coef.ADG , 2)
      coef.NDF.se <- round( d.gbr.row$sd.coef.NDF , 2)
      coef.CP.se <- round( d.gbr.row$sd.coef.CP , 2)
      
      
      
      if (!is.na(coef.BW.mean )  & !is.na(coef.BW.se) ) { stars.bw <- stat.significance( coef.BW.mean  , coef.BW.se) } else { stars.bw <- ''}
      if (!is.na(coef.ADG.mean )  & !is.na(coef.ADG.se) ) { stars.adg <- stat.significance( coef.ADG.mean  , coef.ADG.se) } else { stars.adg <- ''}
      if (!is.na(coef.NDF.mean )  & !is.na(coef.NDF.se) ) { stars.ndf <- stat.significance( coef.NDF.mean  , coef.NDF.se) } else { stars.ndf <- ''}
      if (!is.na(coef.CP.mean )  & !is.na(coef.CP.se) ) { stars.cp <-stat.significance( coef.CP.mean  , coef.CP.se) } else { stars.cp <- ''}
      
      
      
      pls.mns <- ' \u00B1 '
      coef.BW.mn.plus.sd <- str_c( coef.BW.mean  , pls.mns , coef.BW.se   , stars.bw)
      coef.ADG.mn.plus.sd <- str_c( coef.ADG.mean  , pls.mns , coef.ADG.se  , stars.adg)
      coef.NDF.mn.plus.sd <- str_c( coef.NDF.mean  , pls.mns , coef.NDF.se , stars.ndf)
      coef.CP.mn.plus.sd <- str_c( coef.CP.mean  , pls.mns , coef.CP.se , stars.cp)
      
      
      dat.out[ r.count  , 'all.coef.bw.kg'  ] <-  coef.BW.mn.plus.sd
      dat.out[ r.count  , 'all.coef.adg.kg'  ] <-  coef.ADG.mn.plus.sd
      dat.out[ r.count  , 'all.coef.ndf.kg'  ] <-  coef.NDF.mn.plus.sd
      dat.out[ r.count  , 'all.coef.cp.kg'  ] <-  coef.CP.mn.plus.sd
      
      
      
      
      # dat.out[ r.count  , 'mean.coef.bw.kg'  ] <-  coef.BW.mean 
      # dat.out[ r.count  , 'mean.coef.adg.g.d'  ] <-  coef.ADG.mean 
      # dat.out[ r.count  , 'mean.coef.ndf.g.kg'  ] <-  coef.NDF.mean 
      # dat.out[ r.count  , 'mean.coef.cp.g.kg'  ] <-  coef.CP.mean 
      
      # dat.out[ r.count  , 'sd.coef.bw.kg'  ] <-  coef.BW.sd 
      #dat.out[ r.count  , 'sd.coef.adg.g.d'  ] <-  coef.ADG.sd 
      #dat.out[ r.count  , 'sd.coef.ndf.g.kg'  ] <-  coef.NDF.sd 
      #dat.out[ r.count  , 'sd.coef.cp.g.kg'  ] <-  coef.CP.sd 
      
      
      # to add: full formula , feature importance, stars for variable significance
      
      
      r.count <- r.count + 1
    }
  }
}


write.xlsx(  dat.out 
             , str_c(results.out.dir, "dat.out.xlsx")
             #  ,  sheetName = "Sheet1"
             #  , col.names = TRUE
             #  , row.names = TRUE
             # , append = FALSE
)

