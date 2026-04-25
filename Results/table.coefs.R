

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
      
      
      
      
       dat.out[ r.count  , 'mean.coef.bw.kg'  ] <-  coef.BW.mean 
       dat.out[ r.count  , 'mean.coef.adg.g.d'  ] <-  coef.ADG.mean 
       dat.out[ r.count  , 'mean.coef.ndf.g.kg'  ] <-  coef.NDF.mean 
       dat.out[ r.count  , 'mean.coef.cp.g.kg'  ] <-  coef.CP.mean 
      
       dat.out[ r.count  , 'sd.coef.bw.kg'  ] <-  coef.BW.se 
      dat.out[ r.count  , 'sd.coef.adg.g.d'  ] <-  coef.ADG.se 
      dat.out[ r.count  , 'sd.coef.ndf.g.kg'  ] <-  coef.NDF.se 
      dat.out[ r.count  , 'sd.coef.cp.g.kg'  ] <-  coef.CP.se 
      
      
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


plot.vars <- c('bw' , 'adg' , 'ndf' , 'cp')

dat.out.2 <-  data.frame(
  species = NA
)


r.count <- 1

for (s in species.srs[1]){
  for (n in ndf.levs ){
    for (mf in 1:n.mod.form){
      for (v in plot.vars){
      
      
      # Test: s <-  species.srs[1] ; n <- ndf.levs[1] ; mf <- 1  ; v <- plot.vars[1]
      
      dat.out.2[ r.count  , 'species'  ] <- s
      dat.out.2[ r.count  , 'ndf'  ] <- n
      dat.out.2[ r.count  , 'mod.form'  ] <- mf
      dat.out.2[ r.count  , 'var'  ] <- v
      
      d.gbr.row <- d.gbr[ 
        
        (d.gbr$species ==  s 
         &  d.gbr$ndf ==  n  
         & d.gbr$mod.form == mf
         & d.gbr$is.best.model
         & d.gbr$k == 1
        )
        
        ,    ]
      
      # coefficient
      
      if (  v == plot.vars[1]  ) { var.lab.coef <-  'mean.coef.BW' ; var.lab.se <-  'sd.coef.BW'}
      if (  v == plot.vars[2]  ) { var.lab.coef <-  'mean.coef.ADG' ; var.lab.se <-  'sd.coef.ADG'}
      if (  v == plot.vars[3]  ) { var.lab.coef <-  'mean.coef.NDF' ; var.lab.se <-  'sd.coef.NDF'}
      if (  v == plot.vars[4]  ) { var.lab.coef <-  'mean.coef.CP' ; var.lab.se <-  'sd.coef.CP'}
      
    

      coef <- round( d.gbr.row[,var.lab.coef ], 2)
      se <- round( d.gbr.row[,var.lab.se ], 2)
      
      dat.out.2[ r.count  , 'coef'  ] <-  coef
      dat.out.2[ r.count  , 'se'  ] <-  se

      r.count <- r.count + 1
    }
  }
  }
}


gg.coefs.gen <- ggplot(  ) +
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


gg.coefs <- gg.coefs.gen  %>% + 
            geom_bar( dat.out.2 , mapping = aes( x = var ,y = coef) , stat = "identity"  ) +
       #facet_nested( . ~  species + ndf + mod.form  ) +
  facet_nested(rows = vars(ndf), cols = vars(mod.form)) +
      geom_errorbar(dat.out.2 , mapping =  aes( x = var ,ymin = coef - se , ymax = coef + se)   )
  #coord_flip() 




