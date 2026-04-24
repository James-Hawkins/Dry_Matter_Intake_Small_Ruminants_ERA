
{ 
# Factorisation
s.rums$species.ndf <- factor(s.rums$species.ndf , levels = c( sheep.lo.ndf , sheep.hi.ndf ,goat.lo.ndf ,goat.hi.ndf)  )


gg.bp.0 <- ggplot( s.rums[s.rums.dont.exclude , ]) +
            theme(
                axis.title.x = element_blank()
                ,  axis.title.y = element_text( size = 11)
                , axis.text.x = element_text( angle = 90)
                , axis.ticks.x = element_blank()
                ,  panel.grid.major = element_blank()
                ,  panel.background = element_blank(),
                 ,  panel.border = element_rect(colour = "black", fill=NA, linewidth = 1  ),
                ,  strip.background = element_rect(color='black', fill='white', linewidth = 1, linetype="solid")
                , strip.text.x = element_text(size = 12 , color = 'black' )
            )

} # Data prep


gg.bp.dmi.0 <- gg.bp.0   %>%   +
          geom_boxplot( mapping = aes(x = species.ndf , y = feed_intake_kg_d ) )   +
          ylab('Dry matter\nintake (kg/d)')

gg.bp.dmi.0

gg.bp.bw.0 <- gg.bp.0   %>%   +
  geom_boxplot(mapping = aes(x = species.ndf, y = bw_kg ) )  + 
 ylab('\nBodyweight(kg)')


gg.bp.bw.0

gg.bp.ndf.0 <- gg.bp.0   %>%   +
  geom_boxplot(, mapping = aes(x = species.ndf, y = NDF_nutrition ) )  + 
  ylab('Neutral detergent\nfibre (g/kg)')


gg.bp.ndf.0

gg.bp.adg.0 <- gg.bp.0   %>%   +
  geom_boxplot( mapping = aes(x = species.ndf, y = adg_g_day ) )  + 
  ylab('Average daily\ngain (g)')


gg.bp.adg.0 

# Plot prep
{


 gg.bp.adg <- gg.bp.adg.0   
gg.bp.bw <- gg.bp.bw.0     
gg.bp.ndf <- gg.bp.ndf.0   %>%   + theme(  axis.text.x = element_blank())
gg.bp.dmi <- gg.bp.dmi.0     %>%   + theme(  axis.text.x = element_blank())


  
}

gg.bp.all.0 <- ggarrange(
  
   gg.bp.dmi
  , gg.bp.ndf
  , gg.bp.adg
  , gg.bp.bw
  
  , nrow = 2
, ncol = 2

, heights = c(1 , 1.38)
, labels = c('a','b','c','d')

)

gg.bp.dpi <- 1500
gg.bp.glob.scalar <- 0.625
gg.bp.width.sp <- 9 * gg.bp.glob.scalar
gg.bp.height.sp  <- 8.2* gg.bp.glob.scalar

filename.bp = 'Figures.out/boxplots.jpeg'


ggsave(filename =  filename.bp ,  gg.bp.all.0, width = gg.bp.width.sp , height = gg.bp.height.sp   , dpi = gg.bp.dpi)


# Table out

dat.out <-  data.frame(
  species = NA
  , ndf = NA
  #, coef.bw = NA
  #, se.bw = NA
)

r.count <- 1

for (s in species.srs[1]){
for (n in ndf.levs ){
for (mf in 1:n.mod.form){
  
  
  # Test: s <-  species.srs[1] ; n <- ndf.levs[1] ; mf <- 1 ;  
  
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
  

  
  dat.out[ r.count  , 'coef.bw.kg'  ] <-  d.gbr.row$mean.coef.BW
  dat.out[ r.count  , 'coef.adg.g.d'  ] <-  d.gbr.row$mean.coef.ADG
  dat.out[ r.count  , 'coef.ndf.g.kg'  ] <-  d.gbr.row$mean.coef.NDF
  dat.out[ r.count  , 'coef.cp.g.kg'  ] <-  d.gbr.row$mean.coef.CP
  
  
  r.count <- r.count + 1
}
}
}


write.xlsx(  dat.out 
             , "Figures.out/dat.out.xlsx"
             #  ,  sheetName = "Sheet1"
             #  , col.names = TRUE
             #  , row.names = TRUE
             # , append = FALSE
)

