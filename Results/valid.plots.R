

print(' Creating validation plots ')

#   source(str_c(results.dir , 'valid.plots.R'))

# ---- PLOTS -------
{
  
  gg.d.sp.lon.all <- gen.gg.df.specific( 1 , 'pmetric' , species.sheep , ndf.lev.lo) 
  
  for (m in 2:n.mod.form  ){ gg.d.sp.lon.all <- rbind(gg.d.sp.lon.all , gen.gg.df.specific(m , 'pmetric' , species.sheep , ndf.lev.lo))  }
  
  gg.d.sp.hin.all <- gen.gg.df.specific( 1 , 'pmetric' , species.sheep , ndf.lev.hi) 
  
  for (m in 2:n.mod.form ){ gg.d.sp.hin.all <- rbind( gg.d.sp.hin.all , gen.gg.df.specific(m , 'pmetric' , species.sheep , ndf.lev.hi))  }
  
} # Data frame tabulation - parametric


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
  
  
  # gg.dat[gg.dat$species == species.sheep & gg.dat$ndf == ndf.lev.lo , 'species.ndf' ] <- 'Sheep - Low NDF'
  #gg.dat[gg.dat$species == species.sheep & gg.dat$ndf == ndf.lev.hi , 'species.ndf' ] <- 'Sheep - High NDF'
  # gg.dat[gg.dat$species == species.goat & gg.dat$ndf == ndf.lev.lo , 'species.ndf' ] <- 'Goat - Low NDF'
  #gg.dat[gg.dat$species == species.goat & gg.dat$ndf == ndf.lev.hi , 'species.ndf' ] <- 'Goat - High NDF'
  
  
  
  # unique.species.ndf <- unique( gg.dat$species.ndf )
  # gg.dat$species.ndf <- factor( gg.dat$species.ndf  , levels = unique.species.ndf)
  
  
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
  
} # Plot params



# View(d.gbr)



gg.y.valid.sp.lon.0 <- gen.gg.valid(gg.d.sp.lon.all  ,gg.valid.lab.x.crd.sp.lon  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3  )

gg.y.valid.sp.lon.0 

gg.y.valid.sp.hin.0 <- gen.gg.valid( gg.d.sp.hin.all ,gg.valid.lab.x.crd.sp.hin  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3  )

gg.y.valid.sp.hin.0


# GOATS
#gg.y.valid.gt.lon.0 <- gen.gg.valid(gg.d.gt.lon , 500  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3  )

#gg.y.valid.gt.lon.0

#gg.y.valid.gt.hin.0 <- gen.gg.valid(gg.d.gt.hin ,600 , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3  )

#gg.y.valid.gt.hin.0

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

p.glob.scalar <- 0.75
p.width.sp <- 11 * p.glob.scalar
p.height.sp  <- 7.75 * p.glob.scalar

filename.sp =   str_c(results.out.dir  , 'sheep.jpeg')


ggsave(filename =  filename.sp ,  gg.valid.sp.all , width = p.width.sp, height = p.height.sp  , dpi = plot.dpi )

