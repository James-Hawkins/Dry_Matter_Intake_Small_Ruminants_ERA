

d.raw <<- read_excel('Srum_master_ERA_data.xlsx'
                 , sheet = 'feed_intake_raw' 
                 , col_types = "text")

d <- as.data.frame(d.raw)




# Data prep
{
  names(d)[2] <- 'diet.code'
  
  for(r in 1:nrow(d)){
    
    d[r,'id'] <- (r-1)
    }
  
  
  convert.numeric <- c(
    'feed_intake_value'
    , 'DM_nutrition'
    , 'bw_kg'
    , 'adg_g_day'
    , 'NDF_nutrition'
    ,  'ADF_nutrition'
    , 'NDF_digest'
    , 'EE_nutrition'
    ,'Ash_nutrition'
    ,  'ME_nutrition'
    , 'DM_digest'
    , 'CP_nutrition'
    , 'milk_kg_day'
    , 'T.Animals'
    
  )
  d <- as.data.frame(d)
  
  
  
  for (v in convert.numeric){
    
    d[,v] <- as.numeric(d[,v] )
    
    
  }
  
  
  
  
  
  na.value <<- 'NA'
  
  unq.spcs <- unique(d$Species)
  unq.stages <- unique(d$Stage.harmonized)
  
  species.cattle <- c(   unq.spcs[3], unq.spcs[9]  )
  species.shoat <- c(unq.spcs[1] ,unq.spcs[2]  )
  species.goat <- c(unq.spcs[2]  )
  species.sheep <- c(unq.spcs[1]  )
  species.srs <<- c( species.sheep ,   species.goat )
  
  stage.growing.male <- "growing.male"
  stage.growing.female <- "growing.female"
  stage.growing.all <-  "growing.all" 
  
  
  stage.adult.male <- "adult.male"
  stage.adult.female <- "adult.female"
  stage.adult.all <-  "adult.all"
  
  
  stage.lactating <<- "lactating.adult.female"
  stage.gestating <<- "gestating.adult.female"
  
  unique.stages.cond <<- c( 'Growing' , 'Mature')
  
  
  
  
  d$is.growing <- FALSE
  d[ !is.na(d$Stage.harmonized) & (  d$Stage.harmonized == stage.growing.male | d$Stage.harmonized == stage.growing.female | d$Stage.harmonized == stage.growing.all  ) , 'is.growing'] <- TRUE
  
  d$is.adult <- FALSE
  d[ !is.na(d$Stage.harmonized) & (  d$Stage.harmonized == stage.adult.male | d$Stage.harmonized == stage.adult.female | d$Stage.harmonized == stage.adult.all) , 'is.adult'] <- TRUE
  
  d[d$is.growing , 'stage.cond'] <-   unique.stages.cond[1]
  d[d$is.adult , 'stage.cond'] <-   unique.stages.cond[2]
  
  
  d$is.lt.20.kg <- NA
  d[ d$bw_kg < 20 & !is.na(d$bw_kg) , 'is.lt.20.kg'] <- TRUE
  d[ d$bw_kg >= 20 & !is.na(d$bw_kg) , 'is.lt.20.kg'] <- FALSE
  
  
  
  d$ndf.level <- NA
  
  
  
  # Sheep
  d[ !is.na(d$NDF_nutrition) & d$Species == species.sheep & d$NDF_nutrition <= ndf.thresh.sheep, 'ndf.level'] <- ndf.lev.lo
  d[ !is.na(d$NDF_nutrition) & d$Species == species.sheep & d$NDF_nutrition > ndf.thresh.sheep, 'ndf.level'] <- ndf.lev.hi
  
  # Goats
  d[ !is.na(d$NDF_nutrition) & d$Species == species.goat & d$NDF_nutrition <= ndf.thresh.goat , 'ndf.level'] <- ndf.lev.lo
  d[ !is.na(d$NDF_nutrition) & d$Species == species.goat & d$NDF_nutrition > ndf.thresh.goat, 'ndf.level'] <- ndf.lev.hi
  
  
  unq.intake.units <- unique(d$feed_intake_unit)
  
  d[d$feed_intake_unit == unq.intake.units[1] , 'feed_intake_kg_d'] <- d[d$feed_intake_unit == unq.intake.units[1] , 'feed_intake_value'] 
  
  
  d <- d[d$feed_intake_unit == unq.intake.units[1] , ] 
  
  
  d$feed_intake_g_d <- d$feed_intake_kg_d * 1000
  
  d$feeding.level.g.d.kg.bw <-   d$feed_intake_g_d / d$bw_kg
  d$feeding.level.g.d.g.bw <-   d$feeding.level.g.d.kg.bw / 1000
  
  d$Sample.size <- as.numeric(d$Sample.size)
  
  
  # Lactation/gestation controls
  d$milk_prod_kg_d <- 0 
  d[d$Stage.harmonized ==  "lactating.adult.female" & !is.na(d$Stage.harmonized) , 'milk_prod_kg_d' ] <-  d[d$Stage.harmonized ==  "lactating.adult.female" & !is.na(d$Stage.harmonized) , 'milk_kg_day' ]
  d[d$Stage.harmonized !=  "lactating.adult.female" & !is.na(d$Stage.harmonized) , 'milk_prod_kg_d' ] <- 0  
  
  d$is_gestating <- 0 
  d[d$Stage.harmonized ==  "gestating.adult.female" & !is.na(d$Stage.harmonized) , 'is_gestating' ] <-  1 
  d[d$Stage.harmonized !=  "gestating.adult.female" & !is.na(d$Stage.harmonized) , 'is_gestating' ] <-  0
  
  
  d$is_lactating <- 0 
  d[d$Stage.harmonized ==  "lactating.adult.female" & !is.na(d$Stage.harmonized) , 'is_lactating' ] <-  1 
  d[d$Stage.harmonized !=  "lactating.adult.female" & !is.na(d$Stage.harmonized) , 'is_lactating' ] <-  0
    
  
  d$min.data <- FALSE
  
  d[  !is.na(d$NDF_nutrition) & !is.na(d$bw_kg) & !is.na(d$adg_g_day)   , 'min.data'] <- TRUE
  
  
  # If animal is not lactating, set milk yield to zero
  d[  d$is_lactating ==  0 & !is.na(d$is_lactating ) , 'milk_kg_day' ]  <- 0
  d[  is.na(d$is_lactating ) , 'milk_kg_day' ]  <- 0
  
  d[  d$is_lactating ==  1 & !is.na(d$is_lactating ) , 'milk_kg_day' ] 
 
  
  

  
  
  

  print(paste('Quantity of studies before outlier removal: ', length(unique(d$B.Code))))
  d <- d[ !is.na(d$feed_intake_kg_d) , ]
  d <- d[ !is.na(d$T.Animals) , ]
  #d <- d[ !is.na(d$Sample.size) , ]
  print(paste('Quantity of studies after outlier removal: ', length(unique(d$B.Code))))
  
  length(unique(d[ d$Species == species.cattle & !is.na(d$ndf.level),'B.Code']))
  length(unique(d[ d$Species == species.cattle & d$is.growing == TRUE & !is.na(d$ndf.level),'B.Code']))
  
  
  length(unique(d[ d$Species == species.sheep & d$is.growing == TRUE & d$ndf.level == ndf.lev.lo & !is.na(d$ndf.level),'B.Code']))
  length(unique(d[ d$Species == species.sheep & d$is.growing == TRUE & d$ndf.level == ndf.lev.hi & !is.na(d$ndf.level),'B.Code']))
  
  length(unique(d[ d$Species == species.goat & d$is.growing == TRUE & d$ndf.level == ndf.lev.lo & !is.na(d$ndf.level),'B.Code']))
  length(unique(d[ d$Species == species.goat & d$is.growing == TRUE & d$ndf.level == ndf.lev.hi & !is.na(d$ndf.level),'B.Code']))
  
  
  length(unique(d[ d$Species == species.sheep & d$is.adult == TRUE & d$ndf.level == ndf.lev.lo & !is.na(d$ndf.level),'B.Code']))
  length(unique(d[ d$Species == species.sheep & d$is.adult == TRUE & d$ndf.level == ndf.lev.hi & !is.na(d$ndf.level),'B.Code']))
  
  length(unique(d[ d$Species == species.goat & d$is.adult == TRUE & d$ndf.level == ndf.lev.lo & !is.na(d$ndf.level),'B.Code']))
  length(unique(d[ d$Species == species.goat & d$is.adult == TRUE & d$ndf.level == ndf.lev.hi & !is.na(d$ndf.level),'B.Code']))
  

  # Species tallies
  
  length(unique(d[ d$Species == species.sheep & d$min.data   ,'B.Code']))
  length(unique(d[ d$Species == species.sheep & d$min.data   ,'A.Level.Name']))
  
  length(unique(d[ d$Species == species.goat & d$min.data  ,'B.Code']))
  length(unique(d[ d$Species == species.goat & d$min.data  ,'A.Level.Name']))
  
  
  # Growing vs. adult
  length(unique(d[ d$Species == species.sheep & d$is.growing & d$min.data ,'B.Code']))
  length(unique(d[ d$Species == species.sheep & !d$is.growing & d$min.data ,'B.Code']))
  
  length(unique(d[ d$Species == species.goat & d$is.growing & d$min.data ,'B.Code']))
  length(unique(d[ d$Species == species.goat & !d$is.growing & d$min.data ,'B.Code']))
  
  
  
  #Adult females
  length(unique(d[ d$Species == species.sheep & d$is_lactating & d$min.data ,'B.Code']))
  length(unique(d[ d$Species == species.sheep & d$is_gestating & d$min.data ,'B.Code']))
  
  length(unique(d[ d$Species == species.goat & d$is_lactating & d$min.data ,'B.Code']))
  length(unique(d[ d$Species == species.goat & d$is_gestating & d$min.data,'B.Code']))
  
  
  sheep.lo.ndf <<- 'Sheep - Low NDF'
  sheep.hi.ndf <<- 'Sheep - High NDF'
  goat.lo.ndf <<- 'Goat - Low NDF'
  goat.hi.ndf <<- 'Goat - High NDF'
  
  d$species.ndf <- NA
  d[d$Species == species.sheep & d$ndf.level == ndf.lev.lo & !is.na(d$Species) & !is.na(d$ndf.level) , 'species.ndf'] <- sheep.lo.ndf
  d[d$Species == species.sheep & d$ndf.level == ndf.lev.hi & !is.na(d$Species) & !is.na(d$ndf.level) , 'species.ndf'] <- sheep.hi.ndf
  d[d$Species == species.goat & d$ndf.level == ndf.lev.lo & !is.na(d$Species) & !is.na(d$ndf.level) , 'species.ndf'] <- goat.lo.ndf 
  d[d$Species == species.goat & d$ndf.level == ndf.lev.hi & !is.na(d$Species) & !is.na(d$ndf.level) , 'species.ndf'] <- goat.hi.ndf 
  
  
  sheep.lo.ndf.growing <<- 'Sheep - Low NDF - Growing'
  sheep.lo.ndf.adult <<- 'Sheep - Low NDF - Adult'
  
  sheep.hi.ndf.growing <<- 'Sheep - High NDF - Growing'
  sheep.hi.ndf.adult <<- 'Sheep - High NDF - Adult'
  
  goat.lo.ndf.growing <<- 'Goat - Low NDF - Growing'
  goat.lo.ndf.adult <<- 'Goat - Low NDF - Adult'
  
  goat.hi.ndf.growing <<- 'Goat - High NDF - Growing'
  goat.hi.ndf.adult <<- 'Goat - High NDF - Adult'
  
  d$species.ndf.age <- NA
  
  d[d$Species == species.sheep & d$ndf.level == ndf.lev.lo & !is.na(d$Species) & !is.na(d$ndf.level) & !is.na(d$is.growing) & !is.na(d$is.adult) &  d$bw_kg < 20 & !is.na(d$bw_kg )  , 'species.ndf.age'] <- sheep.lo.ndf.growing
  d[d$Species == species.sheep & d$ndf.level == ndf.lev.lo & !is.na(d$Species) & !is.na(d$ndf.level) & !is.na(d$is.growing) & !is.na(d$is.adult) &  d$bw_kg >= 20 & !is.na(d$bw_kg ), 'species.ndf.age'] <- sheep.lo.ndf.adult
  
  d[d$Species == species.sheep & d$ndf.level == ndf.lev.hi & !is.na(d$Species) & !is.na(d$ndf.level) & !is.na(d$is.growing) & !is.na(d$is.adult)  &  d$bw_kg < 20 & !is.na(d$bw_kg ), 'species.ndf.age'] <- sheep.hi.ndf.growing
  d[d$Species == species.sheep & d$ndf.level == ndf.lev.hi & !is.na(d$Species) & !is.na(d$ndf.level) &!is.na(d$is.growing) & !is.na(d$is.adult)  & d$bw_kg >= 20 & !is.na(d$bw_kg ), 'species.ndf.age'] <- sheep.hi.ndf.adult
  
  d[d$Species == species.goat & d$ndf.level == ndf.lev.lo & !is.na(d$Species) & !is.na(d$ndf.level) &  !is.na(d$is.growing) & !is.na(d$is.adult) &  d$bw_kg < 20 & !is.na(d$bw_kg ), 'species.ndf.age'] <- goat.lo.ndf.growing
  d[d$Species == species.goat & d$ndf.level == ndf.lev.lo & !is.na(d$Species) & !is.na(d$ndf.level) & !is.na(d$is.growing) & !is.na(d$is.adult) & d$bw_kg >= 20 & !is.na(d$bw_kg ), 'species.ndf.age'] <- goat.lo.ndf.adult
  
  d[d$Species == species.goat & d$ndf.level == ndf.lev.hi & !is.na(d$Species) & !is.na(d$ndf.level) & !is.na(d$is.growing) & !is.na(d$is.adult) &  d$bw_kg < 20 & !is.na(d$bw_kg ), 'species.ndf.age'] <- goat.hi.ndf.growing
  d[d$Species == species.goat & d$ndf.level == ndf.lev.hi & !is.na(d$Species) & !is.na(d$ndf.level) & !is.na(d$is.growing) & !is.na(d$is.adult) & d$bw_kg >= 20 & !is.na(d$bw_kg ), 'species.ndf.age'] <- goat.hi.ndf.adult
  
  

   d[d$Species == species.sheep & d$ndf.level == ndf.lev.lo & !is.na(d$Species) & !is.na(d$ndf.level) , 'species.ndf'] <- sheep.lo.ndf
   d[d$Species == species.sheep & d$ndf.level == ndf.lev.hi & !is.na(d$Species) & !is.na(d$ndf.level) , 'species.ndf'] <- sheep.hi.ndf
  
  d[d$Species == species.goat & d$ndf.level == ndf.lev.lo & !is.na(d$Species) & !is.na(d$ndf.level) , 'species.ndf'] <- goat.lo.ndf 
  d[d$Species == species.goat & d$ndf.level == ndf.lev.hi & !is.na(d$Species) & !is.na(d$ndf.level) , 'species.ndf'] <- goat.hi.ndf 
  
  
  
  
  d <- d[d$excluded != 1, ]
  
  d <- data.frame(d)
  
}




