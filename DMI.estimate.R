

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

library('readxl')
library('ggplot2')
library('dplyr')
library('ggpubr')
library('stringi')
library('stringr')
library('lme4')
library('lmerTest')
library('MuMIn')
library('sjPlot')
library('sjmisc')
library('sjlabelled')
library(sjstats)
library('performance')
#library(partR2)
library(MASS)
library('ggh4x')


save.image('dmi.estimate.RData')
load('dmi.estimate.RData')


# Data prep
{
names(d)[2] <- 'diet.code'

for(r in 1:nrow(d)){
  
  d[r,'id'] <- (r-1)
  
  
}


convert.numeric <- c(
   'feed_intake_value'
  , 'bw_kg'
  , 'adg_g_day'
  , 'NDF_nutrition'
  ,  'ADF_nutrition'
  , 'NDF_digestibility'
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



na.value <- 'NA'

unq.spcs <- unique(d$Species)

species.cattle <- c(   unq.spcs[3], unq.spcs[9]  )
species.shoat <- c(unq.spcs[1] ,unq.spcs[2]  )
species.goat <- c(unq.spcs[2]  )
species.sheep <- c(unq.spcs[1]  )


unq.intake.units <- unique(d$feed_intake_unit)

d[d$feed_intake_unit == unq.intake.units[1] , 'feed_intake_kg_d'] <- d[d$feed_intake_unit == unq.intake.units[1] , 'feed_intake_value'] 



d <- d[d$feed_intake_unit == unq.intake.units[1] , ] 


d$feed_intake_g_d <- d$feed_intake_kg_d * 1000

d$feeding.level.g.d.kg.bw <-   d$feed_intake_g_d / d$bw_kg
d$feeding.level.g.d.g.bw <-   d$feeding.level.g.d.kg.bw / 1000

print(paste('Quantity of studies before outlier removal: ', length(unique(d$B.Code))))
d <- d[ !is.na(d$feed_intake_kg_d) , ]
d <- d[ !is.na(d$T.Animals) , ]
print(paste('Quantity of studies after outlier removal: ', length(unique(d$B.Code))))


d <- data.frame(d)

}



# Bovines
{

bovines <- d[  d$Species %in% species.cattle  , ]
  unq.brdss <- unique(bovines$Variety)
  
  
# -- Dairy
breeds.dairy <- c(
  unq.brdss[1]
  ,  unq.brdss[3]
  ,  unq.brdss[4]
  
  
  ,  unq.brdss[12]
  ,  unq.brdss[14]
  ,  unq.brdss[15]
  ,  unq.brdss[16]
  ,  unq.brdss[17]
  ,  unq.brdss[18]
  ,  unq.brdss[20]
  ,  unq.brdss[21]
  ,  unq.brdss[23]
  ,  unq.brdss[24]
  ,  unq.brdss[25]
  ,  unq.brdss[29]
  ,  unq.brdss[30]
  ,  unq.brdss[32]
  ,  unq.brdss[33]
  ,  unq.brdss[34]
  ,  unq.brdss[37]
  ,  unq.brdss[38]
  ,  unq.brdss[39]
)



dairy <- bovines[d$Variety %in% breeds.dairy , ]


# Subsetting based on nutrition

dairy <- dairy[  dairy$NDF_nutrition != na.value  , ]

dairy <- dairy[  dairy$CP_nutrition != na.value  , ]

dairy <- dairy[dairy$DM_digest != na.value  , ]




dairy <- dairy[dairy$ADF_nutrition != na.value  , ]

dairy <- dairy[dairy$NE_nutrition != na.value  , ]

dairy <- dairy[dairy$ME_nutrition != na.value  , ]


# Subsetting based on physiology
dairy <- dairy[  dairy$bw_kg != 'NA'  , ]

dairy <- dairy[  dairy$adg_g_day != 'NA'  , ]

dairy <- dairy[  dairy$milk_kg_day != 'NA'  , ]

dairy <- dairy[  dairy$Stage != 'NA'  , ]

dairy[,'B.Code']



length(unique(dairy$B.Code))
length(unique(dairy$A.Level.Name))
sum(as.numeric(dairy$T.Animals))




length(unique(dairy[   dairy$NDF_nutrition  != 'NA' , 'B.Code'])) # 18 
length(unique(dairy[   dairy$DM_digest != 'NA' , 'B.Code'])) # 18 
length(unique(dairy[   dairy$CP_nutrition != 'NA' , 'B.Code'])) # 18 


length(unique(dairy[   dairy$NE_nutrition != 'NA' , 'B.Code'])) # 18 
length(unique(dairy[   dairy$ME_nutrition != 'NA' , 'B.Code'])) # 18


length(unique(dairy[   dairy$milk_kg_day  != 'NA' , 'B.Code'])) # 18 
length(unique(dairy[   dairy$Stage != 'NA' , 'B.Code'])) # 18 
length(unique(dairy[   dairy$adg_g_day != 'NA' , 'B.Code'])) # 18 




# BOS INDCUS
breeds.indicus <- c(
  unq.brdss[1]
  ,  unq.brdss[3]
  ,  unq.brdss[4]
  
  
  ,  unq.brdss[12]
  ,  unq.brdss[14]
  ,  unq.brdss[15]
  ,  unq.brdss[16]
  ,  unq.brdss[17]
  ,  unq.brdss[18]
  ,  unq.brdss[20]
  ,  unq.brdss[21]
  ,  unq.brdss[23]
  ,  unq.brdss[24]
  ,  unq.brdss[25]
  ,  unq.brdss[30]
  ,  unq.brdss[32]
  ,  unq.brdss[37]
  ,  unq.brdss[38]
  ,  unq.brdss[39]
)



# Beef 
breeds.beef <- c(
  unq.brdss[35]
  ,  unq.brdss[36]
)



beef <- d[d$Variety %in% breeds.beef , ]



length(unique(beef$B.Code))
length(unique(beef$A.Level.Name))
sum(as.numeric(beef$T.Animals))



# Breed indicus
breeds.indicus <- unq.brdss[!(unq.brdss %in% breeds.dairy) ]
  
breeds.indicus <-
  c(
  unq.brdss[5]
  ,  unq.brdss[6]
  ,  unq.brdss[4]
  
  ,  unq.brdss[7]
  ,  unq.brdss[8]
  ,  unq.brdss[9]
  ,  unq.brdss[11]
  ,  unq.brdss[13]
  ,  unq.brdss[19]
  ,  unq.brdss[26]
  ,  unq.brdss[27]
  ,  unq.brdss[28]
)


zebu <- d[d$Variety %in% breeds.indicus , ]

View(zebu)

# SUbsetting based on nutrition
zebu <- zebu[zebu$DM_digest != na.value  , ]

zebu <- zebu[zebu$NDF_nutrition != na.value  , ]

zebu <- zebu[zebu$CP_nutrition != na.value  , ]


zebu <- zebu[zebu$ADF_nutrition != na.value  , ]

zebu <- zebu[zebu$DM_digest != na.value  , ]

zebu <- zebu[zebu$NE_nutrition != na.value  , ]

zebu <- zebu[zebu$ME_nutrition != na.value  , ]


# Subsetting based on physiology
zebu <- zebu[  zebu$bw_kg != 'NA'  , ]


zebu <- zebu[  zebu$milk_kg_day != 'NA'  , ]

zebu <- zebu[  zebu$Stage != 'NA'  , ]

zebu <- zebu[  zebu$adg_g_day != 'NA'  , ]


length(unique(zebu$B.Code))
length(unique(zebu$A.Level.Name))
sum(as.numeric(zebu$T.Animals))

}



# Goats
{
  
goats <- d[  d$Species %in% species.goat  , ]


length(unique(goats$B.Code))
length(unique(goats$diet.code))



#hist(goats.cc.f1$NDF_nutrition)
#hist(goats.cc.f1$CP_nutrition)
#hist(goats.cc.f1$EE_nutrition)
#hist(goats.cc.f1$Ash_nutrition)
#hist(goats.cc.f1$CP_nutrition)
#hist(goats.cc.f1$bw_kg)
#hist(goats.cc.f1$adg_g_day)



form.1 <- c(
  'feeding.level.g.d.kg.bw'
  ,  'bw_kg'
  ,   'CP_nutrition' 
  ,  'NDF_nutrition'
)

form.2 <- c(
  'feeding.level.g.d.kg.bw'
  ,  'bw_kg'
  , 'adg_g_day'
  ,   'CP_nutrition' 
  ,  'NDF_nutrition'

)

form.3 <- c(
  'feeding.level.g.d.kg.bw'
  ,  'bw_kg'
  , 'adg_g_day'
  ,   'CP_nutrition' 
  ,  'NDF_nutrition'
  #, 'ADF_nutrition'
  ,'Ash_nutrition'
  , 'EE_nutrition'
  #, 'DM_digest'
)



goats.cc.f1 <- goats[complete.cases(goats[,form.1]),]
goats.cc.f2 <- goats[complete.cases(goats[,form.2]),]
goats.cc.f3 <- goats[complete.cases(goats[,form.3]),]





# Outlier removal

#goats.cc.f1 <- goats.cc.f1[  !(goats.cc.f1$iet.code %in% ol.ids) , ]


#ol.ids <- goats.cc.f1[ abs(goats.cc.f1$residuals) > 2* mean(abs(goats.cc.f1$residuals)) , 'diet.code'  ]



# Normality tests
shapiro.test(goats.cc.f1$feeding.level.g.d.kg.bw)
shapiro.test(goats.cc.f1$adg_g_day)
shapiro.test(goats.cc.f1$bw_kg)
shapiro.test(goats.cc.f1$CP_nutrition)
shapiro.test(goats.cc.f1$NDF_nutrition)
shapiro.test(goats.cc.f1$EE_nutrition)
shapiro.test(goats.cc.f1$Ash_nutrition)

# Outlier removal
{
  
# Handle outliers
min.feed.intake <- 0.1
max.feed.intake <- 5
goats.cc.f1 <- goats.cc.f1[ goats.cc.f1$feed_intake_kg_d > min.feed.intake & goats.cc.f1$feed_intake_kg_d <= max.feed.intake , ]

min.adg.g.d <- -1000
max.adg.g.d <- 5000
goats.cc.f1 <- goats.cc.f1[goats.cc.f1$adg_g_day >  min.adg.g.d & goats.cc.f1$adg_g_day <= max.adg.g.d , ]

min.ndf.g.kg <- 100
max.ndf.g.kg <- 650
goats.cc.f1 <- goats.cc.f1[goats.cc.f1$NDF_nutrition >  min.ndf.g.kg & goats.cc.f1$NDF_nutrition <= max.ndf.g.kg  , ]

min.cp <- 00
max.cp <- 150
goats.cc.f1 <- goats.cc.f1[goats.cc.f1$CP_nutrition >  min.cp & goats.cc.f1$CP_nutrition <= max.cp  , ]

min.ee <- 00
max.ee <- 30
goats.cc.f1 <- goats.cc.f1[goats.cc.f1$EE_nutrition >  min.ee & goats.cc.f1$EE_nutrition <= max.ee  , ]


min.ash <- 00
max.ash <- 150
goats.cc.f1 <- goats.cc.f1[goats.cc.f1$Ash_nutrition >  min.ash & goats.cc.f1$Ash_nutrition <= max.ash  , ]
}

# Variable transform
{
goats.cc.f1$feed_intake_g_d.std <- (goats.cc.f1$feed_intake_g_d - mean(goats.cc.f1$feed_intake_g_d ))/ mean(goats.cc.f1$feed_intake_g_d) 

goats.cc.f1$feed_intake_g_d.ln <- log(goats.cc.f1$feed_intake_g_d ) 

goats$bw_kg.sqrd <- goats$bw_kg^2
goats$bw_kg.sqrt <- goats$bw_kg^.5




}


# Regression parameters
goats.cc.lo.ndf.weight.coef <- 0.5
goats.cc.hi.ndf.weight.coef  <- 0.2



ndf.thresh.lo.goat <- 300
ndf.thresh.hi.goat <- 600

goats.cc.f1.lo.ndf <- goats.cc.f1[goats.cc.f1$NDF_nutrition <= ndf.thresh.lo.goat,]
goats.cc.f1.hi.ndf <- goats.cc.f1[goats.cc.f1$NDF_nutrition >= ndf.thresh.hi.goat ,]

goats.cc.f2.lo.ndf <- goats.cc.f2[goats.cc.f2$NDF_nutrition <= ndf.thresh.lo.goat,]
goats.cc.f2.hi.ndf <- goats.cc.f2[goats.cc.f2$NDF_nutrition >= ndf.thresh.hi.goat ,]

goats.cc.f3.lo.ndf <- goats.cc.f3[goats.cc.f3$NDF_nutrition <= ndf.thresh.lo.goat,]
goats.cc.f3.hi.ndf <- goats.cc.f3[goats.cc.f3$NDF_nutrition >= ndf.thresh.hi.goat ,]


goats.cc.f1.hi.ndf <- goats.cc.f1[goats.cc.f1$NDF_nutrition <= ndf.thresh.lo.goat,]
goats.cc.f1.hi.ndf <- goats.cc.f1[goats.cc.f1$NDF_nutrition >= ndf.thresh.hi.goat ,]

goats.cc.f2.hi.ndf <- goats.cc.f2[goats.cc.f2$NDF_nutrition <= ndf.thresh.lo.goat,]
goats.cc.f2.hi.ndf <- goats.cc.f2[goats.cc.f2$NDF_nutrition >= ndf.thresh.hi.goat ,]

goats.cc.f3.hi.ndf <- goats.cc.f3[goats.cc.f3$NDF_nutrition <= ndf.thresh.lo.goat,]
goats.cc.f3.hi.ndf <- goats.cc.f3[goats.cc.f3$NDF_nutrition >= ndf.thresh.hi.goat ,]




gt.mod.1.lo.ndf <- lmer( 

  feeding.level.g.d.kg.bw ~
    
    bw_kg
  + CP_nutrition
  + NDF_nutrition
  + (
     1 
    | B.Code)
 , weights = goats.cc.lo.NDF.weight.coef *goats.cc.f1.lo.ndf$T.Animals / mean(na.omit(goats.cc.f1.lo.ndf$T.Animals))
  , data = goats.cc.f1.lo.ndf
)

r2_nakagawa(gt.mod.1.lo.ndf)
rmse(gt.mod.1.lo.ndf) / (mean(na.omit(abs(goats.cc.f1.lo.ndf$feeding.level.g.d.kg.bw ) )))



gt.mod.2.lo.ndf <- lmer( 
  
  feeding.level.g.d.kg.bw ~
    
    bw_kg
  + adg_g_day
  + CP_nutrition
  + NDF_nutrition
  + (
    1 
    | B.Code)
  , weights = goats.cc.lo.ndf.weight.coef *goats.cc.f2.lo.ndf$T.Animals / mean(na.omit(goats.cc.f2.lo.ndf$T.Animals))
  , data = goats.cc.f2.lo.ndf
)

r2_nakagawa(gt.mod.2.lo.ndf)
rmse(gt.mod.2.lo.ndf) / (mean(na.omit(abs(goats.cc.f2.lo.ndf$feeding.level.g.d.kg.bw ) )))



gt.mod.3.lo.ndf <- lmer( 
  
  feeding.level.g.d.kg.bw ~
    
    bw_kg
  + adg_g_day
  + CP_nutrition
  + NDF_nutrition
  + EE_nutrition
  + Ash_nutrition
  + (
    1 
    | B.Code)
  , weights = goats.cc.lo.NDF.weight.coef *goats.cc.f3.lo.ndf$T.Animals / mean(na.omit(goats.cc.f2.lo.NDF$T.Animals))
  , data = goats.cc.f3.lo.ndf
)

r2_nakagawa(gt.mod.3.lo.ndf)
rmse(gt.mod.3.lo.ndf) / (mean(na.omit(abs(goats.cc.f3.lo.ndf$feeding.level.g.d.kg.bw ) )))



# High NDF

gt.mod.1.hi.ndf <- lmer( 
  
  feeding.level.g.d.kg.bw ~
    
    bw_kg
  + CP_nutrition
  + NDF_nutrition
  + (
    1 
    | B.Code)
  , weights = goats.cc.hi.ndf.weight.coef *goats.cc.f1.hi.ndf$T.Animals / mean(na.omit(goats.cc.f1.hi.ndf$T.Animals))
  , data = goats.cc.f1.hi.ndf
)

r2_nakagawa(gt.mod.1.hi.ndf)
rmse(gt.mod.1.hi.ndf) / (mean(na.omit(abs(goats.cc.f1.hi.ndf$feeding.level.g.d.kg.bw ) )))



gt.mod.2.hi.ndf <- lmer( 
  
  feeding.level.g.d.kg.bw ~
    
    bw_kg
  + adg_g_day
  + CP_nutrition
  + NDF_nutrition
  + (
    1 
    | B.Code)
  , weights = goats.cc.hi.ndf.weight.coef *goats.cc.f2.hi.ndf$T.Animals / mean(na.omit(goats.cc.f2.hi.ndf$T.Animals))
  , data = goats.cc.f2.hi.ndf
)

r2_nakagawa(gt.mod.2.hi.ndf)
rmse(gt.mod.2.hi.ndf) / (mean(na.omit(abs(goats.cc.f2.hi.ndf$feeding.level.g.d.kg.bw ) )))



gt.mod.3.hi.ndf <- lmer( 
  
  feeding.level.g.d.kg.bw ~
    
    bw_kg
  + adg_g_day
  + CP_nutrition
  + NDF_nutrition
  + EE_nutrition
  + Ash_nutrition
  + (
    1 
    | B.Code)
  , weights = goats.cc.hi.ndf.weight.coef *goats.cc.f3.hi.ndf$T.Animals / mean(na.omit(goats.cc.f2.hi.ndf$T.Animals))
  , data = goats.cc.f3.hi.ndf
)

r2_nakagawa(gt.mod.3.hi.ndf)
rmse(gt.mod.3.hi.ndf) / (mean(na.omit(abs(goats.cc.f3.hi.ndf$feeding.level.g.d.kg.bw ) )))



goats.cc.f1.hi.ndf$fitted <- predict(gt.mod.1.hi.ndf )
goats.cc.f2.hi.ndf$fitted <- predict(gt.mod.2.hi.ndf )
goats.cc.f3.hi.ndf$fitted <- predict(gt.mod.3.hi.ndf )
goats.cc.f1.lo.ndf$fitted <- predict(gt.mod.1.lo.ndf )
goats.cc.f2.lo.ndf$fitted <- predict(gt.mod.2.lo.ndf )
goats.cc.f3.lo.ndf$fitted <- predict(gt.mod.3.lo.ndf )



html.file.name <- "DMI.model.summary.goat.html"


pred.labels.list <- c(
  'Intercept' 
  , 'Bodyweight (kg)'
  , 'Crude protein (g kg)'
  , 'Neutral detergent fibre (g kg)'
  , 'Average daily gain (g d)'
  , 'Ether extract (g d)'
  , 'Ash (g d)'
  
  )
#  https://www.rdocumentation.org/packages/sjPlot/versions/2.8.17/topics/tab_model

mod.sum <- tab_model(
  
  gt.mod.1.lo.ndf
  , gt.mod.2.lo.ndf
  , gt.mod.3.lo.ndf
  
  , gt.mod.1.hi.ndf
  , gt.mod.2.hi.ndf
  , gt.mod.3.hi.ndf
  
  
   , pred.labels = pred.labels.list 
   ,  dv.labels = dv.labels.list 
  , string.pred = "Coefficient"
  #  string.ci = "Conf. Int (95%)"
  # string.p = "P-Value"
  , file = html.file.name
)

mod.sum

species.goat <- 'Goat'

goats.cc.f1.lo.ndf$formula <- 'Equation 1'
goats.cc.f1.lo.ndf$ndf.level <- 'Low NDF'
goats.cc.f1.lo.ndf$species <- species.goat

goats.cc.f2.lo.ndf$formula <- 'Equation 2'
goats.cc.f2.lo.ndf$ndf.level <- 'Low NDF'
goats.cc.f2.lo.ndf$species <- species.goat

goats.cc.f3.lo.ndf$formula <- 'Equation 3'
goats.cc.f3.lo.ndf$ndf.level <- 'Low NDF'
goats.cc.f3.lo.ndf$species <- species.goat

goats.cc.f1.hi.ndf$formula <- 'Equation 1'
goats.cc.f1.hi.ndf$ndf.level <- 'High NDF'
goats.cc.f1.hi.ndf$species <- species.goat

goats.cc.f2.hi.ndf$formula <- 'Equation 2'
goats.cc.f2.hi.ndf$ndf.level <- 'High NDF'
goats.cc.f2.hi.ndf$species <- species.goat

goats.cc.f3.hi.ndf$formula <- 'Equation 3'
goats.cc.f3.hi.ndf$ndf.level <- 'High NDF'
goats.cc.f3.hi.ndf$species <- species.goat

colnames(goats.cc.f1.lo.ndf)
colnames(goats.cc.f2.lo.ndf)
colnames(goats.cc.f3.lo.ndf)
colnames(goats.cc.f1.hi.ndf)
colnames(goats.cc.f2.lo.ndf)
colnames(goats.cc.f3.lo.ndf)

gg.goat.dat.all <- rbind(
  goats.cc.f1.lo.ndf
  , goats.cc.f2.lo.ndf 
  , goats.cc.f3.lo.ndf 
  ,  goats.cc.f1.hi.ndf 
  , goats.cc.f2.hi.ndf 
  , goats.cc.f3.hi.ndf 
  )

# gg plot parameters
{
gg.gt.x.ax.tick.fs <- 11
gg.gt.y.ax.tick.fs <- 11
gg.fit.y.lab <- 'Fitted feeding level (g/day/g BW)'
gg.fit.x.lab <- 'Observed feeding level (g/day/g BW)'
gg.fit.facet.text.size <- 10.5
gg.fit.facet.border.line.thickness <- 1
}


gg.goats.compare <- ggplot( gg.goat.dat.all   ) +
  geom_point( aes( x =   feeding.level.g.d.kg.bw, y = fitted        ) , ) +
  geom_line(aes(x = feeding.level.g.d.kg.bw , y = feeding.level.g.d.kg.bw)) + 
  facet_nested( ~   species + formula + ndf.level , scales = 'fixed')  +
  xlab(gg.fit.x.lab) +
  ylab(gg.fit.y.lab) +
  theme(
  #  , plot.margin = margin(p.mrgn, p.mrgn.side, p.mrgn,p.mrgn.side, "cm")
    ,legend.title = element_blank() 
    ,legend.position = "bottom" 
    ,axis.text.y = element_text(size  = gg.gt.y.ax.tick.fs )
    ,axis.text.x = element_text(size  = gg.gt.x.ax.tick.fs )
  ,panel.grid.major = element_blank()
  ,panel.background = element_blank()
  ,panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  ,strip.background = element_rect(color='black', fill='white', size=gg.fit.facet.border.line.thickness, linetype="solid")
  ,strip.text.x = element_text(size =  gg.fit.facet.text.size , color = 'black' )
  ) 

gg.goats.compare 


}
  

# Small ruminants

s.rums <- d[  d$Species %in% species.sheep | d$Species %in% species.goat , ]


ndf.thresh.lo.sr <- 300
ndf.thresh.hi.sr <- 600

s.rums.lo.ndf  <- s.rums [s.rums $NDF_nutrition <= ndf.thresh.lo.sheep,]
s.rums.hi.ndf   <- s.rums [s.rums $NDF_nutrition > ndf.thresh.lo.sheep,]


s.rums.lo.ndf.cc.f1 <- s.rums.lo.ndf[complete.cases(s.rums.lo.ndf[,form.1]),]

# --- SHEEP
{
  
  
s.rums <- d[  d$Species %in% species.sheep | d$Species %in% species.goat , ]
  
  
ndf.thresh.lo.sr <- 300
ndf.thresh.hi.sr <- 600

sheep <- sheep[sheep$NDF_nutrition <= ndf.thresh.lo.sheep,]
sheep <- sheep[sheep$NDF_nutrition <= ndf.thresh.lo.sheep,]

form.1.shp <- form.1
form.2.shp <- form.2
form.3.shp <- form.3

sheep.cc.f1 <- sheep[complete.cases(sheep[,form.1]),]
sheep.cc.f2 <- sheep[complete.cases(sheep[,form.2]),]
sheep.cc.f3 <- sheep[complete.cases(sheep[,form.3]),]


length(unique(sheep.cc$B.Code))
length(unique(sheep.cc$diet.code))

hist(sheep.cc.f1$feed_intake_kg_d)
hist(sheep.cc.f1$feeding.level.g.d.kg.bw)


hist(sheep.cc.f1$bw_kg)
hist(sheep.cc.f1$adg_g_day)
hist(sheep.cc.f1$CP_nutrition)
hist(sheep.cc.f1$NDF_nutrition)
hist(sheep.cc.f1$Ash_nutrition)
hist(sheep.cc.f1$EE_nutrition)


# Normality tests
shapiro.test(sheep.cc.f1$feeding.level.g.d.kg.bw)
shapiro.test(sheep.cc.f1$adg_g_day)
shapiro.test(sheep.cc.f1$bw_kg)
shapiro.test(sheep.cc.f1$CP_nutrition)
shapiro.test(sheep.cc.f1$NDF_nutrition)
shapiro.test(sheep.cc.f1$EE_nutrition)
shapiro.test(sheep.cc.f1$Ash_nutrition)


ndf.thresh.lo.sheep <- 300
ndf.thresh.hi.sheep <- 600

sheep.cc.f1.lo.ndf <- sheep.cc.f1[sheep.cc.f1$NDF_nutrition <= ndf.thresh.lo.sheep,]
sheep.cc.f1.hi.ndf <- sheep.cc.f1[sheep.cc.f1$NDF_nutrition >= ndf.thresh.hi.sheep ,]

sheep.cc.f2.lo.ndf <- sheep.cc.f2[sheep.cc.f2$NDF_nutrition <= ndf.thresh.lo.sheep,]
sheep.cc.f2.hi.ndf <- sheep.cc.f2[sheep.cc.f2$NDF_nutrition >= ndf.thresh.hi.sheep ,]

sheep.cc.f3.lo.ndf <- sheep.cc.f3[sheep.cc.f3$NDF_nutrition <= ndf.thresh.lo.sheep,]
sheep.cc.f3.hi.ndf <- sheep.cc.f3[sheep.cc.f3$NDF_nutrition >= ndf.thresh.hi.sheep ,]

sheep.cc.f1.hi.ndf <- sheep.cc.f1[sheep.cc.f1$NDF_nutrition <= ndf.thresh.lo.sheep,]
sheep.cc.f1.hi.ndf <- sheep.cc.f1[sheep.cc.f1$NDF_nutrition >= ndf.thresh.hi.sheep ,]

sheep.cc.f2.hi.ndf <- sheep.cc.f2[sheep.cc.f2$NDF_nutrition <= ndf.thresh.lo.sheep,]
sheep.cc.f2.hi.ndf <- sheep.cc.f2[sheep.cc.f2$NDF_nutrition >= ndf.thresh.hi.sheep ,]

sheep.cc.f3.hi.ndf <- sheep.cc.f3[sheep.cc.f3$NDF_nutrition <= ndf.thresh.lo.sheep,]
sheep.cc.f3.hi.ndf <- sheep.cc.f3[sheep.cc.f3$NDF_nutrition >= ndf.thresh.hi.sheep ,]





# Handle outliers
{
min.feed.intake <- 0.1
max.feed.intake <- 5
sheep <- sheep[sheep$feed_intake_value > min.feed.intake & sheep$feed_intake_value <= max.feed.intake , ]



}

# Variable checks


# Multi-collinearity tests
variables.to.check <- c(
  'bw_kg'
  , 'adg_g_day'
  , 'NDF_nutrition'
  , 'CP_nutrition'
  )

for (v in variables.to.check){
  
  for (v2 in variables.to.check[ -c(which(variables.to.check == "CP_nutrition"))]  ){
  
cor <- cor(
sheep[complete.cases(sheep[,c(v, v2)]),v]
,sheep[complete.cases(sheep[,c(v , v2)]),v2]
)
  
print(paste('correlation between ' ,v ,'and ',v2,': ' ,cor ))    
    
}
}


sheep.cc.lo.ndf.weight.coef <- 0.2
sheep.cc.hi.ndf.weight.coef <- 0.2







sheep.cc.f1 <- sheep.cc.f1[  !(sheep.cc.f1$diet.code %in% ol.ids) , ]



ndf.thresh.lo <- 300
ndf.thresh.hi <- 550

sheep.cc.lo.NDF <- sheep.cc[sheep.cc$NDF_nutrition <= ndf.thresh.lo, ]
sheep.cc.hi.NDF <- sheep.cc[sheep.cc$NDF_nutrition >= ndf.thresh.hi, ]


sheep.cc.lo.NDF$adg.sqrd <- sheep.cc.lo.NDF$adg_g_day ^2

sp.mod.1.lo.NDF <- lmer( 
  feeding.level.g.d.kg.bw ~  bw_kg 
 # +  log(adg_g_day)
#  + adg_g_day
 # + adg.sqrd 
 # + log(CP_nutrition )
  +   #+ CP_nutrition *   CP_nutrition 
  #+  NDF_nutrition
 # +    log(NDF_nutrition )
 # + Ash_nutrition
 # + EE_nutrition
  + (
    1 
 #  + bw_kg 
   # + adg_g_day 
   # + CP_nutrition 
   # +  NDF_nutrition
  #  + Ash_nutrition
  #  + EE_nutrition
      | B.Code)
# , weights = .1 * sheep.cc.lo.NDF$T.Animals / (max(na.omit(sheep.cc.lo.NDF$T.Animals)) - min(na.omit(sheep.cc.lo.NDF$T.Animals)))
 , weights = .1 * sheep.cc.lo.NDF$T.Animals / (mean(na.omit(sheep.cc.lo.NDF$T.Animals)) )
 , data = sheep.cc.lo.NDF
  )

summary(sp.mod.1.lo.NDF )

r.squaredGLMM(sp.mod.1.lo.NDF)
rmse(sp.mod.1.lo.NDF) / mean((predict(sp.mod.1.lo.NDF)))


stepw <- step(  sp.mod.1.lo.NDF  
                , direction = 'backward' 
                , scope = formula(sp.mod.1.lo.NDF  )
                , steps = 1000)




sheep.cc.lo.NDF$fitted <- predict(sp.mod.1.lo.NDF)



sp.mod.1.hi.NDF.y.tfm <- 1


sp.mod.1.lo.ndf <- lmer( 
  
  feeding.level.g.d.kg.bw ~
    
    bw_kg
  + CP_nutrition
  + NDF_nutrition
  + (
    1 
    | B.Code)
  , weights = sheep.cc.lo.ndf.weight.coef *sheep.cc.f1.lo.ndf$T.Animals / mean(na.omit(sheep.cc.f1.lo.ndf$T.Animals))
  , data = sheep.cc.f1.lo.ndf
)

r2_nakagawa(sp.mod.1.lo.ndf)
rmse(sp.mod.1.lo.ndf) / (mean(na.omit(abs(sheep.cc.f1.lo.ndf$feeding.level.g.d.kg.bw ) )))


sp.mod.2.lo.ndf <- lmer( 
  
  feeding.level.g.d.kg.bw ~
    
    bw_kg
  + CP_nutrition
  + NDF_nutrition
  + adg_g_day
  + (
    1 
    | B.Code)
  , weights = sheep.cc.lo.ndf.weight.coef *sheep.cc.f2.lo.ndf$T.Animals / mean(na.omit(sheep.cc.f2.lo.ndf$T.Animals))
  , data = sheep.cc.f2.lo.ndf
)

r2_nakagawa(sp.mod.2.lo.ndf)
rmse(sp.mod.2.lo.ndf) / (mean(na.omit(abs(sheep.cc.f2.lo.ndf$feeding.level.g.d.kg.bw ) )))


sp.mod.3.lo.ndf <- lmer( 
  
  feeding.level.g.d.kg.bw ~
    
    bw_kg
  + CP_nutrition
  + NDF_nutrition
  + adg_g_day
  + EE_nutrition
  + Ash_nutrition
  + (
    1 
    | B.Code)
  , weights = sheep.cc.lo.ndf.weight.coef *sheep.cc.f3.lo.ndf$T.Animals / mean(na.omit(sheep.cc.f3.lo.ndf$T.Animals))
  , data = sheep.cc.f3.lo.ndf
)

r2_nakagawa(sp.mod.3.lo.ndf)
rmse(sp.mod.3.lo.ndf) / (mean(na.omit(abs(sheep.cc.f3.lo.ndf$feeding.level.g.d.kg.bw ) )))


# High NDF
sp.mod.1.hi.ndf <- lmer( 
  
  feeding.level.g.d.kg.bw ~
    
    bw_kg
  + CP_nutrition
  + NDF_nutrition
  + (
    1 
    | B.Code)
  , weights = sheep.cc.hi.ndf.weight.coef *sheep.cc.f1.hi.ndf$T.Animals / mean(na.omit(sheep.cc.f1.hi.ndf$T.Animals))
  , data = sheep.cc.f1.hi.ndf
)

r2_nakagawa(sp.mod.1.hi.ndf)
rmse(sp.mod.1.hi.ndf) / (mean(na.omit(abs(sheep.cc.f1.hi.ndf$feeding.level.g.d.kg.bw ) )))


sp.mod.2.hi.ndf <- lmer( 
  
  feeding.level.g.d.kg.bw ~
    
    bw_kg
  + CP_nutrition
  + NDF_nutrition
  + adg_g_day
  + (
    1 
    | B.Code)
  , weights = sheep.cc.hi.ndf.weight.coef *sheep.cc.f2.hi.ndf$T.Animals / mean(na.omit(sheep.cc.f2.hi.ndf$T.Animals))
  , data = sheep.cc.f2.hi.ndf
)

r2_nakagawa(sp.mod.2.hi.ndf)
rmse(sp.mod.2.hi.ndf) / (mean(na.omit(abs(sheep.cc.f2.hi.ndf$feeding.level.g.d.kg.bw ) )))


sp.mod.3.hi.ndf <- lmer( 
  
  feeding.level.g.d.kg.bw ~
    
    bw_kg
  + CP_nutrition
  + NDF_nutrition
  + adg_g_day
  + EE_nutrition
  + Ash_nutrition
  + (
    1 
    | B.Code)
  , weights = sheep.cc.hi.ndf.weight.coef *sheep.cc.f3.hi.ndf$T.Animals / mean(na.omit(sheep.cc.f3.hi.ndf$T.Animals))
  , data = sheep.cc.f3.hi.ndf
)

r2_nakagawa(sp.mod.3.hi.ndf)
rmse(sp.mod.3.hi.ndf) / (mean(na.omit(abs(sheep.cc.f3.hi.ndf$feeding.level.g.d.kg.bw ) )))



sheep.cc.f1 <- sheep.cc.f1[  !(sheep.cc.f1$iet.code %in% ol.ids) , ]




sheep.html.file.name <- "DMI.model.summary.sheep.html"

dv.labels.list <- c(
  
  'Eqn. 1 - Low NDF' 
  , 'Eqn. 2 - Low NDF' 
  , 'Eqn. 3 - Low NDF'
  
  , 'Eqn. 1 - High NDF' 
  , 'Eqn. 2 - High NDF' 
  , 'Eqn. 3 - High NDF'
  
  )


pred.labels.list <- c(
  'Intercept' 
  , 'Bodyweight (kg)'
  , 'Crude protein (g kg)'
  , 'Neutral detergent fibre (g kg)'
  , 'Average daily gain (g d)'
  , 'Ether extract (g d)'
  , 'Ash (g d)'
  
)
#  https://www.rdocumentation.org/packages/sjPlot/versions/2.8.17/topics/tab_model

mod.sum <- tab_model(
  
  sp.mod.1.lo.ndf
  , sp.mod.2.lo.ndf
  , sp.mod.3.lo.ndf
  
  , sp.mod.1.hi.ndf
  , sp.mod.2.hi.ndf
  , sp.mod.3.hi.ndf
  
  
  , pred.labels = pred.labels.list 
  ,  dv.labels = dv.labels.list 
  , string.pred = "Coefficient"
  #  string.ci = "Conf. Int (95%)"
  # string.p = "P-Value"
  , file = sheep.html.file.name
)

mod.sum



sheep.cc.f1.lo.ndf$fitted <- predict(sp.mod.1.lo.ndf)
sheep.cc.f2.lo.ndf$fitted <- predict(sp.mod.2.lo.ndf)
sheep.cc.f3.lo.ndf$fitted <- predict(sp.mod.3.lo.ndf)

sheep.cc.f1.hi.ndf$fitted <- predict(sp.mod.1.hi.ndf)
sheep.cc.f2.hi.ndf$fitted <- predict(sp.mod.2.hi.ndf)
sheep.cc.f3.hi.ndf$fitted <- predict(sp.mod.3.hi.ndf)


species.sheep <- 'Sheep'

sheep.cc.f1.lo.ndf$formula <- 'Equation 1'
sheep.cc.f1.lo.ndf$ndf.level <- 'Low NDF'
sheep.cc.f1.lo.ndf$species <- species.sheep

sheep.cc.f2.lo.ndf$formula <- 'Equation 2'
sheep.cc.f2.lo.ndf$ndf.level <- 'Low NDF'
sheep.cc.f2.lo.ndf$species <- species.sheep

sheep.cc.f3.lo.ndf$formula <- 'Equation 3'
sheep.cc.f3.lo.ndf$ndf.level <- 'Low NDF'
sheep.cc.f3.lo.ndf$species <- species.sheep

sheep.cc.f1.hi.ndf$formula <- 'Equation 1'
sheep.cc.f1.hi.ndf$ndf.level <- 'High NDF'
sheep.cc.f1.hi.ndf$species <- species.sheep

sheep.cc.f2.hi.ndf$formula <- 'Equation 2'
sheep.cc.f2.hi.ndf$ndf.level <- 'High NDF'
sheep.cc.f2.hi.ndf$species <- species.sheep

sheep.cc.f3.hi.ndf$formula <- 'Equation 3'
sheep.cc.f3.hi.ndf$ndf.level <- 'High NDF'
sheep.cc.f3.hi.ndf$species <- species.sheep




# Plots
gg.sheep.dat.all <- rbind(
  sheep.cc.f1.lo.ndf
  , sheep.cc.f2.lo.ndf 
  , sheep.cc.f3.lo.ndf 
  ,  sheep.cc.f1.hi.ndf 
  , sheep.cc.f2.hi.ndf 
  , sheep.cc.f3.hi.ndf 
)

unique.ndf.levels <- unique(gg.sheep.dat.all$ndf.level )
ordered.ndf.levels <- c( unique.ndf.levels[1] , unique.ndf.levels[2] )
gg.sheep.dat.all$ndf.level <- factor(gg.sheep.dat.all$ndf.level , ordered.ndf.levels)


gg.sheep.compare <- ggplot( gg.sheep.dat.all   ) +
  geom_point( aes( x =   feeding.level.g.d.kg.bw, y = fitted        ) , ) +
  geom_line(aes(x = feeding.level.g.d.kg.bw , y = feeding.level.g.d.kg.bw)) + 
  facet_nested( ~   species + formula + ndf.level , scales = 'fixed')  +
  xlab(gg.fit.x.lab) +
  ylab(gg.fit.y.lab) +
  theme(
    #  , plot.margin = margin(p.mrgn, p.mrgn.side, p.mrgn,p.mrgn.side, "cm")
    ,legend.title = element_blank() 
    ,legend.position = "bottom" 
    ,axis.text.y = element_text(size  = gg.gt.y.ax.tick.fs )
    ,axis.text.x = element_text(size  = gg.gt.x.ax.tick.fs )
    ,panel.grid.major = element_blank()
    ,panel.background = element_blank()
    ,panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
    ,strip.background = element_rect(color='black', fill='white', size=gg.fit.facet.border.line.thickness, linetype="solid")
    ,strip.text.x = element_text(size =  gg.fit.facet.text.size , color = 'black' )
  ) 

gg.sheep.compare



ol.ids <- sheep.cc.f1[ abs(sheep.cc.f1$resds) > 170 , 'diet.code'  ]






mod.sum <- tab_model(
  sp.mod.1
  #, logit.boran.ext 
#  , pred.labels = pred.labels ,
#  dv.labels = c("Zebu", "Boran"),
   , string.pred = "Coefficient"
#  string.ci = "Conf. Int (95%)"
 # string.p = "P-Value"
)

mod.sum

}
