

# source('functions.R')


threepoint <<- function(x, y, ladder=c(1, 1/2, 1/3, 0, -1/2, -1)) {
  # x and y are length-three samples from a dataset.
  dx <- diff(x)
  f <- function(parms) (diff(diff(box.cox(y, parms)) / dx))^2
  fit <- nlm(f, c(1,0))
  parms <- fit$estimate 
  lambda <- ladder[which.min(abs(parms[1] - ladder))]
  if (lambda==0) offset = 0 else {
    do <- diff(range(y))
    offset <- optimize(function(x) f(c(lambda, x)), 
                       c(max(-min(x), parms[2]-do), parms[2]+do))$minimum    
  }
  c(lambda, offset)
}



optim.tform <<- function( s.rums , variable, species , ndf.level )  {
  
  # variable <- "feed_intake_g_d" ; ndf.level <- ndf.lev.lo ; species <- 'Sheep'

  
  
  y.var <- s.rums[  s.rums$ndf.level == ndf.level & s.rums$Species == species  ,  variable ]
  y.var.log <- log(y.var )
  y.var.sqt <- sqrt(y.var )
  y.var.sqd <- (y.var * y.var)
  
  
  
  var.mm.s <- str_c(variable , suffx.mm.s)
  y.var.mm.s <- s.rums[  s.rums$ndf.level == ndf.level & s.rums$Species == species  ,  var.mm.s  ]

  var.mean.s <- str_c(variable , suffx.mean.s)
  y.var.mean.s <- s.rums[  s.rums$ndf.level == ndf.level & s.rums$Species == species  ,    var.mean.s ]
  
  
  transforms <- c('NULL' , 'log' , 'sqt' , 'sqd' , suffx.mm.s , suffx.mean.s )
  
  shap.tests <- c()
  
  shap.tests[1] <- as.numeric(shapiro.test(y.var)[1])
  shap.tests[2] <- as.numeric(shapiro.test(y.var.log)[1])
  shap.tests[3] <- as.numeric(shapiro.test(y.var.sqt)[1])
  shap.tests[4] <- as.numeric(shapiro.test(y.var.sqd)[1])
  
  shap.tests[5] <- as.numeric(shapiro.test(  y.var.mm.s )[1])
  shap.tests[6] <- as.numeric(shapiro.test( y.var.mean.s)[1])
  
  
  best.tform <- which( max(shap.tests) == shap.tests )
  
  


if( transforms[best.tform[1]] == transforms[1] ){  var.name <- variable } else{
var.name <- str_c(variable , '.' , transforms[best.tform])
}


  return (list(  transforms[best.tform] , var.name , shap.tests))
  
}



model.sum <<- function( d.frame , c.sp, c.ndf , c.stage){

#d.frame <- stepw.out.adt.sheep.hi.NDF ; c.sp <- species.sheep ; c.ndf <- ndf.lev.hi ; c.stage <- stage.growing.all
#d.frame <- stepw.out.gr.goats.lo.NDF   ; c.sp <- species.goat ; c.ndf <- ndf.lev.lo ; c.stage <- stage.growing.all
 
#d.frame <- stepw.out.gr.sheep.hi.NDF   ; c.sp <- species.sheep ; c.ndf <- ndf.lev.hi ; c.stage <- stage.growing.all
  
  
  
  
if (c.sp == species.sheep & c.ndf == ndf.lev.lo & c.stage == stage.growing.all) { d.frame <- stepw.out.gr.sheep.lo.NDF }
if (c.sp == species.sheep & c.ndf == ndf.lev.hi & c.stage == stage.growing.all) { d.frame <- stepw.out.gr.sheep.hi.NDF }
if (c.sp == species.goat & c.ndf == ndf.lev.lo & c.stage == stage.growing.all) { d.frame <- stepw.out.gr.goats.lo.NDF }
if (c.sp == species.goat & c.ndf == ndf.lev.hi & c.stage == stage.growing.all) { d.frame <- stepw.out.gr.goats.hi.NDF }

if (c.sp == species.sheep & c.ndf == ndf.lev.lo & c.stage == stage.adult.all) { d.frame <- stepw.out.adt.sheep.lo.NDF }
if (c.sp == species.sheep & c.ndf == ndf.lev.hi & c.stage == stage.adult.all) { d.frame <- stepw.out.adt.sheep.hi.NDF }
if (c.sp == species.goat & c.ndf == ndf.lev.lo & c.stage == stage.adult.all) { d.frame <- stepw.out.adt.goats.lo.NDF }
if (c.sp == species.goat & c.ndf == ndf.lev.hi & c.stage == stage.adult.all) { d.frame <- stepw.out.adt.goats.hi.NDF }

  

# Selection criteria 1  - normalized variables
n.var.cond <- (d.frame$normalized.vars & !is.na(d.frame$normalized.vars ))
min.aic.n.cond <-  min(d.frame[n.var.cond,'aic'])
min.bic.n.cond   <-  min(d.frame[n.var.cond,'bic'])
min.rmse.n.cond   <-  min(d.frame[n.var.cond,'rmse'])
max.r2.all.fx.n.cond   <-  max(d.frame[n.var.cond,'r2.cnd'])
max.r2.ffx.n.cond  <-  max(d.frame[n.var.cond,'r2.mrg'])  

rows.min.aic.n.cond <- which ( min.aic.n.cond == d.frame[,'aic'] )
rows.min.bic.n.cond<- which ( min.bic.n.cond == d.frame[,'bic'] )
rows.min.rmse.n.cond <- which ( min.rmse.n.cond == d.frame[,'rmse'] )
rows.max.r2.all.fx.n.cond  <- which ( max.r2.all.fx.n.cond == d.frame[,'r2.cnd'] )
rows.max.r2.ffx.n.cond <- which ( max.r2.ffx.n.cond == d.frame[,'r2.mrg'] )


#best.row.glob.2 <- rows.min.aic.glob 

  
# Selection criteria 2 -- optimal R2 (fixed effects)
min.aic.glob <-  min(d.frame[,'aic'])
min.bic.glob  <-  min(d.frame[,'bic'])
min.rmse.glob  <-  min(d.frame[,'rmse'])
max.r2.all.fx.glob  <-  max(d.frame[,'r2.cnd'])
max.r2.ffx.glob  <-  max(d.frame[,'r2.mrg'])  

rows.min.aic.glob <- which ( min.aic.glob == d.frame[,'aic'] )
rows.min.bic.glob <- which ( min.bic.glob == d.frame[,'bic'] )
rows.min.rmse.glob <- which ( min.rmse.glob == d.frame[,'rmse'] )
rows.max.r2.all.fx.glob  <- which ( max.r2.all.fx.glob == d.frame[,'r2.cnd'] )
rows.max.r2.ffx.glob <- which ( max.r2.ffx.glob == d.frame[,'r2.mrg'] )

# Selection criteria 3 -- best AIC
min.aic <-  min(d.frame[,'aic'])

rows.selc.2 <- which ( min.aic == d.frame[,'aic'] )


best.row.glob.2 <- rows.min.aic.glob 


if (selc.crit == selc.aic) { best.row.n.cond <- rows.min.aic.n.cond ; best.row.glob <- rows.min.aic.glob  }
if (selc.crit == selc.bic) {best.row.n.cond <- rows.min.bic.n.cond ; best.row.glob <- rows.min.bic.glob  }
if (selc.crit == selc.rmse) { best.row.n.cond <- rows.min.rmse.n.cond ; best.row.glob <- rows.min.rmse.glob   }
if (selc.crit == selc.r2.ffx ) { best.row.n.cond <- rows.max.r2.ffx.n.cond ; best.row.glob <- rows.max.r2.ffx.glob }





if (c.sp == species.sheep & c.ndf == ndf.lev.lo & c.stage == stage.growing.all) { 
  
best.model.cr.1.gr.sheep.lo.ndf.glob <<- d.frame[ best.row.glob , 'model'][[1]] ;  
best.model.cr.1.gr.sheep.lo.ndf.glob.2 <<- d.frame[ best.row.glob.2 , 'model'][[1]] ;  
best.model.cr.1.gr.sheep.lo.ndf.n.cond <<- d.frame[ best.row.n.cond , 'model'][[1]]  ; 
reg.inc.cond.gr.sheep.lo.ndf.glob.otpm <<- d.frame[best.row.glob , 'inc.cond'][[1]]  ; 
reg.inc.cond.gr.sheep.lo.ndf.n.cond <<- d.frame[best.row.n.cond , 'inc.cond'][[1]]  ; 
best.model.cr.1.gr.sheep.lo.ndf.n.cond.y.var <<- d.frame[ best.row.n.cond ,'y.var'] ; 
best.model.cr.1.gr.sheep.lo.ndf.glob.y.var <<- d.frame[ best.row.glob,'y.var']



}





if (c.sp == species.sheep & c.ndf == ndf.lev.hi & c.stage == stage.growing.all) { best.model.cr.1.gr.sheep.hi.ndf.glob <<- d.frame[ best.row.glob , 'model'][[1]] ;   best.model.cr.1.gr.sheep.hi.ndf.glob.2 <<- d.frame[ best.row.glob.2 , 'model'][[1]] ; best.model.cr.1.gr.sheep.hi.ndf.n.cond <<- d.frame[ best.row.n.cond , 'model'][[1]]    ; reg.inc.cond.gr.sheep.hi.ndf.glob.otpm <<- d.frame[best.row.glob , 'inc.cond'][[1]]  ; reg.inc.cond.gr.sheep.hi.ndf.n.cond <<- d.frame[best.row.n.cond , 'inc.cond'][[1]]  ; best.model.cr.1.gr.sheep.hi.ndf.n.cond.y.var <<- d.frame[ best.row.n.cond ,'y.var'] ; best.model.cr.1.gr.sheep.hi.ndf.glob.y.var <<- d.frame[ best.row.glob,'y.var'] }
if (c.sp == species.goat & c.ndf == ndf.lev.lo & c.stage == stage.growing.all) { best.model.cr.1.gr.goats.lo.ndf.glob <<- d.frame[ best.row.glob , 'model'][[1]] ;   best.model.cr.1.gr.goats.lo.ndf.glob.2 <<- d.frame[ best.row.glob.2 , 'model'][[1]] ; best.model.cr.1.gr.goats.lo.ndf.n.cond <<- d.frame[ best.row.n.cond , 'model'][[1]]    ; reg.inc.cond.gr.goats.lo.ndf.glob.otpm <<- d.frame[best.row.glob , 'inc.cond'][[1]] ; reg.inc.cond.gr.goats.lo.ndf.n.cond <<- d.frame[best.row.n.cond , 'inc.cond'][[1]] ; best.model.cr.1.gr.goats.lo.ndf.n.cond.y.var <<- d.frame[ best.row.n.cond ,'y.var'] ; best.model.cr.1.gr.goats.lo.ndf.glob.y.var <<- d.frame[ best.row.glob,'y.var'] }
if (c.sp == species.goat & c.ndf == ndf.lev.hi & c.stage == stage.growing.all) { best.model.cr.1.gr.goats.hi.ndf.glob <<- d.frame[ best.row.glob , 'model'][[1]] ;   best.model.cr.1.gr.goats.hi.ndf.glob.2 <<- d.frame[ best.row.glob.2 , 'model'][[1]] ; best.model.cr.1.gr.goats.hi.ndf.n.cond <<- d.frame[ best.row.n.cond , 'model'][[1]]     ; reg.inc.cond.gr.goats.hi.ndf.glob.otpm <<- d.frame[best.row.glob, 'inc.cond'][[1]]  ; reg.inc.cond.gr.goats.hi.ndf.n.cond <<- d.frame[best.row.n.cond , 'inc.cond'][[1]] ; best.model.cr.1.gr.goats.hi.ndf.n.cond.y.var <<- d.frame[ best.row.n.cond ,'y.var'] ; best.model.cr.1.gr.goats.hi.ndf.glob.y.var <<- d.frame[ best.row.glob,'y.var'] }

if (c.sp == species.sheep & c.ndf == ndf.lev.lo & c.stage == stage.adult.all) { best.model.cr.1.adt.sheep.lo.ndf.glob <<- d.frame[ best.row.glob , 'model'][[1]] ;  best.model.cr.1.adt.sheep.lo.ndf.glob.2 <<- d.frame[ best.row.glob.2 , 'model'][[1]] ;  best.model.cr.1.adt.sheep.lo.ndf.n.cond <<- d.frame[ best.row.n.cond , 'model'][[1]]  ; reg.inc.cond.adt.sheep.lo.ndf.glob.otpm <<- d.frame[best.row.glob , 'inc.cond'][[1]]  ; reg.inc.cond.adt.sheep.lo.ndf.n.cond <<- d.frame[best.row.n.cond , 'inc.cond'][[1]]  ; best.model.cr.1.adt.sheep.lo.ndf.n.cond.y.var <<- d.frame[ best.row.n.cond ,'y.var'] ; best.model.cr.1.adt.sheep.lo.ndf.glob.y.var <<- d.frame[ best.row.glob,'y.var'] }
if (c.sp == species.sheep & c.ndf == ndf.lev.hi & c.stage == stage.adult.all) { best.model.cr.1.adt.sheep.hi.ndf.glob <<- d.frame[ best.row.glob , 'model'][[1]] ;  best.model.cr.1.adt.sheep.hi.ndf.glob.2 <<- d.frame[ best.row.glob.2 , 'model'][[1]] ; best.model.cr.1.adt.sheep.hi.ndf.n.cond <<- d.frame[ best.row.n.cond , 'model'][[1]]    ; reg.inc.cond.adt.sheep.hi.ndf.glob.otpm <<- d.frame[best.row.glob , 'inc.cond'][[1]]  ; reg.inc.cond.adt.sheep.hi.ndf.n.cond <<- d.frame[best.row.n.cond , 'inc.cond'][[1]]  ; best.model.cr.1.adt.sheep.hi.ndf.n.cond.y.var <<- d.frame[ best.row.n.cond ,'y.var'] ; best.model.cr.1.adt.sheep.hi.ndf.glob.y.var <<- d.frame[ best.row.glob,'y.var'] }
if (c.sp == species.goat & c.ndf == ndf.lev.lo & c.stage == stage.adult.all) { best.model.cr.1.adt.goats.lo.ndf.glob <<- d.frame[ best.row.glob , 'model'][[1]] ;  best.model.cr.1.adt.goats.lo.ndf.glob.2 <<- d.frame[ best.row.glob.2 , 'model'][[1]] ; best.model.cr.1.adt.goats.lo.ndf.n.cond <<- d.frame[ best.row.n.cond , 'model'][[1]]    ; reg.inc.cond.adt.goats.lo.ndf.glob.otpm <<- d.frame[best.row.glob , 'inc.cond'][[1]] ; reg.inc.cond.adt.goats.lo.ndf.n.cond <<- d.frame[best.row.n.cond , 'inc.cond'][[1]] ; best.model.cr.1.adt.goats.lo.ndf.n.cond.y.var <<- d.frame[ best.row.n.cond ,'y.var'] ; best.model.cr.1.adt.goats.lo.ndf.glob.y.var <<- d.frame[ best.row.glob,'y.var'] }
if (c.sp == species.goat & c.ndf == ndf.lev.hi & c.stage == stage.adult.all) { best.model.cr.1.adt.goats.hi.ndf.glob <<- d.frame[ best.row.glob , 'model'][[1]] ;  best.model.cr.1.adt.goats.hi.ndf.glob.2 <<- d.frame[ best.row.glob.2 , 'model'][[1]] ;  best.model.cr.1.adt.goats.hi.ndf.n.cond <<- d.frame[ best.row.n.cond , 'model'][[1]]     ; reg.inc.cond.adt.goats.hi.ndf.glob.otpm <<- d.frame[best.row.glob, 'inc.cond'][[1]]  ; reg.inc.cond.adt.goats.hi.ndf.n.cond <<- d.frame[best.row.n.cond , 'inc.cond'][[1]] ; best.model.cr.1.adt.goats.hi.ndf.n.cond.y.var <<- d.frame[ best.row.n.cond ,'y.var'] ; best.model.cr.1.adt.goats.hi.ndf.glob.y.var <<- d.frame[ best.row.glob,'y.var'] }





# Obtain IDs of observations with largest residuals, in descending order
model <- d.frame[ best.row.glob.2  , 'model'][[1]]

residuals <- resid(model)

# First best
F1.b.resid <- max(abs(residuals))
residuals.m.1.b <- residuals[-which(abs(residuals) == F1.b.resid )]
first.max.id <- as.numeric(names(residuals[which(abs(residuals) == F1.b.resid )]))

# Second best
F2.b.resid <- max(abs(residuals.m.1.b ))
residuals.m.2.b <- residuals.m.1.b[-which(abs(residuals.m.1.b) == F2.b.resid  )]
second.max.id <- as.numeric(names(residuals.m.1.b[which(abs(residuals.m.1.b) == F2.b.resid)]))

# Third best
F3.b.resid <- max(abs(residuals.m.2.b ))
residuals.m.3.b <- residuals.m.2.b[-which(abs(residuals.m.2.b) == F3.b.resid  )]
third.max.id <- as.numeric(names(residuals.m.2.b[which(abs(residuals.m.2.b) == F3.b.resid)]))

 
first.max.outlier.id <- s.rums.cc[s.rums.cc$id == first.max.id & !is.na(s.rums.cc$id), 'B.Code']
second.max.outlier.id <- s.rums.cc[s.rums.cc$id == second.max.id & !is.na(s.rums.cc$id), 'B.Code']
third.max.outlier.id <- s.rums.cc[s.rums.cc$id == third.max.id  & !is.na(s.rums.cc$id), 'B.Code']


# SHEEP - LOW NDF
if (c.sp == species.sheep & c.ndf == ndf.lev.lo & c.stage == stage.growing.all) { 
  
gr.sheep.lo.ndf.max.resd.id.1 <<- first.max.outlier.id  
gr.sheep.lo.ndf.max.resd.id.2 <<- second.max.outlier.id
gr.sheep.lo.ndf.max.resd.id.3 <<- third.max.outlier.id

# remove outliers only if the boolean is set TRUE
if (ol.rm.gr.sheep.lo.ndf) {

outlier.b.codes <<- c( gr.sheep.lo.ndf.max.resd.id.1  )
outlier.b.codes <<- append(  outlier.b.codes  , gr.sheep.lo.ndf.max.resd.id.2 )
outlier.b.codes <<- append(  outlier.b.codes  , gr.sheep.lo.ndf.max.resd.id.3 )

source('outlier.removal.R')

}
  
}

# SHEEP - HIGH NDF
if (c.sp == species.sheep & c.ndf == ndf.lev.hi & c.stage == stage.growing.all) { 
  
  gr.sheep.hi.ndf.max.resd.id.1 <<- first.max.outlier.id  
  gr.sheep.hi.ndf.max.resd.id.2 <<- second.max.outlier.id
  gr.sheep.hi.ndf.max.resd.id.3 <<- third.max.outlier.id
  
  
  # remove outliers only if the boolean is set TRUE
  if (ol.rm.gr.sheep.hi.ndf) {
  
  outlier.b.codes <<- c( gr.sheep.hi.ndf.max.resd.id.1)
  outlier.b.codes <<- append(  outlier.b.codes  , gr.sheep.hi.ndf.max.resd.id.2 )
  outlier.b.codes <<- append(  outlier.b.codes  , gr.sheep.hi.ndf.max.resd.id.3 )
  
  source('outlier.removal.R')
  
  }
  
}

# GOATS - LOW NDF
if (c.sp == species.goat & c.ndf == ndf.lev.lo & c.stage == stage.growing.all) { 
  
  gr.goat.lo.ndf.max.resd.id.1 <<- first.max.outlier.id  
  gr.goat.lo.ndf.max.resd.id.2 <<- second.max.outlier.id
  gr.goat.lo.ndf.max.resd.id.3 <<- third.max.outlier.id
  
  
  # remove outliers only if the boolean is set TRUE
  if (ol.rm.gr.goat.lo.ndf) {
    
  outlier.b.codes <<- c( gr.goat.lo.ndf.max.resd.id.1  )
  outlier.b.codes <<- append(  outlier.b.codes  , gr.goat.lo.ndf.max.resd.id.2 )
  outlier.b.codes <<- append(  outlier.b.codes  , gr.goat.lo.ndf.max.resd.id.3 )
  
  source('outlier.removal.R')
  
  }
  
}

# GOATS - HIGH NDF
if (c.sp == species.goat & c.ndf == ndf.lev.hi & c.stage == stage.growing.all) { 
  
  gr.goat.hi.ndf.max.resd.id.1 <<- first.max.outlier.id  
  gr.goat.hi.ndf.max.resd.id.2 <<- second.max.outlier.id
  gr.goat.hi.ndf.max.resd.id.3 <<- third.max.outlier.id
  
  
  # remove outliers only if the boolean is set TRUE
  if (ol.rm.gr.goat.hi.ndf) {
    
  outlier.b.codes <<- c( gr.goat.hi.ndf.max.resd.id.1)
  outlier.b.codes <<- append(  outlier.b.codes  , gr.goat.hi.ndf.max.resd.id.2 )
  outlier.b.codes <<- append(  outlier.b.codes  , gr.goat.hi.ndf.max.resd.id.3 )
  
  source('outlier.removal.R')
  
  }
}

}


listify.coef.names <<- function(model){
  
  names <- names(coef(model)[1])
  
  names <- c(names ,    names(coef(model)[2])   )
  names <- c(names ,    names(coef(model)[3])   )
  names <- c(names ,    names(coef(model)[4])   )
  names <- c(names ,    names(coef(model)[5])   )
  names <- c(names ,    names(coef(model)[6])   )
  
  return(names)
  
}


predict.manual <<- function(form , dat , offset , data.set.type , row ){
  
  # test: form <- formula ; offset <-  ws.offset.coef ; dat <- d ; data.set.type <- 'test' ; row <- 1

  # From whole sample ; form <- formula ; dat <- all.data ; offset <- ws.offset.coef ; data.set.type <- 'whole' ; row <- 1 
  
  
  test.ds <- function(){
    
    form <- formula  
    dat <- test.data 
    offset <- model.offset 
    data.set.type <- 'test' 
    row <- r 
    
  }
  
  
  test.test <- function(){
    
    form <- formula 
    offset <-  model.offset 
    dat <- test.data 
    data.set.type <- 'test' 
    row <- 1
    
    
    
  }
 
  x.vars <- de.listify(return.x.vars(form , data.set.type , row )[1])[[1]]
  x.coefs <- de.listify(return.x.vars(form , data.set.type  , row)[2])[[1]]
  

  predicted <- (x.coefs[1] +
  x.coefs[2] * dat[, x.vars[1]  ] ) 
  
  if (   !is.na(x.coefs[3]) ){ predicted <- predicted +  x.coefs[3] * dat[, x.vars[2] ]  }
  if (   !is.na(x.coefs[4]) ){ predicted <- predicted +  x.coefs[4] * dat[, x.vars[3] ]}
  if (   !is.na(x.coefs[5]) ){ predicted <- predicted +  x.coefs[5] * dat[, x.vars[4] ]}
  if (   !is.na(x.coefs[6]) ){ predicted <- predicted +  x.coefs[6] * dat[, x.vars[5] ]}
  if (   !is.na(x.coefs[7]) ){ predicted <- predicted +  x.coefs[7] * dat[, x.vars[6] ]}
  
  return (predicted)
  
}


listify <<- function( d ){

  
  return (  list(list(d)) )
  
}

de.listify <<- function( d ){
  
  return (  d[[1]] )
  
}

gen.weights <- function( ids , ids.exmt ){
    
 # ids.exmt
   # ids 
    
    experiments <- s.rums[  s.rums$ut.id %in%  ids , 'ue.id'  ]
    unique.experiments <- unique(experiments)
      
      
    ws.standard.deviations <- c()
    
    for (  e in 1 : length( experiments )   ){  
      
      e.id <- experiments[e]
      
      ws.standard.deviations[e] <- sd(   s.rums[  s.rums$ue.id ==   e.id   , 'feed_intake_g_d'  ]   )
   
       }
    
    ws.variances <- ws.standard.deviations^2
    
    bs.standard.deviation <- sd(ws.standard.deviations)
    bs.standard.variance <- bs.standard.deviation^2

    
    weights <- 1 / (ws.variances + bs.standard.variance )
    
   weights.stdz <- (weights - min(weights))  / ( max(weights) - min(weights))
  
    return ( weights.stdz )
    
 }
  
    
gen.eval.metrics <- function( m , form ,d , y  , t.ids , e.ids  ,offset , data.set.type , row){
  
test.criteria <- function(){
  
  row <- 1
   m <- model
   form <- formula
   d <- train.data
   y <-  train.data[,y.var] 
   t.ids <- train.ids
   e.ids <- train.ids.exmt 
   offset <- model.offset
   
}


if ( !random.exp.int ) { pred <-  predict( m , newdata = d  )  }
if (  random.exp.int ) { pred <-  predict.manual( form , d , offset , data.set.type , row )   }


form.lhs <- as.character(form[[2]])
observed.tformd <-  d[,  form.lhs  ] 
  
  rsds <- observed.tformd  -  pred
  sq.rdsd <- rsds ^ 2
  msr <- mean( sq.rdsd )
  rmse <- sqrt(msr  )
  nrmse <- 100 * rmse / mean(observed.tformd)
  
  # R2
  rss <- sum( sq.rdsd )
  tss <- sum( (y - mean(y))^2 )
  r2 <- 1 -  rss/ tss
  

  k <- length(labels(terms(form)))
  n <- nrow(train.data)
  
  r2.adj <- 1 - (1 - r2)*(n - 1) / (n - k - 1)
  
  
  # WEIGHTED PERFORMANCE METRICS
  weights <- gen.weights( t.ids , e.ids  ) 
  
  w.sq.rdsd  <- sq.rdsd * weights 
  
  sum.w.sq.rds <- sum(w.sq.rdsd  )
  mean.w.sq.rds <- sum.w.sq.rds / sum( weights )
  sqr.mean.w.sq.rds <- sqrt(  mean.w.sq.rds)
  
  w.nrmse <- 100 * sqr.mean.w.sq.rds / mean(observed.tformd)
  

  # R2
  w.rss <- sum( weights * sq.rdsd)
  w.tss <- sum( weights * (y - mean(y))^2 )
  w.r2 <- 1 - w.rss / w.tss
  
  w.r2.adj <- 1 - (1 - w.r2)*(n - 1) / (n - k - 1)
  
  return.item <- c(  listify( r2.adj ) ,  listify(nrmse )  , listify( w.r2.adj ) ,  listify(w.nrmse )  )
  
  return ( return.item   )
  
}



gen.reg.model <<- function(  data , form   , mod.v      ){
  
  test <- function(){
    
    form <- formula
    data <- all.data
    mod.v <-  d.gbr[r, 'mod.vers'] 
    
  }
  
  cur.type.index <- as.numeric(substr(mod.v , 1 , 1))
  cur.family.index <- as.numeric(substr(mod.v , 3 , 3))
  cur.mstop.index <-  as.numeric( substr(mod.v , 5 , 5) )
  cur.nu.index <- as.numeric(  substr(mod.v , 7 , 7))
  
  family.list <- c(   1  ,  2  ) ;   mod.fam.gaussian <- 1 ; mod.fam.laplace <- 2
  

  mstop.list <- seq( mstop.min , mstop.max , by =  m.stop.range /    n.mod.v.boost.control.mstop )
  nu.list <- seq( nu.min , nu.max  , by =  nu.range /      n.mod.v.boost.control.nu  )
  
  family <- de.listify(ml.boost.families[  cur.family.index ])
  mstop <-  mstop.list[cur.mstop.index]
  nu <- nu.list[cur.nu.index]
  

  
if (  cur.type.index == 1  ){
    
    
  mod.0 <- glmboost( 
    
    form  
    
    , data =  data
    
    , family =    family
   # ,  control = boost_control(mstop =   mstop , nu = nu)
    , center = FALSE
    
  )
  
  


  if (   m.stop.cv   ){
    
 # cv_folds <- cv(model.weights(mod.0 ), type = "kfold", k = 5)
  
   cvm <- cvrisk(mod.0 , grid = cv.risk.min.grid :cv.risk.max.grid  )
   mstop <- mstop(cvm)
   #mstop.opt <- mstop
   
  }
  
  x.vars <- xtract.x.vars(  form )
  cols.2.include <- c( x.vars )
  
  data$ue.id <- factor(  data$ue.id )
  
if (  1 ==  1){ 
  
  model <- glmboost( 
  
    form  
    , data =  data
    
   # , family =  family
   # ,  control = boost_control(mstop =   mstop, nu = nu)
   # , center = FALSE
    
  )
  
  

  # summary(  model )
  
  
  } else if (  family == mod.fam.laplace  ) { 
    

    model <- glmboost( 
      
      form  
      
      , data =  data
      
      , family = Gaussian()
      ,  control = boost_control(mstop =   mstop, nu = nu)
      , center = FALSE
      
    )
    
   # model <-  gbm(
    #  form  , 
     # data = data[,cols.2.include]  , 
     # distribution = "gaussian"
    #  , bag.fraction = 1
     # , n.minobsinnode = 1
      #  cv.folds = 5,         # Perform 10-fold cross-validation
      # shrinkage = .01,       # Learning rate
      # n.minobsinnode = 10,   # Minimum observations in a terminal node
      #  n.trees = 500          # Number of trees (boosting iterations)
   #@ )
    
    #  model <- xgboost(
    # data = data[,cols.2.include] 
    #, formula =     form 
    #, label = data[,  y.var  ]
    # , max.depth = 3
    # , eta = 1
     # , nthread = 2
     # , nrounds = 2
    # , objective = "reg:squarederror"
    #  )
 
 
    # xgb.importance(feature_names = cols.2.include, model = model)
    
  
  }
  
  
  }  # Gradient boosting regression using mlboost
  
if (  cur.type.index == 2){
    

    if (  1 ==  1){ 
      
      model <- lmer(
        form  
        
      )
      
      model <- glmboost( 
        
        form  
        
        , data =  data
        
        , family =  family
        ,  control = boost_control(mstop =   mstop, nu = nu)
        , center = FALSE
        
      )
      
      
      
      # summary(  model )
      
      
    } else if (  family == mod.fam.laplace  ) { 
      
      
      model <- glmboost( 
        
        form  
        
        , data =  data
        
        , family = Gaussian()
        ,  control = boost_control(mstop =   mstop, nu = nu)
        , center = FALSE
        
      )
      
      # model <-  gbm(
      #  form  , 
      # data = data[,cols.2.include]  , 
      # distribution = "gaussian"
      #  , bag.fraction = 1
      # , n.minobsinnode = 1
      #  cv.folds = 5,         # Perform 10-fold cross-validation
      # shrinkage = .01,       # Learning rate
      # n.minobsinnode = 10,   # Minimum observations in a terminal node
      #  n.trees = 500          # Number of trees (boosting iterations)
      #@ )
      
      #  model <- xgboost(
      # data = data[,cols.2.include] 
      #, formula =     form 
      #, label = data[,  y.var  ]
      # , max.depth = 3
      # , eta = 1
      # , nthread = 2
      # , nrounds = 2
      # , objective = "reg:squarederror"
      #  )
      
      
      # xgb.importance(feature_names = cols.2.include, model = model)
      
      
    }
    
    
  }  # Other type
  
  

  return.item <- c( listify(model) ,  mstop ,   nu )
 
  return ( return.item )
   
}


ols.status <<- function( row.cond ){
  
test <- function(){
  
  row.cond <- cnd.sp.lo.ndf
  
}
  
  lin.mod <- lm( feed_intake_g_d ~ bw_kg + adg_g_day + NDF_nutrition   , data = s.rums[ row.cond  ,]  )
  
  predicted <- predict(lin.mod   , data = s.rums[  row.cond  ,] )
  actual <- s.rums[  row.cond  , 'feed_intake_g_d']
  
  
  resds <- predicted -   actual
  resds.sqd <-  resds ^ 2

  msr <- mean(  resds.sqd)
  
  s.rums[  row.cond  , ol.status.var.name ] <- resds.sqd / msr
  
 #summary(  s.rums[  row.cond  , ol.status.var.name] )
  
  
  return (s.rums)
}


gen.formula.label.1r <<- function(  form.obj ){
  
  function(){
    
    form.obj <- mod.1.sp.lo.ndf 
    
  }
  

  form.obj <- Reduce(paste, deparse(  form.obj ))
  
  form.obj <- as.character(  form.obj  ) 
  
  form.list <- strsplit(  form.obj , " ")[[1]]
  
  form.list <-  form.list[ which(form.list != '')]
  
  rhs.c.1 <-  form.list[3]
  rhs.c.2 <- form.list[5]
  rhs.c.3  <- form.list[7]
  rhs.c.4  <- form.list[9]
  rhs.c.5  <- form.list[11]
  
  
  # alternatove
  
  begin <- 'DMI = \u0192( '
  end <- ' )'
  comma <- ', '
  
  
  b1 <- gen.var.alias(  all.x.vars[ which( all.x.vars ==  rhs.c.1 )  ]  )
  b2 <- gen.var.alias( all.x.vars[ which( all.x.vars ==  rhs.c.2 )  ] )
  b3 <- gen.var.alias( all.x.vars[ which( all.x.vars ==  rhs.c.3 )  ] )
  
  if (  !is.na(rhs.c.4) ){ b4 <- gen.var.alias(  all.x.vars[ which(  all.x.vars ==  rhs.c.4 )  ] )  }
  if (  !is.na(rhs.c.5) ){ b5 <- gen.var.alias(  all.x.vars[ which(  all.x.vars ==  rhs.c.5 )  ] )  }
  

  lab <- paste0(  begin,   b1 , comma , b2 , comma , b3 )
  
  if (  is.na(rhs.c.4) ){ lab <- paste0( lab ) } 
  if (   !is.na(rhs.c.4)  ){  lab <- paste0(  lab , comma , b4 )    }
  if (   !is.na(rhs.c.5)  ){  lab <- paste0(  lab , comma , b5 )    }

  lab <- paste0(  lab , end )
  
  return.obj <-   lab  #listify( c(  lab.rhs.r.1 ,   lab.rhs.r.2 ,   lab.rhs.r.3 ) )
  
  return( return.obj )
}


return.x.vars <- function(  form.obj , data.set.type ,r ){
  
  # test: form.obj <-  form ; data.set.type <- 'test'
  # test: whole sampple ; form.obj <-  form ; data.set.type <- 'whole' ; r <- 1
  

  
  form.obj <- Reduce(paste, deparse(  form.obj ))

  form.obj <- as.character(  form.obj  ) 
  
  form.list <- strsplit(  form.obj , " ")[[1]]
  
  form.list <-  form.list[ which(form.list != '')]
  
  rhs.c.1 <-  form.list[3]
  rhs.c.2 <- form.list[5]
  rhs.c.3  <- form.list[7]
  rhs.c.4  <- form.list[9]
  rhs.c.5  <- form.list[11]
  
  
  x.vars.list <- c(  rhs.c.1 )
  
  if (  !is.na(rhs.c.2) ){  x.vars.list <- c( x.vars.list,  rhs.c.2 ) }
  if (  !is.na(rhs.c.3) ){  x.vars.list <- c( x.vars.list,  rhs.c.3 ) }
  if (  !is.na(rhs.c.4) ){  x.vars.list <- c( x.vars.list,  rhs.c.4 ) }
  if (  !is.na(rhs.c.5) ){  x.vars.list <- c( x.vars.list,  rhs.c.5 ) }
  
  
  if ( data.set.type == 'train'){
    
    intercept <-  d.gbr[r, 'gbr.model.train.coef.int']  
    coef.bw <-  d.gbr[r, 'gbr.model.train.coef.BW']  
    coef.adg <-  d.gbr[r, 'gbr.model.train.coef.ADG'] 
    coef.ndf <-  d.gbr[r, 'gbr.model.train.coef.NDF'] 
    coef.ndf.x.digest <- d.gbr[r, 'gbr.model.train.coef.NDF*Digest'] 
    
  } else if (data.set.type == 'test' ){
  
  intercept.plus.offset <- d.gbr[r, 'coef.offset.plus.intercept'] 
  intercept <-  d.gbr[r, 'coef.int']  
  coef.bw <-  d.gbr[r, 'coef.BW']  
  coef.adg <-  d.gbr[r, 'coef.ADG'] 
  coef.ndf <-  d.gbr[r, 'coef.NDF'] 
  coef.ndf.x.digest <- d.gbr[r, 'coef.NDF*Digest'] 
  
  
  
  } else if (  data.set.type == 'whole' ){
  
  intercept.plus.offset <- d.gbr[r, 'ws.coef.offset.plus.intercept'] 
  intercept <- d.gbr[r, 'ws.coef.intercept']
  coef.bw <- d.gbr[ r , 'ws.coef.bw_kg']
  coef.adg <- d.gbr[ r , 'ws.coef.adg_g_day']
  coef.ndf <- d.gbr[ r , 'ws.coef.NDF_nutrition']
  coef.ndf.x.digest <- d.gbr[ r , 'ws.coef.NDF_nutrition']
  
  }
  
  x.coefs.list <- c()
  
  if (  !is.na( intercept.plus.offset) ){  x.coefs.list <- c( x.coefs.list ,   intercept.plus.offset )  }
  if (  !is.na(coef.bw) ) {  x.coefs.list <- c( intercept ,  coef.bw )  }
  if (  !is.na(coef.adg )  ){    x.coefs.list <- c( x.coefs.list ,coef.adg  )   }
  if (  !is.na( coef.ndf )  ){    x.coefs.list <- c( x.coefs.list , coef.ndf )   }
  
  
  # Remove elements of x.vars.list for which no coefficients were estimated
  if (  is.na(coef.bw)     ){  x.vars.list <- x.vars.list[-c(1)]  }
  if (  is.na(coef.adg )   ){    x.vars.list <- x.vars.list[-c(1)]   }
  if (  is.na( coef.ndf )  ){    x.vars.list <- x.vars.list[-c(1)]   }
  
  
  return.object.1 <- listify(    x.vars.list  )
  return.object.2 <- listify(    x.coefs.list )
  return.object.all <- listify( c(return.object.1 ,  return.object.2))
  
  return (  c(return.object.1 ,  return.object.2)  )
}

#class(  model)
#names(  model)
#attributes(  model)

#attributes(model$family)

#names(mboost)


gen.complete.cases <<- function( id.list , formla ){
  
  # Test function: id.list <-  cur.id.list ; formla <- formula

  formla.list <- all.vars(formla) 

  # Why can this not handle ue.id?
  # all.vars.plus.meta.d <- c(    formla.list , 'ut.id' , 'T.Animals') 
  
  all.vars.plus.meta.d <- c(    formla.list , 'ut.id' , 'T.Animals') 
  
  df.ss <- s.rums[  s.rums$ut.id %in% id.list , all.vars.plus.meta.d ] 
  
  new.id.list <- df.ss[  complete.cases(df.ss)  ,'ut.id' ]
  
  return (new.id.list)
  
}


# Export data
gbr.out <<- function(){ 
  
  cols.not.2.export <- c(
    
    "gbr.model" 
    
    , "all.data"
    ,'r.cond'
    
  )
  
  cols.fold.sets <- c()

  
  cols.2.export <- colnames(d.gbr)[ which( !(colnames(d.gbr) %in% cols.not.2.export) )  ]
  
  d.export <- d.gbr[  , cols.2.export ]
  
  
  for (c in 1:ncol(d.export)){
    
    column <- colnames(d.export)[c]
    
    type <- typeof(d.export[,column ])
    
    print(paste('',type))
    
    if (type == 'list'){
      
      d.export[,column ] <- as.character(d.export[,column ])
    }
    
    
  }
  
  d.export.sheep.lon.all <- d.export[ d.export$species  == "Sheep" & d.export$ndf == ndf.lev.lo , cols.2.export ]
  d.export.sheep.hin.all <- d.export[ d.export$species  == "Sheep" & d.export$ndf == ndf.lev.hi  , cols.2.export ]
 # d.export.goat.lon <- d.gbr[ d.export$species  == "Goat" & d.export$ndf == ndf.lev.lo , cols.2.export ]
 # d.export.goat.hin <- d.gbr[ d.export$species  == "Goat" & d.export$ndf == ndf.lev.hi , cols.2.export ]
  

  d.export.sheep.lon.fold.sets <- d.export.sheep.lon.all[ ,fold.cond.variables.all ]
  d.export.sheep.hin.fold.sets <- d.export.sheep.hin.all[ ,fold.cond.variables.all ]
  

  write.xlsx(  d.export.sheep.lon , str_c(results.out.dir , "gbr_out_sheep_lon.xlsx") )
  write.xlsx( d.export.sheep.hin , str_c(results.out.dir , "gbr_out_sheep_hin.xlsx"))
  #write.xlsx( d.export.goat.lon, "Figures.out/gbr_out_goat_lon.xlsx")
 # write.xlsx(d.export.goat.hin, "Figures.out/gbr_out_goat_hin.xlsx")
  
  
  write.xlsx(   d.export.sheep.lon.fold.sets , str_c(results.out.dir , "gbr_out_sheep_lon_fold_sets.xlsx") )
  write.xlsx( d.export.sheep.hin.fold.sets , str_c(results.out.dir , "gbr_out_sheep_hin.xlsx"))
  #write.xlsx( d.export.goat.lon, "Figures.out/gbr_out_goat_lon.xlsx")
  # write.xlsx(d.export.goat.hin, "Figures.out/gbr_out_goat_hin.xlsx")
  
  
 
}


gen.gg.df.specific <<- function(  cur.mod , vers , species , ndf){
  
  # cur.mod <- 3; vers <- 'pmetric'
  # cur.mod <- 1 ; vers <- 'n.pmetric'
  
  
  test.sp.lon <- function(){
    
    species <- species.sheep
    ndf <- ndf.lev.lo
    cur.mod <- 3; vers <- 'pmetric'
    
    gbm.cond <- gbm.cond.shp.lo.ndf
    
    species.ndf <- sheep.lo.ndf
  }
  
  test.sp.hin <- function(){
    
    species <- species.sheep
    ndf <- ndf.lev.hi
    cur.mod <- 1; vers <- 'pmetric'
    
    gbm.cond <- gbm.cond.shp.hi.ndf

  }
  
  # Params
  
  
  if (species == species.sheep & ndf == ndf.lev.lo) { species.ndf <- sheep.lo.ndf ;    gbm.cond <- gbm.cond.shp.lo.ndf }
  if (species == species.sheep & ndf == ndf.lev.hi) { species.ndf <- sheep.hi.ndf ;    gbm.cond <- gbm.cond.shp.hi.ndf}
  if (species == species.goat & ndf == ndf.lev.lo) { species.ndf <- goat.lo.ndf ;    gbm.cond <- gbm.cond.gt.lo.ndf}
  if (species == species.goat & ndf == ndf.lev.hi) { species.ndf <- goat.hi.ndf;    gbm.cond <- gbm.cond.gt.hi.ndf }

  
{
    
    gg.dat.nrow <- unique( d.gbr[ gbm.cond & d.gbr$species.ndf == species.ndf & d.gbr$is.best.model & d.gbr$mod.form == cur.mod   ,  'total.sample.size' ])

    
    # Figure display items
    gbm.cond.opt.mod <- (  gbm.cond & d.gbr$is.best.model & d.gbr$mod.form == cur.mod)

    
    mod.1.form <-  de.listify( d.gbr[   gbm.cond.opt.mod & d.gbr$mod.form == cur.mod ,  'gbr.form' ] )

    
    mod.1.form.label.w.eqn.s1 <- de.listify( gen.formula.label.1r(mod.1.form ) )[[1]][1]

    mod.1.form.label.w.eqn.s2 <- de.listify( gen.formula.label.1r(mod.1.form ) )[[1]][2]

    
    # Treatment IDs
    mod.1.u.tid <- de.listify( d.gbr[  gbm.cond.opt.mod  & d.gbr$mod.form == cur.mod  ,  'ws.ut.ids' ] )

    mod.1.ccc <-  round( (  d.gbr[  gbm.cond.opt.mod  & d.gbr$mod.form == cur.mod,  "best.w.CCC"]  ) , rd.decs.R2)[1] 
 
    mod.1.r2 <-  round( (d.gbr[  gbm.cond.opt.mod  & !is.na(d.gbr$w.R2.mean) & d.gbr$mod.form == cur.mod,  "best.w.R2" ]) , rd.decs.R2)[1] 
  
    mod.1.nRMSE <-  round( (  d.gbr[  gbm.cond.opt.mod  & !is.na(d.gbr$w.nRMSE.mean) & d.gbr$mod.form == cur.mod,  "best.w.nRMSE"] ) , rd.decs.nRMSE )[1]

    gg.dat.labl.ccc <- paste0("CCC = ",   mod.1.ccc )

   # gg.dat.labl.R2 <- #expression("R"[adj]*"10" ^ 2)
      
    #  paste0("R", supsc("2") , subsc('a') , " = ",  mod.1.r2 )
      
     # paste("R"['adh']^2 , " = ",  '1' )
      
     
    gg.dat.labl.R2 <- paste0("R", supsc("2") ,  "adj = ",  mod.1.r2 )
      
      
    gg.dat.labl.nRMSE <- paste0("nRMSE(%) = ",  mod.1.nRMSE )
    
    mod.ss <- d.gbr[  gbm.cond.opt.mod &  d.gbr$mod.form == cur.mod,  'total.sample.size' ][1]
 
    model.label <- str_c('Model ', cur.mod )
    
    gg.dat.fact.labl.r1.mod.ss <- str_c( model.label , ': n = ' ,  mod.ss)

    gg.dat.fact.labl.r2.mod.ss <- str_c( mod.1.form.label.w.eqn.s1)


  } # Plot data prep
  
  
{
    
    col.mod.form <- c(  
      rep( cur.mod, times = (gg.dat.nrow ) )   
    ) 
    
    col.mod.form.label.r1 <- c(  
      rep(   gg.dat.fact.labl.r1.mod.ss , times = (gg.dat.nrow  ) ) 
    )
    
    col.mod.form.label.r2 <- c(  
      rep(   gg.dat.fact.labl.r2.mod.ss , times = (gg.dat.nrow   ) ) 
    )
    
    col.species <- c(  
      rep( species , times = (gg.dat.nrow ) )  
    ) 
    
    col.ndf <- c(  
      rep( ndf , times = (gg.dat.nrow  ) ) 
    )
    
    col.species.ndf <- c(  
      rep( species.ndf , times = (gg.dat.nrow   ) ) 
    )
    
  
    col.sample.size <- c(  
      rep( species.ndf , times = (gg.dat.nrow  ) ) 
    )
    
    
    col.formula  <- c(  
      rep( cur.mod , times = (gg.dat.nrow ) )  
    ) 
    
    
    
    col.residual  <- c(
      de.listify(d.gbr[gbm.cond.opt.mod ,  'ws.residuals' ][1][1] )  
    )
    
    col.modelled  <- c(  
      
      as.numeric(  de.listify(d.gbr[  gbm.cond.opt.mod,  'ws.predicted.base' ] )  )  
      
    )
    
    col.observed  <- c(
      de.listify(d.gbr[gbm.cond.opt.mod  ,  'observed.Y' ][1][1] )  
    )
    
    
    col.bw_kg <- c(
      
      as.numeric(  de.listify(   d.gbr[gbm.cond.opt.mod ,  'ws.bw_kg.measrs' ][1][1] ))  
      
    ) 
    
    col.label.ut.id <- c(
      mod.1.u.tid
    ) 
  
    col.label.ccc  <- c(
      rep( gg.dat.labl.ccc , times = (gg.dat.nrow   ) ) 
    )
    
    
    col.label.r2  <- c(
      rep( gg.dat.labl.R2 , times = (gg.dat.nrow   ) ) 
    )
    
    col.label.nrmse  <- c(
      rep( gg.dat.labl.nRMSE , times = (gg.dat.nrow   ) ) 
    )
    
    
    if (vers == 'pmetric'){
      
  
      col.coef.bw_kg <- c( 
        rep( d.gbr[gbm.cond,  'ws.coef.bw_kg' ][1] , times = gg.dat.nrow )  
      )
      
      col.coef.intercept <- c( 
        rep( d.gbr[   gbm.cond,  'ws.coef.intercept' ][1] , times = gg.dat.nrow)    
        
      )
      
    }
    
    
    
  } # Pre-define dataframe columns
  
  length(col.species)
  
  #length(col.bw_kg)
  #length(col.coef.bw_kg)
  # length(col.coef.intercept)
  # length(col.label.form)
  
  length(col.label.nrmse)
  length(col.label.r2)
  length(col.label.ut.id)
  length(col.ndf)
  length(col.observed)
  length(col.modelled)
  length(col.residual)
  
  
  
  gg.dat <- data.frame(
    
    
    col.mod.form =  col.mod.form
    
    ,  col.mod.form.label.r1 =  col.mod.form.label.r1
    
    ,  col.mod.form.label.r2 =  col.mod.form.label.r2
    
    , species   = col.species
    
    , ndf     = col.ndf
    
    , species.ndf = col.species.ndf 
    
    , observed =   col.observed
    
    , modelled =  col.modelled
    
    ,  residual =   col.residual
    
    # ,  rms.residual.1 =   col.rms.residual
    
    #, bw_kg = col.bw_kg
    
    
    , label.ut.id = col.label.ut.id 
    
    , label.ccc = col.label.ccc
    
    , label.r2 = col.label.r2
    
    , label.nrmse = col.label.nrmse
    
  )
  
  if ( vers == "pmetric") { 
    
    gg.dat$coef.bw_kg <- col.coef.bw_kg ; 
    gg.dat$coef.intercept <- col.coef.intercept
    
    #gg.dat$label.form.r1 <- col.label.r1.form
    # gg.dat$label.form.r2 <- col.label.r2.form
    # gg.dat$label.form.r3 <- col.label.r3.form
    
  }
  
  
  return (gg.dat )
  
}


gen.var.alias <- function( var ){
  
#  var <- x.vars[c(5)]
  
 # print(paste('current var is', var))
  
  if (  str_detect( var , fixed("."))  ){
  
  prefx <- substr( var, 1 , (str_length(var)-4)  )
  suffx <- substr( var, (str_length(var)-3) , (str_length(var)) )
  
 
  new.prefx <- form.aliases[ which( all.x.vars == prefx ) ]
  
  new.suffx <- suffixes.labs[ which(suffixes == suffx ) ]
  
  
  new.lab <- str_c(  new.prefx ,  new.suffx )

  
  } else { 
    
    new.prefx <- form.aliases[ which( all.x.vars == var ) ]
    
    new.lab <-  new.prefx 
  
    
    }
  
  return (new.lab)
  
}



assign.best.model <<- function( r.cond  , d.gbr ){
  
  # test: r.cond <- species.ndf.form.cond.pmetric
  
  optim.metric <- max( na.omit(  d.gbr[   r.cond,  optimization.metric.var.name.1 ])  )
  
  optim.mod.vers.indx <- which( d.gbr[,optimization.metric.var.name.1]  ==  optim.metric    )
  
  optim.mod.vers <-  d.gbr[optim.mod.vers.indx, 'mod.vers' ] 
  d.gbr[optim.mod.vers.indx, 'is.best.model' ] <-  TRUE
  
  
  d.gbr[r.cond, vn.best.w.R2  ] <- d.gbr[optim.mod.vers.indx, optimization.metric.var.name.1 ][1] 
  d.gbr[r.cond, vn.best.w.nRMSE ] <- d.gbr[optim.mod.vers.indx, vn.w.nRMSE.mean ][1] 
  d.gbr[r.cond , vn.best.w.CCC ] <- d.gbr[optim.mod.vers.indx, vn.w.CCC.mean ][1]   
  

  
  # Add coefficient data here.
  
  return (d.gbr)
}




assign.best.model.all.forms <<- function( r.cond  , d.gbr ){
  
  # test: r.cond <- species.ndf.form.cond.pmetric 
  
  
  optim.metric <- max( na.omit(  d.gbr[   r.cond,  optimization.metric.var.name.1 ])  )
  
  
  optim.mod.vers.form.indx <- which( d.gbr[,optimization.metric.var.name.1]  ==  optim.metric    )
  
  optim.mod.vers <-  d.gbr[  optim.mod.vers.form.indx, 'mod.vers' ] 
  optim.mod.form <-  d.gbr[  optim.mod.vers.form.indx, 'mod.form' ] 
  d.gbr[optim.mod.vers.form.indx, 'is.best.global.model' ] <-  TRUE
  
  
  d.gbr[r.cond, vn.best.global.w.R2  ] <- d.gbr[optim.mod.vers.form.indx, optimization.metric.var.name.1 ][1] 
  d.gbr[r.cond, vn.best.global.w.nRMSE ] <- d.gbr[optim.mod.vers.form.indx, vn.w.nRMSE.mean ][1] 
  d.gbr[r.cond , vn.best.global.w.CCC ] <- d.gbr[optim.mod.vers.form.indx, vn.w.CCC.mean ][1]   
  

  # Add coefficient data here.
  
  return (d.gbr)
}


xtract.x.vars <<- function( formula   ){
  
  # test: formula <- form 
  
  formula <- Reduce(paste, deparse(   formula))
  
  # typeof(form.obj)
  
  formula <- as.character(   formula  ) 
  
  
 # formula[ which(formula  == "\"")]
  
  
 # form.x.vars  <- formula[ which(formula  != '' & formula  != '+' & formula  !=  y.var & formula  != '~')]
  
#  Reduce(paste, deparse(   form.x.vars))
  
  
  #strsplit(  form.x.vars , "  ")[[1]]
  
  form.list <- strsplit(  formula , " ")[[1]]
  
  form.x.vars <- form.list[ which( form.list  != '' & form.list != '+' & form.list  !=  y.var & form.list  != '~' & form.list  != 'feed_intake_g_d')]
  
  
 # form.rhs <- form.list[c(3:length(form.list))]
 # form.x.vars <-  form.rhs[ which(form.rhs != '' & form.rhs != '+')]
  
  
  return (form.x.vars)
  
  
}


extract.ordered.vars <<- function( model ){
  
  # test: model <- model
  
  var.names <- listify.coef.names(model)
  
  var.names.ordered <- c()
  
  if (  any( na.omit(var.names == "(Intercept)" ))  ){ var.names.ordered[1] <- var.names[which(var.names == "(Intercept)" )]
  } else { var.names.ordered[1] <- NA }
  
  if (   any( na.omit(str_detect(var.names , 'bw' ) )  )  ){ var.names.ordered[2] <- var.names[which(str_detect(var.names , 'bw' ))]
  } else { var.names.ordered[2] <- NA }
  
  if (   any( na.omit(str_detect(var.names , 'adg' ) )  )  ){ var.names.ordered[3] <- var.names[which(str_detect(var.names , 'adg' ))]
  } else { var.names.ordered[3] <- NA }
  
  if (   any( na.omit(str_detect(var.names , 'NDF_nutr' )) )   ){ var.names.ordered[4] <- var.names[which(str_detect(var.names , 'NDF_nutr' ))]
  } else { var.names.ordered[4] <- NA }
  
  if (   any( na.omit(str_detect(var.names , 'CP_nutrition' )) )   ){ var.names.ordered[5] <- var.names[which(str_detect(var.names , 'CP_' ))]
  } else { var.names.ordered[5] <- NA } 
  
  if (   any( na.omit(str_detect(var.names , 'NDF_digest' )) )   ){ var.names.ordered[6] <- var.names[which(str_detect(var.names , 'NDF_digest' ))] 
  } else { var.names.ordered[6] <- NA } 
  
  
  return (var.names.ordered)
}


rev.tform.y <<- function(form.lhs , ws.predicted){
  
  test <- function(){
    
    form.lhs <<- form.lhs 
    ws.predicted <<- ws.predicted
    
  }
  
  if ( str_detect( form.lhs , '.log' )  ){  ws.predicted.r.tformed <<- base.nat.log.e  ^ ws.predicted  }
  if ( str_detect( form.rhs , '.e25' )  ){  ws.predicted.r.tformed <<- ws.predicted ^ (1/.25) }
  if ( str_detect( form.rhs , '.sqt' )  ){  ws.predicted.r.tformed <<- ws.predicted ^ (1/.5) }
  if ( str_detect( form.rhs , '.e75' )  ){  ws.predicted.r.tformed <<- ws.predicted ^ (1/.75) }
  if ( str_detect( form.rhs , '.sqd' )  ){  ws.predicted.r.tformed <<- ws.predicted ^ (1/.25) }
  if ( str_detect( form.rhs , '.cbd' )  ){  ws.predicted.r.tformed <<- ws.predicted ^ (1/.25) }
  if ( str_detect( form.rhs , '.recip' )  ){  ws.predicted.r.tformed <<- 1 / ws.predicted }
  
  
  return (  ws.predicted.r.tformed  )
  
}



stat.significance <- function( coef, se){
  
  
  test <- function(){
    
    coef <- coef.NDF.mean
    se <-   coef.NDF.se 
    
  }
  
  range.90.pca <- SE.scalar.90.pci * se
  range.95.pca <- SE.scalar.95.pci * se
  range.99.pca <- SE.scalar.99.pci * se
  
  sign <- ifelse( !between(0, ( coef - range.90.pca ), ( coef + range.90.pca  ) ) , 90 , 0)
  
  if (sign == 90){ sign <- ifelse( !between(0, ( coef - range.90.pca ), ( coef + range.90.pca  ) ), 95 , 0) }
  if (sign == 95){ sign <-  ifelse( !between(0, ( coef - range.90.pca ), ( coef + range.90.pca  ) ), 99 , 0)}
  
  if (sign == 0){stars <- ''}
  if (sign == 90){stars <- '*'}
  if (sign == 95){stars <- '**'}
  if (sign == 99){stars <- '***'}
  
  return (  stars  )
  
}
