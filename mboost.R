

source('functions.R') ; source('m.boost.params.R')



# Load workspace components


# Data prep
CV.prep <-  function() { source(   str_c(  data.dir  ,'CV.prep.R')  ) }

formulae.defs <-  function() { source(   str_c(  data.dir ,'formulae.R')  ) }

coefs.table.out <-  function() { source(   str_c(  results.dir  ,'table.coefs.R')  ) }

box.fig.out <-  function() { source(   str_c(  results.dir  ,'box.plot.R')  ) }

valid.plots.out <-  function() { source(   str_c(  results.dir  ,'valid.plots.R')  ) }




# to DO
# create keyword for ue.id and ut.id and use consistently
# replace colnames in d.gbr with global character objects
# harmonize the handling of lists in and out
# add functionality for greater formula configurations
# plots for variable importance



CV.prep()

formulae.defs()



nrow(d.gbr)
colnames(d.gbr)

unique(d.gbr$form)



for ( r in 1 : (  nrow(d.gbr.null)  )  ){
  
if (r == 1) { d.gbr <- d.gbr.null ; start.time <- Sys.time()}

print(paste('Running iteration ' , r , 'of ', nrow(d.gbr)))

#  r <- 28
#if (r == 11){ break }

{
  
{
  
d.gbr[r, 'loop.iter' ] <- r
species <-   d.gbr[r, 'species'] 
ndf  <-   d.gbr[r, 'ndf'] 
k <-  d.gbr[r,'k']
mv <- d.gbr[ r, 'mod.vers']
mf <-  d.gbr[r , 'mod.form']  

r.cnd.ss.mf <-  ( d.gbr$species == species & d.gbr$ndf == ndf & d.gbr$mod.form == mf )
r.cnd.ss.mf.mv <-  ( d.gbr$species == species & d.gbr$ndf == ndf & d.gbr$mod.form == mf & d.gbr$mod.vers == mv)


# Extract complete id cases
# condition: if first iteration of formula
if (    any(  is.na(  d.gbr[ r.cnd.ss.mf , vn.t.IDS.CCs ] ) )  ){
  
cur.form <- d.gbr[r, 'form'][[1]] 

cur.id.list <- de.listify( d.gbr[r, 'all.treatment.IDs'] )

if (  fold.by.exp  ){  
  
cur.id.list <- de.listify( d.gbr[r, 'e.IDs'] )  
cur.id.list <- s.rums[s.rums$ue.id %in% cur.id.list , 'ut.id'] 
}

cc.id.list <- listify( gen.complete.cases(    cur.id.list , cur.form    )) 


d.gbr[ r.cnd.ss.mf , vn.t.IDS.CCs]  <- cc.id.list
d.gbr[ r.cnd.ss.mf , vn.t.IDS.CCs.remaining] <- cc.id.list  

d.gbr[r.cnd.ss.mf , 'total.sample.size'] <-  length( de.listify(cc.id.list)[[1]])

}


mod.vers <- n.mod.v.ids[  (which(mv == n.mod.v.ids)-1)  ]

fold.k.condition <- ( 
  d.gbr$species == species 
  & d.gbr$ndf == ndf 
  & d.gbr$k == k 
  & d.gbr$mod.vers == mod.vers 
  & d.gbr$mod.form == mf )



if ( any(fold.k.condition)  ){

if (       !is.na(( d.gbr[ fold.k.condition , vn.test.t.IDS]  ) )   ) {  
  
  row <- which(fold.k.condition)
  
  ids.remaining <- de.listify( d.gbr[row, vn.t.IDS.CCs.remaining]  )
  sampled.ids <- d.gbr[[ row , vn.test.t.IDS]]
  
  d.gbr[ r , vn.t.IDS.CCs.remaining] <- listify(ids.remaining )
  
}} else {
  
  # Fold conditioning
  ids.remaining <- de.listify( d.gbr[r, vn.t.IDS.CCs.remaining]  )

  q.total.unique.ids <-  length(unique( d.gbr[[r, vn.t.IDS.CCs]]  ))
  
  q.ids.to.sample <- ceiling( ( 1  / p.k ) * q.total.unique.ids)
  
  try.attempt  <- try ( sampled.ids <<- sample(  ids.remaining ,  q.ids.to.sample)   , silent = TRUE )
  
if ( inherits(  try.attempt , "try-error") ) { 
ids.to.sample <-   length(ids.remaining)  #q.ids.to.sample - 1 

try.attempt  <- try ( sampled.ids <<- sample(  ids.remaining ,  ids.to.sample )   , silent = TRUE )

if ( inherits(  try.attempt , "try-error") ) { 

ids.to.sample <-   q.ids.to.sample - 2

sampled.ids <<- sample( ids.remaining ,    ids.to.sample)  
}}

  ids.remaining <-  ids.remaining[  (which( !(ids.remaining %in% sampled.ids))) ]
  
  next.iter <- (d.gbr$species == species & d.gbr$ndf == ndf &  d.gbr$mod.vers == mv & d.gbr$k == (k + 1) & d.gbr$mod.form == mf )
  
  d.gbr[next.iter , vn.t.IDS.CCs.remaining] <-   listify( ids.remaining )
  
}


} # Iteration conditions



all.ids <-   d.gbr[[r, vn.t.IDS.CCs]][  which(( d.gbr[[r, vn.t.IDS.CCs]]  %in% d.gbr[[r, vn.t.IDS.CCs]] ))  ]
train.ids <-   d.gbr[[r, vn.t.IDS.CCs]][  which(!( d.gbr[[r, vn.t.IDS.CCs]]  %in% sampled.ids))  ]
test.ids <-   d.gbr[[r, vn.t.IDS.CCs]][  which(( d.gbr[[r, vn.t.IDS.CCs]]  %in% sampled.ids))  ]

# Ids for experiment
train.ids.exmt <- unique( s.rums[s.rums$ut.id %in% train.ids , 'ue.id'])
test.ids.exmt <- unique( s.rums[s.rums$ut.id %in% test.ids , 'ue.id'])

d.gbr[r, vn.train.t.IDS  ] <-  listify( train.ids )
d.gbr[r, vn.train.t.IDS.length ] <-   length(train.ids)

d.gbr[r, vn.test.t.IDS ] <-  listify( test.ids )
d.gbr[r, vn.test.t.IDS.length ] <-   length(test.ids)



s.rums[ s.rums$ut.id %in% all.ids   , ] 

main.data.r.cond <- d.gbr[r, 'r.cond'][[1]]

all.data <- s.rums[ s.rums$ut.id %in% all.ids   , ]  #main.data.r.cond

d.gbr[r, 'all.data'] <- listify( all.data ) 

total.animal.sample <- 0
for (s.r in 1:nrow(all.data)){ total.animal.sample <-  total.animal.sample + all.data[s.r , 'Sample.size']   }

d.gbr[r, 'Total.animal.sample'] <- total.animal.sample


# Set observed values
train.data <- all.data[  all.data$ut.id %in%  train.ids, ]
test.data <- all.data[  all.data$ut.id %in%  test.ids, ]



{
  

formula <- de.listify( d.gbr[  r , 'form'] )  

model.return <-  gen.reg.model(  train.data ,  formula , d.gbr[r, 'mod.vers'] )


model <-  de.listify( model.return )[[1]]

m.stop <-  de.listify( model.return[2] )

nu <-  de.listify( model.return[3] ) 



model.offset <- as.numeric(model$offset)

# store in dataframe
d.gbr[r,  'model'] <- listify ( model )  
d.gbr[r,   'm.stop'] <- m.stop
d.gbr[r, 'nu']  <-   nu

ordered.var.names <- extract.ordered.vars(model)


d.gbr[r, 'model.train.coef.int']  <- as.numeric(coef(model  )[ordered.var.names[1]] ) 
d.gbr[r, 'model.train.coef.BW']  <- as.numeric(coef(model  )[  ordered.var.names[2]] ) 
d.gbr[r, 'model.train.coef.ADG']  <- as.numeric(coef(model  )[   ordered.var.names[3]] ) 
d.gbr[r, 'model.train.coef.NDF']  <- as.numeric(coef(model  )[ ordered.var.names[4]] ) 
d.gbr[r, 'model.train.coef.NDF*Digest']  <- as.numeric(coef(model  )[ "NDF_nutrition"] ) 

d.gbr[r, 'model.train.coef.offset.plus.intercept']  <- as.numeric(coef(model  )['(Intercept)'] ) + model.offset 
if ( is.na( as.numeric(coef(model  )['(Intercept)'] ))){ d.gbr[r, 'model.train.coef.offset.plus.intercept']  <- model.offset  }


} # -- MODEL TRAINING --


trai.eval <- function(){
  
d.gbr[r, 'train.r2'] <- de.listify( gen.eval.metrics( model  ,  formula , train.data , train.data[,'feed_intake_g_d']  , train.ids , train.ids.exmt , model.offset , 'train' , r)[[1]] )   
d.gbr[r, 'train.nrmse'] <-  de.listify( gen.eval.metrics( model  , formula , train.data , train.data[,'feed_intake_g_d']  , train.ids , train.ids.exmt , model.offset , 'train', r )[[2]] )  
d.gbr[r, 'train.w.r2'] <-  de.listify( gen.eval.metrics( model  ,  formula ,train.data , train.data[,'feed_intake_g_d']  , train.ids , train.ids.exmt , model.offset, 'train' , r)[[3]] )   
d.gbr[r, 'train.w.nrmse'] <- de.listify( gen.eval.metrics( model  ,  formula ,train.data , train.data[,'feed_intake_g_d']  , train.ids , train.ids.exmt , model.offset, 'train', r)[[4]] )  


} # -- TRAINING MODEL EVALUATION - train data


{
  
  
form.lhs <- as.character(formula[[2]])
ws.observed.tformd <-  all.data[,  form.lhs  ] 
  
  
mean.bw <- mean( all.data$bw_kg ) 
mean.adg <- mean( all.data$adg_g_day ) 
mean.ndf <- mean( all.data$NDF_nutrition) 
mean.milk <- mean( all.data$milk_kg_day) 

# Re estimate model
model.return.ws <- gen.reg.model(  all.data,  formula , d.gbr[r, 'mod.vers'] )

ws.model <- de.listify( model.return.ws )[[1]]

ws.m.stop <- de.listify( model.return.ws[2] )

ws.nu <- de.listify( model.return.ws[3] ) 

ws.model.offset <- as.numeric(ws.model$offset)

d.gbr[r, 'gbr.whole.sample'] <- listify ( ws.model )  
d.gbr[r, 'ws.ut.ids'] <- listify ( all.data$ut.id )  



if (  substr(mv,1,1)  == 1 )  {  # Specific to paramettric models
  
 
# other method coef(  ws.model , off2int = TRUE)
  
ordered.var.names <- extract.ordered.vars(ws.model)
  
  
ws.intercept.coef <- as.numeric(coef(ws.model )[ ordered.var.names[1] ]) 
ws.bw_kg.coef <- as.numeric(coef(ws.model)[  ordered.var.names[2]  ] )
ws.adg_g_day.coef <- as.numeric(coef(ws.model )[ ordered.var.names[3]  ] )
ws.NDF_nutrition.coef <- as.numeric(coef(ws.model )[ ordered.var.names[4]  ] )
ws.CP_nutrition.coef <- as.numeric(coef(ws.model )[ ordered.var.names[5]  ] )
ws.NDF_digest.coef <- as.numeric(coef(ws.model )[ ordered.var.names[6]  ] )

ws.offset.coef <- as.numeric(model$offset)

#intercept.scaled <-  ws.intercept.coef + mean.bw *  ws.bw_kg.coef  + mean.adg *  ws.adg_g_day.coef + mean.ndf *  ws.NDF_nutrition.coef


d.gbr[r, 'ws.coef.intercept'] <-  ws.intercept.coef 
d.gbr[r, 'ws.coef.bw_kg'] <- ws.bw_kg.coef
d.gbr[r, 'ws.coef.adg_g_day'] <- ws.adg_g_day.coef
d.gbr[r, 'ws.coef.NDF_nutrition'] <- ws.NDF_nutrition.coef
d.gbr[r, 'ws.coef.CP_nutrition'] <- ws.CP_nutrition.coef
d.gbr[r, 'ws.coef.NDF_digest'] <- ws.NDF_digest.coef
d.gbr[r, 'ws.coef.offset'] <- ws.offset.coef
d.gbr[r, 'ws.coef.offset.plus.intercept'] <- (1) * ws.offset.coef + ws.intercept.coef 

if ( is.na(ws.intercept.coef )) { d.gbr[r, 'ws.coef.offset.plus.intercept'] <- (1) * ws.offset.coef }



if (  random.exp.int ){  ws.predicted <- predict.manual( formula , all.data ,  ws.offset.coef , 'whole' , r )              } 
if ( !random.exp.int ){  ws.predicted <- as.numeric(predict(ws.model , newdata = all.data ))          } 



if ( y.var != form.lhs ){ ws.predicted.base <-  as.numeric(rev.tform.y(form.lhs , ws.predicted)) } else {
  
  ws.predicted.base <-  ws.predicted 
}
  
  d.gbr[r, 'ws.predicted'] <- listify( ws.predicted ) 
  d.gbr[r, 'ws.predicted.base'] <- listify( ws.predicted.base ) 


ws.observed.base <- all.data[,y.var]
d.gbr[r, 'observed.Y'] <- listify( ws.observed.base )


ws.residuals  <- ws.predicted.base  - ws.observed.base
d.gbr[r, 'ws.residuals'] <- listify(ws.residuals )
d.gbr[r, 'ws.rms.residuals'] <- listify( sqrt(ws.residuals^2) )


}

} # -- MODEL PREDICTION and evaluation -- Whole sample


{

#d.gbr[r, 'predicted.mod'] <-   listify( as.numeric(test.predicted) )


try( d.gbr[r, 'var.corr.bw.adg'] <-  cor( all.data[,ordered.var.names[2] ] , all.data[,ordered.var.names[3] ] )  , silent = TRUE )
try( d.gbr[r, 'var.corr.bw.ndf'] <-  cor( all.data[,ordered.var.names[2] ] , all.data[,ordered.var.names[4] ] )  , silent = TRUE )
  
try( d.gbr[r, 'var.corr.bw.ndf'] <-  cor( all.data[,ordered.var.names[3] ] , all.data[,ordered.var.names[4] ] )  , silent = TRUE )
  
  
d.gbr[r, 'var.imp.int']  <- as.numeric(varimp(model  )[ ordered.var.names[1] ] ) 
d.gbr[r, 'var.imp.BW']  <- as.numeric(varimp(model  )[ ordered.var.names[2]   ] ) 
d.gbr[r, 'var.imp.ADG']  <- as.numeric(varimp(model  )[   ordered.var.names[3]  ] ) 
d.gbr[r, 'var.imp.NDF']  <- as.numeric(varimp(model  )[ ordered.var.names[4] ] ) 
d.gbr[r, 'var.imp.CP']  <- as.numeric(varimp(model  )[ ordered.var.names[5] ] ) 
d.gbr[r, 'var.imp.NDF_Digest']  <- as.numeric(coef(model  )[ ordered.var.names[6]]) 


var.imp.sum <- 0

for (v in 1:length(ordered.var.names)){ 
  # test: v <- 1
  var.2.add <-  as.numeric(  varimp(model)[ ordered.var.names[v] ] ) 
  
  if ( !is.na(var.2.add)) { var.imp.sum <- var.imp.sum +  var.2.add }
}

d.gbr[r, 'var.imp.rel.int']  <- d.gbr[r, 'var.imp.int']  / var.imp.sum 
d.gbr[r, 'var.imp.rel.BW']  <- d.gbr[r, 'var.imp.BW']  / var.imp.sum 
d.gbr[r, 'var.imp.rel.ADG']  <- d.gbr[r, 'var.imp.ADG']  / var.imp.sum  
d.gbr[r, 'var.imp.rel.NDF']  <- d.gbr[r, 'var.imp.NDF']  / var.imp.sum 
d.gbr[r, 'var.imp.rel.CP']  <- d.gbr[r, 'var.imp.CP']  / var.imp.sum 
d.gbr[r, 'var.imp.rel.NDF_Digest']  <- d.gbr[r, 'var.imp.NDF_Digest']  / var.imp.sum 

  
d.gbr[r, 'coef.int']  <- as.numeric(coef(model  )[ ordered.var.names[1] ] ) 
d.gbr[r, 'coef.BW']  <- as.numeric(coef(model  )[ ordered.var.names[2]   ] ) 
d.gbr[r, 'coef.ADG']  <- as.numeric(coef(model  )[   ordered.var.names[3]  ] ) 
d.gbr[r, 'coef.NDF']  <- as.numeric(coef(model  )[ ordered.var.names[4] ] ) 
d.gbr[r, 'coef.CP']  <- as.numeric(coef(model  )[ ordered.var.names[5] ] ) 
d.gbr[r, 'coef.NDF_Digest']  <- as.numeric(coef(model  )[ ordered.var.names[6]]) 

d.gbr[r, 'coef.offset.plus.intercept']  <- as.numeric(coef(model  )[ordered.var.names[1]] ) + model.offset 
if ( is.na( as.numeric(coef(model  )['(Intercept)'] ))){ d.gbr[r, 'coef.offset.plus.intercept']  <- model.offset  }


if ( !random.exp.int ){  test.predicted <- predict( model , newdata = test.data)  }
if ( random.exp.int  ){   test.predicted <- predict.manual(  formula  , test.data , model.offset , 'test' , r )  }



# Evaluation
d.gbr[r, 'test.r2'] <- de.listify( gen.eval.metrics( model  , formula ,  test.data , test.data[,'feed_intake_g_d']  , test.ids , test.ids.exmt , model.offset , 'test' , r )[[1]] )   
d.gbr[r, 'test.nrmse'] <-  de.listify( gen.eval.metrics( model  , formula ,   test.data , test.data[,'feed_intake_g_d']  , test.ids , test.ids.exmt , model.offset , 'test' , r)[[2]] )  
d.gbr[r, 'test.w.r2'] <-  de.listify( gen.eval.metrics( model  ,  formula ,  test.data , test.data[,'feed_intake_g_d']  , test.ids , test.ids.exmt , model.offset , 'test' , r)[[3]] )   
d.gbr[r, 'test.w.nrmse'] <- de.listify( gen.eval.metrics( model  ,  formula ,  test.data , test.data[,'feed_intake_g_d']  , test.ids , test.ids.exmt , model.offset , 'test' , r)[[4]] )  

d.gbr[r, 'test.aic'] <- AIC(  model )


ccc <- CCC(  as.numeric(test.predicted)  ,  as.numeric(test.data[,y.var] )  , ci = "z-transform", conf.level = 0.95, na.rm = FALSE)
  
d.gbr[r, 'test.ccc'] <- ccc$rho.c$est  


} # MODEL TESTING


if ( k == p.k) {
  

  d.gbr[ r.cnd.ss.mf.mv ,  vn.w.R2.mean ] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'test.r2']))
  d.gbr[r.cnd.ss.mf.mv,  vn.w.nRMSE.mean ] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv , 'test.nrmse']))
  d.gbr[r.cnd.ss.mf.mv, vn.w.CCC.mean] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'test.ccc']))  
  d.gbr[r.cnd.ss.mf.mv, vn.w.AIC.mean] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv , 'test.aic']))  
  
 
  d.gbr[r.cnd.ss.mf.mv, vn.w.R2.sd ] <- sd(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'test.r2']))
  d.gbr[r.cnd.ss.mf.mv, vn.w.nRMSE.sd ] <- sd(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'test.nrmse']))
  d.gbr[r.cnd.ss.mf.mv, vn.w.CCC.sd] <- sd(na.omit(d.gbr[ r.cnd.ss.mf.mv , 'test.ccc']))  
  d.gbr[r.cnd.ss.mf.mv, vn.w.AIC.sd ] <- sd(na.omit(d.gbr[ d.gbr$species ==  species & d.gbr$ndf == ndf , 'test.aic']))

  # Coefficients - means and standard deviations

  # Average regression coefficients
  d.gbr[ r.cnd.ss.mf.mv , 'mean.coef.int'] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'coef.int']))
  d.gbr[ r.cnd.ss.mf.mv , 'mean.coef.BW'] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'coef.BW']))
  d.gbr[ r.cnd.ss.mf.mv , 'mean.coef.ADG'] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'coef.ADG']))
  d.gbr[ r.cnd.ss.mf.mv , 'mean.coef.NDF'] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'coef.NDF']))
  d.gbr[ r.cnd.ss.mf.mv , 'mean.coef.CP'] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'coef.CP']))
  d.gbr[ r.cnd.ss.mf.mv , 'mean.coef.NDF_digest'] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'coef.NDF_digest']))
  
  
  # Standard deviation in regression coefficients
  d.gbr[ r.cnd.ss.mf.mv , 'sd.coef.int'] <- sd(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'coef.int']))
  d.gbr[ r.cnd.ss.mf.mv , 'sd.coef.BW'] <- sd(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'coef.BW']))
  d.gbr[ r.cnd.ss.mf.mv , 'sd.coef.ADG'] <- sd(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'coef.ADG']))
  d.gbr[ r.cnd.ss.mf.mv , 'sd.coef.NDF'] <- sd(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'coef.NDF']))
  d.gbr[ r.cnd.ss.mf.mv , 'sd.coef.CP'] <- sd(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'coef.CP']))
  d.gbr[ r.cnd.ss.mf.mv , 'sd.coef.NDF_digest'] <- sd(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'coef.NDF_digest']))
  
  
  # Average feature importance coefficients
  d.gbr[ r.cnd.ss.mf.mv , 'mean.var.imp.int'] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'var.imp.int']))
  d.gbr[ r.cnd.ss.mf.mv , 'mean.var.imp.BW'] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'var.imp.BW']))
  d.gbr[ r.cnd.ss.mf.mv , 'mean.var.imp.ADG'] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'var.imp.ADG']))
  d.gbr[ r.cnd.ss.mf.mv , 'mean.var.imp.NDF'] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'var.imp.NDF']))
  d.gbr[ r.cnd.ss.mf.mv , 'mean.var.imp.CP'] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'var.imp.CP']))
  d.gbr[ r.cnd.ss.mf.mv , 'mean.var.imp.NDF_digest'] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'var.imp.NDF_digest']))
  
  
  # Average feature importance coefficients -- relative
  d.gbr[ r.cnd.ss.mf.mv , 'mean.var.imp.rel.int'] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'var.imp.rel.int']))
  d.gbr[ r.cnd.ss.mf.mv , 'mean.var.imp.rel.BW'] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'var.imp.rel.BW']))
  d.gbr[ r.cnd.ss.mf.mv , 'mean.var.imp.rel.ADG'] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'var.imp.rel.ADG']))
  d.gbr[ r.cnd.ss.mf.mv , 'mean.var.imp.rel.NDF'] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'var.imp.rel.NDF']))
  d.gbr[ r.cnd.ss.mf.mv , 'mean.var.imp.rel.CP'] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'var.imp.rel.CP']))
  d.gbr[ r.cnd.ss.mf.mv , 'mean.var.imp.rel.NDF_digest'] <- mean(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'var.imp.rel.NDF_digest']))
  
  
  # Standard deviation in feature importance 
  d.gbr[ r.cnd.ss.mf.mv , 'sd.var.imp.int'] <- sd(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'var.imp.int']))
  d.gbr[ r.cnd.ss.mf.mv , 'sd.var.imp.BW'] <- sd(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'var.imp.BW']))
  d.gbr[ r.cnd.ss.mf.mv , 'sd.var.imp.ADG'] <- sd(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'var.imp.ADG']))
  d.gbr[ r.cnd.ss.mf.mv , 'sd.var.imp.NDF'] <- sd(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'var.imp.NDF']))
  d.gbr[ r.cnd.ss.mf.mv , 'sd.var.imp.CP'] <- sd(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'var.imp.CP']))
  d.gbr[ r.cnd.ss.mf.mv , 'sd.var.imp.NDF_digest'] <- sd(na.omit(d.gbr[ r.cnd.ss.mf.mv  , 'var.imp.NDF_digest']))
  
  

 # d.gbr[r, 'coef.int']  <- as.numeric(coef(model  )[ ordered.var.names[1] ] ) 
#  d.gbr[r, 'coef.BW']  <- as.numeric(coef(model  )[  ordered.var.names[2]  ] ) 
 # d.gbr[r, 'coef.ADG']  <- as.numeric(coef(model  )[   ordered.var.names[3]  ] ) 
 # d.gbr[r, 'coef.NDF']  <- as.numeric(coef(model  )[ ordered.var.names[4] ] ) 
  #d.gbr[r, 'coef.NDF*Digest']  <- as.numeric(coef(model  )[ "NDF_nutrition"] ) 
  
  
  
  
} # MODEL METRICS ACROSS FOLDS
  
# Model identification 1 -- best performing among given formula (i.e. across hyperparameters)
if ( k == p.k &  mv == col.mod.vers[  length(col.mod.vers )  ]   ) {
  
  ss.mf.cond <- (d.gbr$species == species & d.gbr$ndf == ndf & d.gbr$mod.form == mf  )
  
  cond.pmetric <-  rep(TRUE, nrow(d.gbr)) #(substr(d.gbr$mod.vers , 1 ,1 ) == 1)
 # cond.n.pmetric <- (substr(d.gbr$mod.vers , 1 ,1 ) == 2)
  
  ss.form.cond.pmetric <- ss.mf.cond & cond.pmetric
 # species.ndf.form.cond.n.pmetric <- species.ndf.form.cond & cond.n.pmetric
  
  # Parametric model

  d.gbr <- assign.best.model(  ss.form.cond.pmetric  , d.gbr )
 # d.gbr <- assign.best.model(species.ndf.form.cond.n.pmetric , d.gbr)


}
  
# Model identification 2 -- best performing among all formula (i.e. across hyperparameters and formulae)
if ( k == p.k & mv == col.mod.vers[  length(col.mod.vers )  ] & mf == n.mod.form  ) {
  
  ss.best.mod.cond <- (d.gbr$species == species & d.gbr$ndf == ndf & d.gbr$is.best.model  )
  
  cond.pmetric <-  cond.pmetric <-  rep(TRUE, nrow(d.gbr)) #(substr(d.gbr$mod.vers , 1 ,1 ) == 1) (substr(d.gbr$mod.vers , 1 ,1 ) == 1)
#cond.n.pmetric <- (substr(d.gbr$mod.vers , 1 ,1 ) == 2)
  
  ss.form.cond.pmetric <- ss.best.mod.cond  & cond.pmetric
 # species.ndf.form.cond.n.pmetric <- species.ndf.best.mod.cond  & cond.n.pmetric
  
  
  d.gbr <- assign.best.model.all.forms(ss.form.cond.pmetric , d.gbr )
 # d.gbr <- assign.best.model.all.forms(species.ndf.form.cond.n.pmetric , d.gbr)
  
}


if ( r == nrow(d.gbr)){
  
time.elapsed <-  Sys.time() - start.time
print(paste('Evaluation completed in ' , round(time.elapsed ,1), ' minutes.'))

gbr.out()
} # Report simulation time
  
  
} # Run all

} # k-fold CV


View(d.gbr)

gbr.out()



valid.plots.out()

coefs.table.out()
box.fig.out()




# View(d.gbr)




gg.resids <- gg.y.valid.theme   %>% +  
 geom_point( data = gg.dat ,  aes(x = modelled, y = residual.1) , color = 'darkgrey') +
 # stat_summary(data = gg.dat ,  aes(x = modelled, y = residual.1) , fun = mean, geom = "line", color = "blue", size = 1) +
  geom_abline(intercept = 0, slope = 0, color = "black", linetype = "solid") + 
  
  geom_smooth(data = gg.dat , aes(x = modelled , y = residual.1)  , method = 'loess' , se = FALSE , color = 'black' , span = 0.9, size = .75)+
  #geom_text( data = gg.dat ,  aes(x = observed , y = modelled , label = label.ut.id) , color = 'black' , size = 2.5 , nudge_x = 1 , nudge_y =1  )  + 
  facet_grid( . ~ species.ndf ) + 
  ylab(gg.resid.y.tit) + 
  xlab(gg.resid.x.tit) 
   
gg.resids




gg.rmsr <- gg.y.valid.theme   %>% +  
  geom_point( data = gg.dat ,  aes(x = modelled, y = rms.residual.1) , color = 'darkgrey') +
  # stat_summary(data = gg.dat ,  aes(x = modelled, y = residual.1) , fun = mean, geom = "line", color = "blue", size = 1) +
 # geom_abline(intercept = 0, slope = 0, color = "black", linetype = "solid") + 
  
  geom_smooth(data = gg.dat , aes(x = modelled , y = rms.residual.1)  , method = 'loess' , se = FALSE , color = 'black' , span = 4, size = .75) +
  #geom_text( data = gg.dat ,  aes(x = observed , y = modelled , label = label.ut.id) , color = 'black' , size = 2.5 , nudge_x = 1 , nudge_y =1  )  + 
  facet_grid( . ~ species.ndf ) 

gg.resids
   

gg.y.bw <- gg.y.valid.theme   %>% +  
  geom_point( data = gg.dat ,  aes(x = bw_kg , y = modelled) , color = 'darkgrey')  + 
  
  geom_abline( data = gg.dat , aes( intercept = coef.intercept , slope = coef.bw_kg ) , color = "black", linetype = "solid")  +
facet_grid( . ~ species.ndf ) 









