

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

d.gbr$gbr.form



for ( r in 1 : (  nrow(d.gbr.null)  )  ){
  
if (r == 1) { d.gbr <- d.gbr.null ; start.time <- Sys.time()}

print(paste('Running iteration ' , r , 'of ', nrow(d.gbr)))
  
#  r <- 1
#if (r == 11){ break }

{
  
{
  
d.gbr[r, 'loop.iter' ] <- r
species <-   d.gbr[r, 'species'] 
ndf  <-   d.gbr[r, 'ndf'] 
k <-  d.gbr[r,'k']
mv <- d.gbr[ r, 'mod.vers']
mf <-  d.gbr[r , 'mod.form']  



r.cnd.species.ndf.mf <-  ( d.gbr$species == species & d.gbr$ndf == ndf & d.gbr$mod.form == mf )
r.cnd.species.ndf.mf.mv <-  ( d.gbr$species == species & d.gbr$ndf == ndf & d.gbr$mod.form == mf & d.gbr$mod.vers == mv)


# Extract complete id cases
# condition: if first iteration of formula
if (    any(  is.na(  d.gbr[ r.cnd.species.ndf.mf , 'treatment.IDs.CCs'] ) )  ){
  
cur.form <- d.gbr[r, 'gbr.form'][[1]] 

cur.id.list <- de.listify( d.gbr[r, 'treatment.IDs'] )

if (  fold.by.exp  ){  
  
cur.id.list <- de.listify( d.gbr[r, 'experiment.IDs'] )  
cur.id.list <- s.rums[s.rums$ue.id %in% cur.id.list , 'ut.id'] 
}

cc.id.list <- listify( gen.complete.cases(    cur.id.list , cur.form    )) 

#cc.id.list.f1 <- cc.id.list 

d.gbr[ r.cnd.species.ndf.mf , 'treatment.IDs.CCs']  <- cc.id.list
d.gbr[, 'IDs.remaining'] <- cc.id.list  # ??

d.gbr[r.cnd.species.ndf.mf , 'total.sample.size'] <-  length( de.listify(cc.id.list)[[1]])

}


mod.vers <- n.mod.v.ids[  (which(mv == n.mod.v.ids)-1)  ]

fold.k.condition <- ( 
  d.gbr$species == species 
  & d.gbr$ndf == ndf 
  & d.gbr$k == k 
  & d.gbr$mod.vers == mod.vers 
  & d.gbr$mod.form == mf )



if ( any(fold.k.condition)  ){

if (       !is.na(( d.gbr[ fold.k.condition , 'k.IDs']  ) )   ) {  
  
  row <- which(fold.k.condition)
  
  ids.remaining <- de.listify( d.gbr[row, 'IDs.remaining']  )
  sampled.ids <- d.gbr[[ row , 'k.IDs']]
  
  d.gbr[ r , 'IDs.remaining'] <- listify(ids.remaining )
  
}} else {
  
  # Fold conditioning
  ids.remaining <- de.listify( d.gbr[r, 'IDs.remaining']  )

  q.total.unique.ids <-  length(unique( d.gbr[[r, 'treatment.IDs.CCs']]  ))
  
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
  
  d.gbr[next.iter , 'IDs.remaining'] <-   listify( ids.remaining )
  
}


} # Iteration traits


  
  
d.gbr[r, 'k.IDs'] <-  listify( sampled.ids)
d.gbr[r, 'k.IDs.length'] <-  listify( sampled.ids)


all.ids <-   d.gbr[[r, 'treatment.IDs.CCs']][  which(( d.gbr[[r, 'treatment.IDs.CCs']]  %in% d.gbr[[r, 'treatment.IDs.CCs']] ))  ]
train.ids <-   d.gbr[[r, 'treatment.IDs.CCs']][  which(!( d.gbr[[r, 'treatment.IDs.CCs']]  %in% d.gbr[[r, 'k.IDs']] ))  ]
test.ids <-   d.gbr[[r, 'treatment.IDs.CCs']][  which(( d.gbr[[r, 'treatment.IDs.CCs']]  %in% d.gbr[[r, 'k.IDs']] ))  ]

# Ids for experiment
train.ids.exmt <- unique( s.rums[s.rums$ut.id %in% train.ids , 'ue.id'])
test.ids.exmt <- unique( s.rums[s.rums$ut.id %in% test.ids , 'ue.id'])

d.gbr[r, 'train.IDs'] <-  listify( train.ids )
d.gbr[r, 'train.IDs.length'] <-   length(train.ids)

d.gbr[r, 'test.IDs'] <-  listify( test.ids )
d.gbr[r, 'train.IDs.length'] <-   length(test.ids)



main.data.r.cond <- d.gbr[r, 'r.cond'][[1]]

all.data <- s.rums[ s.rums$ut.id %in% all.ids   , ]  #main.data.r.cond

d.gbr[r, 'all.data'] <- listify( all.data ) 

# Set observed values
train.data <- all.data[  all.data$ut.id %in%  train.ids, ]
test.data <- all.data[  all.data$ut.id %in%  test.ids, ]



{
  

formula <- de.listify( d.gbr[  r , 'gbr.form'] )

model.return <- gen.glm.model(  train.data ,  formula , d.gbr[r, 'mod.vers'] )


model <- de.listify( model.return )[[1]]

m.stop <- de.listify( model.return[2] )

nu <- de.listify( model.return[3] ) 



model.offset <- as.numeric(model$offset)

# store in dataframe
d.gbr[r,  'gbr.model'] <- listify ( model )  
d.gbr[r,   'm.stop'] <- m.stop
d.gbr[r, 'nu']  <-   nu

ordered.var.names <- extract.ordered.vars(model)


d.gbr[r, 'gbr.model.train.coef.int']  <- as.numeric(coef(model  )[ordered.var.names[1]] ) 
d.gbr[r, 'gbr.model.train.coef.BW']  <- as.numeric(coef(model  )[  ordered.var.names[2]] ) 
d.gbr[r, 'gbr.model.train.coef.ADG']  <- as.numeric(coef(model  )[   ordered.var.names[3]] ) 
d.gbr[r, 'gbr.model.train.coef.NDF']  <- as.numeric(coef(model  )[ ordered.var.names[4]] ) 
d.gbr[r, 'gbr.model.train.coef.NDF*Digest']  <- as.numeric(coef(model  )[ "NDF_nutrition"] ) 

d.gbr[r, 'gbr.model.train.coef.offset.plus.intercept']  <- as.numeric(coef(model  )['(Intercept)'] ) + model.offset 
if ( is.na( as.numeric(coef(model  )['(Intercept)'] ))){ d.gbr[r, 'gbr.model.train.coef.offset.plus.intercept']  <- model.offset  }


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
model.return.ws <- gen.glm.model(  all.data,  formula , d.gbr[r, 'mod.vers'] )

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

#d.gbr[r, 'test.aic'] <- AIC(  model )


ccc <- CCC(  as.numeric(test.predicted)  ,  as.numeric(test.data[,y.var] )  , ci = "z-transform", conf.level = 0.95, na.rm = FALSE)
  
d.gbr[r, 'test.ccc'] <- ccc$rho.c$est  


} # MODEL TESTING


if ( k == p.k) {
  

  d.gbr[ r.cnd.species.ndf.mf.mv ,  vn.w.R2.mean ] <- mean(na.omit(d.gbr[ d.gbr$species ==  species & d.gbr$ndf == ndf   , 'test.r2']))
  d.gbr[r.cnd.species.ndf.mf.mv,  vn.w.nRMSE.mean ] <- mean(na.omit(d.gbr[ d.gbr$species ==  species & d.gbr$ndf == ndf , 'test.nrmse']))
  d.gbr[r.cnd.species.ndf.mf.mv, vn.w.CCC.mean] <- mean(na.omit(d.gbr[ d.gbr$species ==  species & d.gbr$ndf == ndf , 'test.ccc']))  
  
 
  d.gbr[r.cnd.species.ndf.mf.mv, vn.w.R2.sd ] <- sd(na.omit(d.gbr[ d.gbr$species ==  species & d.gbr$ndf == ndf , 'test.r2']))
  d.gbr[r.cnd.species.ndf.mf.mv, vn.w.nRMSE.sd ] <- sd(na.omit(d.gbr[ d.gbr$species ==  species & d.gbr$ndf == ndf , 'test.nrmse']))
  
  d.gbr[r.cnd.species.ndf.mf.mv, 'w.AIC.mean'] <- mean(na.omit(d.gbr[ d.gbr$species ==  species & d.gbr$ndf == ndf , 'test.aic']))

  # Coefficients - means and standard deviations
  d.gbr[r.cnd.species.ndf.mf.mv, 'test.w.coef.bw.sd'] <- 1 # sd(na.omit(d.gbr[ d.gbr$species ==  species & d.gbr$ndf == ndf , 'test.r2']))
  
  # Average regression coefficients
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.coef.int'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'coef.int']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.coef.BW'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'coef.BW']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.coef.ADG'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'coef.ADG']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.coef.NDF'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'coef.NDF']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.coef.CP'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'coef.CP']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.coef.NDF_digest'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'coef.NDF_digest']))
  
  
  # Standard deviation in regression coefficients
  d.gbr[ r.cnd.species.ndf.mf.mv , 'sd.coef.int'] <- sd(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'coef.int']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'sd.coef.BW'] <- sd(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'coef.BW']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'sd.coef.ADG'] <- sd(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'coef.ADG']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'sd.coef.NDF'] <- sd(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'coef.NDF']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'sd.coef.CP'] <- sd(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'coef.CP']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'sd.coef.NDF_digest'] <- sd(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'coef.NDF_digest']))
  
  
  # Average feature importance coefficients
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.var.imp.int'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'var.imp.int']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.var.imp.BW'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'var.imp.BW']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.var.imp.ADG'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'var.imp.ADG']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.var.imp.NDF'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'var.imp.NDF']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.var.imp.CP'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'var.imp.CP']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.var.imp.NDF_digest'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'var.imp.NDF_digest']))
  
  
  # Average feature importance coefficients -- relative
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.var.imp.rel.int'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'var.imp.rel.int']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.var.imp.rel.BW'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'var.imp.rel.BW']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.var.imp.rel.ADG'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'var.imp.rel.ADG']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.var.imp.rel.NDF'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'var.imp.rel.NDF']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.var.imp.rel.CP'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'var.imp.rel.CP']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'mean.var.imp.rel.NDF_digest'] <- mean(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'var.imp.rel.NDF_digest']))
  
  
  # Standard deviation in feature importance 
  d.gbr[ r.cnd.species.ndf.mf.mv , 'sd.var.imp.int'] <- sd(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'var.imp.int']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'sd.var.imp.BW'] <- sd(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'var.imp.BW']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'sd.var.imp.ADG'] <- sd(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'var.imp.ADG']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'sd.var.imp.NDF'] <- sd(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'var.imp.NDF']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'sd.var.imp.CP'] <- sd(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'var.imp.CP']))
  d.gbr[ r.cnd.species.ndf.mf.mv , 'sd.var.imp.NDF_digest'] <- sd(na.omit(d.gbr[ r.cnd.species.ndf.mf.mv  , 'var.imp.NDF_digest']))
  
  

 # d.gbr[r, 'coef.int']  <- as.numeric(coef(model  )[ ordered.var.names[1] ] ) 
#  d.gbr[r, 'coef.BW']  <- as.numeric(coef(model  )[  ordered.var.names[2]  ] ) 
 # d.gbr[r, 'coef.ADG']  <- as.numeric(coef(model  )[   ordered.var.names[3]  ] ) 
 # d.gbr[r, 'coef.NDF']  <- as.numeric(coef(model  )[ ordered.var.names[4] ] ) 
  #d.gbr[r, 'coef.NDF*Digest']  <- as.numeric(coef(model  )[ "NDF_nutrition"] ) 
  
  
  
  
} # MODEL METRICS ACROSS FOLDS
  
# Model identification 1 -- best performing among given formula (i.e. across hyperparameters)
if ( k == p.k &  mv == col.mod.vers[  length(col.mod.vers )  ]   ) {
  
  species.ndf.form.cond <- (d.gbr$species == species & d.gbr$ndf == ndf & d.gbr$mod.form == mf  )
  
  cond.pmetric <-  rep(TRUE, nrow(d.gbr)) #(substr(d.gbr$mod.vers , 1 ,1 ) == 1)
 # cond.n.pmetric <- (substr(d.gbr$mod.vers , 1 ,1 ) == 2)
  
  species.ndf.form.cond.pmetric <- species.ndf.form.cond & cond.pmetric
 # species.ndf.form.cond.n.pmetric <- species.ndf.form.cond & cond.n.pmetric
  
  # Parametric model

  d.gbr <- assign.best.model(species.ndf.form.cond.pmetric , d.gbr )
 # d.gbr <- assign.best.model(species.ndf.form.cond.n.pmetric , d.gbr)


}
  
# Model identification 2 -- best performing among all formula (i.e. across hyperparameters and formulae)
if ( k == p.k & mv == col.mod.vers[  length(col.mod.vers )  ] & mf == n.mod.form  ) {
  
  species.ndf.best.mod.cond <- (d.gbr$species == species & d.gbr$ndf == ndf & d.gbr$is.best.model  )
  
  cond.pmetric <-  cond.pmetric <-  rep(TRUE, nrow(d.gbr)) #(substr(d.gbr$mod.vers , 1 ,1 ) == 1) (substr(d.gbr$mod.vers , 1 ,1 ) == 1)
#cond.n.pmetric <- (substr(d.gbr$mod.vers , 1 ,1 ) == 2)
  
  species.ndf.form.cond.pmetric <- species.ndf.best.mod.cond  & cond.pmetric
 # species.ndf.form.cond.n.pmetric <- species.ndf.best.mod.cond  & cond.n.pmetric
  
  
  d.gbr <- assign.best.model.all.forms(species.ndf.form.cond.pmetric , d.gbr )
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


d.gbr$var.imp.rel.ADG


valid.plots.out()

coefs.table.out()
box.fig.out()


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
gg.y.valid.gt.lon.0 <- gen.gg.valid(gg.d.gt.lon , 500  , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3  )

gg.y.valid.gt.lon.0

gg.y.valid.gt.hin.0 <- gen.gg.valid(gg.d.gt.hin ,600 , gg.valid.lab.y.crd.lev.1 , gg.valid.lab.y.crd.lev.2  , gg.valid.lab.y.crd.lev.3  )

gg.y.valid.gt.hin.0

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

filename.sp = 'Figures.out/sheep.jpeg'


ggsave(filename =  filename.sp ,  gg.valid.sp.all , width = p.width.sp, height = p.height.sp  , dpi = plot.dpi )





gg.y.valid.gt.hin.0 <- gg.y.valid.theme   %>% +
  
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "solid") + 
  geom_point( data = gg.d.gt.lon  ,  aes(x = observed , y = modelled) , size = gg.valid.pnt.size ,  color = gg.valid.pnt.colr) +
  # geom_text( data = gg.dat ,  aes(x = observed , y = modelled , label = label.ut.id) , color = 'black' , size = 2.5 , nudge_x = 1 , nudge_y =1  )  + 
  ggh4x::facet_nested( 
    species.ndf ~  col.mod.form.label.r2
    , strip = strip_nested(size = "variable")
    
  ) +
  # LABELS
  geom_label( 
    data = gg.d.gt.lon  ,
    aes( x = gg.valid.lab.x.crd.sp.hin  
         , y = gg.valid.lab.y.crd.lev.1 
         , label =  label.ccc )
    , label.size = NA
    , parse = FALSE
    , size = gg.valid.lab.fs 
  ) +
  geom_label( 
    data = gg.d.gt.lon  ,
    aes( x = gg.valid.lab.x.crd.sp.hin  
         , y = gg.valid.lab.y.crd.lev.2 
         , label =  label.r2 )
    , label.size = NA
    , parse = FALSE
    , size = gg.valid.lab.fs 
  ) +
  geom_label( 
    data = gg.d.gt.lon,
    aes( x = gg.valid.lab.x.crd.sp.hin  
         , y = gg.valid.lab.y.crd.lev.3  
         , label =  label.nrmse )
    , label.size = NA
    , parse = FALSE
    , size = gg.valid.lab.fs 
  )  + ylab(gg.valid.y.tit) + 
  xlab(gg.valid.x.tit) 

gg.y.valid.gt.lon.0




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









