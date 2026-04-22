# OUTLIER HANDLING
pre.rows <- nrow(s.rums)
s.rums <- s.rums[    !( s.rums$B.Code %in%  outlier.b.codes  ) & !is.na(  s.rums$B.Code  ), ]
post.rows <- nrow(s.rums)


removed <- pre.rows - post.rows 


print(paste('Outlier data rows removed:' , removed))


