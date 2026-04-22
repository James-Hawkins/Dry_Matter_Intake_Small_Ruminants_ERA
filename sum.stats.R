



v.list.DV

# Sheep
summary(s.rums.cc[ s.rums.cc$Species == species.sheep & s.rums.cc$is.growing == TRUE, "feed_intake_g_d" ])



summary(s.rums.cc[ s.rums.cc$Species == species.sheep , '' ])

s.rums.cc$NDF_nutrition



# GOATS
summary(s.rums.cc[ s.rums.cc$Species == species.goat , "feed_intake_g_d" ])
summary(s.rums.cc[ s.rums.cc$Species == species.goat  & s.rums.cc$Stage.harmonized == stage.gestating  , "feed_intake_g_d" ])


summary(s.rums.cc[ s.rums.cc$Species == species.goat , "bw_kg" ])
summary(s.rums.cc[ s.rums.cc$Species == species.goat ,  "adg_g_day"  ])
summary(s.rums.cc[ s.rums.cc$Species == species.goat ,  "NDF_nutrition"  ])
summary(s.rums.cc[ s.rums.cc$Species == species.goat ,  "milk_prod_kg_d"  ])


summary(s.rums.cc[ s.rums.cc$Species == species.goat & s.rums.cc$Stage.harmonized == stage.lactating ,  "milk_prod_kg_d"  ])
summary(s.rums.cc[ s.rums.cc$Species == species.goat & s.rums.cc$Stage.harmonized == stage.gestating ,  ""  ])


unique(s.rums.cc[s.rums.cc$Stage.harmonized == stage.gestating, 'B.Code'])
unique(s.rums.cc[s.rums.cc$Stage.harmonized == stage.lactating, 'B.Code'])


# Test formula for goats
560/1000 / 16

-1143  +  297 * log(16) + 124 * log(37) + 70 * log(476) #- 549 + 1311* .05

colnames(s.rums.cc)
