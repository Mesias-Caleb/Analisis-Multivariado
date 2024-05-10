##MATRIZ DE CORRELACION
round(cor(x = universidad, method = "pearson"), 3)


## IDENTIFICAR COLINEALIDAD --- POR GRAFICA 1
multi.hist(x = universidad, dcol = c("blue", "red"),
           dlty = c("dotted", "solid"), main = "")

##GRAFICO 2
ggpairs(universidad, lower = list(continuous =
                                  "smooth"), diag = list(continuous = "barDiag"), axisLabels =
          "none")
step(object = modelox, direction = "both", trace=1)

## identificar mejores predictores
modelox = lm(universidad$Founded_year ~ universidad$UK_rank+universidad$World_rank+universidad$score
             +universidad$Minimum_IELTS_score+universidad$fees+universidad$Student_satisfaction+universidad$`Estimated_cost_of_living_per_year_(in_pounds)`+universidad$Latitude)

step(object = modelox, direction = "both", trace=1) 
vif(modelox)

## GRAFICA EN 3D (3 variables 1y y 2x)
plot3d(universidad$score , universidad$World_rank, universidad$Founded_year, pch = ".", size = 0.5)