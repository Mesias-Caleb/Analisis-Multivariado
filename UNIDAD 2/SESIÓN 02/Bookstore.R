#MATRIZ DE CORRELACION
round(cor(x = bookstore, method = "pearson"), 3)

# IDENTIFICAR COLINEALIDAD 
library(psych) 
multi.hist(x = bookstore, dcol = c("blue", "red"),
           dlty = c("dotted", "solid"), main = "")

#GRAFICO 2
library(GGally) 
ggpairs(bookstore, lower = list(continuous =  "smooth"), diag = list(continuous = "barDiag"), axisLabels = "none")

#identificar mejores predictores
modelox = lm(bookstore$price ~ bookstore$pages+bookstore$reviews+bookstore$n_reviews
             +bookstore$star5+bookstore$star4+bookstore$star3+bookstore$star2+bookstore$star1)

step(object = modelox, direction = "both", trace=1)

library(car)

vif(modelox)

# Gráfica en 3D (3 variable 1y y 2x)
library(rgl)

plot3d(bookstore$price , bookstore$pages, bookstore$reviews, pch = ".", size = 0.5)
