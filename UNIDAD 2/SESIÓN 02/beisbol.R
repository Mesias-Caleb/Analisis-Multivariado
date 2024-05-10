#MATRIZ DE CORRELACION

round(cor(x = beisbol, method = "pearson"), 3)


# IDENTIFICAR COLINEALIDAD
multi.hist(x = beisbol, dcol = c("blue", "red"), dlty = c("dotted", "solid"), main = "")

#GRAFICO 2
ggpairs(beisbol, lower = list(continuous = "smooth"), diag = list(continuous = "barDiag"), axisLabels = "none")

#identificar mejores predictores

modelox = lm(beisbol$Age ~ beisbol$Games+beisbol$`Minutes Played`+beisbol$`Fields Goal`
             +beisbol$`Fields Goal Attempted`+beisbol$`3-points Field Goal`+beisbol$`3-points Field Goal Attempted`+beisbol$`2-points Field Goal`
             +beisbol$`2-points Field Goal Attempted`+beisbol$`Free Throws`+beisbol$`Free Throws Attempted`
             +beisbol$`Offensive Rebounds`+beisbol$`Defensive Rebounds`+beisbol$`Total Rebounds`+beisbol$Assists
             +beisbol$Steals+beisbol$Blocks+beisbol$Turnovers+beisbol$`Personal Fouls`+beisbol$Points+beisbol$Rank+beisbol$Rank)

step(object = modelox, direction = "both", trace=1)

vif(modelox)

# GRAFICA EN 3D (3 variables 1y y 2x)

plot3d(beisbol$Age , beisbol$`Minutes Played`, beisbol$Games, pch = ".", size = 0.5)