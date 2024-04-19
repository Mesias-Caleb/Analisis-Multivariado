install.packages("readxl")
library(dplyr)
library(readxl)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(beeswarm)
library(corrplot)


###CARGA DE DATOS


mtcars <- read_delim("D:/AM - IV/mtcars.csv", ",", escape_double = FALSE, trim_ws = TRUE)
head(mtcars,32)


#CUANTAS FILAS Y COLUMNAS TIENEN
dim(mtcars)

#TIPO DE DATOS , double, string
str(mtcars)

#//Nombres tienen las variables
names(mtcars)

#//Datos descriptivos de las variables
summary(mtcars)
mtcars_numeric <- mtcars[, sapply(mtcars, is.numeric)]



# Ejecutar Matriz de correlacion

mtcars_numeric <- as.data.frame(sapply(mtcars, as.numeric))
M <- cor(mtcars_numeric)

#Ejecutar matriz de correlaciones
my_colors <- colorRampPalette(c("yellow", "white", "green"))(200) #definimos los colores
# Ejecutar Matriz de correlacion
M <- cor(mtcars_numeric)
corrplot(M, method = "ellipse", col = my_colors) # Elipse
corrplot(M, method = "circle", col = my_colors) # Circulo
corrplot(M, method = "square", col = my_colors) #Cuadrado
corrplot(M, method = "number", col = my_colors) #Numerico
corrplot(M, method = "shade", col = my_colors) #Sombra
corrplot(M, method = "color", col = my_colors) # Color
corrplot(M, method = "pie", col = my_colors) #Pai o pie







