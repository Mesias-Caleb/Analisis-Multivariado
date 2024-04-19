install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)


# Datos
Peso <- c(51, 59, 49, 54, 50, 55, 48, 53, 52, 57)
largo <- c(33.5, 38, 32, 37.5, 31.5, 33, 31, 36.5, 34, 35)

# Colores para los puntos
mis_colores <- c("blue", "red", "green", "orange", "purple", "pink", "brown", "gray", "cyan", "magenta")

# Color para los ejes
color_nombre_ejes <- "blue"

# Gráfico de dispersión
pairs(largo ~ Peso, col = mis_colores, col.axis = color_nombre_ejes)

# Crear un dataframe con los datos
df <- data.frame(Peso, largo)

# Gráfico de correlación
chart.Correlation(df)

shapiro.test(Peso)=0.9207
shapiro.test(largo)=0.5263



