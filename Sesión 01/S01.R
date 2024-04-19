library(dplyr)
library(readxl)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(beeswarm)
library(sf)
library(raster)

# Lectura de datos
resultados2014 <- read_delim("E:/Data/resultados2014.csv", ",", escape_double = FALSE, trim_ws = TRUE)
resultados2018 <- read_delim("E:/Data/resultados2018.csv", ",", escape_double = FALSE, trim_ws = TRUE)

# Visualización de datos
head(resultados2014, 5)
head(resultados2018, 5)

dim(resultados2014)
str(resultados2014)
summary(resultados2014)
names(resultados2014)

# Definición de funciones personalizadas

# Cambiar nombres de columnas
partidos_nombre = c('pase18', 'pac18', 'adc18', 'pt18', 'fa18', 'pin18', 'pln18', 'pml18', 'png18', 'prc18', 'prsc18', 'prn18', 'pusc18')
cam_nombre <- function(dataframe) {
  for (i in 1:length(partidos_nombre)) {
    names(dataframe)[names(dataframe) == paste0('votos', i)] = partidos_nombre[i]
  }
  return(dataframe)
}

# Porcentaje de votos
votos_porcentaje <- function(dataframe) {
  x <- dataframe %>%
    group_by(codigo) %>%
    mutate_all(funs((. / votos_validos)*100))%>%
    select(-votos_validos)
  return(x)
}

# Partido ganador
winner <- function(dataframe, periodo) {
  x = dataframe %>%
    gather(partido, votos, -codigo) %>%
    group_by(codigo) %>%
    filter(votos == max(votos)) %>%
    separate(partido, c(paste0("partido", periodo)), sep = "1") %>%
    select(-votos)
  return(x)
}

# Grafico de votos
grafico_votos <- function(partido, color) {
  x = por_resultados2018 %>%
    select(codigo, paste0(partido, 18)) %>%
    left_join(
      (por_resultados2014 %>%
         select(codigo, paste0(partido, 14))),
      by = "codigo"
    ) %>%
    gather(anio, votos, -codigo) %>%
    mutate(anio = ifelse(anio == paste0(partido, 14), 2014, 2018))
  
  par(las = 1, bty = "l", family = "mono", font = 1, bg = "transparent")
  
  return(
    beeswarm(
      votos ~ anio, data = x, col = color, pch = 16, method = "hex",
      cex = 0.8, horizontal = TRUE, ylab = "", xlab = paste("Porcentaje de votos del", toupper(partido)),
      main = paste("Porcentaje de votos del", toupper(partido)), xlim = c(0, 60)
    )
  )
}

# Aplicar funciones personalizadas
resultados2018 <- cam_nombre(resultados2018)

por_resultados2014 = resultados2014 %>%
  group_by(codigo) %>%
  mutate_all(list(~ mean(., trim = .2), ~ median(., na.rm = TRUE)))

por_resultados2018 = resultados2018 %>%
  group_by(codigo) %>%
  mutate_all(list(~ mean(., trim = .2), ~ median(., na.rm = TRUE)))

winner2018 <- winner(por_resultados2018, 18)

cambio <- winner2018 %>%
  left_join(winner2014, by = "codigo") %>%
  mutate(
    cambio = ifelse(partido18 == partido14, "sin cambio", "cambio"),
    robo = ifelse(cambio == "cambio", paste(partido18, partido14, sep = " al "), "sin cambio")
  )

grafico_votos("pac", "black")

cantones <- resultados2018 %>%
  filter(codigo == 101 | codigo == 103 | codigo == 119 | codigo == 201 | codigo == 210 | codigo == 301)
sum(cantones$votos_validos) / sum(resultados2018$votos_validos) * 100


# Crear gráficos y mapas
mapa_resultados <- function(dataframe_mapa, partido, color_high, titulo) {
  # Unir bases
  cr_mapa = full_join(cr2, dataframe_mapa, by = c("HASC_2" = "HASC")) %>%
    arrange(desc(order))
  
  return(
    ggplot() +
      geom_sf(data = cr_mapa, aes(fill = {{partido}}), color = "white") +
      coord_sf() + 
      scale_fill_gradient(low = "#E0E0E0", high = color_high, limits = c(0, 70)) +
      labs(x = NULL, 
           y = NULL, 
           title = titulo) +
      theme_void()
  )
}

mapa14 <- left_join(por_resultados2014, codigohasc, by = "codigo")
mapa18 <- left_join(por_resultados2018, codigohasc, by = "codigo")

mapa_resultados(mapa14, pln14, "#219B63", "PLN 2014")
mapa_resultados(mapa18, pln18, "#219B63", "PLN 2018")
