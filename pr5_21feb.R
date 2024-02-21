# Ejercicio 1.
library(ggplot2)
set.seed(123)
n_registros <- 200
site <- sample(paste0("site", 1:10), n_registros, replace = TRUE)
tipo_de_artefactos <- sample(c("Poterry", "Tools","Jewerly", "Weapons"), n_registros, replace = TRUE)
n_artefactos <- sample(1:1000, n_registros, replace = TRUE)
contextos <- sample(c("habitacional", "Funerario", "Otros"), n_registros, replace = TRUE)
latitud <- runif(n_registros, min=0, max =90)
longitud <- runif(n_registros, min=-180, max =180)

archaeology_data <- data.frame(
  site = c(site),
  tipo_de_artefactos = c(tipo_de_artefactos),
  n_artefactos = c(n_artefactos),
  contextos = c(contextos),
  latitud = c(latitud),
  longitud = c(longitud))
View(archaeology_data)

# Ejercicio 2.
media_n_artefactos <- mean(n_artefactos)
print(media_n_artefactos)
quantile(n_artefactos)

# Ejercicio 3.
hist_n_artefactos <- hist(archaeology_data$n_artefactos,
                          main = "Número de artefactos",
                          col = "blue")

# al ser mayor la media (459.155) que la mediana(440.5) estamos ante un caso de asi asimetría positiva.

# Ejercicio 4.
cajabigote_n_artefactos <- boxplot(archaeology_data$n_artefactos,
                                   main ="Número de artefactos",
                                   col = "blue")
# Ejercicio 5.
archaeology_data$site <- factor(archaeology_data$site, levels = paste0("Site", 1:10))


# Ejercicio 6. 
ggplot(archaeology_data, aes(x =longitud, y = latitud)) +
  geom_bin2d() +
  labs(title = "Densidad de artefactos",
       x= x,
       y = y)

# Ejercicio 7. 
total_artefactos <- sum(archaeology_data$n_artefactos)
print(total_artefactos)

# Ejercicio 8.
median_artefactos_por_yacimiento <- median(archaeology_data$n_artefactos)
print(median_artefactos_por_yacimiento)

# Ejericicio 9.
desvi_estandar <- sd(archaeology_data$n_artefactos)
print(desvi_estandar)

# Ejercicio 10.
max_artefactos_yacimiento <- which.max(archaeology_data$n_artefactos)
print(max_artefactos_yacimiento)

# Ejercicio 11. 
tabla_resu <- table(median_artefactos_por_yacimiento, media_n_artefactos, desvi_estandar)
View(tabla_resu)

# Ejercicio 12.
boxplot(archaeology_data$n_artefactos~archaeology_data$site,archaeology_data = df,
        main = "distribución de los artefactos por yacimientos",
        ylab = "número de artefactos",
        xlab = "yacimiento",
        col = "blue")