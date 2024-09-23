#-------LIMPIEZA----------
rm(list=ls(all=TRUE))
graphics.off()
cat("\014")

#-------CARGAR LIBRERÍAS----------
install.packages("tidyverse",dependencies=TRUE)
install.packages("VIM",dependencies=TRUE)
install.packages("mlr", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
library(tidyverse)
library(VIM)
library(mlr)
library(ggplot2)
library(dplyr)
#-------CARGAR LOS DATOS----------

setwd("D:/codigos/TrabajoDataScience")

# Cargar el conjunto de datos usando correctamente parametros para este caso
data <- read.csv('hotel_bookings.csv',header= TRUE, na.strings="", stringsAsFactors = FALSE, sep=',', dec='.')

#-------Inspección de datos-------

# Ver la estructura de datos((dimensión, tipo de dato, nombres de columnas)
str(data)

# Resumen de las variables númericas
summary(data)

#-----------PRE-PROCESAR DATOS-------------

# Conversion de variables a sus respectivos tipos de datos(factor o fecha)

data$hotel<-as.factor(data$hotel)
data$is_canceled<-as.factor(data$is_canceled)
data$arrival_date_year<-as.factor(data$arrival_date_year)
data$arrival_date_month<-as.factor(data$arrival_date_month)
data$children<-as.integer(data$children)
data$meal<-as.factor(data$meal)
data$country<-as.factor(data$country)
data$market_segment<-as.factor(data$market_segment)
data$distribution_channel<-as.factor(data$distribution_channel)
data$is_repeated_guest<-as.factor(data$is_repeated_guest)
data$reserved_room_type<-as.factor(data$reserved_room_type)
data$assigned_room_type<-as.factor(data$assigned_room_type)
data$deposit_type<-as.factor(data$deposit_type)
data$agent<-as.factor(data$agent)
data$company<-as.factor(data$company)
data$customer_type<-as.factor(data$customer_type)
data$reservation_status<-as.factor(data$reservation_status)
data$reservation_status_date<-ymd(data$reservation_status_date)

# Conversion de valores NULL A NA 
data$company[data$company == "NULL"]<-NA
data$agent[data$agent=="NULL"]<-NA
data$country[data$country == "NULL"]<-NA
data$adr[data$adr == 0]<-NA

# DATOS FALTANTES
valores.faltantes<-sum(is.na(data))
cat("Valores faltantes:",valores.faltantes, "\n")
aggr(data,numbers=T,sortVar=T)

# Mostrar ubicacion de los datos faltantes
valores.faltantes.idx<-which(is.na(data), arr.ind = TRUE)
valores.faltantes.idx

# Método Imputación :Reemplazando valores faltantes con la moda(Categorias, enteros) y mediana(Numerico)

data.limpia <- impute(data, classes = list(factor = imputeMode(),
                                           integer = imputeMode(),
                                           numeric = imputeMedian()),
                      dummy.classes = c("integer","factor"), dummy.type = "numeric")
data.limpia=data.limpia$data[,1:min(dim(data))]

#Verificamos si aun existen datos faltantes
valores.faltantes.datos.limpios<-sum(is.na(data.limpia))
cat("Cantidad de datos faltantes después de la limpieza:",valores.faltantes.datos.limpios,"\n")

#----------PRE-PROCESAR CASO DATOS ATIPICOS(OUTLIERS)---------

# Seleccionar columnas numéricas
datos.numericos <- colnames(data.limpia)[sapply(data.limpia, is.numeric)]

# Reemplazo de valores atípicos
for (col in datos.numericos) {
  # Identificar los valores atípicos usando boxplot.stats
  outliers <- boxplot.stats(data.limpia[[col]])$out
  
  # Verificar si hay valores atípicos
  if (length(outliers) > 0) {
    cat("Valores atípicos en", col, ":", outliers, "\n")
    
    # Si la columna es 'children' o 'babies', usar la mediana
    if (col %in% c("children", "babies")) {
      mediana <- median(data.limpia[[col]], na.rm=TRUE)
      # Evitar que el reemplazo sea 0 (a menos que la mediana realmente sea 0)
      if (mediana != 0) {
        data.limpia[[col]][data.limpia[[col]] %in% outliers] <- mediana
      } else {
        cat("Advertencia: la mediana de", col, "es 0. No se reemplazan valores.\n")
      }
    } else {
      # Para otras columnas usamos el promedio
      promedio <- mean(data.limpia[[col]], na.rm=TRUE)
      # Evitar que el promedio sea 0
      if (promedio != 0) {
        data.limpia[[col]][data.limpia[[col]] %in% outliers] <- promedio
      } else {
        cat("Advertencia: el promedio de", col, "es 0. No se reemplazan valores.\n")
      }
    }
  }
}

# Visualizar los cambios
print(head(data.limpia))


#----------VISUALIZACIÓN DE DATOS---------

#1. ¿Cuántas reservas se realizan por tipo de hotel? ¿Qué tipo de hotel prefiere la gente?

# Contar el número de reservas por tipo de hotel
reservas_por_hotel <- data.limpia %>%
  count(hotel)

ggplot(data = reservas_por_hotel, mapping = aes(x = hotel, y = n, fill = hotel)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Número de Reservas por Tipo de Hotel", x = "Tipo de Hotel", y = "Número de Reservas") +
  theme_minimal()



# 2. ¿Está aumentando la demanda con el tiempo?

# Agrupar los datos por año y tipo de hotel
demandaportiempo <- data.limpia %>%
  group_by(year = format(reservation_status_date, "%Y"), hotel) %>%
  summarize(n = n())

# Gráfico de líneas para mostrar la demanda a lo largo del tiempo
ggplot(data = demandaportiempo, aes(x = year, y = n, group = hotel)) +
  geom_line(aes(color = hotel), size = 1) + 
  geom_point(aes(color = hotel), size = 2) +
  labs(title = "Evolución de la Demanda a lo Largo del Tiempo", x = "Año", y = "Número de Reservas") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))




#3. ¿Cuáles son las temporadas de reservas (alta, media, baja)?

# Contar la frecuencia de reservas por mes
tabla_frecuencia <- table(data.limpia$arrival_date_month)
freq_meses <- as.data.frame(tabla_frecuencia)
names(freq_meses) <- c("mes", "frecuencia")

# Ordenar las frecuencias
freq_meses_ordenada <- freq_meses[order(freq_meses$frecuencia, decreasing = TRUE), ]

# Identificar temporadas
frecuencias <- freq_meses_ordenada$frecuencia
meses <- freq_meses_ordenada$mes

temporada_alta <- meses[frecuencias >= frecuencias[4]]  # Top 4 meses
temporada_baja <- meses[frecuencias <= frecuencias[9]]  # Bottom 4 meses
temporada_media <- meses[!(meses %in% c(temporada_alta, temporada_baja))]

# Mostrar temporadas
cat("Temporada Alta:", temporada_alta, "\n")
cat("Temporada Media:", temporada_media, "\n")
cat("Temporada Baja:", temporada_baja, "\n")

# Colores para las temporadas
colores <- c("Temporada Alta" = "#ed5353", "Temporada Media" = "#f7ae77", "Temporada Baja" = "#f5f786")

# Gráfico de barras para la frecuencia de reservas por mes
barplot(freq_meses_ordenada$frecuencia, names.arg = freq_meses_ordenada$mes,
        main = "Frecuencia de Reservas por Mes",
        xlab = "Mes", ylab = "Frecuencia",
        col = ifelse(meses %in% temporada_alta, colores["Temporada Alta"],
                     ifelse(meses %in% temporada_baja, colores["Temporada Baja"], colores["Temporada Media"])))

# Frecuencia de reservas por mes para cada tipo de hotel
freq_meses_hotel_1 <- table(data.limpia$arrival_date_month[data.limpia$hotel == "Resort Hotel"])
freq_meses_hotel_2 <- table(data.limpia$arrival_date_month[data.limpia$hotel == "City Hotel"])

# Gráficos de barras para cada tipo de hotel
barplot(freq_meses_hotel_1, names.arg = names(freq_meses_hotel_1),
        main = "Frecuencia de Reservas por Mes - Resort Hotel",
        xlab = "Mes", ylab = "Frecuencia",
        col = ifelse(names(freq_meses_hotel_1) %in% temporada_alta, colores["Temporada Alta"],
                     ifelse(names(freq_meses_hotel_1) %in% temporada_baja, colores["Temporada Baja"], colores["Temporada Media"])))

barplot(freq_meses_hotel_2, names.arg = names(freq_meses_hotel_2),
        main = "Frecuencia de Reservas por Mes - City Hotel",
        xlab = "Mes", ylab = "Frecuencia",
        col = ifelse(names(freq_meses_hotel_2) %in% temporada_alta, colores["Temporada Alta"],
                     ifelse(names(freq_meses_hotel_2) %in% temporada_baja, colores["Temporada Baja"], colores["Temporada Media"])))



#4. ¿Cuándo es menor la demanda de reservas?


# Crear tabla de frecuencias por mes y tipo de hotel
freq_meses_hotel <- data.limpia %>%
  group_by(hotel, arrival_date_month) %>%
  summarize(frecuencia = n()) %>%
  ungroup()

# Convertir los meses a factor para orden correcto
freq_meses_hotel$arrival_date_month <- factor(freq_meses_hotel$arrival_date_month, 
                                              levels = month.name)

# Gráfico de barras para ambos hoteles
ggplot(freq_meses_hotel, aes(x = arrival_date_month, y = frecuencia, fill = hotel)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frecuencia de Reservas por Mes para cada Tipo de Hotel",
       x = "Mes", y = "Frecuencia") +
  scale_fill_manual(values = c("Resort Hotel" = "#98deed", "City Hotel" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Identificar el mes con menor demanda para cada hotel
mes_menor_demanda <- freq_meses_hotel %>%
  group_by(hotel) %>%
  filter(frecuencia == min(frecuencia)) %>%
  select(hotel, arrival_date_month, frecuencia)

# Mostrar el mes con menor demanda para cada hotel
print(mes_menor_demanda)


#5. ¿Cuántas reservas incluyen niños y/o bebés?


# Calcular reservas con y sin niños/bebés para el conjunto de datos total
reservas_con_ninos <- sum(data.limpia$children > 0 | data.limpia$babies > 0)
reservas_sin_ninos <- nrow(data.limpia) - reservas_con_ninos

# Crear un vector con los datos calculados
datos_reservas <- c(Con_Niños_Bebés = reservas_con_ninos, Sin_Niños_Bebés = reservas_sin_ninos)

# Crear un gráfico de barras para visualizar el número de reservas con y sin niños/bebés
barplot(datos_reservas, 
        main = "Reservas con y sin Niños/Bebés",
        ylab = "Número de Reservas",
        col = c("skyblue", "lightgreen"),
        border = "black")

# Ahora lo hacemos por tipo de hotel
# Agrupar por hotel y calcular reservas con y sin niños/bebés
reservas_hotel_con_ninos <- data.limpia %>%
  group_by(hotel) %>%
  summarise(
    Con_Niños_Bebés = sum(children > 0 | babies > 0),
    Sin_Niños_Bebés = n() - sum(children > 0 | babies > 0)
  )

# Crear un gráfico de barras para visualizar por hotel
reservas_hotel_con_ninos_melt <- reshape2::melt(reservas_hotel_con_ninos, id.vars = "hotel")

ggplot(reservas_hotel_con_ninos_melt, aes(x = hotel, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Reservas con y sin Niños/Bebés por Hotel", x = "Hotel", y = "Número de Reservas") +
  scale_fill_manual(values = c("Con_Niños_Bebés" = "skyblue", "Sin_Niños_Bebés" = "lightgreen")) +
  theme_minimal()




#6. ¿Es importante contar con espacios de estacionamiento?

calcularUsoEstacionamiento <- function(dataFrame) {
  # Agrupar por espacios de estacionamiento requeridos y tipo de hotel
  parkingGroup <- group_by(dataFrame, hotel, required_car_parking_spaces)
  
  # Calcular el porcentaje de uso de estacionamiento para cada grupo
  parkingCount <- summarise(parkingGroup, 
                            parkingUsed = n() / nrow(dataFrame) * 100)
  
  # Mostrar los resultados
  print(parkingCount)
  
  # Crear la visualización con ggplot
  ggplot(parkingCount, aes(x = required_car_parking_spaces, y = parkingUsed, fill = hotel)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Uso del estacionamiento por tipo de hotel", 
         x = "Espacios de estacionamiento requeridos", 
         y = "Porcentaje (%)") +
    scale_y_continuous(limits = c(0, 100)) +
    theme_minimal()
}

calcularUsoEstacionamiento(data.limpia)


#7. ¿En qué meses del año se producen más cancelaciones de reservas?

# Asegurar que is_canceled es de tipo numérico (0 o 1)
data.limpia$is_canceled <- as.numeric(as.character(data.limpia$is_canceled))

# Crear un vector con los nombres de los meses en el orden correcto
nombre_meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", 
                  "agosto", "septiembre", "octubre", "noviembre", "diciembre")

# Agrupar por mes y hotel, y contar el número de cancelaciones
cancelsMonth <- data.limpia %>%
  mutate(month = factor(format(as.Date(reservation_status_date), "%m"), 
                        levels = sprintf("%02d", 1:12), labels = nombre_meses)) %>%
  group_by(month, hotel) %>%
  summarize(amountOfCanceled = sum(is_canceled, na.rm = TRUE))

# Mostrar el dataframe con las cancelaciones por mes y hotel
print(cancelsMonth)

# Visualización de las cancelaciones por mes y hotel
ggplot(data = cancelsMonth, aes(x = month, y = amountOfCanceled, color = hotel)) +
  geom_point() + 
  geom_line(aes(group = hotel)) +
  labs(title = "Cancelaciones por Mes y Tipo de Hotel", x = "Mes", y = "Cantidad de Cancelaciones") +
  theme_minimal()



#-------GUARDAR DATASET----------
write.csv(data, file = "hotel_bookings_modificados.csv", na="NA",row.names=FALSE)
