
# Adjuntamos la base de datos

NL<-read.csv2(file.choose(),header=TRUE)


# Leemos las fechas y horas (incluyendo huso horario) de nuestra base de datos

NL$Fecha<-NL$Fecha+ 20000000
NL$Hora<-NL$Hora/100
NL$date<-paste(NL$Fecha, NL$Hora,sep = " ", collapse = NULL)
NL$date<-as.POSIXct(strptime(NL$date, format = "%Y%m%d %H",tz="Etc/GMT+4"))

# Para hacer el resumen de todos los datos
summary(NL)
#Para realizar un grafico de metrica para cada uno de los años  2017-2023

#La funcion agregate es para hacer los promedios diarios y na,rm eliminan los datos faltantes de la tabla y hacer las medias de cada dia

PM5clean <- aggregate(NL["pm25"], format(NL["date"],"%Y-%j"), mean, na.rm = TRUE)
Pm10clean <- aggregate(NL["pm10"], format(NL["date"],"%Y-%j"), mean, na.rm = TRUE)
# Sale por dia consecutivo 1-2017, 2-2017 

#Otra via para hallar los promedios diarias
NL<-timeAverage(NL[, -c(1)], avg.time = "day", data.thresh = 75, statistic = "mean",
                   start.date = "2017-01-01", end.date = "2024-12-31")
# Graficar Arreglar los nombres de las variables
timePlot(NL_diario, pollutant=c("pm25","pm10"), y.relation="free", lwd=3, lty=1)
# Hacer un nuevo data friends que tenga  cada uno de los promedios diarios NL_diario

NL_diario <- NL %>%
  group_by(date) %>%
  summarise(
    Pm2.5diario = mean(pm25, na.rm = TRUE),
    Pm10diario = mean(pm10, na.rm = TRUE))

# Para graficar

ggplot(NL_diario_long, aes(x = date, y = Concentracion, color = Contaminante)) +
  geom_line(size = 0.8) +  
  geom_hline(yintercept = 50, linetype = "dashed", color = "red", size = 0.8) +  # Límite PM2.5
  geom_hline(yintercept = 130, linetype = "dashed", color = "mediumblue", size = 0.8) +  # Límite PM10
  scale_color_manual(values = c("Pm2.5diario" = "lightcoral", "Pm10diario" = "lightblue")) +  
  labs(title = "Métricas diaria de MP2,5 y MP10 en Santiago (2017-2024)",
       x = "Fecha", y = "Concentración (µg/m³)", color = "Contaminante") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

# Para hacer prpmedios anuales 
# Primera via

NL_anual <-NL<-timeAverage(NL, avg.time = "year", data.thresh = 75, statistic = "mean",
                      start.date = "2016-01-01", end.date = NA, fill = FALSE)

# Otra via
PM25cleananual <- aggregate(NL["pm25"], format(NL["date"],"%Y"), mean, na.rm = TRUE)
Pm10cleananual <- aggregate(NL["pm10"], format(NL["date"],"%Y"), mean, na.rm = TRUE)
#Para graficar anual

timePlot(NL_anual, pollutant=c("pm25","pm10"), y.relation="free", lwd=3, lty=1)


ggplot(NL_anual, aes(x = date, y = Concentracion, color = Contaminante)) +
  geom_line(size = 1.5) +  # Ajusta el grosor de las líneas
  scale_color_manual(values = c("pm25" = "lightcoral", "pm10" = "lightblue")) +  
  labs(title = "Métricas diarias de MP2,5 y MP10 en Santiago (2017-2024)",
       x = "Fecha", y = "Concentración (µg/m³)", color = "Contaminante") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

#La norma primaria de calidad de aire para MP10 es 150 ug/m3 como concentración diaria y 50 ug/m3 como concentrión anual.
##La norma primaria de calidad de aire para MP2.5 es 50 ug/m3 como concentración diaria y 20 ug/m3 como concentración anual.


Inciso b 
# Aplicar el análisis de tendencia para PM2.5
trend_pm25 <- trend(data(NL), pollutant="pm25", method="TheilSen")

install.packages("trend")

install.packages("opinAr")

library(openair)
trend_pm25 <- trend(data(NL), pollutant="pm25", method="TheilSen")
