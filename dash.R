library(readr)
library(xts)
library(dygraphs)
library(dplyr)
library(magrittr)
library(lubridate)
library(forecast)
library(TTR)
library(readxl)
library(plyr)
library(utils)
library(ggplot2)
library(devtools)
library("mxmaps")


# Se usarán  los datos del repositrorio de github de carranco-sga de los datos de covid para México actualizados el día 8 de diciembre.


# Descarga y extracción de los datos
download.file(url="https://github.com/carranco-sga/Mexico-COVID-19/raw/master/Open_data/COVID-19/202012/20201208.zip", destfile="covid19_gobmx.zip")
  unzip(zipfile = "covid19_gobmx.zip")

# Lectura de datos
covid19.Mexico <- read.csv("201208COVID19MEXICO.csv", header = TRUE, sep = ",", na.strings = "")

View(covid19.Mexico)



# Descarga y extraccion de diccionarios de terminos que se guardan en tu directorio de trabajo
download.file(url="https://github.com/carranco-sga/Mexico-COVID-19/raw/master/Open_data/COVID-19/202012/diccionario_20201208.zip", destfile="diccionario_gobmx.zip")
  unzip(zipfile = "diciionario_gobmx.zip")

  
# Tendremos en cuenta los casos confirmados por SARS-CoV2 que indica la columna de Clasificacion_final y seleccionaremos las variables de interés.
covid.mx <- covid19.Mexico %>%
  filter(CLASIFICACION_FINAL == 3 | CLASIFICACION_FINAL == 1) %>% # Seleccion de casos confirmados
  select(ENTIDAD_RES, TIPO_PACIENTE, FECHA_SINTOMAS, FECHA_DEF, ASMA) # Eleccion de variables de interes


# Cambio de formato de las columnas a fechas
covid.mx %<>%
  mutate(FECHA_SINTOMAS=as.Date(covid.mx$FECHA_SINTOMAS, format = "%Y-%m-%d")) %<>%
  mutate(FECHA_DEF=as.Date(covid.mx$FECHA_DEF, format = "%Y-%m-%d"))


# Creación de documento de datos tidy
write.table(covid.mx, file = 'Covid19_asma_gobmx.csv', sep = ",")


# agrupar por fechas y obtener el numero de casos registrados en el dia
x <- covid.mx %>%
  mutate(CASOS=as.integer(1)) %>%
  select(FECHA_SINTOMAS, CASOS) %$%
  aggregate(CASOS ~ FECHA_SINTOMAS, FUN=sum)

# Se hará uso de la variable para poder manejar una barra en las fechas
dateWindow <- c("2019-12-31", "2020-12-10")

# Gráfica para evolución de casos confirmados
x %>%
  select(CASOS) %>%
  xts(order.by = x$FECHA_SINTOMAS) %>%
  dygraph(x$CASOS, main = "Evolución de casos confirmados en México") %>%
  dyAxis("x", label="Fechas") %>%
  dyRoller(showRoller = TRUE, rollPeriod = 7) %>%
  dyRangeSelector(dateWindow=dateWindow)


# agrupar por fechas y obtener el numero de decesos registrados en el dia
y <- covid.mx %>%
  mutate(DECESOS=as.integer(1)) %>%
  select(FECHA_DEF, DECESOS) %>%
  filter(is.na(covid.mx$FECHA_DEF)==FALSE) %$%
  aggregate(DECESOS ~ FECHA_DEF, FUN=sum)

# Gráfica para evolución de decesos
y %>%
  select(DECESOS) %>%
  xts(order.by = y$FECHA_DEF) %>%
  dygraph(y$DECESOS, main = "Evolución de decesos confirmados en México") %>%
  dyAxis("x", label="Fechas") %>%
  dyRoller(showRoller = TRUE, rollPeriod = 7) %>%
  dyRangeSelector(dateWindow=dateWindow)



# Contar casos confirmados que padecen asma o no
df<- covid.mx$ASMA %>%
  count() %>%
  mutate(porcentaje = freq/sum(freq)*100)

head(df)


# Contar por entidades el numero de casos confirmado con asma
asma.ent <- covid.mx %>%
  select(ENTIDAD_RES, ASMA) %>%
  filter(ASMA==1) %$%
  aggregate(ASMA ~ ENTIDAD_RES, FUN=sum)

head(asma.ent)

# Mapa de México para ver los estados con casos positivos y asma
asma.ent %>%
  mutate(region=ENTIDAD_RES, value=ASMA) %>%
  mxstate_choropleth(asma.ent, title="Estados con casos positivos por covid y padecimietno de asma")



# casos en los que padecen asma fueron internados
# TIPO_PACIENTE 2 indica que la persona fue hospitalizado
inter <- covid.mx$TIPO_PACIENTE %>%
  count() %>%
  mutate(porcentaje = freq/sum(freq)*100)


head(inter)



# casos en los que padecen asma fueron internados
# TIPO_PACIENTE 2 indica que la persona fue hospitalizado
asma.inter <- covid.mx %>%
  select(TIPO_PACIENTE, ASMA) %>%
  filter(ASMA==1) %$%
  aggregate(ASMA ~TIPO_PACIENTE, FUN=sum) %>%
  mutate(PORCENTAJE = ASMA/sum(ASMA)*100)

head(asma.inter)


# se creo un dataframe con los valores requerios
r <- data.frame(
  "Ambulatorio" = c(inter$freq[1], asma.inter$ASMA[1]),
  "Hospitalizado" = c(inter$freq[2], asma.inter$ASMA[2])
)

# graficar comparacion entre el total de hospitalizados y hospitalizados con asma
barplot(r$Hospitalizado, main = "Casos psotivos por covid19 hospitalizados",
        names.arg  = c("Total", "Presenta sintomas de asma"),
        col = c("royalblue", "seagreen"))


# Indica con FALSE los casos de covid que fallecieron
def <- is.na(covid.mx$FECHA_DEF) %>%
  count() %>%
  mutate(porcentaje = freq/sum(freq)*100)

head(def)


# Muestra el número de decesos que padecían asma
asma.def <- covid.mx %>%
  select(FECHA_DEF, ASMA) %>%
  filter(is.na(covid.mx$FECHA_DEF)==FALSE) %>%
  filter(ASMA==1) %$%
  aggregate(ASMA ~ FECHA_DEF, FUN=sum) %>%
  mutate(CASOS = cumsum(ASMA))

# Se indica el total de decesos de casos que padecían asma
last(asma.def$CASOS)


# se creo un dataframe con los valores requerios
s <- data.frame(
  "DEF" = c(def$freq[1], last(asma.def$CASOS))
)

# grafica con el total de muertos y los muertos que padecian asma
barplot(s$DEF, main = "Fallecimientos de casos positovos por covid19",
        names.arg  = c("Total", "Presenta sintomas de asma"),
        col = c("royalblue", "seagreen"))


# Muestra el número de decesos de personas que fueron hospitalizadas y padecían asma
asma.def.int <- covid.mx %>%
  filter(is.na(covid.mx$FECHA_DEF)==FALSE) %>%
  filter(ASMA==1 & TIPO_PACIENTE==2) %>%
  select(FECHA_DEF, ASMA) %$%
  aggregate(ASMA ~ FECHA_DEF, FUN=sum) %>%
  mutate(CASOS = cumsum(ASMA))

# Indica el total de los decesos de personas que fueron hospitalizadas y padecían asma
last(asma.def.int$CASOS)


# se creo un dataframe con los valores requerios
u <- data.frame(
  "DEF" = c(last(asma.def$CASOS), last(asma.def.int$CASOS))
)

# grafica comparando el total de muertos con asma y muestos con asma que fueron hospitalizados
barplot(u$DEF, main = "Fallecimientos con covid19 y padecimiento de asma", names.arg  = c("Total", "Hospitalizados"),
        col = c("royalblue", "red"))