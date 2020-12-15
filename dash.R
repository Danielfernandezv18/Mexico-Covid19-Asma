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
library(plotly)
library(devtools)
library("mxmaps")


# Se usarán  los datos del repositrorio de github de carranco-sga de los datos de covid para México actualizados el día 8 de diciembre.


# Descarga y extracción de los datos
#download.file(url="https://github.com/carranco-sga/Mexico-COVID-19/raw/master/Open_data/COVID-19/202012/20201208.zip", destfile="covid19_gobmx.zip")
 # unzip(zipfile = "covid19_gobmx.zip")

# Lectura de datos
covid19.Mexico <- read.csv("201208COVID19MEXICO.csv", header = TRUE, sep = ",", na.strings = "")

head(covid19.Mexico)



# Descarga y extraccion de diccionarios de terminos que se guardan en tu directorio de trabajo
#download.file(url="https://github.com/carranco-sga/Mexico-COVID-19/raw/master/Open_data/COVID-19/202012/diccionario_20201208.zip", destfile="diccionario_gobmx.zip")
 # unzip(zipfile = "diciionario_gobmx.zip")

  
# Tendremos en cuenta los casos confirmados por SARS-CoV2 que indica la columna de Clasificacion_final y seleccionaremos las variables de interés.
covid.mx <- covid19.Mexico %>%
  filter(CLASIFICACION_FINAL == 3 | CLASIFICACION_FINAL == 1) %>% # Seleccion de casos confirmados
  select(ENTIDAD_RES, TIPO_PACIENTE, FECHA_INGRESO, FECHA_SINTOMAS, FECHA_DEF, ASMA) # Eleccion de variables de interes


# Cambio de formato de las columnas a fechas
covid.mx %<>%
  mutate(FECHA_SINTOMAS=as.Date(covid.mx$FECHA_SINTOMAS, format = "%Y-%m-%d")) %<>%
  mutate(FECHA_DEF=as.Date(covid.mx$FECHA_DEF, format = "%Y-%m-%d")) %<>%
  mutate(FECHA_INGRESO=as.Date(covid.mx$FECHA_INGRESO, format = "%Y-%m-%d"))


# Creación de documento de datos tidy
write.table(covid.mx, file = 'Covid19_asma_gobmx.csv', sep = ",")


# agrupar por fechas y obtener el numero de casos registrados en el dia
x <- covid.mx %>%
  filter(ASMA==1) %>%
  select(FECHA_SINTOMAS, ASMA) %$%
  aggregate(ASMA ~ FECHA_SINTOMAS, FUN=sum)

# Se hará uso de la variable para poder manejar una barra en las fechas
dateWindow <- c("2019-12-31", "2020-12-10")

# Gráfica para evolución de casos confirmados
x %>%
  select(ASMA) %>%
  xts(order.by = x$FECHA_SINTOMAS) %>%
  dygraph(x$ASMA, main = "Evolución de casos confirmados en México") %>%
  dyAxis("x", label="Fechas") %>%
  dyRoller(showRoller = TRUE, rollPeriod = 7) %>%
  dyRangeSelector(dateWindow=dateWindow)


# agrupar por fechas y obtener el numero de decesos registrados en el dia
y <- covid.mx %>%
  filter(is.na(covid.mx$FECHA_DEF)==FALSE) %>%
  filter(ASMA==1) %>%
  select(FECHA_DEF, ASMA) %$%
  aggregate(ASMA ~ FECHA_DEF, FUN=sum)

# Gráfica para evolución de decesos
y %>%
  select(ASMA) %>%
  xts(order.by = y$FECHA_DEF) %>%
  dygraph(y$ASMA, main = "Evolución de decesos confirmados en México") %>%
  dyAxis("x", label="Fechas") %>%
  dyRoller(showRoller = TRUE, rollPeriod = 7) %>%
  dyRangeSelector(dateWindow=dateWindow)



# Contar casos confirmados que padecen asma o no
df<- covid.mx$ASMA %>%
  count() %>%
  mutate(porcentaje = freq/sum(freq)*100)

head(df)


# Gráfica de proporcion de confirmados por covid que padecen asma
df1 <- data.frame("ASMA" = c("Si", "No", "No_esp"), "CASOS" = df$freq)

a <- ggplot(df1, aes(x="", y=CASOS, fill=ASMA)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_brewer(palette="Set1")
a



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


# Gráfica de proporción de casos hospitalizados con padecimiento de asma
df2 <- data.frame("ASMA" = c("No", "Si"), "CASOS" = asma.inter$ASMA)

b <- ggplot(df2, aes(x="", y=CASOS, fill=ASMA)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_brewer(palette="Set1")
b




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



# gráfica de proporción de decesos con asma
df4 <- data.frame("ASMA" = c("Si", "No"), "CASOS" = c(last(asma.def$CASOS), (def$freq[1]-last(asma.def$CASOS))))

d <- ggplot(df4, aes(x="", y=CASOS, fill=ASMA)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_brewer(palette="Set1")
d



# Muestra el número de decesos de personas que fueron hospitalizadas y padecían asma
asma.def.int <- covid.mx %>%
  filter(is.na(covid.mx$FECHA_DEF)==FALSE) %>%
  filter(ASMA==1 & TIPO_PACIENTE==2) %>%
  select(FECHA_DEF, ASMA) %$%
  aggregate(ASMA ~ FECHA_DEF, FUN=sum) %>%
  mutate(CASOS = cumsum(ASMA))

# Indica el total de los decesos de personas que fueron hospitalizadas y padecían asma
last(asma.def.int$CASOS)


# Proporción de decesos que padecian asma y estuvieron hospitalizados
df3 <- data.frame("Hospitalizado" = c("Si", "No"), "CASOS" = c(last(asma.def.int$CASOS), (last(asma.def$CASOS)-last(asma.def.int$CASOS))))

c <- ggplot(df3, aes(x="", y=CASOS, fill=Hospitalizado)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_brewer(palette="Set1")
c



# Creación de un dataframe para ajustar fechas y realizar algunas gráficas
dia1 = as.Date("2019-12-31",format="%Y-%m-%d")
dia2 = as.Date("2020-12-08",format="%Y-%m-%d")

# Se puede reutilzar y ajustar a necesidades
u <- data.frame(
  "FECHA_SINTOMAS" = seq(dia1,dia2, by="day"),
  "ASMA" = seq(0, by=length(seq(dia1,dia2, by="day")))
)

e <- data.frame(
  "FECHA_INGRESO" = seq(dia1,dia2, by="day"),
  "CASOS_HOS" = seq(0, by=length(seq(dia1,dia2, by="day")))
)

f <- data.frame(
  "FECHA_DEF" = seq(dia1,dia2, by="day"),
  "CASOS_DEF" = seq(0, by=length(seq(dia1,dia2, by="day")))
)


#Casos con asma que fueron hospitalizados agrupados por fecha
asma.inter.fecha <- covid.mx %>%
  mutate(CASOS_HOS=as.integer(1)) %>%
  filter(ASMA==1) %>%
  filter(TIPO_PACIENTE==2) %>%
  select(FECHA_INGRESO, CASOS_HOS) %>%
  bind_rows(e) %$%
  aggregate(CASOS_HOS ~ FECHA_INGRESO, FUN=sum) %>%
  mutate(TOTAL_HOS=cumsum(CASOS_HOS))


# Muestra el numero de fallecidos que padecian asma
asma.def1 <- covid.mx %>%
  select(FECHA_DEF, ASMA) %>%
  filter(is.na(covid.mx$FECHA_DEF)==FALSE) %>%
  filter(ASMA==1) %>%
  dplyr::rename(CASOS_DEF=ASMA) %>%
  bind_rows(f) %$%
  aggregate(CASOS_DEF ~ FECHA_DEF, FUN=sum) %>%
  mutate(TOTAL_DEF = cumsum(CASOS_DEF))



# agrupar por fechas y obtener el numero de casos registrados en el dia
w <- covid.mx %>%
  filter(ASMA==1) %>%
  select(FECHA_SINTOMAS, ASMA) %>%
  bind_rows(u) %$%
  aggregate(ASMA ~ FECHA_SINTOMAS, FUN=sum) %>%
  mutate(ASMA_TOTAL=cumsum(ASMA))


# Combinación de dataframes
v <- bind_cols(asma.inter.fecha, asma.def1, w)


# Gráficas de relación entre datos
p <- ggplot(v, aes(x=TOTAL_HOS, y=TOTAL_DEF)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth() +          # Add a loess smoothed fit curve with confidence region
  labs(x = "Hospitalizados", y = "Decesos")
ggplotly(p)


s <- ggplot(v, aes(x=ASMA_TOTAL, y=TOTAL_HOS)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth() +          # Add a loess smoothed fit curve with confidence region
  labs(x = "Asmáticos", y = "Hospitalizados")
ggplotly(s)
