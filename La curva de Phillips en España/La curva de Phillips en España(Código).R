library(tidyverse)

paro <- read.csv(file = "./Paro.csv", 
                 dec = ",",
                 sep = ";")

# Solo necesito las dos últimas columnas y llamo la columna con los datos del paro como 'Paro'
paro <- paro[, 3:4]
colnames(paro)[2] <- "Paro"

glimpse(paro)

ipc <- read.csv("./Inflacion.csv", 
                sep = ";", 
                header = FALSE,
                fill = TRUE)

# Solo necesito las dos últimas columnas, y les pongo los nombres 'Periodo' e 'Inflación'. También, no necesito las dos primeras filas que tienen los nombres del dataframe  y la inflación del enero de 2023
ipc <- ipc[-1:-2, 3:4]
colnames(ipc) <- c("Periodo", "Inflación")

# En España, se usa el coma (',') como delimiter. Sin embargo, R recononoce como delimiter el punto (.). Así que en la fila Inflación, cambio el ',' con el '.'
ipc$Inflación <- str_replace_all(ipc$Inflación, ",", ".")
#Luego, convierto los datos de la fila en números
ipc$Inflación <- as.numeric(ipc$Inflación)

glimpse(ipc)

El periodo está escrito con la forma de: Año, seguido por la letra M y el mes que corresponde. 
# Necesito crear dos nuevas filas, una para el año y otra para el mes de cada dato. 
# Para hacerlo, tengo que separar el texto y asignarlo en dos nuevas columnas, cuyos nombres serán 'Año' y 'Mes'
ipc[, 3:4] <- str_split_fixed(ipc$Periodo, pattern = "M", n = 2)
colnames(ipc)[3:4] <- c("Año", "Mes")

# Cambio el típo de las columnas 
ipc$Año <- as.numeric(ipc$Año)
ipc$Mes <- as.numeric(ipc$Mes)

# Creo un dataframe con los meses de cada trimestre
trimestres <- matrix(1:12, nrow = 3, ncol = 4)

# Creo una función que conventirá el mes al su trimestre (1o, 2o, 3o o 4o)
fn_trimestre <- function(mes){
  resultado <- which(trimestres == mes, arr.ind = TRUE)
  return(resultado[2])
}

# Lo aplico en una nueva columna 
ipc$Trimestre <- sapply(ipc$Mes, fn_trimestre)

# La inflación de cada trimestre, es la media de los tres meses:
ipc <- ipc %>% 
  group_by(Año, Trimestre) %>% 
  summarise("Inflación" = round(mean(Inflación), 2)) %>% 
  arrange(desc(Año), desc(Trimestre))

# Creo una nueva columna que tiene el mismo formato que la columna 'Periodo' del dataframe paro, es decir, el año seguido por T y el trimestre.
ipc$Periodo <- paste(ipc$Año, ipc$Trimestre, sep = "T")

glimpse(ipc)

## Limpieza de los outliers

ipc_paro <- paro %>% 
  select(Periodo, Paro) %>% 
  left_join(ipc, by = "Periodo")

#Empiezo con los datos del paro 
boxplot(ipc_paro$Paro, outcol = "red")

# Examino también los datos de la Inflación, la cual ha subido extremadamente los últimos meses del 2022
boxplot(ipc_paro$Inflación, outcol = "red")

# Eliminación de los outliers
mean <- mean(ipc_paro$Inflación)
std <- sd(ipc_paro$Inflación)

Tmin <- mean-(3*std)
Tmax <- mean+(3*std)

ipc_paro <- subset(ipc_paro, ipc_paro$Inflación > Tmin & ipc_paro$Inflación < Tmax)
glimpse(ipc_paro)

## Gráfico

# Para que sea mejor mostrado el gráfico, pongo una columna de formato `Date`. Como no existe función para formato de año y trimestres (sí que existe para cuartos), pondré la fecha como año y mes. En el gráfico solo quiero mostrar el año.
ipc_paro$Dates <- as.Date(paste(as.character(ipc_paro$Año), 
                                as.character(ipc_paro$Trimestre), "1", 
                                sep = "-"), 
                          "%Y-%m-%d")

glimpse(ipc_paro)

ggplot(data = ipc_paro, aes(x = Dates))+
  geom_line(aes(y = Paro), colour = "blue")+
  geom_line(aes(y = Inflación), colour = "red")+
  # Marco los años 2008 y 2012 por la crisis económica
  geom_vline(xintercept = as.Date("2008-1-1", "%Y-%m-%d"), color = "black", linewidth = 0.2)+
  geom_vline(xintercept = as.Date("2012-4-1", "%Y-%m-%d"), color = "black", linewidth = 0.2)+
  scale_y_continuous(name = "Paro",
                     sec.axis = sec_axis(trans = ~., name = "Inflación"))+
  labs(title = "Curva de Phillips en España",)+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5))