library(readxl)
library(agricolae)
library(ggplot2)
library(ggResidpanel)
library(tidyverse)
library(readxl)

#1 Descargue los datos en su computadora y cree un Proyecto con el nombre “Tarea2”.
#2 Crear un archivo (R script) llamado Tarea2.R 
#3 Cargar los datos de Excel y crear un objeto llamado “data

data <- read_excel("data_Plasmopara_All_trt.xlsx")

#4 Usando str(data) observe que la variable bloque es una variable numérica. Transforme esta variable
#numérica en un factor. Además, observe que los valores de la variable de respuesta están en porcentaje.
#Transfórmelos a proporción.

str(Data)

data <- 
  data %>% 
  mutate(trt = factor(trt),
         bloque = factor(bloque),
         ia_h  = (ia_h/100))

#5 Grafique los datos utilizando ggplot2. Coloque el tiempo en eje “x”, la variable de respuesta en eje “y”.
#Utilice facet_grid() para incluir multipanels con los tratamientos en columnas y los bloques en hileras.
#Rotule apropiadamente los Bloques y los ejes en el gráfico.


data %>% 
  ggplot(aes(x=dia, y=ia_h)) +
  geom_point() + geom_line() +
  facet_grid(bloque~trt,
             labeller = labeller(
             bloque = c("1" = "Bloque 1",
                                   "2" = "Bloque 2",
                                   "3" = "Bloque 3",
                                   "4" = "Bloque 4"))) +
           labs(x="Dias", y="Incidencia en hojas (%)")
  
#6 Ajuste la escala del eje “y” para que tenga un rango de valores entre 0 y 1. (Pista: use
#scale_y_continuous() )

data %>%
ggplot(aes(x=dia, y=ia_h)) +
  geom_point() + geom_line() +
  facet_grid(bloque~trt,
             labeller = labeller(
               bloque = c("1" = "Bloque 1",
                          "2" = "Bloque 2",
                          "3" = "Bloque 3",
                          "4" = "Bloque 4"))) +
  labs(x="Días", y="Incidencia en hojas") +
  scale_y_continuous(limits = c(0, 1))

#7 Utilizando library(agricolae) calcule el área bajo la curva para cada una de las curvas del gráfico anterior.
#Para esto debe agrupar los datos por tratamiento y bloque. Asigne este resultado a un objeto llamado
#data_audpc.

library(agricolae)

audpc_data <-
  data %>%
  group_by(trt, bloque) %>%
  summarise(AUDPC = audpc(ia_h,dia)%>% round(2))

audpc_data

#8 Ahora calcule el área bajo la curva agrupando los datos por tratamiento. Asigne este resultado a un
#objeto llamado data_audpc2

audpc2_data <-
  data %>%
  group_by(trt) %>% 
  summarise(AUDPC = audpc(ia_h, dia) %>% round(2))

audpc2_data

#9 Ahora realice un gráfico con los valores de incidencia promedio para cada tratamiento. (Pista: Primero
#debe agrupar los datos por dia y tratamiento y crear una nueva columna con los valores promedio usando
#mutate() ). En el momento de crear el gráfico utilice facet_wrap() y rotule apropiadamente los ejes “x”
#y “y”. Además, ajuste el eje “y” para los valores tengan un rango de 0 a 1. Agregue el siguiente título al
#gráfico: “Valores promedio de incidencia de mildiú velloso de la vid en follaje”

data_prom <- 
  data %>% 
  group_by(dia, trt) %>% mutate(inc_promedio = mean(ia_h))

head(data_prom)

data_prom %>% 
  ggplot(aes(x= dia, y= inc_promedio)) + 
  geom_point() +
  geom_line() +
  facet_wrap(.~trt) + 
  labs(x="Días", y="Incidencia en hojas",
       title = "Valores promedio de incidencia de mildiú velloso de la vid en follaje") +
  scale_y_continuous(limits = c(0, 1))

#10 Agregue los valores de AUDPC a cada panel del gráfico anterior usando geom_text.

ann_text <- data.frame(dia = 40, 
                       inc_promedio = .90,
                       trt = c("Fungicida_A", "Fungicida_B", "Fungicida_C",
                               "Fungicida_D", "Fungicida_E", "Testigo"))
ann_text

data_prom %>% 
  ggplot(aes(x=dia, y=inc_promedio)) + 
  geom_point() +
  geom_line() +
  facet_wrap(.~trt) + 
  labs(x="Días", y="Incidencia en hojas",
       title = "Valores promedio de incidencia de mildiú velloso de la vid en follaje") +
  scale_y_continuous(limits = c(0,1)) + 
  geom_text(data = ann_text,label =  c("AUDPC = 41.5 %*dias",
                                       "AUDPC = 25.5 %*dias",
                                       "AUDPC = 40.4 %*dias",
                                       "AUDPC = 42.0 %*dias",
                                       "AUDPC = 30.0 %*dias", 
                                       "AUDPC = 45.9 %*dias"), size=6)
                                      
#11 Cree el siguiente modelo lineal: AUDPC ~ trt + bloque. Ahora obtenga un análisis de varianza. ¿Se
#observaron diferencias significativas entre tratamientos? (Pista: utilice lm() para crear el modelo lineal y
#después use anova() )

left_join(data_prom, audpc2_data, by = "trt") %>%
  mutate(trt = 
           paste(trt, "(AUDPC =", AUDPC,"%*dias)")) %>% 
  ggplot(aes(x=dia, y=inc_promedio, group=trt, shape=trt)) + 
  geom_point(aes(y=inc_promedio),size=2) +
  geom_line() +
  labs(x="Días", y="Incidencia en hojas (%)") +
  scale_y_continuous(limits = c(0,1)) +
  theme_classic() +
  theme(legend.position = c(0.75, 0.1),legend.title = element_blank())






#DEL 13 EN ADELANTE ES COPIAR Y PEGAR

