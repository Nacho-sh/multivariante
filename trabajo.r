library("summarytools")
library("ggplot2")
library("ggpubr")
library("foreign")
library("readxl")
library("archdata")
library("psych")
library("factoextra")
library("scatterplot3d")

datos <- read_excel("Tarea_Glaucoma_DB.xlsx", sheet = "SLT")
summary(datos)

#glaucoma y sexo: no hemos encontrado ninguna relacion relevante
glaucoma <- data.frame(Sexo = datos$SEXO, Tipo = datos$`TIPO GLAUCOMA`) #inicializamos la variable con la que trabajamos

glaucoma <- glaucoma[!is.na(glaucoma$Sexo), ] #limpiamos las filas que no tienen aclarado el sexo

attach(glaucoma) #inicializamos TIpo y Sexo
table(Tipo, Sexo)
table(Sexo)
table(Tipo)
freq(Sexo)
freq(Tipo)
round(prop.table(table(Tipo, Sexo), 2), 2)



