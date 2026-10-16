library("summarytools")
library("ggplot2")
library("ggpubr")
library("foreign")
library("readxl")
library("archdata")
library("psych")
library("factoextra")
library("scatterplot3d")

SLT <- read_excel("Tarea_Glaucoma_DB.xlsx", sheet = "SLT")

class(SLT) #es dataframe

names(SLT) <- c("fechaslt", "sexo", "edad", "ojo", "glaucoma", "nimpactos", "cuadrantes", 
                "enerimpac", "enertotal", "cirugias", "piopre", "piosem", "piomes", "pio3mes", 
                "medprev", "medmes", "med3mes", "dolor")

sapply(SLT, class) #vemos de que tipo es cada variable
#Tenemos 8 variables numéricas. Además, el número de cuadrantes realizados, energía por impacto y 
#PIO el 1 mes se pueden convertir a numericos tambien. 
#es especialmente interesante el PIO al primer mes, asi que vamos a intentar convertirlo 



