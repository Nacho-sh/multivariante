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
#es especialmente interesante el PIO al primer mes, asi que vamos a intentar convertirlo, pero antes habra que limpiarlo

SLT$piomes <- gsub("[^0-9.]", "", SLT$piomes) #quitamos todo lo que no sea numeros

SLT$piomes <- as.numeric(SLT$piomes) #ahora convertimos el tipo a numerico sin problemas

#vamos a ver si tenemos una muestra representativa de los distintos glaucomas o si tenemos que ajustar o descartar

freq(SLT$glaucoma) #salvo 4 tipos de glaucomas, el resto solo tienen menos de tres observaciones, pero si nos fijamos,
#muchas de las categorias estan simplemente mal introducidas o son diagnosticos dudosos (como el que tiene "?").
#vamos a optar por limpiar manualmente estas categorias
SLT$glaucoma[SLT$glaucoma == "GPAA"] <- "GPPA"
SLT$glaucoma[SLT$glaucoma == "GPAA?"] <- "GPPA"
SLT$glaucoma[SLT$glaucoma == "SD DISPERSION PIGMENTARIA"] <- "PIGMENTARIO"
SLT$glaucoma[SLT$glaucoma == "GPAA?miópico"] <- "GPPA"
SLT$glaucoma[SLT$glaucoma == "HTO PIGMENTARI"] <- "PIGMENTARIO"
SLT$glaucoma[SLT$glaucoma == "GS PXE"] <- "pseudoexfoliativo"
SLT$glaucoma[SLT$glaucoma == "G.PSEUDOEX"] <- "pseudoexfoliativo"
SLT$glaucoma[SLT$glaucoma == "GLAUCO PIGMENT"] <- "PIGMENTARIO"
SLT$glaucoma[SLT$glaucoma == "CPAC"] <- "GPAC"

# por simplicidad y por falta de representacion, podemos agrupar glaucomas poco comunes como el congenito,
# postrabecular, GSAA o el miopico en una categoria de "otros"
SLT$glaucoma[SLT$glaucoma == "GSAA"] <- "otros"
SLT$glaucoma[SLT$glaucoma == "GLAUCOMA CONGENITO"] <- "otros"
SLT$glaucoma[SLT$glaucoma == "GLAUCOMA POSTRABECULAR"] <- "otros"
SLT$glaucoma[SLT$glaucoma == "miópico"] <- "otros"


