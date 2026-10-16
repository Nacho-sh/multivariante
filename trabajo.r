library("summarytools")
library("ggplot2")
library("ggpubr")
library("foreign")
library("readxl")
library("archdata")
library("psych")
library("factoextra")
library("scatterplot3d")
library("tidyr")


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
freq(SLT$glaucoma) #los datos quedan mucho mas resumidos
# podemos estudiar la evolución media del PIO de cada glaucoma a lo largo del tiempo. A menor PIO, mayor mejoría
#esta parte yo no se hacerla, lo he estado intentando un rato y no termino de entender por que es tan complicada 
#y larga de hacer. copiado de deepseek
medias_evolucion <- data.frame(
  Pre      = tapply(SLT$piopre,  SLT$glaucoma, mean, na.rm = TRUE), #na.rm ignora los NA
  Sem1     = tapply(SLT$piosem, SLT$glaucoma, mean, na.rm = TRUE),
  Mes1     = tapply(SLT$piomes, SLT$glaucoma, mean, na.rm = TRUE),
  Mes3     = tapply(SLT$pio3mes, SLT$glaucoma, mean, na.rm = TRUE)
)
medias_evolucion
# 1. Convertir rownames a columna
medias_evolucion$glaucoma <- rownames(medias_evolucion)

# 2. Pasar de ancho a largo con stack() o reshape
medias_largo <- reshape(
  medias_evolucion,
  varying = c("Pre", "Sem1", "Mes1", "Mes3"),
  v.names = "media",
  timevar = "momento",
  times   = c("Pre", "Sem1", "Mes1", "Mes3"),
  direction = "long"
)

# 3. Quitar la columna "id" que añade reshape
medias_largo$id <- NULL
rownames(medias_largo) <- NULL
medias_largo
medias_largo$momento <- factor(medias_largo$momento,
                               levels = c("Pre", "Sem1", "Mes1", "Mes3"))
ggplot(medias_largo, aes(x = momento, y = media,
                         color = glaucoma, group = glaucoma)) +
  geom_line()
# parece que hay glaucomas que curan mejor que otros, el GPPA a lo mejor necesita mas tratamiento postop
# vamos a ver si existe una correlacion sustancial entre el tipo de glaucoma y la mejoria
# para justamente esto era el test ANOVA

anova(lm(piopre ~ glaucoma, data = SLT))
anova(lm(piosem ~ glaucoma, data = SLT))
anova(lm(piomes ~ glaucoma, data = SLT))
anova(lm(pio3mes ~ glaucoma, data = SLT))
# pues no, parece que en ninguno de los grupos podemos rechazar que la media sea distinta en 
# ningun punto del seguimiento. a los 3 meses estamos a punto de concluir que las medias si que cambian (p=0.058)
# si nos la jugamos, podemos concluir que las medias cambian. yo no me la jugaria, es un resultado borderline





