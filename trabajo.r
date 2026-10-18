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

SLT$piomes <- as.character(SLT$piomes)
SLT$piomes[SLT$piomes == ""] <- NA            # vacíos -> NA
SLT$piomes <- gsub(",", ".", SLT$piomes)      # comas decimales -> punto
SLT$piomes <- sub("[^0-9.].*$", "", SLT$piomes)  # corta desde el primer carácter raro
SLT$piomes <- as.numeric(SLT$piomes) #convertimos a numero

#vamos a ver si tenemos una muestra representativa de los distintos glaucomas o si tenemos que ajustar o descartar

freq(SLT$glaucoma) #salvo 4 tipos de glaucomas, el resto solo tienen menos de tres observaciones, pero si nos fijamos,
#muchas de las categorias estan simplemente mal introducidas o son diagnosticos dudosos (como el que tiene "?").
#vamos a optar por limpiar manualmente estas categorias
SLT$glaucoma[SLT$glaucoma == "GPPA"] <- "GPAA"
SLT$glaucoma[SLT$glaucoma == "GPAA?"] <- "GPAA"
SLT$glaucoma[SLT$glaucoma == "SD DISPERSION PIGMENTARIA"] <- "PIGMENTARIO"
SLT$glaucoma[SLT$glaucoma == "GPAA?miópico"] <- "GPAA"
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
# Convertir rownames a columna
medias_evolucion$glaucoma <- rownames(medias_evolucion)

# Pasar de ancho a largo con stack() o reshape
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

# vamos a dar otro enfoque. veamos si hay outliers en las distintas categorias
# podemos verlo con los distintos PIO para cada glaucoma

SLT$id <- 1:nrow(SLT)

SLT_largo <- SLT %>%
  pivot_longer(
    cols = c(piopre, piosem, piomes, pio3mes),
    names_to = "momento",
    values_to = "pio"
  ) %>%
  mutate(momento = factor(momento,
                          levels = c("piopre", "piosem", "piomes", "pio3mes"),
                          labels = c("Pre", "Sem1", "Mes1", "Mes3")))
# Densidad por momento
p1 <- ggplot(SLT_largo, aes(x = pio, fill = momento)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ momento, nrow = 1, scales = "free_y") +
  labs(x = "PIO (mmHg)", y = "Densidad", title = "Densidad por momento") +
  theme_bw() +
  theme(legend.position = "none")

# Boxplot por momento
p2 <- ggplot(SLT_largo, aes(x = momento, y = pio, fill = momento)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, outlier.size = 2) +
  labs(x = "Momento", y = "PIO (mmHg)", title = "Boxplot por momento") +
  theme_bw() +
  theme(legend.position = "none")

ggarrange(p1, p2, nrow = 2)


#vamos a sustituir los outliers por la media en cada momento
outlier <- function(x) {
  H  <- 1.5 * IQR(x, na.rm = TRUE)
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  x[x < q1 - H] <- NA
  x[x > q3 + H] <- NA
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}

SLT$piopre <- outlier(SLT$piopre)
SLT$piosem <- outlier(SLT$piosem)
SLT$piomes <- outlier(SLT$piomes)
SLT$pio3mes <- outlier(SLT$pio3mes)
SLT$nimpactos <- outlier(SLT$piopre)
SLT$enertotal <- outlier(SLT$piosem)
SLT$enerimpac <- outlier(SLT$piomes)


###############################################################################
# BLOQUE 8. PCA (ANÁLISIS DE COMPONENTES PRINCIPALES)
# -----------------------------------------------------------------------------
# ¿Qué hace el PCA?
#   Crea NUEVAS variables (componentes) que son combinaciones lineales de las
#   originales. La 1ª componente (PC1) recoge la mayor varianza posible, la
#   2ª (PC2) recoge la mayor varianza restante y es perpendicular a PC1, etc.
#
# ¿Para qué sirve?
#   - Reducir muchas variables a unas pocas.
#   - Ver qué variables van juntas (correlacionadas).
#   - Visualizar pacientes en 2D.
#
# Parámetros:
#   scale  = TRUE -> estandariza (cada variable media 0, sd 1).
#                    IMPORTANTE si las variables tienen escalas distintas.
#   center = TRUE -> centra en la media.
###############################################################################
SLT$enerimpac <- as.character(SLT$enerimpac)
SLT$enerimpac[SLT$enerimpac == ""] <- NA            # vacíos -> NA
SLT$enerimpac <- gsub(",", ".", SLT$enerimpac)      # comas decimales -> punto
SLT$enerimpac <- sub("[^0-9.].*$", "", SLT$enerimpac)  # corta desde el primer carácter raro
SLT$enerimpac <- as.numeric(SLT$enerimpac) #convertimos a numero
pca <- prcomp(SLT[, c(6,8,9,11:14)], scale = TRUE, center = TRUE) #6,8,9,10-14
descr(SLT)
# "rotation": cuánto pesa cada variable original en cada componente (loadings)
pca$rotation

# "sdev": desviación típica de cada componente. Al cuadrado = varianza.
pca$sdev

# Resumen: proporción de varianza explicada por cada componente
summary(pca)

# Varianza explicada (proporción)
var_expl <- pca$sdev^2 / sum(pca$sdev^2)
var_expl

# Varianza acumulada
cum_var <- cumsum(var_expl)
cum_var

###############################################################################
# BLOQUE 9. GRÁFICOS DEL PCA
# -----------------------------------------------------------------------------
# 1) Varianza explicada por componente (scree plot):
#    Muestra cuánta info aporta cada componente. Se eligen las que van antes
#    del "codo" (donde la curva se aplana), o las que superan la media de
#    varianza (1/nº de variables si estandarizamos).
#
# 2) fviz_pca_var: dibuja las VARIABLES como flechas.
#    - Flechas en la misma dirección = variables correlacionadas positivamente.
#    - Flechas opuestas = correlación negativa.
#    - Flechas perpendiculares = poca relación.
#    - Color "cos2" = calidad de representación (más oscuro = mejor).
#
# 3) fviz_pca_ind: dibuja los INDIVIDUOS (filas).
#    - Puntos cercanos = perfiles parecidos.
#    - Color "contrib" = cuánto contribuye cada punto a la componente.
#
# 4) fviz_pca: biplot, variables + individuos juntos.
###############################################################################

# Scree plot
df_var <- data.frame(pc = 1:length(var_expl), var = var_expl)
p1 <- ggplot(df_var, aes(x = pc, y = var, fill = var)) +
  geom_col(width = 0.4) +
  theme_bw() +
  labs(x = "Componente principal", y = "Proporción de varianza")

# Varianza acumulada
df_cum <- data.frame(pc = 1:length(cum_var), cum = cum_var)
p2 <- ggplot(df_cum, aes(x = pc, y = cum, fill = cum)) +
  geom_col(width = 0.4) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  labs(x = "Componente principal", y = "Varianza acumulada")

ggarrange(p1, p2, nrow = 1)


# Gráficos de variables (flechas)
fviz_pca_var(pca, repel = TRUE, col.var = "cos2",
             legend.title = "cos2", title = "Variables en PC1-PC2") +
  theme_bw()

fviz_pca_var(pca, axes = c(1, 3), repel = TRUE, col.var = "cos2",
             title = "Variables en PC1-PC3") +
  theme_bw()

fviz_pca_var(pca, axes = c(2, 3), repel = TRUE, col.var = "cos2",
             title = "Variables en PC2-PC3") +
  theme_bw()


# Gráficos de individuos
fviz_pca_ind(pca, col.ind = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, legend.title = "Contrib.", title = "Individuos") +
  theme_bw()

fviz_pca_ind(pca, axes = c(1, 3), col.ind = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, title = "Individuos PC1-PC3") +
  theme_bw()


# Biplot: variables + individuos
fviz_pca(pca, alpha.ind = "contrib", col.var = "cos2",
         col.ind = "seagreen",
         gradient.cols = c("#FDF50E", "#FD960E", "#FD1E0E"),
         repel = TRUE, legend.title = "Distancia") +
  theme_bw()




