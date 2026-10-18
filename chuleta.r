###############################################################################
# GUÍA DE ESTUDIO: ANÁLISIS EXPLORATORIO + PCA EN R
# -----------------------------------------------------------------------------
# Basada en los scripts clase1.r y clase2.r
# Dataset de ejemplo: iris (incluido en R por defecto)
# Objetivo: entender paso a paso QUÉ se hace y POR QUÉ en cada bloque.
#
# Ejecuta el archivo por bloques (Ctrl+Enter línea a línea, o selecciona un
# bloque y Ctrl+Enter). No lances todo de golpe la primera vez.
###############################################################################


###############################################################################
# BLOQUE 0. PAQUETES
# -----------------------------------------------------------------------------
# Un paquete es una colección de funciones extra que amplían R.
# Hay que INSTALARLOS una sola vez con install.packages("nombre")
# y CARGARLOS cada vez que abrimos R con library("nombre").
#
# Los que usaremos y para qué sirve cada uno:
#   summarytools -> resúmenes descriptivos bonitos (descr, freq)
#   ggplot2      -> gráficos modernos y flexibles
#   ggpubr       -> combinar varios gráficos ggplot en una sola figura (ggarrange)
#   readxl       -> leer Excel
#   psych        -> estadística descriptiva y test de Bartlett (cortest.bartlett)
#   factoextra   -> gráficos específicos para PCA (fviz_pca_var, fviz_pca_ind, fviz_pca)
#   scatterplot3d-> gráficos 3D (nubes de puntos)
###############################################################################

# install.packages(c("summarytools","ggplot2","ggpubr","readxl",
#                    "psych","factoextra","scatterplot3d"))

library(summarytools)
library(ggplot2)
library(ggpubr)
library(readxl)
library(psych)
library(factoextra)
library(scatterplot3d)


###############################################################################
# BLOQUE 1. IMPORTAR DATOS
# -----------------------------------------------------------------------------
# En clase1/clase2 importan Excel y SPSS. Aquí usamos iris porque ya está
# cargado en R. Cuando tú trabajes con tu Excel de glaucoma, cambia esta línea
# por:
#   datos <- read_excel("Tarea_Glaucoma_DB.xlsx", sheet = "SLT")
#
# "sheet" indica QUÉ HOJA del Excel quieres leer (tu archivo tiene varias).
###############################################################################

data("iris")          # carga el dataset iris en memoria
datos <- iris         # renombramos a "datos" para que sea más claro

# ¿Qué tipo de objeto es? Queremos que sea "data.frame" (tabla con columnas).
class(datos)

# Nombres de las columnas (variables)
colnames(datos)

# Primeras filas para echar un vistazo
head(datos, n = 5)

# Estructura: tipo de cada variable (numérica, factor, etc.)
str(datos)


###############################################################################
# BLOQUE 2. ANÁLISIS DESCRIPTIVO BÁSICO
# -----------------------------------------------------------------------------
# Antes de nada, mira cómo son tus datos: rangos, medias, NA, categorías...
###############################################################################

# Resumen de TODAS las variables a la vez
summary(datos)

# Resumen de UNA variable concreta
summary(datos$Sepal.Length)

# Resumen más detallado (media, mediana, cuartiles, asimetría, curtosis...)
descr(datos$Sepal.Length)

# Variables cualitativas (factores): tabla de frecuencias ABSOLUTAS
table(datos$Species)

# Frecuencias RELATIVAS (proporciones), redondeadas a 2 decimales
round(prop.table(table(datos$Species)), 2)

# Tabla completa con absolutas, relativas y porcentajes (más bonita)
freq(datos$Species)


###############################################################################
# BLOQUE 3 (reemplazo). GRÁFICOS EN GGPLOT2 — LO ESENCIAL
# -----------------------------------------------------------------------------
# Regla mental:  ggplot(datos, aes(...)) + geom_*() + labs() + theme_*()
# - Todo se suma con "+". Nunca comas ni %>% entre capas.
# - Dentro de aes() va SOLO lo que depende de una VARIABLE.
# - Fuera de aes() van los VALORES FIJOS ("red", 2, 0.5...).
###############################################################################


## 1) ESQUELETO MÍNIMO ---------------------------------------------------------

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +  # lienzo: qué va en cada eje
  geom_point()                                          # geometría: cómo se dibuja

# Nada más. Cambiando geom_*() cambia el gráfico; cambiando aes() cambia qué
# representa cada eje y cada color.


## 2) CANALES DE aes(): a qué variable se engancha cada estética ---------------

# x, y        -> posición
# fill        -> relleno (interior de barras/cajas)
# color       -> color de puntos/bordes/líneas
# size        -> tamaño
# shape       -> forma del punto
# alpha       -> transparencia (0 = invisible, 1 = opaco)
# group       -> agrupar sin colorear
# label       -> texto

# Variable -> dentro de aes()
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + geom_point()

# Valor fijo -> fuera de aes()
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point(color = "red", size = 3)

# ERROR TÍPICO: color = "red" dentro de aes() -> R cree que "red" es una categoría.


## 3) GEOMS BÁSICOS: un ejemplo de cada uno ------------------------------------

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()                # puntos
ggplot(iris, aes(x = Sepal.Length))                   + geom_histogram(bins = 15)  # histograma
ggplot(iris, aes(x = Sepal.Length))                   + geom_density()             # densidad
ggplot(iris, aes(x = Species, y = Sepal.Length))      + geom_boxplot()             # caja
ggplot(iris, aes(x = Species))                        + geom_bar()                 # barras (cuenta)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))  + geom_point() + geom_smooth(method = "lm")  # tendencia

# geom_bar  -> cuenta filas por categoría (solo necesita x)
# geom_col  -> dibuja la altura que le des en y
# geom_histogram vs geom_density -> mismos datos, dos vistas de la distribución


## 4) labs(), theme_*(), scale_*(), coord_*(): los 4 ajustes que más usarás -----

ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20) +   # superpuestos, semitransparentes
  labs(title = "Distribución de Sepal.Length",
       x = "Longitud del sépalo", y = "Frecuencia",
       fill = "Especie") +
  scale_y_continuous(limits = c(0, 30)) +                           # fija rango del eje Y
  theme_bw()                                                        # fondo blanco con rejilla

# labs():   títulos y nombres de ejes/leyenda. Puramente cosmético.
# theme_*(): theme_bw(), theme_minimal(), theme_classic(), theme_void().
# scale_*(): scale_y_continuous(limits=...), scale_fill_manual(values=...),
#            scale_fill_gradient(low=, high=).
# coord_*(): coord_flip()  -> intercambia X e Y (boxplots horizontales)
#            coord_polar("y") -> convierte barras en tarta
#            coord_fixed() -> misma escala en X e Y


## 5) TRUCO: gráfico de sectores = geom_bar + coord_polar ---------------------

ggplot(iris, aes(x = factor(1), fill = Species)) +   # factor(1) = una sola barra apilada
  geom_bar() +
  coord_polar("y") +                                 # la enrolla en círculo
  theme_void()                                       # quita ejes y fondo


## 6) EJEMPLO COMPLETO (el del bloque 3 original) ------------------------------
# Densidad + histograma + boxplot de Sepal.Length en una sola fila.

# a) Densidad: curva suavizada de la distribución.
p1 <- ggplot(iris, aes(x = Sepal.Length)) +
  geom_density(fill = "skyblue", alpha = 0.6) +      # relleno y transparencia fijos
  labs(title = "Densidad", x = "Sepal.Length", y = "Densidad") +
  theme_bw()

# b) Histograma: frecuencia por intervalos. bins controla cuántos intervalos.
p2 <- ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(bins = 15, fill = "salmon", color = "white") +
  labs(title = "Histograma", x = "Sepal.Length", y = "Frecuencia") +
  theme_bw()

# c) Boxplot: mediana, cuartiles y outliers. coord_flip lo pone horizontal.
p3 <- ggplot(iris, aes(x = Sepal.Length)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, outlier.size = 2) +
  coord_flip() +
  labs(title = "Boxplot", x = "Valor", y = "") +
  theme_bw()

# ggarrange une los tres en una fila (viene del paquete ggpubr)
ggarrange(p1, p2, p3, nrow = 1, common.legend = FALSE)


###############################################################################
# CÓMO LEER CUALQUIER GRÁFICO DE ESTE BLOQUE
# -----------------------------------------------------------------------------
# Densidad/Histograma:
#   - Simétrico -> media ~ mediana.
#   - Cola a la derecha -> asimetría positiva.
#   - Dos "jorobas" -> posible mezcla de dos grupos.
# Boxplot:
#   - Línea central = mediana.
#   - Caja = Q1 a Q3 (50% central de los datos).
#   - Bigotes = hasta 1.5*IQR.
#   - Puntos rojos = outliers.
###############################################################################


###############################################################################
# BLOQUE 4. GRÁFICOS PARA VARIABLES CUALITATIVAS
# -----------------------------------------------------------------------------
# Para variables categóricas se usan:
#   - Gráfico de sectores (tarta): proporción de cada categoría
#   - Gráfico de barras           : frecuencia de cada categoría
#
# Interpretación: sirven para ver si las categorías están balanceadas o no.
# Si una categoría domina mucho, aviso: puede sesgar análisis posteriores.
###############################################################################

p1 <- ggplot(datos, aes(x = factor(1), fill = Species)) +
  geom_bar() +
  coord_polar("y") +
  labs(x = "Species", y = "%") +
  theme_void()

p2 <- ggplot(datos, aes(x = Species, fill = Species)) +
  geom_bar() +
  labs(x = "Species", y = "Frecuencia")

ggarrange(p1, p2, nrow = 1, common.legend = TRUE)


###############################################################################
# BLOQUE 5. DETECCIÓN DE VALORES ATÍPICOS (OUTLIERS)
# -----------------------------------------------------------------------------
# Un outlier es un valor muy alejado del resto. Puede deberse a error de
# medida o ser un caso real extremo. El PCA es sensible a ellos, por eso
# conviene detectarlos y decidir qué hacer.
#
# Método del rango intercuartílico (IQR):
#   Outlier si valor < Q1 - 1.5*IQR  ó  valor > Q3 + 1.5*IQR
###############################################################################

# Boxplot de TODAS las variables numéricas a la vez (iris: columnas 1 a 4)
boxplot(datos[, 1:4],
        main = "Outliers en iris",
        xlab = "Variables", ylab = "Valores",
        col = c("lightblue", "lightgreen", "salmon", "gold"))

# Si las escalas son muy distintas, normalizamos (z-scores) para comparar
datos_scaled <- scale(datos[, 1:4])
boxplot(datos_scaled,
        main = "Outliers (estandarizado)",
        xlab = "Variables", ylab = "z", col = 1:4)


###############################################################################
# BLOQUE 6. FUNCIONES PARA LIMPIAR DATOS
# -----------------------------------------------------------------------------
# Creamos funciones reutilizables:
#   not_available(): sustituye NA por la media de la variable.
#   outlier()      : sustituye outliers por la media de la variable.
#
# En tu Excel de glaucoma, esta parte es CLAVE porque hay celdas vacías
# y valores como "4 dispersos", "10 (4PA)" que hay que limpiar antes.
###############################################################################

# Sustituir NA por la media
not_available <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}

# Sustituir outliers por la media (se aplica recursivamente hasta que no queden)
outlier <- function(x) {
  H  <- 1.5 * IQR(x, na.rm = TRUE)
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  x[x < q1 - H] <- NA
  x[x > q3 + H] <- NA
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  # Repetir si aún quedan outliers
  if (any(x < q1 - H) || any(x > q3 + H)) outlier(x) else x
}

# Creamos una copia para no tocar el original
datos_limpios <- datos

# Aplicamos outlier() a cada variable numérica que lo necesite
datos_limpios$Sepal.Width <- outlier(datos_limpios$Sepal.Width)
datos_limpios$Petal.Width <- outlier(datos_limpios$Petal.Width)

# Comprobamos que ya no hay outliers
boxplot(datos_limpios[, 1:4],
        main = "Tras limpiar outliers",
        col = c("lightblue", "lightgreen", "salmon", "gold"))


###############################################################################
# BLOQUE 7. CORRELACIONES
# -----------------------------------------------------------------------------
# El PCA tiene sentido si las variables están correlacionadas. Si no lo
# estuvieran, cada componente coincidiría con una variable y no reduciríamos
# nada.
#
#   - Matriz de correlaciones: valores entre -1 y 1.
#     Cerca de 1 o -1 -> fuerte relación lineal.
#     Cerca de 0      -> poca relación.
#   - Determinante: si es 1, variables incorreladas (malo para PCA).
#     Si es cercano a 0, hay mucha correlación (bueno para PCA).
#   - Test de Bartlett: H0 = "las variables están incorreladas".
#     p < 0.05 -> rechazamos H0 -> hay correlación -> PCA procede.
###############################################################################

cor_matrix <- cor(datos_limpios[, 1:4])
cor_matrix

det(cor_matrix)

cortest.bartlett(cor_matrix, n = nrow(datos_limpios))


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

pca <- prcomp(datos_limpios[, 1:4], scale = TRUE, center = TRUE)

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


###############################################################################
# BLOQUE 10. COORDENADAS DE LOS INDIVIDUOS
# -----------------------------------------------------------------------------
# pca$x contiene las coordenadas de cada fila (individuo) en cada componente.
# Útil si quieres exportarlas a Excel o usar las componentes como variables
# en otro análisis (p. ej. regresión).
###############################################################################

head(pca$x)


###############################################################################
# BLOQUE 11. EXTRA: NUBE DE PUNTOS 3D
# -----------------------------------------------------------------------------
# scatterplot3d dibuja 3 variables a la vez en 3D. Útil para ver si los
# grupos se separan en el espacio.
#
# Interpretación: si los colores forman "cúmulos" separados, hay grupos
# naturales en los datos.
###############################################################################

scatterplot3d(datos$Sepal.Length, datos$Sepal.Width, datos$Petal.Length,
              color = c("red", "blue", "green")[datos$Species],
              pch = 19, grid = TRUE,
              xlab = "Sepal.Length", ylab = "Sepal.Width",
              zlab = "Petal.Length", angle = 65, cex.axis = 0.6)


###############################################################################
# BLOQUE 12. EXTRA PARA TU TRABAJO DE GLAUCOMA
# -----------------------------------------------------------------------------
# Esto NO está en los scripts de clase, pero te hará falta porque tu base tiene
# una variable respuesta binaria (Pio_Normal_CAT: 0 = bien, 1 = mal).
#
# Para ver qué variables se asocian al resultado:
#   - Variables categóricas -> tabla de contingencia + chi-cuadrado
#   - Variables numéricas   -> t-test (2 grupos) o Wilcoxon si no normalidad
#   - Modelo conjunto       -> regresión logística (glm family = binomial)
#
# Ejemplo con iris (Species como "resultado" para que veas la sintaxis):
###############################################################################

# Chi-cuadrado entre dos categóricas (aquí solo hay una, así que lo
# simulamos con la variable cortada)
datos$Sepal_grande <- ifelse(datos$Sepal.Length > 5.8, "Si", "No")
tabla <- table(datos$Sepal_grande, datos$Species)
tabla
chisq.test(tabla)

# t-test: ¿difiere Sepal.Width entre plantas con pétalo ancho y estrecho?
t.test(Sepal.Width ~ Petal_grande, data = transform(datos,
       Petal_grande = ifelse(Petal.Width > 1.3, "Ancho", "Estrecho")))

# Regresión logística (ejemplo de sintaxis con iris binario artificial)
datos$binaria <- ifelse(datos$Species == "setosa", 0, 1)
modelo <- glm(binaria ~ Sepal.Length + Sepal.Width + Petal.Length,
              data = datos, family = binomial)
summary(modelo)


###############################################################################
# FIN DE LA GUÍA
# -----------------------------------------------------------------------------
# Resumen del flujo completo:
#   1. Cargar paquetes
#   2. Importar datos
#   3. Descriptivos (summary, descr, table, freq)
#   4. Gráficos exploratorios (densidad, histograma, boxplot, barras)
#   5. Detectar y tratar NA y outliers
#   6. Correlaciones + test de Bartlett
#   7. PCA (prcomp)
#   8. Gráficos del PCA (scree, variables, individuos, biplot)
#   9. Interpretar resultados
#
# Cuando lo pases a tu Excel de glaucoma, sustituye:
#   datos <- iris
# por:
#   datos <- read_excel("Tarea_Glaucoma_DB.xlsx", sheet = "SLT")
# y ajusta los nombres de columnas y la codificación (sexo, ojo, dolor...).
###############################################################################