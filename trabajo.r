library("summarytools")
library("ggplot2")
library("ggpubr")
library("foreign")
library("readxl")
library("archdata")
library("psych")
library("factoextra")
library("scatterplot3d")

SLT <- read_excel("Tarea_Glaucoma_DB.xlsx", sheet = "SLT",
                  col_types = c("skip",     # FECHA DEL SLT (la omitimos)
                                "text",     # SEXO
                                "numeric",  # EDAD
                                "text",     # OJO
                                "text",     # TIPO GLAUCOMA
                                "numeric",  # Nº IMPACTOS
                                "numeric",  # CUADRANTES REALIZADOS
                                "numeric",  # ENERGIA POR IMPACTO
                                "numeric",  # ENERGIA TOTAL
                                "text",     # CIRUGIAS PREVIAS
                                "numeric",  # PIO PRE SLT
                                "numeric",  # PIO 1ª SEMANA
                                "numeric",  # PIO 1er MES
                                "numeric",  # PIO 3er MES
                                "numeric",  # Nº FARMACOS PRE
                                "numeric",  # Nº FARMACOS 1er MES
                                "numeric",  # Nº FARMACOS 3er MES
                                "text"))    # DOLOR

class(SLT)
head(SLT, n = 5)
summary(SLT)        # comprobamos los NA de cada columna

# Variables cuantitativas (posiciones tras omitir la fecha)
quant <- SLT[, c(2, 5:8, 10:16)]

# Matriz de varianzas y covarianzas
cov(quant, use = "pairwise.complete.obs")

# Para compararla con tu script, la de correlaciones igual:
cor(quant, use = "pairwise.complete.obs")