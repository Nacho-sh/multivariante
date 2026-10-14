library("summarytools")
library("ggplot2")
library("ggpubr")
library("foreign")
library("readxl")
library("archdata")
library("psych")
library("factoextra")
library("scatterplot3d")

data_xlsx <- read_excel("Tarea_Glaucoma_DB.xlsx")

data("RBGlass1")
class("Tarea_Glaucoma_DB")
