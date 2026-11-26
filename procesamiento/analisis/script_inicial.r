# Cargar librerías y datos -----------------------------------------------------------
pacman::p_load(tidyverse,
               sjmisc,
               knitr,
               kableExtra,
               sjPlot,
               stargazer,
               janitor,
               crosstable,
               table1,
               haven,
               psych,
               car,
               summarytools,
               ggplot2,
               dplyr,
               hrbrthemes,
               here,
               labelled) 

options(scipen = 999)
rm(list = ls())

set.seed(777)
bbdd <- read_sav("input/data-orig/estudio_ola1.sav")

dim(bbdd)

# 1. FIltrar y seleccionar variables ---------------------------------------------------
## a Selección de variables de interés -------------------------------------------------

df <- bbdd %>%
  dplyr::select(
    sexo = 19,
    edad = 21,
    educ = 77,
    ingresos = 75,
    estat_subj = 70, 
    estat_fam = 71,
    per_effort = 33,
    per_talent = 34  
  )
dim(df)


## b Revisión de variables ---------------------------------------------------------------

# Pasar todas a as.numeric
df$sexo <- as.numeric(df$sexo)
df$edad <- as.numeric(df$edad)
df$educ <- as.numeric(df$educ)
df$ingresos <- as.numeric(df$ingresos)
df$estat_subj <- as.numeric(df$estat_subj)
df$estat_fam <- as.numeric(df$estat_fam)
df$per_effort <- as.numeric(df$per_effort)
df$per_talent <- as.numeric(df$per_talent)

# revisar base de datos
sum(is.na(df))
colSums(is.na(df))
rowSums(is.na(df))
look_for(df)
summary(df)

#tabla para descriptivos de las variables
resumen_df <- sjmisc::descr(df)
resumen_df
View(resumen_df) #abre tabla de descriptivos iniciales en r








#---- 4. Guardar datos limpios -----------------------------------------------------------

# Guardar los datos limpios en un archivo CSV llamado "datos_limpios.csv"
# row.names = FALSE se usa para evitar que los numeros de fila se guarden en el
# archivo
#write.csv(datos, "datos/datos_limpios.csv", row.names = FALSE)

# Guardar los datos limpios en un archivo de Excel llamado "datos_limpios.xlsx"
# Si se desea especificar la hoja de calculo, se puede utilizar el argumento sheetName
#write.xlsx(datos, "datos/datos_limpios.xlsx")

# Guardar los datos limpios en un archivo de Stata llamado "datos_limpios.dta"
#write_dta(datos, "datos/datos_limpios.dta")

# Guardar los datos limpios en un archivo de SPSS llamado "datos_limpios.sav"
#write_sav(datos, "datos/datos_limpios.sav")