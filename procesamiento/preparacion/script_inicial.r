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


bbdd <- read_sav("input/data-orig/estudio_ola1.sav")
set.seed(777)
names(bbdd)
dim(bbdd)  
# 0. VERIFICACIONES DE LA ENCUESTA, CÚAL ES EL N?.

# 1. Filtrar y seleccionar variables ---------------------------------------------------

# base completa
dim(bbdd)                                # 2457 91

# solo quienes aceptan participar
table(bbdd$Intro, useNA = "ifany")       # 1= 2236, 2 = 221
d1 <- bbdd %>% filter(Intro == 1)
nrow(d1)                                 # 2236

# duplicados: se conserva la respuesta más completa y, si hay empate, la primera.
# (Castillo conservan siempre la primera respuesta; con esa regla el N sería 2.072)
d1 <- d1 %>% mutate(id = sub("_.*", "", ticket))
d1 %>% count(id) %>% filter(n > 1) %>% nrow()   # 30 personas repetidas
sum(duplicated(d1$id))                          # 31 filas sobrantes

d2 <- d1 %>%
  arrange(id, desc(Progress), StartDate) %>%
  distinct(id, .keep_all = TRUE)
nrow(d2)                                 # 2205

# quienes terminaron la encuesta
table(d2$Finished, useNA = "ifany")      # 0 = 118, 1 = 2087
d3 <- d2 %>% filter(Finished == 1)
nrow(d3)                                 # 2087

# juntar las tres versiones de cada percepción y sacar perdidos porque se presentaron diferentes versiones
#de baterias para talento y esfuerzo, tres versiones
d3 <- d3 %>%
  mutate(
    esfuerzo = coalesce(as.numeric(meritv01_perc_effort),
                        as.numeric(meritv02_perc_effort),
                        as.numeric(meritv03_perc_effort)),
    talento  = coalesce(as.numeric(meritv01_perc_talent),
                        as.numeric(meritv02_perc_talent),
                        as.numeric(meritv03_perc_talent))
  )

vars <- c("esfuerzo", "talento", "ess01_1", "ess02_1",
          "edcep", "ingresos", "sexo", "edad")
colSums(is.na(d3[, vars]))               # ess01_1 = 4, ess02_1 = 2, ingresos = 2, resto 0

d4 <- d3 %>% filter(if_all(all_of(vars), ~ !is.na(.x)))
nrow(d4)                                 # 2079

# versión de la batería
d4 <- d4 %>%
  mutate(version = case_when(
    FL_21_DO_merit_perc_pref_julio19v01 == 1 ~ 1,
    FL_21_DO_merit_perc_pref_julio19v02 == 1 ~ 2,
    FL_21_DO_merit_perc_pref_julio19v03 == 1 ~ 3
  ))
table(d4$version, useNA = "ifany")       # 1= 693 2= 700 3= 686

##Revisión de variables ---------------------------------------------------------------

#tabla para descriptivos de las variables
#resumen_df <- sjmisc::descr(d4)
#resumen_df
#View(resumen_df) #abre tabla de descriptivos iniciales en r

## a. selecionar variables ----------------------------------------------------------

df <- d4 %>%
  dplyr::transmute(
    id,
    per_effort = esfuerzo,               # 1 a 5 (versiones juntas)
    per_talent = talento,                # 1 a 5
    sexo    = as.numeric(sexo),       # 1 = hombre, 2 = mujer
    edad     = as.numeric(edad),       # 5 tramos
    educ      = as.numeric(edcep),      # 1 a 10
    ingresos   = as.numeric(ingresos),   # 14 tramos
    estat_subj = as.numeric(ess01_1),    #  1 a 11
    estat_fam  = as.numeric(ess02_1),    #  0 a 10
    version
  )
dim(df) #quedamos con un N de 2.079           
names(df) 

## b revisión  de variables ---------------------------------------------------------------

#tabla para descriptivos de las variables
resumen_df <- sjmisc::descr(df %>% dplyr::select(-id))
resumen_df
View(resumen_df) #tabla de descriptivos iniciales en r

## c recodificacion de variables --------------------------------


df_rec1 <- df

#Controles edad y sexo
#sexo
df_rec1 <- df_rec1 %>%
  mutate(
    sexo_rec = if_else(sexo == 2, 1, 0),                 # 0 = hombre, 1 = mujer
    sexo_f   = factor(sexo_rec, levels = c(0, 1),
                      labels = c("Hombre (0)", "Mujer (1)"))
  )

                    


contrasts(df_rec1$sexo_f)   # Hombre = 0, Mujer = 1     

#edad
df_rec1 <- df_rec1 %>%
mutate(
  edad_f = factor(edad,
                    levels = 1:5,
                    labels = c("18-24 (ref.)", "25-34", "35-44", "45-54", "55 o más")))

# Independientes: educacion, ingresos, estatus
#educacion
df_rec1 <- df_rec1 %>%
mutate(
  educ_f = case_when(educ %in% c(1:6, 8) ~ 1,   # hasta media completa + técnica o univ. incompleta
                       educ == 7           ~ 2,   # técnica completa
                       educ %in% 9:10      ~ 3),  # universitaria completa o postgrado
    educ_f = factor(educ_f,
                    levels = 1:3,
                    labels = c("Secundaria alta o menos (ref.)",
                               "Terciaria de ciclo corto",
                               "Terciaria o más"))) #recodificación según CINE

#ingresos
df_rec1 <- df_rec1 %>% 
mutate(ingresos_f = case_when(ingresos <= 10 ~ 1,    # hasta $448.000
                           ingresos == 11 ~ 2,    # $448.001 a $1.000.000
                           ingresos == 12 ~ 3,    # $1.000.001 a $2.000.000
                           ingresos >= 13 ~ 4),   # más de $2.000.000
    ingresos_f = factor(ingresos_f,
                        levels = 1:4,
                        labels = c("Bajos (ref.)", "Medio-bajos", "Medio-altos", "Altos")),
    ing_3m = as.numeric(ingresos == 14))         # robustez: más de $3.000.000
    # ingresos (1 a 14) se mantiene numérica para H2b

#estatus subjetivo
# CORRER SOLO UNA VEZ. Si se corre dos veces resta 2 (para volver atrás DESDE df_rec1 <- df y correr de nuevo desde arriba)
df_rec1 <- df_rec1 %>% mutate (estat_subj = estat_subj - 1)                # de 1-11 a 0 - 10 porque estatus de familia va de 0 a 10 y propio va de 1 a 11
range(df_rec1$estat_subj); range(df_rec1$estat_fam)                       # 0 10 y 0 10

#estatus de familia se queda igual

#movilidad subj . la referencia des inmovil, que permanece donda ya estaba
df_rec1 <- df_rec1 %>% 
mutate(
  niv_subj = cut(estat_subj, breaks = c(-1, 3, 6, 10), labels = c("Bajo", "Medio", "Alto")), #queda en tres categorías
    niv_fam  = cut(estat_fam,  breaks = c(-1, 3, 6, 10), labels = c("Bajo", "Medio", "Alto")),
    movilidad = case_when(
      as.numeric(niv_subj) > as.numeric(niv_fam) ~ "Ascendente",
      as.numeric(niv_subj) < as.numeric(niv_fam) ~ "Descendente",
      TRUE                                       ~ "Inmóvil"
    ), movilidad = factor(movilidad,  
    levels = c("Inmóvil", "Ascendente", "Descendente"),
                       labels = c("Inmóvil (ref.)", "Ascendente", "Descendente")))

table(df_rec1$movilidad)                  # INMOVIL 1346, ascendente 539, descendente 194
table(df_rec1$niv_fam, df_rec1$niv_subj)   

#versión de la batería (control de diseño), referencia desde A
df_rec1 <- df_rec1 %>%
  mutate(version_f = factor(version, levels = 1:3, labels = c("A", "B", "C")))

#diferencia esfuerzo menos talento (para las pruebas de Wald) ---------------------------PARA PRUEBA DE WALD"""
df_rec1 <- df_rec1 %>%
  mutate(dif = per_effort - per_talent)

names(df_rec1) # Muestra los nombres de las variables de la base de datos


#Tabla de descriptivos
# estas cuatro describen TODAS las columnas de df_rec1.
#stargazer(as.data.frame(df_rec1), type = "text")
#sjmisc::descr(df_rec1)
#sjmisc::descr(df_rec1,
#      show = c("label","range", "mean", "sd", "NA.prc", "n"))%>%
#      kable(.,"markdown")
#summarytools::view(summarytools::dfSummary(df_rec1, headings=FALSE))

tabla_df <- df_rec1 %>%
  dplyr::select(
    "Percepción de recompensa al esfuerzo" = per_effort,
    "Percepción de recompensa al talento"  = per_talent,
    "Ingreso del hogar"               = ingresos_f,
    "Nivel educacional"                 = educ_f,
    "Estatus social subjetivo"            = estat_subj,
    "Movilidad social subjetiva"           = movilidad,
    "Sexo"                                 = sexo_f,
    "Edad"                                 = edad_f,
    "Versión de la batería"                = version_f
  )

summarytools::view(summarytools::dfSummary(tabla_df, headings = FALSE))

## d tabla descriptiva para la tesis ---------------------------

cont <- df_rec1 %>%
  dplyr::select("Percepción de recompensa al esfuerzo" = per_effort,
                "Percepción de recompensa al talento"  = per_talent,
                "Estatus social subjetivo"             = estat_subj) %>%
  pivot_longer(everything(), names_to = "variable") %>%
  group_by(variable) %>%
  summarise(est1  = sprintf("%.2f", mean(value)),
            est2  = sprintf("%.2f", sd(value)),
            rango = paste(min(value), "a", max(value)), .groups = "drop") %>%
  mutate(categoria = "")

cat_vars <- c("Ingreso del hogar"          = "ingresos_f",
              "Nivel educacional"          = "educ_f",
              "Movilidad social subjetiva" = "movilidad",
              "Sexo"                       = "sexo_f",
              "Edad"                       = "edad_f",
              "Versión de la batería"      = "version_f")

categ <- bind_rows(lapply(names(cat_vars), function(lab) {
  df_rec1 %>%
    count(categoria = .data[[cat_vars[[lab]]]]) %>%
    mutate(variable  = lab,
           categoria = as.character(categoria),
           est1      = as.character(n),
           est2      = sprintf("%.1f", 100 * n / sum(n)),
           rango     = "") %>%
    dplyr::select(-n)
}))

orden <- c("Percepción de recompensa al esfuerzo", "Percepción de recompensa al talento",
           "Ingreso del hogar", "Nivel educacional", "Estatus social subjetivo",
           "Movilidad social subjetiva", "Sexo", "Edad", "Versión de la batería")

tabla_desc <- bind_rows(cont, categ) %>%
  arrange(factor(variable, levels = orden)) %>%
  dplyr::select(variable, categoria, est1, est2, rango) %>%
  mutate(variable = ifelse(duplicated(variable), "", variable))

tabla_desc   # revisa que se vea bien: 23 filas






sessionInfo()

# Guardar BBDD limpia y procesada ----------------------------------------------
saveRDS(df_rec1, file = "input/data-proc/base_proc_ola1.rds")

df_proc <- df_rec1
saveRDS(df_proc,    file = "input/data-proc/base_proc_ola1.rds")
saveRDS(tabla_desc, file = "input/data-proc/tabla_desc.rds")









