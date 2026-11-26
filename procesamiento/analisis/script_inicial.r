# Cargar librerías y datos -----------------------------------------------------
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
               here) 

options(scipen = 999)
rm(list = ls())


set.seed(777)
bbdd <- read_sav("input/data-orig/Estudio_3_ola1.sav")


dim(bbdd)

# 1. FIltrar y seleccionar variables -------------------------------------------
## a Selección de variables de interés -----------------------------------------

data <- bbdd %>%
  dplyr::select(
    infiel_sex = i_6_p102,
    part_sex_grupo = i_7_p102,
    relacion_exclusiva = p86,
    pers_sex_ult_ano = p71,
    ult_sex_met_concep = p119, 
    ult_ano_frc_preserv = p73,  
    sex_mes_alc_drog = p103_o1,         
    sex_mes_alc_drog_pareja = p104_o1,
    sexo = p1,
    genero = p3,
    age = p4,
    condon_1 = i_1_p33,
    condon_2 = i_2_p33,
    condon_3 = i_3_p33,
    condon_4 = i_4_p33,
    condon_5 = i_5_p33,
    prs = p49,
    os = p134,
    esf = p34
  )

## b Tratar NA's ---------------------------------------------------------------

