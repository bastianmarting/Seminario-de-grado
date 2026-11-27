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
               hrbrthemes) 



muestra_final <- readRDS("data-proc/base_proc_ola1.rds")
set.seed(777)

#6.a Análisis univariados-------------------------------------------------------
