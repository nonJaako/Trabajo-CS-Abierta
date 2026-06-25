
# Abrir librerias ---------------------------------------------------------


pacman::p_load(tidyverse,
               sjPlot,
               sjmisc,
               corrplot,
               psych,
               kableExtra,
               dplyr,
               haven)
options(scipen = 999)
rm(list = ls())


# Base de datos -----------------------------------------------------------


load("C:/Users/joakk/Desktop/Bases de datos/Base de datos EBS 2023.RData")


# Seleccion de casos (muestra) ------------------------------------------------------
Base_final <- sample_n(EBS_2023_vp, 4000)
  
Base_final <- Base_final %>%
  dplyr::select(a1,
                a6,
                a12,
                a2_a,
                a2_b,
                a2_c,
                a2_d,
                rr3,
                ss1,
                u15,
                h1,
                u12)


# Tratamiento casos perdidos ----------------------------------------------


Base_final[Base_final == -88 | Base_final == -99] <- NA


sum(is.na(Base_final))

colSums(is.na(Base_final))

Base_final <- na.omit(Base_final)

dim(Base_final)

# Recodificar variables ---------------------------------------------------
vars_likert <- c("rr3", "a2_c", "a2_d")

Base_final <- Base_final %>%
  mutate(across(
    .cols = all_of(vars_likert),
    .fns = ~ 6 - as.numeric(as.character(.x)),
    .names = "{.col}_inv"))

# corrección

Base_final$u15 <- ifelse(Base_final$u15 == 2, 0, 1)



Base_final <- Base_final %>%
  dplyr::select(a1,
                a6,
                a12,
                a2_a,
                a2_b,
                a2_c_inv,
                a2_d_inv,
                rr3_inv,
                h1,
                u15,
                ss1,
                u12)


# Renombramos variables que no formar parte de las escalas ---------------


Base_final <- Base_final %>%
  rename(salud_física = ss1,
         tiempo_libre = u15,
         seguridad = h1,
         ATD = u12)


# Construcción de escala sumativa satisfacción_vida -----------------------------------------

Base_final <- Base_final %>%
  mutate(
    satisfaccion_vida = rowSums(select(., a1, a6, a12, a2_a), na.rm = FALSE))




# validacion de la escala sumtiva satisfacción_vida -------------------------------------------------

psych::describe(Base_final$satisfaccion_vida)
psych::alpha(Base_final %>% select(a1, a6, a12, a2_a))



# Contrucción escala sumativa salud_mental ------------------------------------------------------

Base_final <- Base_final %>%
  mutate(
    salud_mental = rowSums(select(., rr3_inv, a2_b, a2_c_inv, a2_d_inv), na.rm = FALSE))
# validación escala sumativa -------------------------------------------------------

psych::describe(Base_final$salud_mental)
psych::alpha(Base_final %>% select(rr3_inv, a2_b, a2_c_inv, a2_d_inv))



# PARTE 2 -----------------------------------------------------------------


    
