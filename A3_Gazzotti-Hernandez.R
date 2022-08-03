#Imputacion mediante el metodo Hot deck imputation

#Andres Gazzotti 27588206
#Luis Hernandez 24644352


#Datos faltantes en ingresos

getwd()
setwd()

library(readr)
library(readxl)
library(haven)
library(dplyr)
library(data.table)


personas <- read_sav("C:/Users/Andrés Gazzotti/Documents/R/A3_Gazzotti-Hernandez/encovi_personas2017_ds.sav")

#Imputacion
#Variables a imputar 
#Declara estar trabajando o que no trabajó pero tiene trabajo
#Trabaja de manera remunerada
#NA en ingresos laborales o ingresos menores o iguales a cero



#Creacion y arreglos de variables para imputacion




col_names_personas <- c("ENNUMC", "LIN", "CMHP17", "CMHP18", "CMHP19",
                        "CMHP22", "EMHP28N", "EMHP28A", "EMHP28S",
                        "EMHP32", "TMHP36", "TMHP41", "TMHP43",
                        "TMHP44", "TMHP44BS", "TMHP48", "TMHP45BS",
                        "PMHP60BS", 
                        "PESOPERSONA", "GRPEDAD", "AESTUDIO", "Tciudad_max")

nuevos_nombres_personas <- c("id_hogar", "id_per", "parentesco", "edad", "sexo", 
                    "sit_conyu", "nivel_edu", "edu_ano_aprobado", "edu_sem_aprobado",
                    "tipo_edu", "sit_econo", "sector_eco", "cat_ocupa",
                    "trab_remun", "ing_laboral", "horas_trab", "ing_otro",
                    "ing_pension","pesop", "grp_edad", "anos_edu", "tipo_ciudad")

#Renombrar

personas <- personas %>% 
  setnames(old = col_names_personas, new = nuevos_nombres_personas)   


#Confirmacion
personas

View(personas)


#Variables imputables


V_donantes <- personas %>%
  filter((sit_econo == 1| sit_econo == 2)&
           (ing_laboral!=99)|
           ing_laboral !=98|
           ing_laboral !=0) %>%
  group_by(edu_ano_aprobado, nivel_edu, tipo_edu, sexo) %>%
  summarise(ing_promedio = weighted.mean(ing_laboral, pesop),
            n_imput =length(grp_edad))
  

View(V_donantes)


#Imputacion


Imput_Ing_Laboral <- personas %>%
  left_join(V_donantes, 
            by = c("edu_ano_aprobado", "nivel_edu", "tipo_edu", "sexo" )) %>%
  mutate(ing_laboral = ifelse(ing_laboral %in% c(99,98,0),
                              yes = ing_promedio,
                              no = ing_laboral))

#Las variables donantes son: edu_ano_aprobado (ultimo ano de educacion aprovado),
# el nivel de educacion (primaria, bachicherato, universitario, tecnico superior,
#y postgrado); el tipo de educacion (centro de educacion donde estudia si lo hace) 
# y finalmente el sexo del encuestado

#Enfoque en base al perfil educativo del encuestado se trata de hacer una imputacion, 
#tipo Hotdeck de variables similares para compensar por el missing data en los ingresos
#laborales.


Imput_Ing_Laboral


         
         