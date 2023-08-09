#LIBRERIAS----------------------------------------------------------------------

library(installr)
library(rsconnect)
library(janitor)
library(shiny)
library(googlesheets4)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(dplyr)
library(readxl)
library(stringr)
library(readxl)
library(tidyverse)

#DATOS-USUARIOS-----------------------------------------------------------------

#Traigo la información de jefes y equipo al que pertenece cada persona:

nomina <- data.frame(read_excel(
  "DEVOLUCIONES.xlsx",
  sheet="LISTADO FINAL DE MAILS IMPORT",
  range = "A1:C300",
  col_names = TRUE,
  col_types = NULL,
  na = "",
  trim_ws = TRUE,
  skip = 0) %>% 
    clean_names())

#DATOS-DEVOLUCIONES-------------------------------------------------------------

c_liderazgo_c <-  read_excel("DEVOLUCIONES.xlsx",  sheet="C_LIDERAZGO_C", skip = 4)%>% 
  #saco las evaluaciones de la mesa operativa:
  filter(.[[3]] != "Interna Mesa Operativa (PMs-Coordis)")%>% 
  #saco los que tienen la primera variable de evaluaciones Colaborador a lider vacías:
  filter(!is.na(.[[9]])) %>% 
  #reemplazo las variables categóricas por numéricas y calculo medias en las columnas creadas para eso:
  mutate(across(c(9:11,13:14,16:17,19:20), ~case_when(. == "Desarrollo destacado" ~ 5,
                                                       . == "Desarrollada" ~ 4,
                                                       . == "En desarrollo" ~ 3,
                                                       . == "Poco desarrollada" ~ 2,
                                                       . == "A trabajar" ~ 1,
                                                       TRUE ~ as.numeric(.))),
         "DESARROLLO DE PERSONAS" = rowMeans(across(9:11), na.rm = TRUE),
         "PLANIFICACIÓN" = rowMeans(across(13:14), na.rm = TRUE),
         "TOMA DE DECISIONES" = rowMeans(across(16:17), na.rm = TRUE),
         "EMPODERAMIENTO" = rowMeans(across(19:20), na.rm = TRUE)
         
  ) %>% 
  #uno con la nómina para tener el mail del jefe de la persona que hizo la evaluacion, porque es a quien está evaluando:
  left_join(nomina, by = c("direccion_de_correo_electronico" = "mail_por_rol_actual"))

#Agrupo observaciones para mostrar las medias de las variables desagregadas y los valores máximos y mínimos de cada una teniendo en cuenta que cada lider puede tener más de un colaborador que lo ha evaluado:
resumen <- c_liderazgo_c %>% 
  group_by(mail_jefe) %>%
  #renombro las variables con números para simplificar las operaciones porque no conseguí llamarlas por índice dentro de las funciones....
  rename_with(~ as.character(seq_along(.)), everything()) %>% 
  summarise(
    across(c("8", "12", "15", "18"), ~ mean(.), .names = "{.col}"),
    across(c("9":"11","13":"14","16":"17","19":"20"), ~paste0(min(.),"-",max(.)), .names = "{.col}")
    ) %>% 
  #redondeo a enteros:
  mutate_if(is.numeric,round,digits = 0) %>% 
  #reordeno las variables numericamente porque sus nombres asignados respondían a su índice:
  select(order(as.numeric(colnames(.))))


#devuelvo el nombre que tenían las variables en el df original y renombro la última:
colnames(resumen) <- colnames(c_liderazgo_c[, 8:20])
colnames(resumen)[14] <- "mail_por_rol_actual"

