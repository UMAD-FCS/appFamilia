
# ***************************************************************************
# APP FAMILIA 
# ***************************************************************************

library(rio)
library(shinyWidgets)
library(tidyverse)
library(janitor)
library(plotly)
library(DT)
library(shiny)
library(ggtext)
library(stringr)
library(shinyWidgets)
library(ggrepel)
library(shinycssloaders)
library(bslib)
library(RColorBrewer) 

# ***************************************************************************


source('utils.R') 

theme_set(theme_bdd())
update_geom_defaults("text", list(family = theme_get()$text$family))

#dir.create('~/.fonts')
file.copy("www/Titillium Web.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')

color_defecto <- "#21618C"

## Spinner options
options(
  spinner.color = "#476481",
  spinner.color.background = "#ffffff",
  spinner.size = 2)

## Shiny app customized theme
tema_umad <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#085792",
  primary = "#3E6C9A",
  sCP_compndary = "#3E6C9A",
  base_font = font_google("Roboto"),
  heading_font = font_google("Roboto"),
  code_font = font_google("Space Mono")
)

## Colores
vs_blues <- c("#031633", rev(brewer.pal(n = 9, "Blues")[3:9]))
cat_palette <- c("#1F618D", "#5DADE2", "#16A085", "#9B59B6",
                 "#F39C12", "#F4D03F", "#7F8C8D", "#BDC3C7", "#FF8484")
vs_blues_2 <- c(vs_blues[4], vs_blues[7])


### Carga y modifica los datos 
df <- rio::import("Base_Motor_familia_20052024.xlsx")
metadata <- rio::import("Fichas_Familia.xls")


df <- df %>% mutate(TERCIL = case_when(QUINTIL=="Tercil 1" ~ "Tercil 1",
                                       QUINTIL=="Tercil 2" ~ "Tercil 2",
                                       QUINTIL=="Tercil 3" ~ "Tercil 3"))


df <- df %>%   mutate(QUINTIL = case_when(QUINTIL %in% c("Tercil 1", "Tercil 2", "Tercil 3") ~ "Todos",
                                          TRUE ~ QUINTIL))



df <- df %>% mutate(EDADSALIDA = case_when(NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez (Entre 18 y 20)" ~ "Entre 18 y 20", 
                                           NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez (Menor a 18)" ~ "Menor a 18",
                                           NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez (Entre 21 y 30)" ~ "Entre 21 y 30",
                                           NOMINDICADOR!="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez (Entre 18 y 20)"& 
                                             NOMINDICADOR!="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez (Menor a 18)"&
                                             NOMINDICADOR!="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez (Entre 21 y 30)" ~ "Todos"))

df <- df %>% mutate(SALIDA = case_when(NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Amigos)" ~ "Amigos",
                                       NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Otros no parientes)" ~ "Otros no parientes o institución",
                                       NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Otros parientes)" ~ "Otros parientes",
                                       NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Pareja)" ~ "Pareja",
                                       NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Solo)" ~ "Solo",
                                       NOMINDICADOR!="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Amigos)"&
                                         NOMINDICADOR!="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Otros no parientes)"&
                                         NOMINDICADOR!="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Otros parientes)"&
                                         NOMINDICADOR!="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Pareja)"&
                                         NOMINDICADOR!="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Solo)" ~ "Todos"))

df <- df %>% mutate(PLANIF1 = case_when(NOMINDICADOR=="Porcentaje de personas que en su primer embarazo o el de su pareja querían tener un hijo en ese momento, querían más adelante o no querían (No quería)" ~ "No quería",
                                        NOMINDICADOR=="Porcentaje de personas que en su primer embarazo o el de su pareja querían tener un hijo en ese momento, querían más adelante o no querían (Quería más adelante)" ~ "Quería más adelante",
                                        NOMINDICADOR=="Porcentaje de personas que en su primer embarazo o el de su pareja querían tener un hijo en ese momento, querían más adelante o no querían (Quería)" ~ "Quería",
                                        NOMINDICADOR!="Porcentaje de personas que en su primer embarazo o el de su pareja querían tener un hijo en ese momento, querían más adelante o no querían (No quería)"&
                                          NOMINDICADOR!="Porcentaje de personas que en su primer embarazo o el de su pareja querían tener un hijo en ese momento, querían más adelante o no querían (Quería más adelante)"&
                                          NOMINDICADOR!="Porcentaje de personas que en su primer embarazo o el de su pareja querían tener un hijo en ese momento, querían más adelante o no querían (Quería)" ~ "Todos"))


df <- df %>% mutate(PLANIF2 = case_when(NOMINDICADOR=="Porcentaje de personas según cantidad de hijos tenidos y deseados (IGUAL)" ~ "Igual",
                                        NOMINDICADOR=="Porcentaje de personas según cantidad de hijos tenidos y deseados (MAYOR)" ~ "Mayor",
                                        NOMINDICADOR=="Porcentaje de personas según cantidad de hijos tenidos y deseados (MENOR)" ~ "Menor",
                                        NOMINDICADOR!="Porcentaje de personas según cantidad de hijos tenidos y deseados (IGUAL)"&
                                          NOMINDICADOR!="Porcentaje de personas según cantidad de hijos tenidos y deseados (MAYOR)"&
                                          NOMINDICADOR!="Porcentaje de personas según cantidad de hijos tenidos y deseados (MENOR)" ~ "Todos"))


df <- df %>% mutate(PLANIF3 = case_when(NOMINDICADOR=="Porcentaje de personas según edad a la que tuvieron primer hijo y a la que le hubiese gustado tener  (IGUAL)" ~ "Igual",
                                        NOMINDICADOR=="Porcentaje de personas según edad a la que tuvieron primer hijo y a la que le hubiese gustado tener  (MAYOR)" ~ "Mayor",
                                        NOMINDICADOR=="Porcentaje de personas según edad a la que tuvieron primer hijo y a la que le hubiese gustado tener  (MENOR)" ~ "Menor",
                                        NOMINDICADOR!="Porcentaje de personas según edad a la que tuvieron primer hijo y a la que le hubiese gustado tener  (IGUAL)"&
                                          NOMINDICADOR!="Porcentaje de personas según edad a la que tuvieron primer hijo y a la que le hubiese gustado tener  (MAYOR)"&
                                          NOMINDICADOR!="Porcentaje de personas según edad a la que tuvieron primer hijo y a la que le hubiese gustado tener  (MENOR)" ~ "Todos"))


df <- df %>% mutate(VIOLENCIA = case_when(NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la familia en la infancia (Física)" ~ "Física",
                                          NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la familia en la infancia (Psicológica)" ~ "Psicológica",
                                          NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la familia en la infancia (Sexual)" ~ "Sexual",
                                          NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la familia actual (Económica)" ~ "Económica",
                                          NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la familia actual (Sexual)" ~ "Sexual",
                                          NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la familia actual (Psicológica)" ~ "Psicológica",
                                          NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la familia actual (Física)" ~ "Física",
                                          NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja en los últimos 12 meses (Digital)" ~ "Digital",
                                          NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja en los últimos 12 meses (Económica)" ~ "Económica",
                                          NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja en los últimos 12 meses (Física)" ~ "Física",
                                          NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja en los últimos 12 meses (Psicológia)" ~ "Psicológica",
                                          NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja en los últimos 12 meses (Sexual)" ~ "Sexual",
                                          NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja a lo largo de toda la vida (Digital)" ~ "Digital",
                                          NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja a lo largo de toda la vida (Económica)" ~ "Económica",
                                          NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja a lo largo de toda la vida (Física)" ~ "Física",
                                          NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja a lo largo de toda la vida (Psicológica)" ~ "Psicológica",
                                          NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja a lo largo de toda la vida (Sexual)" ~ "Sexual",
                                          TRUE ~ "Todos"))


df <- df %>% mutate(SEXO2 = case_when(NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años sin menores en el hogar (Mujer)" ~ "Mujer",
                                      NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años sin menores en el hogar (Varón)" ~ "Varón",
                                      NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 4 en el hogar (Mujer)" ~ "Mujer",
                                      NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 4 en el hogar (Varón)" ~ "Varón",
                                      NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 7 en el hogar (Mujer)" ~ "Mujer",
                                      NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 7 en el hogar (Varón)" ~ "Varón",
                                      NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 13 en el hogar (Mujer)" ~ "Mujer",
                                      NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 13 en el hogar (Varón)" ~ "Varón",
                                      NOMINDICADOR=="Distribución porcentual de los hogares según sexo del jefe/a (Mujer)" ~ "Mujer",
                                      NOMINDICADOR=="Distribución porcentual de los hogares según sexo del jefe/a (Varón)" ~ "Varón",
                                      TRUE ~ "Todos"))

df <- df %>% mutate (benef1 = case_when(
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Actividades especiales para perpiodos de vacaciones). Ambos." ~ "Actividades especiales para períodos de vacaciones",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Actividades especiales para perpiodos de vacaciones). Solo madre." ~ "Actividades especiales para períodos de vacaciones",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Actividades especiales para perpiodos de vacaciones). Solo padre." ~ "Actividades especiales para períodos de vacaciones",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidado infantil en días específicos o de complemento del horario escolar). Ambos." ~ "Centros de cuidado infantil en días específicos o de complemento del horario escolar",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidado infantil en días específicos o de complemento del horario escolar). Solo madre." ~ "Centros de cuidado infantil en días específicos o de complemento del horario escolar",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidado infantil en días específicos o de complemento del horario escolar). Solo padre." ~ "Centros de cuidado infantil en días específicos o de complemento del horario escolar",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidados en la empresa). Ambos." ~ "Centros de cuidados en la empresa",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidados en la empresa). Solo madre." ~ "Centros de cuidados en la empresa",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidados en la empresa). Solo padre." ~ "Centros de cuidados en la empresa",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Contabilización horas semanales o mensuales). Ambos." ~ "Contabilización horas semanales o mensuales",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Contabilización horas semanales o mensuales). Solo madre." ~ "Contabilización horas semanales o mensuales",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Contabilización horas semanales o mensuales). Solo padre." ~ "Contabilización horas semanales o mensuales",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Convenios con servicios de cuidados). Ambos." ~ "Convenios con servicios de cuidados",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Convenios con servicios de cuidados). Solo madre." ~ "Convenios con servicios de cuidados",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Convenios con servicios de cuidados). Solo padre." ~ "Convenios con servicios de cuidados",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Flexibilidad horaria en períodos especiales). Ambos." ~ "Flexibilidad horaria en períodos especiales",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Flexibilidad horaria en períodos especiales). Solo madre." ~ "Flexibilidad horaria en períodos especiales",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Flexibilidad horaria en períodos especiales). Solo padre." ~ "Flexibilidad horaria en períodos especiales",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Licencia especial para cuidado familiar). Ambos." ~ "Licencia especial para cuidado familiar",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Licencia especial para cuidado familiar). Solo madre." ~ "Licencia especial para cuidado familiar",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Licencia especial para cuidado familiar). Solo padre." ~ "Licencia especial para cuidado familiar",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Otro). Ambos." ~ "Otro",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Otro). Solo madre." ~ "Otro",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Otro). Solo padre." ~ "Otro",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Permiso acompañar familiares citas médicas o actividades educativas). Ambos." ~ "Permiso acompañar familiares citas médicas o actividades educativas",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Permiso acompañar familiares citas médicas o actividades educativas). Solo madre." ~ "Permiso acompañar familiares citas médicas o actividades educativas",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Permiso acompañar familiares citas médicas o actividades educativas). Solo padre." ~ "Permiso acompañar familiares citas médicas o actividades educativas",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Priorización horarios). Ambos." ~ "Priorización horarios",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Priorización horarios). Solo madre." ~ "Priorización horarios",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Priorización horarios). Solo padre." ~ "Priorización horarios",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Reintegro gradual luego de permiso medio horario). Ambos." ~ "Reintegro gradual luego de permiso medio horario",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Reintegro gradual luego de permiso medio horario). Solo madre." ~ "Reintegro gradual luego de permiso medio horario",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Reintegro gradual luego de permiso medio horario). Solo padre." ~ "Reintegro gradual luego de permiso medio horario",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Salas de lactancia). Ambos." ~ "Salas de lactancia",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Salas de lactancia). Solo madre." ~ "Salas de lactancia",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Salas de lactancia). Solo padre." ~ "Salas de lactancia",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Trabajo a distancia). Ambos." ~ "Trabajo a distancia",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Trabajo a distancia). Solo madre." ~ "Trabajo a distancia",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Trabajo a distancia). Solo padre." ~ "Trabajo a distancia",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Vales o transferencias para servicios de cuidados). Ambos." ~ "Vales o transferencias para servicios de cuidados",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Vales o transferencias para servicios de cuidados). Solo madre." ~ "Vales o transferencias para servicios de cuidados",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Vales o transferencias para servicios de cuidados). Solo padre." ~ "Vales o transferencias para servicios de cuidados",
  TRUE ~  "Todos"))


df <- df %>% mutate (benef2 = case_when(
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Actividades especiales para perpiodos de vacaciones). Ambos." ~ "Ambos",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Actividades especiales para perpiodos de vacaciones). Solo madre." ~ "Solo madre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Actividades especiales para perpiodos de vacaciones). Solo padre." ~ "Solo padre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidado infantil en días específicos o de complemento del horario escolar). Ambos." ~ "Ambos",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidado infantil en días específicos o de complemento del horario escolar). Solo madre." ~ "Solo madre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidado infantil en días específicos o de complemento del horario escolar). Solo padre." ~ "Solo padre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidados en la empresa). Ambos." ~ "Ambos",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidados en la empresa). Solo madre." ~ "Solo madre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidados en la empresa). Solo padre." ~ "Solo padre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Contabilización horas semanales o mensuales). Ambos." ~ "Ambos",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Contabilización horas semanales o mensuales). Solo madre." ~ "Solo madre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Contabilización horas semanales o mensuales). Solo padre." ~ "Solo padre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Convenios con servicios de cuidados). Ambos." ~ "Ambos",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Convenios con servicios de cuidados). Solo madre." ~ "Solo madre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Convenios con servicios de cuidados). Solo padre." ~ "Solo padre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Flexibilidad horaria en períodos especiales). Ambos." ~ "Ambos",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Flexibilidad horaria en períodos especiales). Solo madre." ~ "Solo madre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Flexibilidad horaria en períodos especiales). Solo padre." ~ "Solo padre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Licencia especial para cuidado familiar). Ambos." ~ "Ambos",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Licencia especial para cuidado familiar). Solo madre." ~ "Solo madre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Licencia especial para cuidado familiar). Solo padre." ~ "Solo padre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Otro). Ambos." ~ "Ambos",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Otro). Solo madre." ~ "Solo madre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Otro). Solo padre." ~ "Solo padre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Permiso acompañar familiares citas médicas o actividades educativas). Ambos." ~ "Ambos",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Permiso acompañar familiares citas médicas o actividades educativas). Solo madre." ~ "Solo madre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Permiso acompañar familiares citas médicas o actividades educativas). Solo padre." ~ "Solo padre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Priorización horarios). Ambos." ~ "Ambos",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Priorización horarios). Solo madre." ~ "Solo madre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Priorización horarios). Solo padre." ~ "Solo padre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Reintegro gradual luego de permiso medio horario). Ambos." ~ "Ambos",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Reintegro gradual luego de permiso medio horario). Solo madre." ~ "Solo madre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Reintegro gradual luego de permiso medio horario). Solo padre." ~ "Solo padre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Salas de lactancia). Ambos." ~ "Ambos",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Salas de lactancia). Solo madre." ~ "Solo madre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Salas de lactancia). Solo padre." ~ "Solo padre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Trabajo a distancia). Ambos." ~ "Ambos",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Trabajo a distancia). Solo madre." ~ "Solo madre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Trabajo a distancia). Solo padre." ~ "Solo padre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Vales o transferencias para servicios de cuidados). Ambos." ~ "Ambos",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Vales o transferencias para servicios de cuidados). Solo madre." ~ "Solo madre",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Vales o transferencias para servicios de cuidados). Solo padre." ~ "Solo padre",
  TRUE ~  "Todos"))



df <- df %>% mutate (benef2 = case_when(
  NOMINDICADOR=="Participación de personas en el cuidado de niños/as fuera del horario escolar (Abuelo/a)" |
    NOMINDICADOR=="Participación de personas en el cuidado de niños/as cuando enferman (Abuelo/a)" ~ "Abuelo/a",
  NOMINDICADOR=="Participación de personas en el cuidado de niños/as fuera del horario escolar (Hermano/a)"|
    NOMINDICADOR=="Participación de personas en el cuidado de niños/as cuando enferman (Hermano/a)" ~ "Hermano/a",
  NOMINDICADOR=="Participación de personas en el cuidado de niños/as fuera del horario escolar (Madre)"|
    NOMINDICADOR=="Participación de personas en el cuidado de niños/as cuando enferman (Madre)" ~ "Madre",
  NOMINDICADOR=="Participación de personas en el cuidado de niños/as fuera del horario escolar (Padre)"|
    NOMINDICADOR=="Participación de personas en el cuidado de niños/as cuando enferman (Padre)" ~ "Padre",
  NOMINDICADOR=="Participación de personas en el cuidado de niños/as fuera del horario escolar (Otro pariente o no pariente)"|
    NOMINDICADOR=="Participación de personas en el cuidado de niños/as cuando enferman (Otro pariente o no pariente)" ~ "Otro pariente o no pariente",
  NOMINDICADOR=="Participación de personas en el cuidado de niños/as fuera del horario escolar (Persona remunerada)"|
    NOMINDICADOR=="Participación de personas en el cuidado de niños/as cuando enferman (Persona remunerada)" ~ "Persona remunerada",
  TRUE ~ "Todos"))


df <- df %>% mutate (pob_cuid = case_when(
  NOMINDICADOR=="Tasa de participación en las actividades de cuidados dentro del hogar (mayores de 65 años)"|
    NOMINDICADOR=="Promedio de horas en las actividades de cuidados dentro del hogar (mayores de 65 años)" ~ "Mayores de 65 años",
  NOMINDICADOR=="Tasa de participación en las actividades de cuidados dentro del hogar (niños/as de 0 a 12 años)"|
    NOMINDICADOR=="Promedio de horas en las actividades de cuidados dentro del hogar (niños/as de 0 a 12)" ~ "Niños/as de 0 a 12 años",
  NOMINDICADOR=="Tasa de participación en las actividades de cuidados dentro del hogar (niños/as de 0 a 3 años)"|
    NOMINDICADOR=="Promedio de horas en las actividades de cuidados dentro del hogar (niños/as de 0 a 3 años)" ~ "Niños/as de 0 a 3 años",
  NOMINDICADOR=="Tasa de participación en las actividades de cuidados dentro del hogar (niños/as de 4 a 5 años)"|
    NOMINDICADOR=="Promedio de horas en las actividades de cuidados dentro del hogar (niños/as de 4 a 5 años)" ~ "Niños/as de 4 a 5 años",
  NOMINDICADOR=="Tasa de participación en las actividades de cuidados dentro del hogar (niños/as de 6 a 12 años)"|
    NOMINDICADOR=="Promedio de horas en las actividades de cuidados dentro del hogar (niños/as de 6 a 12 años)" ~ "Niños/as de 6 a 12 años",
  NOMINDICADOR=="Tasa de participación en las actividades de cuidados dentro del hogar (personas con discapacidad)"|
    NOMINDICADOR=="Promedio de horas en las actividades de cuidados dentro del hogar (personas con discapacidad)" ~ "Personas con discapacidad",
  TRUE ~ "Todos"))




df <- df %>% mutate (THOG = case_when(
  NOMINDICADOR=="Distribución porcentual de los hogares según tipo de hogar (Unipersonal)"|
    NOMINDICADOR=="Distribución porcentual de hogares con menores según tipo de hogar (Unipersonal)"|
    NOMINDICADOR=="Distribución porcentual de las personas según tipo de hogar en el que residen (Unipersonal)"|
    NOMINDICADOR=="Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Unipersonal)" ~ "Unipersonal", 
  NOMINDICADOR=="Distribución porcentual de los hogares según tipo de hogar (Pareja sin hijos)"|
    NOMINDICADOR=="Distribución porcentual de hogares con menores según tipo de hogar (Pareja sin hijos)"|
    NOMINDICADOR=="Distribución porcentual de las personas según tipo de hogar en el que residen (Pareja sin hijos)"|
    NOMINDICADOR=="Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Pareja sin hijos)" ~ "Pareja sin hijos", 
  NOMINDICADOR=="Distribución porcentual de los hogares según tipo de hogar (Biparental)"|
    NOMINDICADOR=="Distribución porcentual de hogares con menores según tipo de hogar (Biparental)"|
    NOMINDICADOR=="Distribución porcentual de las personas según tipo de hogar en el que residen (Biparental)"|
    NOMINDICADOR=="Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Biparental)" ~ "Biparental", 
  NOMINDICADOR=="Distribución porcentual de los hogares según tipo de hogar (Monoparental femenino)"|
    NOMINDICADOR=="Distribución porcentual de hogares con menores según tipo de hogar (Monoparental femenino)"|
    NOMINDICADOR=="Distribución porcentual de las personas según tipo de hogar en el que residen (Monoparental femenino)"|
    NOMINDICADOR=="Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Monoparental femenino)" ~ "Monoparental femenino", 
  NOMINDICADOR=="Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Monoparental masculino)"|
    NOMINDICADOR=="Distribución porcentual de los hogares según tipo de hogar (Monoparental masculino)"|
    NOMINDICADOR=="Distribución porcentual de hogares con menores según tipo de hogar (Monoparental masculino)"|
    NOMINDICADOR=="Distribución porcentual de las personas según tipo de hogar en el que residen (Monoparental masculino)" ~ "Monoparental masculino", 
  NOMINDICADOR=="Distribución porcentual de los hogares según tipo de hogar (Extendido o compuesto)"|
    NOMINDICADOR=="Distribución porcentual de hogares con menores según tipo de hogar (Extendido o compuesto)"|
    NOMINDICADOR=="Distribución porcentual de las personas según tipo de hogar en el que residen (Extendido o compuesto)"|
    NOMINDICADOR=="Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Extendido o compuesto)" ~ "Extendido o compuesto", 
  NOMINDICADOR=="Distribución porcentual de los hogares según tipo de hogar (Extendido con núcleo monoparental)"|
    NOMINDICADOR=="Distribución porcentual de hogares con menores según tipo de hogar (Extendido con núcleo monoparental)"|
    NOMINDICADOR=="Distribución porcentual de las personas según tipo de hogar en el que residen (Extendido con núcleo monoparental)"|
    NOMINDICADOR=="Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Extendido con núcleo monoparental)" ~ "Extendido con núcleo monoparental", 
  NOMINDICADOR=="Distribución porcentual de los hogares según tipo de hogar (Sin núcleo)"|
    NOMINDICADOR=="Distribución porcentual de hogares con menores según tipo de hogar (Sin núcleo)"|
    NOMINDICADOR=="Distribución porcentual de las personas según tipo de hogar en el que residen (Sin núcleo)"|
    NOMINDICADOR=="Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Sin núcleo)" ~ "Sin núcleo", 
  TRUE ~ "Todos"))




df <- df %>% mutate (PAREJA = case_when(
  NOMINDICADOR=="Distribución porcentual de los hogares biparentales según modelo de pareja (Doble carrera)" ~ "Doble carrera",
  NOMINDICADOR=="Distribución porcentual de los hogares biparentales según modelo de pareja (Inversión de roles modificado)" ~ "Inversión de roles modificado",
  NOMINDICADOR=="Distribución porcentual de los hogares biparentales según modelo de pareja (Inversión de roles)" ~ "Inversión de roles",
  NOMINDICADOR=="Distribución porcentual de los hogares biparentales según modelo de pareja (Proveedor modificado)" ~ "Proveedor modificado",
  NOMINDICADOR=="Distribución porcentual de los hogares biparentales según modelo de pareja (Proveedor tradicional)" ~ "Proveedor tradicional",
  NOMINDICADOR=="Distribución porcentual de los hogares biparentales según modelo de pareja (Residual)" ~ "Residual",
  TRUE ~ "Todos"))



df <- df %>% mutate (EST_CONY = case_when(
  NOMINDICADOR=="Distribución porcentual de las personas según estado conyugal (Casada/o)" ~ "Casada/o",
  NOMINDICADOR=="Distribución porcentual de las personas según estado conyugal (Divorciada/o o serparada/o)" ~ "Divorciada/o o serparada/o",
  NOMINDICADOR=="Distribución porcentual de las personas según estado conyugal (Soltera/o)" ~ "Soltera/o",
  NOMINDICADOR=="Distribución porcentual de las personas según estado conyugal (Unión libre)" ~ "Unión libre",
  NOMINDICADOR=="Distribución porcentual de las personas según estado conyugal (Viuda/o)" ~ "Viuda/o",
  TRUE ~ "Todos"))


df <- df %>% mutate (CICLOVIDA = case_when(
  NOMINDICADOR=="Distribución porcentual de los hogares según ciclo de vida del hogar (Consolidación)" ~ "Consolidación",
  NOMINDICADOR=="Distribución porcentual de los hogares según ciclo de vida del hogar (Etapa de salida)" ~ "Etapa de salida",
  NOMINDICADOR=="Distribución porcentual de los hogares según ciclo de vida del hogar (Etapa inicial)" ~ "Etapa inicial",
  NOMINDICADOR=="Distribución porcentual de los hogares según ciclo de vida del hogar (Expansión o crecimiento)" ~ "Expansión o crecimiento",
  NOMINDICADOR=="Distribución porcentual de los hogares según ciclo de vida del hogar (Hoagres no familiares)" ~ "Hogares no familiares",
  NOMINDICADOR=="Distribución porcentual de los hogares según ciclo de vida del hogar (Pareja joven sin hijos)" ~ "Pareja joven sin hijos",
  NOMINDICADOR=="Distribución porcentual de los hogares según ciclo de vida del hogar (Pareja mayor sin hijos)" ~ "Pareja mayor sin hijos",
  TRUE ~ "Todos"))




df <- df %>% mutate (NINT = case_when(
  NOMINDICADOR=="Distribución porcentual de los hogares según cantidad de integrantes (1 persona)" ~ "1 persona",
  NOMINDICADOR=="Distribución porcentual de los hogares según cantidad de integrantes (2 personas)" ~ "2 personas",
  NOMINDICADOR=="Distribución porcentual de los hogares según cantidad de integrantes (3 personas)" ~ "3 personas",
  NOMINDICADOR=="Distribución porcentual de los hogares según cantidad de integrantes (4 personas)" ~ "4 personas",
  NOMINDICADOR=="Distribución porcentual de los hogares según cantidad de integrantes (5 o más personas)" ~ "5 o más personas",
  TRUE ~ "Todos"))



# Hay que armar variable corte1 con los nombres de los cortes 
df <- df %>% mutate(CORTE1= case_when(SEXO!="Todos" ~ "SEXO",
                                      ASCENDENCIA!="Todos" ~  "ASCENDENCIA",
                                      QUINTIL!="Todos" ~  "QUINTIL",
                                      DEPARTAMENTOUY!="Todos" ~  "DEPARTAMENTOUY",
                                      URBANORURALUY!="Total país" ~  "URBANORURALUY",
                                      EDAD!="Todos" ~  "EDAD",
                                      POBRE!="Todos" ~  "POBRE",
                                      NSE!="Todos" ~  "NSE",
                                      NIVELEDU!="Todos" ~  "NIVELEDU",
                                      EDAD_HIJ!="Todos" ~  "EDAD_HIJ",
                                      benef2!= "Todos" ~ "benef2"))


df <- df %>% mutate(CORTE2= case_when(EDADSALIDA!="Todos" ~  "EDADSALIDA",
                                      SALIDA!="Todos"~"SALIDA",
                                      PLANIF1!="Todos"~"PLANIF1",
                                      PLANIF2!="Todos"~"PLANIF2",
                                      PLANIF3!="Todos"~"PLANIF3",
                                      VIOLENCIA!="Todos"~"VIOLENCIA",
                                      benef1!="Todos"~"benef1",
                                      pob_cuid!="Todos"~"pob_cuid",
                                      THOG !="Todos"~"THOG",
                                      PAREJA!="Todos"~"PAREJA",
                                      EST_CONY!="Todos"~"EST_CONY",
                                      SEXO2 !="Todos"~"SEXO2",
                                      CICLOVIDA!="Todos"~"CICLOVIDA",
                                      NINT!="Todos"~"NINT",
                                      TRUE ~ "NA"))

#Cambio NOMINDICADOR para que cada indicador tenga un único VALOR en la columna

source('Modif NOMINDICADOR.R')


options(shiny.sanitize.errors = TRUE) 

ui <- fluidPage(tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')), 
                
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"), # Quita mensajes de error (ojo)
                
                # Home page css
                tags$style(".hr {border-top: 1px solid #D4D4D4;}"),
                tags$style(".footer {left: 0;
                      margin-top: -180px; /* negative value of footer height */
	                    position:fixed;
                      bottom: 0;
                      width: 100%;
                      height:150px;
                      background-color: #FFFFFF;
                      border-top: 1px solid #fff;
                      text-align: center;
                      padding-top: 30px;
                  }"),
                tags$style(".img_footer{padding-top: 10px; padding-right: 100px; padding-left: 100px;}"),
                tags$style(".imgContainerFooter{padding-top: 20px; float: right;}"),
                tags$style(".imgContainerFooter img {height: 40px; padding: 5px 5px 5px 5px;}"),
                tags$style(".logoContainerFooter{padding-top: 0px;float: left;}"),
                tags$style(".logoContainerFooter img {height: 50px;}"),
                tags$style(".well {background-color: #D4E6F1;}"),
                tags$style(".col-sm-2 {width: 16.66666667%; padding-top: 50px;}"),
                tags$style(".body {font-family: 'Roboto'}"),
                #                # Revisar
                tags$style(".navbar {background-color: #FFF;
             -webkit-transition: padding .3s;
             -moz-transition: padding .3s;
             transition: padding .3s;
             border: none;}"),
                tags$style(".wrapper {
	display: flex;
	flex-direction: column;
	min-height: 100vh;
}"),
                tags$style(".container {
    width: 100%;
    height: 100%;
    display: flex;
    flex-direction: column;
    flex-wrap: nowrap;
}"),
                tags$style(".body{flex-grow: 1;
    overflow: auto;
    min-height: 2em;
}"),
                tags$style("footer{flex-shrink: 0;}"),
                tags$style(".fa-calculator {color:#21618C}"),
                tags$style(".fa-exclamation {color:#21618C}"),
                tags$style(".fa-clock {color:#21618C}"),              
                
                navbarPage(
                  theme = tema_umad,
                  "Composición de los hogares y familia",
                  collapsible=TRUE,
                  #                  
                  tabPanel(
                    
                    #   2.0. HOME ----
                    title = "Home",
                    value = 'borelito',
                    icon = icon("fas fa-home"),
                    
                    # Top layout
                    HTML("<p><center><h2><b> Bienvenida/o a la app de datos sobre Composición de los hogares y familia. &nbsp; &nbsp; <a href='https://umad.cienciassociales.edu.uy/'> <img src='umadlogo_2.png' width=160' height='80'> </a> </b></h2></p>"),
                    
                    # Border line
                    hr(style = "border:1.5px solid #2c3e50"),
                    
                    fluidRow(
                      br(),
                      br(),
                      column(width = 1),
                      column(width = 5,
                             tags$h4(style="color:#085792;text-align:justify;font-weight: bold;",
                                     "Sobre la app"),
                             tags$h6(style="color:#597ea2;text-align:justify;",
                                     "Esta aplicación es parte del sistema de búsqueda y construcción personalizada de tablas y gráficos en base a motores generados desde bases consolidadas por los equipos de la UMAD."),
                             # actionLink("link_elecciones", "Pestaña Elecciones"),
                             HTML("<h6> <p style='color:#597ea2' align='justify'> El tablero presenta los principales indicadores sobre composición del hogar y familia. Incluye visualizaciones de indicadores sobre composición y estructura de los hogares, planificación familiar, violencia de género, cuidados y educación y salud infantil. </p></h6>"),               
                             tags$h6(style="color:#085792;text-align:left;font-weight: bold;",
                                     "Mantenedor: Jimena Pandolfi y Sharon Katzkowicz"),
                             tags$h6(style="color:#085792;text-align:left;font-weight: bold;",
                                     "Contacto: umad@cienciassociales.edu.uy"),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br()
                      ),
                      column(width = 1),
                      column(width = 4,
                             tags$h4(style="color:#009fe3;text-align:justify;font-weight: bold;",
                                     "Sobre la UMAD"),
                             tags$h6(style="color:#597ea2;text-align:justify;",
                                     "La Unidad de Métodos y Acceso a Datos de la Facultad de Ciencias Sociales se constituye como un espacio de producción, organización y difusión de datos y abordajes metodológicos relativos a la realidad socioeconómica y política del país, la región y el mundo."),
                             tags$h6(style="color:#597ea2;text-align:justify;",
                                     "Lo hace ofreciendo a los usuarios un observatorio socioeconómico y político del Uruguay y de Uruguay en la Región y el Mundo, un sistema de búsqueda y construcción personalizada de tablas y gráficos en base a motores generados desde bases consolidadas por los equipos de la UMAD y un repositorio de acceso libre a micro-datos con sus fichas técnicas, metadatos y formularios así como vínculos a recursos nacionales e internacionales (motores de búsqueda de datos, sistemas de consulta y micro-datos)."),
                             br(),
                             tags$div(
                               tags$h5(style="color:#085792;text-align:left;font-weight: bold;",
                                       "Recursos relacionados:"),
                               tags$ul(style="color:#597ea2;text-align:left;",
                                       tags$li(tags$h6(tags$a(href="https://umad-fcs.github.io/Piso-I-Familia/", "Indicadores Composición de los hogares y familia en Uruguay"))), 
                                       tags$li(tags$h6(tags$a(href="https://bancodedatos-fcs.shinyapps.io/appDemografia/", "Tablero indicadores demográficos"))), 
                                       tags$li(tags$h6(tags$a(href="https://umad-fcs.github.io/Piso-I-Demografia/", "Indicadores demográficos del Uruguay")))
                                       
                               )
                             ),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br()
                      ),
                      column(width = 1),
                    ),
                    div(class = "footer",
                        includeHTML("html/footer.html"))
                    )))
                    
#HASTA ACÄ ESTÄ BIEN 


                    














library(shiny)  

# Definimos el servidor  
server <- function(input, output, session) {  
  
  # Observador para las dimensiones de la ventana  
  observeEvent(input$dimension, {  
    # Puedes realizar acciones basadas en las dimensiones,  
    # Por ahora solo se imprime en la consola.  
    cat("Ancho:", input$dimension[1], "Altura:", input$dimension[2], "\n")  
  })  
  
  # Puedes incluir más funciones de salida y lógicas aquí  
}  

                                                                      
                                                                                             
# Ejecutar la aplicación Shiny  
shinyApp(ui = ui, server = server)
                    
                    

                    