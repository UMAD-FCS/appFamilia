library(tidyverse)
library(dplyr)


### Carga y modifica los datos 

df <- rio::import("Base_motor_02082024.xls")
#df <- rio::import("Base_Motor_familia_20052024.xlsx")
metadata <- rio::import("Fichas_Familia.xls")


df <- df %>% mutate(TERCIL = case_when(QUINTIL=="Tercil 1" ~ "Tercil 1",
                                       QUINTIL=="Tercil 2" ~ "Tercil 2",
                                       QUINTIL=="Tercil 3" ~ "Tercil 3"))


df <- df %>%   mutate(QUINTIL = case_when(QUINTIL %in% c("Tercil 1", "Tercil 2", "Tercil 3") ~ "Todos",
                                          TRUE ~ QUINTIL))


df <- df %>% mutate(SALIDA = case_when(
  NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que nunca se fueron de su hogar de origen" ~ "No se fueron",
  NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen y no volvieron" ~ "Se fueron y no volvieron",
  NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen y volvieron" ~ "Se fueron y volvieron",
  NOMINDICADOR!="Porcentaje de jóvenes y adolescentes que nunca se fueron de su hogar de origen"&
  NOMINDICADOR!="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen y no volvieron"&
  NOMINDICADOR!="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen y volvieron"~"Todos"))

                                                                                                                                 
df <- df %>% mutate(EDADSALIDA = case_when(NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez (Entre 18 y 20)" ~ "De 18 a 20 años", 
                                           NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez (Menor a 18)" ~ "18 años o menos",
                                           NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez (Entre 21 y 30)" ~ "De 21 a 30 años",
                                           NOMINDICADOR!="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez (Entre 18 y 20)"& 
                                             NOMINDICADOR!="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez (Menor a 18)"&
                                             NOMINDICADOR!="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez (Entre 21 y 30)" ~ "Todos"))

df <- df %>% mutate(QSALIDA = case_when(NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Amigos)" ~ "Amigos",
                                       NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Otros no parientes o institución)" ~ "Otros no parientes o institución",
                                       NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Otros parientes)" ~ "Otros parientes",
                                       NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Pareja)" ~ "Pareja",
                                       NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Solo)" ~ "Solo",
                                       NOMINDICADOR!="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Amigos)"&
                                         NOMINDICADOR!="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Otros no parientes o institución)"&
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


df <- df %>% mutate(SEXO2 = case_when(NOMINDICADOR=="Distribución porcentual de los hogares según sexo del jefe/a (Mujer)" ~ "Mujer",
                                      NOMINDICADOR=="Distribución porcentual de los hogares según sexo del jefe/a (Varón)" ~ "Varón",
                                      TRUE ~ "Todos"))

df <- df %>% mutate(SEXO = case_when(NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años sin menores en el hogar (Mujer)" ~ "Mujeres",
                                     NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años sin menores en el hogar (Varón)" ~ "Varones",
                                     NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 4 en el hogar (Mujer)" ~ "Mujer",
                                     NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 4 en el hogar (Varón)" ~ "Varones",
                                     NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 7 en el hogar (Mujer)" ~ "Mujeres",
                                     NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 7 en el hogar (Varón)" ~ "Varones",
                                     NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 12 en el hogar (Mujer)" ~ "Mujeres",
                                     NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 12 en el hogar (Varón)" ~ "Varones",
                                     TRUE ~ SEXO))

df <- df %>% mutate (benef1 = case_when(
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Actividades especiales para perpiodos de vacaciones). Ambos." ~ "Actividades especiales para períodos de vacaciones",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Actividades especiales para perpiodos de vacaciones). Solo madre." ~ "Actividades especiales para períodos de vacaciones",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Actividades especiales para perpiodos de vacaciones). Solo padre." ~ "Actividades especiales para períodos de vacaciones",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidado infantil en días específicos o de complemento del horario escolar). Ambos." ~ "Centros de cuidado infantil complementarios",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidado infantil en días específicos o de complemento del horario escolar). Solo madre." ~ "Centros de cuidado infantil complementarios",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidado infantil en días específicos o de complemento del horario escolar). Solo padre." ~ "Centros de cuidado infantil complementarios",
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
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Permiso acompañar familiares citas médicas o actividades educativas). Ambos." ~ "Permiso acompañamiento familiares",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Permiso acompañar familiares citas médicas o actividades educativas). Solo madre." ~ "Permiso acompañamiento familiares",
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Permiso acompañar familiares citas médicas o actividades educativas). Solo padre." ~ "Permiso acompañamiento familiares",
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



df <- df %>% mutate (Cuidador = case_when(
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
    NOMINDICADOR=="Distribución porcentual de las personas según tipo de hogar en el que residen (Extendido o compuesto con núcleo monoparental)"|
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
  NOMINDICADOR=="Distribución porcentual de las personas según estado conyugal (Divorciada/o o separada/o)" ~ "Divorciada/o o separada/o",
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

df <- df %>% mutate(EDAD = case_when(EDAD == "65 años o más" ~ "De 65 años o más",
                                     EDAD == "Entre 14 y 29 años" ~ "De 14 a 29 años",
                                     EDAD == "Entre 14 y 30 años" ~ "De 14 a 30 años",
                                     EDAD == "Entre 15 y 19 años" ~ "De 15 a 19 años",
                                     EDAD == "Entre 15 y 29 años" ~ "De 15 a 29 años",
                                     EDAD == "Entre 20 y 29 años" ~ "De 20 a 29 años",
                                     EDAD == "Entre 20 y 29 años." ~ "De 20 a 29 años",
                                     EDAD == "Entre 30 y 44" ~ "De 30 a 44 años",
                                     EDAD == "Entre 30 y 44 años" ~ "De 30 a 44 años",
                                     EDAD == "Entre 30 y 44 años." ~ "De 30 a 44 años",
                                     EDAD == "Entre 30 y 49 años" ~ "De 30 a 49 años",
                                     EDAD == "Entre 45 y 64 años" ~ "De 45 a 64 años",
                                     EDAD == "Entre 50 y 64 años" ~ "De 50 a 64 años",
                                     EDAD == "Mayor de 30 años" ~ "De 30 años o más",
                                     EDAD == "Mayor de 64 años" ~ "De 64 años o más",
                                     EDAD == "Menor de 20 años." ~ "De 0 a 20 años",
                                     EDAD == "Menor de 30 años" ~ "De 0 a 30 años",
                                     TRUE ~ EDAD))

df <- df %>% mutate(EDAD = case_when(substring(NOMINDICADOR, 1, 32)=="Tasa de participación de mujeres" & EDAD=="De 0 a 30 años" ~"De 14 a 30 años",
                                     TRUE ~ EDAD))

df <- df %>% mutate(EDAD_HIJ = case_when(EDAD_HIJ == "Menor de 20 años." ~ "20 años o menos",
                                         EDAD_HIJ == "Entre 20 y 29 años." ~ "De 20 a 29 años",
                                         EDAD_HIJ == "Entre 30 y 44 años." ~ "De 30 a 44 años",
                                         TRUE ~ "Todos"))


# Hay que armar variable corte1 con los nombres de los cortes 
df <- df %>% mutate(CORTE1= case_when(SEXO!="Todos" ~ "SEXO",
                                      ASCENDENCIA!="Todos" ~  "ASCENDENCIA",
                                      QUINTIL!="Todos" ~  "QUINTIL",
                                      TERCIL!="Todos" ~  "TERCIL",
                                      EDAD!="Todos" ~  "EDAD",
                                      POBRE!="Todos" ~  "POBRE",
                                      NSE!="Todos" ~  "NSE",
                                      NIVELEDU!="Todos" ~  "NIVELEDU",
                                      EDAD_HIJ!="Todos" ~  "EDAD_HIJ",
                                      benef2!= "Todos" ~ "benef2",
                                      URBANORURALUY!="Total país" ~  "URBANORURALUY"))


df <- df %>% mutate(CORTE2= case_when(EDADSALIDA!="Todos" ~  "EDADSALIDA",
                                      QSALIDA!="Todos"~"QSALIDA",
                                      SALIDA!="Todos"~"SALIDA",
                                      PLANIF1!="Todos"~"PLANIF1",
                                      PLANIF2!="Todos"~"PLANIF2",
                                      PLANIF3!="Todos"~"PLANIF3",
                                      substring(NOMINDICADOR,1,24)=="Prevalencia de violencia"~"VIOLENCIA",
                                      benef1!="Todos"~"benef1",
                                      Cuidador!="Todos"~"Cuidador",
                                      pob_cuid!="Todos"~"pob_cuid",
                                      THOG !="Todos"~"THOG",
                                      PAREJA!="Todos"~"PAREJA",
                                      EST_CONY!="Todos"~"EST_CONY",
                                      SEXO2 !="Todos"~"SEXO2",
                                      CICLOVIDA!="Todos"~"CICLOVIDA",
                                      NINT!="Todos"~"NINT",
                                      TRUE ~ "NA"))

##Corrige corte 1 y corte 2 de Tasa de participación de varones y mujeres entre 14 y 49 años:

df <- df %>% mutate(CORTE2 = case_when(
  substring(NOMINDICADOR, 1, 32)=="Tasa de participación de mujeres" ~ "SEXO",
  TRUE ~ CORTE2))

df <- df %>% mutate(CORTE1= case_when(substring(NOMINDICADOR, 1, 32)=="Tasa de participación de mujeres"&ASCENDENCIA!="Todos" ~  "ASCENDENCIA",
                                      substring(NOMINDICADOR, 1, 32)=="Tasa de participación de mujeres"&QUINTIL!="Todos" ~  "QUINTIL",
                                      substring(NOMINDICADOR, 1, 32)=="Tasa de participación de mujeres"&TERCIL!="Todos" ~  "TERCIL",
                                      substring(NOMINDICADOR, 1, 32)=="Tasa de participación de mujeres"&EDAD!="Todos" ~  "EDAD",
                                      substring(NOMINDICADOR, 1, 32)=="Tasa de participación de mujeres"&POBRE!="Todos" ~  "POBRE",
                                      substring(NOMINDICADOR, 1, 32)=="Tasa de participación de mujeres"&NSE!="Todos" ~  "NSE",
                                      substring(NOMINDICADOR, 1, 32)=="Tasa de participación de mujeres"&NIVELEDU!="Todos" ~  "NIVELEDU",
                                      substring(NOMINDICADOR, 1, 32)=="Tasa de participación de mujeres"&EDAD_HIJ!="Todos" ~  "EDAD_HIJ",
                                      substring(NOMINDICADOR, 1, 32)=="Tasa de participación de mujeres"&benef2!= "Todos" ~ "benef2",
                                      substring(NOMINDICADOR, 1, 32)=="Tasa de participación de mujeres"&URBANORURALUY!="Total país" ~  "URBANORURALUY",
                                      TRUE ~ CORTE1))

df <- df %>% mutate(CORTE1= case_when(substring(NOMINDICADOR, 1, 32)=="Tasa de participación de mujeres"&CORTE1=="SEXO" ~ NA,
                                      TRUE ~ CORTE1))
  



#Cambio NOMINDICADOR para que cada indicador tenga un único VALOR en la columna

source('Modif NOMINDICADOR.R')

  


  
df <- df %>% mutate(VALOR = case_when(NOMINDICADOR=="Tasa de divorcios"|
                                        NOMINDICADOR=="Tasa de nupcialidad"|
                                        NOMINDICADOR=="Tamaño medio de los hogares"|
                                        NOMINDICADOR=="Tasa específica de fecundidad adolescente"|
                                        NOMINDICADOR=="Promedio de horas en las actividades de cuidados dentro del hogar"|
                                        NOMINDICADOR=="Tasa de mortalidad infantil (menores de 1 año)"|
                                        NOMINDICADOR=="Tasa de mortalidad infantil (menores de 5 años)" ~ round(as.numeric(VALOR),1),
                                      TRUE ~ round(as.numeric(VALOR)*100,1)))

df <- df %>% mutate(FECHA2 = as.character(FECHA))

df <- df %>% mutate(CORTE1_rec = case_when(CORTE1=="ASCENDENCIA" ~ "Ascendencia étnico racial", 
                                           CORTE1=="benef2" ~ "Perceptos de beneficios",
                                           CORTE1=="DEPARTAMENTOUY" ~ "Departamento",
                                           CORTE1=="EDAD" ~ "Edad",
                                           CORTE1=="EDAD_HIJ" ~ "Edad al tener el primer hijo",
                                           CORTE1=="NIVELEDU" ~ "Nivel educativo",
                                           CORTE1=="NSE" ~ "Nivel socioeconómico",
                                           CORTE1=="POBRE" ~ "Pobreza",
                                           CORTE1=="QUINTIL" ~ "Quintil de ingresos",
                                           CORTE1=="TERCIL" ~ "Tercil de ingresos",
                                           CORTE1=="SEXO" ~ "Sexo",
                                           CORTE1=="URBANORURALUY" ~ "Región",
                                           is.na(CORTE1)==T ~ "Total"))

df <- df %>% mutate(CORTE2_rec = case_when(
    CORTE2== "SEXO" ~ "Sexo",
    CORTE2== "THOG" ~ "Tipo de hogar",
    CORTE2== "CICLOVIDA" ~ "Ciclo de vida",
    CORTE2== "PAREJA" ~ "Modelo de pareja",
    CORTE2== "EST_CONY" ~ "Estado conyugal",
    CORTE2== "SEXO2" ~ "Sexo del jefe/a",
    CORTE2== "NINT" ~ "Cantidad de integrantes",
    CORTE2== "SALIDA" ~ "Salida del hogar de origen",
    CORTE2== "EDADSALIDA" ~ "Edad salida del hogar de origen",  
    CORTE2== "QSALIDA" ~ "Con quién salió del hogar de origen",
    CORTE2== "PLANIF1" ~ "Planificación familiar - Tiempo",
    CORTE2== "PLANIF2" ~ "Planificación familiar - Cantidad hijos",
    CORTE2== "PLANIF3" ~ "Planificación familiar - Edad",
    CORTE2== "VIOLENCIA" ~ "Tipo de violencia",
    CORTE2== "benef1" ~ "Beneficioslaborales",
    CORTE2== "pob_cuid" ~ "Población que recibe cuidados",
    CORTE2== "Cuidador" ~ "Cuidador/a"))



sectmeta <- metadata %>% select( CODIND, tipo_graf, NOTAS1, FUENTE, DEFINICIÓN, `FORMA DE CÁLCULO`)
df <- merge(df, sectmeta, by = "CODIND")
df <- df %>% mutate(tipo_graf2 = as.character(tipo_graf))


df <- df %>%  mutate(metodo = case_when(FECHA <= 2019 & substring(NOTAS1,1,16) == "La transparencia" ~ "ECH presencial",
                                         FECHA >= 2020 & substring(NOTAS1,1,16) == "La transparencia" ~ "ECH telefonica"))


metadata <- metadata %>% mutate(calculo = `FORMA DE CÁLCULO`)
metadata <- metadata %>% mutate(NOTAS2 = case_when(is.na(NOTAS2) == T ~ "Sin observaciones",
                                                   is.na(NOTAS2) == F ~ NOTAS2))

df<-rename(df,
           `Ascendencia étnico racial`=ASCENDENCIA,
           `Perceptor de beneficios`=benef2,
           Departamento=DEPARTAMENTOUY,
           Edad=EDAD,
           `Edad al tener el primer hijo`=EDAD_HIJ,
           `Nivel educativo`=NIVELEDU ,
           `Nivel socioeconómico`=NSE ,
           Pobreza=POBRE,
           `Quintil de ingresos`=QUINTIL,
           Sexo= SEXO,
           Región=URBANORURALUY,
           `Tercil de ingresos`=TERCIL)


df<-rename(df,
           `Tipo de hogar` = THOG,
           `Ciclo de vida` = CICLOVIDA,
           `Modelo de pareja` = PAREJA,
           `Estado conyugal`=EST_CONY,
           `Sexo del jefe/a` = SEXO2,
           `Cantidad de integrantes`=NINT,
           `Salida del hogar de origen`=SALIDA,
           `Edad salida del hogar de origen`=EDADSALIDA,  
           `Con quién salió del hogar de origen`=QSALIDA,
           `Planificación familiar - Tiempo`=PLANIF1,
           `Planificación familiar - Cantidad hijos`=PLANIF2,
           `Planificación familiar - Edad`=PLANIF3,
           `Tipo de violencia`=VIOLENCIA,
           `Beneficios laborales`=benef1,
           `Población que recibe cuidados`=pob_cuid,
           `Cuidador/a`=Cuidador)




df <- df %>% mutate(CORTE1_rec = if_else((NOMINDICADOR == "Distribución porcentual de los hogares según tipo de hogar" |
                       NOMINDICADOR == "Distribución porcentual de hogares con menores según tipo de hogar" |
                       NOMINDICADOR == "Distribución porcentual de los hogares según ciclo de vida del hogar" |
                       NOMINDICADOR == "Distribución porcentual de los hogares biparentales según modelo de pareja" |
                       NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 0 y 4 años de edad" |
                       NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 5 y 12 años de edad" |
                       NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 13 y 17 años de edad" |
                       NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 0 y 17 años de edad" |
                       NOMINDICADOR == "Porcentaje de hogares en los que residen mayores de 64 años" |
                       NOMINDICADOR == "Tamaño medio de los hogares" |
                       NOMINDICADOR == "Distribución porcentual de los hogares según cantidad de integrantes" |
                       NOMINDICADOR == "Distribución porcentual de hogares con menores de 15 años según tipo de hogar" |
                       NOMINDICADOR == "Distribución porcentual de los hogares según sexo del jefe/a") & CORTE1_rec == "Sexo", "Sexo del/la jefe/a", CORTE1_rec))


df <- df %>% mutate("Sexo del/la jefe/a" = if_else((NOMINDICADOR == "Distribución porcentual de los hogares según tipo de hogar" |
                                            NOMINDICADOR == "Distribución porcentual de hogares con menores según tipo de hogar" |
                                            NOMINDICADOR == "Distribución porcentual de los hogares según ciclo de vida del hogar" |
                                            NOMINDICADOR == "Distribución porcentual de los hogares biparentales según modelo de pareja" |
                                            NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 0 y 4 años de edad" |
                                            NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 5 y 12 años de edad" |
                                            NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 13 y 17 años de edad" |
                                            NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 0 y 17 años de edad" |
                                            NOMINDICADOR == "Porcentaje de hogares en los que residen mayores de 64 años" |
                                            NOMINDICADOR == "Tamaño medio de los hogares" |
                                            NOMINDICADOR == "Distribución porcentual de los hogares según cantidad de integrantes" |
                                            NOMINDICADOR == "Distribución porcentual de hogares con menores de 15 años según tipo de hogar" |
                                            NOMINDICADOR == "Distribución porcentual de los hogares según sexo del jefe/a") & CORTE1_rec == "Sexo del/la jefe/a", Sexo, "Todos"))



df <- df %>% mutate(CORTE1_rec = if_else((NOMINDICADOR == "Distribución porcentual de los hogares según tipo de hogar" |
                                            NOMINDICADOR == "Distribución porcentual de hogares con menores según tipo de hogar" |
                                            NOMINDICADOR == "Distribución porcentual de los hogares según ciclo de vida del hogar" |
                                            NOMINDICADOR == "Distribución porcentual de los hogares biparentales según modelo de pareja" |
                                            NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 0 y 4 años de edad" |
                                            NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 5 y 12 años de edad" |
                                            NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 13 y 17 años de edad" |
                                            NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 0 y 17 años de edad" |
                                            NOMINDICADOR == "Porcentaje de hogares en los que residen mayores de 64 años" |
                                            NOMINDICADOR == "Tamaño medio de los hogares" |
                                            NOMINDICADOR == "Distribución porcentual de los hogares según cantidad de integrantes" |
                                            NOMINDICADOR == "Distribución porcentual de hogares con menores de 15 años según tipo de hogar"|
                                            NOMINDICADOR ==   "Distribución porcentual de los hogares según sexo del jefe/a") & CORTE1_rec == "Edad", "Edad del jefe/a", CORTE1_rec))


df <- df %>% mutate("Edad del jefe/a" = if_else((NOMINDICADOR == "Distribución porcentual de los hogares según tipo de hogar" |
                                                   NOMINDICADOR == "Distribución porcentual de hogares con menores según tipo de hogar" |
                                                   NOMINDICADOR == "Distribución porcentual de los hogares según ciclo de vida del hogar" |
                                                   NOMINDICADOR == "Distribución porcentual de los hogares biparentales según modelo de pareja" |
                                                   NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 0 y 4 años de edad" |
                                                   NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 5 y 12 años de edad" |
                                                   NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 13 y 17 años de edad" |
                                                   NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 0 y 17 años de edad" |
                                                   NOMINDICADOR == "Porcentaje de hogares en los que residen mayores de 64 años" |
                                                   NOMINDICADOR == "Tamaño medio de los hogares" |
                                                   NOMINDICADOR == "Distribución porcentual de los hogares según cantidad de integrantes" |
                                                   NOMINDICADOR == "Distribución porcentual de hogares con menores de 15 años según tipo de hogar"|
                                                   NOMINDICADOR ==   "Distribución porcentual de los hogares según sexo del jefe/a") & CORTE1_rec == "Edad del jefe/a", Edad, "Todos"))

df <- df %>% mutate(`Edad del jefe/a` = case_when(`Edad del jefe/a`=="De 0 a 30 años" ~ "De 14 a 30 años",
                                                  TRUE ~ `Edad del jefe/a`))

df <- df %>% mutate(CORTE1_rec = if_else((NOMINDICADOR == "Distribución porcentual de los hogares según tipo de hogar" |
                                            NOMINDICADOR == "Distribución porcentual de hogares con menores según tipo de hogar" |
                                            NOMINDICADOR == "Distribución porcentual de los hogares según ciclo de vida del hogar" |
                                            NOMINDICADOR == "Distribución porcentual de los hogares biparentales según modelo de pareja" |
                                            NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 0 y 4 años de edad" |
                                            NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 5 y 12 años de edad" |
                                            NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 13 y 17 años de edad" |
                                            NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 0 y 17 años de edad" |
                                            NOMINDICADOR == "Porcentaje de hogares en los que residen mayores de 64 años" |
                                            NOMINDICADOR == "Tamaño medio de los hogares" |
                                            NOMINDICADOR == "Distribución porcentual de los hogares según cantidad de integrantes" |
                                            NOMINDICADOR == "Distribución porcentual de hogares con menores de 15 años según tipo de hogar"|
                                            NOMINDICADOR ==   "Distribución porcentual de los hogares según sexo del jefe/a") & CORTE1_rec == "Ascendencia étnico racial", "Ascendencia del jefe/a", CORTE1_rec))


df <- df %>% mutate("Ascendencia del jefe/a" = if_else((NOMINDICADOR == "Distribución porcentual de los hogares según tipo de hogar" |
                                                   NOMINDICADOR == "Distribución porcentual de hogares con menores según tipo de hogar" |
                                                   NOMINDICADOR == "Distribución porcentual de los hogares según ciclo de vida del hogar" |
                                                   NOMINDICADOR == "Distribución porcentual de los hogares biparentales según modelo de pareja" |
                                                   NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 0 y 4 años de edad" |
                                                   NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 5 y 12 años de edad" |
                                                   NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 13 y 17 años de edad" |
                                                   NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 0 y 17 años de edad" |
                                                   NOMINDICADOR == "Porcentaje de hogares en los que residen mayores de 64 años" |
                                                   NOMINDICADOR == "Tamaño medio de los hogares" |
                                                   NOMINDICADOR == "Distribución porcentual de los hogares según cantidad de integrantes" |
                                                   NOMINDICADOR == "Distribución porcentual de hogares con menores de 15 años según tipo de hogar"|
                                                   NOMINDICADOR ==   "Distribución porcentual de los hogares según sexo del jefe/a") & CORTE1_rec == "Ascendencia del jefe/a", `Ascendencia étnico racial`, "Todos"))


df <- df %>% mutate(Departamento = str_to_title(tolower(df$Departamento)))
df <- df %>% mutate(Departamento = case_when(
  Departamento=="Todos"&(NOMINDICADOR=="Tasa de divorcios"|NOMINDICADOR=="Tasa de nupcialidad") ~ "Total país",
  TRUE ~ Departamento))

df <- df %>% mutate(Pobreza = case_when(
  Pobreza=="Hogares en situación de pobreza" ~ "Pobre",
  Pobreza=="Hogares no en situación de pobreza" ~ "No pobre",
  TRUE ~ Pobreza))

df <- df %>%  
  arrange(NOMINDICADOR, FECHA)  

###Cambia nombres de indicador de ENAJ en metadata
metadata <- metadata %>% mutate(
  NOMINDICADOR = case_when(NOMINDICADOR=="Porcentaje de jóvenes y adolescentes, según si se fueron de su hogar de origen" ~  "Distribución porcentual de jóvenes y adolescentes, según si se fueron de su hogar de origen",
                           NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez" ~ "Distribución porcentual de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez",
                           NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron" ~ "Distribución porcentual de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron",
                           TRUE ~ NOMINDICADOR))
            

##Ajuste país urbano:

df <- df %>% mutate(`Tercil de ingresos` = case_when(is.na(`Tercil de ingresos`)==T ~ "Todos",
                                       TRUE ~ `Tercil de ingresos`))

df <- df %>% mutate(CORTE1 = case_when(PESTAÑA=="País urbano"&CORTE1=="URBANORURALUY"&Región=="Urbano (más de 5.000 habitantes)" ~ "Total",
                                       TRUE ~ CORTE1))

df <- df %>% mutate(CORTE1_rec = case_when(PESTAÑA=="País urbano"&CORTE1_rec=="Región"&Región=="Urbano (más de 5.000 habitantes)" ~ "Total",
                                       TRUE ~ CORTE1_rec))

df <- df %>% mutate(ind = case_when(PESTAÑA=="País urbano"&CORTE1_rec=="Región" ~ 1,
                                    TRUE ~ 0))


##Publicamos la pestaña de País Urbano sin región (ver para mejorar el año que viene)   
df <- df %>% filter(ind == 0)
df <- df %>% select(-ind)


##Duplicamos urbano mayor para que quede región en total país completo


dupli <- subset(df, PESTAÑA=="País urbano"&
                    Región=="Urbano (más de 5.000 habitantes)"&
                    CORTE1_rec=="Total"&
                    FUENTE=="Unidad de Métodos y Acceso a Datos (UMAD), Facultad de Ciencias Sociales  (FCS-UdelaR), a partir de la ECH del Instituto Nacional de Estadística (INE) compatibilizada por el Instituto de Economía (IECON) (2019) y ECH - INE, 2020-2022."&
                    FECHA>=2006)

dupli <- dupli %>% mutate(PESTAÑA = case_when(PESTAÑA== "País urbano" ~ "Total país"))
dupli <- dupli %>% mutate(CORTE1 = case_when(CORTE1 == "Total" ~ "URBANORURALUY"))
dupli <- dupli %>% mutate(CORTE1_rec = case_when(CORTE1_rec == "Total" ~  "Región"))

df  <- rbind(df, dupli)



rio::export(df, "Base_Motor_familia_shinny.xlsx")
rio::export(metadata, "Metadata_shinny.xlsx")
