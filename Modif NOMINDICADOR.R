df <- df %>% mutate(NOMINDICADOR = case_when(NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez (Entre 18 y 20)"| 
                                             NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez (Menor a 18)"|
                                             NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez (Entre 21 y 30)" ~ "Distribución porcentual de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez",

                                             NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Amigos)"|
                                             NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Otros no parientes)"|
                                             NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Otros no parientes o institución)"|
                                             NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Otros parientes)"|
                                             NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Pareja)"|
                                             NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Solo)" ~ "Distribución porcentual de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron",

                                             NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que nunca se fueron de su hogar de origen"|
                                             NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen y no volvieron"|
                                             NOMINDICADOR=="Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen y volvieron" ~ "Distribución porcentual de jóvenes y adolescentes, según si se fueron de su hogar de origen",
                                             
                                             NOMINDICADOR=="Porcentaje de personas que en su primer embarazo o el de su pareja querían tener un hijo en ese momento, querían más adelante o no querían (No quería)"|
                                             NOMINDICADOR=="Porcentaje de personas que en su primer embarazo o el de su pareja querían tener un hijo en ese momento, querían más adelante o no querían (Quería más adelante)"|
                                             NOMINDICADOR=="Porcentaje de personas que en su primer embarazo o el de su pareja querían tener un hijo en ese momento, querían más adelante o no querían (Quería)" ~ "Porcentaje de personas que en su primer embarazo o el de su pareja querían tener un hijo en ese momento, querían más adelante o no querían",
                                        
                                             NOMINDICADOR=="Porcentaje de personas según cantidad de hijos tenidos y deseados (IGUAL)"|
                                             NOMINDICADOR=="Porcentaje de personas según cantidad de hijos tenidos y deseados (MAYOR)"|
                                             NOMINDICADOR=="Porcentaje de personas según cantidad de hijos tenidos y deseados (MENOR)" ~ "Porcentaje de personas según cantidad de hijos tenidos y deseados",
                                        
                                             NOMINDICADOR=="Porcentaje de personas según edad a la que tuvieron primer hijo y a la que le hubiese gustado tener  (IGUAL)"|
                                             NOMINDICADOR=="Porcentaje de personas según edad a la que tuvieron primer hijo y a la que le hubiese gustado tener  (MAYOR)"|
                                             NOMINDICADOR=="Porcentaje de personas según edad a la que tuvieron primer hijo y a la que le hubiese gustado tener  (MENOR)" ~ "Porcentaje de personas según edad a la que tuvieron primer hijo y a la que le hubiese gustado tener",
                                        
                                             
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la familia actual" ~                                    
                                          "Prevalencia de violencia basada en género de mujeres de 65 años o más por parte de la familia",
                                             
                                             
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la familia en la infancia (Física)"|
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la familia en la infancia (Psicológica)"|
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la familia en la infancia (Sexual)"|
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la familia en la infancia (Total)" ~ 
                                               "Prevalencia de violencia basada en género por parte de la familia en la infancia",
                                                                          
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la familia actual (Económica)"|
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la familia actual (Sexual)"|
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la familia actual (Psicológica)"|
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la familia actual (Física)"|
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la familia actual (Total)" ~
                                               "Prevalencia de violencia basada en género por parte de la familia actual",
                                             
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja en los últimos 12 meses (Digital)"|
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja en los últimos 12 meses (Económica)"|
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja en los últimos 12 meses (Física)"|
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja en los últimos 12 meses (Psicológica)"|
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja en los últimos 12 meses (Sexual)"|
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja en los últimos 12 meses (Total)" ~ 
                                               "Prevalencia de violencia basada en género por parte de la pareja o expareja en los últimos 12 meses",
                                             
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja a lo largo de toda la vida (Digital)"|
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja a lo largo de toda la vida (Económica)"|
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja a lo largo de toda la vida (Física)"|
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja a lo largo de toda la vida (Psicológica)"|
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja a lo largo de toda la vida (Sexual)"|
                                             NOMINDICADOR=="Prevalencia de violencia basada en género por parte de la pareja o expareja a lo largo de toda la vida (Total)" ~ 
                                               "Prevalencia de violencia basada en género por parte de la pareja o expareja a lo largo de toda la vida",
                                             
                                             NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años sin menores en el hogar (Mujer)"|
                                             NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años sin menores en el hogar (Varón)" ~ 
                                               "Tasa de participación de varones y mujeres entre 14 y 49 años sin menores entre 0 y 12 años en el hogar",
                                             NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 4 en el hogar (Mujer)"|
                                             NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 4 en el hogar (Varón)" ~ 
                                               "Tasa de participación de varones y mujeres entre 14 y 49 años con presencia de menores de 4 años en el hogar",
                                             NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 7 en el hogar (Mujer)"|
                                             NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 7 en el hogar (Varón)" ~ 
                                               "Tasa de participación de varones y mujeres entre 14 y 49 años con presencia de menores de 7 años en el hogar",
                                             NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 13 en el hogar (Mujer)"|
                                             NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 13 en el hogar (Varón)"|
                                             NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 12 en el hogar (Mujer)"| 
                                             NOMINDICADOR=="Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 12 en el hogar (Varón)" ~ 
                                               "Tasa de participación de varones y mujeres entre 14 y 49 años con presencia de menores de 13 años en el hogar",
                                             
                                          
                                          
                                             
                                             NOMINDICADOR=="Distribución porcentual de los hogares según sexo del jefe/a (Mujer)"|
                                             NOMINDICADOR=="Distribución porcentual de los hogares según sexo del jefe/a (Varón)" ~ 
                                               "Distribución porcentual de los hogares según sexo del jefe/a",
                                                                                        
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Actividades especiales para perpiodos de vacaciones). Ambos."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Actividades especiales para perpiodos de vacaciones). Solo madre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Actividades especiales para perpiodos de vacaciones). Solo padre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidado infantil en días específicos o de complemento del horario escolar). Ambos."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidado infantil en días específicos o de complemento del horario escolar). Solo madre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidado infantil en días específicos o de complemento del horario escolar). Solo padre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidados en la empresa). Ambos."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidados en la empresa). Solo madre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidados en la empresa). Solo padre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Contabilización horas semanales o mensuales). Ambos."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Contabilización horas semanales o mensuales). Solo madre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Contabilización horas semanales o mensuales). Solo padre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Convenios con servicios de cuidados). Ambos."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Convenios con servicios de cuidados). Solo madre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Convenios con servicios de cuidados). Solo padre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Flexibilidad horaria en períodos especiales). Ambos."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Flexibilidad horaria en períodos especiales). Solo madre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Flexibilidad horaria en períodos especiales). Solo padre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Licencia especial para cuidado familiar). Ambos."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Licencia especial para cuidado familiar). Solo madre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Licencia especial para cuidado familiar). Solo padre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Otro). Ambos."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Otro). Solo madre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Otro). Solo padre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Permiso acompañar familiares citas médicas o actividades educativas). Ambos."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Permiso acompañar familiares citas médicas o actividades educativas). Solo madre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Permiso acompañar familiares citas médicas o actividades educativas). Solo padre."|  
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Priorización horarios). Ambos."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Priorización horarios). Solo madre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Priorización horarios). Solo padre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Reintegro gradual luego de permiso medio horario). Ambos."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Reintegro gradual luego de permiso medio horario). Solo madre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Reintegro gradual luego de permiso medio horario). Solo padre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Salas de lactancia). Ambos."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Salas de lactancia). Solo madre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Salas de lactancia). Solo padre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Trabajo a distancia). Ambos."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Trabajo a distancia). Solo madre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Trabajo a distancia). Solo padre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Vales o transferencias para servicios de cuidados). Ambos."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Vales o transferencias para servicios de cuidados). Solo madre."|
  NOMINDICADOR=="Porcentaje de personas según beneficios en el trabajo para la crianza (Vales o transferencias para servicios de cuidados). Solo padre." ~
    "Porcentaje de personas según beneficios en el trabajo para la crianza",
  
  NOMINDICADOR=="Participación de personas en el cuidado de niños/as fuera del horario escolar (Abuelo/a)" |
  NOMINDICADOR=="Participación de personas en el cuidado de niños/as fuera del horario escolar (Hermano/a)"|
  NOMINDICADOR=="Participación de personas en el cuidado de niños/as fuera del horario escolar (Madre)"|
  NOMINDICADOR=="Participación de personas en el cuidado de niños/as fuera del horario escolar (Padre)"|
  NOMINDICADOR=="Participación de personas en el cuidado de niños/as fuera del horario escolar (Otro pariente o no pariente)"|
  NOMINDICADOR=="Participación de personas en el cuidado de niños/as fuera del horario escolar (Persona remunerada)" ~ 
    "Participación de personas en el cuidado de niños/as fuera del horario escolar",
  
  NOMINDICADOR=="Participación de personas en el cuidado de niños/as cuando enferman (Abuelo/a)"|
  NOMINDICADOR=="Participación de personas en el cuidado de niños/as cuando enferman (Hermano/a)"|
  NOMINDICADOR=="Participación de personas en el cuidado de niños/as cuando enferman (Madre)"|
  NOMINDICADOR=="Participación de personas en el cuidado de niños/as cuando enferman (Padre)"|
  NOMINDICADOR=="Participación de personas en el cuidado de niños/as cuando enferman (Otro pariente o no pariente)"|
  NOMINDICADOR=="Participación de personas en el cuidado de niños/as cuando enferman (Persona remunerada)" ~ 
    "Participación de personas en el cuidado de niños/as cuando enferman",
  
  NOMINDICADOR=="Tasa de participación en las actividades de cuidados dentro del hogar (mayores de 65 años)"|
  NOMINDICADOR=="Tasa de participación en las actividades de cuidados dentro del hogar (niños/as de 0 a 12 años)"|
  NOMINDICADOR=="Tasa de participación en las actividades de cuidados dentro del hogar (niños/as de 0 a 3 años)"|
  NOMINDICADOR=="Tasa de participación en las actividades de cuidados dentro del hogar (niños/as de 4 a 5 años)"|
  NOMINDICADOR=="Tasa de participación en las actividades de cuidados dentro del hogar (niños/as de 6 a 12 años)"|
  NOMINDICADOR=="Tasa de participación en las actividades de cuidados dentro del hogar (personas con discapacidad)" ~
    "Tasa de participación en las actividades de cuidados dentro del hogar",

  NOMINDICADOR=="Promedio de horas en las actividades de cuidados dentro del hogar (mayores de 65 años)"|
  NOMINDICADOR=="Promedio de horas en las actividades de cuidados dentro del hogar (niños/as de 0 a 12)"|
  NOMINDICADOR=="Promedio de horas en las actividades de cuidados dentro del hogar (niños/as de 0 a 3 años)"|
  NOMINDICADOR=="Promedio de horas en las actividades de cuidados dentro del hogar (niños/as de 4 a 5 años)"|
  NOMINDICADOR=="Promedio de horas en las actividades de cuidados dentro del hogar (niños/as de 6 a 12 años)"|
  NOMINDICADOR=="Promedio de horas en las actividades de cuidados dentro del hogar (personas con discapacidad)" ~ 
    "Promedio de horas en las actividades de cuidados dentro del hogar",
  

  NOMINDICADOR=="Distribución porcentual de los hogares según tipo de hogar (Unipersonal)"|
  NOMINDICADOR=="Distribución porcentual de los hogares según tipo de hogar (Pareja sin hijos)"|
  NOMINDICADOR=="Distribución porcentual de los hogares según tipo de hogar (Biparental)"|
  NOMINDICADOR=="Distribución porcentual de los hogares según tipo de hogar (Monoparental femenino)"|
  NOMINDICADOR=="Distribución porcentual de los hogares según tipo de hogar (Monoparental masculino)"|
  NOMINDICADOR=="Distribución porcentual de los hogares según tipo de hogar (Extendido o compuesto)"|
  NOMINDICADOR=="Distribución porcentual de los hogares según tipo de hogar (Extendido con núcleo monoparental)"|
  NOMINDICADOR=="Distribución porcentual de los hogares según tipo de hogar (Sin núcleo)" ~
    "Distribución porcentual de los hogares según tipo de hogar",
    
  NOMINDICADOR=="Distribución porcentual de hogares con menores según tipo de hogar (Unipersonal)"|
  NOMINDICADOR=="Distribución porcentual de hogares con menores según tipo de hogar (Pareja sin hijos)"|
  NOMINDICADOR=="Distribución porcentual de hogares con menores según tipo de hogar (Biparental)"|
  NOMINDICADOR=="Distribución porcentual de hogares con menores según tipo de hogar (Monoparental femenino)"|
  NOMINDICADOR=="Distribución porcentual de hogares con menores según tipo de hogar (Monoparental masculino)"|
  NOMINDICADOR=="Distribución porcentual de hogares con menores según tipo de hogar (Extendido o compuesto)"|
  NOMINDICADOR=="Distribución porcentual de hogares con menores según tipo de hogar (Extendido con núcleo monoparental)"|
  NOMINDICADOR=="Distribución porcentual de hogares con menores según tipo de hogar (Sin núcleo)" ~
    "Distribución porcentual de hogares con menores según tipo de hogar",
  
  NOMINDICADOR=="Distribución porcentual de las personas según tipo de hogar en el que residen (Unipersonal)"|
  NOMINDICADOR=="Distribución porcentual de las personas según tipo de hogar en el que residen (Pareja sin hijos)"|
  NOMINDICADOR=="Distribución porcentual de las personas según tipo de hogar en el que residen (Biparental)"|
  NOMINDICADOR=="Distribución porcentual de las personas según tipo de hogar en el que residen (Monoparental femenino)"|
  NOMINDICADOR=="Distribución porcentual de las personas según tipo de hogar en el que residen (Monoparental masculino)"| 
  NOMINDICADOR=="Distribución porcentual de las personas según tipo de hogar en el que residen (Extendido o compuesto)"|
  NOMINDICADOR=="Distribución porcentual de las personas según tipo de hogar en el que residen (Extendido con núcleo monoparental)"|
  NOMINDICADOR=="Distribución porcentual de las personas según tipo de hogar en el que residen (Extendido o compuesto con núcleo monoparental)"|
  NOMINDICADOR=="Distribución porcentual de las personas según tipo de hogar en el que residen (Sin núcleo)" ~
    "Distribución porcentual de las personas según tipo de hogar en el que residen",
  
  NOMINDICADOR=="Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Unipersonal)"| 
  NOMINDICADOR=="Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Pareja sin hijos)"| 
  NOMINDICADOR=="Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Biparental)"| 
  NOMINDICADOR=="Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Monoparental femenino)"| 
  NOMINDICADOR=="Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Monoparental masculino)"|
  NOMINDICADOR=="Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Extendido o compuesto)"| 
  NOMINDICADOR=="Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Extendido con núcleo monoparental)"|
  NOMINDICADOR=="Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Sin núcleo)" ~ 
    "Distribución porcentual de hogares con menores de 15 años según tipo de hogar", 
  
  
  NOMINDICADOR=="Distribución porcentual de los hogares biparentales según modelo de pareja (Doble carrera)"|
  NOMINDICADOR=="Distribución porcentual de los hogares biparentales según modelo de pareja (Inversión de roles modificado)"|
  NOMINDICADOR=="Distribución porcentual de los hogares biparentales según modelo de pareja (Inversión de roles)"|
  NOMINDICADOR=="Distribución porcentual de los hogares biparentales según modelo de pareja (Proveedor modificado)"|
  NOMINDICADOR=="Distribución porcentual de los hogares biparentales según modelo de pareja (Proveedor tradicional)"|
  NOMINDICADOR=="Distribución porcentual de los hogares biparentales según modelo de pareja (Residual)" ~ 
    "Distribución porcentual de los hogares biparentales según modelo de pareja",
  
  NOMINDICADOR=="Distribución porcentual de las personas según estado conyugal (Casada/o)"|
  NOMINDICADOR=="Distribución porcentual de las personas según estado conyugal (Divorciada/o o separada/o)"|
  NOMINDICADOR=="Distribución porcentual de las personas según estado conyugal (Soltera/o)"|
  NOMINDICADOR=="Distribución porcentual de las personas según estado conyugal (Unión libre)"|
  NOMINDICADOR=="Distribución porcentual de las personas según estado conyugal (Viuda/o)" ~ 
    "Distribución porcentual de las personas según estado conyugal",
  

  NOMINDICADOR=="Distribución porcentual de los hogares según ciclo de vida del hogar (Consolidación)"|
  NOMINDICADOR=="Distribución porcentual de los hogares según ciclo de vida del hogar (Etapa de salida)"|
  NOMINDICADOR=="Distribución porcentual de los hogares según ciclo de vida del hogar (Etapa inicial)"|
  NOMINDICADOR=="Distribución porcentual de los hogares según ciclo de vida del hogar (Expansión o crecimiento)"|
  NOMINDICADOR=="Distribución porcentual de los hogares según ciclo de vida del hogar (Hoagres no familiares)"|
  NOMINDICADOR=="Distribución porcentual de los hogares según ciclo de vida del hogar (Pareja joven sin hijos)"|
  NOMINDICADOR=="Distribución porcentual de los hogares según ciclo de vida del hogar (Pareja mayor sin hijos)" ~ 
    "Distribución porcentual de los hogares según ciclo de vida del hogar",
  
  NOMINDICADOR=="Distribución porcentual de los hogares según cantidad de integrantes (1 persona)"|
  NOMINDICADOR=="Distribución porcentual de los hogares según cantidad de integrantes (2 personas)"|
  NOMINDICADOR=="Distribución porcentual de los hogares según cantidad de integrantes (3 personas)"|
  NOMINDICADOR=="Distribución porcentual de los hogares según cantidad de integrantes (4 personas)"|
  NOMINDICADOR=="Distribución porcentual de los hogares según cantidad de integrantes (5 o más personas)" ~ 
    "Distribución porcentual de los hogares según cantidad de integrantes",

  NOMINDICADOR=="TASA DE DIVORCIOS" ~ "Tasa de divorcios",
  NOMINDICADOR=="TASA DE NUPCIALIDAD" ~ "Tasa de nupcialidad",
  
  
  
  
  NOMINDICADOR=="Tasa de mortalidad infantil (menor a 1 año)"	~ "Tasa de mortalidad infantil (menores de 1 año)",
  NOMINDICADOR=="Tasa de mortalidad infantil (menor a 5 años)"	 ~ "Tasa de mortalidad infantil (menores de 5 años)",
  
  NOMINDICADOR=="Porcentaje de niños entre 0 y 17 años sin cobertura de salud"|	
  NOMINDICADOR=="Porcentaje de niños menores de 18 años sin cobertura de salud"	~
  "Porcentaje de niños entre 0 y 17 años sin cobertura de salud",
  
  
  TRUE ~ NOMINDICADOR))


