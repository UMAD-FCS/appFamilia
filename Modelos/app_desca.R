
## ***************************************************************************
## Shiny app para Mirador DESCA
## 11/2021
## ***************************************************************************


##  0. DATA Y PAQUETES  ======================================================

library(DT)
library(shinythemes)
library(here)
library(plotly)
library(shinyWidgets)
library(stringr)
library(scales)
library(viridis)
library(tidyverse)
library(bslib)
library(shinycssloaders)
library(sf)

source("utils.R") 

theme_desca <- bs_theme(
  version = 4,
  bg = "#FFFFFF", fg = "#68478d", 
  primary = "#68478d",
  secondary = "#68478d",
  base_font = font_google("Poppins"),
  code_font = font_google("Poppins"),
  heading_font = font_google("Poppins"),
  font_scale = 0.9
)

color_defecto <- "#68478d"
# bs_theme_preview(theme_desca)

# Spinner options 
options(spinner.color = "#68478d",
        spinner.color.background="#ffffff", 
        spinner.size = 2)

# thematic::thematic_shiny() 
# 
theme_set(theme_bdd(base_size = 12))
update_geom_defaults("text", list(family = theme_get()$text$family))

dir.create('~/.fonts')
file.copy("www/Titillium Web.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')


##  1.  PREPARAR DATA  ======================================================

load("Data/data_motor.rda")

# Pasar luego estas transformaciones a data-raw
dat <- tibble::as_tibble(x) %>%
  mutate(Fecha = format(fecha, format = "%Y")) %>%
  rename(Valor = valor) %>%
  relocate(Fecha) %>%
  mutate(departamento = case_when(
    departamento != "Total" ~ toupper(departamento)
  )) %>%
  mutate(tipoind = case_when(
    tipoind == "Políticas públicas y esfuerzo económico" ~ "Políticas Públicas y Esfuerzo Económico",
    TRUE ~ tipoind 
  )) %>% 
  mutate(fecha = ano) %>% 
  mutate(jerarquia_cat_2 = case_when(
    cuenca_o_embalse == "Cuenca Río de la Plata (Cuenca del Río Santa Lucía)" ~ 1,
    cuenca_o_embalse == "Cuenca del Río de la Plata (Cuenca del Río Santa Lucía)" ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(fecha_cat = case_when(
    is.na(fecha_cat) ~ as.character(ano),
    TRUE ~ fecha_cat
  )) %>%
  mutate(jerarquia_cat_2 = case_when(
    nomindicador == "Distribución porcentual de personas según institución prestadora en la cual declaran tener cobertura vigente" ~ 1,
    nomindicador == "Indice del Estado Trófico – Embalses y Lagunas" ~ 1,
    nomindicador == "(Proyecto SURGE - ACNUDH) Tasa de actividad" ~ 1,
    nomindicador == "(Proyecto SURGE - ACNUDH) Tasa de empleo" ~ 1, # Add datos proyecto SURGE - ACNUDH
    nomindicador == "(Proyecto SURGE - ACNUDH) Tasa de subempleo" ~ 1, # Add datos proyecto SURGE - ACNUDH
    nomindicador == "(Proyecto SURGE - ACNUDH) Tasa de desempleo" ~ 1, # Add datos proyecto SURGE - ACNUDH
    nomindicador == "(Proyecto SURGE - ACNUDH) Porcentaje de personas que viven en asentamientos" ~ 1, # Add datos proyecto SURGE - ACNUDH
    nomindicador == "(Proyecto SURGE - ACNUDH) Porcentaje de personas en hogares con tenencia insegura" ~ 1, # Add datos proyecto SURGE - ACNUDH
    nomindicador == "(Proyecto SURGE - ACNUDH) Porcentaje de ocupados sin aporte a la seguridad social" ~ 1, # Add datos proyecto SURGE - ACNUDH
    nomindicador == "Tasa de actividad de varones y mujeres jefe, jefa y cónyuge entre 14 y 49 años de edad según sexo y presencia de menores de 13 años en el hogar" ~ 1, 
    TRUE ~ jerarquia_cat_2
  )) 


# Cargar data departamento (a nivel local)
# dep <- readRDS("Data/depto.rds")
load("Data/depto.RData")
dep <- depto

dat$departamento  <-  chartr("ÁÉÍÓÚ", "AEIOU", dat$departamento)


# Cargar geometría para mapas
# dep <- geouy::load_geouy("Departamentos")

# Lista de indicadores
ind_edu_pp <- dat %>% 
  filter(derecho == "Educación",
         tipoind == "Políticas Públicas y Esfuerzo Económico") %>% 
  arrange() %>% 
  distinct(nomindicador) %>% 
  pull()

ind_edu_r <- dat %>% 
  filter(derecho == "Educación",
         tipoind == "Resultados") %>% 
  arrange() %>% 
  distinct(nomindicador) %>% 
  pull()

ind_salud_pp <- dat %>% 
  filter(derecho == "Salud",
         tipoind == "Políticas Públicas y Esfuerzo Económico") %>% 
  arrange() %>% 
  distinct(nomindicador) %>% 
  pull()

ind_salud_r <- dat %>% 
  filter(derecho == "Salud",
         tipoind == "Resultados") %>% 
  arrange() %>% 
  distinct(nomindicador) %>% 
  pull()

ind_ssocial_pp <- dat %>% 
  filter(derecho == "Seguridad Social",
         tipoind == "Políticas Públicas y Esfuerzo Económico") %>% 
  arrange() %>% 
  distinct(nomindicador) %>% 
  pull()

ind_ssocial_r <- dat %>% 
  filter(derecho == "Seguridad Social",
         tipoind == "Resultados") %>% 
  arrange() %>% 
  distinct(nomindicador) %>% 
  pull()

ind_vivienda_pp <- dat %>% 
  filter(derecho == "Vivienda",
         tipoind == "Políticas Públicas y Esfuerzo Económico") %>% 
  arrange() %>% 
  distinct(nomindicador) %>% 
  pull()

ind_vivienda_r <- dat %>% 
  filter(derecho == "Vivienda",
         tipoind == "Resultados") %>% 
  arrange() %>% 
  distinct(nomindicador) %>% 
  pull()

ind_trabajo_pp <- dat %>% 
  filter(derecho == "Trabajo",
         tipoind == "Políticas Públicas y Esfuerzo Económico") %>% 
  arrange() %>% 
  distinct(nomindicador) %>% 
  pull()

ind_trabajo_r <- dat %>% 
  filter(derecho == "Trabajo",
         tipoind == "Resultados") %>% 
  arrange() %>% 
  distinct(nomindicador) %>% 
  pull()

ind_ambiente_pp <- dat %>% 
  filter(derecho == "Ambiente",
         tipoind == "Políticas Públicas y Esfuerzo Económico") %>% 
  arrange() %>% 
  distinct(nomindicador) %>% 
  pull()

ind_ambiente_r <- dat %>% 
  filter(derecho == "Ambiente",
         tipoind == "Resultados") %>% 
  arrange() %>% 
  distinct(nomindicador) %>% 
  pull()

ind_alimentacion_pp <- dat %>% 
  filter(derecho == "Alimentación",
         tipoind == "Políticas Públicas y Esfuerzo Económico") %>% 
  arrange() %>% 
  distinct(nomindicador) %>% 
  pull()

ind_alimentacion_r <- dat %>% 
  filter(derecho == "Alimentación",
         tipoind == "Resultados") %>% 
  arrange() %>% 
  distinct(nomindicador) %>% 
  pull()

# Lista indicadores fecha_cat
lista_fecha_cat <- dat %>% 
  filter(!is.na(fecha_cat)) %>% 
  distinct(nomindicador) %>% 
  pull()

# Lista indicadores con 2 cortes  
lista_ind_2 <- dat %>% 
  filter(!is.na(corte_2)) %>% 
  distinct(nomindicador) %>% 
  pull()

# Lista indicadores serie_cat
lista_serie_cat <- dat %>% 
  filter(serie_cat == 1) %>% 
  distinct(nomindicador)  %>% 
  pull()

# Lista indicadores con valores únicos

lista_vunico <- dat %>% 
  group_by(nomindicador) %>% 
  distinct(fecha_cat) %>% 
  summarise(n = n()) %>% 
  filter(n == 1) %>% 
  pull(nomindicador)

# Lista indicadores con cambio metodológico ECH
lista_met <- dat %>% 
  filter(metodologia == 1) %>% 
  distinct(nomindicador) %>% 
  pull(nomindicador)

# Lista indicadores unicos de mujeres sin corte doble
lista_mujeres_unico <- dat %>% 
  filter(mujeres == 1) %>% 
  distinct(nomindicador) %>% 
  pull(nomindicador)

# Lista nota
lista_nota <- dat %>% 
  filter(!is.na(nota)) %>% 
  distinct(nomindicador) %>% 
  pull()



# data poblaciones sacando el preambulo del nombre del indicador con la poblacion entre ()
datpob <- dat %>% 
  mutate(nomindicador = str_replace_all(nomindicador, "\\(Migrantes\\)", ""),
         nomindicador = str_replace_all(nomindicador, "\\(Personas LGBTI\\)", ""),
         nomindicador = str_replace_all(nomindicador, "\\(Personas privadas de libertad\\)", ""),
         nomindicador = str_replace_all(nomindicador, "\\(Personas con discapacidad\\)", ""))

# Lista indicadores de población
ind_poblaciones <- datpob %>% 
  filter(!is.na(poblacion)) %>% 
  distinct(nomindicador) %>% 
  pull()

## Listas poblaciones espcificas

# Indicadores ascendencia
ind_asc <- datpob %>% 
  filter(poblacion == "Afrodescendientes") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_asc_edu <- datpob %>% 
  filter(derecho == "Educación") %>% 
  filter(poblacion == "Afrodescendientes") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_asc_salud <- datpob %>% 
  filter(derecho == "Salud") %>% 
  filter(poblacion == "Afrodescendientes") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_asc_vivienda <- datpob %>% 
  filter(derecho == "Vivienda") %>% 
  filter(poblacion == "Afrodescendientes") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_asc_trabajo <- datpob %>% 
  filter(derecho == "Trabajo") %>% 
  filter(poblacion == "Afrodescendientes") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_asc_ambiente <- datpob %>% 
  filter(derecho == "Ambiente") %>% 
  filter(poblacion == "Afrodescendientes") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_asc_ssocial <- datpob %>% 
  filter(derecho == "Seguridad Social") %>% 
  filter(poblacion == "Afrodescendientes") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_asc_surge <- datpob %>% #Add datos Proyecto SURGE ACNUDH
  filter(derecho == "Proyecto SURGE") %>% 
  filter(poblacion == "Afrodescendientes") %>% 
  distinct(nomindicador) %>% 
  pull()


# Indicadores migrantes
ind_migrantes <- datpob %>%
  filter(poblacion == "Migrantes") %>%
  distinct(nomindicador) %>% 
  pull()

ind_migrantes_edu <- datpob %>% 
  filter(derecho == "Educación") %>% 
  filter(poblacion == "Migrantes") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_migrantes_salud <- datpob %>% 
  filter(derecho == "Salud") %>% 
  filter(poblacion == "Migrantes") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_migrantes_vivienda <- datpob %>% 
  filter(derecho == "Vivienda") %>% 
  filter(poblacion == "Migrantes") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_migrantes_trabajo <- datpob %>% 
  filter(derecho == "Trabajo") %>% 
  filter(poblacion == "Migrantes") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_migrantes_ambiente <- datpob %>% 
  filter(derecho == "Ambiente") %>% 
  filter(poblacion == "Migrantes") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_migrantes_ssocial <- datpob %>% 
  filter(derecho == "Seguridad Social") %>% 
  filter(poblacion == "Migrantes") %>% 
  distinct(nomindicador) %>% 
  pull()


# Indicadores NNA
ind_nna <- datpob %>% 
  filter(poblacion == "Niños, niñas y adolescentes") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_nna_edu <- datpob %>% 
  filter(derecho == "Educación") %>% 
  filter(poblacion == "Niños, niñas y adolescentes") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_nna_salud <- datpob %>% 
  filter(derecho == "Salud") %>% 
  filter(poblacion == "Niños, niñas y adolescentes") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_nna_vivienda <- datpob %>% 
  filter(derecho == "Vivienda") %>% 
  filter(poblacion == "Niños, niñas y adolescentes") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_nna_trabajo <- datpob %>% 
  filter(derecho == "Trabajo") %>% 
  filter(poblacion == "Niños, niñas y adolescentes") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_nna_ambiente <- datpob %>% 
  filter(derecho == "Ambiente") %>% 
  filter(poblacion == "Niños, niñas y adolescentes") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_nna_ssocial <- datpob %>% 
  filter(derecho == "Seguridad Social") %>% 
  filter(poblacion == "Niños, niñas y adolescentes") %>% 
  distinct(nomindicador) %>% 
  pull()

# Indicadores Personas con discapacidad
ind_pd <- datpob %>% 
  filter(poblacion == "Personas con discapacidad") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_pd_edu <- datpob %>% 
  filter(derecho == "Educación") %>% 
  filter(poblacion == "Personas con discapacidad") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_pd_salud <- datpob %>% 
  filter(derecho == "Salud") %>% 
  filter(poblacion == "Personas con discapacidad") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_pd_vivienda <- datpob %>% 
  filter(derecho == "Vivienda") %>% 
  filter(poblacion == "Personas con discapacidad") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_pd_trabajo <- datpob %>% 
  filter(derecho == "Trabajo") %>% 
  filter(poblacion == "Personas con discapacidad") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_pd_ambiente <- datpob %>% 
  filter(derecho == "Ambiente") %>% 
  filter(poblacion == "Personas con discapacidad") %>% 
  distinct(nomindicador) %>% 
  pull()

#ind_pd_social <- datpob %>% 
#  filter(derecho == "Seguridad Social") %>% 
# filter(poblacion == "Personas con discapacidad") %>% 
#  distinct(nomindicador) %>% 
#  pull()


# Indicadores Personas  LGBTI
ind_lgtb <- datpob %>% 
  filter(poblacion == "Personas LGBTI") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_lgtb_edu <- datpob %>% 
  filter(derecho == "Educación") %>% 
  filter(poblacion == "Personas LGBTI") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_lgtb_salud <- datpob %>% 
  filter(derecho == "Salud") %>% 
  filter(poblacion == "Personas LGBTI") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_lgtb_vivienda <- datpob %>% 
  filter(derecho == "Vivienda") %>% 
  filter(poblacion == "Personas LGBTI") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_lgtb_trabajo <- datpob %>% 
  filter(derecho == "Trabajo") %>% 
  filter(poblacion == "Personas LGBTI") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_lgtb_ambiente <- datpob %>% 
  filter(derecho == "Ambiente") %>% 
  filter(poblacion == "Personas LGBTI") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_lgtb_ssocial <- datpob %>% 
  filter(derecho == "Seguridad Social") %>% 
  filter(poblacion == "Personas LGBTI") %>% 
  distinct(nomindicador) %>% 
  pull()

# Indicadores Personas  Personas privadas de libertad
ind_ppl <- datpob %>% 
  filter(poblacion == "Personas privadas de libertad") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_ppl_edu <- datpob %>% 
  filter(derecho == "Educación") %>% 
  filter(poblacion == "Personas privadas de libertad") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_ppl_salud <- datpob %>% 
  filter(derecho == "Salud") %>% 
  filter(poblacion == "Personas privadas de libertad") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_ppl_vivienda <- datpob %>% 
  filter(derecho == "Vivienda") %>% 
  filter(poblacion == "Personas privadas de libertad") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_ppl_trabajo <- datpob %>% 
  filter(derecho == "Trabajo") %>% 
  filter(poblacion == "Personas privadas de libertad") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_ppl_ambiente <- datpob %>% 
  filter(derecho == "Ambiente") %>% 
  filter(poblacion == "Personas privadas de libertad") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_ppl_ssocial <- datpob %>% 
  filter(derecho == "Seguridad Social") %>% 
  filter(poblacion == "Personas privadas de libertad") %>% 
  distinct(nomindicador) %>% 
  pull()

# Indicadores Personas  Mujeres
ind_sexo <- datpob %>% 
  filter(poblacion == "Mujeres") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_sexo_edu <- datpob %>% 
  filter(derecho == "Educación") %>% 
  filter(poblacion == "Mujeres") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_sexo_salud <- datpob %>% 
  filter(derecho == "Salud") %>% 
  filter(poblacion == "Mujeres") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_sexo_vivienda <- datpob %>% 
  filter(derecho == "Vivienda") %>% 
  filter(poblacion == "Mujeres") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_sexo_trabajo <- datpob %>% 
  filter(derecho == "Trabajo") %>% 
  filter(poblacion == "Mujeres") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_sexo_ambiente <- datpob %>% 
  filter(derecho == "Ambiente") %>% 
  filter(poblacion == "Mujeres") %>% 
  distinct(nomindicador) %>% 
  pull()

ind_sexo_ssocial <- datpob %>% 
  filter(derecho == "Seguridad Social") %>% 
  filter(poblacion == "Mujeres") %>% 
  distinct(nomindicador) %>% 
  pull()


# Lista indicadores fecha_cat
lista_fecha_cat_pob <- datpob %>% 
  filter(!is.na(fecha_cat)) %>% 
  distinct(nomindicador) %>% 
  pull()

# Lista indicadores con 2 cortes  
lista_ind_2_pob <- datpob %>% 
  filter(!is.na(corte_2)) %>% 
  distinct(nomindicador) %>% 
  pull()

# Lista indicadores serie_cat
lista_serie_cat_pob <- datpob %>% 
  filter(serie_cat == 1) %>% 
  distinct(nomindicador)  %>% 
  pull()

# Lista indicadore con valores únicos

lista_vunico_pob <- datpob %>% 
  group_by(nomindicador) %>% 
  distinct(fecha_cat) %>% 
  summarise(n = n()) %>% 
  filter(n == 1) %>% 
  pull(nomindicador)


# Lista poblaciones
lista_poblaciones <- datpob %>% 
  filter(!is.na(poblacion)) %>% 
  distinct(poblacion) %>% 
  pull()

# Lista especial eliminar más adelante
lista_especial <- intersect(lista_vunico, lista_ind_2)

# Lista especial eliminar más adelante
lista_especial_pob <- intersect(lista_vunico_pob, lista_ind_2_pob)



# Paleta de colores expandida
library(RColorBrewer)
paleta_expandida <- c(brewer.pal(8, "Dark2"), "#B76A16", "#75A61A", "#D9318E",
                      "#986A74", "#C14D6A", "#C1632B", "#698446", "#7B6BB0",
                      "#A9A80F", "#DEAA03", "#922B21", "#273746", "#ABB2B9")


##  2.  USER INTERFACE  =====================================================

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$style("@import url(https://use.fontawesome.com/releases/v5.15.4/css/all.css);"),
  tags$style(".fa-calculator {color:#68478d}"),
  tags$style(".fa-exclamation {color:#68478d}; "),
  tags$style(".fa-exclamation-triangle {color:#68478d}"),
  tags$head(HTML("<title>Mirador DESCA</title>")),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"), # Quita mensajes de error (ojo)
  tags$style(".fa-calculator {color:#68478d}"),
  tags$style(".bttn-minimal.bttn-primary {
    color: #68478d;
  }"),
  tags$style(".irs--shiny .irs-bar {
    top: 25px;
    height: 8px;
    border-top: 1px solid #68478d;
    border-bottom: 1px solid #68478d;
    background: #68478d;}"),
  
  chooseSliderSkin(color = "#68478d"),

  tags$style(".selectize-dropdown-content .option:hover {
      background-color: #68478d !important;
        color: white !important;
    }"),
  
  # /* Cambiar el color de las opciones dentro del selectInput */
    tags$style( ".selectize-dropdown-content .option {
      background-color: white !important;
        color: #68478d !important;
    }"),
  
    tags$style(".irs-bar {",
      "  border-color: #68478d;",
      "  background-color: #68478d;",
      "}",
      ".irs-bar-edge {",
      "  border-color: #68478d;",
      "  background-color: #68478d;",
      "}"),
  
  
  
  
  navbarPage(
    title = tags$a(
      href="https://www.miradordesca.uy/", 
      tags$img(src="logodesca.png", 
               style="margin-top: -2px;", height = 30, width = 100,
               height=30,
               width = 100)
    ),
    
    # div(img(src='logodesca.png', style="margin-top: -2px;", height = 30, width = 100)),
    # titlePanel(title=div(img(src="logo_umad.png", height="5%", width="5%"), "Mirador DESCA")),
    # title = div("Mirador DESCA", img(src="logo_umad.png", height="90%", width = "90%")),
    collapsible = TRUE,
    fluid = TRUE,
    # theme = shinytheme("cerulean"),
    theme = theme_desca,
    # theme = bs_theme(version = 3, bootswatch = "united"),
    
    ## Educación    =====================================================
    
    tabPanel(
      title = "Educación", icon = icon("fas fa-user-graduate"),
      
      tabsetPanel(
        type = "pills",
        id   = "CP",
        
        tabPanel(
          "Políticas Públicas y esfuerzo económico",
          icon = icon("fas fa-chart-bar"),
          
          br(),
          
          fluidRow(
            
            sidebarPanel(
              
              width = 3,
              
              selectInput(
                inputId = "indicador_edu_pp",
                label = "Seleccione indicador:",
                choices = ind_edu_pp
              ),
              
              uiOutput("selector_edu_pp_corte_2"),
              
              uiOutput("chbox_edu_pp_2"),
              
              uiOutput("selector_edu_pp_corte"),
              
              uiOutput("s_edu_pp_fecha"),
              
              uiOutput("chbox_edu_pp"),
              
              HTML("<b> Instituciones:</b>"),
              br(),
              tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                     tags$img(src="INDDHH-Logo.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="75%",
                              width = "65%")),
              br(),
              br(),
              tags$a( href="https://umad.cienciassociales.edu.uy/",
                      tags$img(src="logo_umad.png",
                               style=";vertical-align:top;",
                               height="75%",
                               width = "65%")),
              br(),
              br(),
              HTML("<b> Con el apoyo de:</b>"),    
              br(),
              tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                     tags$img(src="ohchr.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="60%",
                              width = "45%")),
            ),
            
            mainPanel(
              
              tags$h3(style="display:inline-block;margin: 0px;",
                      uiOutput("title_edu_pp")),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("exclamation", lib = "font-awesome"),
                    uiOutput("conindicador_edu_pp"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("calculator", lib = "font-awesome"),
                    uiOutput("calculo_edu_pp"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-exclamation-triangle"),
                    uiOutput("observacion_edu_pp"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-arrows-rotate"),
                    uiOutput("actualizacion_edu_pp"))
              ),
              tags$h5(uiOutput("subtitle_edu_pp")),
              br(),
              withSpinner(plotOutput("plot_edu_pp", height = "500px"),
                          type = 2),
              br(),
              fluidRow(column(12, div(downloadButton(outputId = "baja_p_edu_pp",
                                                     label = "Descarga el gráfico"),
                                      style = "float: right;"))),
              br(),
              br(),
              DTOutput("table_edu_pp"),
              br(),
              br(),
              fluidRow(column(12, div(downloadButton("dwl_tab_edu_pp",
                                                     "Descarga la tabla"),
                                      style = "float: right"))),
              br(),
              br()
              
            )
          )
          
        ),
        
        # * Resultados ----
        
        tabPanel(
          "Resultados",
          icon = icon("fas fa-chart-bar"),
          
          br(),
          
          fluidRow(
            
            sidebarPanel(
              
              width = 3,
              
              selectInput(
                inputId = "indicador_edu_r",
                label = "Seleccione indicador:",
                choices = ind_edu_r
              ),
              
              uiOutput("selector_edu_r_corte_2"),
              
              uiOutput("chbox_edu_r_2"),
              
              uiOutput("selector_edu_r_corte"),
              
              uiOutput("s_edu_r_fecha"),
              
              uiOutput("chbox_edu_r"),
              
              HTML("<b> Instituciones:</b>"),
              br(),
              tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                     tags$img(src="INDDHH-Logo.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="75%",
                              width = "65%")),
              br(),
              br(),
              tags$a( href="https://umad.cienciassociales.edu.uy/",
                      tags$img(src="logo_umad.png",
                               style=";vertical-align:top;",
                               height="75%",
                               width = "65%")),
              br(),
              br(),
              HTML("<b> Con el apoyo de:</b>"),    
              br(),
              tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                     tags$img(src="ohchr.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="60%",
                              width = "45%")),
            ),
            
            mainPanel(
              
              tags$h3(style="display:inline-block",
                      uiOutput("title_edu_r")),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("exclamation", lib = "font-awesome"),
                    uiOutput("conindicador_edu_r"))
              ),
              div(style="display:inline-block", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("calculator", lib = "font-awesome"),
                    uiOutput("calculo_edu_r"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-exclamation-triangle"),
                    uiOutput("observacion_edu_r"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-arrows-rotate"),
                    uiOutput("actualizacion_edu_r"))
              ),
              tags$h5(uiOutput("subtitle_edu_r")),
              tags$h6(uiOutput("nota_edu_r")),
              br(),
              withSpinner(plotOutput("plot_edu_r", height = "500px" ),
                          type = 2),
              br(),
              fluidRow(column(12, div(downloadButton(outputId = "baja_p_edu_r",
                                                     label = "Descarga el gráfico"),
                                      style = "float: right"))),
              br(),
              br(),
              DTOutput("table_edu_r"),
              br(),
              br(),
              fluidRow(column(12, div(downloadButton("dwl_tab_edu_r", 
                                                     "Descarga la tabla"),
                                      style = "float: right"))),
              br(),
              br()
            )
            
          )
        )
      )
    ),
    
    ## Salud    =========================================================
    
    tabPanel(
      title = "Salud", icon = icon("fas fa-plus-square"),
      
      tabsetPanel(
        type = "pills",
        id   = "CP",
        
        tabPanel(
          "Políticas Públicas y esfuerzo económico", 
          icon = icon("fas fa-chart-bar"),
          br(),
          
          fluidRow(
            
            sidebarPanel(
              
              width = 3,
              
              selectInput(
                inputId = "indicador_salud_pp",
                label = "Seleccione indicador:",
                choices = ind_salud_pp
              ),
              
              uiOutput("selector_salud_pp_corte_2"),
              
              uiOutput("chbox_salud_pp_2"),
              
              uiOutput("selector_salud_pp_corte"),
              
              uiOutput("s_salud_pp_fecha"),
              
              uiOutput("chbox_salud_pp"),
              
              HTML("<b> Instituciones:</b>"),
              br(),
              tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                     tags$img(src="INDDHH-Logo.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="75%",
                              width = "65%")),
              br(),
              br(),
              tags$a( href="https://umad.cienciassociales.edu.uy/",
                      tags$img(src="logo_umad.png",
                               style=";vertical-align:top;",
                               height="75%",
                               width = "65%")),
              br(),
              br(),
              HTML("<b> Con el apoyo de:</b>"),    
              br(),
              tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                     tags$img(src="ohchr.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="60%",
                              width = "45%")),
            ),
            
            mainPanel(
              
              tags$h3(style="display:inline-block",
                      uiOutput("title_salud_pp")),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("exclamation", lib = "font-awesome"),
                    uiOutput("conindicador_salud_pp"))
              ),
              div(style="display:inline-block", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("calculator", lib = "font-awesome"),
                    uiOutput("calculo_salud_pp"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-exclamation-triangle"),
                    uiOutput("observacion_salud_pp"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-arrows-rotate"),
                    uiOutput("actualizacion_salud_pp"))
              ),
              tags$h5(uiOutput("subtitle_salud_pp")),
              br(),
              withSpinner(plotOutput("plot_salud_pp", height = "500px" ),
                          type = 2),
              br(),
              fluidRow(column(12, div(downloadButton(outputId = "baja_p_salud_pp",
                                                     label = "Descarga el gráfico"),
                                      style = "float: right"))),
              br(),
              br(),
              DTOutput("table_salud_pp"),
              br(),
              br(),
              fluidRow(column(12, div(downloadButton("dwl_tab_salud_pp", "Descarga la tabla"),
                                      style = "float: right"))),
              br(),
              br()
            )
          )
          
        ),
        
        # * Resultados ----
        
        tabPanel(
          "Resultados", 
          icon = icon("fas fa-chart-bar"),
          
          br(),
          
          fluidRow(
            
            sidebarPanel(
              
              width = 3,
              
              selectInput(
                inputId = "indicador_salud_r",
                label = "Seleccione indicador:",
                choices = ind_salud_r,
                selected = "Esperanza de vida al nacer"
              ),
              
              uiOutput("selector_salud_r_corte_2"),
              
              uiOutput("chbox_salud_r_2"),
              
              uiOutput("selector_salud_r_corte"),
              
              uiOutput("s_salud_r_fecha"),
              
              uiOutput("chbox_salud_r"),
              
              HTML("<b> Instituciones:</b>"),
              br(),
              tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                     tags$img(src="INDDHH-Logo.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="75%",
                              width = "65%")),
              br(),
              br(),
              tags$a( href="https://umad.cienciassociales.edu.uy/",
                      tags$img(src="logo_umad.png",
                               style=";vertical-align:top;",
                               height="75%",
                               width = "65%")),
              br(),
              br(),
              HTML("<b> Con el apoyo de:</b>"),    
              br(),
              tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                     tags$img(src="ohchr.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="60%",
                              width = "45%")),
            ),
            
            mainPanel(
              
              tags$h3(style="display:inline-block",
                      uiOutput("title_salud_r")),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("exclamation", lib = "font-awesome"),
                    uiOutput("conindicador_salud_r"))
              ),
              div(style="display:inline-block", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("calculator", lib = "font-awesome"),
                    uiOutput("calculo_salud_r"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-exclamation-triangle"),
                    uiOutput("observacion_salud_r"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-arrows-rotate"),
                    uiOutput("actualizacion_salud_r"))
              ),
              tags$h5(uiOutput("subtitle_salud_r")),
              tags$h6(uiOutput("nota_salud_r")),
              br(),
              withSpinner(plotOutput("plot_salud_r", height = "500px" ),
                          type = 2),
              br(),
              fluidRow(column(12, div(downloadButton(outputId = "baja_p_salud_r", 
                                                     label = "Descarga el gráfico"),
                                      style = "float: right"))),
              br(),
              br(),
              DTOutput("table_salud_r"),
              br(),
              br(),
              fluidRow(column(12, div(downloadButton("dwl_tab_salud_r", "Descarga la tabla"),
                                      style = "float: right"))),
              br(),
              br()
            )
          )
        )
      )
    ),
    
    ## Seguridad Social    ==============================================
    
    tabPanel(
      title = "Seguridad Social", icon = icon("briefcase"),
      
      tabsetPanel(
        type = "pills",
        id   = "CP",
        
        tabPanel(
          "Políticas Públicas y esfuerzo económico", 
          icon = icon("fas fa-chart-bar"),
          
          br(),
          
          fluidRow(
            
            
            sidebarPanel(
              
              width = 3,
              
              selectInput(
                inputId = "indicador_ssocial_pp",
                label = "Seleccione indicador:",
                choices = ind_ssocial_pp
              ),
              
              uiOutput("selector_ssocial_pp_corte_2"),
              
              uiOutput("chbox_ssocial_pp_2"),
              
              uiOutput("selector_ssocial_pp_corte"),
              
              uiOutput("s_ssocial_pp_fecha"),
              
              uiOutput("chbox_ssocial_pp"),
              
              HTML("<b> Instituciones:</b>"),
              br(),
              tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                     tags$img(src="INDDHH-Logo.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="75%",
                              width = "65%")),
              br(),
              br(),
              tags$a( href="https://umad.cienciassociales.edu.uy/",
                      tags$img(src="logo_umad.png",
                               style=";vertical-align:top;",
                               height="75%",
                               width = "65%")),
              br(),
              br(),
              HTML("<b> Con el apoyo de:</b>"),    
              br(),
              tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                     tags$img(src="ohchr.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="60%",
                              width = "45%")),
            ),
            
            mainPanel(
              
              tags$h3(style="display:inline-block",
                      uiOutput("title_ssocial_pp")),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("exclamation", lib = "font-awesome"),
                    uiOutput("conindicador_ssocial_pp"))
              ),
              div(style="display:inline-block", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("calculator", lib = "font-awesome"),
                    uiOutput("calculo_ssocial_pp"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-exclamation-triangle"),
                    uiOutput("observacion_ssocial_pp"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-arrows-rotate"),
                    uiOutput("actualizacion_ssocial_pp"))
              ),
              tags$h5(uiOutput("subtitle_ssocial_pp")),
              br(),
              withSpinner(plotOutput("plot_ssocial_pp", height = "500px" ),
                          type = 2),
              br(),
              fluidRow(column(12, div(downloadButton(outputId = "baja_p_ssocial_pp", 
                                                     label = "Descarga el gráfico"),
                                      style = "float: right"))),
              br(),
              br(),
              DTOutput("table_ssocial_pp"),
              br(),
              br(),
              fluidRow(column(12, div(downloadButton("dwl_tab_ssocial_pp", "Descarga la tabla"),
                                      style = "float: right"))),
              br(),
              br()
            )
          )
        ),
        
        # * Resultados ----
        
        tabPanel(
          "Resultados", 
          icon = icon("fas fa-chart-bar"),
          
          br(),
          
          fluidRow(
            
            sidebarPanel(
              
              width = 3,
              
              selectInput(
                inputId = "indicador_ssocial_r",
                label = "Seleccione indicador:",
                choices = ind_ssocial_r
              ),
              
              uiOutput("selector_ssocial_r_corte_2"),
              
              uiOutput("chbox_ssocial_r_2"),
              
              uiOutput("selector_ssocial_r_corte"),
              
              uiOutput("s_ssocial_r_fecha"),
              
              uiOutput("chbox_ssocial_r"),
              
              HTML("<b> Instituciones:</b>"),
              br(),
              tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                     tags$img(src="INDDHH-Logo.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="75%",
                              width = "65%")),
              br(),
              br(),
              tags$a( href="https://umad.cienciassociales.edu.uy/",
                      tags$img(src="logo_umad.png",
                               style=";vertical-align:top;",
                               height="75%",
                               width = "65%")),
              br(),
              br(),
              HTML("<b> Con el apoyo de:</b>"),    
              br(),
              tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                     tags$img(src="ohchr.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="60%",
                              width = "45%")),
            ),
            
            mainPanel(
              
              tags$h3(style="display:inline-block",
                      uiOutput("title_ssocial_r")),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("exclamation", lib = "font-awesome"),
                    uiOutput("conindicador_ssocial_r"))
              ),
              div(style="display:inline-block", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("calculator", lib = "font-awesome"),
                    uiOutput("calculo_ssocial_r"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-exclamation-triangle"),
                    uiOutput("observacion_ssocial_r"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-arrows-rotate"),
                    uiOutput("actualizacion_ssocial_r"))
              ),
              tags$h5(uiOutput("subtitle_ssocial_r")),
              tags$h6(uiOutput("nota_ssocial_r")),
              br(),
              withSpinner(plotOutput("plot_ssocial_r", height = "500px"),
                          type = 2),
              br(),
              fluidRow(column(12, div(downloadButton(outputId = "baja_p_ssocial_r", 
                                                     label = "Descarga el gráfico"),
                                      style = "float: right"))),
              br(),
              br(),
              DTOutput("table_ssocial_r"),
              br(),
              br(),
              fluidRow(column(12, div(downloadButton("dwl_tab_ssocial_r", "Descarga la tabla"),
                                      style = "float: right"))),
              br(),
              br()
            )
          )
        )
      )
    ),
    
    ## Vivienda    =====================================================
    
    tabPanel(
      title = "Vivienda", icon = icon("home"),
      
      tabsetPanel(
        type = "pills",
        id   = "CP",
        
        tabPanel(
          "Políticas Públicas y esfuerzo económico", 
          icon = icon("fas fa-chart-bar"),
          br(),
          
          fluidRow(
            
            sidebarPanel(
              
              width = 3,
              
              selectInput(
                inputId = "indicador_vivienda_pp",
                label = "Seleccione indicador:",
                choices = ind_vivienda_pp
              ),
              
              uiOutput("selector_vivienda_pp_corte_2"),
              
              uiOutput("chbox_vivienda_pp_2"),
              
              uiOutput("selector_vivienda_pp_corte"),
              
              uiOutput("s_vivienda_pp_fecha"),
              
              uiOutput("chbox_vivienda_pp"),
              
              HTML("<b> Instituciones:</b>"),
              br(),
              tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                     tags$img(src="INDDHH-Logo.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="75%",
                              width = "65%")),
              br(),
              br(),
              tags$a( href="https://umad.cienciassociales.edu.uy/",
                      tags$img(src="logo_umad.png",
                               style=";vertical-align:top;",
                               height="75%",
                               width = "65%")),
              br(),
              br(),
              HTML("<b> Con el apoyo de:</b>"),    
              br(),
              tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                     tags$img(src="ohchr.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="60%",
                              width = "45%")),
            ),
            
            mainPanel(
              
              tags$h3(style="display:inline-block",
                      uiOutput("title_vivienda_pp")),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("exclamation", lib = "font-awesome"),
                    uiOutput("conindicador_vivienda_pp"))
              ),
              div(style="display:inline-block", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("calculator", lib = "font-awesome"),
                    uiOutput("calculo_vivienda_pp"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-exclamation-triangle"),
                    uiOutput("observacion_vivienda_pp"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-arrows-rotate"),
                    uiOutput("actualizacion_vivienda_pp"))
              ),
              tags$h5(uiOutput("subtitle_vivienda_pp")),
              br(),
              withSpinner(plotOutput("plot_vivienda_pp", height = "500px"),
                          type = 2),
              br(),
              fluidRow(column(12, div(downloadButton(outputId = "baja_p_vivienda_pp", 
                                                     label = "Descarga el gráfico"),
                                      style = "float: right"))),
              br(),
              br(),
              DTOutput("table_vivienda_pp"),
              br(),
              br(),
              fluidRow(column(12, div(downloadButton("dwl_tab_vivienda_pp", "Descarga la tabla"),
                                      style = "float: right"))),
              br(),
              br()
            )
          )
        ),
        
        # * Resultados ----
        
        
        tabPanel(
          "Resultados", 
          icon = icon("fas fa-chart-bar"),
          
          br(),
          
          fluidRow(
            
            sidebarPanel(
              
              width = 3,
              
              selectInput(
                inputId = "indicador_vivienda_r",
                label = "Seleccione indicador:",
                choices = ind_vivienda_r
              ),
              
              uiOutput("selector_vivienda_r_corte_2"),
              
              uiOutput("chbox_vivienda_r_2"),
              
              uiOutput("selector_vivienda_r_corte"),
              
              uiOutput("s_vivienda_r_fecha"),
              
              uiOutput("chbox_vivienda_r"),
              
              HTML("<b> Instituciones:</b>"),
              br(),
              tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                     tags$img(src="INDDHH-Logo.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="75%",
                              width = "65%")),
              br(),
              br(),
              tags$a( href="https://umad.cienciassociales.edu.uy/",
                      tags$img(src="logo_umad.png",
                               style=";vertical-align:top;",
                               height="75%",
                               width = "65%")),
              br(),
              br(),
              HTML("<b> Con el apoyo de:</b>"),    
              br(),
              tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                     tags$img(src="ohchr.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="60%",
                              width = "45%")),
            ),
            
            mainPanel(
              
              tags$h3(style="display:inline-block",
                      uiOutput("title_vivienda_r")),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("exclamation", lib = "font-awesome"),
                    uiOutput("conindicador_vivienda_r"))
              ),
              div(style="display:inline-block", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("calculator", lib = "font-awesome"),
                    uiOutput("calculo_vivienda_r"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-exclamation-triangle"),
                    uiOutput("observacion_vivienda_r"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-arrows-rotate"),
                    uiOutput("actualizacion_vivienda_r"))
              ),
              tags$h5(uiOutput("subtitle_vivienda_r")),
              tags$h6(uiOutput("nota_vivienda_r")),
              br(),
              withSpinner(plotOutput("plot_vivienda_r", height = "500px"),
                          type = 2),
              br(),
              fluidRow(column(12, div(downloadButton(outputId = "baja_p_vivienda_r", 
                                                     label = "Descarga el gráfico"),
                                      style = "float: right"))),
              br(),
              br(),
              DTOutput("table_vivienda_r"),
              br(),
              br(),
              fluidRow(column(12, div(downloadButton("dwl_tab_vivienda_r", "Descarga la tabla"),
                                      style = "float: right"))),
              br(),
              br()
            )
          )
        )
      )
    ),
    
    ## Trabajo    =====================================================
    
    tabPanel(
      title = "Trabajo", icon = icon("fas fa-users-cog"),
      
      tabsetPanel(
        type = "pills",
        id   = "CP",
        
        tabPanel(
          "Políticas Públicas y esfuerzo económico", 
          icon = icon("fas fa-chart-bar"),
          br(),
          
          fluidRow(
            
            sidebarPanel(
              
              width = 3,
              
              selectInput(
                inputId = "indicador_trabajo_pp",
                label = "Seleccione indicador:",
                choices = ind_trabajo_pp
              ),
              
              uiOutput("selector_trabajo_pp_corte_2"),
              
              uiOutput("chbox_trabajo_pp_2"),
              
              uiOutput("selector_trabajo_pp_corte"),
              
              uiOutput("s_trabajo_pp_fecha"),
              
              uiOutput("chbox_trabajo_pp"),
              
              HTML("<b> Instituciones:</b>"),
              br(),
              tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                     tags$img(src="INDDHH-Logo.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="75%",
                              width = "65%")),
              br(),
              br(),
              tags$a( href="https://umad.cienciassociales.edu.uy/",
                      tags$img(src="logo_umad.png",
                               style=";vertical-align:top;",
                               height="75%",
                               width = "65%")),
              br(),
              br(),
              HTML("<b> Con el apoyo de:</b>"),    
              br(),
              tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                     tags$img(src="ohchr.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="60%",
                              width = "45%")),
            ),
            
            mainPanel(
              
              tags$h3(style="display:inline-block",
                      uiOutput("title_trabajo_pp")),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("exclamation", lib = "font-awesome"),
                    uiOutput("conindicador_trabajo_pp"))
              ),
              div(style="display:inline-block", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("calculator", lib = "font-awesome"),
                    uiOutput("calculo_trabajo_pp"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-exclamation-triangle"),
                    uiOutput("observacion_trabajo_pp"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-arrows-rotate"),
                    uiOutput("actualizacion_trabajo_pp"))
              ),
              tags$h5(uiOutput("subtitle_trabajo_pp")),
              br(),
              withSpinner(plotOutput("plot_trabajo_pp", height = "500px"),
                          type = 2),
              br(),
              fluidRow(column(12, div(downloadButton(outputId = "baja_p_trabajo_pp", 
                                                     label = "Descarga el gráfico"),
                                      style = "float: right"))),
              br(),
              br(),
              DTOutput("table_trabajo_pp"),
              br(),
              br(),
              fluidRow(column(12, div(downloadButton("dwl_tab_trabajo_pp", "Descarga la tabla"),
                                      style = "float: right"))),
              br(),
              br()
            )
          )
        ),
        
        # * Resultados ----
        
        
        tabPanel(
          "Resultados", 
          icon = icon("fas fa-chart-bar"),
          
          br(),
          
          fluidRow(
            
            sidebarPanel(
              
              width = 3,
              
              selectInput(
                inputId = "indicador_trabajo_r",
                label = "Seleccione indicador:",
                choices = ind_trabajo_r
              ),
              
              uiOutput("selector_trabajo_r_corte_2"),
              
              uiOutput("chbox_trabajo_r_2"),
              
              uiOutput("selector_trabajo_r_corte"),
              
              uiOutput("s_trabajo_r_fecha"),
              
              uiOutput("chbox_trabajo_r"),
              
              HTML("<b> Instituciones:</b>"),
              br(),
              tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                     tags$img(src="INDDHH-Logo.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="75%",
                              width = "65%")),
              br(),
              br(),
              tags$a( href="https://umad.cienciassociales.edu.uy/",
                      tags$img(src="logo_umad.png",
                               style=";vertical-align:top;",
                               height="75%",
                               width = "65%")),
              br(),
              br(),
              HTML("<b> Con el apoyo de:</b>"),    
              br(),
              tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                     tags$img(src="ohchr.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="60%",
                              width = "45%")),
            ),
            
            mainPanel(
              
              tags$h3(style="display:inline-block",
                      uiOutput("title_trabajo_r")),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("exclamation", lib = "font-awesome"),
                    uiOutput("conindicador_trabajo_r"))
              ),
              div(style="display:inline-block", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("calculator", lib = "font-awesome"),
                    uiOutput("calculo_trabajo_r"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-exclamation-triangle"),
                    uiOutput("observacion_trabajo_r"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-arrows-rotate"),
                    uiOutput("actualizacion_trabajo_r"))
              ),
              tags$h5(uiOutput("subtitle_trabajo_r")),
              tags$h6(uiOutput("nota_trabajo_r")),
              br(),
              withSpinner(plotOutput("plot_trabajo_r", height = "500px"),
                          type = 2),
              br(),
              fluidRow(column(12, div(downloadButton(outputId = "baja_p_trabajo_r", 
                                                     label = "Descarga el gráfico"),
                                      style = "float: right"))),
              br(),
              br(),
              DTOutput("table_trabajo_r"),
              br(),
              br(),
              fluidRow(column(12, div(downloadButton("dwl_tab_trabajo_r", "Descarga la tabla"),
                                      style = "float: right"))),
              br(),
              br()
            )
          )
        )
      )
    ),
    
    ## Ambiente    =====================================================
    
    tabPanel(
      title = "Ambiente", icon = icon("fas fa-leaf"),
      
      tabsetPanel(
        type = "pills",
        id   = "CP",
        
        tabPanel(
          "Políticas Públicas y esfuerzo económico", 
          icon = icon("fas fa-chart-bar"),
          br(),
          
          fluidRow(
            
            sidebarPanel(
              
              width = 3,
              
              selectInput(
                inputId = "indicador_ambiente_pp",
                label = "Seleccione indicador:",
                choices = ind_ambiente_pp
              ),
              
              uiOutput("selector_ambiente_pp_corte_2"),
              
              uiOutput("chbox_ambiente_pp_2"),
              
              uiOutput("selector_ambiente_pp_corte"),
              
              uiOutput("s_ambiente_pp_fecha"),
              
              uiOutput("chbox_ambiente_pp"),
              
              HTML("<b> Instituciones:</b>"),
              br(),
              tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                     tags$img(src="INDDHH-Logo.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="75%",
                              width = "65%")),
              br(),
              br(),
              tags$a( href="https://umad.cienciassociales.edu.uy/",
                      tags$img(src="logo_umad.png",
                               style=";vertical-align:top;",
                               height="75%",
                               width = "65%")),
              br(),
              br(),
              HTML("<b> Con el apoyo de:</b>"),    
              br(),
              tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                     tags$img(src="ohchr.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="60%",
                              width = "45%")),
            ),
            
            mainPanel(
              
              tags$h3(style="display:inline-block",
                      uiOutput("title_ambiente_pp")),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("exclamation", lib = "font-awesome"),
                    uiOutput("conindicador_ambiente_pp"))
              ),
              div(style="display:inline-block", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("calculator", lib = "font-awesome"),
                    uiOutput("calculo_ambiente_pp"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-exclamation-triangle"),
                    uiOutput("observacion_ambiente_pp"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-arrows-rotate"),
                    uiOutput("actualizacion_ambiente_pp"))
              ),
              tags$h5(uiOutput("subtitle_ambiente_pp")),
              br(),
              withSpinner(plotOutput("plot_ambiente_pp", height = "500px"),
                          type = 2),
              br(),
              fluidRow(column(12, div(downloadButton(outputId = "baja_p_ambiente_pp", 
                                                     label = "Descarga el gráfico"),
                                      style = "float: right"))),
              br(),
              br(),
              DTOutput("table_ambiente_pp"),
              br(),
              br(),
              fluidRow(column(12, div(downloadButton("dwl_tab_ambiente_pp", "Descarga la tabla"),
                                      style = "float: right"))),
              br(),
              br()
            )
          )
        ),
        
        # * Resultados ----
        
        
        tabPanel(
          "Resultados", 
          icon = icon("fas fa-chart-bar"),
          
          br(),
          
          fluidRow(
            
            sidebarPanel(
              
              width = 3,
              
              selectInput(
                inputId = "indicador_ambiente_r",
                label = "Seleccione indicador:",
                choices = ind_ambiente_r
              ),
              
              uiOutput("selector_ambiente_r_corte_2"),
              
              uiOutput("chbox_ambiente_r_2"),
              
              uiOutput("selector_ambiente_r_corte"),
              
              uiOutput("chbox_ambiente_r"),
              
              uiOutput("s_ambiente_r_fecha"),
              
              HTML("<b> Instituciones:</b>"),
              br(),
              tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                     tags$img(src="INDDHH-Logo.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="75%",
                              width = "65%")),
              br(),
              br(),
              tags$a( href="https://umad.cienciassociales.edu.uy/",
                      tags$img(src="logo_umad.png",
                               style=";vertical-align:top;",
                               height="75%",
                               width = "65%")),
              br(),
              br(),
              HTML("<b> Con el apoyo de:</b>"),    
              br(),
              tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                     tags$img(src="ohchr.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link", 
                              height="60%",
                              width = "45%")),
            ),
            
            mainPanel(
              
              tags$h3(style="display:inline-block",
                      uiOutput("title_ambiente_r")),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("exclamation", lib = "font-awesome"),
                    uiOutput("conindicador_ambiente_r"))
              ),
              div(style="display:inline-block", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("calculator", lib = "font-awesome"),
                    uiOutput("calculo_ambiente_r"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-exclamation-triangle"),
                    uiOutput("observacion_ambiente_r"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-arrows-rotate"),
                    uiOutput("actualizacion_ambiente_r"))
              ),
              tags$h5(uiOutput("subtitle_ambiente_r")),
              tags$h6(uiOutput("nota_ambiente_r")),
              br(),
              withSpinner(plotOutput("plot_ambiente_r", height = "500px"),
                          type = 2),
              br(),
              fluidRow(column(12, div(downloadButton(outputId = "baja_p_ambiente_r", 
                                                     label = "Descarga el gráfico"),
                                      style = "float: right"))),
              br(),
              br(),
              DTOutput("table_ambiente_r"),
              br(),
              br(),
              fluidRow(column(12, div(downloadButton("dwl_tab_ambiente_r", "Descarga la tabla"),
                                      style = "float: right"))),
              br(),
              br()
            )
          )
        )
      )
    ),

    ## Alimentación    =====================================================

    tabPanel(
      title = "Alimentación", icon = icon("fas fa-bowl-food"),

      tabsetPanel(
        type = "pills",
        id   = "CP",
    # 
    #     tabPanel(
    #       "Políticas Públicas y esfuerzo económico",
    #       icon = icon("fas fa-chart-bar"),
    #       br(),
    # 
    #       fluidRow(
    # 
    #         sidebarPanel(
    # 
    #           width = 3,
    # 
    #           selectInput(
    #             inputId = "indicador_alimentacion_pp",
    #             label = "Seleccione indicador:",
    #             choices = ind_alimentacion_pp
    #           ),
    # 
    #           uiOutput("selector_alimentacion_pp_corte_2"),
    # 
    #           uiOutput("chbox_alimentacion_pp_2"),
    # 
    #           uiOutput("selector_alimentacion_pp_corte"),
    # 
    #           uiOutput("s_alimentacion_pp_fecha"),
    # 
    #           uiOutput("chbox_alimentacion_pp"),
    # 
    #           HTML("<b> Instituciones:</b>"),
    #           br(),
    #           tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
    #                  tags$img(src="INDDHH-Logo.png",
    #                           style=";vertical-align:top;",
    #                           # title="Example Image Link",
    #                           height="75%",
    #                           width = "65%")),
    #           br(),
    #           br(),
    #           tags$a( href="https://umad.cienciassociales.edu.uy/",
    #                   tags$img(src="logo_umad.png",
    #                            style=";vertical-align:top;",
    #                            height="75%",
    #                            width = "65%")),
    #           br(),
    #           br(),
    #           HTML("<b> Con el apoyo de:</b>"),
    #           br(),
    #           tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
    #                  tags$img(src="ohchr.png",
    #                           style=";vertical-align:top;",
    #                           # title="Example Image Link",
    #                           height="60%",
    #                           width = "45%")),
    #         ),
    # 
    #         mainPanel(
    # 
    #           tags$h3(style="display:inline-block",
    #                   uiOutput("title_alimentacion_pp")),
    #           div(style="display:inline-block;margin: 0px;",
    #               dropdown(
    #                 style = "minimal",
    #                 status = "primary",
    #                 width = "500px",
    #                 right = TRUE,
    #                 icon = icon("exclamation", lib = "font-awesome"),
    #                 uiOutput("conindicador_alimentacion_pp"))
    #           ),
    #           div(style="display:inline-block",
    #               dropdown(
    #                 style = "minimal",
    #                 status = "primary",
    #                 width = "500px",
    #                 right = TRUE,
    #                 icon = icon("calculator", lib = "font-awesome"),
    #                 uiOutput("calculo_alimentacion_pp"))
    #           ),
    #           div(style="display:inline-block;margin: 0px;",
    #               dropdown(
    #                 style = "minimal",
    #                 status = "primary",
    #                 width = "500px",
    #                 right = TRUE,
    #                 icon = icon("fas fa-exclamation-triangle"),
    #                 uiOutput("observacion_alimentacion_pp"))
    #           ),
    #           div(style="display:inline-block;margin: 0px;", 
    #             dropdown(
    #                 style = "minimal",
    #                 status = "primary",
    #                 width = "500px",
    #                 right = TRUE,
    #                 icon = icon("fas fa-arrows-rotate"),
    #                 uiOutput("actualizacion_ambiente_pp"))
    #           ),
    
    #           tags$h5(uiOutput("subtitle_alimentacion_pp")),
    #           br(),
    #           withSpinner(plotOutput("plot_alimentacion_pp", height = "500px"),
    #                       type = 2),
    #           br(),
    #           fluidRow(column(12, div(downloadButton(outputId = "baja_p_alimentacion_pp",
    #                                                  label = "Descarga el gráfico"),
    #                                   style = "float: right"))),
    #           br(),
    #           br(),
    #           DTOutput("table_alimentacion_pp"),
    #           br(),
    #           br(),
    #           fluidRow(column(12, div(downloadButton("dwl_tab_alimentacion_pp", "Descarga la tabla"),
    #                                   style = "float: right"))),
    #           br(),
    #           br()
    #         )
    #       )
    #     ),
    # 
        # * Resultados ----


        tabPanel(
          "Resultados",
          icon = icon("fas fa-chart-bar"),

          br(),

          fluidRow(

            sidebarPanel(

              width = 3,

              selectInput(
                inputId = "indicador_alimentacion_r",
                label = "Seleccione indicador:",
                choices = ind_alimentacion_r
              ),

              uiOutput("selector_alimentacion_r_corte_2"),

              uiOutput("chbox_alimentacion_r_2"),

              uiOutput("selector_alimentacion_r_corte"),

              uiOutput("chbox_alimentacion_r"),

              uiOutput("s_alimentacion_r_fecha"),

              HTML("<b> Instituciones:</b>"),
              br(),
              tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                     tags$img(src="INDDHH-Logo.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link",
                              height="75%",
                              width = "65%")),
              br(),
              br(),
              tags$a( href="https://umad.cienciassociales.edu.uy/",
                      tags$img(src="logo_umad.png",
                               style=";vertical-align:top;",
                               height="75%",
                               width = "65%")),
              br(),
              br(),
              HTML("<b> Con el apoyo de:</b>"),
              br(),
              tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                     tags$img(src="ohchr.png",
                              style=";vertical-align:top;",
                              # title="Example Image Link",
                              height="60%",
                              width = "45%")),
            ),

            mainPanel(

              tags$h3(style="display:inline-block",
                      uiOutput("title_alimentacion_r")),
              div(style="display:inline-block;margin: 0px;",
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("exclamation", lib = "font-awesome"),
                    uiOutput("conindicador_alimentacion_r"))
              ),
              div(style="display:inline-block",
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("calculator", lib = "font-awesome"),
                    uiOutput("calculo_alimentacion_r"))
              ),
              div(style="display:inline-block;margin: 0px;",
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-exclamation-triangle"),
                    uiOutput("observacion_alimentacion_r"))
              ),
              div(style="display:inline-block;margin: 0px;", 
                  dropdown(
                    style = "minimal",
                    status = "primary",
                    width = "500px",
                    right = TRUE,
                    icon = icon("fas fa-arrows-rotate"),
                    uiOutput("actualizacion_alimentacion_r"))
              ),
              tags$h5(uiOutput("subtitle_alimentacion_r")),
              tags$h6(uiOutput("nota_alimentacion_r")),
              br(),
              withSpinner(plotOutput("plot_alimentacion_r", height = "500px"),
                          type = 2),
              br(),
              fluidRow(column(12, div(downloadButton(outputId = "baja_p_alimentacion_r",
                                                     label = "Descarga el gráfico"),
                                      style = "float: right"))),
              br(),
              br(),
              DTOutput("table_alimentacion_r"),
              br(),
              br(),
              fluidRow(column(12, div(downloadButton("dwl_tab_alimentacion_r", "Descarga la tabla"),
                                      style = "float: right"))),
              br(),
              br()
            )
          )
        )
      )
    ),
    
  
    # Poblaciones -----
    
    tabPanel(
      title = "Poblaciones", icon = icon("fas fa-user"),
      
      fluidRow(
        
        sidebarPanel(
          
          width = 3,
          
          selectInput(
            inputId = "poblaciones",
            label = "Seleccione población:",
            choices = sort(lista_poblaciones),
            selected = "Ascendencia étnico-racial"
          ),
          
          uiOutput("selector_poblaciones_indicadores"),
          
          uiOutput("selector_poblaciones_corte_2"),
          
          uiOutput("chbox_poblaciones_2"),
          
          uiOutput("selector_poblaciones_corte"),
          
          uiOutput("s_poblaciones_fecha"),
          
          uiOutput("chbox_poblaciones"),
          
          HTML("<b> Instituciones:</b>"),
          br(),
          tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                 tags$img(src="INDDHH-Logo.png",
                          style=";vertical-align:top;",
                          # title="Example Image Link",
                          height="75%",
                          width = "65%")),
          br(),
          br(),
          tags$a( href="https://umad.cienciassociales.edu.uy/",
                  tags$img(src="logo_umad.png",
                           style=";vertical-align:top;",
                           height="75%",
                           width = "65%")),
          br(),
          br(),
          HTML("<b> Con el apoyo de:</b>"),
          br(),
          tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                 tags$img(src="ohchr.png",
                          style=";vertical-align:top;",
                          # title="Example Image Link",
                          height="60%",
                          width = "45%")),
        ),
        
        mainPanel(
          
          tags$h3(style="display:inline-block",
                  uiOutput("title_poblaciones")),
          div(style="display:inline-block;margin: 0px;",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("exclamation", lib = "font-awesome"),
                uiOutput("conindicador_poblaciones"))
          ),
          div(style="display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("calculator", lib = "font-awesome"),
                uiOutput("calculo_poblaciones"))
          ),
          div(style="display:inline-block;margin: 0px;",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("fas fa-exclamation-triangle"),
                uiOutput("observacion_poblaciones"))
          ),
          div(style="display:inline-block;margin: 0px;",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("fas fa-arrows-rotate"),
                uiOutput("actualizacion_poblaciones"))
          ),
          tags$h5(uiOutput("subtitle_poblaciones")),
          tags$h6(uiOutput("nota_poblaciones_r")),
          br(),
          withSpinner(plotOutput("plot_poblaciones", height = "500px"),
                      type = 2),
          br(),
          fluidRow(column(12, div(downloadButton(outputId = "baja_p_poblaciones",
                                                 label = "Descarga el gráfico"),
                                  style = "float: right"))),
          br(),
          br(),
          DTOutput("table_poblaciones"),
          br(),
          br(),
          fluidRow(column(12, div(downloadButton("dwl_tab_poblaciones", "Descarga la tabla"),
                                  style = "float: right"))),
          br(),
          br()
        )
      )
    ),
  )
)


##  3.  SERVER  =============================================================

server <- function(input, output) {
  
  
  
  
  ### 3.1. Educación Políticas   ==============================================
  
  # * Data reactiva   =================================================    
  dat_edu_pp <- reactive({
    
    req(input$indicador_edu_pp)
    
    dat %>%
      filter(nomindicador == input$indicador_edu_pp) 
    
  })
  
  output$selector_edu_pp_corte_2 <- renderUI({
    
    if(input$indicador_edu_pp %in% lista_ind_2){
      
      selectInput(
        inputId = "edu_pp_corte_2",
        label = "Seleccione primer corte:",
        choices = dat_edu_pp() %>% 
          select(corte_2) %>%
          arrange(corte_2) %>% 
          unique() %>% 
          pull(),
        selected = dat_edu_pp() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte_2) %>% 
          pull()
      )
      
    } else {
      
      NULL
    }
    
  })
  
  output$chbox_edu_pp_2 <- renderUI({
    
    if(input$indicador_edu_pp %in% lista_ind_2 & input$indicador_edu_pp %notin% lista_especial){
      
      # if(input$edu_pp_corte %notin% c("Total", "Departamento") & input$indicador_edu_pp %notin% lista_vunico) {
      
      edu_pp_corte_var_2 <- rlang::sym(to_varname(input$edu_pp_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_edu_pp_2",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_edu_pp() %>%
                           filter(corte_2 == input$edu_pp_corte_2) %>% 
                           distinct(!!edu_pp_corte_var_2) %>%
                           pull(),
                         selected = dat_edu_pp() %>%
                           filter(corte_2 == input$edu_pp_corte_2) %>% 
                           filter(jerarquia_cat_2 == "1") %>%
                           distinct(!!edu_pp_corte_var_2) %>%
                           pull()
      )
      
      # } else {
      #   
      #   return(NULL)
      #   
      #   }
      
    } else {
      
      return(NULL)
      
    }      
  })
  
  
  output$selector_edu_pp_corte <- renderUI({
    
    selectInput(
      inputId = "edu_pp_corte",
      label = "Seleccione corte:",
      choices = dat_edu_pp() %>% 
        select(corte) %>%
        arrange(corte) %>% 
        unique() %>% 
        pull(),
      selected = dat_edu_pp() %>% 
        filter(jerarquia == "1") %>%  
        distinct(corte) %>% 
        pull()
    )
    
  })
  
  output$chbox_edu_pp <- renderUI({
    
    if(input$edu_pp_corte %in% lista_ind_2 & input$edu_pp_corte %notin% c("Total", "Departamento") & input$indicador_edu_pp %notin% lista_vunico) {
      
      edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
      edu_pp_corte_var_2 <- rlang::sym(to_varname(input$edu_pp_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_edu_pp",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_edu_pp() %>%
                           filter(!!edu_pp_corte_var_2 %in% input$checkbox_edu_pp_2) %>%
                           filter(corte == input$edu_pp_corte) %>% 
                           distinct(!!edu_pp_corte_var) %>%
                           pull(),
                         selected = dat_edu_pp() %>%
                           filter(!!edu_pp_corte_var_2 %in% input$checkbox_edu_pp_2) %>%
                           filter(corte == input$edu_pp_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!edu_pp_corte_var) %>%
                           pull()
      )
      
    } else if(input$edu_pp_corte %notin% lista_ind_2 & input$edu_pp_corte %notin% c("Total", "Departamento") & input$indicador_edu_pp %notin% lista_vunico) {
      
      edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
      
      checkboxGroupInput(inputId = "checkbox_edu_pp",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_edu_pp() %>%
                           filter(corte == input$edu_pp_corte) %>% 
                           distinct(!!edu_pp_corte_var) %>%
                           pull(),
                         selected = dat_edu_pp() %>%
                           filter(corte == input$edu_pp_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!edu_pp_corte_var) %>%
                           pull()
      )
      
    } else {
      
      return(NULL)
    }
    
  })
  
  # Selector de fecha
  output$s_edu_pp_fecha <- renderUI({
    
    if(input$edu_pp_corte == "Departamento" & input$indicador_edu_pp %notin% lista_ind_2) {
      
      req(input$edu_pp_corte, input$indicador_edu_pp)
      
      req(nrow(dat_edu_pp()) > 0)
      
      selectInput(
        inputId = "fecha_dpto_edu_pp",
        label = "Seleccione año:",
        choices = dat_edu_pp() %>% 
          filter(nomindicador == input$indicador_edu_pp) %>%
          drop_na(Valor) %>%
          select(ano) %>%
          arrange(desc(ano)) %>% 
          unique() %>% 
          pull(),
        selected = max(input$ano)
      )
      
    } else if (input$indicador_edu_pp %in% lista_serie_cat){
      
      return(NULL)
      
      
    } else if (input$indicador_edu_pp %in% lista_vunico){
      
      return(NULL)
      
    } else  {
      
      req(input$edu_pp_corte, input$indicador_edu_pp)
      req(nrow(dat_edu_pp()) > 0)
      
      tagList(
        # tags$style(type = 'text/css', 
        #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
        # div(id = 'big_slider',
        
        sliderInput("fecha_edu_pp", 
                    label = "Rango de tiempo", 
                    sep = "",
                    dragRange = T,
                    min = min(dat_edu_pp()$ano), 
                    max = max(dat_edu_pp()$ano), 
                    value = c(min(dat_edu_pp()$ano), 
                              max(dat_edu_pp()$ano))
        )
      )
      
    }
  })
  
  # # Selector de corte según categoría y data temporal
  # dat_edu_pp <- reactive({
  #   
  #   req(input$edu_pp_corte)
  #   
  #   if(input$indicador_edu_pp %in% lista_ind_2){
  #     
  #     dat_salarios <- dat_edu_pp() %>%
  #       filter(corte_2 == input$edu_pp_corte_2) %>% 
  #       filter(corte == input$edu_pp_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   } else {
  #     
  #     dat_edu_pp() %>%
  #       filter(corte == input$edu_pp_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   }
  # })
  
  # * Metadata   ======================================================
  
  # Title
  output$title_edu_pp <- renderUI({ 
    helpText(HTML(unique(dat_edu_pp()$nomindicador)))
  })
  
  # Subtitle
  output$subtitle_edu_pp <- renderUI({ 
    helpText(HTML(unique(dat_edu_pp()$definicion)))
  })
  
  # Nombre conceptual
  output$conindicador_edu_pp <- renderUI({ 
    helpText(HTML(paste("<b> Nombre conceptual:</b>", unique(dat_edu_pp()$conindicador))))
  })
  
  # Calculo
  output$calculo_edu_pp <- renderUI({ 
    helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_edu_pp()$calculo))))
  })
  
  # Observaciones
  output$observacion_edu_pp <- renderUI({ 
    helpText(HTML(paste("<b> Observaciones:</b>", unique(dat_edu_pp()$observaciones))))
  })
  
  # Actualización
  output$actualizacion_edu_pp <- renderUI({ 
    helpText(HTML(paste("<b> Última actualización:</b>", unique(dat_edu_pp()$actualizacion))))
  })
  
  
  # * Gráficos   ======================================================
  
  output$plot_edu_pp <- renderPlot({
    
    req(input$indicador_edu_pp, input$edu_pp_corte)
    
    if(input$indicador_edu_pp %in% lista_serie_cat){
      
      req(input$indicador_edu_pp)
      
      # Total
      if(input$edu_pp_corte == "Total"){
        
        dat_plot <- dat_edu_pp() %>%
          filter(corte == "Total")
        
        plot <- ggplot(dat_plot, aes(x = fecha_cat, y = Valor, group = 1)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_edu_pp),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador edu pp.png", width = 40, height = 25, units = "cm")
        
        # Según corte
      } else if(input$edu_pp_corte != "Total") {
        
        edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
        
        dat_plot <- dat_edu_pp() %>%
          filter(corte == input$edu_pp_corte) %>%
          filter(!!edu_pp_corte_var %in% input$checkbox_edu_pp)
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            colour = edu_pp_corte_var, group = edu_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_pp,
                                    "según",
                                    tolower(input$edu_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador edu pp.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_edu_pp %in% lista_especial ){
      
      if (input$edu_pp_corte_2 == "Total"){        
        
        req(input$edu_pp_corte, input$indicador_edu_pp)
        
        edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
        
        dat_plot <- dat_edu_pp() %>%
          filter(ano >= input$fecha_edu_pp[1] &
                   ano <= input$fecha_edu_pp[2]) %>%
          filter(corte == input$edu_pp_corte) %>%
          # filter(!!edu_pp_corte_var %in% input$checkbox_edu_pp) %>% 
          filter(corte_2 == input$edu_pp_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = edu_pp_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_pp,
                                    "según",
                                    tolower(input$edu_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador edu pp.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        req(input$edu_pp_corte, input$indicador_edu_pp)
        
        edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
        edu_pp_corte_var_2 <- rlang::sym(to_varname(input$edu_pp_corte_2))
        
        dat_plot <- dat_edu_pp() %>%
          filter(ano >= input$fecha_edu_pp[1] &
                   ano <= input$fecha_edu_pp[2]) %>%
          filter(corte == input$edu_pp_corte) %>%
          # filter(!!edu_pp_corte_var %in% input$checkbox_edu_pp) %>% 
          filter(corte_2 == input$edu_pp_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = edu_pp_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_pp,
                                    "según",
                                    tolower(input$edu_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", edu_pp_corte_var_2)))
        
        print(plot)
        ggsave("www/indicador edu pp.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_edu_pp %in% lista_vunico & input$indicador_edu_pp %in% lista_ind_2){
      
      req(input$edu_pp_corte, input$indicador_edu_pp)
      
      edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
      edu_pp_corte_var_2 <- rlang::sym(to_varname(input$edu_pp_corte_2))
      
      dat_plot <- dat_edu_pp() %>%
        filter(ano >= input$fecha_edu_pp[1] &
                 ano <= input$fecha_edu_pp[2]) %>%
        filter(corte == input$edu_pp_corte) %>%
        filter(!!edu_pp_corte_var %in% input$checkbox_edu_pp) %>%
        filter(!!edu_pp_corte_var_2 %in% input$checkbox_edu_pp_2) %>%
        filter(corte_2 == input$edu_pp_corte_2)
      
      plot <- ggplot(dat_plot,
                     aes_string(x = "fecha_cat", y = "Valor",
                                fill = edu_pp_corte_var)) +
        geom_col(position = "dodge", width = .7, alpha = .8) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x=element_blank(),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_edu_pp,
                                  "según",
                                  tolower(input$edu_pp_corte),
                                  "en",
                                  unique(dat_plot$fecha_cat))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_fill_brewer(name = "", palette = "Paired") +
        facet_wrap(as.formula(paste("~", edu_pp_corte_var_2)))
      
      print(plot)
      ggsave("www/indicador edu pp.png", width = 40, height = 25, units = "cm")
      
    } else if(input$indicador_edu_pp %in% lista_vunico & input$edu_pp_corte != "Departamento") {
      
      req(input$edu_pp_corte, input$indicador_edu_pp)
      
      dat_plot <- dat_edu_pp() %>%
        filter(corte == input$edu_pp_corte) %>%
        janitor::remove_empty("cols")
      
      if(input$edu_pp_corte == "Total"){
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor")) +
          geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
          geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_pp,
                                    "según",
                                    tolower(input$edu_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador edu pp.png", width = 40, height = 25, units = "cm")
        
        
      } else {
        
        edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor",
                                  fill = edu_pp_corte_var)) +
          geom_col(position = "dodge", alpha = .8, stroke = 1, color = "black") +
          geom_text(aes_string(group = edu_pp_corte_var, label = "Valor"),
                    position = position_dodge2(width = .9), vjust = -.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_pp,
                                    "según",
                                    tolower(input$edu_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador edu pp.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$indicador_edu_pp %in% lista_ind_2) {
      
      req(input$edu_pp_corte, input$edu_pp_corte_2,
          input$fecha_edu_pp, input$checkbox_edu_pp)
      
      edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
      edu_pp_corte_var_2 <- rlang::sym(to_varname(input$edu_pp_corte_2))
      
      dat_plot <- dat_edu_pp() %>%
        filter(ano >= input$fecha_edu_pp[1] &
                 ano <= input$fecha_edu_pp[2]) %>%
        filter(corte == input$edu_pp_corte) %>%
        filter(!!edu_pp_corte_var_2 %in% input$checkbox_edu_pp_2) %>%
        filter(!!edu_pp_corte_var %in% input$checkbox_edu_pp) %>% 
        filter(corte_2 == input$edu_pp_corte_2)
      
      if(input$edu_pp_corte_2 == "Total"){
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == "Total")
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = edu_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_pp,
                                    "según",
                                    tolower(input$edu_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
      } else if(input$edu_pp_corte_2 != "Total") {
        
        edu_pp_corte_var_2 <- rlang::sym(to_varname(input$edu_pp_corte_2))
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == input$edu_pp_corte_2)
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = edu_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          scale_x_continuous(breaks = int_breaks) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_pp,
                                    "según",
                                    tolower(input$edu_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          facet_wrap(as.formula(paste("~", edu_pp_corte_var_2)))
        
      }
      
      print(plot)
      ggsave("www/indicador edu pp.png", width = 40, height = 25, units = "cm")
      
      
      # Grafico simple normal
    } else if(input$edu_pp_corte == "Total") {
      
      req(input$indicador_edu_pp, input$fecha_edu_pp)
      
      dat_plot <- dat_edu_pp() %>%
        filter(ano >= input$fecha_edu_pp[1] &
                 ano <= input$fecha_edu_pp[2]) %>%
        filter(corte == "Total")
      
          if(input$indicador_edu_pp %in% lista_met){
            
            dat_plot <- dat_plot %>% 
              mutate(metodo = case_when(
                fecha <= 2019 ~ "pre",
                fecha == 2020 ~ "2020",
                fecha == 2021 ~ "2021",
                fecha >= 2022 ~ "post",
              ))
            
            plot <- ggplot(dat_plot,
                           aes(x = fecha, y = Valor, alpha = metodo)) +
              geom_line(size = 1, colour = color_defecto) +
              geom_point(size = 3, colour = color_defecto) +
              # scale_x_continuous(breaks = int_breaks) +
              theme_bdd(base_size = 12) +
              theme(axis.text.x = element_text(angle = 0),
                    legend.position = "none") +
              labs(x = "",  y = "",
                   title = wrapit(input$indicador_edu_pp),
                   caption = wrapit(unique(dat_plot$cita))) +
              scale_alpha_manual(values = c("pre" = .3, 
                                            "2020" = .5, 
                                            "2021" = .6, 
                                            "post"= .8))
            
            print(plot)
            ggsave("www/indicador edu pp.png", width = 40, height = 25, units = "cm")
            
          } else {
            
            plot <- ggplot(dat_plot,
                           aes(x = fecha, y = Valor)) +
              geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
              geom_point(size = 3, colour = color_defecto) +
              scale_x_continuous(breaks = int_breaks) +
              theme_bdd(base_size = 12) +
              theme(axis.text.x = element_text(angle = 0),
                    legend.position = "bottom") +
              labs(x = "",  y = "",
                   title = wrapit(input$indicador_edu_pp),
                   caption = wrapit(unique(dat_plot$cita)))
            
            print(plot)
            ggsave("www/indicador edu pp.png", width = 40, height = 25, units = "cm")
            
          }
          
      
    } else if(input$edu_pp_corte == "Departamento" &
              input$indicador_edu_pp %notin% lista_ind_2 ) {
      
      req(input$indicador_edu_pp, input$fecha_dpto_edu_pp)
      
      dat_plot <- dat_edu_pp() %>%
        filter(corte == "Departamento") %>%
        filter(ano == input$fecha_dpto_edu_pp) %>%
        select(departamento, Valor, fuente, cita)
      
      dep_j <- dep %>%
        left_join(dat_plot, by = c("nombre" = "departamento"))
      
      plot <-  ggplot(dep_j, aes(fill = Valor)) +
        geom_sf() +
        geom_sf_text(aes(label = Valor), colour = "black",
                     size = 4, fontface = "bold")+
        viridis::scale_fill_viridis(name = "", direction = -1)+
        labs(x = "",
             y = "",
        )+
        theme_bdd(base_size = 14) +
        theme(axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.grid.major = element_line(colour = "transparent"),
        ) +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_edu_pp,
                                  "en",
                                  input$fecha_dpto_edu_pp), w = 80),
             caption = wrapit(unique(dat_plot$cita), w = 80))
      
      print(plot)
      ggsave("www/indicador edu pp.png", width = 40, height = 25, units = "cm")
      
      
    } else if(input$edu_pp_corte != "Total") {
      
      req(input$edu_pp_corte, input$indicador_edu_pp,
          input$fecha_edu_pp, input$checkbox_edu_pp)
      
      dat_plot <- dat_edu_pp() %>%
        filter(ano >= input$fecha_edu_pp[1] &
                 ano <= input$fecha_edu_pp[2]) %>%
        filter(corte == input$edu_pp_corte) %>%
        janitor::remove_empty("cols")
      
      edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
      
      dat_plot <- filter(dat_plot,
                         !!edu_pp_corte_var %in% input$checkbox_edu_pp)
      
            if(input$indicador_edu_pp %in% lista_met){
              
              dat_plot <- dat_plot %>% 
                mutate(metodo = case_when(
                  fecha <= 2019 ~ "pre",
                  fecha == 2020 ~ "2020",
                  fecha == 2021 ~ "2021",
                  fecha >= 2022 ~ "post",
                ))
              
              plot <- ggplot(dat_plot,
                             aes_string(x = "fecha", y = "Valor", colour = edu_pp_corte_var, alpha = "metodo")) +
                geom_line(size = 1) +
                geom_point(size = 3) +
                theme_bdd(base_size = 12) +
                theme(axis.text.x = element_text(angle = 0),
                      legend.position = "bottom") +
                scale_x_continuous(breaks = int_breaks) +
                labs(x = "",  y = "",
                     title = wrapit(paste(input$indicador_edu_pp,
                                          "según",
                                          tolower(input$edu_pp_corte))),
                     caption = wrapit(unique(dat_plot$cita))) +
                scale_colour_manual(name = "", values = paleta_expandida) +
                scale_alpha_manual(values = c("pre" = .3, 
                                              "2020" = .5, 
                                              "2021" = .6, 
                                              "post"= .8)) +
                guides(alpha = "none")
      
              print(plot)
              ggsave("www/indicador edu pp.png", width = 40, height = 25, units = "cm")
              
              
            } else {
              
              plot <- ggplot(dat_plot,
                             aes_string(x = "fecha", y = "Valor", colour = edu_pp_corte_var)) +
                geom_line(size = 1, alpha = 0.5) +
                geom_point(size = 3) +
                theme_bdd(base_size = 12) +
                theme(axis.text.x = element_text(angle = 0),
                      legend.position = "bottom") +
                scale_x_continuous(breaks = int_breaks) +
                labs(x = "",  y = "",
                     title = wrapit(paste(input$indicador_edu_pp,
                                          "según",
                                          tolower(input$edu_pp_corte))),
                     caption = wrapit(unique(dat_plot$cita))) +
                scale_colour_manual(name = "", values = paleta_expandida)
              
              print(plot)
              ggsave("www/indicador edu pp.png", width = 40, height = 25, units = "cm")
              
              
            }
            
      
    }
    
  })
  
  
  # * Descarga gráficos   =============================================
  
  output$baja_p_edu_pp <- downloadHandler(
    filename <- function() {
      paste("indicador edu pp", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/indicador edu pp.png", file)
    },
    contentType = "www/indicador edu pp"
  )
  
  
  # * Tablas   ========================================================
  
  # Data
  edu_pp_tab <- reactive({
    
    if(input$indicador_edu_pp %in%  lista_especial){
      
      edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
      edu_pp_corte_var_2 <- rlang::sym(to_varname(input$edu_pp_corte_2))
      
      dat_edu_pp() %>%
        filter(corte_2 == input$edu_pp_corte_2) %>% 
        select(Fecha, edu_pp_corte_var, edu_pp_corte_var_2, Valor) %>%
        arrange(desc(Fecha)) %>%
        pivot_wider(values_from = "Valor",
                    names_from = edu_pp_corte_var) 
      
    } else if(input$indicador_edu_pp %in% lista_vunico & input$edu_pp_corte == "Total"){
      
      req(input$edu_pp_corte, input$indicador_edu_pp)
      
      dat_edu_pp() %>%
        filter(corte == "Total") %>% 
        select(fecha_cat, Valor) %>%
        arrange(desc(fecha_cat)) %>%
        rename(Fecha = fecha_cat)
      
    } else if(input$indicador_edu_pp %in% lista_vunico & input$edu_pp_corte != "Total") {
      
      req(input$edu_pp_corte, input$indicador_edu_pp)
      
      edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
      
      dat_cut <- dat_edu_pp() %>%
        filter(corte == input$edu_pp_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        select(fecha_cat, edu_pp_corte_var, Valor) %>%
        arrange(desc(fecha_cat)) %>% 
        rename(Fecha = fecha_cat) %>%
        pivot_wider(values_from = "Valor",
                    names_from = edu_pp_corte_var)
      
      
    } else if(input$indicador_edu_pp %in% lista_ind_2) {
      
      req(input$edu_pp_corte, input$indicador_edu_pp, input$fecha_edu_pp)
      
      edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
      
      dat_cut <- dat_edu_pp() %>%
        filter(corte == input$edu_pp_corte) %>%
        filter(!!edu_pp_corte_var %in% input$checkbox_edu_pp)
      
      if(input$edu_pp_corte_2 == "Total"){
        
        dat_cut %>%
          filter(ano >= input$fecha_edu_pp[1] &
                   ano <= input$fecha_edu_pp[2]) %>%
          filter(corte_2 == "Total") %>% 
          select(Fecha, edu_pp_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = edu_pp_corte_var)
        
      } else if(input$edu_pp_corte_2 != "Total") {
        
        edu_pp_corte_var_2 <- rlang::sym(to_varname(input$edu_pp_corte_2))
        
        dat_cut %>%
          filter(ano >= input$fecha_edu_pp[1] &
                   ano <= input$fecha_edu_pp[2]) %>%
          filter(corte_2 == input$edu_pp_corte_2) %>% 
          select(Fecha, edu_pp_corte_var, edu_pp_corte_var_2, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = edu_pp_corte_var) 
        
      }
      
    } else if(input$edu_pp_corte == "Total") {
      
      req(input$edu_pp_corte, input$indicador_edu_pp, input$fecha_edu_pp)
      
      dat_edu_pp() %>%
        filter(corte == "Total") %>% 
        filter(ano >= input$fecha_edu_pp[1] &
                 ano <= input$fecha_edu_pp[2]) %>%
        select(Fecha, Valor) %>%
        arrange(desc(Fecha))
      
    } else if(input$edu_pp_corte != "Total") {
      
      req(input$edu_pp_corte, input$indicador_edu_pp, input$fecha_edu_pp)
      
      edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
      
      dat_cut <- dat_edu_pp() %>%
        filter(corte == input$edu_pp_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        filter(ano >= input$fecha_edu_pp[1] &
                 ano <= input$fecha_edu_pp[2]) %>% 
        select(Fecha, edu_pp_corte_var, Valor) %>%
        arrange(desc(Fecha)) %>% 
        pivot_wider(values_from = "Valor",
                    names_from = edu_pp_corte_var)
      
    }
  })
  
  # Metadata 
  edu_pp_meta <- reactive({
    
    dat_edu_pp() %>%
      select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, observaciones, actualizacion, cita) %>% 
      mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
      distinct() %>% 
      gather(key = " ", value = " ")
    
  })
  
  # Excel
  list_edu_pp <- reactive({
    list_edu_pp <- list("Data" = edu_pp_tab(),
                        "Metadata" = edu_pp_meta())
  })
  
  # Render
  output$table_edu_pp <- renderDT({
    
    DT::datatable(edu_pp_tab(),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    input$indicador_edu_pp,
                    style = "color:black; font-size:110%;")
    ) 
    
  })
  
  # * Descarga tablas   ================================================
  
  output$dwl_tab_edu_pp <- downloadHandler(
    
    filename = function() {
      paste("resultados-", input$indicador_edu_pp, ".xlsx", sep = "")
    },
    content = function(file) {
      
      openxlsx::write.xlsx(list_edu_pp(), file)
      
    }
  )
  
  
  
  ### 3.2. Educación Resultados   ==============================================
  
  # * Data reactiva   =================================================    
  dat_edu_r <- reactive({
    
    req(input$indicador_edu_r)
    
    dat %>%
      filter(nomindicador == input$indicador_edu_r) 
    
  })
  
  output$selector_edu_r_corte_2 <- renderUI({
    
    if(input$indicador_edu_r %in% lista_ind_2){
      
      selectInput(
        inputId = "edu_r_corte_2",
        label = "Seleccione primer corte:",
        choices = dat_edu_r() %>% 
          select(corte_2) %>%
          arrange(corte_2) %>% 
          unique() %>% 
          pull(),
        selected = dat_edu_r() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte_2) %>% 
          pull()
      )
      
    } else {
      
      NULL
    }
    
  })
  
  output$chbox_edu_r_2 <- renderUI({
    
    if(input$indicador_edu_r %in% lista_ind_2 & input$indicador_edu_r %notin% lista_especial){
      
      # if(input$edu_r_corte %notin% c("Total", "Departamento") & input$indicador_edu_r %notin% lista_vunico) {
      
      edu_r_corte_var_2 <- rlang::sym(to_varname(input$edu_r_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_edu_r_2",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_edu_r() %>%
                           filter(corte_2 == input$edu_r_corte_2) %>% 
                           distinct(!!edu_r_corte_var_2) %>%
                           pull(),
                         selected = dat_edu_r() %>%
                           filter(corte_2 == input$edu_r_corte_2) %>% 
                           filter(jerarquia_cat_2 == "1") %>%
                           distinct(!!edu_r_corte_var_2) %>%
                           pull()
      )
      
      # } else {
      #   
      #   return(NULL)
      #   
      #   }
      
    } else {
      
      return(NULL)
      
    }      
  })
  
  
  output$selector_edu_r_corte <- renderUI({
    
    selectInput(
      inputId = "edu_r_corte",
      label = "Seleccione corte:",
      choices = dat_edu_r() %>% 
        select(corte) %>%
        arrange(corte) %>% 
        unique() %>% 
        pull(),
      selected = dat_edu_r() %>% 
        filter(jerarquia == "1") %>%  
        distinct(corte) %>% 
        pull()
    )
    
  })
  
  output$chbox_edu_r <- renderUI({
    
    if(input$edu_r_corte %in% lista_ind_2 & input$edu_r_corte %notin% c("Total", "Departamento") & input$indicador_edu_r %notin% lista_vunico) {
      
      edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
      edu_r_corte_var_2 <- rlang::sym(to_varname(input$edu_r_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_edu_r",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_edu_r() %>%
                           filter(!!edu_r_corte_var_2 %in% input$checkbox_edu_r_2) %>%
                           filter(corte == input$edu_r_corte) %>% 
                           distinct(!!edu_r_corte_var) %>%
                           pull(),
                         selected = dat_edu_r() %>%
                           filter(!!edu_r_corte_var_2 %in% input$checkbox_edu_r_2) %>%
                           filter(corte == input$edu_r_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!edu_r_corte_var) %>%
                           pull()
      )
      
    } else if(input$edu_r_corte %notin% lista_ind_2 & input$edu_r_corte %notin% c("Total", "Departamento") & input$indicador_edu_r %notin% lista_vunico) {
      
      edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
      
      checkboxGroupInput(inputId = "checkbox_edu_r",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_edu_r() %>%
                           filter(corte == input$edu_r_corte) %>% 
                           distinct(!!edu_r_corte_var) %>%
                           pull(),
                         selected = dat_edu_r() %>%
                           filter(corte == input$edu_r_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!edu_r_corte_var) %>%
                           pull()
      )
      
    } else {
      
      return(NULL)
    }
    
  })
  
  # Selector de fecha
  output$s_edu_r_fecha <- renderUI({
    
    if(input$edu_r_corte == "Departamento" & input$indicador_edu_r %notin% lista_ind_2) {
      
      req(input$edu_r_corte, input$indicador_edu_r)
      
      req(nrow(dat_edu_r()) > 0)
      
      selectInput(
        inputId = "fecha_dpto_edu_r",
        label = "Seleccione año:",
        choices = dat_edu_r() %>% 
          filter(nomindicador == input$indicador_edu_r) %>%
          drop_na(Valor) %>%
          select(ano) %>%
          arrange(desc(ano)) %>% 
          unique() %>% 
          pull(),
        selected = max(input$ano)
      )
      
    } else if (input$indicador_edu_r %in% lista_serie_cat){
      
      return(NULL)
      
      
    } else if (input$indicador_edu_r %in% lista_vunico){
      
      return(NULL)
      
    } else  {
      
      req(input$edu_r_corte, input$indicador_edu_r)
      req(nrow(dat_edu_r()) > 0)
      
      tagList(
        # tags$style(type = 'text/css', 
        #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
        # div(id = 'big_slider',
        
        sliderInput("fecha_edu_r", 
                    label = "Rango de tiempo", 
                    sep = "",
                    dragRange = T,
                    min = min(dat_edu_r()$ano), 
                    max = max(dat_edu_r()$ano), 
                    value = c(min(dat_edu_r()$ano), 
                              max(dat_edu_r()$ano))
        )
      )
      
    }
  })
  
  # # Selector de corte según categoría y data temporal
  # dat_edu_r <- reactive({
  #   
  #   req(input$edu_r_corte)
  #   
  #   if(input$indicador_edu_r %in% lista_ind_2){
  #     
  #     dat_salarios <- dat_edu_r() %>%
  #       filter(corte_2 == input$edu_r_corte_2) %>% 
  #       filter(corte == input$edu_r_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   } else {
  #     
  #     dat_edu_r() %>%
  #       filter(corte == input$edu_r_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   }
  # })
  
  # * Metadata   ======================================================
  
  # Title
  output$title_edu_r <- renderUI({ 
    helpText(HTML(unique(dat_edu_r()$nomindicador)))
  })
  
  # Subtitle
  output$subtitle_edu_r <- renderUI({ 
    helpText(HTML(unique(dat_edu_r()$definicion)))
  })
  
  # Nota
  output$nota_edu_r <- renderUI({ 
    
    if(input$indicador_edu_r %in% lista_nota){
    helpText(HTML(unique(dat_edu_r()$nota)))
    
    } else(NULL)
  })
  
  # Nombre conceptual
  output$conindicador_edu_r <- renderUI({ 
    helpText(HTML(paste("<b> Nombre conceptual:</b>", unique(dat_edu_r()$conindicador))))
  })
  
  # Calculo
  output$calculo_edu_r <- renderUI({ 
    helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_edu_r()$calculo))))
  })
  
  # Observaciones
  output$observacion_edu_r <- renderUI({ 
    helpText(HTML(paste("<b> Observaciones:</b>", unique(dat_edu_r()$observaciones))))
  })
  
  # Actualización
  output$actualizacion_edu_r <- renderUI({ 
    helpText(HTML(paste("<b> Última actualización:</b>", unique(dat_edu_r()$actualizacion))))
  })
  
  
  # * Gráficos   ======================================================
  
  output$plot_edu_r <- renderPlot({
    
    req(input$indicador_edu_r, input$edu_r_corte)
    
    if(input$indicador_edu_r %in% lista_serie_cat){
      
      req(input$indicador_edu_r)
      
      # Total
      if(input$edu_r_corte == "Total"){
        
        dat_plot <- dat_edu_r() %>%
          filter(corte == "Total")
        
        plot <- ggplot(dat_plot, aes(x = fecha_cat, y = Valor, group = 1)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_edu_r),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador edu r.png", width = 40, height = 25, units = "cm")
        
        # Según corte
      } else if(input$edu_r_corte != "Total") {
        
        edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
        
        dat_plot <- dat_edu_r() %>%
          filter(corte == input$edu_r_corte) %>%
          filter(!!edu_r_corte_var %in% input$checkbox_edu_r)
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            colour = edu_r_corte_var, group = edu_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_r,
                                    "según",
                                    tolower(input$edu_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador edu r.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_edu_r %in% lista_especial ){
      
      if (input$edu_r_corte_2 == "Total"){        
        
        req(input$edu_r_corte, input$indicador_edu_r)
        
        edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
        
        dat_plot <- dat_edu_r() %>%
          filter(ano >= input$fecha_edu_r[1] &
                   ano <= input$fecha_edu_r[2]) %>%
          filter(corte == input$edu_r_corte) %>%
          # filter(!!edu_r_corte_var %in% input$checkbox_edu_r) %>% 
          filter(corte_2 == input$edu_r_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = edu_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_r,
                                    "según",
                                    tolower(input$edu_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador edu r.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        req(input$edu_r_corte, input$indicador_edu_r)
        
        edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
        edu_r_corte_var_2 <- rlang::sym(to_varname(input$edu_r_corte_2))
        
        dat_plot <- dat_edu_r() %>%
          filter(ano >= input$fecha_edu_r[1] &
                   ano <= input$fecha_edu_r[2]) %>%
          filter(corte == input$edu_r_corte) %>%
          # filter(!!edu_r_corte_var %in% input$checkbox_edu_r) %>% 
          filter(corte_2 == input$edu_r_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = edu_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_r,
                                    "según",
                                    tolower(input$edu_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", edu_r_corte_var_2)))
        
        print(plot)
        ggsave("www/indicador edu r.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_edu_r %in% lista_vunico & input$indicador_edu_r %in% lista_ind_2){
      
      req(input$edu_r_corte, input$indicador_edu_r)
      
      edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
      edu_r_corte_var_2 <- rlang::sym(to_varname(input$edu_r_corte_2))
      
      dat_plot <- dat_edu_r() %>%
        filter(ano >= input$fecha_edu_r[1] &
                 ano <= input$fecha_edu_r[2]) %>%
        filter(corte == input$edu_r_corte) %>%
        filter(!!edu_r_corte_var %in% input$checkbox_edu_r) %>%
        filter(!!edu_r_corte_var_2 %in% input$checkbox_edu_r_2) %>%
        filter(corte_2 == input$edu_r_corte_2)
      
      plot <- ggplot(dat_plot,
                     aes_string(x = "fecha_cat", y = "Valor",
                                fill = edu_r_corte_var)) +
        geom_col(position = "dodge", width = .7, alpha = .8) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x=element_blank(),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_edu_r,
                                  "según",
                                  tolower(input$edu_r_corte),
                                  "en",
                                  unique(dat_plot$fecha_cat))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_fill_brewer(name = "", palette = "Paired") +
        facet_wrap(as.formula(paste("~", edu_r_corte_var_2)))
      
      print(plot)
      ggsave("www/indicador edu r.png", width = 40, height = 25, units = "cm")
      
    } else if(input$indicador_edu_r %in% lista_vunico & input$edu_r_corte != "Departamento") {
      
      req(input$edu_r_corte, input$indicador_edu_r)
      
      dat_plot <- dat_edu_r() %>%
        filter(corte == input$edu_r_corte) %>%
        janitor::remove_empty("cols")
      
      if(input$edu_r_corte == "Total"){
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor")) +
          geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
          geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_r,
                                    "según",
                                    tolower(input$edu_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador edu r.png", width = 40, height = 25, units = "cm")
        
        
      } else {
        
        edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor",
                                  fill = edu_r_corte_var)) +
          geom_col(position = "dodge", alpha = .8, stroke = 1, color = "black") +
          geom_text(aes_string(group = edu_r_corte_var, label = "Valor"),
                    position = position_dodge2(width = .9), vjust = -.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_r,
                                    "según",
                                    tolower(input$edu_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador edu r.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$indicador_edu_r %in% lista_ind_2) {
      
      req(input$edu_r_corte, input$edu_r_corte_2,
          input$fecha_edu_r, input$checkbox_edu_r)
      
      edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
      edu_r_corte_var_2 <- rlang::sym(to_varname(input$edu_r_corte_2))
      
      dat_plot <- dat_edu_r() %>%
        filter(ano >= input$fecha_edu_r[1] &
                 ano <= input$fecha_edu_r[2]) %>%
        filter(corte == input$edu_r_corte) %>%
        filter(!!edu_r_corte_var_2 %in% input$checkbox_edu_r_2) %>%
        filter(!!edu_r_corte_var %in% input$checkbox_edu_r) %>% 
        filter(corte_2 == input$edu_r_corte_2)
      
      if(input$edu_r_corte_2 == "Total"){
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == "Total")
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = edu_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_r,
                                    "según",
                                    tolower(input$edu_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
      } else if(input$edu_r_corte_2 != "Total") {
        
        edu_r_corte_var_2 <- rlang::sym(to_varname(input$edu_r_corte_2))
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == input$edu_r_corte_2)
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = edu_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          scale_x_continuous(breaks = int_breaks) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_r,
                                    "según",
                                    tolower(input$edu_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          facet_wrap(as.formula(paste("~", edu_r_corte_var_2)))
        
      }
      
      print(plot)
      ggsave("www/indicador edu r.png", width = 40, height = 25, units = "cm")
      
      
      
      # Grafico simple normal
    } else if(input$edu_r_corte == "Total") {
      
      req(input$indicador_edu_r, input$fecha_edu_r)
      
      dat_plot <- dat_edu_r() %>%
        filter(ano >= input$fecha_edu_r[1] &
                 ano <= input$fecha_edu_r[2]) %>%
        filter(corte == "Total")
      
      if(input$indicador_edu_r %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor, alpha = metodo)) +
          geom_line(size = 1, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          # scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "none") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_edu_r),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8))
        
        print(plot)
        ggsave("www/indicador edu r.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_edu_r),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador edu r.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$edu_r_corte == "Departamento" &
              input$indicador_edu_r %notin% lista_ind_2 ) {
      
      req(input$indicador_edu_r, input$fecha_dpto_edu_r)
      
      dat_plot <- dat_edu_r() %>%
        filter(corte == "Departamento") %>%
        filter(ano == input$fecha_dpto_edu_r) %>%
        select(departamento, Valor, fuente, cita)
      
      dep_j <- dep %>%
        left_join(dat_plot, by = c("nombre" = "departamento"))
      
      plot <-  ggplot(dep_j, aes(fill = Valor)) +
        geom_sf() +
        geom_sf_text(aes(label = Valor), colour = "black",
                     size = 4, fontface = "bold")+
        viridis::scale_fill_viridis(name = "", direction = -1)+
        labs(x = "",
             y = "",
        )+
        theme_bdd(base_size = 14) +
        theme(axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.grid.major = element_line(colour = "transparent"),
        ) +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_edu_r,
                                  "en",
                                  input$fecha_dpto_edu_r), w = 80),
             caption = wrapit(unique(dat_plot$cita), w = 80))
      
      print(plot)
      ggsave("www/indicador edu r.png", width = 40, height = 25, units = "cm")
      
      
    } else if(input$edu_r_corte != "Total") {
      
      req(input$edu_r_corte, input$indicador_edu_r,
          input$fecha_edu_r, input$checkbox_edu_r)
      
      dat_plot <- dat_edu_r() %>%
        filter(ano >= input$fecha_edu_r[1] &
                 ano <= input$fecha_edu_r[2]) %>%
        filter(corte == input$edu_r_corte) %>%
        janitor::remove_empty("cols")
      
      edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
      
      dat_plot <- filter(dat_plot,
                         !!edu_r_corte_var %in% input$checkbox_edu_r)
      
      if(input$indicador_edu_r %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = edu_r_corte_var, alpha = "metodo")) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_r,
                                    "según",
                                    tolower(input$edu_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8)) +
          guides(alpha = "none")
        
        print(plot)
        ggsave("www/indicador edu r.png", width = 40, height = 25, units = "cm")      
     
      } else {
        
        edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
        
        dat_plot <- dat_edu_r() %>%
          filter(corte == input$edu_r_corte) %>%
          filter(!!edu_r_corte_var %in% input$checkbox_edu_r)
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            colour = edu_r_corte_var, group = edu_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_r,
                                    "según",
                                    tolower(input$edu_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador edu r.png", width = 40, height = 25, units = "cm")
        
      }
    }
  })
  
  
  # * Descarga gráficos   =============================================
  
  output$baja_p_edu_r <- downloadHandler(
    filename <- function() {
      paste("indicador edu r", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/indicador edu r.png", file)
    },
    contentType = "www/indicador edu r"
  )
  
  
  # * Tablas   ========================================================
  
  # Data
  edu_r_tab <- reactive({
    
    if(input$indicador_edu_r %in%  lista_especial){
      
      edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
      edu_r_corte_var_2 <- rlang::sym(to_varname(input$edu_r_corte_2))
      
      dat_edu_r() %>%
        filter(corte_2 == input$edu_r_corte_2) %>% 
        select(Fecha, edu_r_corte_var, edu_r_corte_var_2, Valor) %>%
        arrange(desc(Fecha)) %>%
        pivot_wider(values_from = "Valor",
                    names_from = edu_r_corte_var) 
      
    } else if(input$indicador_edu_r %in% lista_vunico & input$edu_r_corte == "Total"){
      
      req(input$edu_r_corte, input$indicador_edu_r)
      
      dat_edu_r() %>%
        filter(corte == "Total") %>% 
        select(fecha_cat, Valor) %>%
        arrange(desc(fecha_cat)) %>%
        rename(Fecha = fecha_cat)
      
    } else if(input$indicador_edu_r %in% lista_vunico & input$edu_r_corte != "Total") {
      
      req(input$edu_r_corte, input$indicador_edu_r)
      
      edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
      
      dat_cut <- dat_edu_r() %>%
        filter(corte == input$edu_r_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        select(fecha_cat, edu_r_corte_var, Valor) %>%
        arrange(desc(fecha_cat)) %>% 
        rename(Fecha = fecha_cat) %>%
        pivot_wider(values_from = "Valor",
                    names_from = edu_r_corte_var)
      
      
    } else if(input$indicador_edu_r %in% lista_ind_2) {
      
      req(input$edu_r_corte, input$indicador_edu_r, input$fecha_edu_r)
      
      edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
      
      dat_cut <- dat_edu_r() %>%
        filter(corte == input$edu_r_corte) %>%
        filter(!!edu_r_corte_var %in% input$checkbox_edu_r)
      
      if(input$edu_r_corte_2 == "Total"){
        
        dat_cut %>%
          filter(ano >= input$fecha_edu_r[1] &
                   ano <= input$fecha_edu_r[2]) %>%
          filter(corte_2 == "Total") %>% 
          select(Fecha, edu_r_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = edu_r_corte_var)
        
      } else if(input$edu_r_corte_2 != "Total") {
        
        edu_r_corte_var_2 <- rlang::sym(to_varname(input$edu_r_corte_2))
        
        dat_cut %>%
          filter(ano >= input$fecha_edu_r[1] &
                   ano <= input$fecha_edu_r[2]) %>%
          filter(corte_2 == input$edu_r_corte_2) %>% 
          select(Fecha, edu_r_corte_var, edu_r_corte_var_2, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = edu_r_corte_var) 
        
      }
      
    } else if(input$edu_r_corte == "Total") {
      
      req(input$edu_r_corte, input$indicador_edu_r, input$fecha_edu_r)
      
      dat_edu_r() %>%
        filter(corte == "Total") %>% 
        filter(ano >= input$fecha_edu_r[1] &
                 ano <= input$fecha_edu_r[2]) %>%
        select(Fecha, Valor) %>%
        arrange(desc(Fecha))
      
    } else if(input$edu_r_corte != "Total") {
      
      req(input$edu_r_corte, input$indicador_edu_r, input$fecha_edu_r)
      
      edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
      
      dat_cut <- dat_edu_r() %>%
        filter(corte == input$edu_r_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        filter(ano >= input$fecha_edu_r[1] &
                 ano <= input$fecha_edu_r[2]) %>% 
        select(Fecha, edu_r_corte_var, Valor) %>%
        arrange(desc(Fecha)) %>% 
        pivot_wider(values_from = "Valor",
                    names_from = edu_r_corte_var)
      
    }
  })
  
  # Metadata 
  edu_r_meta <- reactive({
    
    dat_edu_r() %>%
      select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, observaciones, actualizacion, cita) %>% 
      mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
      distinct() %>% 
      gather(key = " ", value = " ")
    
  })
  
  # Excel
  list_edu_r <- reactive({
    list_edu_r <- list("Data" = edu_r_tab(),
                       "Metadata" = edu_r_meta())
  })
  
  # Render
  output$table_edu_r <- renderDT({
    
    DT::datatable(edu_r_tab(),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    input$indicador_edu_r,
                    style = "color:black; font-size:110%;")
    ) 
    
  })
  
  # * Descarga tablas   ================================================
  
  output$dwl_tab_edu_r <- downloadHandler(
    
    filename = function() {
      paste("resultados-", input$indicador_edu_r, ".xlsx", sep = "")
    },
    content = function(file) {
      
      openxlsx::write.xlsx(list_edu_r(), file)
      
    }
  )
  
  
  
  ### 4.1. Salud Políticas ==============================================
  
  # * Data reactiva   =================================================    
  dat_salud_pp <- reactive({
    
    req(input$indicador_salud_pp)
    
    dat %>%
      filter(nomindicador == input$indicador_salud_pp) 
    
  })
  
  output$selector_salud_pp_corte_2 <- renderUI({
    
    if(input$indicador_salud_pp %in% lista_ind_2){
      
      selectInput(
        inputId = "salud_pp_corte_2",
        label = "Seleccione primer corte:",
        choices = dat_salud_pp() %>% 
          select(corte_2) %>%
          arrange(corte_2) %>% 
          unique() %>% 
          pull(),
        selected = dat_salud_pp() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte_2) %>% 
          pull()
      )
      
    } else {
      
      NULL
    }
    
  })
  
  output$chbox_salud_pp_2 <- renderUI({
    
    if(input$indicador_salud_pp %in% lista_ind_2 & input$indicador_salud_pp %notin% lista_especial){
      
      # if(input$salud_pp_corte %notin% c("Total", "Departamento") & input$indicador_salud_pp %notin% lista_vunico) {
      
      salud_pp_corte_var_2 <- rlang::sym(to_varname(input$salud_pp_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_salud_pp_2",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_salud_pp() %>%
                           filter(corte_2 == input$salud_pp_corte_2) %>% 
                           distinct(!!salud_pp_corte_var_2) %>%
                           pull(),
                         selected = dat_salud_pp() %>%
                           filter(corte_2 == input$salud_pp_corte_2) %>% 
                           filter(jerarquia_cat_2 == "1") %>%
                           distinct(!!salud_pp_corte_var_2) %>%
                           pull()
      )
      
      # } else {
      #   
      #   return(NULL)
      #   
      #   }
      
    } else {
      
      return(NULL)
      
    }      
  })
  
  
  output$selector_salud_pp_corte <- renderUI({
    
    selectInput(
      inputId = "salud_pp_corte",
      label = "Seleccione corte:",
      choices = dat_salud_pp() %>% 
        select(corte) %>%
        arrange(corte) %>% 
        unique() %>% 
        pull(),
      selected = dat_salud_pp() %>% 
        filter(jerarquia == "1") %>%  
        distinct(corte) %>% 
        pull()
    )
    
  })
  
  output$chbox_salud_pp <- renderUI({
    
    if(input$salud_pp_corte %in% lista_ind_2 & input$salud_pp_corte %notin% c("Total", "Departamento") & input$indicador_salud_pp %notin% lista_vunico) {
      
      salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
      salud_pp_corte_var_2 <- rlang::sym(to_varname(input$salud_pp_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_salud_pp",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_salud_pp() %>%
                           filter(!!salud_pp_corte_var_2 %in% input$checkbox_salud_pp_2) %>%
                           filter(corte == input$salud_pp_corte) %>% 
                           distinct(!!salud_pp_corte_var) %>%
                           pull(),
                         selected = dat_salud_pp() %>%
                           filter(!!salud_pp_corte_var_2 %in% input$checkbox_salud_pp_2) %>%
                           filter(corte == input$salud_pp_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!salud_pp_corte_var) %>%
                           pull()
      )
      
    } else if(input$salud_pp_corte %notin% lista_ind_2 & input$salud_pp_corte %notin% c("Total", "Departamento") & input$indicador_salud_pp %notin% lista_vunico) {
      
      salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
      
      checkboxGroupInput(inputId = "checkbox_salud_pp",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_salud_pp() %>%
                           filter(corte == input$salud_pp_corte) %>% 
                           distinct(!!salud_pp_corte_var) %>%
                           pull(),
                         selected = dat_salud_pp() %>%
                           filter(corte == input$salud_pp_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!salud_pp_corte_var) %>%
                           pull()
      )
      
    } else {
      
      return(NULL)
    }
    
  })
  
  # Selector de fecha
  output$s_salud_pp_fecha <- renderUI({
    
    if(input$salud_pp_corte == "Departamento" & input$indicador_salud_pp %notin% lista_ind_2) {
      
      req(input$salud_pp_corte, input$indicador_salud_pp)
      
      req(nrow(dat_salud_pp()) > 0)
      
      selectInput(
        inputId = "fecha_dpto_salud_pp",
        label = "Seleccione año:",
        choices = dat_salud_pp() %>% 
          filter(nomindicador == input$indicador_salud_pp) %>%
          drop_na(Valor) %>%
          select(ano) %>%
          arrange(desc(ano)) %>% 
          unique() %>% 
          pull(),
        selected = max(input$ano)
      )
      
    } else if (input$indicador_salud_pp %in% lista_serie_cat){
      
      return(NULL)
      
      
    } else if (input$indicador_salud_pp %in% lista_vunico){
      
      return(NULL)
      
    } else  {
      
      req(input$salud_pp_corte, input$indicador_salud_pp)
      req(nrow(dat_salud_pp()) > 0)
      
      tagList(
        # tags$style(type = 'text/css', 
        #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
        # div(id = 'big_slider',
        
        sliderInput("fecha_salud_pp", 
                    label = "Rango de tiempo", 
                    sep = "",
                    dragRange = T,
                    min = min(dat_salud_pp()$ano), 
                    max = max(dat_salud_pp()$ano), 
                    value = c(min(dat_salud_pp()$ano), 
                              max(dat_salud_pp()$ano))
        )
      )
      
    }
  })
  
  # # Selector de corte según categoría y data temporal
  # dat_salud_pp <- reactive({
  #   
  #   req(input$salud_pp_corte)
  #   
  #   if(input$indicador_salud_pp %in% lista_ind_2){
  #     
  #     dat_salarios <- dat_salud_pp() %>%
  #       filter(corte_2 == input$salud_pp_corte_2) %>% 
  #       filter(corte == input$salud_pp_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   } else {
  #     
  #     dat_salud_pp() %>%
  #       filter(corte == input$salud_pp_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   }
  # })
  
  # * Metadata   ======================================================
  
  # Title
  output$title_salud_pp <- renderUI({ 
    helpText(HTML(unique(dat_salud_pp()$nomindicador)))
  })
  
  # Subtitle
  output$subtitle_salud_pp <- renderUI({ 
    helpText(HTML(unique(dat_salud_pp()$definicion)))
  })
  
  # Nombre conceptual
  output$conindicador_salud_pp <- renderUI({ 
    helpText(HTML(paste("<b> Nombre conceptual:</b>", unique(dat_salud_pp()$conindicador))))
  })
  
  # Calculo
  output$calculo_salud_pp <- renderUI({ 
    helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_salud_pp()$calculo))))
  })
  
  # Observaciones
  output$observacion_salud_pp <- renderUI({ 
    helpText(HTML(paste("<b> Observaciones:</b>", unique(dat_salud_pp()$observaciones))))
  })
  
  # Actualización
  output$actualizacion_salud_pp <- renderUI({ 
    helpText(HTML(paste("<b> Última actualización:</b>", unique(dat_salud_pp()$actualizacion))))
  })
  
  
  # * Gráficos   ======================================================
  
  output$plot_salud_pp <- renderPlot({
    
    req(input$indicador_salud_pp, input$salud_pp_corte)
    
    if(input$indicador_salud_pp %in% lista_serie_cat){
      
      req(input$indicador_salud_pp)
      
      # Total
      if(input$salud_pp_corte == "Total"){
        
        dat_plot <- dat_salud_pp() %>%
          filter(corte == "Total")
        
        plot <- ggplot(dat_plot, aes(x = fecha_cat, y = Valor, group = 1)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_salud_pp),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador salud pp.png", width = 40, height = 25, units = "cm")
        
        # Según corte
      } else if(input$salud_pp_corte != "Total") {
        
        salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
        
        dat_plot <- dat_salud_pp() %>%
          filter(corte == input$salud_pp_corte) %>%
          filter(!!salud_pp_corte_var %in% input$checkbox_salud_pp)
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            colour = salud_pp_corte_var, group = salud_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_pp,
                                    "según",
                                    tolower(input$salud_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador salud pp.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_salud_pp %in% lista_especial ){
      
      if (input$salud_pp_corte_2 == "Total"){        
        
        req(input$salud_pp_corte, input$indicador_salud_pp)
        
        salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
        
        dat_plot <- dat_salud_pp() %>%
          filter(ano >= input$fecha_salud_pp[1] &
                   ano <= input$fecha_salud_pp[2]) %>%
          filter(corte == input$salud_pp_corte) %>%
          # filter(!!salud_pp_corte_var %in% input$checkbox_salud_pp) %>% 
          filter(corte_2 == input$salud_pp_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = salud_pp_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_pp,
                                    "según",
                                    tolower(input$salud_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador salud pp.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        req(input$salud_pp_corte, input$indicador_salud_pp)
        
        salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
        salud_pp_corte_var_2 <- rlang::sym(to_varname(input$salud_pp_corte_2))
        
        dat_plot <- dat_salud_pp() %>%
          filter(ano >= input$fecha_salud_pp[1] &
                   ano <= input$fecha_salud_pp[2]) %>%
          filter(corte == input$salud_pp_corte) %>%
          # filter(!!salud_pp_corte_var %in% input$checkbox_salud_pp) %>% 
          filter(corte_2 == input$salud_pp_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = salud_pp_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_pp,
                                    "según",
                                    tolower(input$salud_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", salud_pp_corte_var_2)))
        
        print(plot)
        ggsave("www/indicador salud pp.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_salud_pp %in% lista_vunico & input$indicador_salud_pp %in% lista_ind_2){
      
      req(input$salud_pp_corte, input$indicador_salud_pp)
      
      salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
      salud_pp_corte_var_2 <- rlang::sym(to_varname(input$salud_pp_corte_2))
      
      dat_plot <- dat_salud_pp() %>%
        filter(ano >= input$fecha_salud_pp[1] &
                 ano <= input$fecha_salud_pp[2]) %>%
        filter(corte == input$salud_pp_corte) %>%
        filter(!!salud_pp_corte_var %in% input$checkbox_salud_pp) %>%
        filter(!!salud_pp_corte_var_2 %in% input$checkbox_salud_pp_2) %>%
        filter(corte_2 == input$salud_pp_corte_2)
      
      plot <- ggplot(dat_plot,
                     aes_string(x = "fecha_cat", y = "Valor",
                                fill = salud_pp_corte_var)) +
        geom_col(position = "dodge", width = .7, alpha = .8) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x=element_blank(),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_salud_pp,
                                  "según",
                                  tolower(input$salud_pp_corte),
                                  "en",
                                  unique(dat_plot$fecha_cat))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_fill_brewer(name = "", palette = "Paired") +
        facet_wrap(as.formula(paste("~", salud_pp_corte_var_2)))
      
      print(plot)
      ggsave("www/indicador salud pp.png", width = 40, height = 25, units = "cm")
      
    } else if(input$indicador_salud_pp %in% lista_vunico & input$salud_pp_corte != "Departamento") {
      
      req(input$salud_pp_corte, input$indicador_salud_pp)
      
      dat_plot <- dat_salud_pp() %>%
        filter(corte == input$salud_pp_corte) %>%
        janitor::remove_empty("cols")
      
      if(input$salud_pp_corte == "Total"){
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor")) +
          geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
          geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_pp,
                                    "según",
                                    tolower(input$salud_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador salud pp.png", width = 40, height = 25, units = "cm")
        
        
      } else {
        
        salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor",
                                  fill = salud_pp_corte_var)) +
          geom_col(position = "dodge", alpha = .8, stroke = 1, color = "black") +
          geom_text(aes_string(group = salud_pp_corte_var, label = "Valor"),
                    position = position_dodge2(width = .9), vjust = -.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_pp,
                                    "según",
                                    tolower(input$salud_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador salud pp.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$indicador_salud_pp %in% lista_ind_2) {
      
      req(input$salud_pp_corte, input$salud_pp_corte_2,
          input$fecha_salud_pp, input$checkbox_salud_pp)
      
      salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
      salud_pp_corte_var_2 <- rlang::sym(to_varname(input$salud_pp_corte_2))
      
      dat_plot <- dat_salud_pp() %>%
        filter(ano >= input$fecha_salud_pp[1] &
                 ano <= input$fecha_salud_pp[2]) %>%
        filter(corte == input$salud_pp_corte) %>%
        filter(!!salud_pp_corte_var_2 %in% input$checkbox_salud_pp_2) %>%
        filter(!!salud_pp_corte_var %in% input$checkbox_salud_pp) %>% 
        filter(corte_2 == input$salud_pp_corte_2)
      
      if(input$salud_pp_corte_2 == "Total"){
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == "Total")
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = salud_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_pp,
                                    "según",
                                    tolower(input$salud_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
      } else if(input$salud_pp_corte_2 != "Total") {
        
        salud_pp_corte_var_2 <- rlang::sym(to_varname(input$salud_pp_corte_2))
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == input$salud_pp_corte_2)
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = salud_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          scale_x_continuous(breaks = int_breaks) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_pp,
                                    "según",
                                    tolower(input$salud_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          facet_wrap(as.formula(paste("~", salud_pp_corte_var_2)))
        
      }
      
      print(plot)
      ggsave("www/indicador salud pp.png", width = 40, height = 25, units = "cm")
      
      
      # Grafico simple normal
    } else if(input$salud_pp_corte == "Total") {
      
      req(input$indicador_salud_pp, input$fecha_salud_pp)
      
      dat_plot <- dat_salud_pp() %>%
        filter(ano >= input$fecha_salud_pp[1] &
                 ano <= input$fecha_salud_pp[2]) %>%
        filter(corte == "Total")
      
      if(input$indicador_salud_pp %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor, alpha = metodo)) +
          geom_line(size = 1, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          # scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "none") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_salud_pp),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8))
        
        print(plot)
        ggsave("www/indicador salud pp.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_salud_pp),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador salud pp.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$salud_pp_corte == "Departamento" &
              input$indicador_salud_pp %notin% lista_ind_2 ) {
      
      req(input$indicador_salud_pp, input$fecha_dpto_salud_pp)
      
      dat_plot <- dat_salud_pp() %>%
        filter(corte == "Departamento") %>%
        filter(ano == input$fecha_dpto_salud_pp) %>%
        select(departamento, Valor, fuente, cita)
      
      dep_j <- dep %>%
        left_join(dat_plot, by = c("nombre" = "departamento"))
      
      plot <-  ggplot(dep_j, aes(fill = Valor)) +
        geom_sf() +
        geom_sf_text(aes(label = Valor), colour = "black",
                     size = 4, fontface = "bold")+
        viridis::scale_fill_viridis(name = "", direction = -1)+
        labs(x = "",
             y = "",
        )+
        theme_bdd(base_size = 14) +
        theme(axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.grid.major = element_line(colour = "transparent"),
        ) +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_salud_pp,
                                  "en",
                                  input$fecha_dpto_salud_pp), w = 80),
             caption = wrapit(unique(dat_plot$cita), w = 80))
      
      print(plot)
      ggsave("www/indicador salud pp.png", width = 40, height = 25, units = "cm")
      
      
    } else if(input$salud_pp_corte != "Total") {
      
      req(input$salud_pp_corte, input$indicador_salud_pp,
          input$fecha_salud_pp, input$checkbox_salud_pp)
      
      dat_plot <- dat_salud_pp() %>%
        filter(ano >= input$fecha_salud_pp[1] &
                 ano <= input$fecha_salud_pp[2]) %>%
        filter(corte == input$salud_pp_corte) %>%
        janitor::remove_empty("cols")
      
      salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
      
      dat_plot <- filter(dat_plot,
                         !!salud_pp_corte_var %in% input$checkbox_salud_pp)
      
      if(input$indicador_salud_pp %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = salud_pp_corte_var, alpha = "metodo")) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_pp,
                                    "según",
                                    tolower(input$salud_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8)) +
          guides(alpha = "none")
        
        print(plot)
        ggsave("www/indicador salud pp.png", width = 40, height = 25, units = "cm")
        
        
      } else {
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = salud_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_pp,
                                    "según",
                                    tolower(input$salud_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador salud pp.png", width = 40, height = 25, units = "cm")
        
        
      }
      
      
    }
    
  })
  
  
  # * Descarga gráficos   =============================================
  
  output$baja_p_salud_pp <- downloadHandler(
    filename <- function() {
      paste("indicador salud pp", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/indicador salud pp.png", file)
    },
    contentType = "www/indicador salud pp"
  )
  
  
  # * Tablas   ========================================================
  
  # Data
  salud_pp_tab <- reactive({
    
    if(input$indicador_salud_pp %in%  lista_especial){
      
      salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
      salud_pp_corte_var_2 <- rlang::sym(to_varname(input$salud_pp_corte_2))
      
      dat_salud_pp() %>%
        filter(corte_2 == input$salud_pp_corte_2) %>% 
        select(Fecha, salud_pp_corte_var, salud_pp_corte_var_2, Valor) %>%
        arrange(desc(Fecha)) %>%
        pivot_wider(values_from = "Valor",
                    names_from = salud_pp_corte_var) 
      
    } else if(input$indicador_salud_pp %in% lista_vunico & input$salud_pp_corte == "Total"){
      
      req(input$salud_pp_corte, input$indicador_salud_pp)
      
      dat_salud_pp() %>%
        filter(corte == "Total") %>% 
        select(fecha_cat, Valor) %>%
        arrange(desc(fecha_cat)) %>%
        rename(Fecha = fecha_cat)
      
    } else if(input$indicador_salud_pp %in% lista_vunico & input$salud_pp_corte != "Total") {
      
      req(input$salud_pp_corte, input$indicador_salud_pp)
      
      salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
      
      dat_cut <- dat_salud_pp() %>%
        filter(corte == input$salud_pp_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        select(fecha_cat, salud_pp_corte_var, Valor) %>%
        arrange(desc(fecha_cat)) %>% 
        rename(Fecha = fecha_cat) %>%
        pivot_wider(values_from = "Valor",
                    names_from = salud_pp_corte_var)
      
      
    } else if(input$indicador_salud_pp %in% lista_ind_2) {
      
      req(input$salud_pp_corte, input$indicador_salud_pp, input$fecha_salud_pp)
      
      salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
      
      dat_cut <- dat_salud_pp() %>%
        filter(corte == input$salud_pp_corte) %>%
        filter(!!salud_pp_corte_var %in% input$checkbox_salud_pp)
      
      if(input$salud_pp_corte_2 == "Total"){
        
        dat_cut %>%
          filter(ano >= input$fecha_salud_pp[1] &
                   ano <= input$fecha_salud_pp[2]) %>%
          filter(corte_2 == "Total") %>% 
          select(Fecha, salud_pp_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = salud_pp_corte_var)
        
      } else if(input$salud_pp_corte_2 != "Total") {
        
        salud_pp_corte_var_2 <- rlang::sym(to_varname(input$salud_pp_corte_2))
        
        dat_cut %>%
          filter(ano >= input$fecha_salud_pp[1] &
                   ano <= input$fecha_salud_pp[2]) %>%
          filter(corte_2 == input$salud_pp_corte_2) %>% 
          select(Fecha, salud_pp_corte_var, salud_pp_corte_var_2, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = salud_pp_corte_var) 
        
      }
      
    } else if(input$salud_pp_corte == "Total") {
      
      req(input$salud_pp_corte, input$indicador_salud_pp, input$fecha_salud_pp)
      
      dat_salud_pp() %>%
        filter(corte == "Total") %>% 
        filter(ano >= input$fecha_salud_pp[1] &
                 ano <= input$fecha_salud_pp[2]) %>%
        select(Fecha, Valor) %>%
        arrange(desc(Fecha))
      
    } else if(input$salud_pp_corte != "Total") {
      
      req(input$salud_pp_corte, input$indicador_salud_pp, input$fecha_salud_pp)
      
      salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
      
      dat_cut <- dat_salud_pp() %>%
        filter(corte == input$salud_pp_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        filter(ano >= input$fecha_salud_pp[1] &
                 ano <= input$fecha_salud_pp[2]) %>% 
        select(Fecha, salud_pp_corte_var, Valor) %>%
        arrange(desc(Fecha)) %>% 
        pivot_wider(values_from = "Valor",
                    names_from = salud_pp_corte_var)
      
    }
  })
  
  # Metadata 
  salud_pp_meta <- reactive({
    
    dat_salud_pp() %>%
      select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, observaciones, actualizacion, cita) %>% 
      mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
      distinct() %>% 
      gather(key = " ", value = " ")
    
  })
  
  # Excel
  list_salud_pp <- reactive({
    list_salud_pp <- list("Data" = salud_pp_tab(),
                          "Metadata" = salud_pp_meta())
  })
  
  # Render
  output$table_salud_pp <- renderDT({
    
    DT::datatable(salud_pp_tab(),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    input$indicador_salud_pp,
                    style = "color:black; font-size:110%;")
    ) 
    
  })
  
  # * Descarga tablas   ================================================
  
  output$dwl_tab_salud_pp <- downloadHandler(
    
    filename = function() {
      paste("resultados-", input$indicador_salud_pp, ".xlsx", sep = "")
    },
    content = function(file) {
      
      openxlsx::write.xlsx(list_salud_pp(), file)
      
    }
  )
  
  
  
  ### 4.2. Salud Resultados ==============================================
  
  # * Data reactiva   =================================================    
  dat_salud_r <- reactive({
    
    req(input$indicador_salud_r)
    
    dat %>%
      filter(nomindicador == input$indicador_salud_r) 
    
  })
  
  output$selector_salud_r_corte_2 <- renderUI({
    
    if(input$indicador_salud_r %in% lista_ind_2){
      
      selectInput(
        inputId = "salud_r_corte_2",
        label = "Seleccione primer corte:",
        choices = dat_salud_r() %>% 
          select(corte_2) %>%
          arrange(corte_2) %>% 
          unique() %>% 
          pull(),
        selected = dat_salud_r() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte_2) %>% 
          pull()
      )
      
    } else {
      
      NULL
    }
    
  })
  
  output$chbox_salud_r_2 <- renderUI({
    
    if(input$indicador_salud_r %in% lista_ind_2 & input$indicador_salud_r %notin% lista_especial){
      
      # if(input$salud_r_corte %notin% c("Total", "Departamento") & input$indicador_salud_r %notin% lista_vunico) {
      
      salud_r_corte_var_2 <- rlang::sym(to_varname(input$salud_r_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_salud_r_2",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_salud_r() %>%
                           filter(corte_2 == input$salud_r_corte_2) %>% 
                           distinct(!!salud_r_corte_var_2) %>%
                           pull(),
                         selected = dat_salud_r() %>%
                           filter(corte_2 == input$salud_r_corte_2) %>% 
                           filter(jerarquia_cat_2 == "1") %>%
                           distinct(!!salud_r_corte_var_2) %>%
                           pull()
      )
      
      # } else {
      #   
      #   return(NULL)
      #   
      #   }
      
    } else {
      
      return(NULL)
      
    }      
  })
  
  
  output$selector_salud_r_corte <- renderUI({
    
    selectInput(
      inputId = "salud_r_corte",
      label = "Seleccione corte:",
      choices = dat_salud_r() %>% 
        select(corte) %>%
        arrange(corte) %>% 
        unique() %>% 
        pull(),
      selected = dat_salud_r() %>% 
        filter(jerarquia == "1") %>%  
        distinct(corte) %>% 
        pull()
    )
    
  })
  
  output$chbox_salud_r <- renderUI({
    
    if(input$salud_r_corte %in% lista_ind_2 & input$salud_r_corte %notin% c("Total", "Departamento") & input$indicador_salud_r %notin% lista_vunico) {
      
      salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
      salud_r_corte_var_2 <- rlang::sym(to_varname(input$salud_r_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_salud_r",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_salud_r() %>%
                           filter(!!salud_r_corte_var_2 %in% input$checkbox_salud_r_2) %>%
                           filter(corte == input$salud_r_corte) %>% 
                           distinct(!!salud_r_corte_var) %>%
                           pull(),
                         selected = dat_salud_r() %>%
                           filter(!!salud_r_corte_var_2 %in% input$checkbox_salud_r_2) %>%
                           filter(corte == input$salud_r_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!salud_r_corte_var) %>%
                           pull()
      )
      
    } else if(input$salud_r_corte %notin% lista_ind_2 & input$salud_r_corte %notin% c("Total", "Departamento") & input$indicador_salud_r %notin% lista_vunico) {
      
      salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
      
      checkboxGroupInput(inputId = "checkbox_salud_r",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_salud_r() %>%
                           filter(corte == input$salud_r_corte) %>% 
                           distinct(!!salud_r_corte_var) %>%
                           pull(),
                         selected = dat_salud_r() %>%
                           filter(corte == input$salud_r_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!salud_r_corte_var) %>%
                           pull()
      )
      
    } else {
      
      return(NULL)
    }
    
  })
  
  # Selector de fecha
  output$s_salud_r_fecha <- renderUI({
    
    if(input$salud_r_corte == "Departamento" & input$indicador_salud_r %notin% lista_ind_2) {
      
      req(input$salud_r_corte, input$indicador_salud_r)
      
      req(nrow(dat_salud_r()) > 0)
      
      selectInput(
        inputId = "fecha_dpto_salud_r",
        label = "Seleccione año:",
        choices = dat_salud_r() %>% 
          filter(nomindicador == input$indicador_salud_r) %>%
          drop_na(Valor) %>%
          select(ano) %>%
          arrange(desc(ano)) %>% 
          unique() %>% 
          pull(),
        selected = max(input$ano)
      )
      
    } else if (input$indicador_salud_r %in% lista_serie_cat){
      
      return(NULL)
      
      
    } else if (input$indicador_salud_r %in% lista_vunico){
      
      return(NULL)
      
    } else  {
      
      req(input$salud_r_corte, input$indicador_salud_r)
      req(nrow(dat_salud_r()) > 0)
      
      tagList(
        # tags$style(type = 'text/css', 
        #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
        # div(id = 'big_slider',
        
        sliderInput("fecha_salud_r", 
                    label = "Rango de tiempo", 
                    sep = "",
                    dragRange = T,
                    min = min(dat_salud_r()$ano), 
                    max = max(dat_salud_r()$ano), 
                    value = c(min(dat_salud_r()$ano), 
                              max(dat_salud_r()$ano))
        )
      )
      
    }
  })
  
  # # Selector de corte según categoría y data temporal
  # dat_salud_r <- reactive({
  #   
  #   req(input$salud_r_corte)
  #   
  #   if(input$indicador_salud_r %in% lista_ind_2){
  #     
  #     dat_salarios <- dat_salud_r() %>%
  #       filter(corte_2 == input$salud_r_corte_2) %>% 
  #       filter(corte == input$salud_r_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   } else {
  #     
  #     dat_salud_r() %>%
  #       filter(corte == input$salud_r_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   }
  # })
  
  # * Metadata   ======================================================
  
  # Title
  output$title_salud_r <- renderUI({ 
    helpText(HTML(unique(dat_salud_r()$nomindicador)))
  })
  
  # Subtitle
  output$subtitle_salud_r <- renderUI({ 
    helpText(HTML(unique(dat_salud_r()$definicion)))
  })
  
  # Nota
  output$nota_salud_r <- renderUI({ 
    
    if(input$indicador_salud_r %in% lista_nota){
      helpText(HTML(unique(dat_salud_r()$nota)))
      
    } else(NULL)
  })
  
  # Nombre conceptual
  output$conindicador_salud_r <- renderUI({ 
    helpText(HTML(paste("<b> Nombre conceptual:</b>", unique(dat_salud_r()$conindicador))))
  })
  
  # Calculo
  output$calculo_salud_r <- renderUI({ 
    helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_salud_r()$calculo))))
  })
  
  # Observaciones
  output$observacion_salud_r <- renderUI({ 
    helpText(HTML(paste("<b> Observaciones:</b>", unique(dat_salud_r()$observaciones))))
  })
  
  # Actualización
  output$actualizacion_salud_r <- renderUI({ 
    helpText(HTML(paste("<b> Última actualización:</b>", unique(dat_salud_r()$actualizacion))))
  })
  
  
  # * Gráficos   ======================================================
  
  output$plot_salud_r <- renderPlot({
    
    req(input$indicador_salud_r, input$salud_r_corte)
    
    if(input$indicador_salud_r %in% lista_serie_cat){
      
      req(input$indicador_salud_r)
      
      # Total
      if(input$salud_r_corte == "Total"){
        
        dat_plot <- dat_salud_r() %>%
          filter(corte == "Total")
        
        plot <- ggplot(dat_plot, aes(x = fecha_cat, y = Valor, group = 1)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_salud_r),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador salud r.png", width = 40, height = 25, units = "cm")
        
        # Según corte
      } else if(input$salud_r_corte != "Total") {
        
        salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
        
        dat_plot <- dat_salud_r() %>%
          filter(corte == input$salud_r_corte) %>%
          filter(!!salud_r_corte_var %in% input$checkbox_salud_r)
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            colour = salud_r_corte_var, group = salud_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_r,
                                    "según",
                                    tolower(input$salud_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador salud r.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_salud_r %in% lista_especial ){
      
      if (input$salud_r_corte_2 == "Total"){        
        
        req(input$salud_r_corte, input$indicador_salud_r)
        
        salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
        
        dat_plot <- dat_salud_r() %>%
          filter(ano >= input$fecha_salud_r[1] &
                   ano <= input$fecha_salud_r[2]) %>%
          filter(corte == input$salud_r_corte) %>%
          # filter(!!salud_r_corte_var %in% input$checkbox_salud_r) %>% 
          filter(corte_2 == input$salud_r_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = salud_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_r,
                                    "según",
                                    tolower(input$salud_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador salud r.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        req(input$salud_r_corte, input$indicador_salud_r)
        
        salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
        salud_r_corte_var_2 <- rlang::sym(to_varname(input$salud_r_corte_2))
        
        dat_plot <- dat_salud_r() %>%
          filter(ano >= input$fecha_salud_r[1] &
                   ano <= input$fecha_salud_r[2]) %>%
          filter(corte == input$salud_r_corte) %>%
          # filter(!!salud_r_corte_var %in% input$checkbox_salud_r) %>% 
          filter(corte_2 == input$salud_r_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = salud_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_r,
                                    "según",
                                    tolower(input$salud_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", salud_r_corte_var_2)))
        
        print(plot)
        ggsave("www/indicador salud r.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_salud_r %in% lista_vunico & input$indicador_salud_r %in% lista_ind_2){
      
      req(input$salud_r_corte, input$indicador_salud_r)
      
      salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
      salud_r_corte_var_2 <- rlang::sym(to_varname(input$salud_r_corte_2))
      
      dat_plot <- dat_salud_r() %>%
        filter(ano >= input$fecha_salud_r[1] &
                 ano <= input$fecha_salud_r[2]) %>%
        filter(corte == input$salud_r_corte) %>%
        filter(!!salud_r_corte_var %in% input$checkbox_salud_r) %>%
        filter(!!salud_r_corte_var_2 %in% input$checkbox_salud_r_2) %>%
        filter(corte_2 == input$salud_r_corte_2)
      
      plot <- ggplot(dat_plot,
                     aes_string(x = "fecha_cat", y = "Valor",
                                fill = salud_r_corte_var)) +
        geom_col(position = "dodge", width = .7, alpha = .8) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x=element_blank(),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_salud_r,
                                  "según",
                                  tolower(input$salud_r_corte),
                                  "en",
                                  unique(dat_plot$fecha_cat))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_fill_brewer(name = "", palette = "Paired") +
        facet_wrap(as.formula(paste("~", salud_r_corte_var_2)))
      
      print(plot)
      ggsave("www/indicador salud r.png", width = 40, height = 25, units = "cm")
      
    } else if(input$indicador_salud_r %in% lista_vunico & input$salud_r_corte != "Departamento") {
      
      req(input$salud_r_corte, input$indicador_salud_r)
      
      dat_plot <- dat_salud_r() %>%
        filter(corte == input$salud_r_corte) %>%
        janitor::remove_empty("cols")
      
      if(input$salud_r_corte == "Total"){
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor")) +
          geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
          geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_r,
                                    "según",
                                    tolower(input$salud_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador salud r.png", width = 40, height = 25, units = "cm")
        
        
      } else {
        
        salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor",
                                  fill = salud_r_corte_var)) +
          geom_col(position = "dodge", alpha = .8, stroke = 1, color = "black") +
          geom_text(aes_string(group = salud_r_corte_var, label = "Valor"),
                    position = position_dodge2(width = .9), vjust = -.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_r,
                                    "según",
                                    tolower(input$salud_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador salud r.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$indicador_salud_r %in% lista_ind_2) {
      
      req(input$salud_r_corte, input$salud_r_corte_2,
          input$fecha_salud_r, input$checkbox_salud_r)
      
      if(input$salud_r_corte_2 == "Total"){
        
        salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
        
        dat_plot <- dat_salud_r() %>%
          filter(ano >= input$fecha_salud_r[1] &
                   ano <= input$fecha_salud_r[2]) %>%
          filter(corte == input$salud_r_corte) %>%
          filter(!!salud_r_corte_var %in% input$checkbox_salud_r) %>% 
          filter(corte_2 == "Total")
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = salud_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_r,
                                    "según",
                                    tolower(input$salud_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
      } else if(input$salud_r_corte_2 != "Total") {
        
        salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
        salud_r_corte_var_2 <- rlang::sym(to_varname(input$salud_r_corte_2))
        
        dat_plot <- dat_salud_r() %>%
          filter(ano >= input$fecha_salud_r[1] &
                   ano <= input$fecha_salud_r[2]) %>%
          filter(corte == input$salud_r_corte) %>%
          filter(!!salud_r_corte_var_2 %in% input$checkbox_salud_r_2) %>%
          filter(!!salud_r_corte_var %in% input$checkbox_salud_r) %>% 
          filter(corte_2 == input$salud_r_corte_2)
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = salud_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          scale_x_continuous(breaks = int_breaks) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_r,
                                    "según",
                                    tolower(input$salud_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          facet_wrap(as.formula(paste("~", salud_r_corte_var_2)))
        
      }
      
      print(plot)
      ggsave("www/indicador salud r.png", width = 40, height = 25, units = "cm")
      
      
      # Grafico simple normal
    } else if(input$salud_r_corte == "Total") {
      
      req(input$indicador_salud_r, input$fecha_salud_r)
      
      dat_plot <- dat_salud_r() %>%
        filter(ano >= input$fecha_salud_r[1] &
                 ano <= input$fecha_salud_r[2]) %>%
        filter(corte == "Total")
      
      if(input$indicador_salud_r %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor, alpha = metodo)) +
          geom_line(size = 1, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          # scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "none") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_salud_r),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8))
        
        print(plot)
        ggsave("www/indicador salud r.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_salud_r),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador salud r.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$salud_r_corte == "Departamento" &
              input$indicador_salud_r %notin% lista_ind_2 ) {
      
      req(input$indicador_salud_r, input$fecha_dpto_salud_r)
      
      dat_plot <- dat_salud_r() %>%
        filter(corte == "Departamento") %>%
        filter(ano == input$fecha_dpto_salud_r) %>%
        select(departamento, Valor, fuente, cita)
      
      dep_j <- dep %>%
        left_join(dat_plot, by = c("nombre" = "departamento"))
      
      plot <-  ggplot(dep_j, aes(fill = Valor)) +
        geom_sf() +
        geom_sf_text(aes(label = Valor), colour = "black",
                     size = 4, fontface = "bold")+
        viridis::scale_fill_viridis(name = "", direction = -1)+
        labs(x = "",
             y = "",
        )+
        theme_bdd(base_size = 14) +
        theme(axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.grid.major = element_line(colour = "transparent"),
        ) +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_salud_r,
                                  "en",
                                  input$fecha_dpto_salud_r), w = 80),
             caption = wrapit(unique(dat_plot$cita), w = 80))
      
      print(plot)
      ggsave("www/indicador salud r.png", width = 40, height = 25, units = "cm")
      
      
    } else if(input$salud_r_corte != "Total") {
      
      req(input$salud_r_corte, input$indicador_salud_r,
          input$fecha_salud_r, input$checkbox_salud_r)
      
      dat_plot <- dat_salud_r() %>%
        filter(ano >= input$fecha_salud_r[1] &
                 ano <= input$fecha_salud_r[2]) %>%
        filter(corte == input$salud_r_corte) %>%
        janitor::remove_empty("cols")
      
      salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
      
      dat_plot <- filter(dat_plot,
                         !!salud_r_corte_var %in% input$checkbox_salud_r)
      
      if(input$indicador_salud_r %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = salud_r_corte_var, alpha = "metodo")) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_r,
                                    "según",
                                    tolower(input$salud_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8)) +
          guides(alpha = "none")
        
        print(plot)
        ggsave("www/indicador salud r.png", width = 40, height = 25, units = "cm")      
        
      } else {
        
        salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
        
        dat_plot <- dat_salud_r() %>%
          filter(corte == input$salud_r_corte) %>%
          filter(!!salud_r_corte_var %in% input$checkbox_salud_r)
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            colour = salud_r_corte_var, group = salud_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_r,
                                    "según",
                                    tolower(input$salud_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador salud r.png", width = 40, height = 25, units = "cm")
        
      }
    }
  })
  
  
  # * Descarga gráficos   =============================================
  
  output$baja_p_salud_r <- downloadHandler(
    filename <- function() {
      paste("indicador salud r", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/indicador salud r.png", file)
    },
    contentType = "www/indicador salud r"
  )
  
  
  # * Tablas   ========================================================
  
  # Data
  salud_r_tab <- reactive({
    
    if(input$indicador_salud_r %in%  lista_especial){
      
      salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
      salud_r_corte_var_2 <- rlang::sym(to_varname(input$salud_r_corte_2))
      
      dat_salud_r() %>%
        filter(corte_2 == input$salud_r_corte_2) %>% 
        select(Fecha, salud_r_corte_var, salud_r_corte_var_2, Valor) %>%
        arrange(desc(Fecha)) %>%
        pivot_wider(values_from = "Valor",
                    names_from = salud_r_corte_var) 
      
    } else if(input$indicador_salud_r %in% lista_vunico & input$salud_r_corte == "Total"){
      
      req(input$salud_r_corte, input$indicador_salud_r)
      
      dat_salud_r() %>%
        filter(corte == "Total") %>% 
        select(fecha_cat, Valor) %>%
        arrange(desc(fecha_cat)) %>%
        rename(Fecha = fecha_cat)
      
    } else if(input$indicador_salud_r %in% lista_vunico & input$salud_r_corte != "Total") {
      
      req(input$salud_r_corte, input$indicador_salud_r)
      
      salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
      
      dat_cut <- dat_salud_r() %>%
        filter(corte == input$salud_r_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        select(fecha_cat, salud_r_corte_var, Valor) %>%
        arrange(desc(fecha_cat)) %>% 
        rename(Fecha = fecha_cat) %>%
        pivot_wider(values_from = "Valor",
                    names_from = salud_r_corte_var)
      
      
    } else if(input$indicador_salud_r %in% lista_ind_2) {
      
      req(input$salud_r_corte, input$indicador_salud_r, input$fecha_salud_r)
      
      salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
      
      dat_cut <- dat_salud_r() %>%
        filter(corte == input$salud_r_corte) %>%
        filter(!!salud_r_corte_var %in% input$checkbox_salud_r)
      
      if(input$salud_r_corte_2 == "Total"){
        
        dat_cut %>%
          filter(ano >= input$fecha_salud_r[1] &
                   ano <= input$fecha_salud_r[2]) %>%
          filter(corte_2 == "Total") %>% 
          select(Fecha, salud_r_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = salud_r_corte_var)
        
      } else if(input$salud_r_corte_2 != "Total") {
        
        salud_r_corte_var_2 <- rlang::sym(to_varname(input$salud_r_corte_2))
        
        dat_cut %>%
          filter(ano >= input$fecha_salud_r[1] &
                   ano <= input$fecha_salud_r[2]) %>%
          filter(corte_2 == input$salud_r_corte_2) %>% 
          select(Fecha, salud_r_corte_var, salud_r_corte_var_2, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = salud_r_corte_var) 
        
      }
      
    } else if(input$salud_r_corte == "Total") {
      
      req(input$salud_r_corte, input$indicador_salud_r, input$fecha_salud_r)
      
      dat_salud_r() %>%
        filter(corte == "Total") %>% 
        filter(ano >= input$fecha_salud_r[1] &
                 ano <= input$fecha_salud_r[2]) %>%
        select(Fecha, Valor) %>%
        arrange(desc(Fecha))
      
    } else if(input$salud_r_corte != "Total") {
      
      req(input$salud_r_corte, input$indicador_salud_r, input$fecha_salud_r)
      
      salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
      
      dat_cut <- dat_salud_r() %>%
        filter(corte == input$salud_r_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        filter(ano >= input$fecha_salud_r[1] &
                 ano <= input$fecha_salud_r[2]) %>% 
        select(Fecha, salud_r_corte_var, Valor) %>%
        arrange(desc(Fecha)) %>% 
        pivot_wider(values_from = "Valor",
                    names_from = salud_r_corte_var)
      
    }
  })
  
  # Metadata 
  salud_r_meta <- reactive({
    
    dat_salud_r() %>%
      select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, observaciones, actualizacion, cita) %>% 
      mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
      distinct() %>% 
      gather(key = " ", value = " ")
    
  })
  
  # Excel
  list_salud_r <- reactive({
    list_salud_r <- list("Data" = salud_r_tab(),
                         "Metadata" = salud_r_meta())
  })
  
  # Render
  output$table_salud_r <- renderDT({
    
    DT::datatable(salud_r_tab(),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    input$indicador_salud_r,
                    style = "color:black; font-size:110%;")
    ) 
    
  })
  
  # * Descarga tablas   ================================================
  
  output$dwl_tab_salud_r <- downloadHandler(
    
    filename = function() {
      paste("resultados-", input$indicador_salud_r, ".xlsx", sep = "")
    },
    content = function(file) {
      
      openxlsx::write.xlsx(list_salud_r(), file)
      
    }
  )
  
  
  
  
  
  ### 5.1. Seguridad Social Politicas ========================================== 
  
  # * Data reactiva   =================================================    
  dat_ssocial_pp <- reactive({
    
    req(input$indicador_ssocial_pp)
    
    dat %>%
      filter(nomindicador == input$indicador_ssocial_pp) 
    
  })
  
  output$selector_ssocial_pp_corte_2 <- renderUI({
    
    if(input$indicador_ssocial_pp %in% lista_ind_2){
      
      selectInput(
        inputId = "ssocial_pp_corte_2",
        label = "Seleccione primer corte:",
        choices = dat_ssocial_pp() %>% 
          select(corte_2) %>%
          arrange(corte_2) %>% 
          unique() %>% 
          pull(),
        selected = dat_ssocial_pp() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte_2) %>% 
          pull()
      )
      
    } else {
      
      NULL
    }
    
  })
  
  output$chbox_ssocial_pp_2 <- renderUI({
    
    if(input$indicador_ssocial_pp %in% lista_ind_2 & input$indicador_ssocial_pp %notin% lista_especial){
      
      # if(input$ssocial_pp_corte %notin% c("Total", "Departamento") & input$indicador_ssocial_pp %notin% lista_vunico) {
      
      ssocial_pp_corte_var_2 <- rlang::sym(to_varname(input$ssocial_pp_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_ssocial_pp_2",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_ssocial_pp() %>%
                           filter(corte_2 == input$ssocial_pp_corte_2) %>% 
                           distinct(!!ssocial_pp_corte_var_2) %>%
                           pull(),
                         selected = dat_ssocial_pp() %>%
                           filter(corte_2 == input$ssocial_pp_corte_2) %>% 
                           filter(jerarquia_cat_2 == "1") %>%
                           distinct(!!ssocial_pp_corte_var_2) %>%
                           pull()
      )
      
      # } else {
      #   
      #   return(NULL)
      #   
      #   }
      
    } else {
      
      return(NULL)
      
    }      
  })
  
  
  output$selector_ssocial_pp_corte <- renderUI({
    
    selectInput(
      inputId = "ssocial_pp_corte",
      label = "Seleccione corte:",
      choices = dat_ssocial_pp() %>% 
        select(corte) %>%
        arrange(corte) %>% 
        unique() %>% 
        pull(),
      selected = dat_ssocial_pp() %>% 
        filter(jerarquia == "1") %>%  
        distinct(corte) %>% 
        pull()
    )
    
  })
  
  output$chbox_ssocial_pp <- renderUI({
    
    if(input$ssocial_pp_corte %in% lista_ind_2 & input$ssocial_pp_corte %notin% c("Total", "Departamento") & input$indicador_ssocial_pp %notin% lista_vunico) {
      
      ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
      ssocial_pp_corte_var_2 <- rlang::sym(to_varname(input$ssocial_pp_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_ssocial_pp",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_ssocial_pp() %>%
                           filter(!!ssocial_pp_corte_var_2 %in% input$checkbox_ssocial_pp_2) %>%
                           filter(corte == input$ssocial_pp_corte) %>% 
                           distinct(!!ssocial_pp_corte_var) %>%
                           pull(),
                         selected = dat_ssocial_pp() %>%
                           filter(!!ssocial_pp_corte_var_2 %in% input$checkbox_ssocial_pp_2) %>%
                           filter(corte == input$ssocial_pp_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!ssocial_pp_corte_var) %>%
                           pull()
      )
      
    } else if(input$ssocial_pp_corte %notin% lista_ind_2 & input$ssocial_pp_corte %notin% c("Total", "Departamento") & input$indicador_ssocial_pp %notin% lista_vunico) {
      
      ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
      
      checkboxGroupInput(inputId = "checkbox_ssocial_pp",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_ssocial_pp() %>%
                           filter(corte == input$ssocial_pp_corte) %>% 
                           distinct(!!ssocial_pp_corte_var) %>%
                           pull(),
                         selected = dat_ssocial_pp() %>%
                           filter(corte == input$ssocial_pp_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!ssocial_pp_corte_var) %>%
                           pull()
      )
      
    } else {
      
      return(NULL)
    }
    
  })
  
  # Selector de fecha
  output$s_ssocial_pp_fecha <- renderUI({
    
    if(input$ssocial_pp_corte == "Departamento" & input$indicador_ssocial_pp %notin% lista_ind_2) {
      
      req(input$ssocial_pp_corte, input$indicador_ssocial_pp)
      
      req(nrow(dat_ssocial_pp()) > 0)
      
      selectInput(
        inputId = "fecha_dpto_ssocial_pp",
        label = "Seleccione año:",
        choices = dat_ssocial_pp() %>% 
          filter(nomindicador == input$indicador_ssocial_pp) %>%
          drop_na(Valor) %>%
          select(ano) %>%
          arrange(desc(ano)) %>% 
          unique() %>% 
          pull(),
        selected = max(input$ano)
      )
      
    } else if (input$indicador_ssocial_pp %in% lista_serie_cat){
      
      return(NULL)
      
      
    } else if (input$indicador_ssocial_pp %in% lista_vunico){
      
      return(NULL)
      
    } else  {
      
      req(input$ssocial_pp_corte, input$indicador_ssocial_pp)
      req(nrow(dat_ssocial_pp()) > 0)
      
      tagList(
        # tags$style(type = 'text/css', 
        #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
        # div(id = 'big_slider',
        
        sliderInput("fecha_ssocial_pp", 
                    label = "Rango de tiempo", 
                    sep = "",
                    dragRange = T,
                    min = min(dat_ssocial_pp()$ano), 
                    max = max(dat_ssocial_pp()$ano), 
                    value = c(min(dat_ssocial_pp()$ano), 
                              max(dat_ssocial_pp()$ano))
        )
      )
      
    }
  })
  
  # # Selector de corte según categoría y data temporal
  # dat_ssocial_pp <- reactive({
  #   
  #   req(input$ssocial_pp_corte)
  #   
  #   if(input$indicador_ssocial_pp %in% lista_ind_2){
  #     
  #     dat_salarios <- dat_ssocial_pp() %>%
  #       filter(corte_2 == input$ssocial_pp_corte_2) %>% 
  #       filter(corte == input$ssocial_pp_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   } else {
  #     
  #     dat_ssocial_pp() %>%
  #       filter(corte == input$ssocial_pp_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   }
  # })
  
  # * Metadata   ======================================================
  
  # Title
  output$title_ssocial_pp <- renderUI({ 
    helpText(HTML(unique(dat_ssocial_pp()$nomindicador)))
  })
  
  # Subtitle
  output$subtitle_ssocial_pp <- renderUI({ 
    helpText(HTML(unique(dat_ssocial_pp()$definicion)))
  })
  
  # Nombre conceptual
  output$conindicador_ssocial_pp <- renderUI({ 
    helpText(HTML(paste("<b> Nombre conceptual:</b>", unique(dat_ssocial_pp()$conindicador))))
  })
  
  # Calculo
  output$calculo_ssocial_pp <- renderUI({ 
    helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_ssocial_pp()$calculo))))
  })
  
  # Observaciones
  output$observacion_ssocial_pp <- renderUI({ 
    helpText(HTML(paste("<b> Observaciones:</b>", unique(dat_ssocial_pp()$observaciones))))
  })
  
  # Actualización
  output$actualizacion_ssocial_pp <- renderUI({ 
    helpText(HTML(paste("<b> Última actualización:</b>", unique(dat_ssocial_pp()$actualizacion))))
  })
  
  
  # * Gráficos   ======================================================
  
  output$plot_ssocial_pp <- renderPlot({
    
    req(input$indicador_ssocial_pp, input$ssocial_pp_corte)
    
    if(input$indicador_ssocial_pp %in% lista_serie_cat){
      
      req(input$indicador_ssocial_pp)
      
      # Total
      if(input$ssocial_pp_corte == "Total"){
        
        dat_plot <- dat_ssocial_pp() %>%
          filter(corte == "Total")
        
        plot <- ggplot(dat_plot, aes(x = fecha_cat, y = Valor, group = 1)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_ssocial_pp),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador ssocial pp.png", width = 40, height = 25, units = "cm")
        
        # Según corte
      } else if(input$ssocial_pp_corte != "Total") {
        
        ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
        
        dat_plot <- dat_ssocial_pp() %>%
          filter(corte == input$ssocial_pp_corte) %>%
          filter(!!ssocial_pp_corte_var %in% input$checkbox_ssocial_pp)
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            colour = ssocial_pp_corte_var, group = ssocial_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_pp,
                                    "según",
                                    tolower(input$ssocial_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador ssocial pp.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_ssocial_pp %in% lista_especial ){
      
      if (input$ssocial_pp_corte_2 == "Total"){        
        
        req(input$ssocial_pp_corte, input$indicador_ssocial_pp)
        
        ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
        
        dat_plot <- dat_ssocial_pp() %>%
          filter(ano >= input$fecha_ssocial_pp[1] &
                   ano <= input$fecha_ssocial_pp[2]) %>%
          filter(corte == input$ssocial_pp_corte) %>%
          # filter(!!ssocial_pp_corte_var %in% input$checkbox_ssocial_pp) %>% 
          filter(corte_2 == input$ssocial_pp_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = ssocial_pp_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_pp,
                                    "según",
                                    tolower(input$ssocial_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador ssocial pp.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        req(input$ssocial_pp_corte, input$indicador_ssocial_pp)
        
        ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
        ssocial_pp_corte_var_2 <- rlang::sym(to_varname(input$ssocial_pp_corte_2))
        
        dat_plot <- dat_ssocial_pp() %>%
          filter(ano >= input$fecha_ssocial_pp[1] &
                   ano <= input$fecha_ssocial_pp[2]) %>%
          filter(corte == input$ssocial_pp_corte) %>%
          # filter(!!ssocial_pp_corte_var %in% input$checkbox_ssocial_pp) %>% 
          filter(corte_2 == input$ssocial_pp_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = ssocial_pp_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_pp,
                                    "según",
                                    tolower(input$ssocial_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", ssocial_pp_corte_var_2)))
        
        print(plot)
        ggsave("www/indicador ssocial pp.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_ssocial_pp %in% lista_vunico & input$indicador_ssocial_pp %in% lista_ind_2){
      
      req(input$ssocial_pp_corte, input$indicador_ssocial_pp)
      
      ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
      ssocial_pp_corte_var_2 <- rlang::sym(to_varname(input$ssocial_pp_corte_2))
      
      dat_plot <- dat_ssocial_pp() %>%
        filter(ano >= input$fecha_ssocial_pp[1] &
                 ano <= input$fecha_ssocial_pp[2]) %>%
        filter(corte == input$ssocial_pp_corte) %>%
        filter(!!ssocial_pp_corte_var %in% input$checkbox_ssocial_pp) %>%
        filter(!!ssocial_pp_corte_var_2 %in% input$checkbox_ssocial_pp_2) %>%
        filter(corte_2 == input$ssocial_pp_corte_2)
      
      plot <- ggplot(dat_plot,
                     aes_string(x = "fecha_cat", y = "Valor",
                                fill = ssocial_pp_corte_var)) +
        geom_col(position = "dodge", width = .7, alpha = .8) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x=element_blank(),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_ssocial_pp,
                                  "según",
                                  tolower(input$ssocial_pp_corte),
                                  "en",
                                  unique(dat_plot$fecha_cat))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_fill_brewer(name = "", palette = "Paired") +
        facet_wrap(as.formula(paste("~", ssocial_pp_corte_var_2)))
      
      print(plot)
      ggsave("www/indicador ssocial pp.png", width = 40, height = 25, units = "cm")
      
    } else if(input$indicador_ssocial_pp %in% lista_vunico & input$ssocial_pp_corte != "Departamento") {
      
      req(input$ssocial_pp_corte, input$indicador_ssocial_pp)
      
      dat_plot <- dat_ssocial_pp() %>%
        filter(corte == input$ssocial_pp_corte) %>%
        janitor::remove_empty("cols")
      
      if(input$ssocial_pp_corte == "Total"){
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor")) +
          geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
          geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_pp,
                                    "según",
                                    tolower(input$ssocial_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador ssocial pp.png", width = 40, height = 25, units = "cm")
        
        
      } else {
        
        ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor",
                                  fill = ssocial_pp_corte_var)) +
          geom_col(position = "dodge", alpha = .8, stroke = 1, color = "black") +
          geom_text(aes_string(group = ssocial_pp_corte_var, label = "Valor"),
                    position = position_dodge2(width = .9), vjust = -.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_pp,
                                    "según",
                                    tolower(input$ssocial_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador ssocial pp.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$indicador_ssocial_pp %in% lista_ind_2) {
      
      req(input$ssocial_pp_corte, input$ssocial_pp_corte_2,
          input$fecha_ssocial_pp, input$checkbox_ssocial_pp)
      
      ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
      ssocial_pp_corte_var_2 <- rlang::sym(to_varname(input$ssocial_pp_corte_2))
      
      dat_plot <- dat_ssocial_pp() %>%
        filter(ano >= input$fecha_ssocial_pp[1] &
                 ano <= input$fecha_ssocial_pp[2]) %>%
        filter(corte == input$ssocial_pp_corte) %>%
        filter(!!ssocial_pp_corte_var_2 %in% input$checkbox_ssocial_pp_2) %>%
        filter(!!ssocial_pp_corte_var %in% input$checkbox_ssocial_pp) %>% 
        filter(corte_2 == input$ssocial_pp_corte_2)
      
      if(input$ssocial_pp_corte_2 == "Total"){
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == "Total")
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = ssocial_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_pp,
                                    "según",
                                    tolower(input$ssocial_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
      } else if(input$ssocial_pp_corte_2 != "Total") {
        
        ssocial_pp_corte_var_2 <- rlang::sym(to_varname(input$ssocial_pp_corte_2))
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == input$ssocial_pp_corte_2)
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = ssocial_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          scale_x_continuous(breaks = int_breaks) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_pp,
                                    "según",
                                    tolower(input$ssocial_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          facet_wrap(as.formula(paste("~", ssocial_pp_corte_var_2)))
        
      }
      
      print(plot)
      ggsave("www/indicador ssocial pp.png", width = 40, height = 25, units = "cm")
      
      
      # Grafico simple normal
    } else if(input$ssocial_pp_corte == "Total") {
      
      req(input$indicador_ssocial_pp, input$fecha_ssocial_pp)
      
      dat_plot <- dat_ssocial_pp() %>%
        filter(ano >= input$fecha_ssocial_pp[1] &
                 ano <= input$fecha_ssocial_pp[2]) %>%
        filter(corte == "Total")
      
      if(input$indicador_ssocial_pp %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor, alpha = metodo)) +
          geom_line(size = 1, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          # scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "none") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_ssocial_pp),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8))
        
        print(plot)
        ggsave("www/indicador ssocial pp.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_ssocial_pp),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador ssocial pp.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$ssocial_pp_corte == "Departamento" &
              input$indicador_ssocial_pp %notin% lista_ind_2 ) {
      
      req(input$indicador_ssocial_pp, input$fecha_dpto_ssocial_pp)
      
      dat_plot <- dat_ssocial_pp() %>%
        filter(corte == "Departamento") %>%
        filter(ano == input$fecha_dpto_ssocial_pp) %>%
        select(departamento, Valor, fuente, cita)
      
      dep_j <- dep %>%
        left_join(dat_plot, by = c("nombre" = "departamento"))
      
      plot <-  ggplot(dep_j, aes(fill = Valor)) +
        geom_sf() +
        geom_sf_text(aes(label = Valor), colour = "black",
                     size = 4, fontface = "bold")+
        viridis::scale_fill_viridis(name = "", direction = -1)+
        labs(x = "",
             y = "",
        )+
        theme_bdd(base_size = 14) +
        theme(axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.grid.major = element_line(colour = "transparent"),
        ) +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_ssocial_pp,
                                  "en",
                                  input$fecha_dpto_ssocial_pp), w = 80),
             caption = wrapit(unique(dat_plot$cita), w = 80))
      
      print(plot)
      ggsave("www/indicador ssocial pp.png", width = 40, height = 25, units = "cm")
      
      
    } else if(input$ssocial_pp_corte != "Total") {
      
      req(input$ssocial_pp_corte, input$indicador_ssocial_pp,
          input$fecha_ssocial_pp, input$checkbox_ssocial_pp)
      
      dat_plot <- dat_ssocial_pp() %>%
        filter(ano >= input$fecha_ssocial_pp[1] &
                 ano <= input$fecha_ssocial_pp[2]) %>%
        filter(corte == input$ssocial_pp_corte) %>%
        janitor::remove_empty("cols")
      
      ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
      
      dat_plot <- filter(dat_plot,
                         !!ssocial_pp_corte_var %in% input$checkbox_ssocial_pp)
      
      if(input$indicador_ssocial_pp %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = ssocial_pp_corte_var, alpha = "metodo")) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_pp,
                                    "según",
                                    tolower(input$ssocial_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8)) +
          guides(alpha = "none")
        
        print(plot)
        ggsave("www/indicador ssocial pp.png", width = 40, height = 25, units = "cm")
        
        
      } else {
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = ssocial_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_pp,
                                    "según",
                                    tolower(input$ssocial_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador ssocial pp.png", width = 40, height = 25, units = "cm")
        
        
      }
      
      
    }
    
  })
  
  
  # * Descarga gráficos   =============================================
  
  output$baja_p_ssocial_pp <- downloadHandler(
    filename <- function() {
      paste("indicador ssocial pp", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/indicador ssocial pp.png", file)
    },
    contentType = "www/indicador ssocial pp"
  )
  
  
  # * Tablas   ========================================================
  
  # Data
  ssocial_pp_tab <- reactive({
    
    if(input$indicador_ssocial_pp %in%  lista_especial){
      
      ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
      ssocial_pp_corte_var_2 <- rlang::sym(to_varname(input$ssocial_pp_corte_2))
      
      dat_ssocial_pp() %>%
        filter(corte_2 == input$ssocial_pp_corte_2) %>% 
        select(Fecha, ssocial_pp_corte_var, ssocial_pp_corte_var_2, Valor) %>%
        arrange(desc(Fecha)) %>%
        pivot_wider(values_from = "Valor",
                    names_from = ssocial_pp_corte_var) 
      
    } else if(input$indicador_ssocial_pp %in% lista_vunico & input$ssocial_pp_corte == "Total"){
      
      req(input$ssocial_pp_corte, input$indicador_ssocial_pp)
      
      dat_ssocial_pp() %>%
        filter(corte == "Total") %>% 
        select(fecha_cat, Valor) %>%
        arrange(desc(fecha_cat)) %>%
        rename(Fecha = fecha_cat)
      
    } else if(input$indicador_ssocial_pp %in% lista_vunico & input$ssocial_pp_corte != "Total") {
      
      req(input$ssocial_pp_corte, input$indicador_ssocial_pp)
      
      ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
      
      dat_cut <- dat_ssocial_pp() %>%
        filter(corte == input$ssocial_pp_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        select(fecha_cat, ssocial_pp_corte_var, Valor) %>%
        arrange(desc(fecha_cat)) %>% 
        rename(Fecha = fecha_cat) %>%
        pivot_wider(values_from = "Valor",
                    names_from = ssocial_pp_corte_var)
      
      
    } else if(input$indicador_ssocial_pp %in% lista_ind_2) {
      
      req(input$ssocial_pp_corte, input$indicador_ssocial_pp, input$fecha_ssocial_pp)
      
      ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
      
      dat_cut <- dat_ssocial_pp() %>%
        filter(corte == input$ssocial_pp_corte) %>%
        filter(!!ssocial_pp_corte_var %in% input$checkbox_ssocial_pp)
      
      if(input$ssocial_pp_corte_2 == "Total"){
        
        dat_cut %>%
          filter(ano >= input$fecha_ssocial_pp[1] &
                   ano <= input$fecha_ssocial_pp[2]) %>%
          filter(corte_2 == "Total") %>% 
          select(Fecha, ssocial_pp_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = ssocial_pp_corte_var)
        
      } else if(input$ssocial_pp_corte_2 != "Total") {
        
        ssocial_pp_corte_var_2 <- rlang::sym(to_varname(input$ssocial_pp_corte_2))
        
        dat_cut %>%
          filter(ano >= input$fecha_ssocial_pp[1] &
                   ano <= input$fecha_ssocial_pp[2]) %>%
          filter(corte_2 == input$ssocial_pp_corte_2) %>% 
          select(Fecha, ssocial_pp_corte_var, ssocial_pp_corte_var_2, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = ssocial_pp_corte_var) 
        
      }
      
    } else if(input$ssocial_pp_corte == "Total") {
      
      req(input$ssocial_pp_corte, input$indicador_ssocial_pp, input$fecha_ssocial_pp)
      
      dat_ssocial_pp() %>%
        filter(corte == "Total") %>% 
        filter(ano >= input$fecha_ssocial_pp[1] &
                 ano <= input$fecha_ssocial_pp[2]) %>%
        select(Fecha, Valor) %>%
        arrange(desc(Fecha))
      
    } else if(input$ssocial_pp_corte != "Total") {
      
      req(input$ssocial_pp_corte, input$indicador_ssocial_pp, input$fecha_ssocial_pp)
      
      ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
      
      dat_cut <- dat_ssocial_pp() %>%
        filter(corte == input$ssocial_pp_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        filter(ano >= input$fecha_ssocial_pp[1] &
                 ano <= input$fecha_ssocial_pp[2]) %>% 
        select(Fecha, ssocial_pp_corte_var, Valor) %>%
        arrange(desc(Fecha)) %>% 
        pivot_wider(values_from = "Valor",
                    names_from = ssocial_pp_corte_var)
      
    }
  })
  
  # Metadata 
  ssocial_pp_meta <- reactive({
    
    dat_ssocial_pp() %>%
      select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, observaciones, actualizacion, cita) %>% 
      mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
      distinct() %>% 
      gather(key = " ", value = " ")
    
  })
  
  # Excel
  list_ssocial_pp <- reactive({
    list_ssocial_pp <- list("Data" = ssocial_pp_tab(),
                            "Metadata" = ssocial_pp_meta())
  })
  
  # Render
  output$table_ssocial_pp <- renderDT({
    
    DT::datatable(ssocial_pp_tab(),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    input$indicador_ssocial_pp,
                    style = "color:black; font-size:110%;")
    ) 
    
  })
  
  # * Descarga tablas   ================================================
  
  output$dwl_tab_ssocial_pp <- downloadHandler(
    
    filename = function() {
      paste("resultados-", input$indicador_ssocial_pp, ".xlsx", sep = "")
    },
    content = function(file) {
      
      openxlsx::write.xlsx(list_ssocial_pp(), file)
      
    }
  )
  
  
  
  ### 5.2. Seguridad Social Resultados ========================================== 
  
  # * Data reactiva   =================================================    
  dat_ssocial_r <- reactive({
    
    req(input$indicador_ssocial_r)
    
    dat %>%
      filter(nomindicador == input$indicador_ssocial_r) 
    
  })
  
  output$selector_ssocial_r_corte_2 <- renderUI({
    
    if(input$indicador_ssocial_r %in% lista_ind_2){
      
      selectInput(
        inputId = "ssocial_r_corte_2",
        label = "Seleccione primer corte:",
        choices = dat_ssocial_r() %>% 
          select(corte_2) %>%
          arrange(corte_2) %>% 
          unique() %>% 
          pull(),
        selected = dat_ssocial_r() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte_2) %>% 
          pull()
      )
      
    } else {
      
      NULL
    }
    
  })
  
  output$chbox_ssocial_r_2 <- renderUI({
    
    if(input$indicador_ssocial_r %in% lista_ind_2 & input$indicador_ssocial_r %notin% lista_especial){
      
      # if(input$ssocial_r_corte %notin% c("Total", "Departamento") & input$indicador_ssocial_r %notin% lista_vunico) {
      
      ssocial_r_corte_var_2 <- rlang::sym(to_varname(input$ssocial_r_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_ssocial_r_2",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_ssocial_r() %>%
                           filter(corte_2 == input$ssocial_r_corte_2) %>% 
                           distinct(!!ssocial_r_corte_var_2) %>%
                           pull(),
                         selected = dat_ssocial_r() %>%
                           filter(corte_2 == input$ssocial_r_corte_2) %>% 
                           filter(jerarquia_cat_2 == "1") %>%
                           distinct(!!ssocial_r_corte_var_2) %>%
                           pull()
      )
      
      # } else {
      #   
      #   return(NULL)
      #   
      #   }
      
    } else {
      
      return(NULL)
      
    }      
  })
  
  
  output$selector_ssocial_r_corte <- renderUI({
    
    selectInput(
      inputId = "ssocial_r_corte",
      label = "Seleccione corte:",
      choices = dat_ssocial_r() %>% 
        select(corte) %>%
        arrange(corte) %>% 
        unique() %>% 
        pull(),
      selected = dat_ssocial_r() %>% 
        filter(jerarquia == "1") %>%  
        distinct(corte) %>% 
        pull()
    )
    
  })
  
  output$chbox_ssocial_r <- renderUI({
    
    if(input$ssocial_r_corte %in% lista_ind_2 & input$ssocial_r_corte %notin% c("Total", "Departamento") & input$indicador_ssocial_r %notin% lista_vunico) {
      
      ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
      ssocial_r_corte_var_2 <- rlang::sym(to_varname(input$ssocial_r_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_ssocial_r",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_ssocial_r() %>%
                           filter(!!ssocial_r_corte_var_2 %in% input$checkbox_ssocial_r_2) %>%
                           filter(corte == input$ssocial_r_corte) %>% 
                           distinct(!!ssocial_r_corte_var) %>%
                           pull(),
                         selected = dat_ssocial_r() %>%
                           filter(!!ssocial_r_corte_var_2 %in% input$checkbox_ssocial_r_2) %>%
                           filter(corte == input$ssocial_r_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!ssocial_r_corte_var) %>%
                           pull()
      )
      
    } else if(input$ssocial_r_corte %notin% lista_ind_2 & input$ssocial_r_corte %notin% c("Total", "Departamento") & input$indicador_ssocial_r %notin% lista_vunico) {
      
      ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
      
      checkboxGroupInput(inputId = "checkbox_ssocial_r",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_ssocial_r() %>%
                           filter(corte == input$ssocial_r_corte) %>% 
                           distinct(!!ssocial_r_corte_var) %>%
                           pull(),
                         selected = dat_ssocial_r() %>%
                           filter(corte == input$ssocial_r_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!ssocial_r_corte_var) %>%
                           pull()
      )
      
    } else {
      
      return(NULL)
    }
    
  })
  
  # Selector de fecha
  output$s_ssocial_r_fecha <- renderUI({
    
    if(input$ssocial_r_corte == "Departamento" & input$indicador_ssocial_r %notin% lista_ind_2) {
      
      req(input$ssocial_r_corte, input$indicador_ssocial_r)
      
      req(nrow(dat_ssocial_r()) > 0)
      
      selectInput(
        inputId = "fecha_dpto_ssocial_r",
        label = "Seleccione año:",
        choices = dat_ssocial_r() %>% 
          filter(nomindicador == input$indicador_ssocial_r) %>%
          drop_na(Valor) %>%
          select(ano) %>%
          arrange(desc(ano)) %>% 
          unique() %>% 
          pull(),
        selected = max(input$ano)
      )
      
    } else if (input$indicador_ssocial_r %in% lista_serie_cat){
      
      return(NULL)
      
      
    } else if (input$indicador_ssocial_r %in% lista_vunico){
      
      return(NULL)
      
    } else  {
      
      req(input$ssocial_r_corte, input$indicador_ssocial_r)
      req(nrow(dat_ssocial_r()) > 0)
      
      tagList(
        # tags$style(type = 'text/css', 
        #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
        # div(id = 'big_slider',
        
        sliderInput("fecha_ssocial_r", 
                    label = "Rango de tiempo", 
                    sep = "",
                    dragRange = T,
                    min = min(dat_ssocial_r()$ano), 
                    max = max(dat_ssocial_r()$ano), 
                    value = c(min(dat_ssocial_r()$ano), 
                              max(dat_ssocial_r()$ano))
        )
      )
      
    }
  })
  
  # # Selector de corte según categoría y data temporal
  # dat_ssocial_r <- reactive({
  #   
  #   req(input$ssocial_r_corte)
  #   
  #   if(input$indicador_ssocial_r %in% lista_ind_2){
  #     
  #     dat_salarios <- dat_ssocial_r() %>%
  #       filter(corte_2 == input$ssocial_r_corte_2) %>% 
  #       filter(corte == input$ssocial_r_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   } else {
  #     
  #     dat_ssocial_r() %>%
  #       filter(corte == input$ssocial_r_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   }
  # })
  
  # * Metadata   ======================================================
  
  # Title
  output$title_ssocial_r <- renderUI({ 
    helpText(HTML(unique(dat_ssocial_r()$nomindicador)))
  })
  
  # Subtitle
  output$subtitle_ssocial_r <- renderUI({ 
    helpText(HTML(unique(dat_ssocial_r()$definicion)))
  })
  
  # Nota
  output$nota_ssocial_r <- renderUI({ 
    
    if(input$indicador_ssocial_r %in% lista_nota){
      helpText(HTML(unique(dat_ssocial_r()$nota)))
      
    } else(NULL)
  })
  
  # Nombre conceptual
  output$conindicador_ssocial_r <- renderUI({ 
    helpText(HTML(paste("<b> Nombre conceptual:</b>", unique(dat_ssocial_r()$conindicador))))
  })
  
  # Calculo
  output$calculo_ssocial_r <- renderUI({ 
    helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_ssocial_r()$calculo))))
  })
  
  # Observaciones
  output$observacion_ssocial_r <- renderUI({ 
    helpText(HTML(paste("<b> Observaciones:</b>", unique(dat_ssocial_r()$observaciones))))
  })
  
  # Actualización
  output$actualizacion_ssocial_r <- renderUI({ 
    helpText(HTML(paste("<b> Última actualización:</b>", unique(dat_ssocial_r()$actualizacion))))
  })
  
  # * Gráficos   ======================================================
  
  output$plot_ssocial_r <- renderPlot({
    
    req(input$indicador_ssocial_r, input$ssocial_r_corte)
    
    if(input$indicador_ssocial_r %in% lista_serie_cat){
      
      req(input$indicador_ssocial_r)
      
      # Total
      if(input$ssocial_r_corte == "Total"){
        
        dat_plot <- dat_ssocial_r() %>%
          filter(corte == "Total")
        
        plot <- ggplot(dat_plot, aes(x = fecha_cat, y = Valor, group = 1)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_ssocial_r),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador ssocial r.png", width = 40, height = 25, units = "cm")
        
        # Según corte
      } else if(input$ssocial_r_corte != "Total") {
        
        ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
        
        dat_plot <- dat_ssocial_r() %>%
          filter(corte == input$ssocial_r_corte) %>%
          filter(!!ssocial_r_corte_var %in% input$checkbox_ssocial_r)
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            colour = ssocial_r_corte_var, group = ssocial_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_r,
                                    "según",
                                    tolower(input$ssocial_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador ssocial r.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_ssocial_r %in% lista_especial ){
      
      if (input$ssocial_r_corte_2 == "Total"){        
        
        req(input$ssocial_r_corte, input$indicador_ssocial_r)
        
        ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
        
        dat_plot <- dat_ssocial_r() %>%
          filter(ano >= input$fecha_ssocial_r[1] &
                   ano <= input$fecha_ssocial_r[2]) %>%
          filter(corte == input$ssocial_r_corte) %>%
          # filter(!!ssocial_r_corte_var %in% input$checkbox_ssocial_r) %>% 
          filter(corte_2 == input$ssocial_r_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = ssocial_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_r,
                                    "según",
                                    tolower(input$ssocial_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador ssocial r.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        req(input$ssocial_r_corte, input$indicador_ssocial_r)
        
        ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
        ssocial_r_corte_var_2 <- rlang::sym(to_varname(input$ssocial_r_corte_2))
        
        dat_plot <- dat_ssocial_r() %>%
          filter(ano >= input$fecha_ssocial_r[1] &
                   ano <= input$fecha_ssocial_r[2]) %>%
          filter(corte == input$ssocial_r_corte) %>%
          # filter(!!ssocial_r_corte_var %in% input$checkbox_ssocial_r) %>% 
          filter(corte_2 == input$ssocial_r_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = ssocial_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_r,
                                    "según",
                                    tolower(input$ssocial_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", ssocial_r_corte_var_2)))
        
        print(plot)
        ggsave("www/indicador ssocial r.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_ssocial_r %in% lista_vunico & input$indicador_ssocial_r %in% lista_ind_2){
      
      req(input$ssocial_r_corte, input$indicador_ssocial_r)
      
      ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
      ssocial_r_corte_var_2 <- rlang::sym(to_varname(input$ssocial_r_corte_2))
      
      dat_plot <- dat_ssocial_r() %>%
        filter(ano >= input$fecha_ssocial_r[1] &
                 ano <= input$fecha_ssocial_r[2]) %>%
        filter(corte == input$ssocial_r_corte) %>%
        filter(!!ssocial_r_corte_var %in% input$checkbox_ssocial_r) %>%
        filter(!!ssocial_r_corte_var_2 %in% input$checkbox_ssocial_r_2) %>%
        filter(corte_2 == input$ssocial_r_corte_2)
      
      plot <- ggplot(dat_plot,
                     aes_string(x = "fecha_cat", y = "Valor",
                                fill = ssocial_r_corte_var)) +
        geom_col(position = "dodge", width = .7, alpha = .8) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x=element_blank(),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_ssocial_r,
                                  "según",
                                  tolower(input$ssocial_r_corte),
                                  "en",
                                  unique(dat_plot$fecha_cat))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_fill_brewer(name = "", palette = "Paired") +
        facet_wrap(as.formula(paste("~", ssocial_r_corte_var_2)))
      
      print(plot)
      ggsave("www/indicador ssocial r.png", width = 40, height = 25, units = "cm")
      
    } else if(input$indicador_ssocial_r %in% lista_vunico & input$ssocial_r_corte != "Departamento") {
      
      req(input$ssocial_r_corte, input$indicador_ssocial_r)
      
      dat_plot <- dat_ssocial_r() %>%
        filter(corte == input$ssocial_r_corte) %>%
        janitor::remove_empty("cols")
      
      if(input$ssocial_r_corte == "Total"){
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor")) +
          geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
          geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_r,
                                    "según",
                                    tolower(input$ssocial_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador ssocial r.png", width = 40, height = 25, units = "cm")
        
        
      } else {
        
        ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor",
                                  fill = ssocial_r_corte_var)) +
          geom_col(position = "dodge", alpha = .8, stroke = 1, color = "black") +
          geom_text(aes_string(group = ssocial_r_corte_var, label = "Valor"),
                    position = position_dodge2(width = .9), vjust = -.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_r,
                                    "según",
                                    tolower(input$ssocial_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador ssocial r.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$indicador_ssocial_r %in% lista_ind_2) {
      
      req(input$ssocial_r_corte, input$ssocial_r_corte_2,
          input$fecha_ssocial_r, input$checkbox_ssocial_r)
      
      ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
      ssocial_r_corte_var_2 <- rlang::sym(to_varname(input$ssocial_r_corte_2))
      
      dat_plot <- dat_ssocial_r() %>%
        filter(ano >= input$fecha_ssocial_r[1] &
                 ano <= input$fecha_ssocial_r[2]) %>%
        filter(corte == input$ssocial_r_corte) %>%
        filter(!!ssocial_r_corte_var_2 %in% input$checkbox_ssocial_r_2) %>%
        filter(!!ssocial_r_corte_var %in% input$checkbox_ssocial_r) %>% 
        filter(corte_2 == input$ssocial_r_corte_2)
      
      if(input$ssocial_r_corte_2 == "Total"){
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == "Total")
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = ssocial_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_r,
                                    "según",
                                    tolower(input$ssocial_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
      } else if(input$ssocial_r_corte_2 != "Total") {
        
        ssocial_r_corte_var_2 <- rlang::sym(to_varname(input$ssocial_r_corte_2))
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == input$ssocial_r_corte_2)
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = ssocial_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          scale_x_continuous(breaks = int_breaks) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_r,
                                    "según",
                                    tolower(input$ssocial_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          facet_wrap(as.formula(paste("~", ssocial_r_corte_var_2)))
        
      }
      
      print(plot)
      ggsave("www/indicador ssocial r.png", width = 40, height = 25, units = "cm")
      
      
      # Grafico simple normal
    } else if(input$ssocial_r_corte == "Total") {
      
      req(input$indicador_ssocial_r, input$fecha_ssocial_r)
      
      dat_plot <- dat_ssocial_r() %>%
        filter(ano >= input$fecha_ssocial_r[1] &
                 ano <= input$fecha_ssocial_r[2]) %>%
        filter(corte == "Total")
      
      if(input$indicador_ssocial_r %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor, alpha = metodo)) +
          geom_line(size = 1, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          # scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "none") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_ssocial_r),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8))
        
        print(plot)
        ggsave("www/indicador ssocial r.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_ssocial_r),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador ssocial r.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$ssocial_r_corte == "Departamento" &
              input$indicador_ssocial_r %notin% lista_ind_2 ) {
      
      req(input$indicador_ssocial_r, input$fecha_dpto_ssocial_r)
      
      dat_plot <- dat_ssocial_r() %>%
        filter(corte == "Departamento") %>%
        filter(ano == input$fecha_dpto_ssocial_r) %>%
        select(departamento, Valor, fuente, cita)
      
      dep_j <- dep %>%
        left_join(dat_plot, by = c("nombre" = "departamento"))
      
      plot <-  ggplot(dep_j, aes(fill = Valor)) +
        geom_sf() +
        geom_sf_text(aes(label = Valor), colour = "black",
                     size = 4, fontface = "bold")+
        viridis::scale_fill_viridis(name = "", direction = -1)+
        labs(x = "",
             y = "",
        )+
        theme_bdd(base_size = 14) +
        theme(axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.grid.major = element_line(colour = "transparent"),
        ) +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_ssocial_r,
                                  "en",
                                  input$fecha_dpto_ssocial_r), w = 80),
             caption = wrapit(unique(dat_plot$cita), w = 80))
      
      print(plot)
      ggsave("www/indicador ssocial r.png", width = 40, height = 25, units = "cm")
      
      
    } else if(input$ssocial_r_corte != "Total") {
      
      req(input$ssocial_r_corte, input$indicador_ssocial_r,
          input$fecha_ssocial_r, input$checkbox_ssocial_r)
      
      dat_plot <- dat_ssocial_r() %>%
        filter(ano >= input$fecha_ssocial_r[1] &
                 ano <= input$fecha_ssocial_r[2]) %>%
        filter(corte == input$ssocial_r_corte) %>%
        janitor::remove_empty("cols")
      
      ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
      
      dat_plot <- filter(dat_plot,
                         !!ssocial_r_corte_var %in% input$checkbox_ssocial_r)
      
      if(input$indicador_ssocial_r %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = ssocial_r_corte_var, alpha = "metodo")) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_r,
                                    "según",
                                    tolower(input$ssocial_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8)) +
          guides(alpha = "none")
        
        print(plot)
        ggsave("www/indicador ssocial r.png", width = 40, height = 25, units = "cm")      
        
      } else {
        
        ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
        
        dat_plot <- dat_ssocial_r() %>%
          filter(corte == input$ssocial_r_corte) %>%
          filter(!!ssocial_r_corte_var %in% input$checkbox_ssocial_r)
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            colour = ssocial_r_corte_var, group = ssocial_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_r,
                                    "según",
                                    tolower(input$ssocial_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador ssocial r.png", width = 40, height = 25, units = "cm")
        
      }
    }
  })
  
  
  # * Descarga gráficos   =============================================
  
  output$baja_p_ssocial_r <- downloadHandler(
    filename <- function() {
      paste("indicador ssocial r", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/indicador ssocial r.png", file)
    },
    contentType = "www/indicador ssocial r"
  )
  
  
  # * Tablas   ========================================================
  
  # Data
  ssocial_r_tab <- reactive({
    
    if(input$indicador_ssocial_r %in%  lista_especial){
      
      ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
      ssocial_r_corte_var_2 <- rlang::sym(to_varname(input$ssocial_r_corte_2))
      
      dat_ssocial_r() %>%
        filter(corte_2 == input$ssocial_r_corte_2) %>% 
        select(Fecha, ssocial_r_corte_var, ssocial_r_corte_var_2, Valor) %>%
        arrange(desc(Fecha)) %>%
        pivot_wider(values_from = "Valor",
                    names_from = ssocial_r_corte_var) 
      
    } else if(input$indicador_ssocial_r %in% lista_vunico & input$ssocial_r_corte == "Total"){
      
      req(input$ssocial_r_corte, input$indicador_ssocial_r)
      
      dat_ssocial_r() %>%
        filter(corte == "Total") %>% 
        select(fecha_cat, Valor) %>%
        arrange(desc(fecha_cat)) %>%
        rename(Fecha = fecha_cat)
      
    } else if(input$indicador_ssocial_r %in% lista_vunico & input$ssocial_r_corte != "Total") {
      
      req(input$ssocial_r_corte, input$indicador_ssocial_r)
      
      ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
      
      dat_cut <- dat_ssocial_r() %>%
        filter(corte == input$ssocial_r_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        select(fecha_cat, ssocial_r_corte_var, Valor) %>%
        arrange(desc(fecha_cat)) %>% 
        rename(Fecha = fecha_cat) %>%
        pivot_wider(values_from = "Valor",
                    names_from = ssocial_r_corte_var)
      
      
    } else if(input$indicador_ssocial_r %in% lista_ind_2) {
      
      req(input$ssocial_r_corte, input$indicador_ssocial_r, input$fecha_ssocial_r)
      
      ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
      
      dat_cut <- dat_ssocial_r() %>%
        filter(corte == input$ssocial_r_corte) %>%
        filter(!!ssocial_r_corte_var %in% input$checkbox_ssocial_r)
      
      if(input$ssocial_r_corte_2 == "Total"){
        
        dat_cut %>%
          filter(ano >= input$fecha_ssocial_r[1] &
                   ano <= input$fecha_ssocial_r[2]) %>%
          filter(corte_2 == "Total") %>% 
          select(Fecha, ssocial_r_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = ssocial_r_corte_var)
        
      } else if(input$ssocial_r_corte_2 != "Total") {
        
        ssocial_r_corte_var_2 <- rlang::sym(to_varname(input$ssocial_r_corte_2))
        
        dat_cut %>%
          filter(ano >= input$fecha_ssocial_r[1] &
                   ano <= input$fecha_ssocial_r[2]) %>%
          filter(corte_2 == input$ssocial_r_corte_2) %>% 
          select(Fecha, ssocial_r_corte_var, ssocial_r_corte_var_2, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = ssocial_r_corte_var) 
        
      }
      
    } else if(input$ssocial_r_corte == "Total") {
      
      req(input$ssocial_r_corte, input$indicador_ssocial_r, input$fecha_ssocial_r)
      
      dat_ssocial_r() %>%
        filter(corte == "Total") %>% 
        filter(ano >= input$fecha_ssocial_r[1] &
                 ano <= input$fecha_ssocial_r[2]) %>%
        select(Fecha, Valor) %>%
        arrange(desc(Fecha))
      
    } else if(input$ssocial_r_corte != "Total") {
      
      req(input$ssocial_r_corte, input$indicador_ssocial_r, input$fecha_ssocial_r)
      
      ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
      
      dat_cut <- dat_ssocial_r() %>%
        filter(corte == input$ssocial_r_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        filter(ano >= input$fecha_ssocial_r[1] &
                 ano <= input$fecha_ssocial_r[2]) %>% 
        select(Fecha, ssocial_r_corte_var, Valor) %>%
        arrange(desc(Fecha)) %>% 
        pivot_wider(values_from = "Valor",
                    names_from = ssocial_r_corte_var)
      
    }
  })
  
  # Metadata 
  ssocial_r_meta <- reactive({
    
    dat_ssocial_r() %>%
      select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, observaciones, actualizacion, cita) %>% 
      mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
      distinct() %>% 
      gather(key = " ", value = " ")
    
  })
  
  # Excel
  list_ssocial_r <- reactive({
    list_ssocial_r <- list("Data" = ssocial_r_tab(),
                           "Metadata" = ssocial_r_meta())
  })
  
  # Render
  output$table_ssocial_r <- renderDT({
    
    DT::datatable(ssocial_r_tab(),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    input$indicador_ssocial_r,
                    style = "color:black; font-size:110%;")
    ) 
    
  })
  
  # * Descarga tablas   ================================================
  
  output$dwl_tab_ssocial_r <- downloadHandler(
    
    filename = function() {
      paste("resultados-", input$indicador_ssocial_r, ".xlsx", sep = "")
    },
    content = function(file) {
      
      openxlsx::write.xlsx(list_ssocial_r(), file)
      
    }
  )
  
  
  
  ### 6.1. Vivienda Politicas ==============================================
  
  # * Data reactiva   =================================================    
  dat_vivienda_pp <- reactive({
    
    req(input$indicador_vivienda_pp)
    
    dat %>%
      filter(nomindicador == input$indicador_vivienda_pp) 
    
  })
  
  output$selector_vivienda_pp_corte_2 <- renderUI({
    
    if(input$indicador_vivienda_pp %in% lista_ind_2){
      
      selectInput(
        inputId = "vivienda_pp_corte_2",
        label = "Seleccione primer corte:",
        choices = dat_vivienda_pp() %>% 
          select(corte_2) %>%
          arrange(corte_2) %>% 
          unique() %>% 
          pull(),
        selected = dat_vivienda_pp() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte_2) %>% 
          pull()
      )
      
    } else {
      
      NULL
    }
    
  })
  
  output$chbox_vivienda_pp_2 <- renderUI({
    
    if(input$indicador_vivienda_pp %in% lista_ind_2 & input$indicador_vivienda_pp %notin% lista_especial){
      
      # if(input$vivienda_pp_corte %notin% c("Total", "Departamento") & input$indicador_vivienda_pp %notin% lista_vunico) {
      
      vivienda_pp_corte_var_2 <- rlang::sym(to_varname(input$vivienda_pp_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_vivienda_pp_2",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_vivienda_pp() %>%
                           filter(corte_2 == input$vivienda_pp_corte_2) %>% 
                           distinct(!!vivienda_pp_corte_var_2) %>%
                           pull(),
                         selected = dat_vivienda_pp() %>%
                           filter(corte_2 == input$vivienda_pp_corte_2) %>% 
                           filter(jerarquia_cat_2 == "1") %>%
                           distinct(!!vivienda_pp_corte_var_2) %>%
                           pull()
      )
      
      # } else {
      #   
      #   return(NULL)
      #   
      #   }
      
    } else {
      
      return(NULL)
      
    }      
  })
  
  
  output$selector_vivienda_pp_corte <- renderUI({
    
    selectInput(
      inputId = "vivienda_pp_corte",
      label = "Seleccione corte:",
      choices = dat_vivienda_pp() %>% 
        select(corte) %>%
        arrange(corte) %>% 
        unique() %>% 
        pull(),
      selected = dat_vivienda_pp() %>% 
        filter(jerarquia == "1") %>%  
        distinct(corte) %>% 
        pull()
    )
    
  })
  
  output$chbox_vivienda_pp <- renderUI({
    
    if(input$vivienda_pp_corte %in% lista_ind_2 & input$vivienda_pp_corte %notin% c("Total", "Departamento") & input$indicador_vivienda_pp %notin% lista_vunico) {
      
      vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
      vivienda_pp_corte_var_2 <- rlang::sym(to_varname(input$vivienda_pp_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_vivienda_pp",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_vivienda_pp() %>%
                           filter(!!vivienda_pp_corte_var_2 %in% input$checkbox_vivienda_pp_2) %>%
                           filter(corte == input$vivienda_pp_corte) %>% 
                           distinct(!!vivienda_pp_corte_var) %>%
                           pull(),
                         selected = dat_vivienda_pp() %>%
                           filter(!!vivienda_pp_corte_var_2 %in% input$checkbox_vivienda_pp_2) %>%
                           filter(corte == input$vivienda_pp_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!vivienda_pp_corte_var) %>%
                           pull()
      )
      
    } else if(input$vivienda_pp_corte %notin% lista_ind_2 & input$vivienda_pp_corte %notin% c("Total", "Departamento") & input$indicador_vivienda_pp %notin% lista_vunico) {
      
      vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
      
      checkboxGroupInput(inputId = "checkbox_vivienda_pp",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_vivienda_pp() %>%
                           filter(corte == input$vivienda_pp_corte) %>% 
                           distinct(!!vivienda_pp_corte_var) %>%
                           pull(),
                         selected = dat_vivienda_pp() %>%
                           filter(corte == input$vivienda_pp_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!vivienda_pp_corte_var) %>%
                           pull()
      )
      
    } else {
      
      return(NULL)
    }
    
  })
  
  # Selector de fecha
  output$s_vivienda_pp_fecha <- renderUI({
    
    if(input$vivienda_pp_corte == "Departamento" & input$indicador_vivienda_pp %notin% lista_ind_2) {
      
      req(input$vivienda_pp_corte, input$indicador_vivienda_pp)
      
      req(nrow(dat_vivienda_pp()) > 0)
      
      selectInput(
        inputId = "fecha_dpto_vivienda_pp",
        label = "Seleccione año:",
        choices = dat_vivienda_pp() %>% 
          filter(nomindicador == input$indicador_vivienda_pp) %>%
          drop_na(Valor) %>%
          select(ano) %>%
          arrange(desc(ano)) %>% 
          unique() %>% 
          pull(),
        selected = max(input$ano)
      )
      
    } else if (input$indicador_vivienda_pp %in% lista_serie_cat){
      
      return(NULL)
      
      
    } else if (input$indicador_vivienda_pp %in% lista_vunico){
      
      return(NULL)
      
    } else  {
      
      req(input$vivienda_pp_corte, input$indicador_vivienda_pp)
      req(nrow(dat_vivienda_pp()) > 0)
      
      tagList(
        # tags$style(type = 'text/css', 
        #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
        # div(id = 'big_slider',
        
        sliderInput("fecha_vivienda_pp", 
                    label = "Rango de tiempo", 
                    sep = "",
                    dragRange = T,
                    min = min(dat_vivienda_pp()$ano), 
                    max = max(dat_vivienda_pp()$ano), 
                    value = c(min(dat_vivienda_pp()$ano), 
                              max(dat_vivienda_pp()$ano))
        )
      )
      
    }
  })
  
  # # Selector de corte según categoría y data temporal
  # dat_vivienda_pp <- reactive({
  #   
  #   req(input$vivienda_pp_corte)
  #   
  #   if(input$indicador_vivienda_pp %in% lista_ind_2){
  #     
  #     dat_salarios <- dat_vivienda_pp() %>%
  #       filter(corte_2 == input$vivienda_pp_corte_2) %>% 
  #       filter(corte == input$vivienda_pp_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   } else {
  #     
  #     dat_vivienda_pp() %>%
  #       filter(corte == input$vivienda_pp_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   }
  # })
  
  # * Metadata   ======================================================
  
  # Title
  output$title_vivienda_pp <- renderUI({ 
    helpText(HTML(unique(dat_vivienda_pp()$nomindicador)))
  })
  
  # Subtitle
  output$subtitle_vivienda_pp <- renderUI({ 
    helpText(HTML(unique(dat_vivienda_pp()$definicion)))
  })
  
  # Nombre conceptual
  output$conindicador_vivienda_pp <- renderUI({ 
    helpText(HTML(paste("<b> Nombre conceptual:</b>", unique(dat_vivienda_pp()$conindicador))))
  })
  
  # Calculo
  output$calculo_vivienda_pp <- renderUI({ 
    helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_vivienda_pp()$calculo))))
  })
  
  # Observaciones
  output$observacion_vivienda_pp <- renderUI({ 
    helpText(HTML(paste("<b> Observaciones:</b>", unique(dat_vivienda_pp()$observaciones))))
  })
  
  # Actualización
  output$actualizacion_vivienda_pp <- renderUI({ 
    helpText(HTML(paste("<b> Última actualización:</b>", unique(dat_vivienda_pp()$actualizacion))))
  })
  
  # * Gráficos   ======================================================
  
  output$plot_vivienda_pp <- renderPlot({
    
    req(input$indicador_vivienda_pp, input$vivienda_pp_corte)
    
    if(input$indicador_vivienda_pp %in% lista_serie_cat){
      
      req(input$indicador_vivienda_pp)
      
      # Total
      if(input$vivienda_pp_corte == "Total"){
        
        dat_plot <- dat_vivienda_pp() %>%
          filter(corte == "Total")
        
        plot <- ggplot(dat_plot, aes(x = fecha_cat, y = Valor, group = 1)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_vivienda_pp),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador vivienda pp.png", width = 40, height = 25, units = "cm")
        
        # Según corte
      } else if(input$vivienda_pp_corte != "Total") {
        
        vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
        
        dat_plot <- dat_vivienda_pp() %>%
          filter(corte == input$vivienda_pp_corte) %>%
          filter(!!vivienda_pp_corte_var %in% input$checkbox_vivienda_pp)
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            colour = vivienda_pp_corte_var, group = vivienda_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_pp,
                                    "según",
                                    tolower(input$vivienda_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador vivienda pp.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_vivienda_pp %in% lista_especial ){
      
      if (input$vivienda_pp_corte_2 == "Total"){        
        
        req(input$vivienda_pp_corte, input$indicador_vivienda_pp)
        
        vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
        
        dat_plot <- dat_vivienda_pp() %>%
          filter(ano >= input$fecha_vivienda_pp[1] &
                   ano <= input$fecha_vivienda_pp[2]) %>%
          filter(corte == input$vivienda_pp_corte) %>%
          # filter(!!vivienda_pp_corte_var %in% input$checkbox_vivienda_pp) %>% 
          filter(corte_2 == input$vivienda_pp_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = vivienda_pp_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_pp,
                                    "según",
                                    tolower(input$vivienda_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador vivienda pp.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        req(input$vivienda_pp_corte, input$indicador_vivienda_pp)
        
        vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
        vivienda_pp_corte_var_2 <- rlang::sym(to_varname(input$vivienda_pp_corte_2))
        
        dat_plot <- dat_vivienda_pp() %>%
          filter(ano >= input$fecha_vivienda_pp[1] &
                   ano <= input$fecha_vivienda_pp[2]) %>%
          filter(corte == input$vivienda_pp_corte) %>%
          # filter(!!vivienda_pp_corte_var %in% input$checkbox_vivienda_pp) %>% 
          filter(corte_2 == input$vivienda_pp_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = vivienda_pp_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_pp,
                                    "según",
                                    tolower(input$vivienda_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", vivienda_pp_corte_var_2)))
        
        print(plot)
        ggsave("www/indicador vivienda pp.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_vivienda_pp %in% lista_vunico & input$indicador_vivienda_pp %in% lista_ind_2){
      
      req(input$vivienda_pp_corte, input$indicador_vivienda_pp)
      
      vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
      vivienda_pp_corte_var_2 <- rlang::sym(to_varname(input$vivienda_pp_corte_2))
      
      dat_plot <- dat_vivienda_pp() %>%
        filter(ano >= input$fecha_vivienda_pp[1] &
                 ano <= input$fecha_vivienda_pp[2]) %>%
        filter(corte == input$vivienda_pp_corte) %>%
        filter(!!vivienda_pp_corte_var %in% input$checkbox_vivienda_pp) %>%
        filter(!!vivienda_pp_corte_var_2 %in% input$checkbox_vivienda_pp_2) %>%
        filter(corte_2 == input$vivienda_pp_corte_2)
      
      plot <- ggplot(dat_plot,
                     aes_string(x = "fecha_cat", y = "Valor",
                                fill = vivienda_pp_corte_var)) +
        geom_col(position = "dodge", width = .7, alpha = .8) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x=element_blank(),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_vivienda_pp,
                                  "según",
                                  tolower(input$vivienda_pp_corte),
                                  "en",
                                  unique(dat_plot$fecha_cat))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_fill_brewer(name = "", palette = "Paired") +
        facet_wrap(as.formula(paste("~", vivienda_pp_corte_var_2)))
      
      print(plot)
      ggsave("www/indicador vivienda pp.png", width = 40, height = 25, units = "cm")
      
    } else if(input$indicador_vivienda_pp %in% lista_vunico & input$vivienda_pp_corte != "Departamento") {
      
      req(input$vivienda_pp_corte, input$indicador_vivienda_pp)
      
      dat_plot <- dat_vivienda_pp() %>%
        filter(corte == input$vivienda_pp_corte) %>%
        janitor::remove_empty("cols")
      
      if(input$vivienda_pp_corte == "Total"){
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor")) +
          geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
          geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_pp,
                                    "según",
                                    tolower(input$vivienda_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador vivienda pp.png", width = 40, height = 25, units = "cm")
        
        
      } else {
        
        vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor",
                                  fill = vivienda_pp_corte_var)) +
          geom_col(position = "dodge", alpha = .8, stroke = 1, color = "black") +
          geom_text(aes_string(group = vivienda_pp_corte_var, label = "Valor"),
                    position = position_dodge2(width = .9), vjust = -.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_pp,
                                    "según",
                                    tolower(input$vivienda_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador vivienda pp.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$indicador_vivienda_pp %in% lista_ind_2) {
      
      req(input$vivienda_pp_corte, input$vivienda_pp_corte_2,
          input$fecha_vivienda_pp, input$checkbox_vivienda_pp)
      
      vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
      vivienda_pp_corte_var_2 <- rlang::sym(to_varname(input$vivienda_pp_corte_2))
      
      dat_plot <- dat_vivienda_pp() %>%
        filter(ano >= input$fecha_vivienda_pp[1] &
                 ano <= input$fecha_vivienda_pp[2]) %>%
        filter(corte == input$vivienda_pp_corte) %>%
        filter(!!vivienda_pp_corte_var_2 %in% input$checkbox_vivienda_pp_2) %>%
        filter(!!vivienda_pp_corte_var %in% input$checkbox_vivienda_pp) %>% 
        filter(corte_2 == input$vivienda_pp_corte_2)
      
      if(input$vivienda_pp_corte_2 == "Total"){
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == "Total")
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = vivienda_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_pp,
                                    "según",
                                    tolower(input$vivienda_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
      } else if(input$vivienda_pp_corte_2 != "Total") {
        
        vivienda_pp_corte_var_2 <- rlang::sym(to_varname(input$vivienda_pp_corte_2))
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == input$vivienda_pp_corte_2)
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = vivienda_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          scale_x_continuous(breaks = int_breaks) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_pp,
                                    "según",
                                    tolower(input$vivienda_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          facet_wrap(as.formula(paste("~", vivienda_pp_corte_var_2)))
        
      }
      
      print(plot)
      ggsave("www/indicador vivienda pp.png", width = 40, height = 25, units = "cm")
      
      
      # Grafico simple normal
    } else if(input$vivienda_pp_corte == "Total") {
      
      req(input$indicador_vivienda_pp, input$fecha_vivienda_pp)
      
      dat_plot <- dat_vivienda_pp() %>%
        filter(ano >= input$fecha_vivienda_pp[1] &
                 ano <= input$fecha_vivienda_pp[2]) %>%
        filter(corte == "Total")
      
      if(input$indicador_vivienda_pp %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor, alpha = metodo)) +
          geom_line(size = 1, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          # scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "none") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_vivienda_pp),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8))
        
        print(plot)
        ggsave("www/indicador vivienda pp.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_vivienda_pp),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador vivienda pp.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$vivienda_pp_corte == "Departamento" &
              input$indicador_vivienda_pp %notin% lista_ind_2 ) {
      
      req(input$indicador_vivienda_pp, input$fecha_dpto_vivienda_pp)
      
      dat_plot <- dat_vivienda_pp() %>%
        filter(corte == "Departamento") %>%
        filter(ano == input$fecha_dpto_vivienda_pp) %>%
        select(departamento, Valor, fuente, cita)
      
      dep_j <- dep %>%
        left_join(dat_plot, by = c("nombre" = "departamento"))
      
      plot <-  ggplot(dep_j, aes(fill = Valor)) +
        geom_sf() +
        geom_sf_text(aes(label = Valor), colour = "black",
                     size = 4, fontface = "bold")+
        viridis::scale_fill_viridis(name = "", direction = -1)+
        labs(x = "",
             y = "",
        )+
        theme_bdd(base_size = 14) +
        theme(axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.grid.major = element_line(colour = "transparent"),
        ) +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_vivienda_pp,
                                  "en",
                                  input$fecha_dpto_vivienda_pp), w = 80),
             caption = wrapit(unique(dat_plot$cita), w = 80))
      
      print(plot)
      ggsave("www/indicador vivienda pp.png", width = 40, height = 25, units = "cm")
      
      
    } else if(input$vivienda_pp_corte != "Total") {
      
      req(input$vivienda_pp_corte, input$indicador_vivienda_pp,
          input$fecha_vivienda_pp, input$checkbox_vivienda_pp)
      
      dat_plot <- dat_vivienda_pp() %>%
        filter(ano >= input$fecha_vivienda_pp[1] &
                 ano <= input$fecha_vivienda_pp[2]) %>%
        filter(corte == input$vivienda_pp_corte) %>%
        janitor::remove_empty("cols")
      
      vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
      
      dat_plot <- filter(dat_plot,
                         !!vivienda_pp_corte_var %in% input$checkbox_vivienda_pp)
      
      if(input$indicador_vivienda_pp %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = vivienda_pp_corte_var, alpha = "metodo")) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_pp,
                                    "según",
                                    tolower(input$vivienda_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8)) +
          guides(alpha = "none")
        
        print(plot)
        ggsave("www/indicador vivienda pp.png", width = 40, height = 25, units = "cm")
        
        
      } else {
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = vivienda_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_pp,
                                    "según",
                                    tolower(input$vivienda_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador vivienda pp.png", width = 40, height = 25, units = "cm")
        
        
      }
      
      
    }
    
  })
  
  
  # * Descarga gráficos   =============================================
  
  output$baja_p_vivienda_pp <- downloadHandler(
    filename <- function() {
      paste("indicador vivienda pp.png", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/indicador vivienda pp.png", file)
    },
    contentType = "www/indicador vivienda pp"
  )
  
  
  # * Tablas   ========================================================
  
  # Data
  vivienda_pp_tab <- reactive({
    
    if(input$indicador_vivienda_pp %in%  lista_especial){
      
      vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
      vivienda_pp_corte_var_2 <- rlang::sym(to_varname(input$vivienda_pp_corte_2))
      
      dat_vivienda_pp() %>%
        filter(corte_2 == input$vivienda_pp_corte_2) %>% 
        select(Fecha, vivienda_pp_corte_var, vivienda_pp_corte_var_2, Valor) %>%
        arrange(desc(Fecha)) %>%
        pivot_wider(values_from = "Valor",
                    names_from = vivienda_pp_corte_var) 
      
    } else if(input$indicador_vivienda_pp %in% lista_vunico & input$vivienda_pp_corte == "Total"){
      
      req(input$vivienda_pp_corte, input$indicador_vivienda_pp)
      
      dat_vivienda_pp() %>%
        filter(corte == "Total") %>% 
        select(fecha_cat, Valor) %>%
        arrange(desc(fecha_cat)) %>%
        rename(Fecha = fecha_cat)
      
    } else if(input$indicador_vivienda_pp %in% lista_vunico & input$vivienda_pp_corte != "Total") {
      
      req(input$vivienda_pp_corte, input$indicador_vivienda_pp)
      
      vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
      
      dat_cut <- dat_vivienda_pp() %>%
        filter(corte == input$vivienda_pp_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        select(fecha_cat, vivienda_pp_corte_var, Valor) %>%
        arrange(desc(fecha_cat)) %>% 
        rename(Fecha = fecha_cat) %>%
        pivot_wider(values_from = "Valor",
                    names_from = vivienda_pp_corte_var)
      
      
    } else if(input$indicador_vivienda_pp %in% lista_ind_2) {
      
      req(input$vivienda_pp_corte, input$indicador_vivienda_pp, input$fecha_vivienda_pp)
      
      vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
      
      dat_cut <- dat_vivienda_pp() %>%
        filter(corte == input$vivienda_pp_corte) %>%
        filter(!!vivienda_pp_corte_var %in% input$checkbox_vivienda_pp)
      
      if(input$vivienda_pp_corte_2 == "Total"){
        
        dat_cut %>%
          filter(ano >= input$fecha_vivienda_pp[1] &
                   ano <= input$fecha_vivienda_pp[2]) %>%
          filter(corte_2 == "Total") %>% 
          select(Fecha, vivienda_pp_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = vivienda_pp_corte_var)
        
      } else if(input$vivienda_pp_corte_2 != "Total") {
        
        vivienda_pp_corte_var_2 <- rlang::sym(to_varname(input$vivienda_pp_corte_2))
        
        dat_cut %>%
          filter(ano >= input$fecha_vivienda_pp[1] &
                   ano <= input$fecha_vivienda_pp[2]) %>%
          filter(corte_2 == input$vivienda_pp_corte_2) %>% 
          select(Fecha, vivienda_pp_corte_var, vivienda_pp_corte_var_2, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = vivienda_pp_corte_var) 
        
      }
      
    } else if(input$vivienda_pp_corte == "Total") {
      
      req(input$vivienda_pp_corte, input$indicador_vivienda_pp, input$fecha_vivienda_pp)
      
      dat_vivienda_pp() %>%
        filter(corte == "Total") %>% 
        filter(ano >= input$fecha_vivienda_pp[1] &
                 ano <= input$fecha_vivienda_pp[2]) %>%
        select(Fecha, Valor) %>%
        arrange(desc(Fecha))
      
    } else if(input$vivienda_pp_corte != "Total") {
      
      req(input$vivienda_pp_corte, input$indicador_vivienda_pp, input$fecha_vivienda_pp)
      
      vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
      
      dat_cut <- dat_vivienda_pp() %>%
        filter(corte == input$vivienda_pp_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        filter(ano >= input$fecha_vivienda_pp[1] &
                 ano <= input$fecha_vivienda_pp[2]) %>% 
        select(Fecha, vivienda_pp_corte_var, Valor) %>%
        arrange(desc(Fecha)) %>% 
        pivot_wider(values_from = "Valor",
                    names_from = vivienda_pp_corte_var)
      
    }
  })
  
  # Metadata 
  vivienda_pp_meta <- reactive({
    
    dat_vivienda_pp() %>%
      select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, observaciones, actualizacion, cita) %>% 
      mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
      distinct() %>% 
      gather(key = " ", value = " ")
    
  })
  
  # Excel
  list_vivienda_pp <- reactive({
    list_vivienda_pp <- list("Data" = vivienda_pp_tab(),
                             "Metadata" = vivienda_pp_meta())
  })
  
  # Render
  output$table_vivienda_pp <- renderDT({
    
    DT::datatable(vivienda_pp_tab(),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    input$indicador_vivienda_pp,
                    style = "color:black; font-size:110%;")
    ) 
    
  })
  
  # * Descarga tablas   ================================================
  
  output$dwl_tab_vivienda_pp <- downloadHandler(
    
    filename = function() {
      paste("resultados-", input$indicador_vivienda_pp, ".xlsx", sep = "")
    },
    content = function(file) {
      
      openxlsx::write.xlsx(list_vivienda_pp(), file)
      
    }
  )
  
  
  ### 6.2. Vivienda Resultados ==============================================
  
  # * Data reactiva   =================================================    
  dat_vivienda_r <- reactive({
    
    req(input$indicador_vivienda_r)
    
    dat %>%
      filter(nomindicador == input$indicador_vivienda_r) 
    
  })
  
  output$selector_vivienda_r_corte_2 <- renderUI({
    
    if(input$indicador_vivienda_r %in% lista_ind_2){
      
      selectInput(
        inputId = "vivienda_r_corte_2",
        label = "Seleccione primer corte:",
        choices = dat_vivienda_r() %>% 
          select(corte_2) %>%
          arrange(corte_2) %>% 
          unique() %>% 
          pull(),
        selected = dat_vivienda_r() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte_2) %>% 
          pull()
      )
      
    } else {
      
      NULL
    }
    
  })
  
  output$chbox_vivienda_r_2 <- renderUI({
    
    if(input$indicador_vivienda_r %in% lista_ind_2 & input$indicador_vivienda_r %notin% lista_especial){
      
      # if(input$vivienda_r_corte %notin% c("Total", "Departamento") & input$indicador_vivienda_r %notin% lista_vunico) {
      
      vivienda_r_corte_var_2 <- rlang::sym(to_varname(input$vivienda_r_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_vivienda_r_2",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_vivienda_r() %>%
                           filter(corte_2 == input$vivienda_r_corte_2) %>% 
                           distinct(!!vivienda_r_corte_var_2) %>%
                           pull(),
                         selected = dat_vivienda_r() %>%
                           filter(corte_2 == input$vivienda_r_corte_2) %>% 
                           filter(jerarquia_cat_2 == "1") %>%
                           distinct(!!vivienda_r_corte_var_2) %>%
                           pull()
      )
      
      # } else {
      #   
      #   return(NULL)
      #   
      #   }
      
    } else {
      
      return(NULL)
      
    }      
  })
  
  
  output$selector_vivienda_r_corte <- renderUI({
    
    selectInput(
      inputId = "vivienda_r_corte",
      label = "Seleccione corte:",
      choices = dat_vivienda_r() %>% 
        select(corte) %>%
        arrange(corte) %>% 
        unique() %>% 
        pull(),
      selected = dat_vivienda_r() %>% 
        filter(jerarquia == "1") %>%  
        distinct(corte) %>% 
        pull()
    )
    
  })
  
  output$chbox_vivienda_r <- renderUI({
    
    if(input$vivienda_r_corte %in% lista_ind_2 & input$vivienda_r_corte %notin% c("Total", "Departamento") & input$indicador_vivienda_r %notin% lista_vunico) {
      
      vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
      vivienda_r_corte_var_2 <- rlang::sym(to_varname(input$vivienda_r_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_vivienda_r",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_vivienda_r() %>%
                           filter(!!vivienda_r_corte_var_2 %in% input$checkbox_vivienda_r_2) %>%
                           filter(corte == input$vivienda_r_corte) %>% 
                           distinct(!!vivienda_r_corte_var) %>%
                           pull(),
                         selected = dat_vivienda_r() %>%
                           filter(!!vivienda_r_corte_var_2 %in% input$checkbox_vivienda_r_2) %>%
                           filter(corte == input$vivienda_r_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!vivienda_r_corte_var) %>%
                           pull()
      )
      
    } else if(input$vivienda_r_corte %notin% lista_ind_2 & input$vivienda_r_corte %notin% c("Total", "Departamento") & input$indicador_vivienda_r %notin% lista_vunico) {
      
      vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
      
      checkboxGroupInput(inputId = "checkbox_vivienda_r",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_vivienda_r() %>%
                           filter(corte == input$vivienda_r_corte) %>% 
                           distinct(!!vivienda_r_corte_var) %>%
                           pull(),
                         selected = dat_vivienda_r() %>%
                           filter(corte == input$vivienda_r_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!vivienda_r_corte_var) %>%
                           pull()
      )
      
    } else {
      
      return(NULL)
    }
    
  })
  
  # Selector de fecha
  output$s_vivienda_r_fecha <- renderUI({
    
    if(input$vivienda_r_corte == "Departamento" & input$indicador_vivienda_r %notin% lista_ind_2) {
      
      req(input$vivienda_r_corte, input$indicador_vivienda_r)
      
      req(nrow(dat_vivienda_r()) > 0)
      
      selectInput(
        inputId = "fecha_dpto_vivienda_r",
        label = "Seleccione año:",
        choices = dat_vivienda_r() %>% 
          filter(nomindicador == input$indicador_vivienda_r) %>%
          drop_na(Valor) %>%
          select(ano) %>%
          arrange(desc(ano)) %>% 
          unique() %>% 
          pull(),
        selected = max(input$ano)
      )
      
    } else if (input$indicador_vivienda_r %in% lista_serie_cat){
      
      return(NULL)
      
      
    } else if (input$indicador_vivienda_r %in% lista_vunico){
      
      return(NULL)
      
    } else  {
      
      req(input$vivienda_r_corte, input$indicador_vivienda_r)
      req(nrow(dat_vivienda_r()) > 0)
      
      tagList(
        # tags$style(type = 'text/css', 
        #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
        # div(id = 'big_slider',
        
        sliderInput("fecha_vivienda_r", 
                    label = "Rango de tiempo", 
                    sep = "",
                    dragRange = T,
                    min = min(dat_vivienda_r()$ano), 
                    max = max(dat_vivienda_r()$ano), 
                    value = c(min(dat_vivienda_r()$ano), 
                              max(dat_vivienda_r()$ano))
        )
      )
      
    }
  })
  
  # # Selector de corte según categoría y data temporal
  # dat_vivienda_r <- reactive({
  #   
  #   req(input$vivienda_r_corte)
  #   
  #   if(input$indicador_vivienda_r %in% lista_ind_2){
  #     
  #     dat_salarios <- dat_vivienda_r() %>%
  #       filter(corte_2 == input$vivienda_r_corte_2) %>% 
  #       filter(corte == input$vivienda_r_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   } else {
  #     
  #     dat_vivienda_r() %>%
  #       filter(corte == input$vivienda_r_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   }
  # })
  
  # * Metadata   ======================================================
  
  # Title
  output$title_vivienda_r <- renderUI({ 
    helpText(HTML(unique(dat_vivienda_r()$nomindicador)))
  })
  
  # Subtitle
  output$subtitle_vivienda_r <- renderUI({ 
    helpText(HTML(unique(dat_vivienda_r()$definicion)))
  })
  
  # Nota
  output$nota_vivienda_r <- renderUI({ 
    
    if(input$indicador_vivienda_r %in% lista_nota){
      helpText(HTML(unique(dat_vivienda_r()$nota)))
      
    } else(NULL)
  })
  
  
  # Nombre conceptual
  output$conindicador_vivienda_r <- renderUI({ 
    helpText(HTML(paste("<b> Nombre conceptual:</b>", unique(dat_vivienda_r()$conindicador))))
  })
  
  # Calculo
  output$calculo_vivienda_r <- renderUI({ 
    helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_vivienda_r()$calculo))))
  })
  
  # Observaciones
  output$observacion_vivienda_r <- renderUI({ 
    helpText(HTML(paste("<b> Observaciones:</b>", unique(dat_vivienda_r()$observaciones))))
  })
  
  # Actualización
  output$actualizacion_vivienda_r <- renderUI({ 
    helpText(HTML(paste("<b> Última actualización:</b>", unique(dat_vivienda_r()$actualizacion))))
  })
  
  
  # * Gráficos   ======================================================
  
  output$plot_vivienda_r <- renderPlot({
    
    req(input$indicador_vivienda_r, input$vivienda_r_corte)
    
    if(input$indicador_vivienda_r %in% lista_serie_cat){
      
      req(input$indicador_vivienda_r)
      
      # Total
      if(input$vivienda_r_corte == "Total"){
        
        dat_plot <- dat_vivienda_r() %>%
          filter(corte == "Total")
        
        plot <- ggplot(dat_plot, aes(x = fecha_cat, y = Valor, group = 1)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_vivienda_r),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador vivienda r.png", width = 40, height = 25, units = "cm")
        
        # Según corte
      } else if(input$vivienda_r_corte != "Total") {
        
        vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
        
        dat_plot <- dat_vivienda_r() %>%
          filter(corte == input$vivienda_r_corte) %>%
          filter(!!vivienda_r_corte_var %in% input$checkbox_vivienda_r)
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            colour = vivienda_r_corte_var, group = vivienda_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_r,
                                    "según",
                                    tolower(input$vivienda_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador vivienda r.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_vivienda_r %in% lista_especial ){
      
      if (input$vivienda_r_corte_2 == "Total"){        
        
        req(input$vivienda_r_corte, input$indicador_vivienda_r)
        
        vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
        
        dat_plot <- dat_vivienda_r() %>%
          filter(ano >= input$fecha_vivienda_r[1] &
                   ano <= input$fecha_vivienda_r[2]) %>%
          filter(corte == input$vivienda_r_corte) %>%
          # filter(!!vivienda_r_corte_var %in% input$checkbox_vivienda_r) %>% 
          filter(corte_2 == input$vivienda_r_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = vivienda_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_r,
                                    "según",
                                    tolower(input$vivienda_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador vivienda r.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        req(input$vivienda_r_corte, input$indicador_vivienda_r)
        
        vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
        vivienda_r_corte_var_2 <- rlang::sym(to_varname(input$vivienda_r_corte_2))
        
        dat_plot <- dat_vivienda_r() %>%
          filter(ano >= input$fecha_vivienda_r[1] &
                   ano <= input$fecha_vivienda_r[2]) %>%
          filter(corte == input$vivienda_r_corte) %>%
          # filter(!!vivienda_r_corte_var %in% input$checkbox_vivienda_r) %>% 
          filter(corte_2 == input$vivienda_r_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = vivienda_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_r,
                                    "según",
                                    tolower(input$vivienda_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", vivienda_r_corte_var_2)))
        
        print(plot)
        ggsave("www/indicador vivienda r.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_vivienda_r %in% lista_vunico & input$indicador_vivienda_r %in% lista_ind_2){
      
      req(input$vivienda_r_corte, input$indicador_vivienda_r)
      
      vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
      vivienda_r_corte_var_2 <- rlang::sym(to_varname(input$vivienda_r_corte_2))
      
      dat_plot <- dat_vivienda_r() %>%
        filter(ano >= input$fecha_vivienda_r[1] &
                 ano <= input$fecha_vivienda_r[2]) %>%
        filter(corte == input$vivienda_r_corte) %>%
        filter(!!vivienda_r_corte_var %in% input$checkbox_vivienda_r) %>%
        filter(!!vivienda_r_corte_var_2 %in% input$checkbox_vivienda_r_2) %>%
        filter(corte_2 == input$vivienda_r_corte_2)
      
      plot <- ggplot(dat_plot,
                     aes_string(x = "fecha_cat", y = "Valor",
                                fill = vivienda_r_corte_var)) +
        geom_col(position = "dodge", width = .7, alpha = .8) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x=element_blank(),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_vivienda_r,
                                  "según",
                                  tolower(input$vivienda_r_corte),
                                  "en",
                                  unique(dat_plot$fecha_cat))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_fill_brewer(name = "", palette = "Paired") +
        facet_wrap(as.formula(paste("~", vivienda_r_corte_var_2)))
      
      print(plot)
      ggsave("www/indicador vivienda r.png", width = 40, height = 25, units = "cm")
      
    } else if(input$indicador_vivienda_r %in% lista_vunico & input$vivienda_r_corte != "Departamento") {
      
      req(input$vivienda_r_corte, input$indicador_vivienda_r)
      
      dat_plot <- dat_vivienda_r() %>%
        filter(corte == input$vivienda_r_corte) %>%
        janitor::remove_empty("cols")
      
      if(input$vivienda_r_corte == "Total"){
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor")) +
          geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
          geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_r,
                                    "según",
                                    tolower(input$vivienda_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador vivienda r.png", width = 40, height = 25, units = "cm")
        
        
      } else {
        
        vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor",
                                  fill = vivienda_r_corte_var)) +
          geom_col(position = "dodge", alpha = .8, stroke = 1, color = "black") +
          geom_text(aes_string(group = vivienda_r_corte_var, label = "Valor"),
                    position = position_dodge2(width = .9), vjust = -.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_r,
                                    "según",
                                    tolower(input$vivienda_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador vivienda r.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$indicador_vivienda_r %in% lista_ind_2) {
      
      req(input$vivienda_r_corte, input$vivienda_r_corte_2,
          input$fecha_vivienda_r, input$checkbox_vivienda_r)
      
      vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
      vivienda_r_corte_var_2 <- rlang::sym(to_varname(input$vivienda_r_corte_2))
      
      dat_plot <- dat_vivienda_r() %>%
        filter(ano >= input$fecha_vivienda_r[1] &
                 ano <= input$fecha_vivienda_r[2]) %>%
        filter(corte == input$vivienda_r_corte) %>%
        filter(!!vivienda_r_corte_var_2 %in% input$checkbox_vivienda_r_2) %>%
        filter(!!vivienda_r_corte_var %in% input$checkbox_vivienda_r) %>% 
        filter(corte_2 == input$vivienda_r_corte_2)
      
      if(input$vivienda_r_corte_2 == "Total"){
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == "Total")
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = vivienda_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_r,
                                    "según",
                                    tolower(input$vivienda_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
      } else if(input$vivienda_r_corte_2 != "Total") {
        
        vivienda_r_corte_var_2 <- rlang::sym(to_varname(input$vivienda_r_corte_2))
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == input$vivienda_r_corte_2)
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = vivienda_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          scale_x_continuous(breaks = int_breaks) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_r,
                                    "según",
                                    tolower(input$vivienda_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          facet_wrap(as.formula(paste("~", vivienda_r_corte_var_2)))
        
      }
      
      print(plot)
      ggsave("www/indicador vivienda r.png", width = 40, height = 25, units = "cm")
      
      
      # Grafico simple normal
    } else if(input$vivienda_r_corte == "Total") {
      
      req(input$indicador_vivienda_r, input$fecha_vivienda_r)
      
      dat_plot <- dat_vivienda_r() %>%
        filter(ano >= input$fecha_vivienda_r[1] &
                 ano <= input$fecha_vivienda_r[2]) %>%
        filter(corte == "Total")
      
      if(input$indicador_vivienda_r %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor, alpha = metodo)) +
          geom_line(size = 1, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          # scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "none") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_vivienda_r),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8))
        
        print(plot)
        ggsave("www/indicador vivienda r.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_vivienda_r),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador vivienda r.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$vivienda_r_corte == "Departamento" &
              input$indicador_vivienda_r %notin% lista_ind_2 ) {
      
      req(input$indicador_vivienda_r, input$fecha_dpto_vivienda_r)
      
      dat_plot <- dat_vivienda_r() %>%
        filter(corte == "Departamento") %>%
        filter(ano == input$fecha_dpto_vivienda_r) %>%
        select(departamento, Valor, fuente, cita)
      
      dep_j <- dep %>%
        left_join(dat_plot, by = c("nombre" = "departamento"))
      
      plot <-  ggplot(dep_j, aes(fill = Valor)) +
        geom_sf() +
        geom_sf_text(aes(label = Valor), colour = "black",
                     size = 4, fontface = "bold")+
        viridis::scale_fill_viridis(name = "", direction = -1)+
        labs(x = "",
             y = "",
        )+
        theme_bdd(base_size = 14) +
        theme(axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.grid.major = element_line(colour = "transparent"),
        ) +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_vivienda_r,
                                  "en",
                                  input$fecha_dpto_vivienda_r), w = 80),
             caption = wrapit(unique(dat_plot$cita), w = 80))
      
      print(plot)
      ggsave("www/indicador vivienda r.png", width = 40, height = 25, units = "cm")
      
      
    } else if(input$vivienda_r_corte != "Total") {
      
      req(input$vivienda_r_corte, input$indicador_vivienda_r,
          input$fecha_vivienda_r, input$checkbox_vivienda_r)
      
      dat_plot <- dat_vivienda_r() %>%
        filter(ano >= input$fecha_vivienda_r[1] &
                 ano <= input$fecha_vivienda_r[2]) %>%
        filter(corte == input$vivienda_r_corte) %>%
        janitor::remove_empty("cols")
      
      vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
      
      dat_plot <- filter(dat_plot,
                         !!vivienda_r_corte_var %in% input$checkbox_vivienda_r)
      
      if(input$indicador_vivienda_r %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = vivienda_r_corte_var, alpha = "metodo")) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_r,
                                    "según",
                                    tolower(input$vivienda_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8)) +
          guides(alpha = "none")
        
        print(plot)
        ggsave("www/indicador vivienda r.png", width = 40, height = 25, units = "cm")      
        
      } else {
        
        vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
        
        dat_plot <- dat_vivienda_r() %>%
          filter(corte == input$vivienda_r_corte) %>%
          filter(!!vivienda_r_corte_var %in% input$checkbox_vivienda_r)
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            colour = vivienda_r_corte_var, group = vivienda_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_r,
                                    "según",
                                    tolower(input$vivienda_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador vivienda r.png", width = 40, height = 25, units = "cm")
        
      }
    }
  })
  
  
  # * Descarga gráficos   =============================================
  
  output$baja_p_vivienda_r <- downloadHandler(
    filename <- function() {
      paste("indicador vivienda r", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/indicador vivienda r.png", file)
    },
    contentType = "www/indicador vivienda r"
  )
  
  
  # * Tablas   ========================================================
  
  # Data
  vivienda_r_tab <- reactive({
    
    if(input$indicador_vivienda_r %in%  lista_especial){
      
      vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
      vivienda_r_corte_var_2 <- rlang::sym(to_varname(input$vivienda_r_corte_2))
      
      dat_vivienda_r() %>%
        filter(corte_2 == input$vivienda_r_corte_2) %>% 
        select(Fecha, vivienda_r_corte_var, vivienda_r_corte_var_2, Valor) %>%
        arrange(desc(Fecha)) %>%
        pivot_wider(values_from = "Valor",
                    names_from = vivienda_r_corte_var) 
      
    } else if(input$indicador_vivienda_r %in% lista_vunico & input$vivienda_r_corte == "Total"){
      
      req(input$vivienda_r_corte, input$indicador_vivienda_r)
      
      dat_vivienda_r() %>%
        filter(corte == "Total") %>% 
        select(fecha_cat, Valor) %>%
        arrange(desc(fecha_cat)) %>%
        rename(Fecha = fecha_cat)
      
    } else if(input$indicador_vivienda_r %in% lista_vunico & input$vivienda_r_corte != "Total") {
      
      req(input$vivienda_r_corte, input$indicador_vivienda_r)
      
      vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
      
      dat_cut <- dat_vivienda_r() %>%
        filter(corte == input$vivienda_r_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        select(fecha_cat, vivienda_r_corte_var, Valor) %>%
        arrange(desc(fecha_cat)) %>% 
        rename(Fecha = fecha_cat) %>%
        pivot_wider(values_from = "Valor",
                    names_from = vivienda_r_corte_var)
      
      
    } else if(input$indicador_vivienda_r %in% lista_ind_2) {
      
      req(input$vivienda_r_corte, input$indicador_vivienda_r, input$fecha_vivienda_r)
      
      vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
      
      dat_cut <- dat_vivienda_r() %>%
        filter(corte == input$vivienda_r_corte) %>%
        filter(!!vivienda_r_corte_var %in% input$checkbox_vivienda_r)
      
      if(input$vivienda_r_corte_2 == "Total"){
        
        dat_cut %>%
          filter(ano >= input$fecha_vivienda_r[1] &
                   ano <= input$fecha_vivienda_r[2]) %>%
          filter(corte_2 == "Total") %>% 
          select(Fecha, vivienda_r_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = vivienda_r_corte_var)
        
      } else if(input$vivienda_r_corte_2 != "Total") {
        
        vivienda_r_corte_var_2 <- rlang::sym(to_varname(input$vivienda_r_corte_2))
        
        dat_cut %>%
          filter(ano >= input$fecha_vivienda_r[1] &
                   ano <= input$fecha_vivienda_r[2]) %>%
          filter(corte_2 == input$vivienda_r_corte_2) %>% 
          select(Fecha, vivienda_r_corte_var, vivienda_r_corte_var_2, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = vivienda_r_corte_var) 
        
      }
      
    } else if(input$vivienda_r_corte == "Total") {
      
      req(input$vivienda_r_corte, input$indicador_vivienda_r, input$fecha_vivienda_r)
      
      dat_vivienda_r() %>%
        filter(corte == "Total") %>% 
        filter(ano >= input$fecha_vivienda_r[1] &
                 ano <= input$fecha_vivienda_r[2]) %>%
        select(Fecha, Valor) %>%
        arrange(desc(Fecha))
      
    } else if(input$vivienda_r_corte != "Total") {
      
      req(input$vivienda_r_corte, input$indicador_vivienda_r, input$fecha_vivienda_r)
      
      vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
      
      dat_cut <- dat_vivienda_r() %>%
        filter(corte == input$vivienda_r_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        filter(ano >= input$fecha_vivienda_r[1] &
                 ano <= input$fecha_vivienda_r[2]) %>% 
        select(Fecha, vivienda_r_corte_var, Valor) %>%
        arrange(desc(Fecha)) %>% 
        pivot_wider(values_from = "Valor",
                    names_from = vivienda_r_corte_var)
      
    }
  })
  
  # Metadata 
  vivienda_r_meta <- reactive({
    
    dat_vivienda_r() %>%
      select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, observaciones, actualizacion, cita) %>% 
      mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
      distinct() %>% 
      gather(key = " ", value = " ")
    
  })
  
  # Excel
  list_vivienda_r <- reactive({
    list_vivienda_r <- list("Data" = vivienda_r_tab(),
                            "Metadata" = vivienda_r_meta())
  })
  
  # Render
  output$table_vivienda_r <- renderDT({
    
    DT::datatable(vivienda_r_tab(),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    input$indicador_vivienda_r,
                    style = "color:black; font-size:110%;")
    ) 
    
  })
  
  # * Descarga tablas   ================================================
  
  output$dwl_tab_vivienda_r <- downloadHandler(
    
    filename = function() {
      paste("resultados-", input$indicador_vivienda_r, ".xlsx", sep = "")
    },
    content = function(file) {
      
      openxlsx::write.xlsx(list_vivienda_r(), file)
      
    }
  )
  
  
  
  ### 7.1. Trabajo Politicas ==============================================
  
  # * Data reactiva   =================================================    
  dat_trabajo_pp <- reactive({
    
    req(input$indicador_trabajo_pp)
    
    dat %>%
      filter(nomindicador == input$indicador_trabajo_pp) 
    
  })
  
  output$selector_trabajo_pp_corte_2 <- renderUI({
    
    if(input$indicador_trabajo_pp %in% lista_ind_2){
      
      selectInput(
        inputId = "trabajo_pp_corte_2",
        label = "Seleccione primer corte:",
        choices = dat_trabajo_pp() %>% 
          select(corte_2) %>%
          arrange(corte_2) %>% 
          unique() %>% 
          pull(),
        selected = dat_trabajo_pp() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte_2) %>% 
          pull()
      )
      
    } else {
      
      NULL
    }
    
  })
  
  output$chbox_trabajo_pp_2 <- renderUI({
    
    if(input$indicador_trabajo_pp %in% lista_ind_2 & input$indicador_trabajo_pp %notin% lista_especial){
      
      # if(input$trabajo_pp_corte %notin% c("Total", "Departamento") & input$indicador_trabajo_pp %notin% lista_vunico) {
      
      trabajo_pp_corte_var_2 <- rlang::sym(to_varname(input$trabajo_pp_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_trabajo_pp_2",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_trabajo_pp() %>%
                           filter(corte_2 == input$trabajo_pp_corte_2) %>% 
                           distinct(!!trabajo_pp_corte_var_2) %>%
                           pull(),
                         selected = dat_trabajo_pp() %>%
                           filter(corte_2 == input$trabajo_pp_corte_2) %>% 
                           filter(jerarquia_cat_2 == "1") %>%
                           distinct(!!trabajo_pp_corte_var_2) %>%
                           pull()
      )
      
      # } else {
      #   
      #   return(NULL)
      #   
      #   }
      
    } else {
      
      return(NULL)
      
    }      
  })
  
  
  output$selector_trabajo_pp_corte <- renderUI({
    
    selectInput(
      inputId = "trabajo_pp_corte",
      label = "Seleccione corte:",
      choices = dat_trabajo_pp() %>% 
        select(corte) %>%
        arrange(corte) %>% 
        unique() %>% 
        pull(),
      selected = dat_trabajo_pp() %>% 
        filter(jerarquia == "1") %>%  
        distinct(corte) %>% 
        pull()
    )
    
  })
  
  output$chbox_trabajo_pp <- renderUI({
    
    if(input$trabajo_pp_corte %in% lista_ind_2 & input$trabajo_pp_corte %notin% c("Total", "Departamento") & input$indicador_trabajo_pp %notin% lista_vunico) {
      
      trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
      trabajo_pp_corte_var_2 <- rlang::sym(to_varname(input$trabajo_pp_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_trabajo_pp",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_trabajo_pp() %>%
                           filter(!!trabajo_pp_corte_var_2 %in% input$checkbox_trabajo_pp_2) %>%
                           filter(corte == input$trabajo_pp_corte) %>% 
                           distinct(!!trabajo_pp_corte_var) %>%
                           pull(),
                         selected = dat_trabajo_pp() %>%
                           filter(!!trabajo_pp_corte_var_2 %in% input$checkbox_trabajo_pp_2) %>%
                           filter(corte == input$trabajo_pp_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!trabajo_pp_corte_var) %>%
                           pull()
      )
      
    } else if(input$trabajo_pp_corte %notin% lista_ind_2 & input$trabajo_pp_corte %notin% c("Total", "Departamento") & input$indicador_trabajo_pp %notin% lista_vunico) {
      
      trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
      
      checkboxGroupInput(inputId = "checkbox_trabajo_pp",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_trabajo_pp() %>%
                           filter(corte == input$trabajo_pp_corte) %>% 
                           distinct(!!trabajo_pp_corte_var) %>%
                           pull(),
                         selected = dat_trabajo_pp() %>%
                           filter(corte == input$trabajo_pp_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!trabajo_pp_corte_var) %>%
                           pull()
      )
      
    } else {
      
      return(NULL)
    }
    
  })
  
  # Selector de fecha
  output$s_trabajo_pp_fecha <- renderUI({
    
    if(input$trabajo_pp_corte == "Departamento" & input$indicador_trabajo_pp %notin% lista_ind_2) {
      
      req(input$trabajo_pp_corte, input$indicador_trabajo_pp)
      
      req(nrow(dat_trabajo_pp()) > 0)
      
      selectInput(
        inputId = "fecha_dpto_trabajo_pp",
        label = "Seleccione año:",
        choices = dat_trabajo_pp() %>% 
          filter(nomindicador == input$indicador_trabajo_pp) %>%
          drop_na(Valor) %>%
          select(ano) %>%
          arrange(desc(ano)) %>% 
          unique() %>% 
          pull(),
        selected = max(input$ano)
      )
      
    } else if (input$indicador_trabajo_pp %in% lista_serie_cat){
      
      return(NULL)
      
      
    } else if (input$indicador_trabajo_pp %in% lista_vunico){
      
      return(NULL)
      
    } else  {
      
      req(input$trabajo_pp_corte, input$indicador_trabajo_pp)
      req(nrow(dat_trabajo_pp()) > 0)
      
      tagList(
        # tags$style(type = 'text/css', 
        #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
        # div(id = 'big_slider',
        
        sliderInput("fecha_trabajo_pp", 
                    label = "Rango de tiempo", 
                    sep = "",
                    dragRange = T,
                    min = min(dat_trabajo_pp()$ano), 
                    max = max(dat_trabajo_pp()$ano), 
                    value = c(min(dat_trabajo_pp()$ano), 
                              max(dat_trabajo_pp()$ano))
        )
      )
      
    }
  })
  
  # # Selector de corte según categoría y data temporal
  # dat_trabajo_pp <- reactive({
  #   
  #   req(input$trabajo_pp_corte)
  #   
  #   if(input$indicador_trabajo_pp %in% lista_ind_2){
  #     
  #     dat_salarios <- dat_trabajo_pp() %>%
  #       filter(corte_2 == input$trabajo_pp_corte_2) %>% 
  #       filter(corte == input$trabajo_pp_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   } else {
  #     
  #     dat_trabajo_pp() %>%
  #       filter(corte == input$trabajo_pp_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   }
  # })
  
  # * Metadata   ======================================================
  
  # Title
  output$title_trabajo_pp <- renderUI({ 
    helpText(HTML(unique(dat_trabajo_pp()$nomindicador)))
  })
  
  # Subtitle
  output$subtitle_trabajo_pp <- renderUI({ 
    helpText(HTML(unique(dat_trabajo_pp()$definicion)))
  })
  
  # Nombre conceptual
  output$conindicador_trabajo_pp <- renderUI({ 
    helpText(HTML(paste("<b> Nombre conceptual:</b>", unique(dat_trabajo_pp()$conindicador))))
  })
  
  # Calculo
  output$calculo_trabajo_pp <- renderUI({ 
    helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_trabajo_pp()$calculo))))
  })
  
  # Observaciones
  output$observacion_trabajo_pp <- renderUI({ 
    helpText(HTML(paste("<b> Observaciones:</b>", unique(dat_trabajo_pp()$observaciones))))
  })
  
  # Actualización
  output$actualizacion_trabajo_pp <- renderUI({ 
    helpText(HTML(paste("<b> Última actualización:</b>", unique(dat_trabajo_pp()$actualizacion))))
  })
  
  # * Gráficos   ======================================================
  
  output$plot_trabajo_pp <- renderPlot({
    
    req(input$indicador_trabajo_pp, input$trabajo_pp_corte)
    
    if(input$indicador_trabajo_pp %in% lista_serie_cat){
      
      req(input$indicador_trabajo_pp)
      
      # Total
      if(input$trabajo_pp_corte == "Total"){
        
        dat_plot <- dat_trabajo_pp() %>%
          filter(corte == "Total")
        
        plot <- ggplot(dat_plot, aes(x = fecha_cat, y = Valor, group = 1)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_trabajo_pp),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador trabajo pp.png", width = 40, height = 25, units = "cm")
        
        # Según corte
      } else if(input$trabajo_pp_corte != "Total") {
        
        trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
        
        dat_plot <- dat_trabajo_pp() %>%
          filter(corte == input$trabajo_pp_corte) %>%
          filter(!!trabajo_pp_corte_var %in% input$checkbox_trabajo_pp)
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            colour = trabajo_pp_corte_var, group = trabajo_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_pp,
                                    "según",
                                    tolower(input$trabajo_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador trabajo pp.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_trabajo_pp %in% lista_especial ){
      
      if (input$trabajo_pp_corte_2 == "Total"){        
        
        req(input$trabajo_pp_corte, input$indicador_trabajo_pp)
        
        trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
        
        dat_plot <- dat_trabajo_pp() %>%
          filter(ano >= input$fecha_trabajo_pp[1] &
                   ano <= input$fecha_trabajo_pp[2]) %>%
          filter(corte == input$trabajo_pp_corte) %>%
          # filter(!!trabajo_pp_corte_var %in% input$checkbox_trabajo_pp) %>% 
          filter(corte_2 == input$trabajo_pp_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = trabajo_pp_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_pp,
                                    "según",
                                    tolower(input$trabajo_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador trabajo pp.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        req(input$trabajo_pp_corte, input$indicador_trabajo_pp)
        
        trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
        trabajo_pp_corte_var_2 <- rlang::sym(to_varname(input$trabajo_pp_corte_2))
        
        dat_plot <- dat_trabajo_pp() %>%
          filter(ano >= input$fecha_trabajo_pp[1] &
                   ano <= input$fecha_trabajo_pp[2]) %>%
          filter(corte == input$trabajo_pp_corte) %>%
          # filter(!!trabajo_pp_corte_var %in% input$checkbox_trabajo_pp) %>% 
          filter(corte_2 == input$trabajo_pp_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = trabajo_pp_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_pp,
                                    "según",
                                    tolower(input$trabajo_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", trabajo_pp_corte_var_2)))
        
        print(plot)
        ggsave("www/indicador trabajo pp.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_trabajo_pp %in% lista_vunico & input$indicador_trabajo_pp %in% lista_ind_2){
      
      req(input$trabajo_pp_corte, input$indicador_trabajo_pp)
      
      trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
      trabajo_pp_corte_var_2 <- rlang::sym(to_varname(input$trabajo_pp_corte_2))
      
      dat_plot <- dat_trabajo_pp() %>%
        filter(ano >= input$fecha_trabajo_pp[1] &
                 ano <= input$fecha_trabajo_pp[2]) %>%
        filter(corte == input$trabajo_pp_corte) %>%
        filter(!!trabajo_pp_corte_var %in% input$checkbox_trabajo_pp) %>%
        filter(!!trabajo_pp_corte_var_2 %in% input$checkbox_trabajo_pp_2) %>%
        filter(corte_2 == input$trabajo_pp_corte_2)
      
      plot <- ggplot(dat_plot,
                     aes_string(x = "fecha_cat", y = "Valor",
                                fill = trabajo_pp_corte_var)) +
        geom_col(position = "dodge", width = .7, alpha = .8) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x=element_blank(),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_trabajo_pp,
                                  "según",
                                  tolower(input$trabajo_pp_corte),
                                  "en",
                                  unique(dat_plot$fecha_cat))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_fill_brewer(name = "", palette = "Paired") +
        facet_wrap(as.formula(paste("~", trabajo_pp_corte_var_2)))
      
      print(plot)
      ggsave("www/indicador trabajo pp.png", width = 40, height = 25, units = "cm")
      
    } else if(input$indicador_trabajo_pp %in% lista_vunico & input$trabajo_pp_corte != "Departamento") {
      
      req(input$trabajo_pp_corte, input$indicador_trabajo_pp)
      
      dat_plot <- dat_trabajo_pp() %>%
        filter(corte == input$trabajo_pp_corte) %>%
        janitor::remove_empty("cols")
      
      if(input$trabajo_pp_corte == "Total"){
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor")) +
          geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
          geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_pp,
                                    "según",
                                    tolower(input$trabajo_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador trabajo pp.png", width = 40, height = 25, units = "cm")
        
        
      } else {
        
        trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor",
                                  fill = trabajo_pp_corte_var)) +
          geom_col(position = "dodge", alpha = .8, stroke = 1, color = "black") +
          geom_text(aes_string(group = trabajo_pp_corte_var, label = "Valor"),
                    position = position_dodge2(width = .9), vjust = -.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_pp,
                                    "según",
                                    tolower(input$trabajo_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador trabajo pp.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$indicador_trabajo_pp %in% lista_ind_2) {
      
      req(input$trabajo_pp_corte, input$trabajo_pp_corte_2,
          input$fecha_trabajo_pp, input$checkbox_trabajo_pp)
      
      trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
      trabajo_pp_corte_var_2 <- rlang::sym(to_varname(input$trabajo_pp_corte_2))
      
      dat_plot <- dat_trabajo_pp() %>%
        filter(ano >= input$fecha_trabajo_pp[1] &
                 ano <= input$fecha_trabajo_pp[2]) %>%
        filter(corte == input$trabajo_pp_corte) %>%
        filter(!!trabajo_pp_corte_var_2 %in% input$checkbox_trabajo_pp_2) %>%
        filter(!!trabajo_pp_corte_var %in% input$checkbox_trabajo_pp) %>% 
        filter(corte_2 == input$trabajo_pp_corte_2)
      
      if(input$trabajo_pp_corte_2 == "Total"){
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == "Total")
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = trabajo_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_pp,
                                    "según",
                                    tolower(input$trabajo_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
      } else if(input$trabajo_pp_corte_2 != "Total") {
        
        trabajo_pp_corte_var_2 <- rlang::sym(to_varname(input$trabajo_pp_corte_2))
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == input$trabajo_pp_corte_2)
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = trabajo_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          scale_x_continuous(breaks = int_breaks) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_pp,
                                    "según",
                                    tolower(input$trabajo_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          facet_wrap(as.formula(paste("~", trabajo_pp_corte_var_2)))
        
      }
      
      print(plot)
      ggsave("www/indicador trabajo pp.png", width = 40, height = 25, units = "cm")
      
      
      # Grafico simple normal
    } else if(input$trabajo_pp_corte == "Total") {
      
      req(input$indicador_trabajo_pp, input$fecha_trabajo_pp)
      
      dat_plot <- dat_trabajo_pp() %>%
        filter(ano >= input$fecha_trabajo_pp[1] &
                 ano <= input$fecha_trabajo_pp[2]) %>%
        filter(corte == "Total")
      
      if(input$indicador_trabajo_pp %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor, alpha = metodo)) +
          geom_line(size = 1, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          # scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "none") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_trabajo_pp),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8))
        
        print(plot)
        ggsave("www/indicador trabajo pp.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_trabajo_pp),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador trabajo pp.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$trabajo_pp_corte == "Departamento" &
              input$indicador_trabajo_pp %notin% lista_ind_2 ) {
      
      req(input$indicador_trabajo_pp, input$fecha_dpto_trabajo_pp)
      
      dat_plot <- dat_trabajo_pp() %>%
        filter(corte == "Departamento") %>%
        filter(ano == input$fecha_dpto_trabajo_pp) %>%
        select(departamento, Valor, fuente, cita)
      
      dep_j <- dep %>%
        left_join(dat_plot, by = c("nombre" = "departamento"))
      
      plot <-  ggplot(dep_j, aes(fill = Valor)) +
        geom_sf() +
        geom_sf_text(aes(label = Valor), colour = "black",
                     size = 4, fontface = "bold")+
        viridis::scale_fill_viridis(name = "", direction = -1)+
        labs(x = "",
             y = "",
        )+
        theme_bdd(base_size = 14) +
        theme(axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.grid.major = element_line(colour = "transparent"),
        ) +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_trabajo_pp,
                                  "en",
                                  input$fecha_dpto_trabajo_pp), w = 80),
             caption = wrapit(unique(dat_plot$cita), w = 80))
      
      print(plot)
      ggsave("www/indicador trabajo pp.png", width = 40, height = 25, units = "cm")
      
      
    } else if(input$trabajo_pp_corte != "Total") {
      
      req(input$trabajo_pp_corte, input$indicador_trabajo_pp,
          input$fecha_trabajo_pp, input$checkbox_trabajo_pp)
      
      dat_plot <- dat_trabajo_pp() %>%
        filter(ano >= input$fecha_trabajo_pp[1] &
                 ano <= input$fecha_trabajo_pp[2]) %>%
        filter(corte == input$trabajo_pp_corte) %>%
        janitor::remove_empty("cols")
      
      trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
      
      dat_plot <- filter(dat_plot,
                         !!trabajo_pp_corte_var %in% input$checkbox_trabajo_pp)
      
      if(input$indicador_trabajo_pp %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = trabajo_pp_corte_var, alpha = "metodo")) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_pp,
                                    "según",
                                    tolower(input$trabajo_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8)) +
          guides(alpha = "none")
        
        print(plot)
        ggsave("www/indicador trabajo pp.png", width = 40, height = 25, units = "cm")
        
        
      } else {
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = trabajo_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_pp,
                                    "según",
                                    tolower(input$trabajo_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador trabajo pp.png", width = 40, height = 25, units = "cm")
        
        
      }
      
      
    }
    
  })
  
  
  # * Descarga gráficos   =============================================
  
  output$baja_p_trabajo_pp <- downloadHandler(
    filename <- function() {
      paste("indicador trabajo pp", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/indicador trabajo pp.png", file)
    },
    contentType = "www/indicador trabajo pp"
  )
  
  
  # * Tablas   ========================================================
  
  # Data
  trabajo_pp_tab <- reactive({
    
    if(input$indicador_trabajo_pp %in%  lista_especial){
      
      trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
      trabajo_pp_corte_var_2 <- rlang::sym(to_varname(input$trabajo_pp_corte_2))
      
      dat_trabajo_pp() %>%
        filter(corte_2 == input$trabajo_pp_corte_2) %>% 
        select(Fecha, trabajo_pp_corte_var, trabajo_pp_corte_var_2, Valor) %>%
        arrange(desc(Fecha)) %>%
        pivot_wider(values_from = "Valor",
                    names_from = trabajo_pp_corte_var) 
      
    } else if(input$indicador_trabajo_pp %in% lista_vunico & input$trabajo_pp_corte == "Total"){
      
      req(input$trabajo_pp_corte, input$indicador_trabajo_pp)
      
      dat_trabajo_pp() %>%
        filter(corte == "Total") %>% 
        select(fecha_cat, Valor) %>%
        arrange(desc(fecha_cat)) %>%
        rename(Fecha = fecha_cat)
      
    } else if(input$indicador_trabajo_pp %in% lista_vunico & input$trabajo_pp_corte != "Total") {
      
      req(input$trabajo_pp_corte, input$indicador_trabajo_pp)
      
      trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
      
      dat_cut <- dat_trabajo_pp() %>%
        filter(corte == input$trabajo_pp_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        select(fecha_cat, trabajo_pp_corte_var, Valor) %>%
        arrange(desc(fecha_cat)) %>% 
        rename(Fecha = fecha_cat) %>%
        pivot_wider(values_from = "Valor",
                    names_from = trabajo_pp_corte_var)
      
      
    } else if(input$indicador_trabajo_pp %in% lista_ind_2) {
      
      req(input$trabajo_pp_corte, input$indicador_trabajo_pp, input$fecha_trabajo_pp)
      
      trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
      
      dat_cut <- dat_trabajo_pp() %>%
        filter(corte == input$trabajo_pp_corte) %>%
        filter(!!trabajo_pp_corte_var %in% input$checkbox_trabajo_pp)
      
      if(input$trabajo_pp_corte_2 == "Total"){
        
        dat_cut %>%
          filter(ano >= input$fecha_trabajo_pp[1] &
                   ano <= input$fecha_trabajo_pp[2]) %>%
          filter(corte_2 == "Total") %>% 
          select(Fecha, trabajo_pp_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = trabajo_pp_corte_var)
        
      } else if(input$trabajo_pp_corte_2 != "Total") {
        
        trabajo_pp_corte_var_2 <- rlang::sym(to_varname(input$trabajo_pp_corte_2))
        
        dat_cut %>%
          filter(ano >= input$fecha_trabajo_pp[1] &
                   ano <= input$fecha_trabajo_pp[2]) %>%
          filter(corte_2 == input$trabajo_pp_corte_2) %>% 
          select(Fecha, trabajo_pp_corte_var, trabajo_pp_corte_var_2, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = trabajo_pp_corte_var) 
        
      }
      
    } else if(input$trabajo_pp_corte == "Total") {
      
      req(input$trabajo_pp_corte, input$indicador_trabajo_pp, input$fecha_trabajo_pp)
      
      dat_trabajo_pp() %>%
        filter(corte == "Total") %>% 
        filter(ano >= input$fecha_trabajo_pp[1] &
                 ano <= input$fecha_trabajo_pp[2]) %>%
        select(Fecha, Valor) %>%
        arrange(desc(Fecha))
      
    } else if(input$trabajo_pp_corte != "Total") {
      
      req(input$trabajo_pp_corte, input$indicador_trabajo_pp, input$fecha_trabajo_pp)
      
      trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
      
      dat_cut <- dat_trabajo_pp() %>%
        filter(corte == input$trabajo_pp_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        filter(ano >= input$fecha_trabajo_pp[1] &
                 ano <= input$fecha_trabajo_pp[2]) %>% 
        select(Fecha, trabajo_pp_corte_var, Valor) %>%
        arrange(desc(Fecha)) %>% 
        pivot_wider(values_from = "Valor",
                    names_from = trabajo_pp_corte_var)
      
    }
  })
  
  # Metadata 
  trabajo_pp_meta <- reactive({
    
    dat_trabajo_pp() %>%
      select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, observaciones, actualizacion, cita) %>% 
      mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
      distinct() %>% 
      gather(key = " ", value = " ")
    
  })
  
  # Excel
  list_trabajo_pp <- reactive({
    list_trabajo_pp <- list("Data" = trabajo_pp_tab(),
                            "Metadata" = trabajo_pp_meta())
  })
  
  # Render
  output$table_trabajo_pp <- renderDT({
    
    DT::datatable(trabajo_pp_tab(),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    input$indicador_trabajo_pp,
                    style = "color:black; font-size:110%;")
    ) 
    
  })
  
  # * Descarga tablas   ================================================
  
  output$dwl_tab_trabajo_pp <- downloadHandler(
    
    filename = function() {
      paste("resultados-", input$indicador_trabajo_pp, ".xlsx", sep = "")
    },
    content = function(file) {
      
      openxlsx::write.xlsx(list_trabajo_pp(), file)
      
    }
  )
  
  
  
  ### 7.2. Trabajo Resultados ==============================================
  
  # * Data reactiva   =================================================    
  dat_trabajo_r <- reactive({
    
    req(input$indicador_trabajo_r)
    
    dat %>%
      filter(nomindicador == input$indicador_trabajo_r) 
    
  })
  
  output$selector_trabajo_r_corte_2 <- renderUI({
    
    if(input$indicador_trabajo_r %in% lista_ind_2){
      
      selectInput(
        inputId = "trabajo_r_corte_2",
        label = "Seleccione primer corte:",
        choices = dat_trabajo_r() %>% 
          select(corte_2) %>%
          arrange(corte_2) %>% 
          unique() %>% 
          pull(),
        selected = dat_trabajo_r() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte_2) %>% 
          pull()
      )
      
    } else {
      
      NULL
    }
    
  })
  
  output$chbox_trabajo_r_2 <- renderUI({
    
    if(input$indicador_trabajo_r %in% lista_ind_2 & input$indicador_trabajo_r %notin% lista_especial){
      
      # if(input$trabajo_r_corte %notin% c("Total", "Departamento") & input$indicador_trabajo_r %notin% lista_vunico) {
      
      trabajo_r_corte_var_2 <- rlang::sym(to_varname(input$trabajo_r_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_trabajo_r_2",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_trabajo_r() %>%
                           filter(corte_2 == input$trabajo_r_corte_2) %>% 
                           distinct(!!trabajo_r_corte_var_2) %>%
                           pull(),
                         selected = dat_trabajo_r() %>%
                           filter(corte_2 == input$trabajo_r_corte_2) %>% 
                           filter(jerarquia_cat_2 == "1") %>%
                           distinct(!!trabajo_r_corte_var_2) %>%
                           pull()
      )
      
      # } else {
      #   
      #   return(NULL)
      #   
      #   }
      
    } else {
      
      return(NULL)
      
    }      
  })
  
  
  output$selector_trabajo_r_corte <- renderUI({
    
    selectInput(
      inputId = "trabajo_r_corte",
      label = "Seleccione corte:",
      choices = dat_trabajo_r() %>% 
        select(corte) %>%
        arrange(corte) %>% 
        unique() %>% 
        pull(),
      selected = dat_trabajo_r() %>% 
        filter(jerarquia == "1") %>%  
        distinct(corte) %>% 
        pull()
    )
    
  })
  
  output$chbox_trabajo_r <- renderUI({
    
    if(input$trabajo_r_corte %in% lista_ind_2 & input$trabajo_r_corte %notin% c("Total", "Departamento") & input$indicador_trabajo_r %notin% lista_vunico) {
      
      trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
      trabajo_r_corte_var_2 <- rlang::sym(to_varname(input$trabajo_r_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_trabajo_r",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_trabajo_r() %>%
                           filter(!!trabajo_r_corte_var_2 %in% input$checkbox_trabajo_r_2) %>%
                           filter(corte == input$trabajo_r_corte) %>% 
                           distinct(!!trabajo_r_corte_var) %>%
                           pull(),
                         selected = dat_trabajo_r() %>%
                           filter(!!trabajo_r_corte_var_2 %in% input$checkbox_trabajo_r_2) %>%
                           filter(corte == input$trabajo_r_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!trabajo_r_corte_var) %>%
                           pull()
      )
      
    } else if(input$trabajo_r_corte %notin% lista_ind_2 & input$trabajo_r_corte %notin% c("Total", "Departamento") & input$indicador_trabajo_r %notin% lista_vunico) {
      
      trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
      
      checkboxGroupInput(inputId = "checkbox_trabajo_r",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_trabajo_r() %>%
                           filter(corte == input$trabajo_r_corte) %>% 
                           distinct(!!trabajo_r_corte_var) %>%
                           pull(),
                         selected = dat_trabajo_r() %>%
                           filter(corte == input$trabajo_r_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!trabajo_r_corte_var) %>%
                           pull()
      )
      
    } else {
      
      return(NULL)
    }
    
  })
  
  # Selector de fecha
  output$s_trabajo_r_fecha <- renderUI({
    
    if(input$trabajo_r_corte == "Departamento" & input$indicador_trabajo_r %notin% lista_ind_2) {
      
      req(input$trabajo_r_corte, input$indicador_trabajo_r)
      
      req(nrow(dat_trabajo_r()) > 0)
      
      selectInput(
        inputId = "fecha_dpto_trabajo_r",
        label = "Seleccione año:",
        choices = dat_trabajo_r() %>% 
          filter(nomindicador == input$indicador_trabajo_r) %>%
          drop_na(Valor) %>%
          select(ano) %>%
          arrange(desc(ano)) %>% 
          unique() %>% 
          pull(),
        selected = max(input$ano)
      )
      
    } else if (input$indicador_trabajo_r %in% lista_serie_cat){
      
      return(NULL)
      
      
    } else if (input$indicador_trabajo_r %in% lista_vunico){
      
      return(NULL)
      
    } else  {
      
      req(input$trabajo_r_corte, input$indicador_trabajo_r)
      req(nrow(dat_trabajo_r()) > 0)
      
      tagList(
        # tags$style(type = 'text/css', 
        #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
        # div(id = 'big_slider',
        
        sliderInput("fecha_trabajo_r", 
                    label = "Rango de tiempo", 
                    sep = "",
                    dragRange = T,
                    min = min(dat_trabajo_r()$ano), 
                    max = max(dat_trabajo_r()$ano), 
                    value = c(min(dat_trabajo_r()$ano), 
                              max(dat_trabajo_r()$ano))
        )
      )
      
    }
  })
  
  # # Selector de corte según categoría y data temporal
  # dat_trabajo_r <- reactive({
  #   
  #   req(input$trabajo_r_corte)
  #   
  #   if(input$indicador_trabajo_r %in% lista_ind_2){
  #     
  #     dat_salarios <- dat_trabajo_r() %>%
  #       filter(corte_2 == input$trabajo_r_corte_2) %>% 
  #       filter(corte == input$trabajo_r_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   } else {
  #     
  #     dat_trabajo_r() %>%
  #       filter(corte == input$trabajo_r_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   }
  # })
  
  # * Metadata   ======================================================
  
  # Title
  output$title_trabajo_r <- renderUI({ 
    helpText(HTML(unique(dat_trabajo_r()$nomindicador)))
  })
  
  # Subtitle
  output$subtitle_trabajo_r <- renderUI({ 
    helpText(HTML(unique(dat_trabajo_r()$definicion)))
  })
  
  # Nota
  output$nota_trabajo_r <- renderUI({ 
    
    if(input$indicador_trabajo_r %in% lista_nota){
      helpText(HTML(unique(dat_trabajo_r()$nota)))
      
    } else(NULL)
  })
  
  
  # Nombre conceptual
  output$conindicador_trabajo_r <- renderUI({ 
    helpText(HTML(paste("<b> Nombre conceptual:</b>", unique(dat_trabajo_r()$conindicador))))
  })
  
  # Calculo
  output$calculo_trabajo_r <- renderUI({ 
    helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_trabajo_r()$calculo))))
  })
  
  # Observaciones
  output$observacion_trabajo_r <- renderUI({ 
    helpText(HTML(paste("<b> Observaciones:</b>", unique(dat_trabajo_r()$observaciones))))
  })
  
  # Actualización
  output$actualizacion_trabajo_r <- renderUI({ 
    helpText(HTML(paste("<b> Última actualización:</b>", unique(dat_trabajo_r()$actualizacion))))
  })
  
  # * Gráficos   ======================================================
  
  output$plot_trabajo_r <- renderPlot({
    
    req(input$indicador_trabajo_r, input$trabajo_r_corte)
    
    if(input$indicador_trabajo_r %in% lista_serie_cat){
      
      req(input$indicador_trabajo_r)
      
      # Total
      if(input$trabajo_r_corte == "Total"){
        
        dat_plot <- dat_trabajo_r() %>%
          filter(corte == "Total")
        
        plot <- ggplot(dat_plot, aes(x = fecha_cat, y = Valor, group = 1)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_trabajo_r),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador trabajo r.png", width = 40, height = 25, units = "cm")
        
        # Según corte
      } else if(input$trabajo_r_corte != "Total") {
        
        trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
        
        dat_plot <- dat_trabajo_r() %>%
          filter(corte == input$trabajo_r_corte) %>%
          filter(!!trabajo_r_corte_var %in% input$checkbox_trabajo_r)
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            colour = trabajo_r_corte_var, group = trabajo_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_r,
                                    "según",
                                    tolower(input$trabajo_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador trabajo r.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_trabajo_r %in% lista_especial ){
      
      if (input$trabajo_r_corte_2 == "Total"){        
        
        req(input$trabajo_r_corte, input$indicador_trabajo_r)
        
        trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
        
        dat_plot <- dat_trabajo_r() %>%
          filter(ano >= input$fecha_trabajo_r[1] &
                   ano <= input$fecha_trabajo_r[2]) %>%
          filter(corte == input$trabajo_r_corte) %>%
          # filter(!!trabajo_r_corte_var %in% input$checkbox_trabajo_r) %>% 
          filter(corte_2 == input$trabajo_r_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = trabajo_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_r,
                                    "según",
                                    tolower(input$trabajo_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador trabajo r.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        req(input$trabajo_r_corte, input$indicador_trabajo_r)
        
        trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
        trabajo_r_corte_var_2 <- rlang::sym(to_varname(input$trabajo_r_corte_2))
        
        dat_plot <- dat_trabajo_r() %>%
          filter(ano >= input$fecha_trabajo_r[1] &
                   ano <= input$fecha_trabajo_r[2]) %>%
          filter(corte == input$trabajo_r_corte) %>%
          # filter(!!trabajo_r_corte_var %in% input$checkbox_trabajo_r) %>% 
          filter(corte_2 == input$trabajo_r_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = trabajo_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_r,
                                    "según",
                                    tolower(input$trabajo_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", trabajo_r_corte_var_2)))
        
        print(plot)
        ggsave("www/indicador trabajo r.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_trabajo_r %in% lista_vunico & input$indicador_trabajo_r %in% lista_ind_2){
      
      req(input$trabajo_r_corte, input$indicador_trabajo_r)
      
      trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
      trabajo_r_corte_var_2 <- rlang::sym(to_varname(input$trabajo_r_corte_2))
      
      dat_plot <- dat_trabajo_r() %>%
        filter(ano >= input$fecha_trabajo_r[1] &
                 ano <= input$fecha_trabajo_r[2]) %>%
        filter(corte == input$trabajo_r_corte) %>%
        filter(!!trabajo_r_corte_var %in% input$checkbox_trabajo_r) %>%
        filter(!!trabajo_r_corte_var_2 %in% input$checkbox_trabajo_r_2) %>%
        filter(corte_2 == input$trabajo_r_corte_2)
      
      plot <- ggplot(dat_plot,
                     aes_string(x = "fecha_cat", y = "Valor",
                                fill = trabajo_r_corte_var)) +
        geom_col(position = "dodge", width = .7, alpha = .8) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x=element_blank(),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_trabajo_r,
                                  "según",
                                  tolower(input$trabajo_r_corte),
                                  "en",
                                  unique(dat_plot$fecha_cat))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_fill_brewer(name = "", palette = "Paired") +
        facet_wrap(as.formula(paste("~", trabajo_r_corte_var_2)))
      
      print(plot)
      ggsave("www/indicador trabajo r.png", width = 40, height = 25, units = "cm")
      
    } else if(input$indicador_trabajo_r %in% lista_vunico & input$trabajo_r_corte != "Departamento") {
      
      req(input$trabajo_r_corte, input$indicador_trabajo_r)
      
      dat_plot <- dat_trabajo_r() %>%
        filter(corte == input$trabajo_r_corte) %>%
        janitor::remove_empty("cols")
      
      if(input$trabajo_r_corte == "Total"){
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor")) +
          geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
          geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_r,
                                    "según",
                                    tolower(input$trabajo_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador trabajo r.png", width = 40, height = 25, units = "cm")
        
        
      } else {
        
        trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor",
                                  fill = trabajo_r_corte_var)) +
          geom_col(position = "dodge", alpha = .8, stroke = 1, color = "black") +
          geom_text(aes_string(group = trabajo_r_corte_var, label = "Valor"),
                    position = position_dodge2(width = .9), vjust = -.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_r,
                                    "según",
                                    tolower(input$trabajo_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador trabajo r.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$indicador_trabajo_r %in% lista_ind_2) {
      
      req(input$trabajo_r_corte, input$trabajo_r_corte_2,
          input$fecha_trabajo_r, input$checkbox_trabajo_r)
      
      trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
      trabajo_r_corte_var_2 <- rlang::sym(to_varname(input$trabajo_r_corte_2))
      
      dat_plot <- dat_trabajo_r() %>%
        filter(ano >= input$fecha_trabajo_r[1] &
                 ano <= input$fecha_trabajo_r[2]) %>%
        filter(corte == input$trabajo_r_corte) %>%
        filter(!!trabajo_r_corte_var_2 %in% input$checkbox_trabajo_r_2) %>%
        filter(!!trabajo_r_corte_var %in% input$checkbox_trabajo_r) %>% 
        filter(corte_2 == input$trabajo_r_corte_2)
      
      if(input$trabajo_r_corte_2 == "Total"){
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == "Total")
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = trabajo_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_r,
                                    "según",
                                    tolower(input$trabajo_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
      } else if(input$trabajo_r_corte_2 != "Total") {
        
        trabajo_r_corte_var_2 <- rlang::sym(to_varname(input$trabajo_r_corte_2))
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == input$trabajo_r_corte_2)
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = trabajo_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          scale_x_continuous(breaks = int_breaks) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_r,
                                    "según",
                                    tolower(input$trabajo_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          facet_wrap(as.formula(paste("~", trabajo_r_corte_var_2)))
        
      }
      
      print(plot)
      ggsave("www/indicador trabajo r.png", width = 40, height = 25, units = "cm")
      
      
      # Grafico simple normal
    } else if(input$trabajo_r_corte == "Total") {
      
      req(input$indicador_trabajo_r, input$fecha_trabajo_r)
      
      dat_plot <- dat_trabajo_r() %>%
        filter(ano >= input$fecha_trabajo_r[1] &
                 ano <= input$fecha_trabajo_r[2]) %>%
        filter(corte == "Total")
      
      if(input$indicador_trabajo_r %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor, alpha = metodo)) +
          geom_line(size = 1, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          # scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "none") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_trabajo_r),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8))
        
        print(plot)
        ggsave("www/indicador trabajo r.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_trabajo_r),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador trabajo r.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$trabajo_r_corte == "Departamento" &
              input$indicador_trabajo_r %notin% lista_ind_2 ) {
      
      req(input$indicador_trabajo_r, input$fecha_dpto_trabajo_r)
      
      dat_plot <- dat_trabajo_r() %>%
        filter(corte == "Departamento") %>%
        filter(ano == input$fecha_dpto_trabajo_r) %>%
        select(departamento, Valor, fuente, cita)
      
      dep_j <- dep %>%
        left_join(dat_plot, by = c("nombre" = "departamento"))
      
      plot <-  ggplot(dep_j, aes(fill = Valor)) +
        geom_sf() +
        geom_sf_text(aes(label = Valor), colour = "black",
                     size = 4, fontface = "bold")+
        viridis::scale_fill_viridis(name = "", direction = -1)+
        labs(x = "",
             y = "",
        )+
        theme_bdd(base_size = 14) +
        theme(axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.grid.major = element_line(colour = "transparent"),
        ) +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_trabajo_r,
                                  "en",
                                  input$fecha_dpto_trabajo_r), w = 80),
             caption = wrapit(unique(dat_plot$cita), w = 80))
      
      print(plot)
      ggsave("www/indicador trabajo r.png", width = 40, height = 25, units = "cm")
      
      
    } else if(input$trabajo_r_corte != "Total") {
      
      req(input$trabajo_r_corte, input$indicador_trabajo_r,
          input$fecha_trabajo_r, input$checkbox_trabajo_r)
      
      dat_plot <- dat_trabajo_r() %>%
        filter(ano >= input$fecha_trabajo_r[1] &
                 ano <= input$fecha_trabajo_r[2]) %>%
        filter(corte == input$trabajo_r_corte) %>%
        janitor::remove_empty("cols")
      
      trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
      
      dat_plot <- filter(dat_plot,
                         !!trabajo_r_corte_var %in% input$checkbox_trabajo_r)
      
      if(input$indicador_trabajo_r %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = trabajo_r_corte_var, alpha = "metodo")) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_r,
                                    "según",
                                    tolower(input$trabajo_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8)) +
          guides(alpha = "none")
        
        print(plot)
        ggsave("www/indicador trabajo r.png", width = 40, height = 25, units = "cm")      
        

    } else {
      
      plot <- ggplot(dat_plot,
                     aes_string(x = "fecha", y = "Valor", colour = trabajo_r_corte_var)) +
        geom_line(size = 1, alpha = 0.5) +
        geom_point(size = 3) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        scale_x_continuous(breaks = int_breaks) +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_trabajo_r,
                                  "según",
                                  tolower(input$trabajo_r_corte))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_colour_manual(name = "", values = paleta_expandida)
      
      print(plot)
      ggsave("www/indicador trabajo r.png", width = 40, height = 25, units = "cm")
      
      }
    }
  })
  
  
  # * Descarga gráficos   =============================================
  
  output$baja_p_trabajo_r <- downloadHandler(
    filename <- function() {
      paste("indicador trabajo r", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/indicador trabajo r.png", file)
    },
    contentType = "www/indicador trabajo r"
  )
  
  
  # * Tablas   ========================================================
  
  # Data
  trabajo_r_tab <- reactive({
    
    if(input$indicador_trabajo_r %in%  lista_especial){
      
      trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
      trabajo_r_corte_var_2 <- rlang::sym(to_varname(input$trabajo_r_corte_2))
      
      dat_trabajo_r() %>%
        filter(corte_2 == input$trabajo_r_corte_2) %>% 
        select(Fecha, trabajo_r_corte_var, trabajo_r_corte_var_2, Valor) %>%
        arrange(desc(Fecha)) %>%
        pivot_wider(values_from = "Valor",
                    names_from = trabajo_r_corte_var) 
      
    } else if(input$indicador_trabajo_r %in% lista_vunico & input$trabajo_r_corte == "Total"){
      
      req(input$trabajo_r_corte, input$indicador_trabajo_r)
      
      dat_trabajo_r() %>%
        filter(corte == "Total") %>% 
        select(fecha_cat, Valor) %>%
        arrange(desc(fecha_cat)) %>%
        rename(Fecha = fecha_cat)
      
    } else if(input$indicador_trabajo_r %in% lista_vunico & input$trabajo_r_corte != "Total") {
      
      req(input$trabajo_r_corte, input$indicador_trabajo_r)
      
      trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
      
      dat_cut <- dat_trabajo_r() %>%
        filter(corte == input$trabajo_r_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        select(fecha_cat, trabajo_r_corte_var, Valor) %>%
        arrange(desc(fecha_cat)) %>% 
        rename(Fecha = fecha_cat) %>%
        pivot_wider(values_from = "Valor",
                    names_from = trabajo_r_corte_var)
      
      
    } else if(input$indicador_trabajo_r %in% lista_ind_2) {
      
      req(input$trabajo_r_corte, input$indicador_trabajo_r, input$fecha_trabajo_r)
      
      trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
      
      dat_cut <- dat_trabajo_r() %>%
        filter(corte == input$trabajo_r_corte) %>%
        filter(!!trabajo_r_corte_var %in% input$checkbox_trabajo_r)
      
      if(input$trabajo_r_corte_2 == "Total"){
        
        dat_cut %>%
          filter(ano >= input$fecha_trabajo_r[1] &
                   ano <= input$fecha_trabajo_r[2]) %>%
          filter(corte_2 == "Total") %>% 
          select(Fecha, trabajo_r_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = trabajo_r_corte_var)
        
      } else if(input$trabajo_r_corte_2 != "Total") {
        
        trabajo_r_corte_var_2 <- rlang::sym(to_varname(input$trabajo_r_corte_2))
        
        dat_cut %>%
          filter(ano >= input$fecha_trabajo_r[1] &
                   ano <= input$fecha_trabajo_r[2]) %>%
          filter(corte_2 == input$trabajo_r_corte_2) %>% 
          select(Fecha, trabajo_r_corte_var, trabajo_r_corte_var_2, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = trabajo_r_corte_var) 
        
      }
      
    } else if(input$trabajo_r_corte == "Total") {
      
      req(input$trabajo_r_corte, input$indicador_trabajo_r, input$fecha_trabajo_r)
      
      dat_trabajo_r() %>%
        filter(corte == "Total") %>% 
        filter(ano >= input$fecha_trabajo_r[1] &
                 ano <= input$fecha_trabajo_r[2]) %>%
        select(Fecha, Valor) %>%
        arrange(desc(Fecha))
      
    } else if(input$trabajo_r_corte != "Total") {
      
      req(input$trabajo_r_corte, input$indicador_trabajo_r, input$fecha_trabajo_r)
      
      trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
      
      dat_cut <- dat_trabajo_r() %>%
        filter(corte == input$trabajo_r_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        filter(ano >= input$fecha_trabajo_r[1] &
                 ano <= input$fecha_trabajo_r[2]) %>% 
        select(Fecha, trabajo_r_corte_var, Valor) %>%
        arrange(desc(Fecha)) %>% 
        pivot_wider(values_from = "Valor",
                    names_from = trabajo_r_corte_var)
      
    }
  })
  
  # Metadata 
  trabajo_r_meta <- reactive({
    
    dat_trabajo_r() %>%
      select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, observaciones, actualizacion, cita) %>% 
      mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
      distinct() %>% 
      gather(key = " ", value = " ")
    
  })
  
  # Excel
  list_trabajo_r <- reactive({
    list_trabajo_r <- list("Data" = trabajo_r_tab(),
                           "Metadata" = trabajo_r_meta())
  })
  
  # Render
  output$table_trabajo_r <- renderDT({
    
    DT::datatable(trabajo_r_tab(),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    input$indicador_trabajo_r,
                    style = "color:black; font-size:110%;")
    ) 
    
  })
  
  # * Descarga tablas   ================================================
  
  output$dwl_tab_trabajo_r <- downloadHandler(
    
    filename = function() {
      paste("resultados-", input$indicador_trabajo_r, ".xlsx", sep = "")
    },
    content = function(file) {
      
      openxlsx::write.xlsx(list_trabajo_r(), file)
      
    }
  )
  
  
  
  ### 8.1. Ambiente Politicas==============================================
  
  # * Data reactiva   =================================================    
  dat_ambiente_pp <- reactive({
    
    req(input$indicador_ambiente_pp)
    
    dat %>%
      filter(nomindicador == input$indicador_ambiente_pp) 
    
  })
  
  output$selector_ambiente_pp_corte_2 <- renderUI({
    
    if(input$indicador_ambiente_pp %in% lista_ind_2){
      
      selectInput(
        inputId = "ambiente_pp_corte_2",
        label = "Seleccione primer corte:",
        choices = dat_ambiente_pp() %>% 
          select(corte_2) %>%
          arrange(corte_2) %>% 
          unique() %>% 
          pull(),
        selected = dat_ambiente_pp() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte_2) %>% 
          pull()
      )
      
    } else {
      
      NULL
    }
    
  })
  
  output$chbox_ambiente_pp_2 <- renderUI({
    
    if(input$indicador_ambiente_pp %in% lista_ind_2 & input$indicador_ambiente_pp %notin% lista_especial){
      
      # if(input$ambiente_pp_corte %notin% c("Total", "Departamento") & input$indicador_ambiente_pp %notin% lista_vunico) {
      
      ambiente_pp_corte_var_2 <- rlang::sym(to_varname(input$ambiente_pp_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_ambiente_pp_2",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_ambiente_pp() %>%
                           filter(corte_2 == input$ambiente_pp_corte_2) %>% 
                           distinct(!!ambiente_pp_corte_var_2) %>%
                           pull(),
                         selected = dat_ambiente_pp() %>%
                           filter(corte_2 == input$ambiente_pp_corte_2) %>% 
                           filter(jerarquia_cat_2 == "1") %>%
                           distinct(!!ambiente_pp_corte_var_2) %>%
                           pull()
      )
      
      # } else {
      #   
      #   return(NULL)
      #   
      #   }
      
    } else {
      
      return(NULL)
      
    }      
  })
  
  
  output$selector_ambiente_pp_corte <- renderUI({
    
    selectInput(
      inputId = "ambiente_pp_corte",
      label = "Seleccione corte:",
      choices = dat_ambiente_pp() %>% 
        select(corte) %>%
        arrange(corte) %>% 
        unique() %>% 
        pull(),
      selected = dat_ambiente_pp() %>% 
        filter(jerarquia == "1") %>%  
        distinct(corte) %>% 
        pull()
    )
    
  })
  
  output$chbox_ambiente_pp <- renderUI({
    
    if(input$ambiente_pp_corte %in% lista_ind_2 & input$ambiente_pp_corte %notin% c("Total", "Departamento") & input$indicador_ambiente_pp %notin% lista_vunico) {
      
      ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
      ambiente_pp_corte_var_2 <- rlang::sym(to_varname(input$ambiente_pp_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_ambiente_pp",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_ambiente_pp() %>%
                           filter(!!ambiente_pp_corte_var_2 %in% input$checkbox_ambiente_pp_2) %>%
                           filter(corte == input$ambiente_pp_corte) %>% 
                           distinct(!!ambiente_pp_corte_var) %>%
                           pull(),
                         selected = dat_ambiente_pp() %>%
                           filter(!!ambiente_pp_corte_var_2 %in% input$checkbox_ambiente_pp_2) %>%
                           filter(corte == input$ambiente_pp_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!ambiente_pp_corte_var) %>%
                           pull()
      )
      
    } else if(input$ambiente_pp_corte %notin% lista_ind_2 & input$ambiente_pp_corte %notin% c("Total", "Departamento") & input$indicador_ambiente_pp %notin% lista_vunico) {
      
      ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
      
      checkboxGroupInput(inputId = "checkbox_ambiente_pp",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_ambiente_pp() %>%
                           filter(corte == input$ambiente_pp_corte) %>% 
                           distinct(!!ambiente_pp_corte_var) %>%
                           pull(),
                         selected = dat_ambiente_pp() %>%
                           filter(corte == input$ambiente_pp_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!ambiente_pp_corte_var) %>%
                           pull()
      )
      
    } else {
      
      return(NULL)
    }
    
  })
  
  # Selector de fecha
  output$s_ambiente_pp_fecha <- renderUI({
    
    if(input$ambiente_pp_corte == "Departamento" & input$indicador_ambiente_pp %notin% lista_ind_2) {
      
      req(input$ambiente_pp_corte, input$indicador_ambiente_pp)
      
      req(nrow(dat_ambiente_pp()) > 0)
      
      selectInput(
        inputId = "fecha_dpto_ambiente_pp",
        label = "Seleccione año:",
        choices = dat_ambiente_pp() %>% 
          filter(nomindicador == input$indicador_ambiente_pp) %>%
          drop_na(Valor) %>%
          select(ano) %>%
          arrange(desc(ano)) %>% 
          unique() %>% 
          pull(),
        selected = max(input$ano)
      )
      
    } else if (input$indicador_ambiente_pp %in% lista_serie_cat){
      
      return(NULL)
      
      
    } else if (input$indicador_ambiente_pp %in% lista_vunico){
      
      return(NULL)
      
    } else  {
      
      req(input$ambiente_pp_corte, input$indicador_ambiente_pp)
      req(nrow(dat_ambiente_pp()) > 0)
      
      tagList(
        # tags$style(type = 'text/css', 
        #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
        # div(id = 'big_slider',
        
        sliderInput("fecha_ambiente_pp", 
                    label = "Rango de tiempo", 
                    sep = "",
                    dragRange = T,
                    min = min(dat_ambiente_pp()$ano), 
                    max = max(dat_ambiente_pp()$ano), 
                    value = c(min(dat_ambiente_pp()$ano), 
                              max(dat_ambiente_pp()$ano))
        )
      )
      
    }
  })
  
  # # Selector de corte según categoría y data temporal
  # dat_ambiente_pp <- reactive({
  #   
  #   req(input$ambiente_pp_corte)
  #   
  #   if(input$indicador_ambiente_pp %in% lista_ind_2){
  #     
  #     dat_salarios <- dat_ambiente_pp() %>%
  #       filter(corte_2 == input$ambiente_pp_corte_2) %>% 
  #       filter(corte == input$ambiente_pp_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   } else {
  #     
  #     dat_ambiente_pp() %>%
  #       filter(corte == input$ambiente_pp_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   }
  # })
  
  # * Metadata   ======================================================
  
  # Title
  output$title_ambiente_pp <- renderUI({ 
    helpText(HTML(unique(dat_ambiente_pp()$nomindicador)))
  })
  
  # Subtitle
  output$subtitle_ambiente_pp <- renderUI({ 
    helpText(HTML(unique(dat_ambiente_pp()$definicion)))
  })
  
  # Nombre conceptual
  output$conindicador_ambiente_pp <- renderUI({ 
    helpText(HTML(paste("<b> Nombre conceptual:</b>", unique(dat_ambiente_pp()$conindicador))))
  })
  
  # Calculo
  output$calculo_ambiente_pp <- renderUI({ 
    helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_ambiente_pp()$calculo))))
  })
  
  # Observaciones
  output$observacion_ambiente_pp <- renderUI({ 
    helpText(HTML(paste("<b> Observaciones:</b>", unique(dat_ambiente_pp()$observaciones))))
  })
  
  # Actualización
  output$actualizacion_ambiente_pp <- renderUI({ 
    helpText(HTML(paste("<b> Última actualización:</b>", unique(dat_ambiente_pp()$actualizacion))))
  })
  
  # * Gráficos   ======================================================
  
  output$plot_ambiente_pp <- renderPlot({
    
    req(input$indicador_ambiente_pp, input$ambiente_pp_corte)
    
    if(input$indicador_ambiente_pp %in% lista_serie_cat){
      
      req(input$indicador_ambiente_pp)
      
      # Total
      if(input$ambiente_pp_corte == "Total"){
        
        dat_plot <- dat_ambiente_pp() %>%
          filter(corte == "Total")
        
        plot <- ggplot(dat_plot, aes(x = fecha_cat, y = Valor, group = 1)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_ambiente_pp),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador ambiente pp.png", width = 40, height = 25, units = "cm")
        
        # Según corte
      } else if(input$ambiente_pp_corte != "Total") {
        
        ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
        
        dat_plot <- dat_ambiente_pp() %>%
          filter(corte == input$ambiente_pp_corte) %>%
          filter(!!ambiente_pp_corte_var %in% input$checkbox_ambiente_pp)
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            colour = ambiente_pp_corte_var, group = ambiente_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_pp,
                                    "según",
                                    tolower(input$ambiente_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador ambiente pp.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_ambiente_pp %in% lista_especial ){
      
      if (input$ambiente_pp_corte_2 == "Total"){        
        
        req(input$ambiente_pp_corte, input$indicador_ambiente_pp)
        
        ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
        
        dat_plot <- dat_ambiente_pp() %>%
          filter(ano >= input$fecha_ambiente_pp[1] &
                   ano <= input$fecha_ambiente_pp[2]) %>%
          filter(corte == input$ambiente_pp_corte) %>%
          # filter(!!ambiente_pp_corte_var %in% input$checkbox_ambiente_pp) %>% 
          filter(corte_2 == input$ambiente_pp_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = ambiente_pp_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_pp,
                                    "según",
                                    tolower(input$ambiente_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador ambiente pp.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        req(input$ambiente_pp_corte, input$indicador_ambiente_pp)
        
        ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
        ambiente_pp_corte_var_2 <- rlang::sym(to_varname(input$ambiente_pp_corte_2))
        
        dat_plot <- dat_ambiente_pp() %>%
          filter(ano >= input$fecha_ambiente_pp[1] &
                   ano <= input$fecha_ambiente_pp[2]) %>%
          filter(corte == input$ambiente_pp_corte) %>%
          # filter(!!ambiente_pp_corte_var %in% input$checkbox_ambiente_pp) %>% 
          filter(corte_2 == input$ambiente_pp_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = ambiente_pp_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_pp,
                                    "según",
                                    tolower(input$ambiente_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", ambiente_pp_corte_var_2)))
        
        print(plot)
        ggsave("www/indicador ambiente pp.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_ambiente_pp %in% lista_vunico & input$indicador_ambiente_pp %in% lista_ind_2){
      
      req(input$ambiente_pp_corte, input$indicador_ambiente_pp)
      
      ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
      ambiente_pp_corte_var_2 <- rlang::sym(to_varname(input$ambiente_pp_corte_2))
      
      dat_plot <- dat_ambiente_pp() %>%
        filter(ano >= input$fecha_ambiente_pp[1] &
                 ano <= input$fecha_ambiente_pp[2]) %>%
        filter(corte == input$ambiente_pp_corte) %>%
        filter(!!ambiente_pp_corte_var %in% input$checkbox_ambiente_pp) %>%
        filter(!!ambiente_pp_corte_var_2 %in% input$checkbox_ambiente_pp_2) %>%
        filter(corte_2 == input$ambiente_pp_corte_2)
      
      plot <- ggplot(dat_plot,
                     aes_string(x = "fecha_cat", y = "Valor",
                                fill = ambiente_pp_corte_var)) +
        geom_col(position = "dodge", width = .7, alpha = .8) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x=element_blank(),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_ambiente_pp,
                                  "según",
                                  tolower(input$ambiente_pp_corte),
                                  "en",
                                  unique(dat_plot$fecha_cat))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_fill_brewer(name = "", palette = "Paired") +
        facet_wrap(as.formula(paste("~", ambiente_pp_corte_var_2)))
      
      print(plot)
      ggsave("www/indicador ambiente pp.png", width = 40, height = 25, units = "cm")
      
    } else if(input$indicador_ambiente_pp %in% lista_vunico & input$ambiente_pp_corte != "Departamento") {
      
      req(input$ambiente_pp_corte, input$indicador_ambiente_pp)
      
      dat_plot <- dat_ambiente_pp() %>%
        filter(corte == input$ambiente_pp_corte) %>%
        janitor::remove_empty("cols")
      
      if(input$ambiente_pp_corte == "Total"){
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor")) +
          geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
          geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_pp,
                                    "según",
                                    tolower(input$ambiente_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador ambiente pp.png", width = 40, height = 25, units = "cm")
        
        
      } else {
        
        ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor",
                                  fill = ambiente_pp_corte_var)) +
          geom_col(position = "dodge", alpha = .8, stroke = 1, color = "black") +
          geom_text(aes_string(group = ambiente_pp_corte_var, label = "Valor"),
                    position = position_dodge2(width = .9), vjust = -.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_pp,
                                    "según",
                                    tolower(input$ambiente_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador ambiente pp.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$indicador_ambiente_pp %in% lista_ind_2) {
      
      req(input$ambiente_pp_corte, input$ambiente_pp_corte_2,
          input$fecha_ambiente_pp, input$checkbox_ambiente_pp)
      
      ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
      ambiente_pp_corte_var_2 <- rlang::sym(to_varname(input$ambiente_pp_corte_2))
      
      dat_plot <- dat_ambiente_pp() %>%
        filter(ano >= input$fecha_ambiente_pp[1] &
                 ano <= input$fecha_ambiente_pp[2]) %>%
        filter(corte == input$ambiente_pp_corte) %>%
        filter(!!ambiente_pp_corte_var_2 %in% input$checkbox_ambiente_pp_2) %>%
        filter(!!ambiente_pp_corte_var %in% input$checkbox_ambiente_pp) %>% 
        filter(corte_2 == input$ambiente_pp_corte_2)
      
      if(input$ambiente_pp_corte_2 == "Total"){
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == "Total")
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = ambiente_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_pp,
                                    "según",
                                    tolower(input$ambiente_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
      } else if(input$ambiente_pp_corte_2 != "Total") {
        
        ambiente_pp_corte_var_2 <- rlang::sym(to_varname(input$ambiente_pp_corte_2))
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == input$ambiente_pp_corte_2)
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = ambiente_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          scale_x_continuous(breaks = int_breaks) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_pp,
                                    "según",
                                    tolower(input$ambiente_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          facet_wrap(as.formula(paste("~", ambiente_pp_corte_var_2)))
        
      }
      
      print(plot)
      ggsave("www/indicador ambiente pp.png", width = 40, height = 25, units = "cm")
      
      
      # Grafico simple normal
    } else if(input$ambiente_pp_corte == "Total") {
      
      req(input$indicador_ambiente_pp, input$fecha_ambiente_pp)
      
      dat_plot <- dat_ambiente_pp() %>%
        filter(ano >= input$fecha_ambiente_pp[1] &
                 ano <= input$fecha_ambiente_pp[2]) %>%
        filter(corte == "Total")
      
      if(input$indicador_ambiente_pp %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor, alpha = metodo)) +
          geom_line(size = 1, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          # scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "none") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_ambiente_pp),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8))
        
        print(plot)
        ggsave("www/indicador ambiente pp.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_ambiente_pp),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador ambiente pp.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$ambiente_pp_corte == "Departamento" &
              input$indicador_ambiente_pp %notin% lista_ind_2 ) {
      
      req(input$indicador_ambiente_pp, input$fecha_dpto_ambiente_pp)
      
      dat_plot <- dat_ambiente_pp() %>%
        filter(corte == "Departamento") %>%
        filter(ano == input$fecha_dpto_ambiente_pp) %>%
        select(departamento, Valor, fuente, cita)
      
      dep_j <- dep %>%
        left_join(dat_plot, by = c("nombre" = "departamento"))
      
      plot <-  ggplot(dep_j, aes(fill = Valor)) +
        geom_sf() +
        geom_sf_text(aes(label = Valor), colour = "black",
                     size = 4, fontface = "bold")+
        viridis::scale_fill_viridis(name = "", direction = -1)+
        labs(x = "",
             y = "",
        )+
        theme_bdd(base_size = 14) +
        theme(axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.grid.major = element_line(colour = "transparent"),
        ) +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_ambiente_pp,
                                  "en",
                                  input$fecha_dpto_ambiente_pp), w = 80),
             caption = wrapit(unique(dat_plot$cita), w = 80))
      
      print(plot)
      ggsave("www/indicador ambiente pp.png", width = 40, height = 25, units = "cm")
      
      
    } else if(input$ambiente_pp_corte != "Total") {
      
      req(input$ambiente_pp_corte, input$indicador_ambiente_pp,
          input$fecha_ambiente_pp, input$checkbox_ambiente_pp)
      
      dat_plot <- dat_ambiente_pp() %>%
        filter(ano >= input$fecha_ambiente_pp[1] &
                 ano <= input$fecha_ambiente_pp[2]) %>%
        filter(corte == input$ambiente_pp_corte) %>%
        janitor::remove_empty("cols")
      
      ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
      
      dat_plot <- filter(dat_plot,
                         !!ambiente_pp_corte_var %in% input$checkbox_ambiente_pp)
      
      if(input$indicador_ambiente_pp %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = ambiente_pp_corte_var, alpha = "metodo")) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_pp,
                                    "según",
                                    tolower(input$ambiente_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8)) +
          guides(alpha = "none")
        
        print(plot)
        ggsave("www/indicador ambiente pp.png", width = 40, height = 25, units = "cm")
        
        
      } else {
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = ambiente_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_pp,
                                    "según",
                                    tolower(input$ambiente_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador ambiente pp.png", width = 40, height = 25, units = "cm")
        
        
      }
      
      
    }
    
  })
  
  
  # * Descarga gráficos   =============================================
  
  output$baja_p_ambiente_pp <- downloadHandler(
    filename <- function() {
      paste("indicador ambiente pp", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/indicador ambiente pp.png", file)
    },
    contentType = "www/indicador ambiente pp"
  )
  
  
  # * Tablas   ========================================================
  
  # Data
  ambiente_pp_tab <- reactive({
    
    if(input$indicador_ambiente_pp %in%  lista_especial){
      
      ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
      ambiente_pp_corte_var_2 <- rlang::sym(to_varname(input$ambiente_pp_corte_2))
      
      dat_ambiente_pp() %>%
        filter(corte_2 == input$ambiente_pp_corte_2) %>% 
        select(Fecha, ambiente_pp_corte_var, ambiente_pp_corte_var_2, Valor) %>%
        arrange(desc(Fecha)) %>%
        pivot_wider(values_from = "Valor",
                    names_from = ambiente_pp_corte_var) 
      
    } else if(input$indicador_ambiente_pp %in% lista_vunico & input$ambiente_pp_corte == "Total"){
      
      req(input$ambiente_pp_corte, input$indicador_ambiente_pp)
      
      dat_ambiente_pp() %>%
        filter(corte == "Total") %>% 
        select(fecha_cat, Valor) %>%
        arrange(desc(fecha_cat)) %>%
        rename(Fecha = fecha_cat)
      
    } else if(input$indicador_ambiente_pp %in% lista_vunico & input$ambiente_pp_corte != "Total") {
      
      req(input$ambiente_pp_corte, input$indicador_ambiente_pp)
      
      ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
      
      dat_cut <- dat_ambiente_pp() %>%
        filter(corte == input$ambiente_pp_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        select(fecha_cat, ambiente_pp_corte_var, Valor) %>%
        arrange(desc(fecha_cat)) %>% 
        rename(Fecha = fecha_cat) %>%
        pivot_wider(values_from = "Valor",
                    names_from = ambiente_pp_corte_var)
      
      
    } else if(input$indicador_ambiente_pp %in% lista_ind_2) {
      
      req(input$ambiente_pp_corte, input$indicador_ambiente_pp, input$fecha_ambiente_pp)
      
      ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
      
      dat_cut <- dat_ambiente_pp() %>%
        filter(corte == input$ambiente_pp_corte) %>%
        filter(!!ambiente_pp_corte_var %in% input$checkbox_ambiente_pp)
      
      if(input$ambiente_pp_corte_2 == "Total"){
        
        dat_cut %>%
          filter(ano >= input$fecha_ambiente_pp[1] &
                   ano <= input$fecha_ambiente_pp[2]) %>%
          filter(corte_2 == "Total") %>% 
          select(Fecha, ambiente_pp_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = ambiente_pp_corte_var)
        
      } else if(input$ambiente_pp_corte_2 != "Total") {
        
        ambiente_pp_corte_var_2 <- rlang::sym(to_varname(input$ambiente_pp_corte_2))
        
        dat_cut %>%
          filter(ano >= input$fecha_ambiente_pp[1] &
                   ano <= input$fecha_ambiente_pp[2]) %>%
          filter(corte_2 == input$ambiente_pp_corte_2) %>% 
          select(Fecha, ambiente_pp_corte_var, ambiente_pp_corte_var_2, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = ambiente_pp_corte_var) 
        
      }
      
    } else if(input$ambiente_pp_corte == "Total") {
      
      req(input$ambiente_pp_corte, input$indicador_ambiente_pp, input$fecha_ambiente_pp)
      
      dat_ambiente_pp() %>%
        filter(corte == "Total") %>% 
        filter(ano >= input$fecha_ambiente_pp[1] &
                 ano <= input$fecha_ambiente_pp[2]) %>%
        select(Fecha, Valor) %>%
        arrange(desc(Fecha))
      
    } else if(input$ambiente_pp_corte != "Total") {
      
      req(input$ambiente_pp_corte, input$indicador_ambiente_pp, input$fecha_ambiente_pp)
      
      ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
      
      dat_cut <- dat_ambiente_pp() %>%
        filter(corte == input$ambiente_pp_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        filter(ano >= input$fecha_ambiente_pp[1] &
                 ano <= input$fecha_ambiente_pp[2]) %>% 
        select(Fecha, ambiente_pp_corte_var, Valor) %>%
        arrange(desc(Fecha)) %>% 
        pivot_wider(values_from = "Valor",
                    names_from = ambiente_pp_corte_var)
      
    }
  })
  
  # Metadata 
  ambiente_pp_meta <- reactive({
    
    dat_ambiente_pp() %>%
      select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, observaciones, actualizacion, cita) %>% 
      mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
      distinct() %>% 
      gather(key = " ", value = " ")
    
  })
  
  # Excel
  list_ambiente_pp <- reactive({
    list_ambiente_pp <- list("Data" = ambiente_pp_tab(),
                             "Metadata" = ambiente_pp_meta())
  })
  
  # Render
  output$table_ambiente_pp <- renderDT({
    
    DT::datatable(ambiente_pp_tab(),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    input$indicador_ambiente_pp,
                    style = "color:black; font-size:110%;")
    ) 
    
  })
  
  # * Descarga tablas   ================================================
  
  output$dwl_tab_ambiente_pp <- downloadHandler(
    
    filename = function() {
      paste("resultados-", input$indicador_ambiente_pp, ".xlsx", sep = "")
    },
    content = function(file) {
      
      openxlsx::write.xlsx(list_ambiente_pp(), file)
      
    }
  )
  
  
  

  
  ### 8.2. Ambiente Resultados   =============================================
  
  # * Data reactiva   =================================================
  
  dat_ambiente_r <- reactive({
    
    req(input$indicador_ambiente_r)
    
    dat %>%
      filter(nomindicador == input$indicador_ambiente_r) 
    
  })
  
  output$selector_ambiente_r_corte_2 <- renderUI({
    
    if(input$indicador_ambiente_r %in% lista_ind_2){
      
      selectInput(
        inputId = "ambiente_r_corte_2",
        label = "Seleccione primer corte:",
        choices = dat_ambiente_r() %>% 
          select(corte_2) %>%
          arrange(corte_2) %>% 
          unique() %>% 
          pull(),
        selected = dat_ambiente_r() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte_2) %>% 
          pull()
      )
      
    } else {
      
      NULL
    }
    
  })
  
  output$chbox_ambiente_r_2 <- renderUI({
    
    if(input$indicador_ambiente_r %in% lista_ind_2 & input$indicador_ambiente_r %notin% lista_especial){
      
      # if(input$ambiente_r_corte %notin% c("Total", "Departamento") & input$indicador_ambiente_r %notin% lista_vunico) {
      
      ambiente_r_corte_var_2 <- rlang::sym(to_varname(input$ambiente_r_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_ambiente_r_2",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_ambiente_r() %>%
                           filter(corte_2 == input$ambiente_r_corte_2) %>% 
                           distinct(!!ambiente_r_corte_var_2) %>%
                           pull(),
                         selected = dat_ambiente_r() %>%
                           filter(corte_2 == input$ambiente_r_corte_2) %>% 
                           filter(jerarquia_cat_2 == "1") %>%
                           distinct(!!ambiente_r_corte_var_2) %>%
                           pull()
      )
      
      # } else {
      #   
      #   return(NULL)
      #   
      #   }
      
    } else {
      
      return(NULL)
      
    }      
  })
  
  
  output$selector_ambiente_r_corte <- renderUI({
    
    selectInput(
      inputId = "ambiente_r_corte",
      label = "Seleccione corte:",
      choices = dat_ambiente_r() %>% 
        select(corte) %>%
        arrange(corte) %>% 
        unique() %>% 
        pull(),
      selected = dat_ambiente_r() %>% 
        filter(jerarquia == "1") %>%  
        distinct(corte) %>% 
        pull()
    )
    
  })
  
  output$chbox_ambiente_r <- renderUI({
    
    if(input$ambiente_r_corte %in% lista_ind_2 & input$ambiente_r_corte %notin% c("Total", "Departamento") & input$indicador_ambiente_r %notin% lista_vunico) {
      
      ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
      ambiente_r_corte_var_2 <- rlang::sym(to_varname(input$ambiente_r_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_ambiente_r",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_ambiente_r() %>%
                           filter(!!ambiente_r_corte_var_2 %in% input$checkbox_ambiente_r_2) %>%
                           filter(corte == input$ambiente_r_corte) %>% 
                           distinct(!!ambiente_r_corte_var) %>%
                           pull(),
                         selected = dat_ambiente_r() %>%
                           filter(!!ambiente_r_corte_var_2 %in% input$checkbox_ambiente_r_2) %>%
                           filter(corte == input$ambiente_r_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!ambiente_r_corte_var) %>%
                           pull()
      )
      
    } else if(input$ambiente_r_corte %notin% lista_ind_2 & input$ambiente_r_corte %notin% c("Total", "Departamento") & input$indicador_ambiente_r %notin% lista_vunico) {
      
      ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
      
      checkboxGroupInput(inputId = "checkbox_ambiente_r",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_ambiente_r() %>%
                           filter(corte == input$ambiente_r_corte) %>% 
                           distinct(!!ambiente_r_corte_var) %>%
                           pull(),
                         selected = dat_ambiente_r() %>%
                           filter(corte == input$ambiente_r_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!ambiente_r_corte_var) %>%
                           pull()
      )
      
    } else {
      
      return(NULL)
    }
    
  })
  
  # Selector de fecha
  output$s_ambiente_r_fecha <- renderUI({
    
    if(input$ambiente_r_corte == "Departamento" & input$indicador_ambiente_r %notin% lista_ind_2) {
      
      req(input$ambiente_r_corte, input$indicador_ambiente_r)
      
      req(nrow(dat_ambiente_r()) > 0)
      
      selectInput(
        inputId = "fecha_dpto_ambiente_r",
        label = "Seleccione año:",
        choices = dat_ambiente_r() %>% 
          filter(nomindicador == input$indicador_ambiente_r) %>%
          drop_na(Valor) %>%
          select(ano) %>%
          arrange(desc(ano)) %>% 
          unique() %>% 
          pull(),
        selected = max(input$ano)
      )
      
    } else if (input$indicador_ambiente_r %in% lista_serie_cat){
      
      return(NULL)
      
      
    } else if (input$indicador_ambiente_r %in% lista_vunico){
      
      return(NULL)
      
    } else  {
      
      req(input$ambiente_r_corte, input$indicador_ambiente_r)
      req(nrow(dat_ambiente_r()) > 0)
      
      tagList(
        # tags$style(type = 'text/css', 
        #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
        # div(id = 'big_slider',
        
        sliderInput("fecha_ambiente_r", 
                    label = "Rango de tiempo", 
                    sep = "",
                    dragRange = T,
                    min = min(dat_ambiente_r()$ano), 
                    max = max(dat_ambiente_r()$ano), 
                    value = c(min(dat_ambiente_r()$ano), 
                              max(dat_ambiente_r()$ano))
        )
      )
      
    }
  })
  
  # # Selector de corte según categoría y data temporal
  # dat_ambiente_r <- reactive({
  #   
  #   req(input$ambiente_r_corte)
  #   
  #   if(input$indicador_ambiente_r %in% lista_ind_2){
  #     
  #     dat_salarios <- dat_ambiente_r() %>%
  #       filter(corte_2 == input$ambiente_r_corte_2) %>% 
  #       filter(corte == input$ambiente_r_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   } else {
  #     
  #     dat_ambiente_r() %>%
  #       filter(corte == input$ambiente_r_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   }
  # })
  
  # * Metadata   ======================================================
  
  # Title
  output$title_ambiente_r <- renderUI({ 
    helpText(HTML(unique(dat_ambiente_r()$nomindicador)))
  })
  
  # Subtitle
  output$subtitle_ambiente_r <- renderUI({ 
    helpText(HTML(unique(dat_ambiente_r()$definicion)))
  })
  
  # Nota
  output$nota_ambiente_r <- renderUI({ 
    
    if(input$indicador_ambiente_r %in% lista_nota){
      helpText(HTML(unique(dat_ambiente_r()$nota)))
      
    } else(NULL)
  })  
  
  # Nombre conceptual
  output$conindicador_ambiente_r <- renderUI({ 
    helpText(HTML(paste("<b> Nombre conceptual:</b>", unique(dat_ambiente_r()$conindicador))))
  })
  
  # Calculo
  output$calculo_ambiente_r <- renderUI({ 
    helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_ambiente_r()$calculo))))
  })
  
  # Observaciones
  output$observacion_ambiente_r <- renderUI({ 
    helpText(HTML(paste("<b> Observaciones:</b>", unique(dat_ambiente_r()$observaciones))))
  })
  
  # Actualización
  output$actualizacion_ambiente_r <- renderUI({ 
    helpText(HTML(paste("<b> Última actualización:</b>", unique(dat_ambiente_r()$actualizacion))))
  })
  
  # * Gráficos   ======================================================
  
  output$plot_ambiente_r <- renderPlot({
    
    req(input$indicador_ambiente_r, input$ambiente_r_corte)
    
    if(input$indicador_ambiente_r %in% lista_serie_cat){
      
      req(input$indicador_ambiente_r)
      
      # Total
      if(input$ambiente_r_corte == "Total"){
        
        dat_plot <- dat_ambiente_r() %>%
          filter(corte == "Total")
        
        plot <- ggplot(dat_plot, aes(x = fecha_cat, y = Valor, group = 1)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_ambiente_r),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador ambiente r.png", width = 40, height = 25, units = "cm")
        
        # Según corte
      } else if(input$ambiente_r_corte != "Total") {
        
        ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
        
        dat_plot <- dat_ambiente_r() %>%
          filter(corte == input$ambiente_r_corte) %>%
          filter(!!ambiente_r_corte_var %in% input$checkbox_ambiente_r)
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            colour = ambiente_r_corte_var, group = ambiente_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_r,
                                    "según",
                                    tolower(input$ambiente_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador ambiente r.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_ambiente_r %in% lista_especial ){
      
      if (input$ambiente_r_corte_2 == "Total"){        
        
        req(input$ambiente_r_corte, input$indicador_ambiente_r)
        
        ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
        
        dat_plot <- dat_ambiente_r() %>%
          filter(ano >= input$fecha_ambiente_r[1] &
                   ano <= input$fecha_ambiente_r[2]) %>%
          filter(corte == input$ambiente_r_corte) %>%
          # filter(!!ambiente_r_corte_var %in% input$checkbox_ambiente_r) %>% 
          filter(corte_2 == input$ambiente_r_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = ambiente_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_r,
                                    "según",
                                    tolower(input$ambiente_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador ambiente r.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        req(input$ambiente_r_corte, input$indicador_ambiente_r)
        
        ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
        ambiente_r_corte_var_2 <- rlang::sym(to_varname(input$ambiente_r_corte_2))
        
        dat_plot <- dat_ambiente_r() %>%
          filter(ano >= input$fecha_ambiente_r[1] &
                   ano <= input$fecha_ambiente_r[2]) %>%
          filter(corte == input$ambiente_r_corte) %>%
          # filter(!!ambiente_r_corte_var %in% input$checkbox_ambiente_r) %>% 
          filter(corte_2 == input$ambiente_r_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = ambiente_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_r,
                                    "según",
                                    tolower(input$ambiente_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", ambiente_r_corte_var_2)))
        
        print(plot)
        ggsave("www/indicador ambiente r.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_ambiente_r %in% lista_vunico & input$indicador_ambiente_r %in% lista_ind_2){
      
      req(input$ambiente_r_corte, input$indicador_ambiente_r)
      
      ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
      ambiente_r_corte_var_2 <- rlang::sym(to_varname(input$ambiente_r_corte_2))
      
      dat_plot <- dat_ambiente_r() %>%
        filter(ano >= input$fecha_ambiente_r[1] &
                 ano <= input$fecha_ambiente_r[2]) %>%
        filter(corte == input$ambiente_r_corte) %>%
        filter(!!ambiente_r_corte_var %in% input$checkbox_ambiente_r) %>%
        filter(!!ambiente_r_corte_var_2 %in% input$checkbox_ambiente_r_2) %>%
        filter(corte_2 == input$ambiente_r_corte_2)
      
      plot <- ggplot(dat_plot,
                     aes_string(x = "fecha_cat", y = "Valor",
                                fill = ambiente_r_corte_var)) +
        geom_col(position = "dodge", width = .7, alpha = .8) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x=element_blank(),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_ambiente_r,
                                  "según",
                                  tolower(input$ambiente_r_corte),
                                  "en",
                                  unique(dat_plot$fecha_cat))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_fill_brewer(name = "", palette = "Paired") +
        facet_wrap(as.formula(paste("~", ambiente_r_corte_var_2)))
      
      print(plot)
      ggsave("www/indicador ambiente r.png", width = 40, height = 25, units = "cm")
      
    } else if(input$indicador_ambiente_r %in% lista_vunico & input$ambiente_r_corte != "Departamento") {
      
      req(input$ambiente_r_corte, input$indicador_ambiente_r)
      
      dat_plot <- dat_ambiente_r() %>%
        filter(corte == input$ambiente_r_corte) %>%
        janitor::remove_empty("cols")
      
      if(input$ambiente_r_corte == "Total"){
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor")) +
          geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
          geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_r,
                                    "según",
                                    tolower(input$ambiente_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador ambiente r.png", width = 40, height = 25, units = "cm")
        
        
      } else {
        
        ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor",
                                  fill = ambiente_r_corte_var)) +
          geom_col(position = "dodge", alpha = .8, stroke = 1, color = "black") +
          geom_text(aes_string(group = ambiente_r_corte_var, label = "Valor"),
                    position = position_dodge2(width = .9), vjust = -.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_r,
                                    "según",
                                    tolower(input$ambiente_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador ambiente r.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$indicador_ambiente_r %in% lista_ind_2) {
      
      req(input$ambiente_r_corte, input$ambiente_r_corte_2,
          input$fecha_ambiente_r, input$checkbox_ambiente_r)
      
      ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
      ambiente_r_corte_var_2 <- rlang::sym(to_varname(input$ambiente_r_corte_2))
      
      dat_plot <- dat_ambiente_r() %>%
        filter(ano >= input$fecha_ambiente_r[1] &
                 ano <= input$fecha_ambiente_r[2]) %>%
        filter(corte == input$ambiente_r_corte) %>%
        filter(!!ambiente_r_corte_var_2 %in% input$checkbox_ambiente_r_2) %>%
        filter(!!ambiente_r_corte_var %in% input$checkbox_ambiente_r) %>% 
        filter(corte_2 == input$ambiente_r_corte_2)
      
      if(input$ambiente_r_corte_2 == "Total"){
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == "Total")
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = ambiente_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_r,
                                    "según",
                                    tolower(input$ambiente_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
      } else if(input$ambiente_r_corte_2 != "Total") {
        
        ambiente_r_corte_var_2 <- rlang::sym(to_varname(input$ambiente_r_corte_2))
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == input$ambiente_r_corte_2)
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = ambiente_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          scale_x_continuous(breaks = int_breaks) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_r,
                                    "según",
                                    tolower(input$ambiente_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          facet_wrap(as.formula(paste("~", ambiente_r_corte_var_2)))
        
      }
      
      print(plot)
      ggsave("www/indicador ambiente r.png", width = 40, height = 25, units = "cm")
      
      
    } else if(input$ambiente_r_corte == "Total") {
      
      req(input$indicador_ambiente_r, input$fecha_ambiente_r)
      
      dat_plot <- dat_ambiente_r() %>%
        filter(ano >= input$fecha_ambiente_r[1] &
                 ano <= input$fecha_ambiente_r[2]) %>%
        filter(corte == "Total")
      
      plot <- ggplot(dat_plot,
                     aes(x = fecha, y = Valor)) +
        geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
        geom_point(size = 3, colour = color_defecto) +
        scale_x_continuous(breaks = int_breaks) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(input$indicador_ambiente_r),
             caption = wrapit(unique(dat_plot$cita)))
      
      print(plot)
      ggsave("www/indicador ambiente r.png", width = 40, height = 25, units = "cm")
      
      
    } else if(input$ambiente_r_corte == "Departamento" &
              input$indicador_ambiente_r %notin% lista_ind_2 ) {
      
      req(input$indicador_ambiente_r, input$fecha_dpto_ambiente_r)
      
      dat_plot <- dat_ambiente_r() %>%
        filter(corte == "Departamento") %>%
        filter(ano == input$fecha_dpto_ambiente_r) %>%
        select(departamento, Valor, fuente, cita)
      
      dep_j <- dep %>%
        left_join(dat_plot, by = c("nombre" = "departamento"))
      
      plot <-  ggplot(dep_j, aes(fill = Valor)) +
        geom_sf() +
        geom_sf_text(aes(label = Valor), colour = "black",
                     size = 4, fontface = "bold")+
        viridis::scale_fill_viridis(name = "", direction = -1)+
        labs(x = "",
             y = "",
        )+
        theme_bdd(base_size = 14) +
        theme(axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.grid.major = element_line(colour = "transparent"),
        ) +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_ambiente_r,
                                  "en",
                                  input$fecha_dpto_ambiente_r), w = 80),
             caption = wrapit(unique(dat_plot$cita), w = 80))
      
      print(plot)
      ggsave("www/indicador ambiente r.png", width = 40, height = 25, units = "cm")
      
      
    } else if(input$ambiente_r_corte != "Total") {
      
      req(input$ambiente_r_corte, input$indicador_ambiente_r,
          input$fecha_ambiente_r, input$checkbox_ambiente_r)
      
      dat_plot <- dat_ambiente_r() %>%
        filter(ano >= input$fecha_ambiente_r[1] &
                 ano <= input$fecha_ambiente_r[2]) %>%
        filter(corte == input$ambiente_r_corte) %>%
        janitor::remove_empty("cols")
      
      ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
      
      dat_plot <- filter(dat_plot,
                         !!ambiente_r_corte_var %in% input$checkbox_ambiente_r)
      
      plot <- ggplot(dat_plot,
                     aes_string(x = "fecha", y = "Valor", colour = ambiente_r_corte_var)) +
        geom_line(size = 1, alpha = 0.5) +
        geom_point(size = 3) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        scale_x_continuous(breaks = int_breaks) +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_ambiente_r,
                                  "según",
                                  tolower(input$ambiente_r_corte))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_colour_manual(name = "", values = paleta_expandida)
      
      print(plot)
      ggsave("www/indicador ambiente r.png", width = 40, height = 25, units = "cm")
      
    }
    
  })
  
  
  # * Descarga gráficos   =============================================
  
  output$baja_p_ambiente_r <- downloadHandler(
    filename <- function() {
      paste("indicador ambiente r", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/indicador ambiente r.png", file)
    },
    contentType = "www/indicador ambiente r"
  )
  
  
  # * Tablas   ========================================================
  
  # Data
  ambiente_r_tab <- reactive({
    
    if(input$indicador_ambiente_r %in%  lista_especial){
      
      ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
      ambiente_r_corte_var_2 <- rlang::sym(to_varname(input$ambiente_r_corte_2))
      
      dat_ambiente_r() %>%
        filter(corte_2 == input$ambiente_r_corte_2) %>% 
        select(Fecha, ambiente_r_corte_var, ambiente_r_corte_var_2, Valor) %>%
        arrange(desc(Fecha)) %>%
        pivot_wider(values_from = "Valor",
                    names_from = ambiente_r_corte_var) 
      
    } else if(input$indicador_ambiente_r %in% lista_vunico & input$ambiente_r_corte == "Total"){
      
      req(input$ambiente_r_corte, input$indicador_ambiente_r)
      
      dat_ambiente_r() %>%
        filter(corte == "Total") %>% 
        select(fecha_cat, Valor) %>%
        arrange(desc(fecha_cat)) %>%
        rename(Fecha = fecha_cat)
      
    } else if(input$indicador_ambiente_r %in% lista_vunico & input$ambiente_r_corte != "Total") {
      
      req(input$ambiente_r_corte, input$indicador_ambiente_r)
      
      ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
      
      dat_cut <- dat_ambiente_r() %>%
        filter(corte == input$ambiente_r_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        select(fecha_cat, ambiente_r_corte_var, Valor) %>%
        arrange(desc(fecha_cat)) %>% 
        rename(Fecha = fecha_cat) %>%
        pivot_wider(values_from = "Valor",
                    names_from = ambiente_r_corte_var)
      
      
    } else if(input$indicador_ambiente_r %in% lista_ind_2) {
      
      req(input$ambiente_r_corte, input$indicador_ambiente_r, input$fecha_ambiente_r)
      
      ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
      
      dat_cut <- dat_ambiente_r() %>%
        filter(corte == input$ambiente_r_corte) %>%
        filter(!!ambiente_r_corte_var %in% input$checkbox_ambiente_r)
      
      if(input$ambiente_r_corte_2 == "Total"){
        
        dat_cut %>%
          filter(ano >= input$fecha_ambiente_r[1] &
                   ano <= input$fecha_ambiente_r[2]) %>%
          filter(corte_2 == "Total") %>% 
          select(Fecha, ambiente_r_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = ambiente_r_corte_var)
        
      } else if(input$ambiente_r_corte_2 != "Total") {
        
        ambiente_r_corte_var_2 <- rlang::sym(to_varname(input$ambiente_r_corte_2))
        
        dat_cut %>%
          filter(ano >= input$fecha_ambiente_r[1] &
                   ano <= input$fecha_ambiente_r[2]) %>%
          filter(corte_2 == input$ambiente_r_corte_2) %>% 
          select(Fecha, ambiente_r_corte_var, ambiente_r_corte_var_2, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = ambiente_r_corte_var) 
        
      }
      
    } else if(input$ambiente_r_corte == "Total") {
      
      req(input$ambiente_r_corte, input$indicador_ambiente_r, input$fecha_ambiente_r)
      
      dat_ambiente_r() %>%
        filter(corte == "Total") %>% 
        filter(ano >= input$fecha_ambiente_r[1] &
                 ano <= input$fecha_ambiente_r[2]) %>%
        select(Fecha, Valor) %>%
        arrange(desc(Fecha))
      
    } else if(input$ambiente_r_corte != "Total") {
      
      req(input$ambiente_r_corte, input$indicador_ambiente_r, input$fecha_ambiente_r)
      
      ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
      
      dat_cut <- dat_ambiente_r() %>%
        filter(corte == input$ambiente_r_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        filter(ano >= input$fecha_ambiente_r[1] &
                 ano <= input$fecha_ambiente_r[2]) %>% 
        select(Fecha, ambiente_r_corte_var, Valor) %>%
        arrange(desc(Fecha)) %>% 
        pivot_wider(values_from = "Valor",
                    names_from = ambiente_r_corte_var)
      
    }
  })
  
  # Metadata 
  ambiente_r_meta <- reactive({
    
    dat_ambiente_r() %>%
      select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, observaciones, actualizacion, cita) %>% 
      mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
      distinct() %>% 
      gather(key = " ", value = " ")
    
  })
  
  # Excel
  list_ambiente_r <- reactive({
    list_ambiente_r <- list("Data" = ambiente_r_tab(),
                            "Metadata" = ambiente_r_meta())
  })
  
  # Render
  output$table_ambiente_r <- renderDT({
    
    DT::datatable(ambiente_r_tab(),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    input$indicador_ambiente_r,
                    style = "color:black; font-size:110%;")
    ) 
    
  })
  
  # * Descarga tablas   ================================================
  
  output$dwl_tab_ambiente_r <- downloadHandler(
    
    filename = function() {
      paste("resultados-", input$indicador_ambiente_r, ".xlsx", sep = "")
    },
    content = function(file) {
      
      openxlsx::write.xlsx(list_ambiente_r(), file)
      
    }
  )
  
  
  ### 9.1. Alimentación Politicas==============================================

  # # * Data reactiva   =================================================
  # dat_alimentacion_pp <- reactive({
  # 
  #   req(input$indicador_alimentacion_pp)
  # 
  #   dat %>%
  #     filter(nomindicador == input$indicador_alimentacion_pp)
  # 
  # })
  # 
  # output$selector_alimentacion_pp_corte_2 <- renderUI({
  # 
  #   if(input$indicador_alimentacion_pp %in% lista_ind_2){
  # 
  #     selectInput(
  #       inputId = "alimentacion_pp_corte_2",
  #       label = "Seleccione primer corte:",
  #       choices = dat_alimentacion_pp() %>%
  #         select(corte_2) %>%
  #         arrange(corte_2) %>%
  #         unique() %>%
  #         pull(),
  #       selected = dat_alimentacion_pp() %>%
  #         filter(jerarquia == "1") %>%
  #         distinct(corte_2) %>%
  #         pull()
  #     )
  # 
  #   } else {
  # 
  #     NULL
  #   }
  # 
  # })
  # 
  # output$chbox_alimentacion_pp_2 <- renderUI({
  # 
  #   if(input$indicador_alimentacion_pp %in% lista_ind_2 & input$indicador_alimentacion_pp %notin% lista_especial){
  # 
  #     # if(input$alimentacion_pp_corte %notin% c("Total", "Departamento") & input$indicador_alimentacion_pp %notin% lista_vunico) {
  # 
  #     alimentacion_pp_corte_var_2 <- rlang::sym(to_varname(input$alimentacion_pp_corte_2))
  # 
  #     checkboxGroupInput(inputId = "checkbox_alimentacion_pp_2",
  #                        label = "Seleccione categorías",
  #                        inline = TRUE,
  #                        choices =  dat_alimentacion_pp() %>%
  #                          filter(corte_2 == input$alimentacion_pp_corte_2) %>%
  #                          distinct(!!alimentacion_pp_corte_var_2) %>%
  #                          pull(),
  #                        selected = dat_alimentacion_pp() %>%
  #                          filter(corte_2 == input$alimentacion_pp_corte_2) %>%
  #                          filter(jerarquia_cat_2 == "1") %>%
  #                          distinct(!!alimentacion_pp_corte_var_2) %>%
  #                          pull()
  #     )
  # 
  #     # } else {
  #     #
  #     #   return(NULL)
  #     #
  #     #   }
  # 
  #   } else {
  # 
  #     return(NULL)
  # 
  #   }
  # })
  # 
  # 
  # output$selector_alimentacion_pp_corte <- renderUI({
  # 
  #   selectInput(
  #     inputId = "alimentacion_pp_corte",
  #     label = "Seleccione corte:",
  #     choices = dat_alimentacion_pp() %>%
  #       select(corte) %>%
  #       arrange(corte) %>%
  #       unique() %>%
  #       pull(),
  #     selected = dat_alimentacion_pp() %>%
  #       filter(jerarquia == "1") %>%
  #       distinct(corte) %>%
  #       pull()
  #   )
  # 
  # })
  # 
  # output$chbox_alimentacion_pp <- renderUI({
  # 
  #   if(input$alimentacion_pp_corte %in% lista_ind_2 & input$alimentacion_pp_corte %notin% c("Total", "Departamento") & input$indicador_alimentacion_pp %notin% lista_vunico) {
  # 
  #     alimentacion_pp_corte_var <- rlang::sym(to_varname(input$alimentacion_pp_corte))
  #     alimentacion_pp_corte_var_2 <- rlang::sym(to_varname(input$alimentacion_pp_corte_2))
  # 
  #     checkboxGroupInput(inputId = "checkbox_alimentacion_pp",
  #                        label = "Seleccione categorías",
  #                        inline = TRUE,
  #                        choices =  dat_alimentacion_pp() %>%
  #                          filter(!!alimentacion_pp_corte_var_2 %in% input$checkbox_alimentacion_pp_2) %>%
  #                          filter(corte == input$alimentacion_pp_corte) %>%
  #                          distinct(!!alimentacion_pp_corte_var) %>%
  #                          pull(),
  #                        selected = dat_alimentacion_pp() %>%
  #                          filter(!!alimentacion_pp_corte_var_2 %in% input$checkbox_alimentacion_pp_2) %>%
  #                          filter(corte == input$alimentacion_pp_corte) %>%
  #                          filter(jerarquia_cat == "1") %>%
  #                          distinct(!!alimentacion_pp_corte_var) %>%
  #                          pull()
  #     )
  # 
  #   } else if(input$alimentacion_pp_corte %notin% lista_ind_2 & input$alimentacion_pp_corte %notin% c("Total", "Departamento") & input$indicador_alimentacion_pp %notin% lista_vunico) {
  # 
  #     alimentacion_pp_corte_var <- rlang::sym(to_varname(input$alimentacion_pp_corte))
  # 
  #     checkboxGroupInput(inputId = "checkbox_alimentacion_pp",
  #                        label = "Seleccione categorías",
  #                        inline = TRUE,
  #                        choices =  dat_alimentacion_pp() %>%
  #                          filter(corte == input$alimentacion_pp_corte) %>%
  #                          distinct(!!alimentacion_pp_corte_var) %>%
  #                          pull(),
  #                        selected = dat_alimentacion_pp() %>%
  #                          filter(corte == input$alimentacion_pp_corte) %>%
  #                          filter(jerarquia_cat == "1") %>%
  #                          distinct(!!alimentacion_pp_corte_var) %>%
  #                          pull()
  #     )
  # 
  #   } else {
  # 
  #     return(NULL)
  #   }
  # 
  # })
  # 
  # # Selector de fecha
  # output$s_alimentacion_pp_fecha <- renderUI({
  # 
  #   if(input$alimentacion_pp_corte == "Departamento" & input$indicador_alimentacion_pp %notin% lista_ind_2) {
  # 
  #     req(input$alimentacion_pp_corte, input$indicador_alimentacion_pp)
  # 
  #     req(nrow(dat_alimentacion_pp()) > 0)
  # 
  #     selectInput(
  #       inputId = "fecha_dpto_alimentacion_pp",
  #       label = "Seleccione año:",
  #       choices = dat_alimentacion_pp() %>%
  #         filter(nomindicador == input$indicador_alimentacion_pp) %>%
  #         drop_na(Valor) %>%
  #         select(ano) %>%
  #         arrange(desc(ano)) %>%
  #         unique() %>%
  #         pull(),
  #       selected = max(input$ano)
  #     )
  # 
  #   } else if (input$indicador_alimentacion_pp %in% lista_serie_cat){
  # 
  #     return(NULL)
  # 
  # 
  #   } else if (input$indicador_alimentacion_pp %in% lista_vunico){
  # 
  #     return(NULL)
  # 
  #   } else  {
  # 
  #     req(input$alimentacion_pp_corte, input$indicador_alimentacion_pp)
  #     req(nrow(dat_alimentacion_pp()) > 0)
  # 
  #     tagList(
  #       # tags$style(type = 'text/css',
  #       #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
  #       # div(id = 'big_slider',
  # 
  #       sliderInput("fecha_alimentacion_pp",
  #                   label = "Rango de tiempo",
  #                   sep = "",
  #                   dragRange = T,
  #                   min = min(dat_alimentacion_pp()$ano),
  #                   max = max(dat_alimentacion_pp()$ano),
  #                   value = c(min(dat_alimentacion_pp()$ano),
  #                             max(dat_alimentacion_pp()$ano))
  #       )
  #     )
  # 
  #   }
  # })
  # 
  # # # Selector de corte según categoría y data temporal
  # # dat_alimentacion_pp <- reactive({
  # #
  # #   req(input$alimentacion_pp_corte)
  # #
  # #   if(input$indicador_alimentacion_pp %in% lista_ind_2){
  # #
  # #     dat_salarios <- dat_alimentacion_pp() %>%
  # #       filter(corte_2 == input$alimentacion_pp_corte_2) %>%
  # #       filter(corte == input$alimentacion_pp_corte) %>%
  # #       janitor::remove_empty("cols")
  # #
  # #   } else {
  # #
  # #     dat_alimentacion_pp() %>%
  # #       filter(corte == input$alimentacion_pp_corte) %>%
  # #       janitor::remove_empty("cols")
  # #
  # #   }
  # # })
  # 
  # # * Metadata   ======================================================
  # 
  # # Title
  # output$title_alimentacion_pp <- renderUI({
  #   helpText(HTML(unique(dat_alimentacion_pp()$nomindicador)))
  # })
  # 
  # # Subtitle
  # output$subtitle_alimentacion_pp <- renderUI({
  #   helpText(HTML(unique(dat_alimentacion_pp()$definicion)))
  # })
  # 
  # # Nombre conceptual
  # output$conindicador_alimentacion_pp <- renderUI({
  #   helpText(HTML(paste("<b> Nombre conceptual:</b>", unique(dat_alimentacion_pp()$conindicador))))
  # })
  # 
  # # Calculo
  # output$calculo_alimentacion_pp <- renderUI({
  #   helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_alimentacion_pp()$calculo))))
  # })
  # 
  # # Observaciones
  # output$observacion_alimentacion_pp <- renderUI({
  #   helpText(HTML(paste("<b> Observaciones:</b>", unique(dat_alimentacion_pp()$observaciones))))
  # })
  # 
  #   # Actualización
  # output$actualizacion_alimentacion_pp <- renderUI({ 
  #   helpText(HTML(paste("<b> Última actualización:</b>", unique(dat_alimentacion_pp()$actualizacion))))
  # })
  
  # # * Gráficos   ======================================================
  # 
  # output$plot_alimentacion_pp <- renderPlot({
  # 
  #   req(input$indicador_alimentacion_pp, input$alimentacion_pp_corte)
  # 
  #   if(input$indicador_alimentacion_pp %in% lista_serie_cat){
  # 
  #     req(input$indicador_alimentacion_pp)
  # 
  #     # Total
  #     if(input$alimentacion_pp_corte == "Total"){
  # 
  #       dat_plot <- dat_alimentacion_pp() %>%
  #         filter(corte == "Total")
  # 
  #       plot <- ggplot(dat_plot, aes(x = fecha_cat, y = Valor, group = 1)) +
  #         geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
  #         geom_point(size = 3, colour = color_defecto) +
  #         theme_bdd(base_size = 12) +
  #         theme(axis.text.x = element_text(angle = 0),
  #               legend.position = "bottom") +
  #         labs(x = "",  y = "",
  #              title = wrapit(input$indicador_alimentacion_pp),
  #              caption = wrapit(unique(dat_plot$cita)))
  # 
  #       print(plot)
  #       ggsave("www/indicador alimentacion pp.png", width = 40, height = 25, units = "cm")
  # 
  #       # Según corte
  #     } else if(input$alimentacion_pp_corte != "Total") {
  # 
  #       alimentacion_pp_corte_var <- rlang::sym(to_varname(input$alimentacion_pp_corte))
  # 
  #       dat_plot <- dat_alimentacion_pp() %>%
  #         filter(corte == input$alimentacion_pp_corte) %>%
  #         filter(!!alimentacion_pp_corte_var %in% input$checkbox_alimentacion_pp)
  # 
  #       plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
  #                                           colour = alimentacion_pp_corte_var, group = alimentacion_pp_corte_var)) +
  #         geom_line(size = 1, alpha = 0.5) +
  #         geom_point(size = 3) +
  #         theme_bdd(base_size = 12) +
  #         theme(axis.text.x = element_text(angle = 0),
  #               legend.position = "bottom") +
  #         labs(x = "",  y = "",
  #              title = wrapit(paste(input$indicador_alimentacion_pp,
  #                                   "según",
  #                                   tolower(input$alimentacion_pp_corte))),
  #              caption = wrapit(unique(dat_plot$cita))) +
  #         scale_colour_manual(name = "", values = paleta_expandida)
  # 
  #       print(plot)
  #       ggsave("www/indicador alimentacion pp.png", width = 40, height = 25, units = "cm")
  # 
  #     }
  # 
  #   } else if(input$indicador_alimentacion_pp %in% lista_especial ){
  # 
  #     if (input$alimentacion_pp_corte_2 == "Total"){
  # 
  #       req(input$alimentacion_pp_corte, input$indicador_alimentacion_pp)
  # 
  #       alimentacion_pp_corte_var <- rlang::sym(to_varname(input$alimentacion_pp_corte))
  # 
  #       dat_plot <- dat_alimentacion_pp() %>%
  #         filter(ano >= input$fecha_alimentacion_pp[1] &
  #                  ano <= input$fecha_alimentacion_pp[2]) %>%
  #         filter(corte == input$alimentacion_pp_corte) %>%
  #         # filter(!!alimentacion_pp_corte_var %in% input$checkbox_alimentacion_pp) %>%
  #         filter(corte_2 == input$alimentacion_pp_corte_2)
  # 
  #       plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
  #                                           fill = alimentacion_pp_corte_var)) +
  #         geom_col(position = "dodge", width = .7, alpha = .8) +
  #         theme_bdd(base_size = 12) +
  #         theme(axis.text.x=element_blank(),
  #               legend.position = "bottom") +
  #         labs(x = "",  y = "",
  #              title = wrapit(paste(input$indicador_alimentacion_pp,
  #                                   "según",
  #                                   tolower(input$alimentacion_pp_corte),
  #                                   "en",
  #                                   unique(dat_plot$fecha_cat))),
  #              caption = wrapit(unique(dat_plot$cita))) +
  #         scale_fill_brewer(name = "", palette = "Paired")
  # 
  #       print(plot)
  #       ggsave("www/indicador alimentacion pp.png", width = 40, height = 25, units = "cm")
  # 
  #     } else {
  # 
  #       req(input$alimentacion_pp_corte, input$indicador_alimentacion_pp)
  # 
  #       alimentacion_pp_corte_var <- rlang::sym(to_varname(input$alimentacion_pp_corte))
  #       alimentacion_pp_corte_var_2 <- rlang::sym(to_varname(input$alimentacion_pp_corte_2))
  # 
  #       dat_plot <- dat_alimentacion_pp() %>%
  #         filter(ano >= input$fecha_alimentacion_pp[1] &
  #                  ano <= input$fecha_alimentacion_pp[2]) %>%
  #         filter(corte == input$alimentacion_pp_corte) %>%
  #         # filter(!!alimentacion_pp_corte_var %in% input$checkbox_alimentacion_pp) %>%
  #         filter(corte_2 == input$alimentacion_pp_corte_2)
  # 
  #       plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
  #                                           fill = alimentacion_pp_corte_var)) +
  #         geom_col(position = "dodge", width = .7, alpha = .8) +
  #         theme_bdd(base_size = 12) +
  #         theme(axis.text.x=element_blank(),
  #               legend.position = "bottom") +
  #         labs(x = "",  y = "",
  #              title = wrapit(paste(input$indicador_alimentacion_pp,
  #                                   "según",
  #                                   tolower(input$alimentacion_pp_corte),
  #                                   "en",
  #                                   unique(dat_plot$fecha_cat))),
  #              caption = wrapit(unique(dat_plot$cita))) +
  #         scale_fill_brewer(name = "", palette = "Paired") +
  #         facet_wrap(as.formula(paste("~", alimentacion_pp_corte_var_2)))
  # 
  #       print(plot)
  #       ggsave("www/indicador alimentacion pp.png", width = 40, height = 25, units = "cm")
  # 
  #     }
  # 
  #   } else if(input$indicador_alimentacion_pp %in% lista_vunico & input$indicador_alimentacion_pp %in% lista_ind_2){
  # 
  #     req(input$alimentacion_pp_corte, input$indicador_alimentacion_pp)
  # 
  #     alimentacion_pp_corte_var <- rlang::sym(to_varname(input$alimentacion_pp_corte))
  #     alimentacion_pp_corte_var_2 <- rlang::sym(to_varname(input$alimentacion_pp_corte_2))
  # 
  #     dat_plot <- dat_alimentacion_pp() %>%
  #       filter(ano >= input$fecha_alimentacion_pp[1] &
  #                ano <= input$fecha_alimentacion_pp[2]) %>%
  #       filter(corte == input$alimentacion_pp_corte) %>%
  #       filter(!!alimentacion_pp_corte_var %in% input$checkbox_alimentacion_pp) %>%
  #       filter(!!alimentacion_pp_corte_var_2 %in% input$checkbox_alimentacion_pp_2) %>%
  #       filter(corte_2 == input$alimentacion_pp_corte_2)
  # 
  #     plot <- ggplot(dat_plot,
  #                    aes_string(x = "fecha_cat", y = "Valor",
  #                               fill = alimentacion_pp_corte_var)) +
  #       geom_col(position = "dodge", width = .7, alpha = .8) +
  #       theme_bdd(base_size = 12) +
  #       theme(axis.text.x=element_blank(),
  #             legend.position = "bottom") +
  #       labs(x = "",  y = "",
  #            title = wrapit(paste(input$indicador_alimentacion_pp,
  #                                 "según",
  #                                 tolower(input$alimentacion_pp_corte),
  #                                 "en",
  #                                 unique(dat_plot$fecha_cat))),
  #            caption = wrapit(unique(dat_plot$cita))) +
  #       scale_fill_brewer(name = "", palette = "Paired") +
  #       facet_wrap(as.formula(paste("~", alimentacion_pp_corte_var_2)))
  # 
  #     print(plot)
  #     ggsave("www/indicador alimentacion pp.png", width = 40, height = 25, units = "cm")
  # 
  #   } else if(input$indicador_alimentacion_pp %in% lista_vunico & input$alimentacion_pp_corte != "Departamento") {
  # 
  #     req(input$alimentacion_pp_corte, input$indicador_alimentacion_pp)
  # 
  #     dat_plot <- dat_alimentacion_pp() %>%
  #       filter(corte == input$alimentacion_pp_corte) %>%
  #       janitor::remove_empty("cols")
  # 
  #     if(input$alimentacion_pp_corte == "Total"){
  # 
  #       plot <- ggplot(dat_plot,
  #                      aes_string(x = "fecha_cat", y = "Valor")) +
  #         geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
  #         geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
  #         theme_bdd(base_size = 12) +
  #         theme(axis.text.x=element_blank(),
  #               legend.position = "bottom") +
  #         labs(x = "",  y = "",
  #              title = wrapit(paste(input$indicador_alimentacion_pp,
  #                                   "según",
  #                                   tolower(input$alimentacion_pp_corte),
  #                                   "en",
  #                                   unique(dat_plot$fecha_cat))),
  #              caption = wrapit(unique(dat_plot$cita)))
  # 
  #       print(plot)
  #       ggsave("www/indicador alimentacion pp.png", width = 40, height = 25, units = "cm")
  # 
  # 
  #     } else {
  # 
  #       alimentacion_pp_corte_var <- rlang::sym(to_varname(input$alimentacion_pp_corte))
  # 
  #       plot <- ggplot(dat_plot,
  #                      aes_string(x = "fecha_cat", y = "Valor",
  #                                 fill = alimentacion_pp_corte_var)) +
  #         geom_col(position = "dodge", alpha = .8, stroke = 1, color = "black") +
  #         geom_text(aes_string(group = alimentacion_pp_corte_var, label = "Valor"),
  #                   position = position_dodge2(width = .9), vjust = -.4, fontface = "bold", size = 5) +
  #         theme_bdd(base_size = 12) +
  #         theme(axis.text.x=element_blank(),
  #               legend.position = "bottom") +
  #         labs(x = "",  y = "",
  #              title = wrapit(paste(input$indicador_alimentacion_pp,
  #                                   "según",
  #                                   tolower(input$alimentacion_pp_corte),
  #                                   "en",
  #                                   unique(dat_plot$fecha_cat))),
  #              caption = wrapit(unique(dat_plot$cita))) +
  #         scale_fill_brewer(name = "", palette = "Paired")
  # 
  #       print(plot)
  #       ggsave("www/indicador alimentacion pp.png", width = 40, height = 25, units = "cm")
  # 
  #     }
  # 
  # 
  #   } else if(input$indicador_alimentacion_pp %in% lista_ind_2) {
  # 
  #     req(input$alimentacion_pp_corte, input$alimentacion_pp_corte_2,
  #         input$fecha_alimentacion_pp, input$checkbox_alimentacion_pp)
  # 
  #     alimentacion_pp_corte_var <- rlang::sym(to_varname(input$alimentacion_pp_corte))
  #     alimentacion_pp_corte_var_2 <- rlang::sym(to_varname(input$alimentacion_pp_corte_2))
  # 
  #     dat_plot <- dat_alimentacion_pp() %>%
  #       filter(ano >= input$fecha_alimentacion_pp[1] &
  #                ano <= input$fecha_alimentacion_pp[2]) %>%
  #       filter(corte == input$alimentacion_pp_corte) %>%
  #       filter(!!alimentacion_pp_corte_var_2 %in% input$checkbox_alimentacion_pp_2) %>%
  #       filter(!!alimentacion_pp_corte_var %in% input$checkbox_alimentacion_pp) %>%
  #       filter(corte_2 == input$alimentacion_pp_corte_2)
  # 
  #     if(input$alimentacion_pp_corte_2 == "Total"){
  # 
  #       dat_plot <- dat_plot %>%
  #         filter(corte_2 == "Total")
  # 
  #       plot <- ggplot(dat_plot,
  #                      aes_string(x = "fecha", y = "Valor", colour = alimentacion_pp_corte_var)) +
  #         geom_line(size = 1, alpha = 0.5) +
  #         geom_point(size = 3) +
  #         theme_bdd(base_size = 12) +
  #         theme(axis.text.x = element_text(angle = 0),
  #               legend.position = "bottom") +
  #         scale_x_continuous(breaks = int_breaks) +
  #         labs(x = "",  y = "",
  #              title = wrapit(paste(input$indicador_alimentacion_pp,
  #                                   "según",
  #                                   tolower(input$alimentacion_pp_corte))),
  #              caption = wrapit(unique(dat_plot$cita))) +
  #         scale_colour_manual(name = "", values = paleta_expandida)
  # 
  #     } else if(input$alimentacion_pp_corte_2 != "Total") {
  # 
  #       alimentacion_pp_corte_var_2 <- rlang::sym(to_varname(input$alimentacion_pp_corte_2))
  # 
  #       dat_plot <- dat_plot %>%
  #         filter(corte_2 == input$alimentacion_pp_corte_2)
  # 
  #       plot <- ggplot(dat_plot,
  #                      aes_string(x = "fecha", y = "Valor", colour = alimentacion_pp_corte_var)) +
  #         geom_line(size = 1, alpha = 0.5) +
  #         geom_point(size = 3) +
  #         theme_bdd(base_size = 12) +
  #         scale_x_continuous(breaks = int_breaks) +
  #         theme(axis.text.x = element_text(angle = 0),
  #               legend.position = "bottom") +
  #         labs(x = "",  y = "",
  #              title = wrapit(paste(input$indicador_alimentacion_pp,
  #                                   "según",
  #                                   tolower(input$alimentacion_pp_corte))),
  #              caption = wrapit(unique(dat_plot$cita))) +
  #         scale_colour_manual(name = "", values = paleta_expandida) +
  #         facet_wrap(as.formula(paste("~", alimentacion_pp_corte_var_2)))
  # 
  #     }
  # 
  #     print(plot)
  #     ggsave("www/indicador alimentacion pp.png", width = 40, height = 25, units = "cm")
  # 
  # 
  #     # Grafico simple normal
  #   } else if(input$alimentacion_pp_corte == "Total") {
  # 
  #     req(input$indicador_alimentacion_pp, input$fecha_alimentacion_pp)
  # 
  #     dat_plot <- dat_alimentacion_pp() %>%
  #       filter(ano >= input$fecha_alimentacion_pp[1] &
  #                ano <= input$fecha_alimentacion_pp[2]) %>%
  #       filter(corte == "Total")
  # 
  #     if(input$indicador_alimentacion_pp %in% lista_met){
  # 
  #       dat_plot <- dat_plot %>%
  #         mutate(metodo = case_when(
  #           fecha <= 2019 ~ "pre",
  #           fecha == 2020 ~ "2020",
  #           fecha == 2021 ~ "2021",
  #           fecha >= 2022 ~ "post",
  #         ))
  # 
  #       plot <- ggplot(dat_plot,
  #                      aes(x = fecha, y = Valor, alpha = metodo)) +
  #         geom_line(size = 1, colour = color_defecto) +
  #         geom_point(size = 3, colour = color_defecto) +
  #         # scale_x_continuous(breaks = int_breaks) +
  #         theme_bdd(base_size = 12) +
  #         theme(axis.text.x = element_text(angle = 0),
  #               legend.position = "none") +
  #         labs(x = "",  y = "",
  #              title = wrapit(input$indicador_alimentacion_pp),
  #              caption = wrapit(unique(dat_plot$cita))) +
  #         scale_alpha_manual(values = c("pre" = .3,
  #                                       "2020" = .5,
  #                                       "2021" = .6,
  #                                       "post"= .8))
  # 
  #       print(plot)
  #       ggsave("www/indicador alimentacion pp.png", width = 40, height = 25, units = "cm")
  # 
  #     } else {
  # 
  #       plot <- ggplot(dat_plot,
  #                      aes(x = fecha, y = Valor)) +
  #         geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
  #         geom_point(size = 3, colour = color_defecto) +
  #         scale_x_continuous(breaks = int_breaks) +
  #         theme_bdd(base_size = 12) +
  #         theme(axis.text.x = element_text(angle = 0),
  #               legend.position = "bottom") +
  #         labs(x = "",  y = "",
  #              title = wrapit(input$indicador_alimentacion_pp),
  #              caption = wrapit(unique(dat_plot$cita)))
  # 
  #       print(plot)
  #       ggsave("www/indicador alimentacion pp.png", width = 40, height = 25, units = "cm")
  # 
  #     }
  # 
  # 
  #   } else if(input$alimentacion_pp_corte == "Departamento" &
  #             input$indicador_alimentacion_pp %notin% lista_ind_2 ) {
  # 
  #     req(input$indicador_alimentacion_pp, input$fecha_dpto_alimentacion_pp)
  # 
  #     dat_plot <- dat_alimentacion_pp() %>%
  #       filter(corte == "Departamento") %>%
  #       filter(ano == input$fecha_dpto_alimentacion_pp) %>%
  #       select(departamento, Valor, fuente, cita)
  # 
  #     dep_j <- dep %>%
  #       left_join(dat_plot, by = c("nombre" = "departamento"))
  # 
  #     plot <-  ggplot(dep_j, aes(fill = Valor)) +
  #       geom_sf() +
  #       geom_sf_text(aes(label = Valor), colour = "black",
  #                    size = 4, fontface = "bold")+
  #       viridis::scale_fill_viridis(name = "", direction = -1)+
  #       labs(x = "",
  #            y = "",
  #       )+
  #       theme_bdd(base_size = 14) +
  #       theme(axis.line = element_blank(),
  #             axis.text.x = element_blank(),
  #             axis.text.y = element_blank(),
  #             axis.ticks = element_blank(),
  #             axis.title.x = element_blank(),
  #             axis.title.y = element_blank(),
  #             panel.grid.major = element_line(colour = "transparent"),
  #       ) +
  #       labs(x = "",  y = "",
  #            title = wrapit(paste(input$indicador_alimentacion_pp,
  #                                 "en",
  #                                 input$fecha_dpto_alimentacion_pp), w = 80),
  #            caption = wrapit(unique(dat_plot$cita), w = 80))
  # 
  #     print(plot)
  #     ggsave("www/indicador alimentacion pp.png", width = 40, height = 25, units = "cm")
  # 
  # 
  #   } else if(input$alimentacion_pp_corte != "Total") {
  # 
  #     req(input$alimentacion_pp_corte, input$indicador_alimentacion_pp,
  #         input$fecha_alimentacion_pp, input$checkbox_alimentacion_pp)
  # 
  #     dat_plot <- dat_alimentacion_pp() %>%
  #       filter(ano >= input$fecha_alimentacion_pp[1] &
  #                ano <= input$fecha_alimentacion_pp[2]) %>%
  #       filter(corte == input$alimentacion_pp_corte) %>%
  #       janitor::remove_empty("cols")
  # 
  #     alimentacion_pp_corte_var <- rlang::sym(to_varname(input$alimentacion_pp_corte))
  # 
  #     dat_plot <- filter(dat_plot,
  #                        !!alimentacion_pp_corte_var %in% input$checkbox_alimentacion_pp)
  # 
  #     if(input$indicador_alimentacion_pp %in% lista_met){
  # 
  #       dat_plot <- dat_plot %>%
  #         mutate(metodo = case_when(
  #           fecha <= 2019 ~ "pre",
  #           fecha == 2020 ~ "2020",
  #           fecha == 2021 ~ "2021",
  #           fecha >= 2022 ~ "post",
  #         ))
  # 
  #       plot <- ggplot(dat_plot,
  #                      aes_string(x = "fecha", y = "Valor", colour = alimentacion_pp_corte_var, alpha = "metodo")) +
  #         geom_line(size = 1) +
  #         geom_point(size = 3) +
  #         theme_bdd(base_size = 12) +
  #         theme(axis.text.x = element_text(angle = 0),
  #               legend.position = "bottom") +
  #         scale_x_continuous(breaks = int_breaks) +
  #         labs(x = "",  y = "",
  #              title = wrapit(paste(input$indicador_alimentacion_pp,
  #                                   "según",
  #                                   tolower(input$alimentacion_pp_corte))),
  #              caption = wrapit(unique(dat_plot$cita))) +
  #         scale_colour_manual(name = "", values = paleta_expandida) +
  #         scale_alpha_manual(values = c("pre" = .3,
  #                                       "2020" = .5,
  #                                       "2021" = .6,
  #                                       "post"= .8)) +
  #         guides(alpha = "none")
  # 
  #       print(plot)
  #       ggsave("www/indicador alimentacion pp.png", width = 40, height = 25, units = "cm")
  # 
  # 
  #     } else {
  # 
  #       plot <- ggplot(dat_plot,
  #                      aes_string(x = "fecha", y = "Valor", colour = alimentacion_pp_corte_var)) +
  #         geom_line(size = 1, alpha = 0.5) +
  #         geom_point(size = 3) +
  #         theme_bdd(base_size = 12) +
  #         theme(axis.text.x = element_text(angle = 0),
  #               legend.position = "bottom") +
  #         scale_x_continuous(breaks = int_breaks) +
  #         labs(x = "",  y = "",
  #              title = wrapit(paste(input$indicador_alimentacion_pp,
  #                                   "según",
  #                                   tolower(input$alimentacion_pp_corte))),
  #              caption = wrapit(unique(dat_plot$cita))) +
  #         scale_colour_manual(name = "", values = paleta_expandida)
  # 
  #       print(plot)
  #       ggsave("www/indicador alimentacion pp.png", width = 40, height = 25, units = "cm")
  # 
  # 
  #     }
  # 
  # 
  #   }
  # 
  # })
  # 
  # 
  # # * Descarga gráficos   =============================================
  # 
  # output$baja_p_alimentacion_pp <- downloadHandler(
  #   filename <- function() {
  #     paste("indicador alimentacion pp", "png", sep = ".")
  #   },
  # 
  #   content <- function(file) {
  #     file.copy("www/indicador alimentacion pp.png", file)
  #   },
  #   contentType = "www/indicador alimentacion pp"
  # )
  # 
  # 
  # # * Tablas   ========================================================
  # 
  # # Data
  # alimentacion_pp_tab <- reactive({
  # 
  #   if(input$indicador_alimentacion_pp %in%  lista_especial){
  # 
  #     alimentacion_pp_corte_var <- rlang::sym(to_varname(input$alimentacion_pp_corte))
  #     alimentacion_pp_corte_var_2 <- rlang::sym(to_varname(input$alimentacion_pp_corte_2))
  # 
  #     dat_alimentacion_pp() %>%
  #       filter(corte_2 == input$alimentacion_pp_corte_2) %>%
  #       select(Fecha, alimentacion_pp_corte_var, alimentacion_pp_corte_var_2, Valor) %>%
  #       arrange(desc(Fecha)) %>%
  #       pivot_wider(values_from = "Valor",
  #                   names_from = alimentacion_pp_corte_var)
  # 
  #   } else if(input$indicador_alimentacion_pp %in% lista_vunico & input$alimentacion_pp_corte == "Total"){
  # 
  #     req(input$alimentacion_pp_corte, input$indicador_alimentacion_pp)
  # 
  #     dat_alimentacion_pp() %>%
  #       filter(corte == "Total") %>%
  #       select(fecha_cat, Valor) %>%
  #       arrange(desc(fecha_cat)) %>%
  #       rename(Fecha = fecha_cat)
  # 
  #   } else if(input$indicador_alimentacion_pp %in% lista_vunico & input$alimentacion_pp_corte != "Total") {
  # 
  #     req(input$alimentacion_pp_corte, input$indicador_alimentacion_pp)
  # 
  #     alimentacion_pp_corte_var <- rlang::sym(to_varname(input$alimentacion_pp_corte))
  # 
  #     dat_cut <- dat_alimentacion_pp() %>%
  #       filter(corte == input$alimentacion_pp_corte) %>%
  #       janitor::remove_empty("cols")
  # 
  #     dat_cut %>%
  #       select(fecha_cat, alimentacion_pp_corte_var, Valor) %>%
  #       arrange(desc(fecha_cat)) %>%
  #       rename(Fecha = fecha_cat) %>%
  #       pivot_wider(values_from = "Valor",
  #                   names_from = alimentacion_pp_corte_var)
  # 
  # 
  #   } else if(input$indicador_alimentacion_pp %in% lista_ind_2) {
  # 
  #     req(input$alimentacion_pp_corte, input$indicador_alimentacion_pp, input$fecha_alimentacion_pp)
  # 
  #     alimentacion_pp_corte_var <- rlang::sym(to_varname(input$alimentacion_pp_corte))
  # 
  #     dat_cut <- dat_alimentacion_pp() %>%
  #       filter(corte == input$alimentacion_pp_corte) %>%
  #       filter(!!alimentacion_pp_corte_var %in% input$checkbox_alimentacion_pp)
  # 
  #     if(input$alimentacion_pp_corte_2 == "Total"){
  # 
  #       dat_cut %>%
  #         filter(ano >= input$fecha_alimentacion_pp[1] &
  #                  ano <= input$fecha_alimentacion_pp[2]) %>%
  #         filter(corte_2 == "Total") %>%
  #         select(Fecha, alimentacion_pp_corte_var, Valor) %>%
  #         arrange(desc(Fecha)) %>%
  #         pivot_wider(values_from = "Valor",
  #                     names_from = alimentacion_pp_corte_var)
  # 
  #     } else if(input$alimentacion_pp_corte_2 != "Total") {
  # 
  #       alimentacion_pp_corte_var_2 <- rlang::sym(to_varname(input$alimentacion_pp_corte_2))
  # 
  #       dat_cut %>%
  #         filter(ano >= input$fecha_alimentacion_pp[1] &
  #                  ano <= input$fecha_alimentacion_pp[2]) %>%
  #         filter(corte_2 == input$alimentacion_pp_corte_2) %>%
  #         select(Fecha, alimentacion_pp_corte_var, alimentacion_pp_corte_var_2, Valor) %>%
  #         arrange(desc(Fecha)) %>%
  #         pivot_wider(values_from = "Valor",
  #                     names_from = alimentacion_pp_corte_var)
  # 
  #     }
  # 
  #   } else if(input$alimentacion_pp_corte == "Total") {
  # 
  #     req(input$alimentacion_pp_corte, input$indicador_alimentacion_pp, input$fecha_alimentacion_pp)
  # 
  #     dat_alimentacion_pp() %>%
  #       filter(corte == "Total") %>%
  #       filter(ano >= input$fecha_alimentacion_pp[1] &
  #                ano <= input$fecha_alimentacion_pp[2]) %>%
  #       select(Fecha, Valor) %>%
  #       arrange(desc(Fecha))
  # 
  #   } else if(input$alimentacion_pp_corte != "Total") {
  # 
  #     req(input$alimentacion_pp_corte, input$indicador_alimentacion_pp, input$fecha_alimentacion_pp)
  # 
  #     alimentacion_pp_corte_var <- rlang::sym(to_varname(input$alimentacion_pp_corte))
  # 
  #     dat_cut <- dat_alimentacion_pp() %>%
  #       filter(corte == input$alimentacion_pp_corte) %>%
  #       janitor::remove_empty("cols")
  # 
  #     dat_cut %>%
  #       filter(ano >= input$fecha_alimentacion_pp[1] &
  #                ano <= input$fecha_alimentacion_pp[2]) %>%
  #       select(Fecha, alimentacion_pp_corte_var, Valor) %>%
  #       arrange(desc(Fecha)) %>%
  #       pivot_wider(values_from = "Valor",
  #                   names_from = alimentacion_pp_corte_var)
  # 
  #   }
  # })
  # 
  # # Metadata
  # alimentacion_pp_meta <- reactive({
  # 
  #   dat_alimentacion_pp() %>%
  #     select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, observaciones, actualizacion, cita) %>%
  #     mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>%
  #     distinct() %>%
  #     gather(key = " ", value = " ")
  # 
  # })
  # 
  # # Excel
  # list_alimentacion_pp <- reactive({
  #   list_alimentacion_pp <- list("Data" = alimentacion_pp_tab(),
  #                            "Metadata" = alimentacion_pp_meta())
  # })
  # 
  # # Render
  # output$table_alimentacion_pp <- renderDT({
  # 
  #   DT::datatable(alimentacion_pp_tab(),
  #                 rownames = FALSE,
  #                 caption = htmltools::tags$caption(
  #                   input$indicador_alimentacion_pp,
  #                   style = "color:black; font-size:110%;")
  #   )
  # 
  # })
  # 
  # # * Descarga tablas   ================================================
  # 
  # output$dwl_tab_alimentacion_pp <- downloadHandler(
  # 
  #   filename = function() {
  #     paste("resultados-", input$indicador_alimentacion_pp, ".xlsx", sep = "")
  #   },
  #   content = function(file) {
  # 
  #     openxlsx::write.xlsx(list_alimentacion_pp(), file)
  # 
  #   }
  # )




  ### 9.2. Alimentación Resultados   =============================================

  # * Data reactiva   =================================================

  dat_alimentacion_r <- reactive({

    req(input$indicador_alimentacion_r)

    dat %>%
      filter(nomindicador == input$indicador_alimentacion_r)

  })

  output$selector_alimentacion_r_corte_2 <- renderUI({

    if(input$indicador_alimentacion_r %in% lista_ind_2){

      selectInput(
        inputId = "alimentacion_r_corte_2",
        label = "Seleccione primer corte:",
        choices = dat_alimentacion_r() %>%
          select(corte_2) %>%
          arrange(corte_2) %>%
          unique() %>%
          pull(),
        selected = dat_alimentacion_r() %>%
          filter(jerarquia == "1") %>%
          distinct(corte_2) %>%
          pull()
      )

    } else {

      NULL
    }

  })

  output$chbox_alimentacion_r_2 <- renderUI({

    if(input$indicador_alimentacion_r %in% lista_ind_2 & input$indicador_alimentacion_r %notin% lista_especial){

      # if(input$alimentacion_r_corte %notin% c("Total", "Departamento") & input$indicador_alimentacion_r %notin% lista_vunico) {

      alimentacion_r_corte_var_2 <- rlang::sym(to_varname(input$alimentacion_r_corte_2))

      checkboxGroupInput(inputId = "checkbox_alimentacion_r_2",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_alimentacion_r() %>%
                           filter(corte_2 == input$alimentacion_r_corte_2) %>%
                           distinct(!!alimentacion_r_corte_var_2) %>%
                           pull(),
                         selected = dat_alimentacion_r() %>%
                           filter(corte_2 == input$alimentacion_r_corte_2) %>%
                           filter(jerarquia_cat_2 == "1") %>%
                           distinct(!!alimentacion_r_corte_var_2) %>%
                           pull()
      )

      # } else {
      #
      #   return(NULL)
      #
      #   }

    } else {

      return(NULL)

    }
  })


  output$selector_alimentacion_r_corte <- renderUI({

    selectInput(
      inputId = "alimentacion_r_corte",
      label = "Seleccione corte:",
      choices = dat_alimentacion_r() %>%
        select(corte) %>%
        arrange(corte) %>%
        unique() %>%
        pull(),
      selected = dat_alimentacion_r() %>%
        filter(jerarquia == "1") %>%
        distinct(corte) %>%
        pull()
    )

  })

  output$chbox_alimentacion_r <- renderUI({

    if(input$alimentacion_r_corte %in% lista_ind_2 & input$alimentacion_r_corte %notin% c("Total", "Departamento") & input$indicador_alimentacion_r %notin% lista_vunico) {

      alimentacion_r_corte_var <- rlang::sym(to_varname(input$alimentacion_r_corte))
      alimentacion_r_corte_var_2 <- rlang::sym(to_varname(input$alimentacion_r_corte_2))

      checkboxGroupInput(inputId = "checkbox_alimentacion_r",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_alimentacion_r() %>%
                           filter(!!alimentacion_r_corte_var_2 %in% input$checkbox_alimentacion_r_2) %>%
                           filter(corte == input$alimentacion_r_corte) %>%
                           distinct(!!alimentacion_r_corte_var) %>%
                           pull(),
                         selected = dat_alimentacion_r() %>%
                           filter(!!alimentacion_r_corte_var_2 %in% input$checkbox_alimentacion_r_2) %>%
                           filter(corte == input$alimentacion_r_corte) %>%
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!alimentacion_r_corte_var) %>%
                           pull()
      )

    } else if(input$alimentacion_r_corte %notin% lista_ind_2 & input$alimentacion_r_corte %notin% c("Total", "Departamento") & input$indicador_alimentacion_r %notin% lista_vunico) {

      alimentacion_r_corte_var <- rlang::sym(to_varname(input$alimentacion_r_corte))

      checkboxGroupInput(inputId = "checkbox_alimentacion_r",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_alimentacion_r() %>%
                           filter(corte == input$alimentacion_r_corte) %>%
                           distinct(!!alimentacion_r_corte_var) %>%
                           pull(),
                         selected = dat_alimentacion_r() %>%
                           filter(corte == input$alimentacion_r_corte) %>%
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!alimentacion_r_corte_var) %>%
                           pull()
      )

    } else {

      return(NULL)
    }

  })

  # Selector de fecha
  output$s_alimentacion_r_fecha <- renderUI({

    if(input$alimentacion_r_corte == "Departamento" & input$indicador_alimentacion_r %notin% lista_ind_2) {

      req(input$alimentacion_r_corte, input$indicador_alimentacion_r)

      req(nrow(dat_alimentacion_r()) > 0)

      selectInput(
        inputId = "fecha_dpto_alimentacion_r",
        label = "Seleccione año:",
        choices = dat_alimentacion_r() %>%
          filter(nomindicador == input$indicador_alimentacion_r) %>%
          drop_na(Valor) %>%
          select(ano) %>%
          arrange(desc(ano)) %>%
          unique() %>%
          pull(),
        selected = max(input$ano)
      )

    } else if (input$indicador_alimentacion_r %in% lista_serie_cat){

      return(NULL)


    } else if (input$indicador_alimentacion_r %in% lista_vunico){

      return(NULL)

    } else  {

      req(input$alimentacion_r_corte, input$indicador_alimentacion_r)
      req(nrow(dat_alimentacion_r()) > 0)

      tagList(
        # tags$style(type = 'text/css',
        #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
        # div(id = 'big_slider',

        sliderInput("fecha_alimentacion_r",
                    label = "Rango de tiempo",
                    sep = "",
                    dragRange = T,
                    min = min(dat_alimentacion_r()$ano),
                    max = max(dat_alimentacion_r()$ano),
                    value = c(min(dat_alimentacion_r()$ano),
                              max(dat_alimentacion_r()$ano))
        )
      )

    }
  })

  # # Selector de corte según categoría y data temporal
  # dat_alimentacion_r <- reactive({
  #
  #   req(input$alimentacion_r_corte)
  #
  #   if(input$indicador_alimentacion_r %in% lista_ind_2){
  #
  #     dat_salarios <- dat_alimentacion_r() %>%
  #       filter(corte_2 == input$alimentacion_r_corte_2) %>%
  #       filter(corte == input$alimentacion_r_corte) %>%
  #       janitor::remove_empty("cols")
  #
  #   } else {
  #
  #     dat_alimentacion_r() %>%
  #       filter(corte == input$alimentacion_r_corte) %>%
  #       janitor::remove_empty("cols")
  #
  #   }
  # })

  # * Metadata   ======================================================

  # Title
  output$title_alimentacion_r <- renderUI({
    helpText(HTML(unique(dat_alimentacion_r()$nomindicador)))
  })

  # Subtitle
  output$subtitle_alimentacion_r <- renderUI({
    helpText(HTML(unique(dat_alimentacion_r()$definicion)))
  })

  # Nota
  output$nota_alimentacion_r <- renderUI({

    if(input$indicador_alimentacion_r %in% lista_nota){
      helpText(HTML(unique(dat_alimentacion_r()$nota)))

    } else(NULL)
  })

  # Nombre conceptual
  output$conindicador_alimentacion_r <- renderUI({
    helpText(HTML(paste("<b> Nombre conceptual:</b>", unique(dat_alimentacion_r()$conindicador))))
  })

  # Calculo
  output$calculo_alimentacion_r <- renderUI({
    helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_alimentacion_r()$calculo))))
  })

  # Observaciones
  output$observacion_alimentacion_r <- renderUI({
    helpText(HTML(paste("<b> Observaciones:</b>", unique(dat_alimentacion_r()$observaciones))))
  })

    # Actualización
  output$actualizacion_alimentacion_r <- renderUI({
    helpText(HTML(paste("<b> Última actualización:</b>", unique(dat_alimentacion_r()$actualizacion))))
  })
  
  # * Gráficos   ======================================================

  output$plot_alimentacion_r <- renderPlot({

    req(input$indicador_alimentacion_r, input$alimentacion_r_corte)

    if(input$indicador_alimentacion_r %in% lista_serie_cat){

      req(input$indicador_alimentacion_r)

      # Total
      if(input$alimentacion_r_corte == "Total"){

        dat_plot <- dat_alimentacion_r() %>%
          filter(corte == "Total")

        plot <- ggplot(dat_plot, aes(x = fecha_cat, y = Valor, group = 1)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_alimentacion_r),
               caption = wrapit(unique(dat_plot$cita)))

        print(plot)
        ggsave("www/indicador alimentacion r.png", width = 40, height = 25, units = "cm")

        # Según corte
      } else if(input$alimentacion_r_corte != "Total") {

        alimentacion_r_corte_var <- rlang::sym(to_varname(input$alimentacion_r_corte))

        dat_plot <- dat_alimentacion_r() %>%
          filter(corte == input$alimentacion_r_corte) %>%
          filter(!!alimentacion_r_corte_var %in% input$checkbox_alimentacion_r)

        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            colour = alimentacion_r_corte_var, group = alimentacion_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_alimentacion_r,
                                    "según",
                                    tolower(input$alimentacion_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)

        print(plot)
        ggsave("www/indicador alimentacion r.png", width = 40, height = 25, units = "cm")

      }

    } else if(input$indicador_alimentacion_r %in% lista_especial ){

      if (input$alimentacion_r_corte_2 == "Total"){

        req(input$alimentacion_r_corte, input$indicador_alimentacion_r)

        alimentacion_r_corte_var <- rlang::sym(to_varname(input$alimentacion_r_corte))

        dat_plot <- dat_alimentacion_r() %>%
          filter(ano >= input$fecha_alimentacion_r[1] &
                   ano <= input$fecha_alimentacion_r[2]) %>%
          filter(corte == input$alimentacion_r_corte) %>%
          # filter(!!alimentacion_r_corte_var %in% input$checkbox_alimentacion_r) %>%
          filter(corte_2 == input$alimentacion_r_corte_2)

        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = alimentacion_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_alimentacion_r,
                                    "según",
                                    tolower(input$alimentacion_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")

        print(plot)
        ggsave("www/indicador alimentacion r.png", width = 40, height = 25, units = "cm")

      } else {

        req(input$alimentacion_r_corte, input$indicador_alimentacion_r)

        alimentacion_r_corte_var <- rlang::sym(to_varname(input$alimentacion_r_corte))
        alimentacion_r_corte_var_2 <- rlang::sym(to_varname(input$alimentacion_r_corte_2))

        dat_plot <- dat_alimentacion_r() %>%
          filter(ano >= input$fecha_alimentacion_r[1] &
                   ano <= input$fecha_alimentacion_r[2]) %>%
          filter(corte == input$alimentacion_r_corte) %>%
          # filter(!!alimentacion_r_corte_var %in% input$checkbox_alimentacion_r) %>%
          filter(corte_2 == input$alimentacion_r_corte_2)

        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = alimentacion_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_alimentacion_r,
                                    "según",
                                    tolower(input$alimentacion_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", alimentacion_r_corte_var_2)))

        print(plot)
        ggsave("www/indicador alimentacion r.png", width = 40, height = 25, units = "cm")

      }

    } else if(input$indicador_alimentacion_r %in% lista_vunico & input$indicador_alimentacion_r %in% lista_ind_2){

      req(input$alimentacion_r_corte, input$indicador_alimentacion_r)

      alimentacion_r_corte_var <- rlang::sym(to_varname(input$alimentacion_r_corte))
      alimentacion_r_corte_var_2 <- rlang::sym(to_varname(input$alimentacion_r_corte_2))

      dat_plot <- dat_alimentacion_r() %>%
        filter(ano >= input$fecha_alimentacion_r[1] &
                 ano <= input$fecha_alimentacion_r[2]) %>%
        filter(corte == input$alimentacion_r_corte) %>%
        filter(!!alimentacion_r_corte_var %in% input$checkbox_alimentacion_r) %>%
        filter(!!alimentacion_r_corte_var_2 %in% input$checkbox_alimentacion_r_2) %>%
        filter(corte_2 == input$alimentacion_r_corte_2)

      plot <- ggplot(dat_plot,
                     aes_string(x = "fecha_cat", y = "Valor",
                                fill = alimentacion_r_corte_var)) +
        geom_col(position = "dodge", width = .7, alpha = .8) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x=element_blank(),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_alimentacion_r,
                                  "según",
                                  tolower(input$alimentacion_r_corte),
                                  "en",
                                  unique(dat_plot$fecha_cat))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_fill_brewer(name = "", palette = "Paired") +
        facet_wrap(as.formula(paste("~", alimentacion_r_corte_var_2)))

      print(plot)
      ggsave("www/indicador alimentacion r.png", width = 40, height = 25, units = "cm")

    } else if(input$indicador_alimentacion_r %in% lista_vunico & input$alimentacion_r_corte != "Departamento") {

      req(input$alimentacion_r_corte, input$indicador_alimentacion_r)

      dat_plot <- dat_alimentacion_r() %>%
        filter(corte == input$alimentacion_r_corte) %>%
        janitor::remove_empty("cols")

      if(input$alimentacion_r_corte == "Total"){

        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor")) +
          geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
          geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_alimentacion_r,
                                    "según",
                                    tolower(input$alimentacion_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita)))

        print(plot)
        ggsave("www/indicador alimentacion r.png", width = 40, height = 25, units = "cm")


      } else {

        alimentacion_r_corte_var <- rlang::sym(to_varname(input$alimentacion_r_corte))

        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor",
                                  fill = alimentacion_r_corte_var)) +
          geom_col(position = "dodge", alpha = .8, stroke = 1, color = "black") +
          geom_text(aes_string(group = alimentacion_r_corte_var, label = "Valor"),
                    position = position_dodge2(width = .9), vjust = -.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_alimentacion_r,
                                    "según",
                                    tolower(input$alimentacion_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")

        print(plot)
        ggsave("www/indicador alimentacion r.png", width = 40, height = 25, units = "cm")

      }


    } else if(input$indicador_alimentacion_r %in% lista_ind_2) {

      req(input$alimentacion_r_corte, input$alimentacion_r_corte_2,
          input$fecha_alimentacion_r, input$checkbox_alimentacion_r)

      alimentacion_r_corte_var <- rlang::sym(to_varname(input$alimentacion_r_corte))
      alimentacion_r_corte_var_2 <- rlang::sym(to_varname(input$alimentacion_r_corte_2))

      dat_plot <- dat_alimentacion_r() %>%
        filter(ano >= input$fecha_alimentacion_r[1] &
                 ano <= input$fecha_alimentacion_r[2]) %>%
        filter(corte == input$alimentacion_r_corte) %>%
        filter(!!alimentacion_r_corte_var_2 %in% input$checkbox_alimentacion_r_2) %>%
        filter(!!alimentacion_r_corte_var %in% input$checkbox_alimentacion_r) %>%
        filter(corte_2 == input$alimentacion_r_corte_2)

      if(input$alimentacion_r_corte_2 == "Total"){

        dat_plot <- dat_plot %>%
          filter(corte_2 == "Total")

        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = alimentacion_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_alimentacion_r,
                                    "según",
                                    tolower(input$alimentacion_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)

      } else if(input$alimentacion_r_corte_2 != "Total") {

        alimentacion_r_corte_var_2 <- rlang::sym(to_varname(input$alimentacion_r_corte_2))

        dat_plot <- dat_plot %>%
          filter(corte_2 == input$alimentacion_r_corte_2)

        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = alimentacion_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          scale_x_continuous(breaks = int_breaks) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_alimentacion_r,
                                    "según",
                                    tolower(input$alimentacion_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          facet_wrap(as.formula(paste("~", alimentacion_r_corte_var_2)))

      }

      print(plot)
      ggsave("www/indicador alimentacion r.png", width = 40, height = 25, units = "cm")


      # Grafico simple normal
    } else if(input$alimentacion_r_corte == "Total") {

      req(input$indicador_alimentacion_r, input$fecha_alimentacion_r)

      dat_plot <- dat_alimentacion_r() %>%
        filter(ano >= input$fecha_alimentacion_r[1] &
                 ano <= input$fecha_alimentacion_r[2]) %>%
        filter(corte == "Total")

      if(input$indicador_alimentacion_r %in% lista_met){

        dat_plot <- dat_plot %>%
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))

        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor, alpha = metodo)) +
          geom_line(size = 1, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          # scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "none") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_alimentacion_r),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_alpha_manual(values = c("pre" = .3,
                                        "2020" = .5,
                                        "2021" = .6,
                                        "post"= .8))

        print(plot)
        ggsave("www/indicador alimentacion r.png", width = 40, height = 25, units = "cm")

      } else {

        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_alimentacion_r),
               caption = wrapit(unique(dat_plot$cita)))

        print(plot)
        ggsave("www/indicador alimentacion r.png", width = 40, height = 25, units = "cm")

      }


    } else if(input$alimentacion_r_corte == "Departamento" &
              input$indicador_alimentacion_r %notin% lista_ind_2 ) {

      req(input$indicador_alimentacion_r, input$fecha_dpto_alimentacion_r)

      dat_plot <- dat_alimentacion_r() %>%
        filter(corte == "Departamento") %>%
        filter(ano == input$fecha_dpto_alimentacion_r) %>%
        select(departamento, Valor, fuente, cita)

      dep_j <- dep %>%
        left_join(dat_plot, by = c("nombre" = "departamento"))

      plot <-  ggplot(dep_j, aes(fill = Valor)) +
        geom_sf() +
        geom_sf_text(aes(label = Valor), colour = "black",
                     size = 4, fontface = "bold")+
        viridis::scale_fill_viridis(name = "", direction = -1)+
        labs(x = "",
             y = "",
        )+
        theme_bdd(base_size = 14) +
        theme(axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.grid.major = element_line(colour = "transparent"),
        ) +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_alimentacion_r,
                                  "en",
                                  input$fecha_dpto_alimentacion_r), w = 80),
             caption = wrapit(unique(dat_plot$cita), w = 80))

      print(plot)
      ggsave("www/indicador alimentacion r.png", width = 40, height = 25, units = "cm")


    } else if(input$alimentacion_r_corte != "Total") {

      req(input$alimentacion_r_corte, input$indicador_alimentacion_r,
          input$fecha_alimentacion_r, input$checkbox_alimentacion_r)

      dat_plot <- dat_alimentacion_r() %>%
        filter(ano >= input$fecha_alimentacion_r[1] &
                 ano <= input$fecha_alimentacion_r[2]) %>%
        filter(corte == input$alimentacion_r_corte) %>%
        janitor::remove_empty("cols")

      alimentacion_r_corte_var <- rlang::sym(to_varname(input$alimentacion_r_corte))

      dat_plot <- filter(dat_plot,
                         !!alimentacion_r_corte_var %in% input$checkbox_alimentacion_r)

      if(input$indicador_alimentacion_r %in% lista_met){

        dat_plot <- dat_plot %>%
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))

        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = alimentacion_r_corte_var, alpha = "metodo")) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_alimentacion_r,
                                    "según",
                                    tolower(input$alimentacion_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          scale_alpha_manual(values = c("pre" = .3,
                                        "2020" = .5,
                                        "2021" = .6,
                                        "post"= .8)) +
          guides(alpha = "none")

        print(plot)
        ggsave("www/indicador alimentacion r.png", width = 40, height = 25, units = "cm")

      } else {
        
        alimentacion_r_corte_var <- rlang::sym(to_varname(input$alimentacion_r_corte))
        
        dat_plot <- dat_alimentacion_r() %>%
          filter(corte == input$alimentacion_r_corte) %>%
          filter(!!alimentacion_r_corte_var %in% input$checkbox_alimentacion_r)
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            colour = alimentacion_r_corte_var, group = alimentacion_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_alimentacion_r,
                                    "según",
                                    tolower(input$alimentacion_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador alimentacion r.png", width = 40, height = 25, units = "cm")
        
      }
    }
  })


  # * Descarga gráficos   =============================================

  output$baja_p_alimentacion_r <- downloadHandler(
    filename <- function() {
      paste("indicador alimentacion r", "png", sep = ".")
    },

    content <- function(file) {
      file.copy("www/indicador alimentacion r.png", file)
    },
    contentType = "www/indicador alimentacion r"
  )


  # * Tablas   ========================================================

  # Data
  alimentacion_r_tab <- reactive({

    if(input$indicador_alimentacion_r %in%  lista_especial){

      alimentacion_r_corte_var <- rlang::sym(to_varname(input$alimentacion_r_corte))
      alimentacion_r_corte_var_2 <- rlang::sym(to_varname(input$alimentacion_r_corte_2))

      dat_alimentacion_r() %>%
        filter(corte_2 == input$alimentacion_r_corte_2) %>%
        select(Fecha, alimentacion_r_corte_var, alimentacion_r_corte_var_2, Valor) %>%
        arrange(desc(Fecha)) %>%
        pivot_wider(values_from = "Valor",
                    names_from = alimentacion_r_corte_var)

    } else if(input$indicador_alimentacion_r %in% lista_vunico & input$alimentacion_r_corte == "Total"){

      req(input$alimentacion_r_corte, input$indicador_alimentacion_r)

      dat_alimentacion_r() %>%
        filter(corte == "Total") %>%
        select(fecha_cat, Valor) %>%
        arrange(desc(fecha_cat)) %>%
        rename(Fecha = fecha_cat)

    } else if(input$indicador_alimentacion_r %in% lista_vunico & input$alimentacion_r_corte != "Total") {

      req(input$alimentacion_r_corte, input$indicador_alimentacion_r)

      alimentacion_r_corte_var <- rlang::sym(to_varname(input$alimentacion_r_corte))

      dat_cut <- dat_alimentacion_r() %>%
        filter(corte == input$alimentacion_r_corte) %>%
        janitor::remove_empty("cols")

      dat_cut %>%
        select(fecha_cat, alimentacion_r_corte_var, Valor) %>%
        arrange(desc(fecha_cat)) %>%
        rename(Fecha = fecha_cat) %>%
        pivot_wider(values_from = "Valor",
                    names_from = alimentacion_r_corte_var)


    } else if(input$indicador_alimentacion_r %in% lista_ind_2) {

      req(input$alimentacion_r_corte, input$indicador_alimentacion_r, input$fecha_alimentacion_r)

      alimentacion_r_corte_var <- rlang::sym(to_varname(input$alimentacion_r_corte))

      dat_cut <- dat_alimentacion_r() %>%
        filter(corte == input$alimentacion_r_corte) %>%
        filter(!!alimentacion_r_corte_var %in% input$checkbox_alimentacion_r)

      if(input$alimentacion_r_corte_2 == "Total"){

        dat_cut %>%
          filter(ano >= input$fecha_alimentacion_r[1] &
                   ano <= input$fecha_alimentacion_r[2]) %>%
          filter(corte_2 == "Total") %>%
          select(Fecha, alimentacion_r_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = alimentacion_r_corte_var)

      } else if(input$alimentacion_r_corte_2 != "Total") {

        alimentacion_r_corte_var_2 <- rlang::sym(to_varname(input$alimentacion_r_corte_2))

        dat_cut %>%
          filter(ano >= input$fecha_alimentacion_r[1] &
                   ano <= input$fecha_alimentacion_r[2]) %>%
          filter(corte_2 == input$alimentacion_r_corte_2) %>%
          select(Fecha, alimentacion_r_corte_var, alimentacion_r_corte_var_2, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = alimentacion_r_corte_var)

      }

    } else if(input$alimentacion_r_corte == "Total") {

      req(input$alimentacion_r_corte, input$indicador_alimentacion_r, input$fecha_alimentacion_r)

      dat_alimentacion_r() %>%
        filter(corte == "Total") %>%
        filter(ano >= input$fecha_alimentacion_r[1] &
                 ano <= input$fecha_alimentacion_r[2]) %>%
        select(Fecha, Valor) %>%
        arrange(desc(Fecha))

    } else if(input$alimentacion_r_corte != "Total") {

      req(input$alimentacion_r_corte, input$indicador_alimentacion_r, input$fecha_alimentacion_r)

      alimentacion_r_corte_var <- rlang::sym(to_varname(input$alimentacion_r_corte))

      dat_cut <- dat_alimentacion_r() %>%
        filter(corte == input$alimentacion_r_corte) %>%
        janitor::remove_empty("cols")

      dat_cut %>%
        filter(ano >= input$fecha_alimentacion_r[1] &
                 ano <= input$fecha_alimentacion_r[2]) %>%
        select(Fecha, alimentacion_r_corte_var, Valor) %>%
        arrange(desc(Fecha)) %>%
        pivot_wider(values_from = "Valor",
                    names_from = alimentacion_r_corte_var)

    }
  })

  # Metadata
  alimentacion_r_meta <- reactive({

    dat_alimentacion_r() %>%
      select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, observaciones, actualizacion, cita) %>%
      mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>%
      distinct() %>%
      gather(key = " ", value = " ")

  })

  # Excel
  list_alimentacion_r <- reactive({
    list_alimentacion_r <- list("Data" = alimentacion_r_tab(),
                            "Metadata" = alimentacion_r_meta())
  })

  # Render
  output$table_alimentacion_r <- renderDT({

    DT::datatable(alimentacion_r_tab(),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    input$indicador_alimentacion_r,
                    style = "color:black; font-size:110%;")
    )

  })

  # * Descarga tablas   ================================================

  output$dwl_tab_alimentacion_r <- downloadHandler(

    filename = function() {
      paste("resultados-", input$indicador_alimentacion_r, ".xlsx", sep = "")
    },
    content = function(file) {

      openxlsx::write.xlsx(list_alimentacion_r(), file)

    }
  )


   
  ### 10. Poblaciones   =============================================
  
  # * Data reactiva   =================================================
  
  output$selector_poblaciones_indicadores <- renderUI({
    
    
    if(input$poblaciones == "Afrodescendientes"){
      
      selectInput(inputId = "indicador_poblaciones",
                  label = "Seleccione indicador:",
                  choices = list(
                    "Educación" = ind_asc_edu,
                    "Salud" = c("Distribución porcentual de personas según institución prestadora en la cual tienen derecho vigente" = ind_asc_salud), 
                    "Vivienda" = ind_asc_vivienda,
                    "Trabajo" = ind_asc_trabajo,
                    "Seguridad Social" = ind_asc_ssocial,
                    "Ambiente" = ind_asc_ambiente,
                    "Proyecto SURGE (ACNUDH)" = ind_asc_surge)) # Add datos Proyecto SURGE ACNUDH
      
    } else if (input$poblaciones == "Mujeres"){
      
      selectInput(inputId = "indicador_poblaciones",
                  label = "Seleccione indicador:",
                  choices = list(
                    "Educación" = ind_sexo_edu,
                    "Salud" = ind_sexo_salud, 
                    "Vivienda" = ind_sexo_vivienda,
                    "Trabajo" = ind_sexo_trabajo,
                    "Seguridad Social" = ind_sexo_ssocial,
                    "Ambiente" = ind_sexo_ambiente))
      
    } else if (input$poblaciones == "Migrantes"){
      
      selectInput(inputId = "indicador_poblaciones",
                  label = "Seleccione indicador:",
                  choices = list(
                    "Educación" = ind_migrantes_edu,
                    "Salud" = c("Distribución porcentual de personas según institución prestadora en la cual tienen derecho vigente" = ind_migrantes_salud), 
                    "Vivienda" = ind_migrantes_vivienda,
                    "Trabajo" = ind_migrantes_trabajo,
                    "Seguridad Social" = ind_migrantes_ssocial,
                    "Ambiente" = ind_migrantes_ambiente))
      
    } else if (input$poblaciones == "Niños, niñas y adolescentes"){
      
      selectInput(inputId = "indicador_poblaciones",
                  label = "Seleccione indicador:",
                  choices = list(
                    "Educación" = ind_nna_edu,
                    "Salud" = ind_nna_salud, 
                    "Vivienda" = ind_nna_vivienda,
                    "Trabajo" = ind_nna_trabajo,
                    "Seguridad Social" = ind_nna_ssocial,
                    "Ambiente" = c("Porcentaje de personas que residen en viviendas sin agua potable" = ind_nna_ambiente)))
      
    } else if (input$poblaciones == "Personas con discapacidad"){
      
      selectInput(inputId = "indicador_poblaciones",
                  label = "Seleccione indicador:",
                  choices = list(
                    "Educación" = ind_pd_edu,
                    "Salud" = ind_pd_salud, 
                    "Vivienda" = ind_pd_vivienda,
                    "Trabajo" = ind_pd_trabajo,
                    #"Seguridad Social" = ind_pd_ssocial,
                    "Ambiente" = ind_pd_ambiente))
      
    } else if (input$poblaciones == "Personas privadas de libertad"){
      
      selectInput(inputId = "indicador_poblaciones",
                  label = "Seleccione indicador:",
                  choices = list(
                    "Educación" = ind_ppl_edu,
                    "Salud" = ind_ppl_salud, 
                    "Vivienda" = c("Densidad penitenciaria (Promedio anual)" = ind_ppl_vivienda),
                    "Trabajo" = ind_ppl_trabajo,
                    "Seguridad Social" = ind_ppl_ssocial,
                    "Ambiente" = ind_ppl_ambiente))
      
    } else if (input$poblaciones == "Personas LGBTI"){
      
      selectInput(inputId = "indicador_poblaciones",
                  label = "Seleccione indicador:",
                  choices = list(
                    "Educación" = ind_lgtb_edu,
                    "Salud" = ind_lgtb_salud, 
                    "Vivienda" = ind_lgtb_vivienda,
                    "Trabajo" = ind_lgtb_trabajo,
                    "Seguridad Social" = c("Porcentaje de personas trans con cobertura de TUS-Trans" = ind_lgtb_ssocial),
                    "Ambiente" = ind_lgtb_ambiente))
    }      
    
  })
  
  dat_poblaciones <- reactive({
    
    req(input$indicador_poblaciones)
    
    if(input$poblaciones == "Afrodescendientes" & input$indicador_poblaciones == "Distribución porcentual de personas según institución prestadora en la cual declaran tener cobertura vigente"){
      
      dat %>%
        filter(nomindicador == input$indicador_poblaciones) %>% 
        filter(corte_2 %in% "Ascendencia étnico-racial")
      
    } else if(input$poblaciones == "Afrodescendientes"){
      
      datpob %>%
        filter(nomindicador == input$indicador_poblaciones) %>% 
        filter(corte %in% "Ascendencia étnico-racial")
      
    } else if(input$poblaciones == "Mujeres" & input$indicador_poblaciones == "Distribución porcentual de personas según institución prestadora en la cual declaran tener cobertura vigente"){
        
        dat %>%
          filter(nomindicador == input$indicador_poblaciones) %>% 
          filter(corte_2 %in% "Sexo")
      
    } else if(input$poblaciones == "Mujeres" & input$indicador_poblaciones %in% lista_mujeres_unico){
      
      dat %>%
        filter(nomindicador == input$indicador_poblaciones) %>% 
        filter(mujeres == 1)
      
      
      } else if (input$poblaciones == "Mujeres"){

      datpob %>%
        filter(nomindicador == input$indicador_poblaciones) %>%
        filter(corte == "Sexo")
      
      } else if(input$poblaciones == "Niños, niñas y adolescentes" & input$indicador_poblaciones == "Distribución porcentual de personas según institución prestadora en la cual declaran tener cobertura vigente"){
        
        dat %>%
          filter(nomindicador == input$indicador_poblaciones) %>% 
          filter(corte_2 %in% "Edad")
      
      } else if (input$poblaciones == "Niños, niñas y adolescentes"){
      
      datpob <- datpob %>%
        filter(nomindicador == input$indicador_poblaciones) 
      
      n_cortes <- length(unique(datpob$corte))
      
      if(n_cortes == 1){
        
        datpob %>%
          filter(nomindicador == input$indicador_poblaciones) 
        
      } else if (n_cortes > 1){
        
        datpob %>%
          filter(nomindicador == input$indicador_poblaciones) %>% 
          filter(corte == "Edad")
      }
      
      
    } else {
      
      datpob %>%
        filter(nomindicador == input$indicador_poblaciones) 
      
    }
    
  })
  
  output$selector_poblaciones_corte_2 <- renderUI({
    
    if(input$indicador_poblaciones %in% lista_ind_2_pob){
      
      selectInput(
        inputId = "poblaciones_corte_2",
        label = "Seleccione primer corte:",
        choices = dat_poblaciones() %>% 
          select(corte_2) %>%
          arrange(corte_2) %>% 
          unique() %>% 
          pull(),
        selected = dat_poblaciones() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte_2) %>% 
          pull()
      )
      
    } else {
      
      NULL
    }
    
  })
  
  output$chbox_poblaciones_2 <- renderUI({
    
    if(input$indicador_poblaciones %in% lista_ind_2_pob & input$indicador_poblaciones %notin% lista_especial_pob){
      
      # if(input$poblaciones_corte %notin% c("Total", "Departamento") & input$indicador_poblaciones %notin% lista_vunico_pob) {
      
      poblaciones_corte_var_2 <- rlang::sym(to_varname(input$poblaciones_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_poblaciones_2",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_poblaciones() %>%
                           filter(corte_2 == input$poblaciones_corte_2) %>% 
                           distinct(!!poblaciones_corte_var_2) %>%
                           pull(),
                         selected = dat_poblaciones() %>%
                           filter(corte_2 == input$poblaciones_corte_2) %>% 
                           filter(jerarquia_cat_2 == "1") %>%
                           distinct(!!poblaciones_corte_var_2) %>%
                           pull()
      )
      
      # } else {
      #   
      #   return(NULL)
      #   
      #   }
      
    } else {
      
      return(NULL)
      
    }      
  })
  
  
  output$selector_poblaciones_corte <- renderUI({
    
    selectInput(
      inputId = "poblaciones_corte",
      label = "Seleccione corte:",
      choices = dat_poblaciones() %>% 
        select(corte) %>%
        arrange(corte) %>% 
        unique() %>% 
        pull(),
      selected = dat_poblaciones() %>% 
        filter(jerarquia == "1") %>%  
        distinct(corte) %>% 
        pull()
    )
    
  })
  
  output$chbox_poblaciones <- renderUI({
    
    if(input$poblaciones_corte %in% lista_ind_2_pob & input$poblaciones_corte %notin% c("Total", "Departamento") & input$indicador_poblaciones %notin% lista_vunico_pob) {
      
      poblaciones_corte_var <- rlang::sym(to_varname(input$poblaciones_corte))
      poblaciones_corte_var_2 <- rlang::sym(to_varname(input$poblaciones_corte_2))
      
      checkboxGroupInput(inputId = "checkbox_poblaciones",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_poblaciones() %>%
                           filter(!!poblaciones_corte_var_2 %in% input$checkbox_poblaciones_2) %>%
                           filter(corte == input$poblaciones_corte) %>% 
                           distinct(!!poblaciones_corte_var) %>%
                           pull(),
                         selected = dat_poblaciones() %>%
                           filter(!!poblaciones_corte_var_2 %in% input$checkbox_poblaciones_2) %>%
                           filter(corte == input$poblaciones_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!poblaciones_corte_var) %>%
                           pull()
      )
      
    } else if(input$poblaciones_corte %notin% lista_ind_2_pob & input$poblaciones_corte %notin% c("Total", "Departamento") & input$indicador_poblaciones %notin% lista_vunico_pob) {
      
      poblaciones_corte_var <- rlang::sym(to_varname(input$poblaciones_corte))
      
      checkboxGroupInput(inputId = "checkbox_poblaciones",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_poblaciones() %>%
                           filter(corte == input$poblaciones_corte) %>% 
                           distinct(!!poblaciones_corte_var) %>%
                           pull(),
                         selected = dat_poblaciones() %>%
                           filter(corte == input$poblaciones_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!poblaciones_corte_var) %>%
                           pull()
      )
      
    } else {
      
      return(NULL)
    }
    
  })
  
  # Selector de fecha
  output$s_poblaciones_fecha <- renderUI({
    
    if(input$poblaciones_corte == "Departamento" & input$indicador_poblaciones %notin% lista_ind_2_pob) {
      
      req(input$poblaciones_corte, input$indicador_poblaciones)
      
      req(nrow(dat_poblaciones()) > 0)
      
      selectInput(
        inputId = "fecha_dpto_poblaciones",
        label = "Seleccione año:",
        choices = dat_poblaciones() %>% 
          filter(nomindicador == input$indicador_poblaciones) %>%
          drop_na(Valor) %>%
          select(ano) %>%
          arrange(desc(ano)) %>% 
          unique() %>% 
          pull(),
        selected = max(input$ano)
      )
      
    } else if (input$indicador_poblaciones %in% lista_serie_cat_pob){
      
      return(NULL)
      
      
    } else if (input$indicador_poblaciones %in% lista_vunico_pob){
      
      return(NULL)
      
    } else  {
      
      req(input$poblaciones_corte, input$indicador_poblaciones)
      req(nrow(dat_poblaciones()) > 0)
      
      tagList(
        # tags$style(type = 'text/css', 
        #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
        # div(id = 'big_slider',
        
        sliderInput("fecha_poblaciones", 
                    label = "Rango de tiempo", 
                    sep = "",
                    dragRange = T,
                    min = min(dat_poblaciones()$ano), 
                    max = max(dat_poblaciones()$ano), 
                    value = c(min(dat_poblaciones()$ano), 
                              max(dat_poblaciones()$ano))
        )
      )
      
    }
  })
  
  # # Selector de corte según categoría y data temporal
  # dat_poblaciones <- reactive({
  #   
  #   req(input$poblaciones_corte)
  #   
  #   if(input$indicador_poblaciones %in% lista_ind_2_pob){
  #     
  #     dat_salarios <- dat_poblaciones() %>%
  #       filter(corte_2 == input$poblaciones_corte_2) %>% 
  #       filter(corte == input$poblaciones_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   } else {
  #     
  #     dat_poblaciones() %>%
  #       filter(corte == input$poblaciones_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   }
  # })
  
  # * Metadata   ======================================================
  
  # Title
  output$title_poblaciones <- renderUI({ 
    helpText(HTML(unique(dat_poblaciones()$nomindicador)))
  })
  
  # Subtitle
  output$subtitle_poblaciones <- renderUI({ 
    helpText(HTML(unique(dat_poblaciones()$definicion)))
  })
  
  # Nota
  output$nota_poblaciones <- renderUI({ 
    
    if(input$indicador_poblaciones %in% lista_nota){
      helpText(HTML(unique(dat_poblaciones()$nota)))
      
    } else(NULL)
  })  
  
  # Nombre conceptual
  output$conindicador_poblaciones <- renderUI({ 
    helpText(HTML(paste("<b> Nombre conceptual:</b>", unique(dat_poblaciones()$conindicador))))
  })
  
  # Calculo
  output$calculo_poblaciones <- renderUI({ 
    helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_poblaciones()$calculo))))
  })
  
  # Observaciones
  output$observacion_poblaciones <- renderUI({ 
    helpText(HTML(paste("<b> Observaciones:</b>", unique(dat_poblaciones()$observaciones))))
  })
  
  # Actualización
  output$actualizacion_poblaciones <- renderUI({
    helpText(HTML(paste("<b> Última actualización:</b>", unique(dat_poblaciones()$actualizacion))))
  })
  
  # * Gráficos   ======================================================
  
  output$plot_poblaciones <- renderPlot({
    
    req(input$indicador_poblaciones)
    
    if(input$indicador_poblaciones %in% lista_serie_cat_pob){
      
      req(input$poblaciones_corte, input$indicador_poblaciones)
      
      # Total
      if(input$poblaciones_corte == "Total"){
        
        dat_plot <- dat_poblaciones() %>%
          filter(corte == "Total")
        
        plot <- ggplot(dat_plot, aes(x = fecha_cat, y = Valor, group = 1)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_poblaciones),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador pob.png", width = 40, height = 25, units = "cm")
        
        # Según corte
      } else if(input$poblaciones_corte != "Total") {
        
        poblaciones_corte_var <- rlang::sym(to_varname(input$poblaciones_corte))
        
        dat_plot <- dat_poblaciones() %>%
          filter(corte == input$poblaciones_corte) %>%
          filter(!!poblaciones_corte_var %in% input$checkbox_poblaciones)
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            colour = poblaciones_corte_var, group = poblaciones_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_poblaciones,
                                    "según",
                                    tolower(input$poblaciones_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador pob.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_poblaciones %in% lista_especial_pob ){
      
      if (input$poblaciones_corte_2 == "Total"){        
        
        req(input$poblaciones_corte, input$indicador_poblaciones)
        
        poblaciones_corte_var <- rlang::sym(to_varname(input$poblaciones_corte))
        
        dat_plot <- dat_poblaciones() %>%
          filter(ano >= input$fecha_poblaciones[1] &
                   ano <= input$fecha_poblaciones[2]) %>%
          filter(corte == input$poblaciones_corte) %>%
          # filter(!!poblaciones_corte_var %in% input$checkbox_poblaciones) %>% 
          filter(corte_2 == input$poblaciones_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = poblaciones_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_poblaciones,
                                    "según",
                                    tolower(input$poblaciones_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador pob.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        req(input$poblaciones_corte, input$indicador_poblaciones)
        
        poblaciones_corte_var <- rlang::sym(to_varname(input$poblaciones_corte))
        poblaciones_corte_var_2 <- rlang::sym(to_varname(input$poblaciones_corte_2))
        
        dat_plot <- dat_poblaciones() %>%
          filter(ano >= input$fecha_poblaciones[1] &
                   ano <= input$fecha_poblaciones[2]) %>%
          filter(corte == input$poblaciones_corte) %>%
          # filter(!!poblaciones_corte_var %in% input$checkbox_poblaciones) %>% 
          filter(corte_2 == input$poblaciones_corte_2)  
        
        plot <- ggplot(dat_plot, aes_string(x = "fecha_cat", y = "Valor",
                                            fill = poblaciones_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_poblaciones,
                                    "según",
                                    tolower(input$poblaciones_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", poblaciones_corte_var_2)))
        
        print(plot)
        ggsave("www/indicador pob.png", width = 40, height = 25, units = "cm")
        
      }
      
    } else if(input$indicador_poblaciones %in% lista_vunico_pob & input$indicador_poblaciones %in% lista_ind_2_pob){
      
      req(input$poblaciones_corte, input$indicador_poblaciones)
      
      poblaciones_corte_var <- rlang::sym(to_varname(input$poblaciones_corte))
      poblaciones_corte_var_2 <- rlang::sym(to_varname(input$poblaciones_corte_2))
      
      dat_plot <- dat_poblaciones() %>%
        filter(ano >= input$fecha_poblaciones[1] &
                 ano <= input$fecha_poblaciones[2]) %>%
        filter(corte == input$poblaciones_corte) %>%
        filter(!!poblaciones_corte_var %in% input$checkbox_poblaciones) %>%
        filter(!!poblaciones_corte_var_2 %in% input$checkbox_poblaciones_2) %>%
        filter(corte_2 == input$poblaciones_corte_2)
      
      plot <- ggplot(dat_plot,
                     aes_string(x = "fecha_cat", y = "Valor",
                                fill = poblaciones_corte_var)) +
        geom_col(position = "dodge", width = .7, alpha = .8) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x=element_blank(),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_poblaciones,
                                  "según",
                                  tolower(input$poblaciones_corte),
                                  "en",
                                  unique(dat_plot$fecha_cat))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_fill_brewer(name = "", palette = "Paired") +
        facet_wrap(as.formula(paste("~", poblaciones_corte_var_2)))
      
      print(plot)
      ggsave("www/indicador pob.png", width = 40, height = 25, units = "cm")
      
    } else if(input$indicador_poblaciones %in% lista_vunico_pob & input$poblaciones_corte != "Departamento") {
      
      req(input$poblaciones_corte, input$indicador_poblaciones)
      
      dat_plot <- dat_poblaciones() %>%
        filter(corte == input$poblaciones_corte) %>%
        janitor::remove_empty("cols")
      
      if(input$poblaciones_corte == "Total"){
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor")) +
          geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
          geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_poblaciones,
                                    "según",
                                    tolower(input$poblaciones_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador pob.png", width = 40, height = 25, units = "cm")
        
        
      } else {
        
        poblaciones_corte_var <- rlang::sym(to_varname(input$poblaciones_corte))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha_cat", y = "Valor",
                                  fill = poblaciones_corte_var)) +
          geom_col(position = "dodge", alpha = .8, stroke = 1, color = "black") +
          geom_text(aes_string(group = poblaciones_corte_var, label = "Valor"),
                    position = position_dodge2(width = .9), vjust = -.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_poblaciones,
                                    "según",
                                    tolower(input$poblaciones_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired")
        
        print(plot)
        ggsave("www/indicador pob.png", width = 40, height = 25, units = "cm")
        
      }
      
      
    } else if(input$indicador_poblaciones %in% lista_ind_2_pob) {
      
      req(input$poblaciones_corte, input$poblaciones_corte_2,
          input$fecha_poblaciones, input$checkbox_poblaciones)
      
      poblaciones_corte_var <- rlang::sym(to_varname(input$poblaciones_corte))
      poblaciones_corte_var_2 <- rlang::sym(to_varname(input$poblaciones_corte_2))
      
      dat_plot <- dat_poblaciones() %>%
        filter(ano >= input$fecha_poblaciones[1] &
                 ano <= input$fecha_poblaciones[2]) %>%
        filter(corte == input$poblaciones_corte) %>%
        filter(!!poblaciones_corte_var_2 %in% input$checkbox_poblaciones_2) %>%
        filter(!!poblaciones_corte_var %in% input$checkbox_poblaciones) %>% 
        filter(corte_2 == input$poblaciones_corte_2)
      
      if(input$poblaciones_corte_2 == "Total"){
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == "Total")
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = poblaciones_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_poblaciones,
                                    "según",
                                    tolower(input$poblaciones_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
      } else if(input$poblaciones_corte_2 != "Total") {
        
        poblaciones_corte_var_2 <- rlang::sym(to_varname(input$poblaciones_corte_2))
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == input$poblaciones_corte_2)
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = poblaciones_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          scale_x_continuous(breaks = int_breaks) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_poblaciones,
                                    "según",
                                    tolower(input$poblaciones_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          facet_wrap(as.formula(paste("~", poblaciones_corte_var_2)))
        
      }
      
      print(plot)
      ggsave("www/indicador pob.png", width = 40, height = 25, units = "cm")
      
    } else if(input$poblaciones_corte == "Total") {
      
      req(input$indicador_poblaciones, input$fecha_poblaciones)
      
      dat_plot <- dat_poblaciones() %>%
        filter(ano >= input$fecha_poblaciones[1] &
                 ano <= input$fecha_poblaciones[2]) %>%
        filter(corte == "Total")
      
      if(input$indicador_poblaciones %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor, alpha = metodo)) +
          geom_line(size = 1, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          # scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "none") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_poblaciones),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8))
        
        print(plot)
        ggsave("www/indicador pob.png", width = 40, height = 25, units = "cm")
        
      } else {
        
        plot <- ggplot(dat_plot,
                       aes(x = fecha, y = Valor)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          scale_x_continuous(breaks = int_breaks) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_poblaciones),
               caption = wrapit(unique(dat_plot$cita)))
        
        print(plot)
        ggsave("www/indicador pob.png", width = 40, height = 25, units = "cm")
        
      }   

      
    } else if(input$poblaciones_corte == "Departamento" &
              input$indicador_poblaciones %notin% lista_ind_2_pob ) {
      
      req(input$indicador_poblaciones, input$fecha_dpto_poblaciones)
      
      dat_plot <- dat_poblaciones() %>%
        filter(corte == "Departamento") %>%
        filter(ano == input$fecha_dpto_poblaciones) %>%
        select(departamento, Valor, fuente, cita)
      
      dep_j <- dep %>%
        left_join(dat_plot, by = c("nombre" = "departamento"))
      
      plot <-  ggplot(dep_j, aes(fill = Valor)) +
        geom_sf() +
        geom_sf_text(aes(label = Valor), colour = "black",
                     size = 4, fontface = "bold")+
        viridis::scale_fill_viridis(name = "", direction = -1)+
        labs(x = "",
             y = "",
        )+
        theme_bdd(base_size = 14) +
        theme(axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.grid.major = element_line(colour = "transparent"),
        ) +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_poblaciones,
                                  "en",
                                  input$fecha_dpto_poblaciones), w = 80),
             caption = wrapit(unique(dat_plot$cita), w = 80))
      
      print(plot)
      ggsave("www/indicador pob.png", width = 40, height = 25, units = "cm")
      
      
    } else if(input$poblaciones_corte != "Total") {
      
      req(input$poblaciones_corte, input$indicador_poblaciones,
          input$fecha_poblaciones, input$checkbox_poblaciones)
      
      dat_plot <- dat_poblaciones() %>%
        filter(ano >= input$fecha_poblaciones[1] &
                 ano <= input$fecha_poblaciones[2]) %>%
        filter(corte == input$poblaciones_corte) %>%
        janitor::remove_empty("cols")
      
      poblaciones_corte_var <- rlang::sym(to_varname(input$poblaciones_corte))
      
      dat_plot <- filter(dat_plot,
                         !!poblaciones_corte_var %in% input$checkbox_poblaciones)
      
      if(input$indicador_poblaciones %in% lista_met){
        
        dat_plot <- dat_plot %>% 
          mutate(metodo = case_when(
            fecha <= 2019 ~ "pre",
            fecha == 2020 ~ "2020",
            fecha == 2021 ~ "2021",
            fecha >= 2022 ~ "post",
          ))
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = poblaciones_corte_var, alpha = "metodo")) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_poblaciones,
                                    "según",
                                    tolower(input$poblaciones_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) +
          scale_alpha_manual(values = c("pre" = .3, 
                                        "2020" = .5, 
                                        "2021" = .6, 
                                        "post"= .8)) +
          guides(alpha = "none")
        
        print(plot)
        ggsave("www/indicador pob.png", width = 40, height = 25, units = "cm")
        
        
      } else {
        
        plot <- ggplot(dat_plot,
                       aes_string(x = "fecha", y = "Valor", colour = poblaciones_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          scale_x_continuous(breaks = int_breaks) +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_poblaciones,
                                    "según",
                                    tolower(input$poblaciones_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida)
        
        print(plot)
        ggsave("www/indicador pob.png", width = 40, height = 25, units = "cm")

      }
        
    }
    
  })
  
  
  # * Descarga gráficos   =============================================
  
  output$baja_p_poblaciones <- downloadHandler(
    filename <- function() {
      paste("indicador pob", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/indicador pob.png", file)
    },
    contentType = "www/indicador pob"
  )
  
  
  # * Tablas   ========================================================
  
  # Data
  poblaciones_tab <- reactive({
    
    if(input$indicador_poblaciones %in%  lista_especial_pob){
      
      poblaciones_corte_var <- rlang::sym(to_varname(input$poblaciones_corte))
      poblaciones_corte_var_2 <- rlang::sym(to_varname(input$poblaciones_corte_2))
      
      dat_poblaciones() %>%
        filter(corte_2 == input$poblaciones_corte_2) %>% 
        select(Fecha, poblaciones_corte_var, poblaciones_corte_var_2, Valor) %>%
        arrange(desc(Fecha)) %>%
        pivot_wider(values_from = "Valor",
                    names_from = poblaciones_corte_var) 
      
    } else if(input$indicador_poblaciones %in% lista_vunico_pob & input$poblaciones_corte == "Total"){
      
      req(input$poblaciones_corte, input$indicador_poblaciones)
      
      dat_poblaciones() %>%
        filter(corte == "Total") %>% 
        select(fecha_cat, Valor) %>%
        arrange(desc(fecha_cat)) %>%
        rename(Fecha = fecha_cat)
      
    } else if(input$indicador_poblaciones %in% lista_vunico_pob & input$poblaciones_corte != "Total") {
      
      req(input$poblaciones_corte, input$indicador_poblaciones)
      
      poblaciones_corte_var <- rlang::sym(to_varname(input$poblaciones_corte))
      
      dat_cut <- dat_poblaciones() %>%
        filter(corte == input$poblaciones_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        select(fecha_cat, poblaciones_corte_var, Valor) %>%
        arrange(desc(fecha_cat)) %>% 
        rename(Fecha = fecha_cat) %>%
        pivot_wider(values_from = "Valor",
                    names_from = poblaciones_corte_var)
      
      
    } else if(input$indicador_poblaciones %in% lista_ind_2_pob) {
      
      req(input$poblaciones_corte, input$indicador_poblaciones, input$fecha_poblaciones)
      
      poblaciones_corte_var <- rlang::sym(to_varname(input$poblaciones_corte))
      
      dat_cut <- dat_poblaciones() %>%
        filter(corte == input$poblaciones_corte) %>%
        filter(!!poblaciones_corte_var %in% input$checkbox_poblaciones)
      
      if(input$poblaciones_corte_2 == "Total"){
        
        dat_cut %>%
          filter(ano >= input$fecha_poblaciones[1] &
                   ano <= input$fecha_poblaciones[2]) %>%
          filter(corte_2 == "Total") %>% 
          select(Fecha, poblaciones_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = poblaciones_corte_var)
        
      } else if(input$poblaciones_corte_2 != "Total") {
        
        poblaciones_corte_var_2 <- rlang::sym(to_varname(input$poblaciones_corte_2))
        
        dat_cut %>%
          filter(ano >= input$fecha_poblaciones[1] &
                   ano <= input$fecha_poblaciones[2]) %>%
          filter(corte_2 == input$poblaciones_corte_2) %>% 
          select(Fecha, poblaciones_corte_var, poblaciones_corte_var_2, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = poblaciones_corte_var) 
        
      }
      
    } else if(input$poblaciones_corte == "Total") {
      
      req(input$poblaciones_corte, input$indicador_poblaciones, input$fecha_poblaciones)
      
      dat_poblaciones() %>%
        filter(corte == "Total") %>% 
        filter(ano >= input$fecha_poblaciones[1] &
                 ano <= input$fecha_poblaciones[2]) %>%
        select(Fecha, Valor) %>%
        arrange(desc(Fecha))
      
    } else if(input$poblaciones_corte != "Total") {
      
      req(input$poblaciones_corte, input$indicador_poblaciones, input$fecha_poblaciones)
      
      poblaciones_corte_var <- rlang::sym(to_varname(input$poblaciones_corte))
      
      dat_cut <- dat_poblaciones() %>%
        filter(corte == input$poblaciones_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        filter(ano >= input$fecha_poblaciones[1] &
                 ano <= input$fecha_poblaciones[2]) %>% 
        select(Fecha, poblaciones_corte_var, Valor) %>%
        arrange(desc(Fecha)) %>% 
        pivot_wider(values_from = "Valor",
                    names_from = poblaciones_corte_var)
      
    }
  })
  
  # Metadata 
  poblaciones_meta <- reactive({
    
    dat_poblaciones() %>%
      select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, observaciones, actualizacion, cita) %>% 
      mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
      distinct() %>% 
      gather(key = " ", value = " ")
    
  })
  
  # Excel
  list_poblaciones <- reactive({
    list_poblaciones <- list("Data" = poblaciones_tab(),
                             "Metadata" = poblaciones_meta())
  })
  
  # Render
  output$table_poblaciones <- renderDT({
    
    DT::datatable(poblaciones_tab(),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    input$indicador_poblaciones,
                    style = "color:black; font-size:110%;")
    ) 
    
  })
  
  # * Descarga tablas   ================================================
  
  output$dwl_tab_poblaciones <- downloadHandler(
    
    filename = function() {
      paste("poblaciones-", input$indicador_poblaciones, ".xlsx", sep = "")
    },
    content = function(file) {
      
      openxlsx::write.xlsx(list_poblaciones(), file)
      
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
