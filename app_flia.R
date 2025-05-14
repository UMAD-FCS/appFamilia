
# ***************************************************************************
# APP FAMILIA 
# ***************************************************************************
library(rio)
library(shinyWidgets)
library(dplyr)
library(janitor)
library(extrafont)
library(plotly)
library(DT)
library(shiny)
library(ggtext)
library(stringr)
library(ggrepel)
library(shinycssloaders)
library(bslib)
library(RColorBrewer) 
library(ggplot2) 
library(viridis)
# ***************************************************************************


source('utils.R') 

theme_set(theme_bdd())
update_geom_defaults("text", list(family = theme_get()$text$family))

#dir.create('~/.fonts')
file.copy("www/Titillium Web.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')

## Colores de gráficos
color_defecto <- "#fc8d62"
paleta <- "Set2"
#paleta2 <- "Set3"

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



df <- rio::import("Base_Motor_familia_shinny.xlsx")
metadata <- rio::import("Metadata_shinny.xlsx")

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
                    
                    ),
                    
                
   ## Composición y estructura de los hogares
                  
                  
                  # Total país

                    tabPanel(
                      title = "Composición y estructura de los hogares",
                      value = 'borelito',
                      icon = icon("people-group", lib = "font-awesome"),
                   
                      tabsetPanel(
                        type = "pills",
                        id   = "tabsetborel",
                        
                        
                        tabPanel(
                          "Total País",
                          br(),
                          
                          fluidRow(
                            
                            #div( id ="Sidebar",
                      
                            
                     # Menú lateral
                            
                            sidebarPanel(width = 3,
                                         style = "background-color: #F8F8FF",
                                         selectInput(
                                           "indicador_CE_tp",
                                           "Seleccione el indicador:",
                                           choices = unique(df %>%
                                                              filter(CATEGORIA == "Composición y estructura de los hogares" & PESTAÑA =="Total país") %>%
                                                              pull(NOMINDICADOR))),

                                         uiOutput("selectcorte1"),
                                         uiOutput("anio1"),
                                         uiOutput("selec_dpto"),
                                         tags$a(href="https://umad.cienciassociales.edu.uy/", 
                                                "Unidad de Métodos y Acceso a Datos",
                                                style = "font-size:12px; color:Navy;
                             text-decoration:underline;"),
                                         br(),
                                         br(),
                                         img(src = "logo_umad.png", height="70%",
                                             width = "70%", align = "left"),
                                         br(),
                                         br()
                                         
                        ),
                        
                     mainPanel(
                       tags$style(type = "text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"
                       ),
                       tags$h3(style="display:inline-block",
                               uiOutput("title_indicador_CE_tp")),
                       div(style="display:inline-block",
                           dropdown(
                             style = "minimal",
                             status = "primary",
                             width = "500px",
                             right = TRUE,
                             icon = icon("calculator", lib = "font-awesome"),
                             uiOutput("calculo_CE_tp"))
                       ),
                       div(style="display:inline-block",
                           dropdown(
                             style = "minimal",
                             status = "primary",
                             width = "500px",
                             right = TRUE,
                             icon = icon("info", lib = "font-awesome"),
                             uiOutput("info_CE_tp"))
                       ),
                       br(),
                       br(),
                       tags$h6(uiOutput("subtitle_indicador_CE_tp")),
                       
                     br(),
                     plotOutput("plot_CE_tp", height = "500px")%>% withSpinner(color="#5b6f8a"),
                     br(),
                     fluidRow(column(12, div(downloadButton("graficos_resultado_CE_tp_descarga", 
                                                            "Descarga el gráfico",
                                                            style = "background-color: #3E6C9A; color: white; border: none;"),
                                                            style = "float: right"))),
                     br(),
                     br(),
                     DTOutput("tabla_resultado_CE_tp"),
                     br(),
                     br(),
                     br(),
                     fluidRow(column(12, div(downloadButton("tabla_resultado_CE_tp_descarga", 
                                                            "Descarga la tabla",
                                                            style = "background-color: #3E6C9A; color: white; border: none;"),
                                                            style = "float: right"))),
                     br(),
                     br()
                                              )
                      
                                          )),
                     
                     
                     
                     tabPanel(
                       "País urbano",
                       br(),
                       
                       fluidRow(
                         
                         #div( id ="Sidebar",
                         
                         
                         # Menú lateral
                         
                         sidebarPanel(width = 3,
                                      style = "background-color: #F8F8FF",
                                      selectInput(
                                        "indicador_CE_pu",
                                        "Seleccione el indicador:",
                                        choices = unique(df %>%
                                                           filter(CATEGORIA == "Composición y estructura de los hogares" & PESTAÑA =="País urbano") %>%
                                                           pull(NOMINDICADOR))),

                                      uiOutput("selectcorte2"),
                                      uiOutput("anio2"),
                                      tags$a(href="https://umad.cienciassociales.edu.uy/",
                                             "Unidad de Métodos y Acceso a Datos",
                                             style = "font-size:12px; color:Navy;
                             text-decoration:underline;"),
                                      br(),
                                      br(),
                                      img(src = "logo_umad.png", height="70%",
                                          width = "70%", align = "left"),
                                      br(),
                                      br()

                         ),

                         mainPanel(
                           tags$style(type = "text/css",
                                      ".shiny-output-error { visibility: hidden; }",
                                      ".shiny-output-error:before { visibility: hidden; }"
                           ),
                           tags$h3(style="display:inline-block",
                                   uiOutput("title_indicador_CE_pu")),
                           div(style="display:inline-block",
                               dropdown(
                                 style = "minimal",
                                 status = "primary",
                                 width = "500px",
                                 right = TRUE,
                                 icon = icon("calculator", lib = "font-awesome"),
                                 uiOutput("calculo_CE_pu"))
                           ),
                           div(style="display:inline-block",
                               dropdown(
                                 style = "minimal",
                                 status = "primary",
                                 width = "500px",
                                 right = TRUE,
                                 icon = icon("info", lib = "font-awesome"),
                                 uiOutput("info_CE_pu"))
                           ),
                           br(),
                           br(),
                           tags$h6(uiOutput("subtitle_indicador_CE_pu")),

                           br(),
                           plotOutput("plot_CE_pu", height = "500px")%>% withSpinner(color="#5b6f8a"),
                           br(),
                           fluidRow(column(12, div(downloadButton("graficos_resultado_CE_pu_descarga",
                                                                  "Descarga el gráfico",
                                                                  style = "background-color: #3E6C9A; color: white; border: none;"),
                                                   style = "float: right"))),
                           br(),
                           br(),
                           DTOutput("tabla_resultado_CE_pu"),
                           br(),
                           br(),
                           br(),
                           fluidRow(column(12, div(downloadButton("tabla_resultado_CE_pu_descarga",
                                                                  "Descarga la tabla",
                                                                  style = "background-color: #3E6C9A; color: white; border: none;"),
                                                   style = "float: right"))),
                           br(),
                           br()
                         )

                       )))),
                     
   ## Cuidados y educación
   
   
   # Total país
   
   tabPanel(
     title = "Cuidados y educación",
     value = 'borelito',
     icon = icon("child", lib = "font-awesome"),
     
     tabsetPanel(
       type = "pills",
       id   = "tabsetborel",
       
       
       tabPanel(
         "Total País",
         br(),
         
         fluidRow(
           
           #div( id ="Sidebar",
           
           
           # Menú lateral
           
           sidebarPanel(width = 3,
                        style = "background-color: #F8F8FF",
                        selectInput(
                          "indicador_CUID_tp",
                          "Seleccione el indicador:",
                          choices = unique(df %>%
                                             filter(CATEGORIA == "Cuidados y educación" & PESTAÑA =="Total país") %>%
                                             pull(NOMINDICADOR))),
                        
                        uiOutput("selectcorte4"),
                        tags$a(href="https://umad.cienciassociales.edu.uy/", 
                               "Unidad de Métodos y Acceso a Datos",
                               style = "font-size:12px; color:Navy;
                             text-decoration:underline;"),
                        br(),
                        br(),
                        img(src = "logo_umad.png", height="70%",
                            width = "70%", align = "left"),
                        br(),
                        br()
                        
           ),
           
           mainPanel(
             tags$style(type = "text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             ),
             tags$h3(style="display:inline-block",
                     uiOutput("title_indicador_CUID_tp")),
             div(style="display:inline-block",
                 dropdown(
                   style = "minimal",
                   status = "primary",
                   width = "500px",
                   right = TRUE,
                   icon = icon("calculator", lib = "font-awesome"),
                   uiOutput("calculo_CUID_tp"))
             ),
             div(style="display:inline-block",
                 dropdown(
                   style = "minimal",
                   status = "primary",
                   width = "500px",
                   right = TRUE,
                   icon = icon("info", lib = "font-awesome"),
                   uiOutput("info_CUID_tp"))
             ),
             br(),
             br(),
             tags$h6(uiOutput("subtitle_indicador_CUID_tp")),
             
             br(),
             plotOutput("plot_CUID_tp", height = "500px")%>% withSpinner(color="#5b6f8a"),
             br(),
             fluidRow(column(12, div(downloadButton("graficos_resultado_CUID_tp_descarga", 
                                                    "Descarga el gráfico",
                                                    style = "background-color: #3E6C9A; color: white; border: none;"),
                                     style = "float: right"))),
             br(),
             br(),
             DTOutput("tabla_resultado_CUID_tp"),
             br(),
             br(),
             br(),
             fluidRow(column(12, div(downloadButton("tabla_resultado_CUID_tp_descarga", 
                                                    "Descarga la tabla",
                                                    style = "background-color: #3E6C9A; color: white; border: none;"),
                                     style = "float: right"))),
             br(),
             br()
           )
           
         )),
       
       
       
       tabPanel(
         "País urbano",
         br(),
         
         fluidRow(
           
           #div( id ="Sidebar",
           
           
           # Menú lateral
           
           sidebarPanel(width = 3,
                        style = "background-color: #F8F8FF",
                        selectInput(
                          "indicador_CUID_pu",
                          "Seleccione el indicador:",
                          choices = unique(df %>%
                                             filter(CATEGORIA == "Cuidados y educación" & PESTAÑA =="País urbano") %>%
                                             pull(NOMINDICADOR))),
                        
                        uiOutput("selectcorte5"),
                        tags$a(href="https://umad.cienciassociales.edu.uy/",
                               "Unidad de Métodos y Acceso a Datos",
                               style = "font-size:12px; color:Navy;
                             text-decoration:underline;"),
                        br(),
                        br(),
                        img(src = "logo_umad.png", height="70%",
                            width = "70%", align = "left"),
                        br(),
                        br()
                        
           ),
           
           mainPanel(
             tags$style(type = "text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             ),
             tags$h3(style="display:inline-block",
                     uiOutput("title_indicador_CUID_pu")),
             div(style="display:inline-block",
                 dropdown(
                   style = "minimal",
                   status = "primary",
                   width = "500px",
                   right = TRUE,
                   icon = icon("calculator", lib = "font-awesome"),
                   uiOutput("calculo_CUID_pu"))
             ),
             div(style="display:inline-block",
                 dropdown(
                   style = "minimal",
                   status = "primary",
                   width = "500px",
                   right = TRUE,
                   icon = icon("info", lib = "font-awesome"),
                   uiOutput("info_CUID_pu"))
             ),
             br(),
             br(),
             tags$h6(uiOutput("subtitle_indicador_CUID_pu")),
             
             br(),
             plotOutput("plot_CUID_pu", height = "500px")%>% withSpinner(color="#5b6f8a"),
             br(),
             fluidRow(column(12, div(downloadButton("graficos_resultado_CUID_pu_descarga",
                                                    "Descarga el gráfico",
                                                    style = "background-color: #3E6C9A; color: white; border: none;"),
                                     style = "float: right"))),
             br(),
             br(),
             DTOutput("tabla_resultado_CUID_pu"),
             br(),
             br(),
             br(),
             fluidRow(column(12, div(downloadButton("tabla_resultado_CUID_pu_descarga",
                                                    "Descarga la tabla",
                                                    style = "background-color: #3E6C9A; color: white; border: none;"),
                                     style = "float: right"))),
             br(),
             br()
           )
           
         )))),
   
   
   
   
   ## Violencia de género
   
   
   # País urbano
   
   tabPanel(
     title = "Violencia de género",
     value = 'borelito',
     icon = icon("exclamation-triangle", lib = "font-awesome"),
     
     tabsetPanel(
       type = "pills",
       id   = "tabsetborel",
       
       
       tabPanel(
         "País urbano",
         br(),
         
         fluidRow(
           
           #div( id ="Sidebar",
           
           
           # Menú lateral
           
           sidebarPanel(width = 3,
                        style = "background-color: #F8F8FF",
                        selectInput(
                          "indicador_VG",
                          "Seleccione el indicador:",
                          choices = unique(df %>%
                                             filter(CATEGORIA == "Violencia de género" & PESTAÑA =="País urbano") %>%
                                             pull(NOMINDICADOR))),
                        uiOutput("selectcorte6"),
                        uiOutput("anio3"),
                        tags$a(href="https://umad.cienciassociales.edu.uy/", 
                               "Unidad de Métodos y Acceso a Datos",
                               style = "font-size:12px; color:Navy;
                             text-decoration:underline;"),
                        br(),
                        br(),
                        img(src = "logo_umad.png", height="70%",
                            width = "70%", align = "left"),
                        br(),
                        br()
                        
           ),
           
           mainPanel(
             tags$style(type = "text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             ),
             tags$h3(style="display:inline-block",
                     uiOutput("title_indicador_VG")),
             div(style="display:inline-block",
                 dropdown(
                   style = "minimal",
                   status = "primary",
                   width = "500px",
                   right = TRUE,
                   icon = icon("calculator", lib = "font-awesome"),
                   uiOutput("calculo_VG"))
             ),
             div(style="display:inline-block",
                 dropdown(
                   style = "minimal",
                   status = "primary",
                   width = "500px",
                   right = TRUE,
                   icon = icon("info", lib = "font-awesome"),
                   uiOutput("info_VG"))
             ),
             br(),
             br(),
             tags$h6(uiOutput("subtitle_indicador_VG")),
             
             br(),
             plotOutput("plot_VG", height = "500px")%>% withSpinner(color="#5b6f8a"),
             br(),
             fluidRow(column(12, div(downloadButton("graficos_resultado_VG_descarga", 
                                                    "Descarga el gráfico",
                                                    style = "background-color: #3E6C9A; color: white; border: none;"),
                                     style = "float: right"))),
             br(),
             br(),
             DTOutput("tabla_resultado_VG"),
             br(),
             br(),
             br(),
             fluidRow(column(12, div(downloadButton("tabla_resultado_VG_descarga", 
                                                    "Descarga la tabla",
                                                    style = "background-color: #3E6C9A; color: white; border: none;"),
                                     style = "float: right"))),
             br(),
             br()
           )
           
         )))),
   
   
   ## Salud infantil
   
   
   # Total país
   
   tabPanel(
     title = "Salud infantil",
     value = 'borelito',
     icon = icon("stethoscope", lib = "font-awesome"),
     
     tabsetPanel(
       type = "pills",
       id   = "tabsetborel",
       
       
       tabPanel(
         "Total País",
         br(),
         
         fluidRow(
           
           #div( id ="Sidebar",
           
           
           # Menú lateral
           
           sidebarPanel(width = 3,
                        style = "background-color: #F8F8FF",
                        selectInput(
                          "indicador_SI_tp",
                          "Seleccione el indicador:",
                          choices = unique(df %>%
                                             filter(CATEGORIA == "Salud infantil" & PESTAÑA =="Total país") %>%
                                             pull(NOMINDICADOR))),
                        
                        uiOutput("selectcorte7"),
                        tags$a(href="https://umad.cienciassociales.edu.uy/", 
                               "Unidad de Métodos y Acceso a Datos",
                               style = "font-size:12px; color:Navy;
                             text-decoration:underline;"),
                        br(),
                        br(),
                        img(src = "logo_umad.png", height="70%",
                            width = "70%", align = "left"),
                        br(),
                        br()
                        
           ),
           
           mainPanel(
             tags$style(type = "text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             ),
             tags$h3(style="display:inline-block",
                     uiOutput("title_indicador_SI_tp")),
             div(style="display:inline-block",
                 dropdown(
                   style = "minimal",
                   status = "primary",
                   width = "500px",
                   right = TRUE,
                   icon = icon("calculator", lib = "font-awesome"),
                   uiOutput("calculo_SI_tp"))
             ),
             div(style="display:inline-block",
                 dropdown(
                   style = "minimal",
                   status = "primary",
                   width = "500px",
                   right = TRUE,
                   icon = icon("info", lib = "font-awesome"),
                   uiOutput("info_SI_tp"))
             ),
             br(),
             br(),
             tags$h6(uiOutput("subtitle_indicador_SI_tp")),
             
             br(),
             plotOutput("plot_SI_tp", height = "500px")%>% withSpinner(color="#5b6f8a"),
             br(),
             fluidRow(column(12, div(downloadButton("graficos_resultado_SI_tp_descarga", 
                                                    "Descarga el gráfico",
                                                    style = "background-color: #3E6C9A; color: white; border: none;"),
                                     style = "float: right"))),
             br(),
             br(),
             DTOutput("tabla_resultado_SI_tp"),
             br(),
             br(),
             br(),
             fluidRow(column(12, div(downloadButton("tabla_resultado_SI_tp_descarga", 
                                                    "Descarga la tabla",
                                                    style = "background-color: #3E6C9A; color: white; border: none;"),
                                     style = "float: right"))),
             br(),
             br()
           )
           
         )),
       
       
       
       tabPanel(
         "País urbano",
         br(),
         
         fluidRow(
           
           #div( id ="Sidebar",
           
           
           # Menú lateral
           
           sidebarPanel(width = 3,
                        style = "background-color: #F8F8FF",
                        selectInput(
                          "indicador_SI_pu",
                          "Seleccione el indicador:",
                          choices = unique(df %>%
                                             filter(CATEGORIA == "Salud infantil" & PESTAÑA =="País urbano") %>%
                                             pull(NOMINDICADOR))),
                        
                        uiOutput("selectcorte8"),
                        tags$a(href="https://umad.cienciassociales.edu.uy/",
                               "Unidad de Métodos y Acceso a Datos",
                               style = "font-size:12px; color:Navy;
                             text-decoration:underline;"),
                        br(),
                        br(),
                        img(src = "logo_umad.png", height="70%",
                            width = "70%", align = "left"),
                        br(),
                        br()
                        
           ),
           
           mainPanel(
             tags$style(type = "text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             ),
             tags$h3(style="display:inline-block",
                     uiOutput("title_indicador_SI_pu")),
             div(style="display:inline-block",
                 dropdown(
                   style = "minimal",
                   status = "primary",
                   width = "500px",
                   right = TRUE,
                   icon = icon("calculator", lib = "font-awesome"),
                   uiOutput("calculo_SI_pu"))
             ),
             div(style="display:inline-block",
                 dropdown(
                   style = "minimal",
                   status = "primary",
                   width = "500px",
                   right = TRUE,
                   icon = icon("info", lib = "font-awesome"),
                   uiOutput("info_SI_pu"))
             ),
             br(),
             br(),
             tags$h6(uiOutput("subtitle_indicador_SI_pu")),
             
             br(),
             plotOutput("plot_SI_pu", height = "500px")%>% withSpinner(color="#5b6f8a"),
             br(),
             fluidRow(column(12, div(downloadButton("graficos_resultado_SI_pu_descarga",
                                                    "Descarga el gráfico",
                                                    style = "background-color: #3E6C9A; color: white; border: none;"),
                                     style = "float: right"))),
             br(),
             br(),
             DTOutput("tabla_resultado_SI_pu"),
             br(),
             br(),
             br(),
             fluidRow(column(12, div(downloadButton("tabla_resultado_SI_pu_descarga",
                                                    "Descarga la tabla",
                                                    style = "background-color: #3E6C9A; color: white; border: none;"),
                                     style = "float: right"))),
             br(),
             br()
           )
           
         )))),  
                     
                     ## Planificación familiar
                     
                     
                     # Total país
                     
                     tabPanel(
                       title = "Planificación familiar",
                       value = 'borelito',
                       icon = icon("calendar-alt", lib = "font-awesome"),
                       
                       tabsetPanel(
                         type = "pills",
                         id   = "tabsetborel",
                         
                         
                         tabPanel(
                           "Total País",
                           br(),
                           
                           fluidRow(
                             
                             #div( id ="Sidebar",
                             
                             
                             # Menú lateral
                             
                             sidebarPanel(width = 3,
                                          style = "background-color: #F8F8FF",
                                          selectInput(
                                            "indicador_PF_tp",
                                            "Seleccione el indicador:",
                                            choices = unique(df %>%
                                                               filter(CATEGORIA == "Planificación familiar" & PESTAÑA =="Total país") %>%
                                                               pull(NOMINDICADOR))),
                                          
                                          tags$a(href="https://umad.cienciassociales.edu.uy/", 
                                                 "Unidad de Métodos y Acceso a Datos",
                                                 style = "font-size:12px; color:Navy;
                             text-decoration:underline;"),
                                          br(),
                                          br(),
                                          img(src = "logo_umad.png", height="70%",
                                              width = "70%", align = "left"),
                                          br(),
                                          br()
                                          
                             ),
                             
                             mainPanel(
                               tags$style(type = "text/css",
                                          ".shiny-output-error { visibility: hidden; }",
                                          ".shiny-output-error:before { visibility: hidden; }"
                               ),
                               tags$h3(style="display:inline-block",
                                       uiOutput("title_indicador_PF_tp")),
                               div(style="display:inline-block",
                                   dropdown(
                                     style = "minimal",
                                     status = "primary",
                                     width = "500px",
                                     right = TRUE,
                                     icon = icon("calculator", lib = "font-awesome"),
                                     uiOutput("calculo_PF_tp"))
                               ),
                               div(style="display:inline-block",
                                   dropdown(
                                     style = "minimal",
                                     status = "primary",
                                     width = "500px",
                                     right = TRUE,
                                     icon = icon("info", lib = "font-awesome"),
                                     uiOutput("info_PF_tp"))
                               ),
                               br(),
                               br(),
                               tags$h6(uiOutput("subtitle_indicador_PF_tp")),
                               
                               br(),
                               plotOutput("plot_PF_tp", height = "500px")%>% withSpinner(color="#5b6f8a"),
                               br(),
                               fluidRow(column(12, div(downloadButton("graficos_resultado_PF_tp_descarga", 
                                                                      "Descarga el gráfico",
                                                                      style = "background-color: #3E6C9A; color: white; border: none;"),
                                                       style = "float: right"))),
                               br(),
                               br(),
                               DTOutput("tabla_resultado_PF_tp"),
                               br(),
                               br(),
                               br(),
                               fluidRow(column(12, div(downloadButton("tabla_resultado_PF_tp_descarga", 
                                                                      "Descarga la tabla",
                                                                      style = "background-color: #3E6C9A; color: white; border: none;"),
                                                       style = "float: right"))),
                               br(),
                               br()
                             )
                             
                           )),
                         
                         
                         
                         tabPanel(
                           "País urbano",
                           br(),
                           
                           fluidRow(
                             
                             #div( id ="Sidebar",
                             
                             
                             # Menú lateral
                             
                             sidebarPanel(width = 3,
                                          style = "background-color: #F8F8FF",
                                          selectInput(
                                            "indicador_PF_pu",
                                            "Seleccione el indicador:",
                                            choices = unique(df %>%
                                                               filter(CATEGORIA == "Planificación familiar" & PESTAÑA =="País urbano") %>%
                                                               pull(NOMINDICADOR))),
                                          
                                          uiOutput("selectcorte3"),
                                          tags$a(href="https://umad.cienciassociales.edu.uy/",
                                                 "Unidad de Métodos y Acceso a Datos",
                                                 style = "font-size:12px; color:Navy;
                             text-decoration:underline;"),
                                          br(),
                                          br(),
                                          img(src = "logo_umad.png", height="70%",
                                              width = "70%", align = "left"),
                                          br(),
                                          br()
                                          
                             ),
                             
                             mainPanel(
                               tags$style(type = "text/css",
                                          ".shiny-output-error { visibility: hidden; }",
                                          ".shiny-output-error:before { visibility: hidden; }"
                               ),
                               tags$h3(style="display:inline-block",
                                       uiOutput("title_indicador_PF_pu")),
                               div(style="display:inline-block",
                                   dropdown(
                                     style = "minimal",
                                     status = "primary",
                                     width = "500px",
                                     right = TRUE,
                                     icon = icon("calculator", lib = "font-awesome"),
                                     uiOutput("calculo_PF_pu"))
                               ),
                               div(style="display:inline-block",
                                   dropdown(
                                     style = "minimal",
                                     status = "primary",
                                     width = "500px",
                                     right = TRUE,
                                     icon = icon("info", lib = "font-awesome"),
                                     uiOutput("info_PF_pu"))
                               ),
                               br(),
                               br(),
                               tags$h6(uiOutput("subtitle_indicador_PF_pu")),
                               
                               br(),
                               plotOutput("plot_PF_pu", height = "500px")%>% withSpinner(color="#5b6f8a"),
                               br(),
                               fluidRow(column(12, div(downloadButton("graficos_resultado_PF_pu_descarga",
                                                                      "Descarga el gráfico",
                                                                      style = "background-color: #3E6C9A; color: white; border: none;"),
                                                       style = "float: right"))),
                               br(),
                               br(),
                               DTOutput("tabla_resultado_PF_pu"),
                               br(),
                               br(),
                               br(),
                               fluidRow(column(12, div(downloadButton("tabla_resultado_PF_pu_descarga",
                                                                      "Descarga la tabla",
                                                                      style = "background-color: #3E6C9A; color: white; border: none;"),
                                                       style = "float: right"))),
                               br(),
                               br()
                             )))
                       ))
                  ))
                          









# Definimos el servidor  
server <- function(input, output, session) {  
  
  
  #Composición y estructura de los hogares
  base_CE_tp <- reactive({
     df %>%
       filter(CATEGORIA == "Composición y estructura de los hogares" & PESTAÑA== "Total país"&
              NOMINDICADOR == input$indicador_CE_tp) %>%
       mutate(VALOR = as.numeric(VALOR))
     
   }) 
   
  
   base_CE_pu <- reactive({
     df %>%
       filter(CATEGORIA == "Composición y estructura de los hogares" & PESTAÑA== "País urbano" &
                NOMINDICADOR == input$indicador_CE_pu) %>%
       mutate(VALOR = as.numeric(VALOR))
     
   }) 
   
   
#Cuidados y educación
   base_CUID_tp <- reactive({
     df %>%
       filter(CATEGORIA == "Cuidados y educación" & PESTAÑA== "Total país" &
                NOMINDICADOR == input$indicador_CUID_tp) %>%
       mutate(VALOR = as.numeric(VALOR))
     
   }) 
   
   base_CUID_pu <- reactive({
     df %>%
       filter(CATEGORIA == "Cuidados y educación" & PESTAÑA== "País urbano" &
                NOMINDICADOR == input$indicador_CUID_pu) %>%
       mutate(VALOR = as.numeric(VALOR))
     
   }) 
   
   
#Violencia de género
   base_VG <- reactive({
     df %>%
       filter(CATEGORIA == "Violencia de género" & PESTAÑA== "País urbano" &
                NOMINDICADOR == input$indicador_VG) %>%
       mutate(VALOR = as.numeric(VALOR))
     
   }) 
   
   

   #Salud infantil
   base_SI_tp <- reactive({
     df %>%
       filter(CATEGORIA == "Salud infantil" & PESTAÑA== "Total país" &
                NOMINDICADOR == input$indicador_SI_tp) %>%
       mutate(VALOR = as.numeric(VALOR))
     
   }) 
   
   base_SI_pu <- reactive({
     df %>%
       filter(CATEGORIA == "Salud infantil" & PESTAÑA== "País urbano" &
                NOMINDICADOR == input$indicador_SI_pu) %>%
       mutate(VALOR = as.numeric(VALOR))
     
   }) 
   
   
   
   
#Planificación familiar   
   
   
   base_PF_tp <- reactive({
     df %>%
       filter(CATEGORIA == "Planificación familiar" & PESTAÑA== "Total país" &
                NOMINDICADOR == input$indicador_PF_tp) %>%
       mutate(VALOR = as.numeric(VALOR))
     
   }) 
   
   base_PF_pu <- reactive({
     df %>%
       filter(CATEGORIA == "Planificación familiar" & PESTAÑA== "País urbano" &
                NOMINDICADOR == input$indicador_PF_pu) %>%
       mutate(VALOR = as.numeric(VALOR))
     
   }) 
   
   
   
   #Composición y estructura de los hogares
   
   
      output$selectcorte1 <- renderUI({
     if(input$indicador_CE_tp == "Tasa de divorcios"| input$indicador_CE_tp == "Tasa de nupcialidad") {
    
        } else {
     
       selectInput("corte1", "Resultados por:", choices = unique(df$CORTE1_rec[df$PESTAÑA=="Total país"&df$NOMINDICADOR == input$indicador_CE_tp]),
                 selected = "Total")
    }
   })
   
   output$selectcorte2 <- renderUI({
       selectInput("corte2", "Resultados por:", choices = unique(df$CORTE1_rec[df$PESTAÑA=="País urbano"&df$NOMINDICADOR ==input$indicador_CE_pu]),
                   selected = "Total")
   })

   
   # output$anio1 <- renderUI({
   #   
   # tipo_graf <- rlang::sym(rlang::as_string(df$tipo_graf2[1]))
   # 
   #   if(tipo_graf == "1") {
   #          } else if(input$corte1!="Total"&(tipo_graf == "2" | tipo_graf == "3")) {
   #                selectInput("anio1", "Año:",
   #                 choices = unique(base_CE_tp()$FECHA2[base_CE_tp()$NOMINDICADOR ==input$indicador_CE_tp])
   #     )
   #     
   #   }
   # })
   
   output$anio1 <- renderUI({

     if(substr(input$indicador_CE_tp,1,4)=="Dist"&input$corte1!="Total") {
       selectInput("anio1", "Año:",
                   choices = unique(base_CE_tp()$FECHA2[base_CE_tp()$NOMINDICADOR ==input$indicador_CE_tp&
                                                        base_CE_tp()$CORTE1_rec ==input$corte1]))
                   } else  {

     }
   })
   
   
   output$anio2 <- renderUI({
     
     tipo_graf <- rlang::sym(rlang::as_string(base_CE_pu()$tipo_graf2[1]))
     
     if(tipo_graf == "1") {
     } else if(input$corte2!="Total"&(tipo_graf == "2" | tipo_graf == "3")) {
       selectInput("anio2", "Año:",
                   choices = unique(base_CE_pu()$FECHA2[base_CE_pu()$NOMINDICADOR ==input$indicador_CE_pu])
       )
       
     }
   })
   
   
   output$selec_dpto <- renderUI({
     
     req(input$indicador_CE_tp)
     
     if (input$indicador_CE_tp %in% c("Tasa de divorcios", "Tasa de nupcialidad")) {
       
              selectInput("dpto", "Departamento:", 
                    choices = c("Total país", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida",
                           "Lavalleja", "Maldonado", "Montevideo", "Paysandu", "Rio Negro", "Rivera", "Rocha",
                           "Salto", "San Jose", "Soriano", "Tacuarembo", "Treinta Y Tres"),
                    selected = "Total país",
                   # options = list(`actions-box` = TRUE,
                   #                `deselect-all-text` = "Borrar todo",
                   #                `select-all-text` = "Seleccionar todo",
                   #                `none-selected-text` = "Total",
                   #                `live-search` = TRUE),
       multiple = T)
     } else {
       
     }
       
   })
   

   
   
   ##Cuidados y educación
   
   
      output$selectcorte4 <- renderUI({
       selectInput("corte4", "Resultados por:", choices = unique(df$CORTE1_rec[df$PESTAÑA=="Total país"&df$NOMINDICADOR ==input$indicador_CUID_tp]),
                   selected = "Total")
     
   })
   
   output$selectcorte5 <- renderUI({
     selectInput("corte5", "Resultados por:", choices = unique(df$CORTE1_rec[df$PESTAÑA=="País urbano"&df$NOMINDICADOR ==input$indicador_CUID_pu]),
                 selected = "Total")
   })
   

   #Violencia de género
   
   
   output$selectcorte6 <- renderUI({
       selectInput("corte6", "Resultados por:", choices = unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_VG]),
                   selected = "Total")
     
   })
   
   output$anio3 <- renderUI({
     
     if(input$corte6 == "Total") {
     } else if(input$corte6!="Total") {
       selectInput("anio3", "Año:",
                   choices = unique(base_VG()$FECHA2[base_VG()$NOMINDICADOR ==input$indicador_VG])
       )
       
     }
   })
   
   
   
   ##Salud infantil
   
   
   output$selectcorte7 <- renderUI({
     selectInput("corte7", "Resultados por:", choices = unique(df$CORTE1_rec[df$PESTAÑA=="Total país"&df$NOMINDICADOR ==input$indicador_SI_tp]),
                 selected = "Total")
     
   })
   
   output$selectcorte8 <- renderUI({
     selectInput("corte8", "Resultados por:", choices = unique(df$CORTE1_rec[df$PESTAÑA=="País urbano"&df$NOMINDICADOR ==input$indicador_SI_pu]),
                 selected = "Total")
   })
   
   
      
   #Planificación familiar
   output$selectcorte3 <- renderUI({
     selectInput("corte3", "Resultados por:", choices = unique(df$CORTE1_rec[df$PESTAÑA=="País urbano"&df$NOMINDICADOR ==input$indicador_PF_pu]),
                 selected = "Total")
   })
   
   
   
   
      
   ## TITULOS
   
   #Composición y estructura de los hogares
       output$title_indicador_CE_tp <- renderUI({ 
         helpText(HTML(input$indicador_CE_tp))
       })
   
       output$title_indicador_CE_pu <- renderUI({ 
         helpText(HTML(input$indicador_CE_pu))
       })
       
    
   #Cuidados y educación
       output$title_indicador_CUID_tp <- renderUI({ 
         helpText(HTML(input$indicador_CUID_tp))
       })
       
       output$title_indicador_CUID_pu <- renderUI({ 
         helpText(HTML(input$indicador_CUID_pu))
       })
       
       
    #Violencia de género
       output$title_indicador_VG <- renderUI({ 
         helpText(HTML(input$indicador_VG))
       })
       
       
   #Salud infantil
       output$title_indicador_SI_tp <- renderUI({ 
         helpText(HTML(input$indicador_SI_tp))
       })
       
       output$title_indicador_SI_pu <- renderUI({ 
         helpText(HTML(input$indicador_SI_pu))
       })
       
       
       
       
    #Planificación familiar
       output$title_indicador_PF_tp <- renderUI({ 
         helpText(HTML(input$indicador_PF_tp))
       })
       
       output$title_indicador_PF_pu <- renderUI({ 
         helpText(HTML(input$indicador_PF_pu))
       })
       
    ## SUB- TÍTULO
       
       #Composición y estructura de los hogares
       output$subtitle_indicador_CE_tp <- renderUI({ 

         subtitle1 = unique(metadata %>%
                           filter(NOMINDICADOR == input$indicador_CE_tp) %>% pull(RELEVANCIA))

         helpText(HTML(subtitle1))
         
         })
       
       output$subtitle_indicador_CE_pu <- renderUI({ 
         
         subtitle2 = unique(metadata %>%
                             filter(NOMINDICADOR == input$indicador_CE_pu) %>% pull(RELEVANCIA))
         
         helpText(HTML(subtitle2))
         
       })
       
       

   #Cuidados y educación
       output$subtitle_indicador_CUID_tp <- renderUI({ 
         
         subtitle5 = unique(metadata %>%
                              filter(NOMINDICADOR == input$indicador_CUID_tp) %>% pull(RELEVANCIA))
         
         helpText(HTML(subtitle5))
         
       })
       
       output$subtitle_indicador_CUID_pu <- renderUI({ 
         
         subtitle6 = unique(metadata %>%
                              filter(NOMINDICADOR == input$indicador_CUID_pu) %>% pull(RELEVANCIA))
         
         helpText(HTML(subtitle6))
         
       })
       
       
       #Violencia de género
       output$subtitle_indicador_VG <- renderUI({ 
         
         subtitle7 = unique(metadata %>%
                              filter(NOMINDICADOR == input$indicador_VG) %>% pull(RELEVANCIA))
         
         helpText(HTML(subtitle7))
         
       })
       
       
       #Cuidados y educación
       output$subtitle_indicador_SI_tp <- renderUI({ 
         
         subtitle8 = unique(metadata %>%
                              filter(NOMINDICADOR == input$indicador_SI_tp) %>% pull(RELEVANCIA))
         
         helpText(HTML(subtitle8))
         
       })
       
       output$subtitle_indicador_SI_pu <- renderUI({ 
         
         subtitle9 = unique(metadata %>%
                              filter(NOMINDICADOR == input$indicador_SI_pu) %>% pull(RELEVANCIA))
         
         helpText(HTML(subtitle9))
         
       })       
       
       
       #Planificación familiar
       output$subtitle_indicador_PF_tp <- renderUI({ 
         
         subtitle3 = unique(metadata %>%
                              filter(NOMINDICADOR == input$indicador_PF_tp) %>% pull(RELEVANCIA))
         
         helpText(HTML(subtitle3))
         
       })
       
       output$subtitle_indicador_PF_pu <- renderUI({ 
         
         subtitle4 = unique(metadata %>%
                              filter(NOMINDICADOR == input$indicador_PF_pu) %>% pull(RELEVANCIA))
         
         helpText(HTML(subtitle4))
         
       })
       
       
       
       
       
       # Calculo

       #Composición y estructura de los hogares
       output$calculo_CE_tp <- renderUI({ 
         
         calculo1 = unique(metadata %>%
                             filter(NOMINDICADOR == input$indicador_CE_tp) %>% pull(calculo))
         
         helpText(HTML(paste("<b> Forma de cálculo:</b>", calculo1)))
         
       })
       
       output$calculo_CE_pu <- renderUI({ 
         
         calculo2 = unique(metadata %>%
                            filter(NOMINDICADOR == input$indicador_CE_pu) %>% pull(calculo))
         
         helpText(HTML(paste("<b> Forma de cálculo:</b>", calculo2)))
         
       })
       

       
       
       
       #Cuidados y educación
       
       output$calculo_CUID_tp <- renderUI({ 
         
         calculo5 = unique(metadata %>%
                             filter(NOMINDICADOR == input$indicador_CUID_tp) %>% pull(calculo))
         
         helpText(HTML(paste("<b> Forma de cálculo:</b>", calculo5)))
         
       })
       
       output$calculo_CUID_pu <- renderUI({ 
         
         calculo6 = unique(metadata %>%
                             filter(NOMINDICADOR == input$indicador_CUID_pu) %>% pull(calculo))
         
         helpText(HTML(paste("<b> Forma de cálculo:</b>", calculo6)))
         
       })
       
       #Violencia de género
       
       output$calculo_VG <- renderUI({ 
         
         calculo7 = unique(metadata %>%
                             filter(NOMINDICADOR == input$indicador_VG) %>% pull(calculo))
         
         helpText(HTML(paste("<b> Forma de cálculo:</b>", calculo7)))
         
       })
       

       #Salud infantil
       
       output$calculo_SI_tp <- renderUI({ 
         
         calculo8 = unique(metadata %>%
                             filter(NOMINDICADOR == input$indicador_SI_tp) %>% pull(calculo))
         
         helpText(HTML(paste("<b> Forma de cálculo:</b>", calculo8)))
         
       })
       
       output$calculo_SI_pu <- renderUI({ 
         
         calculo9 = unique(metadata %>%
                             filter(NOMINDICADOR == input$indicador_SI_pu) %>% pull(calculo))
         
         helpText(HTML(paste("<b> Forma de cálculo:</b>", calculo9)))
         
       })
       
       
       #Planificación familiar
       output$calculo_PF_tp <- renderUI({ 
         
         calculo3 = unique(metadata %>%
                             filter(NOMINDICADOR == input$indicador_PF_tp) %>% pull(calculo))
         
         helpText(HTML(paste("<b> Forma de cálculo:</b>", calculo3)))
         
       })
       
       output$calculo_PF_pu <- renderUI({ 
         
         calculo4 = unique(metadata %>%
                             filter(NOMINDICADOR == input$indicador_PF_pu) %>% pull(calculo))
         
         helpText(HTML(paste("<b> Forma de cálculo:</b>", calculo4)))
         
       })
       
       
       
       # Observaciones
       
       #Composición y estructura de los hogares
       output$info_CE_tp <- renderUI({ 
         
         info1 = unique(metadata %>%
                            filter(NOMINDICADOR == input$indicador_CE_tp) %>% pull(NOTAS2))
         
         helpText(HTML(paste("<b> Observaciones:</b>", info1)))
         
       })
       
       output$info_CE_tp <- renderUI({ 
         
         info2 = unique(metadata %>%
                         filter(NOMINDICADOR == input$indicador_CE_tp) %>% pull(NOTAS2))
         
         helpText(HTML(paste("<b> Observaciones:</b>", info2)))
         
       })
       

       #Cuidados y educación
       output$info_CUID_tp <- renderUI({ 
         
         info5 = unique(metadata %>%
                          filter(NOMINDICADOR == input$indicador_CUID_tp) %>% pull(NOTAS2))
         
         helpText(HTML(paste("<b> Observaciones:</b>", info5)))
         
       })
       
       output$info_CUID_tp <- renderUI({ 
         
         info6 = unique(metadata %>%
                          filter(NOMINDICADOR == input$indicador_CUID_tp) %>% pull(NOTAS2))
         
         helpText(HTML(paste("<b> Observaciones:</b>", info6)))
         
       })
       
       
       
       #Violencia de género
       output$info_VG <- renderUI({ 
         
         info7 = unique(metadata %>%
                          filter(NOMINDICADOR == input$indicador_VG) %>% pull(NOTAS2))
         
         helpText(HTML(paste("<b> Observaciones:</b>", info7)))
         
       })
       

       
       #Salud infantil
       output$info_SI_tp <- renderUI({ 
         
         info8 = unique(metadata %>%
                          filter(NOMINDICADOR == input$indicador_SI_tp) %>% pull(NOTAS2))
         
         helpText(HTML(paste("<b> Observaciones:</b>", info8)))
         
       })
       
       output$info_SI_tp <- renderUI({ 
         
         info9 = unique(metadata %>%
                          filter(NOMINDICADOR == input$indicador_SI_tp) %>% pull(NOTAS2))
         
         helpText(HTML(paste("<b> Observaciones:</b>", info9)))
         
       })
       
      
       #Planificación familiar
       output$info_PF_tp <- renderUI({ 
         
         info3 = unique(metadata %>%
                          filter(NOMINDICADOR == input$indicador_PF_tp) %>% pull(NOTAS2))
         
         helpText(HTML(paste("<b> Observaciones:</b>", info3)))
         
       })
       
       output$info_PF_tp <- renderUI({ 
         
         info4 = unique(metadata %>%
                          filter(NOMINDICADOR == input$indicador_PF_tp) %>% pull(NOTAS2))
         
         helpText(HTML(paste("<b> Observaciones:</b>", info4)))
         
       })

       
       
              
    ###### COMPOSICIÓN Y ESTRUCTURA DE LOS HOGARES. TOTAL PAÍS
    

              ## GRÁFICOS
       
       
       output$plot_CE_tp <- renderPlot({
         
         
         # Gráficos de línea para totales de indicadores con ECH (Transparencia por cambio metodológico)
         
         if(substring(input$indicador_CE_tp,1,5) != "Distr" & input$corte1 == "Total" & substring(input$indicador_CE_tp,1,4) != "Tasa"){
           
           dat <- base_CE_tp() %>% filter(NOMINDICADOR == input$indicador_CE_tp & is.na(CORTE1) == T)
           
           g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), alpha = metodo, color = NOMINDICADOR)) +
             geom_line(dat = dat %>% filter(FECHA <= 2019),
                       size = 1, alpha = 0.4) +
             geom_line(dat = dat %>% filter(FECHA >= 2022),
                       size = 1, alpha = 0.8) +
             geom_point(size = 2, show.legend = FALSE) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12), 
                   axis.title = element_text(size = 14), 
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_alpha_manual(values = c(.4, .8)) +
             scale_color_manual(
               values = c(color_defecto)) +
             scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CE_tp,
               caption = str_wrap(paste("Fuente:",unique(metadata %>% 
                                                           filter(NOMINDICADOR == input$indicador_CE_tp) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "")
           
           
           
           # Gráficos de línea para totales de indicadores que no se calculan con ECH
           
         } else if(substring(input$indicador_CE_tp,1,5) != "Distr" & substring(input$indicador_CE_tp,1,4) == "Tasa") {  
           
           dat <- base_CE_tp() %>% filter(NOMINDICADOR == input$indicador_CE_tp) %>%
             filter(Departamento %in% input$dpto)
           
           g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), color = Departamento)) +
             geom_line(dat = dat, size = 1) +
             geom_point(size = 2, show.legend = FALSE) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12), 
                   axis.title = element_text(size = 14), 
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CE_tp,
               caption = str_wrap(paste("Fuente:",unique(metadata %>% 
                                                           filter(NOMINDICADOR == input$indicador_CE_tp) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +
             
             scale_color_brewer(name = "", palette = paleta)  
           
           
           # Gráficos de línea para cortes de indicadores con ECH (Transparencia por cambio metodológico) 
           
         } else if(substring(input$indicador_CE_tp,1,5) != "Distr" & input$corte1 != "Total" & substring(input$indicador_CE_tp,1,4) != "Tasa") {
           
           
           dat <- base_CE_tp() %>% filter(NOMINDICADOR == input$indicador_CE_tp & CORTE1_rec == input$corte1)
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CE_tp&df$CORTE1_rec==input$corte1])))
           
           g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), alpha = metodo, 
                                    group = !!cut, 
                                    color = !!cut)) +
             geom_line(dat = dat %>% filter(FECHA <= 2019),
                       size = 1, alpha = 0.4) +
             geom_line(dat = dat %>% filter(FECHA >= 2022),
                       size = 1, alpha = 0.8) +
             geom_point(size = 2, show.legend = FALSE) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12), 
                   axis.title = element_text(size = 14), 
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_alpha_manual(values = c(.4, .8)) +
             scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CE_tp,
               caption = str_wrap(paste("Fuente:",unique(metadata %>% 
                                                           filter(NOMINDICADOR == input$indicador_CE_tp) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +
             
             scale_color_brewer(name = "", palette = paleta)  
           
           
         } else if(substring(input$indicador_CE_tp,1,5) == "Distr" & input$corte1 == "Total"){
           
           # Gráficos de barras para totales de indicadores con ECH (Transparencia por cambio metodológico)
           
           dat <- base_CE_tp() %>% filter(NOMINDICADOR == input$indicador_CE_tp & is.na(CORTE1) == T) 
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_CE_tp])))
           
           g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), 
                                    y = as.numeric(VALOR), 
                                    fill = !!cut)) +
             geom_bar(dat = dat %>% filter(FECHA <= 2019),
                      size = 1, stat = "identity", alpha = 0.4) +
             geom_bar(dat = dat %>% filter(FECHA == 2020|FECHA == 2021),
                      size = 1, stat = "identity", alpha = 0.6) +
             geom_bar(dat = dat %>% filter(FECHA >= 2022),
                      size = 1, stat = "identity", alpha = 0.8) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 14),
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CE_tp,
               caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                           filter(NOMINDICADOR == input$indicador_CE_tp) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "")+
             scale_fill_brewer(name = "", palette = paleta)
           
           
           
         } else if(substring(input$indicador_CE_tp,1,5) == "Distr" & input$corte1 != "Total") {
           
           # Gráficos de barras para cortes de indicadores con ECH (Transparencia por cambio metodológico)
           
           
           dat <- base_CE_tp() %>% filter(NOMINDICADOR == input$indicador_CE_tp & CORTE1_rec == input$corte1) %>%
             filter(FECHA %in% input$anio1)
           cut1 <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_CE_tp])))
           cut2 <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CE_tp&df$CORTE1_rec==input$corte1])))
           
           g1 <- dat %>% ggplot(aes(x = !!cut2, 
                                    y = as.numeric(VALOR), 
                                    fill = !!cut1)) +
             geom_bar(size = 1, stat = "identity", width = 0.4) +
             
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12), 
                   axis.title = element_text(size = 14), 
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CE_tp,
               caption = str_wrap(paste("Fuente:",unique(metadata %>% 
                                                           filter(NOMINDICADOR == input$indicador_CE_tp) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "")+
             scale_fill_brewer(name = "", palette = paleta)
           
           
           
         } else {
           g1 <- base_CE_tp() %>%
             
             ggplot(aes(x = FECHA, y = VALOR)) +
             
             geom_line(aes(group = 1, color = NOMINDICADOR)) +
             
             geom_point(size = 2, show.legend = FALSE) +
             
             theme_minimal() +
             
             theme(legend.position = "bottom",
                   legend.title = element_blank()) +
             
             scale_alpha_manual(values = c(.4, .8)) +
             
             scale_color_manual(
               values = c(639)) 
         }
         
         
         print(g1)
         # ggsave("www/g1.png", width = 30, height = 20, units = "cm")
         
       }) 
       
       ### DESCARGA DE GRÁFICOS
       
       output$graficos_resultado_CE_tp_descarga <- downloadHandler(
         filename <- function() {
           paste("g1", "png", sep = ".")
         },
         
         content <- function(file) {
           file.copy("www/g1.png", file)
         },
         contentType = "www/g1"
       )
       
       ### TABLAS
       
       output$tabla_resultado_CE_tp <- renderDT({
         
         
         if(substring(input$indicador_CE_tp,1,5) != "Distr" & input$corte1 == "Total" & substring(input$indicador_CE_tp,1,4) != "Tasa"){
           
           # Gráficos de línea para totales de indicadores ECH
           
           dat <- base_CE_tp() %>% filter(NOMINDICADOR == input$indicador_CE_tp & is.na(CORTE1) == T)
           
           datatable(dat  %>% 
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
           
           
         } else if(substring(input$indicador_CE_tp,1,5) != "Distr" & substring(input$indicador_CE_tp,1,4) == "Tasa") {  
           
           # Gráficos de línea para cortes de indicadores no ECH
           
           dat <- base_CE_tp() %>% filter(NOMINDICADOR == input$indicador_CE_tp) %>%
             filter(Departamento %in% input$dpto) %>% 
             select(FECHA,Departamento,VALOR)
           
           datatable(dat  %>% 
                       arrange(Departamento)%>%
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         "Departamento" = Departamento,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
           
           
         } else if(substring(input$indicador_CE_tp,1,5) != "Distr" & input$corte1 != "Total" & substring(input$indicador_CE_tp,1,4) != "Tasa") {
           
           # Gráficos de línea para cortes de indicadores ECH  
           
           dat <- base_CE_tp() %>% filter(NOMINDICADOR == input$indicador_CE_tp & CORTE1_rec == input$corte1 & substring(input$indicador_CE_tp,1,4) != "Tasa")
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CE_tp&df$CORTE1_rec==input$corte1])))
           cut_nom <- quo_name(cut)
           
           datatable(dat  %>% 
                       arrange(!!cut)%>%
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         !!cut_nom := !!cut,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
           
         } else if(substring(input$indicador_CE_tp,1,5) == "Distr" & input$corte1 == "Total"){
           # Gráficos de barras para totales de indicadores ENAJ
           
           dat <- base_CE_tp() %>% filter(NOMINDICADOR == input$indicador_CE_tp & input$corte1 == "Total") 
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_CE_tp])))
           cut_nom <- quo_name(cut)
           
           datatable(dat %>% 
                       filter(NOMINDICADOR==input$indicador_CE_tp & is.na(CORTE1) == T) %>%
                       arrange(!!cut)%>%
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         !!cut_nom := !!cut,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
         } else if(substring(input$indicador_CE_tp,1,5) == "Distr" & input$corte1 != "Total") {
           
           
           dat <- base_CE_tp() %>% filter(NOMINDICADOR == input$indicador_CE_tp & CORTE1_rec == input$corte1)   %>%
             filter(FECHA %in% input$anio1)
           cut1 <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_CE_tp])))
           cut2 <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CE_tp&df$CORTE1_rec==input$corte1])))
           cut_nom1 <- quo_name(cut1)
           cut_nom2 <- quo_name(cut2)
           
           datatable(dat %>% 
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         !!cut_nom1 := !!cut1,
                         !!cut_nom2 := !!cut2,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
           
           
           
         } else {          
           
           datatable(base_CE_tp() %>% 
                       filter(NOMINDICADOR==input$indicador_CE_tp&CORTE1_rec==input$corte1) %>%
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
         }
       }) 
       
       output$tabla_resultado_CE_tp_descarga <- downloadHandler(
         filename = function() {
           paste0(input$indicador_CE_tp, ".xlsx", sep = "")
         },
         content = function(file) {  
           
           if(substring(input$indicador_CE_tp,1,5) != "Distr" & input$corte1 == "Total" & substring(input$indicador_CE_tp,1,4) != "Tasa"){
             
             dat <- base_CE_tp() %>% filter(NOMINDICADOR == input$indicador_CE_tp & is.na(CORTE1) == T)
             
             openxlsx::write.xlsx(list( "Data" = dat %>%
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            "Valor" = VALOR),
                                        "Metadata" = base_CE_tp() %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
           } else if(substring(input$indicador_CE_tp,1,5) != "Distr" & substring(input$indicador_CE_tp,1,4) == "Tasa") {  
             
             dat <- base_CE_tp() %>% filter(NOMINDICADOR == input$indicador_CE_tp) %>%
               filter(Departamento %in% input$dpto) %>% 
               select(FECHA,Departamento,VALOR)
             
             openxlsx::write.xlsx(list( "Data" = dat %>%
                                          arrange(Departamento)%>%
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            "Departamento" = Departamento,
                                            "Valor" = VALOR),
                                        "Metadata" = base_CE_tp() %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
           } else if(substring(input$indicador_CE_tp,1,5) != "Distr" & input$corte1 != "Total" & substring(input$indicador_CE_tp,1,4) != "Tasa") {
             
             dat <- base_CE_tp() %>% filter(NOMINDICADOR == input$indicador_CE_tp & CORTE1_rec == input$corte1 & substring(input$indicador_CE_tp,1,4) != "Tasa")
             cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CE_tp&df$CORTE1_rec==input$corte1])))
             cut_nom <- quo_name(cut)
             
             openxlsx::write.xlsx(list( "Data" = dat %>%
                                          arrange(!!cut)%>%
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            !!cut_nom := !!cut,
                                            "Valor" = VALOR),
                                        "Metadata" = dat %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
             
           } else if(substring(input$indicador_CE_tp,1,5) == "Distr" & input$corte1 == "Total"){
             
             dat <- base_CE_tp() %>% filter(NOMINDICADOR == input$indicador_CE_tp & CORTE1_rec == "Total") 
             cut <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_CE_tp])))
             cut_nom <- quo_name(cut)
             
             openxlsx::write.xlsx(list( "Data" = dat %>%
                                          arrange(!!cut)%>%
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            !!cut_nom := !!cut,
                                            "Valor" = VALOR),
                                        "Metadata" = dat %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
           }           else if(substring(input$indicador_CE_tp,1,5) == "Distr" & input$corte1 != "Total") {
             
             
             dat <- base_CE_tp() %>% filter(NOMINDICADOR == input$indicador_CE_tp & CORTE1_rec == input$corte1)   %>%
               filter(FECHA %in% input$anio1)
             cut1 <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_CE_tp])))
             cut2 <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CE_tp&df$CORTE1_rec==input$corte1])))
             cut_nom1 <- quo_name(cut1)
             cut_nom2 <- quo_name(cut2)
             
             openxlsx::write.xlsx(list( "Data" = dat %>%
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            !!cut_nom1 := !!cut1,
                                            !!cut_nom2 := !!cut2,
                                            "Valor" = VALOR),
                                        "Metadata" = dat %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
             
           }
           
           
           
           
         })  
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       ###### COMPOSICIÓN Y ESTRUCTURA DE LOS HOGARES. PAÍS URBANO
       
       ## GRÁFICOS
       
       
       output$plot_CE_pu <- renderPlot({
         
         
         # Gráficos de línea para totales de indicadores con ECH (Transparencia por cambio metodológico)
         
         if(substring(input$indicador_CE_pu,1,5) != "Distr" & input$corte2 == "Total" & substring(input$indicador_CE_pu,1,4) != "Tasa"){
           
           dat <- base_CE_pu() %>% filter(NOMINDICADOR == input$indicador_CE_pu & CORTE1_rec == "Total")
           
           g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), alpha = metodo, color = NOMINDICADOR)) +
             geom_line(dat = dat %>% filter(FECHA <= 2019),
                       size = 1, alpha = 0.4) +
             geom_line(dat = dat %>% filter(FECHA >= 2022),
                       size = 1, alpha = 0.8) +
             geom_point(size = 2, show.legend = FALSE) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12), 
                   axis.title = element_text(size = 14), 
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_alpha_manual(values = c(.4, .8)) +
             scale_color_manual(
               values = c(color_defecto)) +
             scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CE_pu,
               caption = str_wrap(paste("Fuente:",unique(metadata %>% 
                                                           filter(NOMINDICADOR == input$indicador_CE_pu) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "")
           
           
           
           # Gráficos de línea para cortes de indicadores con ECH (Transparencia por cambio metodológico) 
           
         } else if(substring(input$indicador_CE_pu,1,5) != "Distr" & (input$corte2 != "Total"&input$corte2 != "Total") & substring(input$indicador_CE_pu,1,4) != "Tasa") {


           dat <- base_CE_pu() %>% filter(NOMINDICADOR == input$indicador_CE_pu & CORTE1_rec == input$corte2)
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CE_pu&df$CORTE1_rec==input$corte2])))

           g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), alpha = metodo,
                                    group = !!cut,
                                    color = !!cut)) +
             geom_line(dat = dat %>% filter(FECHA <= 2019),
                       size = 1, alpha = 0.4) +
             geom_line(dat = dat %>% filter(FECHA >= 2022),
                       size = 1, alpha = 0.8) +
             geom_point(size = 2, show.legend = FALSE) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 14),
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_alpha_manual(values = c(.4, .8)) +
             scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CE_pu,
               caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                           filter(NOMINDICADOR == input$indicador_CE_pu) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +

             scale_color_brewer(name = "", palette = paleta)





         } else if(substring(input$indicador_CE_pu,1,34) == "Distribución porcentual de jóvenes" & input$corte2 == "Total"){
           # Gráficos de barras para totales de indicadores ENAJ

           dat <- base_CE_pu() %>% filter(NOMINDICADOR == input$indicador_CE_pu & CORTE1_rec == "Total")
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_CE_pu])))

           g1 <- dat %>% ggplot(aes(x = FECHA,
                                    y = as.numeric(VALOR),
                                    fill = !!cut)) +
             geom_bar(size = 1, stat = "identity", width = 1.5) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 14),
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_x_continuous(breaks = c(2008, 2013, 2018)) +
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CE_pu,
               caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                           filter(NOMINDICADOR == input$indicador_CE_pu) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +
             scale_fill_brewer(name = "", palette = paleta)


         } else if(substring(input$indicador_CE_pu,1,34) == "Distribución porcentual de jóvenes" & input$corte2 == "Sexo") {

           # Gráficos de barras para cortes de indicadores con ENAJ 

           dat <- base_CE_pu() %>% filter(NOMINDICADOR == input$indicador_CE_pu & CORTE1_rec == input$corte2)   %>%
             filter(FECHA %in% input$anio2)
           cut1 <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_CE_pu])))

           g1 <- dat %>% ggplot(aes(x = Sexo,
                                    y = as.numeric(VALOR),
                                    fill = !!cut1)) +
             geom_bar(size = 1, stat = "identity", width = 0.4) +

             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 14),
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CE_pu,
               caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                           filter(NOMINDICADOR == input$indicador_CE_pu) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +
             scale_fill_brewer(name = "", palette = paleta)


           
         } else if(substring(input$indicador_CE_pu,1,5) == "Distr" & input$corte2 == "Total"){

           # Gráficos de barras para totales de indicadores con ECH (Transparencia por cambio metodológico)

           dat <- base_CE_pu() %>% filter(NOMINDICADOR == input$indicador_CE_pu & CORTE1_rec == "Total")
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_CE_pu])))

           g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"),
                                    y = as.numeric(VALOR),
                                    fill = !!cut)) +
             geom_bar(dat = dat %>% filter(FECHA <= 2019),
                      size = 1, stat = "identity", alpha = 0.4) +
             geom_bar(dat = dat %>% filter(FECHA == 2020|FECHA == 2021),
                      size = 1, stat = "identity", alpha = 0.6) +
             geom_bar(dat = dat %>% filter(FECHA >= 2022),
                      size = 1, stat = "identity", alpha = 0.8) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 14),
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CE_pu,
               caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                           filter(NOMINDICADOR == input$indicador_CE_pu) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "")+
             scale_fill_brewer(name = "", palette = paleta)



         } else if(substring(input$indicador_CE_pu,1,5) == "Distr" & input$corte2 != "Total") {

           # Gráficos de barras para cortes de indicadores con ECH (Transparencia por cambio metodológico)


           dat <- base_CE_pu() %>% filter(NOMINDICADOR == input$indicador_CE_pu & CORTE1_rec == input$corte2) %>%
             filter(FECHA %in% input$anio2)
           cut1 <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_CE_pu])))
           cut2 <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CE_pu&df$CORTE1_rec==input$corte2])))

           g1 <- dat %>% ggplot(aes(x = !!cut2,
                                    y = as.numeric(VALOR),
                                    fill = !!cut1)) +
             geom_bar(size = 1, stat = "identity", width = 0.4) +

             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 14),
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CE_pu,
               caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                           filter(NOMINDICADOR == input$indicador_CE_pu) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "")+
             scale_fill_brewer(name = "", palette = paleta)
           
           
           
         } else {
           g1 <- base_CE_pu() %>%
             
             ggplot(aes(x = FECHA, y = VALOR)) +
             
             geom_line(aes(group = 1, color = NOMINDICADOR)) +
             
             geom_point(size = 2, show.legend = FALSE) +
             
             theme_minimal() +
             
             theme(legend.position = "bottom",
                   legend.title = element_blank()) +
             
             scale_alpha_manual(values = c(.4, .8)) +
             
             scale_color_manual(
               values = c(639)) 
         }
         
         
         print(g1)
         # ggsave("www/g1.png", width = 30, height = 20, units = "cm")
         
       }) 
       
       ### DESCARGA DE GRÁFICOS
       
       output$graficos_resultado_CE_pu_descarga <- downloadHandler(
         filename <- function() {
           paste("g1", "png", sep = ".")
         },
         
         content <- function(file) {
           file.copy("www/g1.png", file)
         },
         contentType = "www/g1"
       )
       
       ### TABLAS
       
       output$tabla_resultado_CE_pu <- renderDT({
         
         
         if(substring(input$indicador_CE_pu,1,5) != "Distr" & input$corte2 == "Total" & substring(input$indicador_CE_pu,1,4) != "Tasa"){
           
           # Gráficos de línea para totales de indicadores ECH
           
           dat <- base_CE_pu() %>% filter(NOMINDICADOR == input$indicador_CE_pu & CORTE1_rec == "Total")
           
           datatable(dat  %>% 
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
           
           
           
         } else if(substring(input$indicador_CE_pu,1,5) != "Distr" & input$corte2 != "Total" & substring(input$indicador_CE_pu,1,4) != "Tasa") {

           # Gráficos de línea para cortes de indicadores ECH

           dat <- base_CE_pu() %>% filter(NOMINDICADOR == input$indicador_CE_pu & CORTE1_rec == input$corte2 & substring(input$indicador_CE_pu,1,4) != "Tasa")
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CE_pu&df$CORTE1_rec==input$corte2])))
           cut_nom <- quo_name(cut)

           datatable(dat  %>%
                       arrange(!!cut)%>%
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         !!cut_nom := !!cut,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs =
                                      list(list(className = 'dt-center',
                                                targets = "_all"))))


         } else if(substring(input$indicador_CE_pu,1,5) == "Distr" & input$corte2 == "Total"){
           # Gráficos de barras para totales de indicadores ENAJ

           dat <- base_CE_pu() %>% filter(NOMINDICADOR == input$indicador_CE_pu & input$corte2 == "Total")
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_CE_pu])))
           cut_nom <- quo_name(cut)

           datatable(dat %>%
                       filter(NOMINDICADOR==input$indicador_CE_pu & CORTE1_rec == "Total") %>%
                       arrange(!!cut)%>%
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         !!cut_nom := !!cut,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs =
                                      list(list(className = 'dt-center',
                                                targets = "_all"))))

         } else if(substring(input$indicador_CE_pu,1,5) == "Distr" & input$corte2 != "Total") {


           dat <- base_CE_pu() %>% filter(NOMINDICADOR == input$indicador_CE_pu & CORTE1_rec == input$corte2)   %>%
             filter(FECHA %in% input$anio2)
           cut1 <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_CE_pu])))
           cut2 <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CE_pu&df$CORTE1_rec==input$corte2])))
           cut_nom1 <- quo_name(cut1)
           cut_nom2 <- quo_name(cut2)

           datatable(dat %>%
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         !!cut_nom1 := !!cut1,
                         !!cut_nom2 := !!cut2,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs =
                                      list(list(className = 'dt-center',
                                                targets = "_all"))))
           
           
           
           
         } else {          
           
           datatable(base_CE_pu() %>% 
                       filter(NOMINDICADOR==input$indicador_CE_pu&CORTE1_rec==input$corte2) %>%
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
         }
       }) 
       
       output$tabla_resultado_CE_pu_descarga <- downloadHandler(
         filename = function() {
           paste0(input$indicador_CE_pu, ".xlsx", sep = "")
         },
         content = function(file) {  
           
           if(substring(input$indicador_CE_pu,1,5) != "Distr" & input$corte2 == "Total" & substring(input$indicador_CE_pu,1,4) != "Tasa"){
             
             dat <- base_CE_pu() %>% filter(NOMINDICADOR == input$indicador_CE_pu & is.na(CORTE1) == T)
             
             openxlsx::write.xlsx(list( "Data" = dat %>%
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            "Valor" = VALOR),
                                        "Metadata" = base_CE_pu() %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
             
           } else if(substring(input$indicador_CE_pu,1,5) != "Distr" & input$corte2 != "Total" & substring(input$indicador_CE_pu,1,4) != "Tasa") {
             
             dat <- base_CE_pu() %>% filter(NOMINDICADOR == input$indicador_CE_pu & CORTE1_rec == input$corte2 & substring(input$indicador_CE_pu,1,4) != "Tasa")
             cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CE_pu&df$CORTE1_rec==input$corte2])))
             cut_nom <- quo_name(cut)
             
             openxlsx::write.xlsx(list( "Data" = dat %>%
                                          arrange(!!cut)%>%
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            !!cut_nom := !!cut,
                                            "Valor" = VALOR),
                                        "Metadata" = dat %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
             
           } else if(substring(input$indicador_CE_pu,1,5) == "Distr" & input$corte2 == "Total"){
             
             dat <- base_CE_pu() %>% filter(NOMINDICADOR == input$indicador_CE_pu & CORTE1_rec == "Total") 
             cut <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_CE_pu])))
             cut_nom <- quo_name(cut)
             
             openxlsx::write.xlsx(list( "Data" = dat %>%
                                          arrange(!!cut)%>%
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            !!cut_nom := !!cut,
                                            "Valor" = VALOR),
                                        "Metadata" = dat %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
           }           else if(substring(input$indicador_CE_pu,1,5) == "Distr" & input$corte2 != "Total") {
             
             
             dat <- base_CE_pu() %>% filter(NOMINDICADOR == input$indicador_CE_pu & CORTE1_rec == input$corte2)   %>%
               filter(FECHA %in% input$anio1)
             cut1 <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_CE_pu])))
             cut2 <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CE_pu&df$CORTE1_rec==input$corte2])))
             cut_nom1 <- quo_name(cut1)
             cut_nom2 <- quo_name(cut2)
             
             openxlsx::write.xlsx(list( "Data" = dat %>%
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            !!cut_nom1 := !!cut1,
                                            !!cut_nom2 := !!cut2,
                                            "Valor" = VALOR),
                                        "Metadata" = dat %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
             
           }
         })
       
       
#######Cuidados y educación. Total país       
       

       
##Gráficos
       
       output$plot_CUID_tp <- renderPlot({
         
         
         if(input$corte4 == "Total" & substring(input$indicador_CUID_tp, 1, 32)=="Tasa de participación de varones"){
           
           dat <- base_CUID_tp() %>% filter(NOMINDICADOR == input$indicador_CUID_tp & is.na(CORTE1) == T) %>%
                       arrange(`Sexo del jefe/a`)%>%
                       arrange(FECHA)           
           g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), alpha = metodo, 
                                    group = `Sexo del jefe/a`, color = `Sexo del jefe/a`)) +
             geom_line(dat = dat %>% filter(FECHA <= 2019),
                       size = 1, alpha = 0.4) +
             geom_line(dat = dat %>% filter(FECHA >= 2022),
                       size = 1, alpha = 0.8)  +
             geom_point(size = 2, show.legend = FALSE)  +
              theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 14),
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA)) +
               scale_alpha_manual(values = c(.4, .8)) +
              scale_x_date(date_breaks = "3 years",date_labels  = "%Y") +
              labs(
                color = "",
                alpha = "",
                title = input$indicador_CUID_tp,
                caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                            filter(NOMINDICADOR == input$indicador_CUID_tp) %>% pull(FUENTE))), width = 160),
                x = "",
                y = "") +
                scale_color_brewer(name = "", palette = paleta)  
              
           
         } else if(input$corte4 != "Total" & substring(input$indicador_CUID_tp, 1, 32)=="Tasa de participación de varones") {
           
           # Gráficos de línea por sexo y panel para cortes 
           
           dat <- base_CUID_tp() %>% filter(NOMINDICADOR == input$indicador_CUID_tp & CORTE1_rec == input$corte4)
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CUID_tp&df$CORTE1_rec==input$corte4])))
           
           g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), alpha = metodo, 
                                    group = !!cut, 
                                    color = !!cut)) +
             geom_line(dat = dat %>% filter(FECHA <= 2019),
                       size = 1, alpha = 0.4) +
             geom_line(dat = dat %>% filter(FECHA >= 2022),
                       size = 1, alpha = 0.8) +
             geom_point(size = 2, show.legend = FALSE) +
             facet_wrap(~ `Sexo del jefe/a`) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12), 
                   axis.title = element_text(size = 14), 
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_alpha_manual(values = c(.4, .8)) +
             scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CUID_tp,
               caption = str_wrap(paste("Fuente:",unique(metadata %>% 
                                                           filter(NOMINDICADOR == input$indicador_CUID_tp) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +
             scale_color_brewer(name = "", palette = paleta)  
           
           
           
         } else if(substring(input$indicador_CUID_tp,1,23)=="Tasa de asistencia a la" & input$corte4 == "Total"){
           
           ##Línea común
           
           dat <- base_CUID_tp() %>% filter(NOMINDICADOR == input$indicador_CUID_tp & is.na(CORTE1) == T)
           g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), alpha = metodo, color = NOMINDICADOR)) +
             geom_line(dat = dat %>% filter(FECHA <= 2019),
                       size = 1, alpha = 0.4) +
             geom_line(dat = dat %>% filter(FECHA >= 2022),
                       size = 1, alpha = 0.8) +
             geom_point(size = 2, show.legend = FALSE) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12), 
                   axis.title = element_text(size = 14), 
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_alpha_manual(values = c(.4, .8)) +
             scale_color_manual(
               values = c(color_defecto)) +
             scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CUID_tp,
               caption = str_wrap(paste("Fuente:",unique(metadata %>% 
                                                           filter(NOMINDICADOR == input$indicador_CUID_tp) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "")
           
         } else if(input$corte4 != "Total" & substring(input$indicador_CUID_tp,1,23)=="Tasa de asistencia a la") {
           
           #Línea común por corte
           
           dat <- base_CUID_tp() %>% filter(NOMINDICADOR == input$indicador_CUID_tp & CORTE1_rec == input$corte4)
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CUID_tp&df$CORTE1_rec==input$corte4])))
           
           g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), alpha = metodo, 
                                    group = !!cut, 
                                    color = !!cut)) +
             geom_line(dat = dat %>% filter(FECHA <= 2019),
                       size = 1, alpha = 0.4) +
             geom_line(dat = dat %>% filter(FECHA >= 2022),
                       size = 1, alpha = 0.8) +
             geom_point(size = 2, show.legend = FALSE) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12), 
                   axis.title = element_text(size = 14), 
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_alpha_manual(values = c(.4, .8)) +
             scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CUID_tp,
               caption = str_wrap(paste("Fuente:",unique(metadata %>% 
                                                           filter(NOMINDICADOR == input$indicador_CUID_tp) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +
             
             scale_color_brewer(name = "", palette = paleta)  
           
           
         } else {
           
         }
         
           # Gráficos de línea por sexo y panel para cortes 
         print(g1)
         
         
         
       })
       
              
       
### DESCARGA DE GRÁFICOS
       
       output$graficos_resultado_CUID_tp_descarga <- downloadHandler(
         filename <- function() {
           paste("g1", "png", sep = ".")
         },
         
         content <- function(file) {
           file.copy("www/g1.png", file)
         },
         contentType = "www/g1"
       )
       
### TABLAS
       
       output$tabla_resultado_CUID_tp <- renderDT({
         
         
         
         if(input$corte4 == "Total" & substring(input$indicador_CUID_tp, 1, 32)=="Tasa de participación de varones"){
           
           dat <- base_CUID_tp() %>% filter(NOMINDICADOR == input$indicador_CUID_tp & is.na(CORTE1) == T)
           
           datatable(dat  %>% 
                       arrange(`Sexo del jefe/a`)%>%
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         "Sexo" = `Sexo del jefe/a`,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
         
         } else if(input$corte4 != "Total" & substring(input$indicador_CUID_tp, 1, 32)=="Tasa de participación de varones") {
           
           # Gráficos de línea por sexo y panel para cortes 
           
           dat <- base_CUID_tp() %>% filter(NOMINDICADOR == input$indicador_CUID_tp & CORTE1_rec == input$corte4)
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CUID_tp&df$CORTE1_rec==input$corte4])))
           cut_nom <- quo_name(cut)
           
           datatable(dat  %>% 
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         "Sexo" = `Sexo del jefe/a`,
                         !!cut_nom := !!cut,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
         } else if(substring(input$indicador_CUID_tp,1,23)=="Tasa de asistencia a la" & input$corte4 == "Total"){
           
           ##Línea común
           
           dat <- base_CUID_tp() %>% filter(NOMINDICADOR == input$indicador_CUID_tp & is.na(CORTE1) == T)
           
           datatable(dat  %>% 
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
           
         } else if(input$corte4 != "Total" & substring(input$indicador_CUID_tp,1,23)=="Tasa de asistencia a la") {
           
           #Línea común por corte
           
           dat <- base_CUID_tp() %>% filter(NOMINDICADOR == input$indicador_CUID_tp & CORTE1_rec == input$corte4)
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CUID_tp&df$CORTE1_rec==input$corte4])))
           cut_nom <- quo_name(cut)
           
           datatable(dat  %>% 
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         !!cut_nom := !!cut,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
           
         } else {
             
           }
       
       })      

       
### DESCARGA DE TABLAS       
       output$tabla_resultado_CUID_tp_descarga <- downloadHandler(
         filename = function() {
           paste0(input$indicador_CUID_tp, ".xlsx", sep = "")
         },
         content = function(file) {  
           
           # Gráficos de línea por sexo
           
           if(input$corte4 == "Total" & substring(input$indicador_CUID_tp, 1, 32)=="Tasa de participación de varones"){
             
             dat <- base_CUID_tp() %>% filter(NOMINDICADOR == input$indicador_CUID_tp & is.na(CORTE1) == T)
             
             
             
             openxlsx::write.xlsx(list( "Data" = dat  %>% 
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            "Sexo" = `Sexo del jefe/a`,
                                            "Valor" = VALOR),
                                        "Metadata" = base_CUID_tp() %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
             # Gráficos de línea por sexo y panel para cortes 
             
           } else if(input$corte4 != "Total" & substring(input$indicador_CUID_tp, 1, 32)=="Tasa de participación de varones") {
             
             
             dat <- base_CUID_tp() %>% filter(NOMINDICADOR == input$indicador_CUID_tp & CORTE1_rec == input$corte4)
             cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CUID_tp&df$CORTE1_rec==input$corte4])))
             cut_nom <- quo_name(cut)
             
             
             
             openxlsx::write.xlsx(list( "Data" = dat  %>% 
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            "Sexo" = `Sexo del jefe/a`,
                                            !!cut_nom := !!cut,
                                            "Valor" = VALOR),
                                        "Metadata" = base_CUID_tp() %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
           } else if(substring(input$indicador_CUID_tp,1,23)=="Tasa de asistencia a la" & input$corte4 == "Total"){
             
             ##Línea común
             
             dat <- base_CUID_tp() %>% filter(NOMINDICADOR == input$indicador_CUID_tp & is.na(CORTE1) == T)
             
             openxlsx::write.xlsx(list( "Data" = dat  %>% 
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            "Valor" = VALOR),
                                        "Metadata" = dat %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
             
           } else if(input$corte4 != "Total" & substring(input$indicador_CUID_tp,1,23)=="Tasa de asistencia a la") {
             
             #Línea común por corte
             
             dat <- base_CUID_tp() %>% filter(NOMINDICADOR == input$indicador_CUID_tp & CORTE1_rec == input$corte4)
             cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CUID_tp&df$CORTE1_rec==input$corte4])))
             cut_nom <- quo_name(cut)
             
             openxlsx::write.xlsx(list( "Data" = dat  %>% 
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            !!cut_nom := !!cut,
                                            "Valor" = VALOR), 
                                        "Metadata" = dat %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
           } 
           
           
         })  
       
       
       
       
       
#######Cuidados y educación. País urbano       
       
       
       ##Gráficos
       
       output$plot_CUID_pu <- renderPlot({
         
         
         if(input$corte5 == "Total" & substring(input$indicador_CUID_pu, 1, 32)=="Tasa de participación de varones"){
           
           dat <- base_CUID_pu() %>% filter(NOMINDICADOR == input$indicador_CUID_pu & CORTE1 == "Total") %>%
             arrange(Sexo)%>%
             arrange(FECHA)           
           g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), alpha = metodo, 
                                    group = Sexo, color = Sexo)) +
             geom_line(dat = dat %>% filter(FECHA <= 2019),
                       size = 1, alpha = 0.4) +
             geom_line(dat = dat %>% filter(FECHA >= 2022),
                       size = 1, alpha = 0.8)  +
             geom_point(size = 2, show.legend = FALSE)  +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 14),
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA)) +
             scale_alpha_manual(values = c(.4, .8)) +
             scale_x_date(date_breaks = "3 years",date_labels  = "%Y") +
           labs(
             color = "",
             alpha = "",
             title = input$indicador_CUID_pu,
             caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                         filter(NOMINDICADOR == input$indicador_CUID_pu) %>% pull(FUENTE))), width = 160),
             x = "",
             y = "") +
             scale_color_brewer(name = "", palette = paleta)  
           
           
         } else if(input$corte5 != "Total" & substring(input$indicador_CUID_pu, 1, 32)=="Tasa de participación de varones") {
           
           # Gráficos de línea por sexo y panel para cortes 
           
           dat <- base_CUID_pu() %>% filter(NOMINDICADOR == input$indicador_CUID_pu & CORTE1_rec == input$corte5)
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CUID_pu&df$CORTE1_rec==input$corte5])))
           
           g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), alpha = metodo, 
                                    group = !!cut, 
                                    color = !!cut)) +
             geom_line(dat = dat %>% filter(FECHA <= 2019),
                       size = 1, alpha = 0.4) +
             geom_line(dat = dat %>% filter(FECHA >= 2022),
                       size = 1, alpha = 0.8) +
             geom_point(size = 2, show.legend = FALSE) +
             facet_wrap(~ Sexo) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12), 
                   axis.title = element_text(size = 14), 
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_alpha_manual(values = c(.4, .8)) +
             scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CUID_pu,
               caption = str_wrap(paste("Fuente:",unique(metadata %>% 
                                                           filter(NOMINDICADOR == input$indicador_CUID_pu) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +
             scale_color_brewer(name = "", palette = paleta)  
           
           
           
         } else if(substring(input$indicador_CUID_pu,1,23)=="Tasa de asistencia a la" & input$corte5 == "Total"){
           
           ##Línea común
           
           dat <- base_CUID_pu() %>% filter(NOMINDICADOR == input$indicador_CUID_pu & CORTE1 == "Total")
           g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), alpha = metodo, color = NOMINDICADOR)) +
             geom_line(dat = dat %>% filter(FECHA <= 2019),
                       size = 1, alpha = 0.4) +
             geom_line(dat = dat %>% filter(FECHA >= 2022),
                       size = 1, alpha = 0.8) +
             geom_point(size = 2, show.legend = FALSE) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12), 
                   axis.title = element_text(size = 14), 
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_alpha_manual(values = c(.4, .8)) +
             scale_color_manual(
               values = c(color_defecto)) +
             scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CUID_pu,
               caption = str_wrap(paste("Fuente:",unique(metadata %>% 
                                                           filter(NOMINDICADOR == input$indicador_CUID_pu) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "")
           
         } else if(input$corte5 != "Total" & substring(input$indicador_CUID_pu,1,23)=="Tasa de asistencia a la") {
           
           #Línea común por corte
           
           dat <- base_CUID_pu() %>% filter(NOMINDICADOR == input$indicador_CUID_pu & CORTE1_rec == input$corte5)
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CUID_pu&df$CORTE1_rec==input$corte5])))
           
           g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), alpha = metodo, 
                                    group = !!cut, 
                                    color = !!cut)) +
             geom_line(dat = dat %>% filter(FECHA <= 2019),
                       size = 1, alpha = 0.4) +
             geom_line(dat = dat %>% filter(FECHA >= 2022),
                       size = 1, alpha = 0.8) +
             geom_point(size = 2, show.legend = FALSE) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12), 
                   axis.title = element_text(size = 14), 
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_alpha_manual(values = c(.4, .8)) +
             scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CUID_pu,
               caption = str_wrap(paste("Fuente:",unique(metadata %>% 
                                                           filter(NOMINDICADOR == input$indicador_CUID_pu) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +
             
             scale_color_brewer(name = "", palette = paleta)  
           
         } else if((input$indicador_CUID_pu=="Participación de personas en el cuidado de niños/as fuera del horario escolar"|
                    input$indicador_CUID_pu=="Participación de personas en el cuidado de niños/as cuando enferman"|
                    input$indicador_CUID_pu=="Tasa de participación en las actividades de cuidados dentro del hogar"|
                    input$indicador_CUID_pu=="Promedio de horas en las actividades de cuidados dentro del hogar")
                   & input$corte5 == "Total"){
           
           
           
           # Gráficos de barras por año
           
           dat <- base_CUID_pu() %>% filter(NOMINDICADOR == input$indicador_CUID_pu & CORTE1_rec == "Total")
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_CUID_pu])))
           
           g1 <- dat %>% ggplot(aes(x = FECHA,
                                    y = as.numeric(VALOR),
                                    fill = !!cut)) +
             geom_bar(size = 1, stat = "identity", position = position_dodge(width = 1), width = 0.8) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 14),
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_x_continuous(breaks = c(2013, 2018)) +
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CUID_pu,
               caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                           filter(NOMINDICADOR == input$indicador_CUID_pu) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +
             scale_fill_brewer(name = "", palette = paleta)
           
         } else if((input$indicador_CUID_pu=="Participación de personas en el cuidado de niños/as fuera del horario escolar"|
                    input$indicador_CUID_pu=="Participación de personas en el cuidado de niños/as cuando enferman"|
                    input$indicador_CUID_pu=="Tasa de participación en las actividades de cuidados dentro del hogar"|
                    input$indicador_CUID_pu=="Promedio de horas en las actividades de cuidados dentro del hogar")
                   & input$corte5 != "Total"){
           
           
           # Gráficos de barras por año y corte
           
           dat <- base_CUID_pu() %>% filter(NOMINDICADOR == input$indicador_CUID_pu & CORTE1_rec == input$corte5)
           cut1 <- rlang::sym(rlang::as_string(unique(dat$CORTE1_rec[dat$NOMINDICADOR ==input$indicador_CUID_pu])))
           cut2 <- rlang::sym(rlang::as_string(unique(dat$CORTE2_rec[dat$NOMINDICADOR ==input$indicador_CUID_pu])))
           
           g1 <- dat %>% ggplot(aes(x = FECHA,
                                    y = as.numeric(VALOR),
                                    fill = !!cut2)) +
             geom_bar(size = 1, stat = "identity", position = position_dodge(width = 1), width = 0.8) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 14),
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             facet_wrap(vars(!!cut1)) +
             scale_x_continuous(breaks = c(2013, 2018)) +
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CUID_pu,
               caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                           filter(NOMINDICADOR == input$indicador_CUID_pu) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +
             scale_fill_brewer(name = "", palette = paleta)           
           
           
         } else if(input$indicador_CUID_pu=="Porcentaje de niños/as que asisten a centros de cuidado infantil"
                   & input$corte5 == "Total"){
           
           
           # Gráficos de barras por año
           
           dat <- df %>% filter(NOMINDICADOR == "Porcentaje de niños/as que asisten a centros de cuidado infantil" & CORTE1_rec == "Total")
           
           g1 <- dat %>% ggplot(aes(x = FECHA,
                                    y = as.numeric(VALOR),
                                    fill = NOMINDICADOR)) +
             geom_bar(size = 1, stat = "identity", position = position_dodge(width = 1), width = 0.8) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 14),
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_x_continuous(breaks = c(2013, 2018)) +
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CUID_pu,
               caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                           filter(NOMINDICADOR == input$indicador_CUID_pu) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +
             scale_fill_brewer(name = "", palette = paleta)
           
           
         } else if(input$indicador_CUID_pu=="Porcentaje de niños/as que asisten a centros de cuidado infantil"
          & input$corte5 != "Total"){
  
  
  # Gráficos de barras por año
  
  dat <- df %>% filter(NOMINDICADOR == "Porcentaje de niños/as que asisten a centros de cuidado infantil" & CORTE1_rec == input$corte5)

  cut1 <- rlang::sym(rlang::as_string(unique(dat$CORTE1_rec[dat$NOMINDICADOR =="Porcentaje de niños/as que asisten a centros de cuidado infantil"])))
  cut_nom1 <- quo_name(cut1)
  
  
  g1 <- dat %>% ggplot(aes(x = FECHA,
                           y = as.numeric(VALOR),
                           fill = !!cut1)) +
    geom_bar(size = 1, stat = "identity", position = position_dodge(width = 1), width = 0.8) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA))+
    scale_x_continuous(breaks = c(2013, 2018)) +
    labs(
      color = "",
      alpha = "",
      title = input$indicador_CUID_pu,
      caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                  filter(NOMINDICADOR == input$indicador_CUID_pu) %>% pull(FUENTE))), width = 160),
      x = "",
      y = "") +
    scale_fill_brewer(name = "", palette = paleta)

  
  
         } else if(input$indicador_CUID_pu=="Porcentaje de personas según beneficios en el trabajo para la crianza"){
           
           
           # Gráficos de barras por año
           
           dat <- base_CUID_pu() %>% filter(NOMINDICADOR == "Porcentaje de personas según beneficios en el trabajo para la crianza")%>%
             arrange(VALOR)
           
           dat$`Beneficios laborales` <- factor(dat$`Beneficios laborales`, levels = unique(dat$`Beneficios laborales`)[order(dat$VALOR)])
           g1 <- dat %>% ggplot(aes(x = FECHA,
                                    y = as.numeric(VALOR),
                                    fill = `Beneficios laborales`)) +
             geom_bar(size = 1, stat = "identity", position = position_dodge(width = 1), width = 0.8) +
             scale_fill_viridis(discrete = TRUE, option = "D") +  
             facet_wrap(~`Perceptor de beneficios`)+
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 14),
                   legend.text = element_text(size = 6),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_x_continuous(breaks = c(2018)) +
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CUID_pu,
               caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                           filter(NOMINDICADOR == input$indicador_CUID_pu) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") 
           # +
           #   scale_fill_brewer(name = "", palette = paleta2)
           
               
         } else {
           
         }
         
         # Gráficos de línea por sexo y panel para cortes 
         print(g1)
         
         
         
       })
       
       
       
       ### DESCARGA DE GRÁFICOS
       
       output$graficos_resultado_CUID_pu_descarga <- downloadHandler(
         filename <- function() {
           paste("g1", "png", sep = ".")
         },
         
         content <- function(file) {
           file.copy("www/g1.png", file)
         },
         contentType = "www/g1"
       )
       
       ### TABLAS
       
       output$tabla_resultado_CUID_pu <- renderDT({
         
         
         
         if(input$corte5 == "Total" & substring(input$indicador_CUID_pu, 1, 32)=="Tasa de participación de varones"){
           
           dat <- base_CUID_pu() %>% filter(NOMINDICADOR == input$indicador_CUID_pu & CORTE1 == "Total")
           
           datatable(dat  %>% 
                       arrange(Sexo)%>%
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         "Sexo" = Sexo,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
           
         } else if(input$corte5 != "Total" & substring(input$indicador_CUID_pu, 1, 32)=="Tasa de participación de varones") {
           
           # Gráficos de línea por sexo y panel para cortes 
           
           dat <- base_CUID_pu() %>% filter(NOMINDICADOR == input$indicador_CUID_pu & CORTE1_rec == input$corte5)
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CUID_pu&df$CORTE1_rec==input$corte5])))
           cut_nom <- quo_name(cut)
           
           datatable(dat  %>% 
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         "Sexo" = Sexo,
                         !!cut_nom := !!cut,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
         } else if(substring(input$indicador_CUID_pu,1,23)=="Tasa de asistencia a la" & input$corte5 == "Total"){
           
           ##Línea común
           
           dat <- base_CUID_pu() %>% filter(NOMINDICADOR == input$indicador_CUID_pu & CORTE1 == "Total")
           
           datatable(dat  %>% 
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
           
         } else if(input$corte5 != "Total" & substring(input$indicador_CUID_pu,1,23)=="Tasa de asistencia a la") {
           
           #Línea común por corte
           
           dat <- base_CUID_pu() %>% filter(NOMINDICADOR == input$indicador_CUID_pu & CORTE1_rec == input$corte5)
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CUID_pu&df$CORTE1_rec==input$corte5])))
           cut_nom <- quo_name(cut)
           
           datatable(dat  %>% 
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         !!cut_nom := !!cut,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
           

         } else if((input$indicador_CUID_pu=="Participación de personas en el cuidado de niños/as fuera del horario escolar"|
                    input$indicador_CUID_pu=="Participación de personas en el cuidado de niños/as cuando enferman"|
                    input$indicador_CUID_pu=="Tasa de participación en las actividades de cuidados dentro del hogar"|
                    input$indicador_CUID_pu=="Promedio de horas en las actividades de cuidados dentro del hogar")
                   & input$corte5 == "Total"){
           
           
           
           # Gráficos de barras por año
           
           dat <- base_CUID_pu() %>% filter(NOMINDICADOR == input$indicador_CUID_pu & CORTE1_rec == "Total")
           cut <- rlang::sym(rlang::as_string(unique(dat$CORTE2_rec[dat$NOMINDICADOR ==input$indicador_CUID_pu])))
           cut_nom <- quo_name(cut)
           
           datatable(dat  %>% 
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         !!cut_nom := !!cut,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
           
             } else if((input$indicador_CUID_pu=="Participación de personas en el cuidado de niños/as fuera del horario escolar"|
                    input$indicador_CUID_pu=="Participación de personas en el cuidado de niños/as cuando enferman"|
                    input$indicador_CUID_pu=="Tasa de participación en las actividades de cuidados dentro del hogar"|
                    input$indicador_CUID_pu=="Promedio de horas en las actividades de cuidados dentro del hogar")
                   & input$corte5 != "Total"){
           
           
           # Gráficos de barras por año y corte
           
           dat <- base_CUID_pu() %>% filter(NOMINDICADOR == input$indicador_CUID_pu & CORTE1_rec == input$corte5)
           cut1 <- rlang::sym(rlang::as_string(unique(dat$CORTE1_rec[dat$NOMINDICADOR ==input$indicador_CUID_pu])))
           cut2 <- rlang::sym(rlang::as_string(unique(dat$CORTE2_rec[dat$NOMINDICADOR ==input$indicador_CUID_pu])))
           cut_nom1 <- quo_name(cut1)
           cut_nom2 <- quo_name(cut2)
           
           datatable(dat  %>% 
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         !!cut_nom2 := !!cut2,
                         !!cut_nom1 := !!cut1,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))

           
           
             } else if(input$indicador_CUID_pu=="Porcentaje de niños/as que asisten a centros de cuidado infantil"
                       & input$corte5 == "Total"){
               
               
               # Gráficos de barras por año
               
               dat <- df %>% filter(NOMINDICADOR == "Porcentaje de niños/as que asisten a centros de cuidado infantil" & CORTE1_rec == "Total")
               
                 datatable(dat  %>% 
                             arrange(FECHA)%>%
                             transmute(
                               "Año" = FECHA,
                               "Valor" = VALOR),
                           rownames = FALSE,
                           options = list(columnDefs = 
                                            list(list(className = 'dt-center', 
                                                      targets = "_all"))))
               
               
             } else if(input$indicador_CUID_pu=="Porcentaje de niños/as que asisten a centros de cuidado infantil"
                       & input$corte5 != "Total"){
               
               
               # Gráficos de barras por año
               
               dat <- df %>% filter(NOMINDICADOR == "Porcentaje de niños/as que asisten a centros de cuidado infantil" & CORTE1_rec == input$corte5)
               cut1 <- rlang::sym(rlang::as_string(unique(dat$CORTE1_rec[dat$NOMINDICADOR =="Porcentaje de niños/as que asisten a centros de cuidado infantil"])))
               cut_nom1 <- quo_name(cut1)
               
                 datatable(dat  %>% 
                             arrange(FECHA)%>%
                             transmute(
                               "Año" = FECHA,
                               !!cut_nom1 := !!cut1,
                               "Valor" = VALOR),
                           rownames = FALSE,
                           options = list(columnDefs = 
                                            list(list(className = 'dt-center', 
                                                      targets = "_all"))))
               
             } else if(input$indicador_CUID_pu=="Porcentaje de personas según beneficios en el trabajo para la crianza"){
               
               
               # Gráficos de barras por año
               
               dat <- base_CUID_pu() %>% filter(NOMINDICADOR == "Porcentaje de personas según beneficios en el trabajo para la crianza")%>%
                 arrange(VALOR)
               
               dat$`Beneficios laborales` <- factor(dat$`Beneficios laborales`, levels = unique(dat$`Beneficios laborales`)[order(dat$VALOR)])

                 datatable(dat  %>% 
                             arrange(FECHA)%>%
                             arrange(`Perceptor de beneficios`)%>%
                             arrange(`Beneficios laborales`)%>%
                             transmute(
                               "Año" = FECHA,
                               "Perceptor beneficios" = `Perceptor de beneficios`,
                               "Beneficios laborales" = `Beneficios laborales`,
                               "Valor" = VALOR),
                           rownames = FALSE,
                           options = list(columnDefs = 
                                            list(list(className = 'dt-center', 
                                                      targets = "_all"))))
               
           
             } else {
           
         }
         
       })      
       
       
       ### DESCARGA DE TABLAS       
       output$tabla_resultado_CUID_pu_descarga <- downloadHandler(
         filename = function() {
           paste0(input$indicador_CUID_pu, ".xlsx", sep = "")
         },
         content = function(file) {  
           
           # Gráficos de línea por sexo
           
           if(input$corte5 == "Total" & substring(input$indicador_CUID_pu, 1, 32)=="Tasa de participación de varones"){
             
             dat <- base_CUID_pu() %>% filter(NOMINDICADOR == input$indicador_CUID_pu & CORTE1 == "Total")
             
             
             
             openxlsx::write.xlsx(list( "Data" = dat  %>% 
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            "Sexo" = Sexo,
                                            "Valor" = VALOR),
                                        "Metadata" = base_CUID_pu() %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
             # Gráficos de línea por sexo y panel para cortes 
             
           } else if(input$corte5 != "Total" & substring(input$indicador_CUID_pu, 1, 32)=="Tasa de participación de varones") {
             
             
             dat <- base_CUID_pu() %>% filter(NOMINDICADOR == input$indicador_CUID_pu & CORTE1_rec == input$corte5)
             cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CUID_pu&df$CORTE1_rec==input$corte5])))
             cut_nom <- quo_name(cut)
             
             
             
             openxlsx::write.xlsx(list( "Data" = dat  %>% 
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            "Sexo" = Sexo,
                                            !!cut_nom := !!cut,
                                            "Valor" = VALOR),
                                        "Metadata" = base_CUID_pu() %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
           } else if(substring(input$indicador_CUID_pu,1,23)=="Tasa de asistencia a la" & input$corte5 == "Total"){
             
             ##Línea común
             
             dat <- base_CUID_pu() %>% filter(NOMINDICADOR == input$indicador_CUID_pu & CORTE1 == "Total")
             
             openxlsx::write.xlsx(list( "Data" = dat  %>% 
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            "Valor" = VALOR),
                                        "Metadata" = dat %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
             
           } else if(input$corte5 != "Total" & substring(input$indicador_CUID_pu,1,23)=="Tasa de asistencia a la") {
             
             #Línea común por corte
             
             dat <- base_CUID_pu() %>% filter(NOMINDICADOR == input$indicador_CUID_pu & CORTE1_rec == input$corte5)
             cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CUID_pu&df$CORTE1_rec==input$corte5])))
             cut_nom <- quo_name(cut)
             
             openxlsx::write.xlsx(list( "Data" = dat  %>% 
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            "Sexo" = Sexo,
                                            !!cut_nom := !!cut,
                                            "Valor" = VALOR), 
                                        "Metadata" = dat %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
           } else if((input$indicador_CUID_pu=="Participación de personas en el cuidado de niños/as fuera del horario escolar"|
                      input$indicador_CUID_pu=="Participación de personas en el cuidado de niños/as cuando enferman"|
                      input$indicador_CUID_pu=="Tasa de participación en las actividades de cuidados dentro del hogar"|
                      input$indicador_CUID_pu=="Promedio de horas en las actividades de cuidados dentro del hogar")
                     & input$corte5 == "Total"){
             
             
             
             # Gráficos de barras por año
             
             dat <- base_CUID_pu() %>% filter(NOMINDICADOR == input$indicador_CUID_pu & CORTE1_rec == "Total")
             cut <- rlang::sym(rlang::as_string(unique(dat$CORTE2_rec[dat$NOMINDICADOR ==input$indicador_CUID_pu])))
             cut_nom <- quo_name(cut)
             

             openxlsx::write.xlsx(list( "Data" = dat  %>% 
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            !!cut_nom := !!cut,
                                            "Valor" = VALOR),
                                        "Metadata" = base_CUID_pu() %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
             
           } else if((input$indicador_CUID_pu=="Participación de personas en el cuidado de niños/as fuera del horario escolar"|
                      input$indicador_CUID_pu=="Participación de personas en el cuidado de niños/as cuando enferman"|
                      input$indicador_CUID_pu=="Tasa de participación en las actividades de cuidados dentro del hogar"|
                      input$indicador_CUID_pu=="Promedio de horas en las actividades de cuidados dentro del hogar")
                     & input$corte5 != "Total"){
             
             
             # Gráficos de barras por año y corte
             
             dat <- base_CUID_pu() %>% filter(NOMINDICADOR == input$indicador_CUID_pu & CORTE1_rec == input$corte5)
             cut1 <- rlang::sym(rlang::as_string(unique(dat$CORTE1_rec[dat$NOMINDICADOR ==input$indicador_CUID_pu])))
             cut2 <- rlang::sym(rlang::as_string(unique(dat$CORTE2_rec[dat$NOMINDICADOR ==input$indicador_CUID_pu])))
             cut_nom1 <- quo_name(cut1)
             cut_nom2 <- quo_name(cut2)
             

             
             openxlsx::write.xlsx(list( "Data" = dat  %>% 
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            !!cut_nom2 := !!cut2,
                                            !!cut_nom1 := !!cut1,
                                            "Valor" = VALOR),
                                        "Metadata" = base_CUID_pu() %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
           } else if(input$indicador_CUID_pu=="Porcentaje de niños/as que asisten a centros de cuidado infantil"
                     & input$corte5 == "Total"){
             
             
             # Gráficos de barras por año
             
             dat <- df %>% filter(NOMINDICADOR == "Porcentaje de niños/as que asisten a centros de cuidado infantil" & CORTE1_rec == "Total")
             
                       openxlsx::write.xlsx(list( "Data" = dat  %>% 
                                                    arrange(FECHA)%>%
                                                    transmute(
                                                      "Año" = FECHA,
                                                      "Valor" = VALOR),
                                                  "Metadata" = base_CUID_pu() %>%
                                                    dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                                    transmute(
                                                      "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                                      "FUENTE" = FUENTE,
                                                      "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                            file,
                                            rowNames = TRUE)
                       
                       
             
           } else if(input$indicador_CUID_pu=="Porcentaje de niños/as que asisten a centros de cuidado infantil"
                     & input$corte5 != "Total"){
             
             
             # Gráficos de barras por año
             
             dat <- df %>% filter(NOMINDICADOR == "Porcentaje de niños/as que asisten a centros de cuidado infantil" & CORTE1_rec == input$corte5)
             cut1 <- rlang::sym(rlang::as_string(unique(dat$CORTE1_rec[dat$NOMINDICADOR =="Porcentaje de niños/as que asisten a centros de cuidado infantil"])))
             cut_nom1 <- quo_name(cut1)
             
                       
                       openxlsx::write.xlsx(list( "Data" = dat  %>% 
                                                    arrange(FECHA)%>%
                                                    transmute(
                                                      "Año" = FECHA,
                                                      !!cut_nom1 := !!cut1,
                                                      "Valor" = VALOR),
                                                  "Metadata" = base_CUID_pu() %>%
                                                    dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                                    transmute(
                                                      "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                                      "FUENTE" = FUENTE,
                                                      "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                            file,
                                            rowNames = TRUE)
                       
                       
           } else {
             
           }
           
           
         })  
       
       
       
       
       ####Violencia de género
       
       ### Gráficos
       
       output$plot_VG <- renderPlot({
         
         if (input$corte6 == "Total") {
           
           dat <- base_VG() %>% filter(NOMINDICADOR == input$indicador_VG & input$corte6 == "Total")
           
           cut2 <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_VG])))
           
           g1 <- dat %>% ggplot(aes(x = FECHA,
                                    y = as.numeric(VALOR),
                                    fill = !!cut2)) +
             geom_bar(size = 1, stat = "identity", position = position_dodge(width = 1), width = 0.8) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 14),
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_x_continuous(breaks = c(2013,2019)) +
             labs(
               color = "",
               alpha = "",
               title = input$indicador_VG_pu,
               caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                           filter(NOMINDICADOR == input$indicador_CUID_pu) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +
             scale_fill_brewer(name = "", palette = paleta)           
           
         } else if (input$corte6 != "Total") {
           
           
           dat <- base_VG() %>% filter(NOMINDICADOR == input$indicador_VG & CORTE1_rec == input$corte6)%>%
             filter(FECHA %in% input$anio3)
           
           cut1 <- rlang::sym(rlang::as_string(unique(dat$CORTE1_rec[dat$NOMINDICADOR ==input$indicador_VG])))
           cut2 <- rlang::sym(rlang::as_string(unique(dat$CORTE2_rec[dat$NOMINDICADOR ==input$indicador_VG])))
           
           g1 <- dat %>% ggplot(aes(x = !!cut1,
                                    y = as.numeric(VALOR),
                                    fill = !!cut2)) +
             geom_bar(size = 1, stat = "identity", position = position_dodge(width = 1), width = 0.8) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 14),
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA)) +
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CUID_pu,
               caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                           filter(NOMINDICADOR == input$indicador_CUID_pu) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +
             scale_fill_brewer(name = "", palette = paleta)           
           
         } else {
           
         }
         
         print(g1)
         
         
         
       })  
       
       ### DESCARGA DE GRÁFICOS
       
       output$graficos_resultado_VG_descarga <- downloadHandler(
         filename <- function() {
           paste("g1", "png", sep = ".")
         },
         
         content <- function(file) {
           file.copy("www/g1.png", file)
         },
         contentType = "www/g1"
       )
       
       
       
       ### TABLAS
       
       output$tabla_resultado_VG <- renderDT({
         
         
         
         if (input$corte6 == "Total") {
           
           dat <- base_VG() %>% filter(NOMINDICADOR == input$indicador_VG & input$corte6 == "Total")
           
           cut2 <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_VG])))
           cut_nom <- quo_name(cut2)
           
           
           datatable(dat  %>% 
                       arrange(!!cut2)%>%
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                        !!cut_nom := !!cut2,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
           
           
           
         } else if (input$corte6 != "Total") {
           
           
           dat <- base_VG() %>% filter(NOMINDICADOR == input$indicador_VG & CORTE1_rec == input$corte6)%>%
             filter(FECHA %in% input$anio3)
           
           cut1 <- rlang::sym(rlang::as_string(unique(dat$CORTE1_rec[dat$NOMINDICADOR ==input$indicador_VG])))
           cut2 <- rlang::sym(rlang::as_string(unique(dat$CORTE2_rec[dat$NOMINDICADOR ==input$indicador_VG])))
           cut_nom1 <- quo_name(cut1)
           cut_nom2 <- quo_name(cut2)
           
           
           datatable(dat  %>% 
                       arrange(!!cut1)%>%
                       arrange(!!cut2)%>%
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         !!cut_nom2 := !!cut2,
                         !!cut_nom1 := !!cut1,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
         } else {
           
         }
       })      
       
       
       ### DESCARGA DE TABLAS       
       output$tabla_resultado_VG_descarga <- downloadHandler(
         filename = function() {
           paste0(input$indicador_VG_pu, ".xlsx", sep = "")
         },
         content = function(file) {  
           
           if (input$corte6 == "Total") {
             
             dat <- base_VG() %>% filter(NOMINDICADOR == input$indicador_VG & CORTE1_rec == "Total")
             
             cut2 <- rlang::sym(rlang::as_string(unique(dat$CORTE2_rec[dat$NOMINDICADOR ==input$indicador_VG])))
             cut_nom <- quo_name(cut2)
             
             
             openxlsx::write.xlsx(list( "Data" = dat  %>% 
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            !!cut_nom := !!cut2,
                                            "Valor" = VALOR),
                                        "Metadata" = base_CUID_pu() %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
             
             
             
           } else if (input$corte6 != "Total") {
             
             
             dat <- base_VG() %>% filter(NOMINDICADOR == input$indicador_VG & CORTE1_rec == input$corte6)%>%
               filter(FECHA %in% input$anio3)
             
             cut1 <- rlang::sym(rlang::as_string(unique(dat$CORTE1_rec[dat$NOMINDICADOR ==input$indicador_VG])))
             cut2 <- rlang::sym(rlang::as_string(unique(dat$CORTE2_rec[dat$NOMINDICADOR ==input$indicador_VG])))
             cut_nom1 <- quo_name(cut1)
             cut_nom2 <- quo_name(cut2)
             
             
             openxlsx::write.xlsx(list( "Data" = dat  %>% 
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            !!cut_nom2 := !!cut2,
                                            !!cut_nom1 := !!cut1,
                                            "Valor" = VALOR),
                                        "Metadata" = base_CUID_pu() %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
             
             
           } else {
             
           }
           
           
           
         })               
       
       
       
       
       #######Salud infantil. Total país       
       
       
       
       ##Gráficos
       
       output$plot_SI_tp <- renderPlot({
         
         
         if(input$corte7 == "Total" & (substring(input$indicador_SI_tp, 1, 25)=="Porcentaje de niños entre"|
                                       substring(input$indicador_SI_tp, 1, 32)=="Porcentaje de niños menores de 5")) {
           
           dat <- base_SI_tp() %>% filter(NOMINDICADOR == input$indicador_SI_tp & is.na(CORTE1) == T) 

             g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), alpha = metodo, color = NOMINDICADOR)) +
             geom_line(dat = dat %>% filter(FECHA <= 2019),
                       size = 1, alpha = 0.4) +
             geom_line(dat = dat %>% filter(FECHA >= 2022),
                       size = 1, alpha = 0.8) +
             geom_point(size = 2, show.legend = FALSE) +
               theme_minimal() +
               theme(legend.position = "bottom",
                     legend.title = element_blank(),
                     axis.text = element_text(size = 12), 
                     axis.title = element_text(size = 14), 
                     legend.text = element_text(size = 12),
                     plot.background = element_rect(fill = "white", color = NA),
                     panel.background = element_rect(fill = "white", color = NA))+
               scale_alpha_manual(values = c(.4, .8)) +
               scale_color_manual(
                 values = c(color_defecto)) +
               scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
               labs(
             color = "",
             alpha = "",
             title = input$indicador_SI_tp,
             caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                         filter(NOMINDICADOR == input$indicador_SI_tp) %>% pull(FUENTE))), width = 160),
             x = "",
             y = "") +
             scale_color_brewer(name = "", palette = paleta)  
           
           
         } else if (input$corte7 == "Total" & substring(input$indicador_SI_tp, 1, 4)=="Tasa") {
           
           dat <- base_SI_tp() %>% filter(NOMINDICADOR == input$indicador_SI_tp & is.na(CORTE1) == T) 
           
           g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), color = NOMINDICADOR)) +
             geom_line(dat = dat, size = 1) +
             geom_point(size = 2, show.legend = FALSE) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12), 
                   axis.title = element_text(size = 14), 
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_alpha_manual(values = c(.4, .8)) +
             scale_color_manual(
               values = c(color_defecto)) +
             scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_SI_tp,
               caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                           filter(NOMINDICADOR == input$indicador_SI_tp) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +
             scale_color_brewer(name = "", palette = paleta)     
           
           
           
           
           
           
         } else if(input$corte7 != "Total" & (substring(input$indicador_SI_tp, 1, 25)=="Porcentaje de niños entre"|
                                              substring(input$indicador_SI_tp, 1, 32)=="Porcentaje de niños menores de 5")){
           
           
           # Gráficos de línea por sexo y panel para cortes 
           
           dat <- base_SI_tp() %>% filter(NOMINDICADOR == input$indicador_SI_tp & CORTE1_rec == input$corte7)
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_SI_tp&df$CORTE1_rec==input$corte7])))
           
           g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), alpha = metodo, 
                                    group = !!cut, 
                                    color = !!cut)) +
             geom_line(dat = dat %>% filter(FECHA <= 2019),
                       size = 1, alpha = 0.4) +
             geom_line(dat = dat %>% filter(FECHA >= 2022),
                       size = 1, alpha = 0.8) +
             geom_point(size = 2, show.legend = FALSE) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12), 
                   axis.title = element_text(size = 14), 
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_alpha_manual(values = c(.4, .8)) +
             scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_SI_tp,
               caption = str_wrap(paste("Fuente:",unique(metadata %>% 
                                                           filter(NOMINDICADOR == input$indicador_SI_tp) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +
             scale_color_brewer(name = "", palette = paleta)  
           
         } else {
           
         }
         
         # Gráficos de línea por sexo y panel para cortes 
         print(g1)
         
         
         
       })
       
       
       
       ### DESCARGA DE GRÁFICOS
       
       output$graficos_resultado_SI_tp_descarga <- downloadHandler(
         filename <- function() {
           paste("g1", "png", sep = ".")
         },
         
         content <- function(file) {
           file.copy("www/g1.png", file)
         },
         contentType = "www/g1"
       )
       
       ### TABLAS
       
       output$tabla_resultado_SI_tp <- renderDT({
         
         
         if(input$corte7 == "Total" & (substring(input$indicador_SI_tp, 1, 25)=="Porcentaje de niños entre"|
                                       substring(input$indicador_SI_tp, 1, 32)=="Porcentaje de niños menores de 5"|
                                       substring(input$indicador_SI_tp, 1, 4)=="Tasa")){
           
           dat <- base_SI_tp() %>% filter(NOMINDICADOR == input$indicador_SI_tp & is.na(CORTE1) == T) 
           
           datatable(dat  %>% 
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
           
         } else if(input$corte7 != "Total" & (substring(input$indicador_SI_tp, 1, 25)=="Porcentaje de niños entre"|
                                              substring(input$indicador_SI_tp, 1, 32)=="Porcentaje de niños menores de 5")){
           
           
           dat <- base_SI_tp() %>% filter(NOMINDICADOR == input$indicador_SI_tp & CORTE1_rec == input$corte7)
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_SI_tp&df$CORTE1_rec==input$corte7])))
           cut_nom <- quo_name(cut)
           
           datatable(dat  %>% 
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         !!cut_nom := !!cut,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
         } else {
           
         }
         
       })      
       
       
       ### DESCARGA DE TABLAS       
       output$tabla_resultado_SI_tp_descarga <- downloadHandler(
         filename = function() {
           paste0(input$indicador_SI_tp, ".xlsx", sep = "")
         },
         content = function(file) {  
           
           if(input$corte7 == "Total" & (substring(input$indicador_SI_tp, 1, 25)=="Porcentaje de niños entre"|
                                         substring(input$indicador_SI_tp, 1, 32)=="Porcentaje de niños menores de 5"|
                                         substring(input$indicador_SI_tp, 1, 4)=="Tasa")){
             
             dat <- base_SI_tp() %>% filter(NOMINDICADOR == input$indicador_SI_tp & is.na(CORTE1) == T) 
             
             openxlsx::write.xlsx(list( "Data" = dat  %>% 
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            "Valor" = VALOR),
                                        "Metadata" = base_SI_tp() %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
             # Gráficos de línea por sexo y panel para cortes 
             
           } else if(input$corte7 != "Total" & (substring(input$indicador_SI_tp, 1, 25)=="Porcentaje de niños entre"|
                                                substring(input$indicador_SI_tp, 1, 32)=="Porcentaje de niños menores de 5")){
             
             
             dat <- base_SI_tp() %>% filter(NOMINDICADOR == input$indicador_SI_tp & CORTE1_rec == input$corte7)
             cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_SI_tp&df$CORTE1_rec==input$corte7])))
             cut_nom <- quo_name(cut)
             
             
             openxlsx::write.xlsx(list( "Data" = dat  %>% 
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            !!cut_nom := !!cut,
                                            "Valor" = VALOR),
                                        "Metadata" = base_SI_tp() %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
           } else {
             
           }
           
           
         })  
       
       
       
       #######Salud infantil. País urbano       
       
       
       ##Gráficos
       
       output$plot_SI_pu <- renderPlot({
         
         if(input$corte8 == "Total" & (substring(input$indicador_SI_pu, 1, 25)=="Porcentaje de niños entre"|
                                       substring(input$indicador_SI_pu, 1, 32)=="Porcentaje de niños menores de 5")){
           
           dat <- base_SI_pu() %>% filter(NOMINDICADOR == input$indicador_SI_pu & CORTE1 == "Total") 
           
           g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), alpha = metodo, group = NOMINDICADOR)) +
             geom_line(dat = dat %>% filter(FECHA <= 2019),
                       size = 1, alpha = 0.4) +
             geom_line(dat = dat %>% filter(FECHA >= 2022),
                       size = 1, alpha = 0.8)  +
             geom_point(size = 2, show.legend = FALSE)  +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 14),
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA)) +
             scale_alpha_manual(values = c(.4, .8)) +
             scale_x_date(date_breaks = "3 years",date_labels  = "%Y") +
           labs(
             color = "",
             alpha = "",
             title = input$indicador_SI_pu,
             caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                         filter(NOMINDICADOR == input$indicador_SI_pu) %>% pull(FUENTE))), width = 160),
             x = "",
             y = "") +
             scale_color_brewer(name = "", palette = paleta)  
           
           
           } else if (input$corte8 == "Total" & substring(input$indicador_SI_pu, 1, 4)=="Tasa") {
             
             dat <- base_SI_pu() %>% filter(NOMINDICADOR == input$indicador_SI_pu & is.na(CORTE1) == T) 
             
             g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), color = NOMINDICADOR)) +
               geom_line(dat = dat, size = 1) +
               geom_point(size = 2, show.legend = FALSE) +
               theme_minimal() +
               theme(legend.position = "bottom",
                     legend.title = element_blank(),
                     axis.text = element_text(size = 12), 
                     axis.title = element_text(size = 14), 
                     legend.text = element_text(size = 12),
                     plot.background = element_rect(fill = "white", color = NA),
                     panel.background = element_rect(fill = "white", color = NA))+
               scale_alpha_manual(values = c(.4, .8)) +
               scale_color_manual(
                 values = c(color_defecto)) +
               scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
               labs(
                 color = "",
                 alpha = "",
                 title = input$indicador_SI_pu,
                 caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                             filter(NOMINDICADOR == input$indicador_SI_pu) %>% pull(FUENTE))), width = 160),
                 x = "",
                 y = "") +
               scale_color_brewer(name = "", palette = paleta)     
             
             
           
           
           
           
           
           
           
           
           
         } else if(input$corte8 != "Total" & (substring(input$indicador_SI_pu, 1, 25)=="Porcentaje de niños entre"|
                                              substring(input$indicador_SI_pu, 1, 32)=="Porcentaje de niños menores de 5")){
           
           
           
           dat <- base_SI_pu() %>% filter(NOMINDICADOR == input$indicador_SI_pu & CORTE1_rec == input$corte8)
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_SI_pu&df$CORTE1_rec==input$corte8])))
           
           g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), alpha = metodo, 
                                    group = !!cut, 
                                    color = !!cut)) +
             geom_line(dat = dat %>% filter(FECHA <= 2019),
                       size = 1, alpha = 0.4) +
             geom_line(dat = dat %>% filter(FECHA >= 2022),
                       size = 1, alpha = 0.8) +
             geom_point(size = 2, show.legend = FALSE) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12), 
                   axis.title = element_text(size = 14), 
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_alpha_manual(values = c(.4, .8)) +
             scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_SI_pu,
               caption = str_wrap(paste("Fuente:",unique(metadata %>% 
                                                           filter(NOMINDICADOR == input$indicador_SI_pu) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +
             scale_color_brewer(name = "", palette = paleta)  
           
           
           
         } else if((substring(input$indicador_SI_pu,1,21)=="Porcentaje de nacidos"|
                    substring(input$indicador_SI_pu,1,29)=="Porcentaje de niños lactantes")
                   & input$corte8 == "Total"){
           
           
           # Gráficos de barras por año
           
           dat <- base_SI_pu() %>% filter(NOMINDICADOR == input$indicador_SI_pu & CORTE1_rec == "Total")
           
           g1 <- dat %>% ggplot(aes(x = FECHA,
                                    y = as.numeric(VALOR),
                                    fill = NOMINDICADOR)) +
             geom_bar(size = 1, stat = "identity", position = position_dodge(width = 1), width = 0.8) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 14),
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_x_continuous(breaks = c(2013, 2018)) +
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CUID_pu,
               caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                           filter(NOMINDICADOR == input$indicador_CUID_pu) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +
             scale_fill_brewer(name = "", palette = paleta)
           
         } else if((substring(input$indicador_SI_pu,1,21)=="Porcentaje de nacidos"|
                    substring(input$indicador_SI_pu,1,29)=="Porcentaje de niños lactantes")
                   & input$corte8 != "Total"){
           
           
           # Gráficos de barras por año y corte
           
           dat <- base_SI_pu() %>% filter(NOMINDICADOR == input$indicador_SI_pu & CORTE1_rec == input$corte8)
           cut1 <- rlang::sym(rlang::as_string(unique(dat$CORTE1_rec[dat$NOMINDICADOR ==input$indicador_SI_pu])))
           
           g1 <- dat %>% ggplot(aes(x = FECHA,
                                    y = as.numeric(VALOR),
                                    fill = !!cut1)) +
             geom_bar(size = 1, stat = "identity", position = position_dodge(width = 1), width = 0.8) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 14),
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             scale_x_continuous(breaks = c(2013, 2018)) +
             labs(
               color = "",
               alpha = "",
               title = input$indicador_CUID_pu,
               caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                           filter(NOMINDICADOR == input$indicador_CUID_pu) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +
             scale_fill_brewer(name = "", palette = paleta)
           
           
           
         } else {
           
         }
         
         # Gráficos de línea por sexo y panel para cortes 
         print(g1)
         
         
         
       })
       
       
       
       ### DESCARGA DE GRÁFICOS
       
       output$graficos_resultado_SI_pu_descarga <- downloadHandler(
         filename <- function() {
           paste("g1", "png", sep = ".")
         },
         
         content <- function(file) {
           file.copy("www/g1.png", file)
         },
         contentType = "www/g1"
       )
       
       
       
       
       ### TABLAS
       
       output$tabla_resultado_SI_pu <- renderDT({
         
         
         if(input$corte8 == "Total" & (substring(input$indicador_SI_pu, 1, 25)=="Porcentaje de niños entre"|
                                       substring(input$indicador_SI_pu, 1, 32)=="Porcentaje de niños menores de 5"|
                                       substring(input$indicador_SI_pu, 1, 4)=="Tasa")){
           
           dat <- base_SI_pu() %>% filter(NOMINDICADOR == input$indicador_SI_pu & CORTE1 == "Total") 
           
           datatable(dat  %>% 
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
           
         } else if(input$corte8 != "Total" & (substring(input$indicador_SI_pu, 1, 25)=="Porcentaje de niños entre"|
                                              substring(input$indicador_SI_pu, 1, 32)=="Porcentaje de niños menores de 5")){
           
           
           dat <- base_SI_pu() %>% filter(NOMINDICADOR == input$indicador_SI_pu & CORTE1_rec == input$corte8)
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_SI_pu&df$CORTE1_rec==input$corte8])))
           cut_nom <- quo_name(cut)
           
           datatable(dat  %>% 
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         !!cut_nom := !!cut,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
         } else if((substring(input$indicador_SI_pu,1,21)=="Porcentaje de nacidos"|
                    substring(input$indicador_SI_pu,1,29)=="Porcentaje de niños lactantes")
                   & input$corte8 == "Total"){
           
           # Gráficos de barras por año
           
           dat <- base_SI_pu() %>% filter(NOMINDICADOR == input$indicador_SI_pu & CORTE1_rec == "Total")
           
           
           datatable(dat  %>% 
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
           
           
           
           
         } else if((substring(input$indicador_SI_pu,1,21)=="Porcentaje de nacidos"|
                    substring(input$indicador_SI_pu,1,29)=="Porcentaje de niños lactantes")
                   & input$corte8 != "Total"){
           
           
           # Gráficos de barras por año y corte
           
           dat <- base_SI_pu() %>% filter(NOMINDICADOR == input$indicador_SI_pu & CORTE1_rec == input$corte8)
           cut1 <- rlang::sym(rlang::as_string(unique(dat$CORTE1_rec[dat$NOMINDICADOR ==input$indicador_SI_pu])))
           cut_nom <- quo_name(cut1)
           
           datatable(dat  %>% 
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         !!cut_nom := !!cut1,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
           
           
         } else {
           
         }
         
       })      
       
       
       
       ### DESCARGA DE TABLAS       
       output$tabla_resultado_SI_pu_descarga <- downloadHandler(
         filename = function() {
           paste0(input$indicador_SI_pu, ".xlsx", sep = "")
         },
         content = function(file) {  
           
           if(input$corte8 == "Total" & (substring(input$indicador_SI_pu, 1, 25)=="Porcentaje de niños entre"|
                                         substring(input$indicador_SI_pu, 1, 32)=="Porcentaje de niños menores de 5"|
                                         substring(input$indicador_SI_pu, 1, 4)=="Tasa")){
             
             dat <- base_SI_pu() %>% filter(NOMINDICADOR == input$indicador_SI_pu & CORTE1 == "Total") 
             
             openxlsx::write.xlsx(list( "Data" = dat  %>% 
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            "Valor" = VALOR),
                                        "Metadata" = base_SI_pu() %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
             # Gráficos de línea por sexo y panel para cortes 
             
           } else if(input$corte8 != "Total" & (substring(input$indicador_SI_pu, 1, 25)=="Porcentaje de niños entre"|
                                                substring(input$indicador_SI_pu, 1, 32)=="Porcentaje de niños menores de 5")){
             
             
             dat <- base_SI_pu() %>% filter(NOMINDICADOR == input$indicador_SI_pu & CORTE1_rec == input$corte8)
             cut <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_SI_pu&df$CORTE1_rec==input$corte8])))
             cut_nom <- quo_name(cut)
             
             
             openxlsx::write.xlsx(list( "Data" = dat  %>% 
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            !!cut_nom := !!cut,
                                            "Valor" = VALOR),
                                        "Metadata" = base_SI_pu() %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
           } else if((substring(input$indicador_SI_pu,1,21)=="Porcentaje de nacidos"|
                      substring(input$indicador_SI_pu,1,29)=="Porcentaje de niños lactantes")
                     & input$corte8 == "Total"){
             
             # Gráficos de barras por año
             
             dat <- base_SI_pu() %>% filter(NOMINDICADOR == input$indicador_SI_pu & CORTE1_rec == "Total")
             
             
             
             openxlsx::write.xlsx(list( "Data" = dat  %>% 
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            "Valor" = VALOR),
                                        "Metadata" = base_SI_pu() %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
             
           } else if((substring(input$indicador_SI_pu,1,21)=="Porcentaje de nacidos"|
                      substring(input$indicador_SI_pu,1,29)=="Porcentaje de niños lactantes")
                     & input$corte8 != "Total"){
             
             
             # Gráficos de barras por año y corte
             
             dat <- base_SI_pu() %>% filter(NOMINDICADOR == input$indicador_SI_pu & CORTE1_rec == input$corte8)
             cut1 <- rlang::sym(rlang::as_string(unique(dat$CORTE1_rec[dat$NOMINDICADOR ==input$indicador_SI_pu])))
             cut_nom <- quo_name(cut1)
             
             
             
             openxlsx::write.xlsx(list( "Data" = dat  %>% 
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            !!cut_nom := !!cut1,
                                            "Valor" = VALOR),
                                        "Metadata" = base_SI_pu() %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
           } else {
             
           }
           
           
         })  
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       ###### PLANIFICACIÓN FAMILIAR. TOTAL PAÍS
       
       output$plot_PF_tp <- renderPlot({
         
         
         # Gráficos de línea para totales de tasa
         
         dat <- base_PF_tp() %>% filter(NOMINDICADOR == input$indicador_PF_tp & is.na(CORTE1) == T)
         
        g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), color = NOMINDICADOR)) +
           geom_line(dat = dat, size = 1) +
           geom_point(size = 2, show.legend = FALSE) +
           theme_minimal() +
           theme(legend.position = "bottom",
                 legend.title = element_blank(),
                 axis.text = element_text(size = 12), 
                 axis.title = element_text(size = 14), 
                 legend.text = element_text(size = 12),
                 plot.background = element_rect(fill = "white", color = NA),
                 panel.background = element_rect(fill = "white", color = NA))+
           scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
           labs(
             color = "",
             alpha = "",
             title = input$indicador_PF_tp,
             caption = str_wrap(paste("Fuente:",unique(metadata %>% 
                                                         filter(NOMINDICADOR == input$indicador_PF_tp) %>% pull(FUENTE))), width = 160),
             x = "",
             y = "") +
           
           scale_color_brewer(name = "", palette = paleta)  
         
         
         print(g1)
         # ggsave("www/g1.png", width = 30, height = 20, units = "cm")
         
       }) 
       
       ### DESCARGA DE GRÁFICOS
       
       output$graficos_resultado_PF_tp_descarga <- downloadHandler(
         filename <- function() {
           paste("g1", "png", sep = ".")
         },
         
         content <- function(file) {
           file.copy("www/g1.png", file)
         },
         contentType = "www/g1"
       )
       
       ### TABLAS
       
       output$tabla_resultado_PF_tp <- renderDT({
         
         dat <- base_PF_tp() %>% filter(NOMINDICADOR == input$indicador_PF_tp & is.na(CORTE1) == T)
         
         datatable(dat  %>% 
                     arrange(FECHA)%>%
                     transmute(
                       "Año" = FECHA,
                       "Valor" = VALOR),
                   rownames = FALSE,
                   options = list(columnDefs = 
                                    list(list(className = 'dt-center', 
                                              targets = "_all"))))
         
         
       }) 
       
       output$tabla_resultado_PF_tp_descarga <- downloadHandler(
         filename = function() {
           paste0(input$indicador_PF_tp, ".xlsx", sep = "")
         },
         content = function(file) {  
           
           dat <- base_PF_tp() %>% filter(NOMINDICADOR == input$indicador_PF_tp & is.na(CORTE1) == T)
           
           openxlsx::write.xlsx(list( "Data" = dat %>%
                                        arrange(FECHA)%>%
                                        transmute(
                                          "Año" = FECHA,
                                          "Valor" = VALOR),
                                      "Metadata" = base_PF_tp() %>%
                                        dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                        transmute(
                                          "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                          "FUENTE" = FUENTE,
                                          "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                file,
                                rowNames = TRUE)
           
         })  
       
       
       ###### PLANIFICACIÓN FAMILIAR. PAÍS URBANO
       
       output$plot_PF_pu <- renderPlot({
         
         
         if(input$corte3 == "Total"){
           # Gráficos de barras para totales de indicadores ENCOR
           
           dat <- base_PF_pu() %>% filter(NOMINDICADOR == input$indicador_PF_pu & is.na(CORTE1) == T) 
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_PF_pu])))
           
           g1 <- dat %>% ggplot(aes(x = as.factor(FECHA), 
                                    y = as.numeric(VALOR), 
                                    fill = !!cut)) +
             geom_bar(size = 1, stat = "identity", width = 0.25) +
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 14),
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_PF_pu,
               caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                           filter(NOMINDICADOR == input$indicador_PF_pu) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +
             scale_fill_brewer(name = "", palette = paleta)+
             scale_x_discrete(drop = TRUE) 
           
           
         } else if(input$corte3 != "Total") {
           
           # Gráficos de barras para cortes de indicadores con ENCOR
           
           
           dat <- base_PF_pu() %>% filter(NOMINDICADOR == input$indicador_PF_pu & CORTE1_rec == input$corte3)
           cut1 <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_PF_pu])))
           cut2 <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_PF_pu&df$CORTE1_rec==input$corte3])))
           
           
           
           g1 <- dat %>% ggplot(aes(x = !!cut2, 
                                    y = as.numeric(VALOR), 
                                    fill = !!cut1)) +
             geom_bar(size = 1, stat = "identity", width = 0.4) +
             
             theme_minimal() +
             theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   axis.text = element_text(size = 12), 
                   axis.title = element_text(size = 14), 
                   legend.text = element_text(size = 12),
                   plot.background = element_rect(fill = "white", color = NA),
                   panel.background = element_rect(fill = "white", color = NA))+
             labs(
               color = "",
               alpha = "",
               title = input$indicador_PF_pu,
               caption = str_wrap(paste("Fuente:",unique(metadata %>% 
                                                           filter(NOMINDICADOR == input$indicador_PF_pu) %>% pull(FUENTE))), width = 160),
               x = "",
               y = "") +
             scale_fill_brewer(name = "", palette = paleta)
           
           
         } 
         print(g1)
         # ggsave("www/g1.png", width = 30, height = 20, units = "cm")
         
       }) 
       
       ### DESCARGA DE GRÁFICOS
       
       output$graficos_resultado_PF_pu_descarga <- downloadHandler(
         filename <- function() {
           paste("g1", "png", sep = ".")
         },
         
         content <- function(file) {
           file.copy("www/g1.png", file)
         },
         contentType = "www/g1"
       )
       
       ### TABLAS
       
       output$tabla_resultado_PF_pu <- renderDT({
         
         
         if(input$corte3 == "Total"){
           # Gráficos de barras para totales de indicadores ENCOR
           
           dat <- base_PF_pu() %>% filter(NOMINDICADOR == input$indicador_PF_pu & input$corte3 == "Total") 
           cut <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_PF_pu])))
           cut_nom <- quo_name(cut)
           
           datatable(dat %>% 
                       filter(NOMINDICADOR==input$indicador_PF_pu & is.na(CORTE1) == T) %>%
                       arrange(!!cut)%>%
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         !!cut_nom := !!cut,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
         } else if(input$corte3 != "Total") {
           
           
           dat <- base_PF_pu() %>% filter(NOMINDICADOR == input$indicador_PF_pu & CORTE1_rec == input$corte3)   
           cut1 <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_PF_pu])))
           cut2 <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_PF_pu&df$CORTE1_rec==input$corte3])))
           cut_nom1 <- quo_name(cut1)
           cut_nom2 <- quo_name(cut2)
           
           datatable(dat %>% 
                       arrange(FECHA)%>%
                       transmute(
                         "Año" = FECHA,
                         !!cut_nom1 := !!cut1,
                         !!cut_nom2 := !!cut2,
                         "Valor" = VALOR),
                     rownames = FALSE,
                     options = list(columnDefs = 
                                      list(list(className = 'dt-center', 
                                                targets = "_all"))))
           
           
           
           
         } 
       }) 
       
       output$tabla_resultado_PF_pu_descarga <- downloadHandler(
         filename = function() {
           paste0(input$indicador_PF_pu, ".xlsx", sep = "")
         },
         content = function(file) {  
           
           if(input$corte3 == "Total"){
             
             dat <- base_PF_pu() %>% filter(NOMINDICADOR == input$indicador_PF_pu & CORTE1_rec == "Total") 
             cut <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_PF_pu])))
             cut_nom <- quo_name(cut)
             
             openxlsx::write.xlsx(list( "Data" = dat %>%
                                          arrange(!!cut)%>%
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            !!cut_nom := !!cut,
                                            "Valor" = VALOR),
                                        "Metadata" = dat %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
           } else if(input$corte3 != "Total") {
             
             
             dat <- base_PF_pu() %>% filter(NOMINDICADOR == input$indicador_PF_pu & CORTE1_rec == input$corte3)   
             cut1 <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_PF_pu])))
             cut2 <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_PF_pu&df$CORTE1_rec==input$corte3])))
             cut_nom1 <- quo_name(cut1)
             cut_nom2 <- quo_name(cut2)
             
             openxlsx::write.xlsx(list( "Data" = dat %>%
                                          arrange(FECHA)%>%
                                          transmute(
                                            "Año" = FECHA,
                                            !!cut_nom1 := !!cut1,
                                            !!cut_nom2 := !!cut2,
                                            "Valor" = VALOR),
                                        "Metadata" = dat %>%
                                          dplyr::distinct(NOMINDICADOR,FUENTE,DEFINICIÓN,`FORMA DE CÁLCULO`)%>%
                                          transmute(
                                            "NOMBRE DEL INDICADOR" = NOMINDICADOR,
                                            "FUENTE" = FUENTE,
                                            "FORMA DE CÁLCULO"=`FORMA DE CÁLCULO`)%>%sjmisc::rotate_df()),
                                  file,
                                  rowNames = TRUE)
             
             
             
           }
           
           
           
           
         })       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
}


                                                                      
                                                                                             
# Ejecutar la aplicación Shiny  
shinyApp(ui = ui, server = server)
                    
                    

                    