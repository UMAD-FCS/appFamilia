
## ***************************************************************************
## Shiny APP Pobreza - Unidad de Métodos y Acceso a Datos
## ***************************************************************************

##  0. PAQUETES Y FUNCIONES  =================================================

#extrafont::loadfonts(device = "win", quiet = T)

library(tidyverse)
library(labelled)
library(DT)
library(shiny)
library(shinythemes)
library(patchwork)
library(here)
library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(bslib)
library(RColorBrewer)
library(viridis)
library(sjmisc)


source('utils.R')

theme_set(theme_bdd())

update_geom_defaults("text", list(family = theme_get()$text$family))

dir.create('~/.fonts')
file.copy("www/Titillium Web.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')

color_defecto <- "#21618C"

# Spinner options
options(
  spinner.color = "#476481",
  spinner.color.background = "#ffffff",
  spinner.size = 2
)

# Shiny app customized theme
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

# Colores
vs_blues <- c("#031633", rev(brewer.pal(n = 9, "Blues")[3:9]))
cat_palette <- c("#1F618D", "#5DADE2", "#16A085", "#9B59B6",
                 "#F39C12", "#F4D03F", "#7F8C8D", "#BDC3C7", "#FF8484")
vs_blues_2 <- c(vs_blues[4], vs_blues[7])


##  1. PREPARAR DATA  =======================================================

##Ficha metodológica (ver compatibiidad de nombres)

base_fichas <- readxl::read_excel("Base_Fichas_Tecnicas.xls")%>% 
  janitor::clean_names() %>% 
  mutate(nomindicador=replace(nomindicador, nomindicador=="Relación entre el ingreso medio per cápita del primer y décimo decil", 
                              "Relación entre el Ingreso medio per cápita del primer y décimo decil"))%>%
  mutate(nomindicador=replace(nomindicador, nomindicador=="Relación entre el ingreso medio per cápita del primer y quinto quintil", 
                              "Relación entre el Ingreso medio per cápita del primer y quinto quintil")) %>%
  mutate(nomindicador=replace(nomindicador, nomindicador=="Promedio de ingreso per-cápita de los hogares (cte. base diciembre 2006)", 
                              "Promedio de ingreso per-cápita de los hogares (Cte. Base diciembre 2006)")) %>%
  mutate(nomindicador=replace(nomindicador, nomindicador=="Mediana de ingreso per-cápita de los hogares (cte. base diciembre 2006)", 
                              "Mediana de ingreso per-cápita de los hogares (Cte. Base diciembre 2006)")) 



df_generica <- readxl::read_excel("Base_Motor_Pobreza.xlsx") %>% 
  janitor::clean_names() %>% 
  select(- codind, - responsable) %>%
  mutate(corte_nueva=replace(corte_nueva, corte_nueva=="Edad", 
                              "Tramo de edad")) %>%
  left_join(.,base_fichas,by="nomindicador")


df_generica$valor=as.numeric(df_generica$valor)

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
                # Revisar
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
  "Pobreza y desigualdad",
  collapsible=TRUE,
  
  tabPanel(
    
    #   2.0. HOME ----
    title = "Home",
    value = 'borelito',
    icon = icon("fas fa-home"),
    
    # Top layout
    HTML("<p><center><h2><b> Bienvenida/o a la app de datos sobre Pobreza y desigualdad. &nbsp; &nbsp; <a href='https://umad.cienciassociales.edu.uy/'> <img src='umadlogo_2.png' width=160' height='80'> </a> </b></h2></p>"),
    
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
             HTML("<h6> <p style='color:#597ea2' align='justify'> El tablero presenta los principales indicadores sobre ingresos y desigualdad, desagragados entre total país y país urbano. Incluye visualizaciones sobre estimaciones de pobreza e indigencia a partir de estadísticas nacionales. Por último, indicadores sobre NBI con diferentes desagregaciones (región, sexo, edad, ascendencia, quintiles de ingreso, entre otros). </p></h6>"),               
             # br(),
             # tags$h5(style="color:#085792;text-align:justify;font-weight: bold;",
             #         "¿Cómo citar?"),
             # tags$h6(style="color:#597ea2;text-align:justify;",
             #         paste("Datos electorales: Schmidt, Nicolás, Cardarello, Antonio, Luján, Diego",
             #               paste0("(", anio_pkg("Boreluy"), ")"),
             #               "Boreluy: Datos electorales de Uruguay 1910-2020, R package version", 
             #               packageVersion("Boreluy"), "https://nicolas-schmidt.github.io/Boreluy/.")),
             # tags$h6(style="color:#597ea2;text-align:justify;",
             #         paste("Datos de opinión pública: Schmidt, Nicolás, Vairo, Daniela, Opertti, Martín , UMAD",
             #               paste0("(", anio_pkg("opuy"), ")"),
             #               "opuy: Datos de Opinión Pública de Uruguay 1989 - 2020, R package version,", 
             #               packageVersion("opuy"), ", https://nicolas-schmidt.github.io/opuy/.")),
             # br(),
             tags$h6(style="color:#085792;text-align:left;font-weight: bold;",
                     "Mantenedor: Elina Gómez"),
             tags$h6(style="color:#085792;text-align:left;font-weight: bold;",
                     "Contacto: umad@cienciassociales.edu.uy"),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
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
                       tags$li(tags$h6(tags$a(href="https://umad-fcs.github.io/Piso-I-Pobreza/", "Indicadores Pobreza y desigualdad en Uruguay"))), 
                       tags$li(tags$h6(tags$a(href="https://bancodedatos-fcs.shinyapps.io/appDemografia/", "Tablero indicadores demográficos"))), 
                       tags$li(tags$h6(tags$a(href="https://umad-fcs.github.io/Piso-I-Demografia/", "Indicadores demográficos del Uruguay"))),
                       
               )
             ),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
      ),
      column(width = 1),
    ),
    div(class = "footer",
        includeHTML("html/footer.html"))
    
  ),
  
  
 
 tabPanel(
   
   title = "Ingresos y desigualdad",
   value = 'borelito',
   
   icon = icon("hand-holding-dollar", lib = "font-awesome"),
   
   tabsetPanel(
     type = "pills",
     id   = "tabsetborel",
     
     
     tabPanel(
       "Total País",
       br(),
       
       fluidRow(
         
       #div( id ="Sidebar",
        sidebarPanel(width = 3,
         #style = "position:fixed;width:22%;",
         selectInput(
           "indicador_id_tp",
           "Seleccione el indicador:",
           choices = unique(df_generica %>%
                              filter(categoria == "Ingresos y desigualdad" & pestana =="Total País") %>%
                              pull(nomindicador))),
         uiOutput("selectcorte1"),
         
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
         tags$h4(style="display:inline-block",
                 uiOutput("title_indicador_id_tp")),
         div(style="display:inline-block", 
             dropdown(
               style = "minimal",
               status = "primary",
               width = "400px",
               right = TRUE,
               icon = icon("info", lib = "font-awesome"),
               uiOutput("def_id_tp"))
         ),
         div(style="display:inline-block", 
             dropdown(
               style = "minimal",
               status = "primary",
               width = "400px",
               right = TRUE,
               icon = icon("calculator", lib = "font-awesome"),
               uiOutput("info_id_tp"))
         ),
         br(),
         br(),
         plotly::plotlyOutput("plot_id_tp",height = 'auto', width = 'auto'),
         br(),
         br(),
         br(),
         tags$h6(style="display:inline-block",
                 uiOutput("cita_indicador_id_tp")),
         br(),
         br(),
         DTOutput("tabla_resultado_id_tp"),
         br(),
         br(),
         br(),
         downloadButton("tabla_resultado_id_tp_descarga", "Descargá la tabla"),
         br(),
         br(),
         br(),
         br(),
         br(),
       ))),
       
     tabPanel(
       "País Urbano",
       br(),
       
       fluidRow(
       #div( id ="Sidebar",
            sidebarPanel(width = 3,
         #style = "position:fixed;width:22%;",
         selectInput(
           "indicador_id_pu",
           "Seleccione el indicador:",
           choices = unique(df_generica %>%
                              filter(categoria == "Ingresos y desigualdad" & pestana =="País Urbano"
                              ) %>%
                              pull(nomindicador))),
         uiOutput("selectcorte2"),
         
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
         tags$h4(style="display:inline-block",
                 uiOutput("title_indicador_id_pu")),
         div(style="display:inline-block", 
             dropdown(
               style = "minimal",
               status = "primary",
               width = "400px",
               right = TRUE,
               icon = icon("info", lib = "font-awesome"),
               uiOutput("def_id_pu"))
         ),
         div(style="display:inline-block", 
             dropdown(
               style = "minimal",
               status = "primary",
               width = "400px",
               right = TRUE,
               icon = icon("calculator", lib = "font-awesome"),
               uiOutput("info_id_pu"))
         ),
         br(),
         br(),
                 plotly::plotlyOutput("plot_id_pu",height = 'auto', width = 'auto'),
                 br(),
                 br(),
                 br(),
         tags$h6(style="display:inline-block",
                 uiOutput("cita_indicador_id_pu")),
                 br(),
                 br(),
                 DTOutput("tabla_resultado_id_pu"),
                 br(),
                 br(),
                 br(),
                 downloadButton("tabla_resultado_id_pu_descarga", "Descargá la tabla"),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
       ))
     
   ))),
 
 
 tabPanel(
   title = "Pobreza e indigencia",
   value = 'borelito',
   icon = icon("chart-simple", lib = "font-awesome"),
   
   br(),
   fluidRow(
   #div( id ="Sidebar",
        sidebarPanel(width = 3,
     #style = "position:fixed;width:22%;",
     selectInput(
       "indicador_pobreza",
       "Seleccione el indicador:",
       choices = unique(df_generica %>% 
                          filter(categoria == "Pobreza") %>% 
                          pull(nomindicador))),
     
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
     tags$h4(style="display:inline-block",
             uiOutput("title_indicador_p")),
     div(style="display:inline-block", 
         dropdown(
           style = "minimal",
           status = "primary",
           width = "400px",
           right = TRUE,
           icon = icon("info", lib = "font-awesome"),
           uiOutput("def_p"))
     ),
     div(style="display:inline-block", 
         dropdown(
           style = "minimal",
           status = "primary",
           width = "400px",
           right = TRUE,
           icon = icon("calculator", lib = "font-awesome"),
           uiOutput("info_p"))
     ),
     br(),
     br(),
     
     # tags$h4(style="display:inline-block",
     #                 uiOutput("title_indicador_p")),
             plotly::plotlyOutput("plot_p",height = 'auto', width = 'auto'),
             br(),

     tags$h6(style="display:inline-block",
             uiOutput("cita_indicador_p")),
             br(),
             br(),
             DTOutput("tabla_resultado_p"),
             br(),
             br(),
             br(),
             downloadButton("tabla_resultado_p_descarga", "Descargá la tabla"),
             br(),
             br(),
             br(),
             br(),
             br(),
             
   )
 )),
 

 tabPanel(
   
   title = "NBI",
   value = 'borelito',
   icon = icon("house", lib = "font-awesome"),
   
   tabsetPanel(
     type = "pills",
     id   = "tabsetborel",
     tabPanel(
       "Total País",
       br(),
       fluidRow(
       #div( id ="Sidebar",
            sidebarPanel(width = 3,
         #style = "position:fixed;width:22%;",
         selectInput(
           "indicador_nbi_p",
           "Seleccione el indicador:",
           choices = unique(df_generica %>%
                              filter(categoria == "Necesidades Básicas" & pestana =="Total País") %>%
                              pull(nomindicador))),
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
         
         tags$h4(style="display:inline-block",
                 uiOutput("title_indicador_nbi_p")),
         div(style="display:inline-block", 
             dropdown(
               style = "minimal",
               status = "primary",
               width = "400px",
               right = TRUE,
               icon = icon("info", lib = "font-awesome"),
               uiOutput("def_nbi_p"))
         ),
         div(style="display:inline-block", 
             dropdown(
               style = "minimal",
               status = "primary",
               width = "400px",
               right = TRUE,
               icon = icon("calculator", lib = "font-awesome"),
               uiOutput("info_nbi_p"))
         ),
         br(),
         br(),
         
         # tags$h4(style="display:inline-block",
         #                 uiOutput("title_indicador_nbi_p")),
                 plotly::plotlyOutput("plot_nbi_p",height = 'auto', width = 'auto'),
                 br(),

         tags$h6(style="display:inline-block",
                 uiOutput("cita_indicador_nbi_p")),
                 br(),
                 br(),
                 DTOutput("tabla_resultado_nbi_p"),
                 br(),
                 br(),
                 br(),
                 downloadButton("tabla_resultado_nbi_p_descarga", "Descargá la tabla"),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 
                 
       ))),
     tabPanel(
       "País Urbano",
       br(),
      # div( id ="Sidebar",
      fluidRow(
            sidebarPanel(width = 3,
         #style = "position:fixed;width:22%;",
         selectInput(
           "indicador_nbi_h",
           "Seleccione el indicador:",
           choices = unique(df_generica %>%
                              filter(categoria == "Necesidades Básicas" & pestana =="País Urbano"
                                     ) %>%
                              pull(nomindicador))),
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
         tags$h4(style="display:inline-block",
                 uiOutput("title_indicador_nbi_h")),
         div(style="display:inline-block", 
             dropdown(
               style = "minimal",
               status = "primary",
               width = "400px",
               right = TRUE,
               icon = icon("info", lib = "font-awesome"),
               uiOutput("def_nbi_h"))
         ),
         div(style="display:inline-block", 
             dropdown(
               style = "minimal",
               status = "primary",
               width = "400px",
               right = TRUE,
               icon = icon("calculator", lib = "font-awesome"),
               uiOutput("info_nbi_h"))
         ),
         br(),
         br(),
         # tags$h4(style="display:inline-block",
         #                 uiOutput("title_indicador_nbi_h")),
                 div(plotly::plotlyOutput("plot_nbi_h",height = 'auto', width = 'auto'),align = "center"),
                 br(),
         tags$h6(style="display:inline-block",
                 uiOutput("cita_indicador_nbi_h")),
                 br(),
                 br(),
                 DTOutput("tabla_resultado_nbi_h"),
                 br(),
                 br(),
                 br(),
                 downloadButton("tabla_resultado_nbi_h_descarga", "Descargá la tabla"),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 
       )))
     
   ))


 ))




server <- function(session, input, output) {
  


  # output$selectcorte1 <- renderUI({
  #   selectInput("corte1", "Resultados por:", choices = unique(df_id_tp$corte_nueva[df_id_tp$nomindicador ==input$indicador_id_tp]))
  # })
  
  output$selectcorte1 <- renderUI({
    selectInput("corte1", "Resultados por:", choices = base_id_tp()  %>% pull(corte_nueva) %>% unique())
  })
  
  output$selectcorte2 <- renderUI({
    selectInput("corte2", "Resultados por:", choices = base_id_pu()  %>% pull(corte_nueva) %>% unique())
  })

  output$selectcorte3 <- renderUI({
    selectInput("corte3", "Resultados por:", choices = base_p()  %>% pull(corte_nueva) %>% unique())
  })
  
  output$selectcorte4 <- renderUI({
    selectInput("corte4", "Resultados por:", choices = base_nbi_p()  %>% pull(corte_nueva) %>% unique())
  }) 
  output$selectcorte5 <- renderUI({
    selectInput("corte5", "Resultados por:", choices = base_nbi_h()  %>% pull(corte_nueva) %>% unique())
  }) 



base_id_tp <- reactive({

  df_generica %>%
     filter(categoria == "Ingresos y desigualdad"&pestana=="Total País",
            nomindicador == input$indicador_id_tp) %>%
  mutate(valor = round(valor))

})

base_id_pu <- reactive({
  
  df_generica %>%
    filter(categoria == "Ingresos y desigualdad"&pestana=="País Urbano",
           nomindicador == input$indicador_id_pu)%>%
    mutate(valor = round(valor))
  
})


##TITULOS


output$title_indicador_id_tp <- renderUI({ 
  helpText(HTML(unique(base_id_tp()$nomindicador)))
})

output$cita_indicador_id_tp <- renderUI({ 
  helpText(HTML(paste("Fuente:",unique(base_id_tp()$cita))))
})

# Info: forma de CALCULO
output$info_id_tp <- renderUI({ 
  helpText(HTML(paste("<b>Forma de cálculo:</b>", 
                      unique(base_id_tp()$calculo))))
  
})

# Definición:
output$def_id_tp <- renderUI({ 
  helpText(HTML(paste("<b>Definición:</b>", unique(base_id_tp()$definicion))))
})




output$title_indicador_id_pu <- renderUI({ 
  helpText(HTML(unique(base_id_pu()$nomindicador)))
})


output$cita_indicador_id_pu <- renderUI({ 
  helpText(HTML(paste("Fuente:",unique(base_id_pu()$cita))))
})

# Info: forma de CALCULO
output$info_id_pu <- renderUI({ 
  helpText(HTML(paste("<b>Forma de cálculo:</b>", 
                      unique(base_id_pu()$calculo))))
  
})

# Definición:
output$def_id_pu <- renderUI({ 
  helpText(HTML(paste("<b>Definición:</b>", unique(base_id_pu()$definicion))))
})



output$title_indicador_p <- renderUI({ 
  helpText(HTML(unique(base_p()$nomindicador)))
})

output$cita_indicador_p <- renderUI({ 
  helpText(HTML(paste("Fuente:",unique(base_p()$cita))))
})

# Info: forma de CALCULO
output$info_p <- renderUI({ 
  helpText(HTML(paste("<b>Forma de cálculo:</b>", 
                      unique(base_p()$calculo))))
  
})

# Definición:
output$def_p <- renderUI({ 
  helpText(HTML(paste("<b>Definición:</b>", unique(base_p()$definicion))))
})


output$title_indicador_nbi_p <- renderUI({ 
  helpText(HTML(unique(base_nbi_p()$nomindicador)))
})

output$cita_indicador_nbi_p <- renderUI({ 
  helpText(HTML(paste("Fuente:",unique(base_nbi_p()$cita))))
})

# Info: forma de CALCULO
output$info_nbi_p <- renderUI({ 
  helpText(HTML(paste("<b>Forma de cálculo:</b>", 
                      unique(base_nbi_p()$calculo))))
  
})

# Definición:
output$def_nbi_p <- renderUI({ 
  helpText(HTML(paste("<b>Definición:</b>", unique(base_nbi_p()$definicion))))
})



output$title_indicador_nbi_h <- renderUI({ 
  helpText(HTML(unique(base_nbi_h()$nomindicador)))
})

output$cita_indicador_nbi_h <- renderUI({ 
  helpText(HTML(paste("Fuente:",unique(base_nbi_h()$cita))))
})

# Info: forma de CALCULO
output$info_nbi_h <- renderUI({ 
  helpText(HTML(paste("<b>Forma de cálculo:</b>", 
                      unique(base_nbi_h()$calculo))))
  
})

# Definición:
output$def_nbi_h <- renderUI({ 
  helpText(HTML(paste("<b>Definición:</b>", unique(base_nbi_h()$definicion))))
})




output$plot_id_tp <- plotly::renderPlotly({

  if(input$corte1 == "Región") {
    
    
    g1 <- base_id_tp() %>%
      filter(corte_nueva == "Región") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = urbanoruraluy,
                 text = paste("</br>Año:",anio,"</br>Región:",urbanoruraluy,"</br>Valor:",round(as.numeric(valor),1)))) +
      geom_line(aes(group=urbanoruraluy),size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(
           x = "",
           y = "")  
    

    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",
                                   xanchor = "center",
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    


  } else if(input$corte1 == "Sexo del jefe(a)"){

    
    g1 <- base_id_tp() %>%
      filter(corte_nueva == "Sexo del jefe(a)") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = sexojefatura,
                 text = paste("</br>Año:",anio,"</br>Sexo del jefe(a):",sexojefatura,"</br>Valor:",round(as.numeric(valor),1)))) +
      geom_line(aes(group=sexojefatura),size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(
        x = "",
        y = "")  
    
    

    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",
                                   xanchor = "center",
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)

  } else if (input$corte1 == "Pobreza") {

    
    g1 <- base_id_tp() %>%
      filter(corte_nueva == "Pobreza") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = pobreza,
                 text = paste("</br>Año:",anio,"</br>Pobreza:",pobreza,"</br>Valor:",round(as.numeric(valor),1)))) +
      geom_line(aes(group=pobreza),size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(
        x = "",
        y = "")  

    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",
                                   xanchor = "center",
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)

    }   else if (input$corte1 == "Decil de ingreso") {

      g1 <- base_id_tp() %>%
        filter(corte_nueva == "Decil de ingreso") %>%
        ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = decil,
                   text = paste("</br>Año:",anio,"</br>Decil de ingreso:",decil,"</br>Valor:",round(as.numeric(valor),1)))) +
        geom_line(aes(group=decil),size = 1, alpha = 0.5) +
        geom_point( size = 2.5) +
        scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
        theme(axis.text = element_text(size = 8))+
        scale_color_viridis(name = " ", discrete = T, direction = 1)+
        labs(
          x = "",
          y = "")  
      

      plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                       hoverinfo = 'text',tooltip = c("text"))%>%
        plotly::layout(legend = list(orientation = "h",
                                     xanchor = "center",
                                     x = 0.5,y=-0.2)) %>%
        
        plotly::config(displayModeBar = TRUE,
                       modeBarButtonsToRemove = list(
                         "pan2d",
                         "autoScale2d",
                         "resetScale2d",
                         "hoverClosestCartesian",
                         "hoverCompareCartesian",
                         "sendDataToCloud",
                         "toggleHover",
                         "resetViews",
                         "toggleSpikelines",
                         "resetViewMapbox"
                       ),showLink = FALSE,
                       displaylogo = FALSE)

    } else if (input$corte1 == "Quintil de ingreso") {

      g1 <- base_id_tp() %>%
        filter(corte_nueva == "Quintil de ingreso") %>%
        ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = quintil,
                   text = paste("</br>Año:",anio,"</br>Quintil de ingreso:",quintil,"</br>Valor:",round(as.numeric(valor),1)))) +
        geom_line(aes(group=quintil),size = 1, alpha = 0.5) +
        geom_point( size = 2.5) +
        scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
        theme(axis.text = element_text(size = 8))+
        scale_color_viridis(name = " ", discrete = T, direction = 1)+
        labs(
          x = "",
          y = "")  

      plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                       hoverinfo = 'text',tooltip = c("text"))%>%
        plotly::layout(legend = list(orientation = "h",
                                     xanchor = "center",
                                     x = 0.5,y=-0.2)) %>%
        
        plotly::config(displayModeBar = TRUE,
                       modeBarButtonsToRemove = list(
                         "pan2d",
                         "autoScale2d",
                         "resetScale2d",
                         "hoverClosestCartesian",
                         "hoverCompareCartesian",
                         "sendDataToCloud",
                         "toggleHover",
                         "resetViews",
                         "toggleSpikelines",
                         "resetViewMapbox"
                       ),showLink = FALSE,
                       displaylogo = FALSE)


  }
  else {


    g1 <- base_id_tp() %>%
      filter(corte_nueva=="Total") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), round(as.numeric(valor),1), color=corte_nueva,group = corte_nueva,
                 text = paste("</br>Año:",anio,"</br>Valor:",round(as.numeric(valor),1))
      )) +
      geom_line(size = 1, color = "#476481", alpha = 0.5) +
      geom_point(color = "#476481", size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8),axis.text.x = element_text(angle = 0),
            legend.position = "none") +
      #scale_y_continuous(labels = addUnits) +
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(x = "",
           y = "")
    

    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",
                                   xanchor = "center",
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)

  }

})




##Tablas shiny

output$tabla_resultado_id_tp <- renderDT({
  
  if(input$corte1 == "Región") {
    

    datatable(base_id_tp() %>%
              filter(corte_nueva == "Región") %>%
              arrange(anio)%>%
              transmute(
              "Año" = anio,
              "Región" = urbanoruraluy,
              "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
          

    
  } else if(input$corte1 == "Sexo del jefe(a)"){
    
    datatable(base_id_tp() %>%
                filter(corte_nueva == "Sexo del jefe(a)") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Sexo del jefe(a)" = sexojefatura,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if (input$corte1 == "Pobreza") {
    
    datatable(base_id_tp() %>%
                filter(corte_nueva == "Pobreza") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Pobreza" = pobreza,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  }  else if (input$corte1 == "Quintil de ingreso") {
    

    datatable(base_id_tp() %>%
                filter(corte_nueva == "Quintil de ingreso") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Quintil de ingreso" = quintil,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if (input$corte1 == "Decil de ingreso") {
    
    datatable(base_id_tp() %>%
                filter(corte_nueva == "Decil de ingreso") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Decil de ingreso" = decil,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
    
  } 
  else {
    
    datatable(base_id_tp() %>%
                filter(corte_nueva == "Total") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Valor" = valor),
              rownames = FALSE,options = list(columnDefs = 
                                                list(list(className = 'dt-center', 
                                                          targets = "_all"))))
    
    
  }
  
})


output$tabla_resultado_id_tp_descarga <- downloadHandler(
  
  filename = function() {
    paste0("resultados-", input$indicador_id_tp,"-",input$corte1, ".xlsx", sep = "")
  },
  content = function(file) {
  
  if(input$corte1 == "Región") {
    
    
    openxlsx::write.xlsx(list("Data" = base_id_tp() %>%
                filter(corte_nueva == "Región") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Región" = urbanoruraluy,
                  "Valor" = valor),
                  "Metadata" = base_id_tp() %>%
                  dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                  transmute(
                    "NOMBRE DEL INDICADOR" = nomindicador,
                    "FUENTE" = fuente,
                    "DEFINICIÓN" = definicion,
                    "FORMAS DE CÁLCULO"=calculo,
                    "COBERTURA"= cobertura,
                    "CITA"=cita)%>%sjmisc::rotate_df()),file,
                row.names = TRUE)
                
    
  } else if(input$corte1 == "Sexo del jefe(a)"){
    
    openxlsx::write.xlsx(list( "Data" = base_id_tp() %>%
                                 filter(corte_nueva == "Sexo del jefe(a)") %>%
                                 arrange(anio)%>%
                                 transmute(
                                   "Año" = anio,
                                   "Sexo del jefe(a)" = sexojefatura,
                                   "Valor" = valor),
                               "Metadata" = base_id_tp() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%sjmisc::rotate_df()),file,
                         row.names = TRUE)
    
  } else if (input$corte1 == "Pobreza") {
    
    openxlsx::write.xlsx(list( "Data" = base_id_tp() %>%
                                 filter(corte_nueva == "Pobreza") %>%
                                 arrange(anio)%>%
                                 transmute(
                                   "Año" = anio,
                                   "Pobreza" = pobreza,
                                   "Valor" = valor),
                               "Metadata" = base_id_tp() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%sjmisc::rotate_df()),file,
                         row.names = TRUE)
    
  }  else if (input$corte1 == "Quintil de ingreso") {
    
    
    openxlsx::write.xlsx(list( "Data" = base_id_tp() %>%
                                 filter(corte_nueva == "Quintil de ingreso") %>%
                                 arrange(anio)%>%
                                 transmute(
                                   "Año" = anio,
                                   "Quintil de ingreso" = quintil,
                                   "Valor" = valor),
                               "Metadata" = base_id_tp() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%sjmisc::rotate_df()),file,
                         row.names = TRUE)

    
  } else if (input$corte1 == "Decil de ingreso") {
    
    openxlsx::write.xlsx(list( "Data" = base_id_tp() %>%
                                 filter(corte_nueva == "Decil de ingreso") %>%
                                 arrange(anio)%>%
                                 transmute(
                                   "Año" = anio,
                                   "Decil de ingreso" = decil,
                                   "Valor" = valor),
                               "Metadata" = base_id_tp() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%sjmisc::rotate_df()),file,
                         row.names = TRUE)

    
    
  } 
  else {
    
    openxlsx::write.xlsx(list( "Data" = base_id_tp() %>%
                                 filter(corte_nueva == "Total") %>%
                                 arrange(anio)%>%
                                 transmute(
                                   "Año" = anio,
                                   "Valor" = valor),
                               "Metadata" = base_id_tp() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%sjmisc::rotate_df()),file,
                         row.names = TRUE)
    
    
  }
  

})







output$plot_id_pu <- plotly::renderPlotly({
  
  if(input$corte2 == "Sexo del jefe(a)"){
    
    g1 <- base_id_pu() %>%
      filter(corte_nueva == "Sexo del jefe(a)") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = sexojefatura,
                 text = paste("</br>Año:",anio,"</br>Sexo del jefe(a):",sexojefatura,"</br>Valor:",round(as.numeric(valor),1)))) +
      geom_line(aes(group=sexojefatura),size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(
        x = "",
        y = "")  
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
    
  } else if (input$corte2 == "Quintil de ingreso") {
    
    g1 <- base_id_pu() %>%
      filter(corte_nueva == "Quintil de ingreso") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = quintil,
                 text = paste("</br>Año:",anio,"</br>Quintil de ingreso:",quintil,"</br>Valor:",round(as.numeric(valor),1)))) +
      geom_line(aes(group=quintil),size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(
        x = "",
        y = "")  
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
    
  }   else {
    

    g1 <- base_id_pu() %>%
      filter(corte_nueva=="Total") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), round(as.numeric(valor),1), color=corte_nueva,group = corte_nueva,
                 text = paste("</br>Año:",anio,"</br>Valor:",round(as.numeric(valor),1))
      )) +
      geom_line(size = 1, color = "#476481", alpha = 0.5) +
      geom_point(color = "#476481", size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8),axis.text.x = element_text(angle = 0),
            legend.position = "none") +
      #scale_y_continuous(labels = addUnits) +
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(x = "",
           y = "")
    
    
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
    
    
  }
  
})


output$tabla_resultado_id_pu <- renderDT({
  
  if(input$corte2 == "Sexo del jefe(a)"){
    
    
    datatable(base_id_pu() %>%
                filter(corte_nueva == "Sexo del jefe(a)") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Sexo del jefe(a)" = sexojefatura,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if (input$corte2 == "Quintil de ingreso") {
    

    datatable(base_id_pu() %>%
                filter(corte_nueva == "Quintil de ingreso") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Quintil de ingreso" = quintil,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  }   else {
    
    datatable(base_id_pu() %>%
                filter(corte_nueva == "Total") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  }
  
})


output$tabla_resultado_id_pu_descarga <- downloadHandler(
  
  filename = function() {
    paste0("resultados-", input$indicador_id_pu,"-",input$corte2, ".xlsx", sep = "")
  },
  content = function(file) {
  
  if(input$corte2 == "Sexo del jefe(a)"){
    
    
    
    openxlsx::write.xlsx(list( "Data" = base_id_pu() %>%
                                 filter(corte_nueva == "Sexo del jefe(a)") %>%
                                 arrange(anio)%>%
                                 transmute("Año" = anio,
                                           "Sexo del jefe(a)" = sexojefatura,
                                           "Valor" = valor),
                               "Metadata" = base_id_pu() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%sjmisc::rotate_df()),file,
                         row.names = TRUE)
    
  } else if (input$corte2 == "Quintil de ingreso") {
    
    
    
    openxlsx::write.xlsx(list( "Data" = base_id_pu() %>%
                                 filter(corte_nueva == "Quintil de ingreso") %>%
                                 arrange(anio)%>%
                                 transmute("Año" = anio,
                                           "Quintil de ingreso" = quintil,
                                           "Valor" = valor),
                               "Metadata" = base_id_pu() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%sjmisc::rotate_df()),file,
                         row.names = TRUE)
    
  }   else {
    
    openxlsx::write.xlsx(list( "Data" = base_id_pu() %>%
                                 filter(corte_nueva == "Total") %>%
                                 arrange(anio)%>%
                                 transmute("Año" = anio,
                                           "Valor" = valor),
                               "Metadata" = base_id_pu() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%sjmisc::rotate_df()),file,
                         row.names = TRUE)
    
  }
  
})








base_p <- reactive({
  
  validate(
    need(input$indicador_pobreza != "", "Seleccione un indicador")
  )
  
  df_generica %>%
    filter(categoria == "Pobreza",
           nomindicador == input$indicador_pobreza)%>% 
    mutate(valor = round(valor*100,1))
  
})


output$plot_p <- plotly::renderPlotly({
  
  if(input$corte3 == "Sexo"){
    
    g1 <- base_p() %>%
      filter(corte_nueva == "Sexo") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = sexo,group=sexo,
                 text = paste("</br>Año:",anio,"</br>Sexo:",sexo,"</br>Valor:",round(as.numeric(valor),1)))) +
      geom_line(size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(
        x = "",
        y = "")  
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
    
  } else if (input$corte3 == "Ascendencia étnico-racial") {
    

    
    g1 <- base_p() %>%
      filter(corte_nueva == "Ascendencia étnico-racial") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = ascendencia,group = ascendencia,group=ascendencia,
                 text = paste("</br>Año:",anio,"</br>Ascendencia étnico-racial:",ascendencia,"</br>Valor:",round(as.numeric(valor),1)))) +
      geom_line(size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(
        x = "",
        y = "")  
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
    
  } else if (input$corte3 == "Tramo de edad") {
    

    g1 <- base_p() %>%
      filter(corte_nueva == "Tramo de edad" & corte_nueva != "Edad") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = tramo,group=tramo,
                 text = paste("</br>Año:",anio,"</br>Tramo de edad",tramo,"</br>Valor:",round(as.numeric(valor),1)))) +
      geom_line(size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(
        x = "",
        y = "")  
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
    
  } else if (input$corte3 == "Sexo del jefe(a)") {
    
    g1 <- base_p() %>%
      filter(corte_nueva == "Sexo del jefe(a)") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = sexojefatura,group=sexojefatura,
                 text = paste("</br>Año:",anio,"</br>Sexo del jefe(a)",sexojefatura,"</br>Valor:",round(as.numeric(valor),1)))) +
      geom_line(size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(
        x = "",
        y = "")  
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
    
  }  
  
  else {

    
    g1 <- base_p() %>%
      filter(corte_nueva=="Total") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), round(as.numeric(valor),1), color=urbanoruraluy,group=urbanoruraluy,
                 text = paste("</br>Año:",anio,"</br>Total",urbanoruraluy,"</br>Valor:",round(as.numeric(valor),1))
      )) +
      geom_line(size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      #scale_y_continuous(labels = addUnits) +
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(x = "",
           y = "")

    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
    
    
  }
  
})



output$tabla_resultado_p <- renderDT({
  
  if(input$corte3 == "Sexo"){
    
    datatable(base_p() %>%
                filter(corte_nueva == "Sexo") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Sexo" = sexo,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if (input$corte3 == "Ascendencia étnico-racial") {
    
    datatable(base_p() %>%
                filter(corte_nueva == "Ascendencia étnico-racial") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Ascendencia étnico-racial" = ascendencia,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if (input$corte3 == "Tramo de edad") {
    
    datatable(base_p() %>%
                filter(corte_nueva == "Tramo de edad") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Tramo de edad" = tramo,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if (input$corte3 == "Sexo del jefe(a)") {
    
    datatable(base_p() %>%
                filter(corte_nueva == "Sexo del jefe(a)") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Sexo del jefe(a)" = sexojefatura,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } 
  
  else {
    
    
  datatable(base_p() %>%
                filter(corte_nueva == "Total") %>%
                arrange(anio)%>%
                transmute("Año" = anio,
                          "Región" = urbanoruraluy,
                          "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
    
  }
  
})


output$tabla_resultado_p_descarga <- downloadHandler(
  
  filename = function() {
    paste0("resultados-", input$indicador_pobreza,"-",input$corte3, ".xlsx", sep = "")
  },
  content = function(file) {
  
  if(input$corte3 == "Sexo"){
    

    openxlsx::write.xlsx(list( "Data" = base_p() %>%
                                 filter(corte_nueva == "Sexo") %>%
                                 arrange(anio)%>%
                                 transmute("Año" = anio,
                                           "Sexo" = sexo,
                                           "Valor" = valor),
                               "Metadata" = base_p() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%sjmisc::rotate_df()),file,
                         row.names = TRUE)
    
  } else if (input$corte3 == "Ascendencia étnico-racial") {
    
    
    openxlsx::write.xlsx(list( "Data" = base_p() %>%
                                 filter(corte_nueva == "Ascendencia étnico-racial") %>%
                                 arrange(anio)%>%
                                 transmute("Año" = anio,
                                           "Ascendencia étnico-racial" = ascendencia,
                                           "Valor" = valor),
                               "Metadata" = base_p() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%sjmisc::rotate_df()),file,
                         row.names = TRUE)
    
  } else if (input$corte3 == "Tramo de edad") {
    
    openxlsx::write.xlsx(list( "Data" = base_p() %>%
                                 filter(corte_nueva == "Tramo de edad") %>%
                                 arrange(anio)%>%
                                 transmute("Año" = anio,
                                           "Tramo de edad" = tramo,
                                           "Valor" = valor),
                               "Metadata" = base_p() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%sjmisc::rotate_df()),file,
                         row.names = TRUE)
    
  } else if (input$corte3 == "Sexo del jefe(a)") {
    
    openxlsx::write.xlsx(list( "Data" = base_p() %>%
                                 filter(corte_nueva == "Sexo del jefe(a)") %>%
                                 arrange(anio)%>%
                                 transmute("Año" = anio,
                                           "Sexo del jefe(a)" = sexojefatura,
                                           "Valor" = valor),
                               "Metadata" = base_p() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%sjmisc::rotate_df()),file,
                         row.names = TRUE)
    
  } 
  
  else {
    
    
    openxlsx::write.xlsx(list( "Data" = base_p() %>%
                                 filter(corte_nueva == "Total") %>%
                                 arrange(anio)%>%
                                 transmute("Año" = anio,
                                           "Región" = urbanoruraluy,
                                           "Valor" = valor),
                               "Metadata" = base_p() %>%
                                 dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                 transmute(
                                   "NOMBRE DEL INDICADOR" = nomindicador,
                                   "FUENTE" = fuente,
                                   "DEFINICIÓN" = definicion,
                                   "FORMAS DE CÁLCULO"=calculo,
                                   "COBERTURA"= cobertura,
                                   "CITA"=cita)%>%sjmisc::rotate_df()),file,
                         row.names = TRUE)
    
    
  }
  
})









base_nbi_p <- reactive({
  
  validate(
    need(input$indicador_nbi_p != "", "Seleccione un indicador")
  )
  
  df_generica %>%
    filter(categoria == "Necesidades Básicas",
           pestana=="Total País",
           nomindicador == input$indicador_nbi_p)%>% 
    mutate(valor = round(valor*100,1))
  
})


base_nbi_h <- reactive({
  
  validate(
    need(input$indicador_nbi_h != "", "Seleccione un indicador")
  )
  
  
  df_generica %>%
    filter(categoria == "Necesidades Básicas",
           pestana=="País Urbano",
           nomindicador == input$indicador_nbi_h)%>% 
  mutate(valor = round(valor*100,1))
  
})


##NBI TOTAL PAÍS


output$plot_nbi_p <- plotly::renderPlotly({

  if(input$corte4 == "Región") {


    g1 <- base_nbi_p() %>%
      filter(corte_nueva == "Región") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = urbanoruraluy,group=urbanoruraluy,
                 text = paste("</br>Año:",anio,"</br>Región:",urbanoruraluy,"</br>Valor:",round(as.numeric(valor),1)))) +
      geom_line(size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(
        x = "",
        y = "")  

    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    


  } else if(input$corte4 == "Sexo"){


    g1 <- base_nbi_p() %>%
      filter(corte_nueva == "Sexo") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = sexo,group=sexo,
                 text = paste("</br>Año:",anio,"</br>Sexo:",sexo,"</br>Valor:",round(as.numeric(valor),1)))) +
      geom_line(size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(
        x = "",
        y = "")  
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    

  } else if (input$corte4 == "Ascendencia étnico-racial") {


    g1 <- base_nbi_p() %>%
      filter(corte_nueva == "Ascendencia étnico-racial") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = ascendencia,group=ascendencia,
                 text = paste("</br>Año:",anio,"</br>Ascendencia étnico-racial:",ascendencia,"</br>Valor:",round(as.numeric(valor),1)))) +
      geom_line(size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(
        x = "",
        y = "")  
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)

  } else if (input$corte4 == "Tramo de edad") {


    g1 <- base_nbi_p() %>%
      filter(corte_nueva == "Tramo de edad") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = tramo,group=tramo,
                 text = paste("</br>Año:",anio,"</br>Tramo de edad:",tramo,"</br>Valor:",round(as.numeric(valor),1)))) +
      geom_line(size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(
        x = "",
        y = "")  
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)

  } else if (input$corte4 == "Pobreza") {

    g1 <- base_nbi_p() %>%
      filter(corte_nueva == "Pobreza") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = pobreza,group=pobreza,
                 text = paste("</br>Año:",anio,"</br>Pobreza:",pobreza,"</br>Valor:",round(as.numeric(valor),1)))) +
      geom_line(size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(
        x = "",
        y = "")  
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
  } else if (input$corte4 == "Quintil de ingreso") {

    g1 <- base_nbi_p() %>%
      filter(corte_nueva == "Quintil de ingreso") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = quintil,group=quintil,
                 text = paste("</br>Año:",anio,"</br>Quintil de ingreso:",quintil,"</br>Valor:",round(as.numeric(valor),1)))) +
      geom_line(size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(
        x = "",
        y = "")  
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    

  }

  else {

        g1 <- base_nbi_p() %>%
      filter(corte_nueva == "Total") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = urbanoruraluy,group=urbanoruraluy,
                 text = paste("</br>Año:",anio,"</br>Región:",urbanoruraluy,"</br>Valor:",round(as.numeric(valor),1)))) +
      geom_line(size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(
        x = "",
        y = "")  
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
    
  }

})


output$tabla_resultado_nbi_p <- renderDT({
  
  if(input$corte4 == "Región") {
    
    
    datatable(base_nbi_p() %>%
                filter(corte_nueva == "Región") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Región" = urbanoruraluy,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
    
  } else if(input$corte4 == "Sexo"){
    
    
    datatable(base_nbi_p() %>%
                filter(corte_nueva == "Sexo") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Sexo" = sexo,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if (input$corte4 == "Ascendencia étnico-racial") {

    
    datatable(base_nbi_p() %>%
                filter(corte_nueva == "Ascendencia étnico-racial") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Ascendencia étnico-racial" = ascendencia,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if (input$corte4 == "Tramo de edad") {
    
    datatable(base_nbi_p() %>%
                filter(corte_nueva == "Tramo de edad") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Tramo de edad" = tramo,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if (input$corte4 == "Pobreza") {
    
    datatable(base_nbi_p() %>%
                filter(corte_nueva == "Pobreza") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Pobreza" = pobreza,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if (input$corte4 == "Quintil de ingreso") {
  
    
    datatable(base_nbi_p() %>%
                filter(corte_nueva == "Quintil de ingreso") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Quintil de ingreso" = quintil,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  }
  
  else {
    
    datatable(base_nbi_p() %>%
                filter(corte_nueva == "Total") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Región"= urbanoruraluy,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  }
  
})




output$tabla_resultado_nbi_p_descarga <- downloadHandler(
  
  filename = function() {
    paste0("resultados-", input$indicador_nbi_p,"-",input$corte4, ".xlsx", sep = "")
  },
  content = function(file) {
        
        if(input$corte4 == "Región") {
          
          
          openxlsx::write.xlsx(list( "Data" = base_nbi_p() %>%
                                       filter(corte_nueva == "Región") %>%
                                       arrange(anio)%>%
                                       transmute("Año" = anio,
                                                 "Región"= urbanoruraluy,
                                                 "Valor" = valor),
                                     "Metadata" = base_nbi_p() %>%
                                       dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                       transmute(
                                         "NOMBRE DEL INDICADOR" = nomindicador,
                                         "FUENTE" = fuente,
                                         "DEFINICIÓN" = definicion,
                                         "FORMAS DE CÁLCULO"=calculo,
                                         "COBERTURA"= cobertura,
                                         "CITA"=cita)%>%sjmisc::rotate_df()),file,
                               row.names = TRUE)
          
          
        } else if(input$corte4 == "Sexo"){
          
          
          openxlsx::write.xlsx(list( "Data" = base_nbi_p() %>%
                                       filter(corte_nueva == "Sexo") %>%
                                       arrange(anio)%>%
                                       transmute("Año" = anio,
                                                 "Sexo"= sexo,
                                                 "Valor" = valor),
                                     "Metadata" = base_nbi_p() %>%
                                       dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                       transmute(
                                         "NOMBRE DEL INDICADOR" = nomindicador,
                                         "FUENTE" = fuente,
                                         "DEFINICIÓN" = definicion,
                                         "FORMAS DE CÁLCULO"=calculo,
                                         "COBERTURA"= cobertura,
                                         "CITA"=cita)%>%sjmisc::rotate_df()),file,
                               row.names = TRUE)
          
        } else if (input$corte4 == "Ascendencia étnico-racial") {
          
          
          openxlsx::write.xlsx(list( "Data" = base_nbi_p() %>%
                                       filter(corte_nueva == "Ascendencia étnico-racial") %>%
                                       arrange(anio)%>%
                                       transmute("Año" = anio,
                                                 "Ascendencia étnico-racial"= ascendencia,
                                                 "Valor" = valor),
                                     "Metadata" = base_nbi_p() %>%
                                       dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                       transmute(
                                         "NOMBRE DEL INDICADOR" = nomindicador,
                                         "FUENTE" = fuente,
                                         "DEFINICIÓN" = definicion,
                                         "FORMAS DE CÁLCULO"=calculo,
                                         "COBERTURA"= cobertura,
                                         "CITA"=cita)%>%sjmisc::rotate_df()),file,
                               row.names = TRUE)
          
        } else if (input$corte4 == "Tramo de edad") {
          
          openxlsx::write.xlsx(list( "Data" = base_nbi_p() %>%
                                       filter(corte_nueva == "Tramo de edad") %>%
                                       arrange(anio)%>%
                                       transmute("Año" = anio,
                                                 "Tramo de edad"= tramo,
                                                 "Valor" = valor),
                                     "Metadata" = base_nbi_p() %>%
                                       dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                       transmute(
                                         "NOMBRE DEL INDICADOR" = nomindicador,
                                         "FUENTE" = fuente,
                                         "DEFINICIÓN" = definicion,
                                         "FORMAS DE CÁLCULO"=calculo,
                                         "COBERTURA"= cobertura,
                                         "CITA"=cita)%>%sjmisc::rotate_df()),file,
                               row.names = TRUE)
          
        } else if (input$corte4 == "Pobreza") {
          
          openxlsx::write.xlsx(list( "Data" = base_nbi_p() %>%
                                       filter(corte_nueva == "Pobreza") %>%
                                       arrange(anio)%>%
                                       transmute("Año" = anio,
                                                 "Pobreza"= pobreza,
                                                 "Valor" = valor),
                                     "Metadata" = base_nbi_p() %>%
                                       dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                       transmute(
                                         "NOMBRE DEL INDICADOR" = nomindicador,
                                         "FUENTE" = fuente,
                                         "DEFINICIÓN" = definicion,
                                         "FORMAS DE CÁLCULO"=calculo,
                                         "COBERTURA"= cobertura,
                                         "CITA"=cita)%>%sjmisc::rotate_df()),file,
                               row.names = TRUE)
          
        } else if (input$corte4 == "Quintil de ingreso") {
          
          
          openxlsx::write.xlsx(list( "Data" = base_nbi_p() %>%
                                       filter(corte_nueva == "Quintil de ingreso") %>%
                                       arrange(anio)%>%
                                       transmute("Año" = anio,
                                                 "Quintil de ingreso"= quintil,
                                                 "Valor" = valor),
                                     "Metadata" = base_nbi_p() %>%
                                       dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                       transmute(
                                         "NOMBRE DEL INDICADOR" = nomindicador,
                                         "FUENTE" = fuente,
                                         "DEFINICIÓN" = definicion,
                                         "FORMAS DE CÁLCULO"=calculo,
                                         "COBERTURA"= cobertura,
                                         "CITA"=cita)%>%sjmisc::rotate_df()),file,
                               row.names = TRUE)
          
        }
        
        else {
          
          openxlsx::write.xlsx(list( "Data" = base_nbi_p() %>%
                                       filter(corte_nueva == "Total") %>%
                                       arrange(anio)%>%
                                       transmute("Año" = anio,
                                                  "Región"= urbanoruraluy,
                                                 "Valor" = valor),
                                     "Metadata" = base_nbi_p() %>%
                                       dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                       transmute(
                                         "NOMBRE DEL INDICADOR" = nomindicador,
                                         "FUENTE" = fuente,
                                         "DEFINICIÓN" = definicion,
                                         "FORMAS DE CÁLCULO"=calculo,
                                         "COBERTURA"= cobertura,
                                         "CITA"=cita)%>%sjmisc::rotate_df()),file,
                               row.names = TRUE)
          
        }
        
      })



##País urbano

output$plot_nbi_h <- plotly::renderPlotly({

  if(input$corte5 == "Sexo") {

    
    g1 <- base_nbi_h() %>%
      filter(corte_nueva == "Sexo") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = sexo,group=sexo,
                 text = paste("</br>Año:",anio,"</br>Sexo:",sexo,"</br>Valor:",round(as.numeric(valor),1)))) +
      geom_line(size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(
        x = "",
        y = "")  
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    


  } else if(input$corte5 == "Tramo de edad"){


    g1 <- base_nbi_h() %>%
      filter(corte_nueva == "Tramo de edad") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = tramo,group=tramo,
                 text = paste("</br>Año:",anio,"</br>Tramo de edad:",tramo,"</br>Valor:",round(as.numeric(valor),1)))) +
      geom_line(size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(
        x = "",
        y = "")  
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    

  } else if(input$corte5 == "Quintil de ingreso"){

    g1 <- base_nbi_h() %>%
      filter(corte_nueva == "Quintil de ingreso") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = quintil,group=quintil,
                 text = paste("</br>Año:",anio,"</br>Quintil de ingreso:",quintil,"</br>Valor:",round(as.numeric(valor),1)))) +
      geom_line(size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(
        x = "",
        y = "")  
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    

  }else {
    
    g1 <- base_nbi_h() %>%
      filter(corte_nueva == "Total") %>%
      ggplot(aes(as.Date(as.character(anio), format = "%Y"), valor, color = urbanoruraluy,group=urbanoruraluy,
                 text = paste("</br>Año:",anio,"</br>Región:",urbanoruraluy,"</br>Valor:",round(as.numeric(valor),1)))) +
      geom_line(size = 1, alpha = 0.5) +
      geom_point( size = 2.5) +
      scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
      theme(axis.text = element_text(size = 8))+
      scale_color_viridis(name = " ", discrete = T, direction = 1)+
      labs(
        x = "",
        y = "")  
    
    plotly::ggplotly(g1, width = (0.60*as.numeric(input$dimension[1])), height = 0.70*as.numeric(input$dimension[2]),
                     hoverinfo = 'text',tooltip = c("text"))%>%
      plotly::layout(legend = list(orientation = "h",   
                                   xanchor = "center",  
                                   x = 0.5,y=-0.2)) %>%
      
      plotly::config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list(
                       "pan2d",
                       "autoScale2d",
                       "resetScale2d",
                       "hoverClosestCartesian",
                       "hoverCompareCartesian",
                       "sendDataToCloud",
                       "toggleHover",
                       "resetViews",
                       "toggleSpikelines",
                       "resetViewMapbox"
                     ),showLink = FALSE,
                     displaylogo = FALSE)
    
  }

  


})

output$tabla_resultado_nbi_h <- renderDT({
  
  if(input$corte5 == "Sexo") {
    
    datatable(base_nbi_h() %>%
                filter(corte_nueva == "Sexo") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Sexo" = sexo,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
    
  } else if(input$corte5 == "Tramo de edad"){
  
    
    datatable(base_nbi_h() %>%
                filter(corte_nueva == "Tramo de edad") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Tramo de edad" = tramo,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  } else if(input$corte5 == "Quintil de ingreso"){
    
    datatable(base_nbi_h() %>%
                filter(corte_nueva == "Quintil de ingreso") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Quintil de ingreso" = quintil,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
    
  }else {
    
    datatable(base_nbi_h() %>%
                filter(corte_nueva == "Total") %>%
                arrange(anio)%>%
                transmute(
                  "Año" = anio,
                  "Región" = urbanoruraluy,
                  "Valor" = valor),
              rownames = FALSE,
              options = list(columnDefs = 
                               list(list(className = 'dt-center', 
                                         targets = "_all"))))
  }
  
  
  
  
})


output$tabla_resultado_nbi_h_descarga <- downloadHandler(
  
  filename = function() {
    paste0("resultados-", input$indicador_nbi_h,"-",input$corte5, ".xlsx", sep = "")
  },
  content = function(file) {

    if(input$corte5 == "Sexo") {
      
      openxlsx::write.xlsx(list( "Data" = base_nbi_h() %>%
                                   filter(corte_nueva == "Sexo") %>%
                                   arrange(anio)%>%
                                   transmute("Año" = anio,
                                             "Sexo" = sexo,
                                             "Valor" = valor),
                                 "Metadata" = base_nbi_h() %>%
                                   dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                   transmute(
                                     "NOMBRE DEL INDICADOR" = nomindicador,
                                     "FUENTE" = fuente,
                                     "DEFINICIÓN" = definicion,
                                     "FORMAS DE CÁLCULO"=calculo,
                                     "COBERTURA"= cobertura,
                                     "CITA"=cita)%>%sjmisc::rotate_df()),file,
                           row.names = TRUE)
      
      
    } else if(input$corte5 == "Tramo de edad"){
      
      
      openxlsx::write.xlsx(list( "Data" = base_nbi_h() %>%
                                   filter(corte_nueva == "Tramo de edad") %>%
                                   arrange(anio)%>%
                                   transmute("Año" = anio,
                                             "Tramo de edad" = tramo,
                                             "Valor" = valor),
                                 "Metadata" = base_nbi_h() %>%
                                   dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                   transmute(
                                     "NOMBRE DEL INDICADOR" = nomindicador,
                                     "FUENTE" = fuente,
                                     "DEFINICIÓN" = definicion,
                                     "FORMAS DE CÁLCULO"=calculo,
                                     "COBERTURA"= cobertura,
                                     "CITA"=cita)%>%sjmisc::rotate_df()),file,
                           row.names = TRUE)
      
    } else if(input$corte5 == "Quintil de ingreso"){
      
      openxlsx::write.xlsx(list( "Data" = base_nbi_h() %>%
                                   filter(corte_nueva == "Quintil de ingreso") %>%
                                   arrange(anio)%>%
                                   transmute("Año" = anio,
                                             "Quintil de ingreso" = quintil,
                                             "Valor" = valor),
                                 "Metadata" = base_nbi_h() %>%
                                   dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                   transmute(
                                     "NOMBRE DEL INDICADOR" = nomindicador,
                                     "FUENTE" = fuente,
                                     "DEFINICIÓN" = definicion,
                                     "FORMAS DE CÁLCULO"=calculo,
                                     "COBERTURA"= cobertura,
                                     "CITA"=cita)%>%sjmisc::rotate_df()),file,
                           row.names = TRUE)
      
    }else {
      
      openxlsx::write.xlsx(list( "Data" = base_nbi_h() %>%
                                   filter(corte_nueva == "Total") %>%
                                   arrange(anio)%>%
                                   transmute("Año" = anio,
                                             "Región" = urbanoruraluy,
                                             "Valor" = valor),
                                 "Metadata" = base_nbi_h() %>%
                                   dplyr::distinct(nomindicador,fuente,definicion,calculo,cobertura,cita)%>%
                                   transmute(
                                     "NOMBRE DEL INDICADOR" = nomindicador,
                                     "FUENTE" = fuente,
                                     "DEFINICIÓN" = definicion,
                                     "FORMAS DE CÁLCULO"=calculo,
                                     "COBERTURA"= cobertura,
                                     "CITA"=cita)%>%sjmisc::rotate_df()),file,
                           row.names = TRUE)
    }
    
    
    
    
  })



  }

  
shinyApp(ui = ui, server = server)

