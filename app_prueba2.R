
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
library(ggplot2) 
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
                      
                                          ))))))
                          









# Definimos el servidor  
server <- function(input, output, session) {  
  

   base_CE_tp <- reactive({
     df %>%
       filter(CATEGORIA == "Composición y estructura de los hogares" & (PESTAÑA== "Total país" |
              (`Ascendencia étnico racial`=="Todos"&
              `Beneficios laborales`=="Todos"&
              Departamento=="Todos"&
              Edad=="Todos"&
              `Edad al tener el primer hijo`=="Todos"&
              `Nivel educativo`=="Todos"&
              `Nivel socioeconómico`=="Todos"&
              Pobreza=="Todos"&
              `Quintil de ingresos`=="Todos"&
              Sexo=="Todos"&Región=="Urbano (más de 5.000 habitantes)"&FECHA>=2006))&
              NOMINDICADOR == input$indicador_CE_tp) %>%
       mutate(VALOR = as.numeric(VALOR))
     
   }) 
   

   metadataR <- reactive({
     df %>%
       filter(NOMINDICADOR == input$indicador_CE_tp) 
   }) 
   
   
   output$selectcorte1 <- renderUI({
     if(input$indicador_CE_tp == "Tasa de divorcios"| input$indicador_CE_tp == "Tasa de nupcialidad") {
    
        } else {
     
       selectInput("corte1", "Resultados por:", choices = unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CE_tp]),
                 selected = "Total")
    }
   })
   
   
   
   output$anio1 <- renderUI({
     
   tipo_graf <- rlang::sym(rlang::as_string(base_CE_tp()$tipo_graf2[1]))

     if(tipo_graf == "1") {
            } else if(input$corte1!="Total"&(tipo_graf == "2" | tipo_graf == "3")) {
                  selectInput("anio1", "Año:",
                   choices = unique(base_CE_tp()$FECHA2[base_CE_tp()$NOMINDICADOR ==input$indicador_CE_tp])
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
   
   
   ## TITULOS
   
   
       output$title_indicador_CE_tp <- renderUI({ 
         helpText(HTML(input$indicador_CE_tp))
       })
   

    ## SUB- TÍTULO
       
       output$subtitle_indicador_CE_tp <- renderUI({ 

         subtitle = unique(metadata %>%
                           filter(NOMINDICADOR == input$indicador_CE_tp) %>% pull(RELEVANCIA))

         helpText(HTML(subtitle))
         
         })
       
    
       # Calculo
       output$calculo_CE_tp <- renderUI({ 
         
         calculo = unique(metadata %>%
                             filter(NOMINDICADOR == input$indicador_CE_tp) %>% pull(calculo))
         
         helpText(HTML(paste("<b> Forma de cálculo:</b>", calculo)))
         
       })
       
       
       # Observaciones
       
       output$info_CE_tp <- renderUI({ 
         
         info = unique(metadata %>%
                            filter(NOMINDICADOR == input$indicador_CE_tp) %>% pull(NOTAS2))
         
         helpText(HTML(paste("<b> Observaciones:</b>", info)))
         
       })
       
      
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
         

         
         
         
       } else if(substring(input$indicador_CE_tp,1,34) == "Distribución porcentual de jóvenes" & input$corte1 == "Total"){
         # Gráficos de barras para totales de indicadores ENAJ
         
         dat <- base_CE_tp() %>% filter(NOMINDICADOR == input$indicador_CE_tp & is.na(CORTE1) == T) 
         cut <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_CE_tp])))

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
             title = input$indicador_CE_tp,
             caption = str_wrap(paste("Fuente:",unique(metadata %>%
                                                         filter(NOMINDICADOR == input$indicador_CE_tp) %>% pull(FUENTE))), width = 160),
             x = "",
             y = "") +
           scale_fill_brewer(name = "", palette = paleta)
         
         
       } else if(substring(input$indicador_CE_tp,1,34) == "Distribución porcentual de jóvenes" & input$corte1 != "Total") {
         
         # Gráficos de barras para cortes de indicadores con ENAJ (Transparencia por cambio metodológico)
         

         dat <- base_CE_tp() %>% filter(NOMINDICADOR == input$indicador_CE_tp & CORTE1_rec == input$corte1)   %>%
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
             y = "") +
           scale_fill_brewer(name = "", palette = paleta)
         
        
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
}


                                                                      
                                                                                             
# Ejecutar la aplicación Shiny  
shinyApp(ui = ui, server = server)
                    
                    

                    