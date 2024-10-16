
library(shinyWidgets)
library(tidyverse)
library(janitor)
library(plotly)
library(DT)
library(shiny)
library(ggtext)
library(stringr)
library(shinyWidgets)
#library(ggh4x)
library(ggrepel)
library(shinycssloaders)



source('utils.R')

theme_set(theme_bdd())
update_geom_defaults("text", list(family = theme_get()$text$family))

#indicadores=read.csv2("indicadores_omif_2.csv")
#save(indicadores,file="indicadores.RData")
load("indicadores.RData")

indicadores =  indicadores%>%
  filter((nomindicador =="PERSONAS SEGÚN CONDICIÓN MIGRATORIA Y GRUPO DE EDAD" & anio=="2021")==F)%>%
  filter((nomindicador =="PORCENTAJE DE POBLACIÓN AFRO SEGÚN CONDICIÓN MIGRATORIA" & anio=="2021")==F)%>%
  filter((nomindicador =="PORCENTAJE DE MUJERES SEGÚN CONDICIÓN MIGRATORIA" & anio=="2021")==F)


load("RRAA/indicadores_rraa.RData")
rraa= rraa %>%
  mutate(pais=factor(pais,levels = c("Argentina", "Bolivia","Brasil","Chile","Colombia","Ecuador","Paraguay","Perú","Venezuela","Cuba",
                                     "República Dominicana","Resto Caribe","Centro América","Estados Unidos","Resto América del Norte",
                                     "España","Italia","Francia","Resto Europa occidental","Europa del Este",
                                     "Africa","Asia","Oceanía", "Otros","No especificado"),
                     labels = c("Argentina", "Bolivia","Brasil","Chile","Colombia","Ecuador","Paraguay","Perú","Venezuela","Cuba",
                                "República Dominicana","Resto Caribe","Centro América","Estados Unidos","Resto América del Norte",
                                "España","Italia","Francia","Resto Europa occidental","Europa del Este",
                                "Africa","Asia","Oceanía","Otros","No especificado"),ordered =TRUE))%>%
  
  mutate(depto=factor(depto,levels = c("Artigas", "Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja","Maldonado","Montevideo",
                                     "Paysandú","Rio Negro","Río Negro","Rivera","Rocha",
                                     "Salto","San Jose","San José","Soriano","Tacuarembó",
                                     "Treinta y Tres","Treinta Y Tres","Extranjero", "No especificado","Sin dato cargado"),
                     labels = c("Artigas", "Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja","Maldonado","Montevideo",
                                "Paysandú","Rio Negro","Río Negro","Rivera","Rocha",
                                "Salto","San Jose","San José","Soriano","Tacuarembó",
                                "Treinta y Tres","Treinta Y Tres","Extranjero", "No especificado","Sin dato cargado"),ordered =TRUE))%>%
  
  filter((subdimension =="Defunciones" & anio=="2016")==F)%>%
  filter((subdimension =="Nacimientos" & anio=="1998")==F)%>%
  filter((nomindicador =="Cantidad de personas en condición de refugiadas según sexo por país de nacionalidad" 
          & anio%in%c(as.character(2005:2017)))==F)%>%
  filter((nomindicador =="Distribución porcentual de personas en condición de refugiadas según sexo por país de nacionalidad" 
          & anio%in%c(as.character(2005:2017)))==F)%>%
  filter((nomindicador =="Cantidad de personas en condición de refugiadas según edad por país de nacionalidad" 
          & anio%in%c(as.character(2005:2017)))==F)%>%
  filter((nomindicador =="Distribución porcentual de personas en condición de refugiadas según edad por país de nacionalidad" 
          & anio%in%c(as.character(2005:2017)))==F)%>%
  filter((nomindicador=="Distribución porcentual de personas con nacionalidad extranjera que accedieron a una cédulas de identidad según edad por país de nacionalidad"
          & valor ==1)==F)%>%
  filter((nomindicador=="Distribución porcentual de personas con nacionalidad extranjera que accedieron a una cédulas de identidad según sexo por país de nacionalidad"
          & valor ==1)==F)%>%
  filter(movimiento_ineg=="Ingresos"|movimiento_ineg=="Egresos"|is.na(movimiento_ineg)==T)

metadata=openxlsx::read.xlsx("RRAA/metadata_rraa.xlsx",sheet = "Indicadores")%>%
  filter(Nombre!="Distribución porcentual de personas con nacionalidad extranjera que accedieron a una cédulas de identidad según calidad de residencia")

metadata$Nombre=stringr::str_trim(metadata$Nombre, side = c("both"))
metadata$Nombre=stringr::str_squish(metadata$Nombre)
metadata$Nota.metodológica=ifelse(is.na(metadata$Nota.metodológica)==T,"-",metadata$Nota.metodológica)

excep= metadata %>% select(Nombre,Excepc)%>% filter(is.na(Excepc)==F & Excepc==1)%>%pull(Nombre)
excep2= metadata %>% select(Nombre,Excepc)%>% filter(is.na(Excepc)==F & Excepc==2)%>%pull(Nombre)
excep3= metadata %>% select(Nombre,Excepc)%>% filter(is.na(Excepc)==F & Excepc==3)%>%pull(Nombre)
excep4= metadata %>% select(Nombre,Excepc)%>% filter(is.na(Excepc)==F & Excepc==4)%>%pull(Nombre)



#depto=geouy::load_geouy("Departamentos")
load("depto.RData")
options(shiny.sanitize.errors = TRUE) 

ui <- tagList(
  
  
  tags$head(tags$script(type = "text/javascript", src = "code.js") ),
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"), # Quita mensajes de error (ojo)
  
  navbarPage(
    theme = "estilo_bdd.css",
    title = "Observatorio de Movilidad, Infancia y Familia",
    collapsible = FALSE,
    fluid = TRUE,
    
    tabPanel(
      title = "Registros administrativos",
      value = 'borelito',
      
      
      sidebarPanel(uiOutput("dimensiones_rraa"),
                   uiOutput("subdimensiones_rraa"),
                   uiOutput("indicadores_rraa"),
                   uiOutput("anio_rraa"),
                   uiOutput("selec_pais")),
      mainPanel(
        tags$style(type = "text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        tags$h3(style="display:inline-block;font-size:18px;",
                uiOutput("title_rraa")),
        div(style="display:inline-block", 
            dropdown(
              style = "minimal",
              status = "primary",
              width = "300px",
              right = TRUE,
              icon = icon("info", lib = "font-awesome"),
              uiOutput("nota_met"),
              animate = animateOptions(
                enter = animations$fading_entrances$fadeInLeftBig,
                exit = animations$fading_exits$fadeOutRightBig))),
        br(),br(),
        plotOutput("barPlot_rraa", height = "600px")%>% withSpinner(color="#5b6f8a"),
        tags$h3(style="display:inline-block;font-size:14px;",
                uiOutput("fuente_rraa")),
        br(),br(),
        downloadButton(outputId = "baja_barPlot_rraa", label = "Descargá el gráfico"),
        br(),
        br(),
        DTOutput("tabla_resultado_rraa"),
        br(),
        downloadButton("dl_tabla_resultado_rraa", "Descargá la tabla"),
        br(),
        br())          
      
      
      
      
      
      
    ),
    tabPanel(
      title = "Encuesta Continua de Hogares",
      value = 'borelito',
    
      
    sidebarPanel(selectInput("dimension",
                  "Seleccione la dimensión:",
                  choices = c("Caracterización Sociodemográfica","Mercado Laboral",
                              "Pobreza")),
    
      #uiOutput("side_dimensiones"),
      conditionalPanel(
        condition = "input.dimension == 'Caracterización Sociodemográfica'",
      
      
                   selectInput(
                     "indicador_csd",
                     "Seleccione el indicador:",
                     choices = unique(indicadores %>% 
                                        filter(pestana == "Caracterización Sociodemográfica") %>%
                                        mutate(nomindicador = fct_relevel(nomindicador, 
                                                                          "PERSONAS SEGÚN CONDICIÓN MIGRATORIA", 
                                                                          "PERSONAS SEGÚN CONDICIÓN MIGRATORIA Y LUGAR DE RESIDENCIA (MONTEVIDEO E INTERIOR)",
                                                                          "PERSONAS SEGÚN CONDICIÓN MIGRATORIA Y GRUPO DE EDAD",
                                                                          "PORCENTAJE DE MUJERES SEGÚN CONDICIÓN MIGRATORIA",   
                                                                          "PORCENTAJE DE POBLACIÓN AFRO SEGÚN CONDICIÓN MIGRATORIA", 
                                                                          "DISTRIBUCIÓN RELATIVA POR NIVEL EDUCATIVO ALCANZADO SEGÚN CONDICIÓN MIGRATORIA PARA LAS PERSONAS DE 25 AÑOS Y MÁS",
                                                                          "DISTRIBUCIÓN RELATIVA DE LA INMIGRACIÓN RECIENTE POR LUGAR DE NACIMIENTO",                                         
                                                                          "DISTRIBUCIÓN RELATIVA DEL RETORNO RECIENTE POR LUGAR DE RESIDENCIA CINCO AÑOS ANTES",
                                                                          "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN DE 0 A 17 AÑOS POR ARREGLOS RESIDENCIALES SEGÚN GENERACIÓN MIGRATORIA",
                                                                          "PERSONAS DE 0 A 17 AÑOS VINCULADAS A LA INMIGRACIÓN EXTRANJERA SEGÚN GENERACIÓN MIGRATORIA")) %>% 
                                        arrange(nomindicador) %>% 
                                        pull(nomindicador))),
                   
                   uiOutput("anio_csd"),          
                   dropdown(
                     style = "bordered", 
                     status = "primary", 
                     icon = icon("warning-sign", lib = "glyphicon"),
                     uiOutput("texto1"),
                     animate = animateOptions(
                       enter = animations$fading_entrances$fadeInLeftBig,
                       exit = animations$fading_exits$fadeOutRightBig)),
                   dropdown(
                     style = "bordered", 
                     status = "primary", 
                     icon = icon("info-sign", lib = "glyphicon"),
                     uiOutput("texto4"),
                     animate = animateOptions(
                       enter = animations$fading_entrances$fadeInLeftBig,
                       exit = animations$fading_exits$fadeOutRightBig))),
      
      conditionalPanel(
        condition = "input.dimension == 'Mercado Laboral'",
        
        selectInput(
          "indicador_mdt",
          "Seleccione el indicador:",
          choices = unique(indicadores %>% 
                             filter(pestana == "Mercado Laboral") %>% 
                             arrange(desc(nomindicador)) %>% 
                             pull(nomindicador))),
        uiOutput("anio_mdt"),
        dropdown(
          style = "bordered", 
          status = "primary", 
          icon = icon("warning-sign", lib = "glyphicon"),
          uiOutput("texto2"),
          animate = animateOptions(
            enter = animations$fading_entrances$fadeInLeftBig,
            exit = animations$fading_exits$fadeOutRightBig)),
        dropdown(
          style = "bordered", 
          status = "primary", 
          icon = icon("info-sign", lib = "glyphicon"),
          uiOutput("texto5"),
          animate = animateOptions(
            enter = animations$fading_entrances$fadeInLeftBig,
            exit = animations$fading_exits$fadeOutRightBig))),
      
      conditionalPanel(
        condition = "input.dimension == 'Pobreza'",
        
        selectInput(
          "indicador_p",
          "Seleccione el indicador:",
          choices = unique(indicadores %>% 
                             filter(pestana == "Pobreza" & fuente == "ECH") %>% 
                             pull(nomindicador))),
        selectInput(
          inputId = "anio_p",
          label = "Seleccione año:",
          choices = c("2015-2019")),
        dropdown(
          style = "bordered", 
          status = "primary", 
          icon = icon("warning-sign", lib = "glyphicon"),
          uiOutput("texto3"),
          animate = animateOptions(
            enter = animations$fading_entrances$fadeInLeftBig,
            exit = animations$fading_exits$fadeOutRightBig)),
        dropdown(
          style = "bordered", 
          status = "primary", 
          icon = icon("info-sign", lib = "glyphicon"),
          uiOutput("texto6"),
          animate = animateOptions(
            enter = animations$fading_entrances$fadeInLeftBig,
            exit = animations$fading_exits$fadeOutRightBig)))),
      

      uiOutput("main_dimensiones")
      
      
      
    ),

    tabPanel(
      title = "Etnoencuesta de Inmigración Reciente",
      value = 'borelito',
    
      
      sidebarPanel(selectInput("dimension_enir",
                  "Seleccione la dimensión:",
                  choices = c("Perfil sociodemográfico","Experiencia migratoria")),
                  
     conditionalPanel(
     condition = "input.dimension_enir == 'Perfil sociodemográfico'",
     
     selectInput(
       "indicador_enir_ps",
       "Seleccione el indicador:",
       choices = unique(indicadores %>%
                          filter(fuente == "ENIR") %>%
                          filter(pestana == "PERFIL SOCIODEMOGRÁFICO") %>%
                          arrange(desc(nomindicador)) %>%
                          pull(nomindicador))),
     selectInput(
       "comunidad_ps",
       "Seleccione la comunidad:",
       choices = c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA","TODAS")),
     dropdown(
       style = "bordered", 
       status = "primary", 
       icon = icon("warning-sign", lib = "glyphicon"),
       uiOutput("texto_enir_ad_1"),
       animate = animateOptions(
         enter = animations$fading_entrances$fadeInLeftBig,
         exit = animations$fading_exits$fadeOutRightBig)),
     dropdown(
       style = "bordered",
       status = "primary",
       icon = icon("info-sign", lib = "glyphicon"),
       uiOutput("texto_enir_1"),
       animate = animateOptions(
         enter = animations$fading_entrances$fadeInLeftBig,
         exit = animations$fading_exits$fadeOutRightBig))),
     
     conditionalPanel(
       condition = "input.dimension_enir == 'Experiencia migratoria'",
     
       selectInput(
         "indicador_enir_em",
         "Seleccione el indicador:",
         choices = unique(indicadores %>%
                            filter(fuente == "ENIR") %>%
                            filter(pestana == "EXPERIENCIA MIGRATORIA") %>%
                            arrange(desc(nomindicador)) %>%
                            pull(nomindicador))),
       selectInput(
         "comunidad_em",
         "Seleccione la comunidad:",
         choices = c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA","TODAS")),
       dropdown(
         style = "bordered", 
         status = "primary", 
         icon = icon("warning-sign", lib = "glyphicon"),
         uiOutput("texto_enir_ad_2"),
         animate = animateOptions(
           enter = animations$fading_entrances$fadeInLeftBig,
           exit = animations$fading_exits$fadeOutRightBig)),
       dropdown(
         style = "bordered",
         status = "primary",
         icon = icon("info-sign", lib = "glyphicon"),
         uiOutput("texto_enir_2"),
         animate = animateOptions(
           enter = animations$fading_entrances$fadeInLeftBig,
           exit = animations$fading_exits$fadeOutRightBig)))),
  
      
      uiOutput("main_dimensiones_enir")
      
      
      
      
    ),
    tabPanel(
      title = "Censo",
      value = 'opuy'
      ,
      br(),
      sidebarPanel(
        #   style = "position:fixed;width:30%;",
        selectInput(
          "indicador_c",
          "Seleccione el indicador:",
          choices = unique(indicadores %>%
                             filter(fuente == "CENSO")%>% 
                             pull(nomindicador))
        ),
        
        uiOutput("anio_censo"),
      ),
      mainPanel(tags$h3(style="display:inline-block;font-size:18px;",
                        uiOutput("title_censo")),
                plotOutput("barPlot_c", height = "600px" ),
                br(),
                downloadButton(outputId = "baja_barPlot_c", label = "Descargá el gráfico"),
                br(),
                br(),
                DTOutput("tabla_resultado_c"),
                br(),
                downloadButton("dl_tabla_resultado_c", "Descargá la tabla"),
                br(),
                br()
      )
    )
    
  )
  
  
)


##EMPIEZA SERVER

server <- function(session, input, output) {
  
  
  
  output$main_dimensiones <- renderUI({
    
    if(input$dimension == "Caracterización Sociodemográfica") {
      
      
      mainPanel(tags$h3(style="display:inline-block;font-size:18px;",
                        uiOutput("title_csd")),
                
                plotOutput("barPlot_csd",height = '600px'),
                br(),
                downloadButton(outputId = "baja_barPlot_csd", label = "Descargá el gráfico"),
                br(),
                br(),
                DTOutput("tabla_resultado_csd"),
                br(),
                downloadButton("dl_tabla_resultado_csd", "Descargá la tabla"))
      
      
    } else if(input$dimension == "Mercado Laboral") {
      
      
      mainPanel(
        tags$style(type = "text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        tags$h3(style="display:inline-block;font-size:18px;",
                uiOutput("title_mdt")),
        plotOutput("barPlot_mdt", height = "600px" ),
        br(),
        downloadButton(outputId = "baja_barPlot_mdt", label = "Descargá el gráfico"),
        br(),
        br(),
        DTOutput("tabla_resultado_mdt"),
        br(),
        downloadButton("dl_tabla_resultado_mdt", "Descargá la tabla"))
      
    } else if(input$dimension == "Pobreza") {
      
      
      mainPanel(tags$h3(style="display:inline-block;font-size:18px;",
                        uiOutput("title_p")),
                plotOutput("barPlot_p", height = "600px" ),
                br(),
                downloadButton(outputId = "baja_barPlot_p", label = "Descargá el gráfico"),
                br(),
                br(),
                DTOutput("tabla_resultado_p"),
                br(),
                downloadButton("dl_tabla_resultado_p", "Descargá la tabla"))
    }
    
    
    
  })    
  
  
  ##ENIR
  

  
  output$main_dimensiones_enir <- renderUI({ 
    
    if(input$dimension_enir == "Perfil sociodemográfico") {
      
      
      mainPanel(
        tags$style(type = "text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        tags$h3(style="display:inline-block;font-size:18px;",
                uiOutput("title_enir_ps")),
        plotOutput("barPlot_enir_ps", height = "600px" ),
        br(),
        downloadButton(outputId = "baja_barPlot_enir_ps", label = "Descargá el gráfico"),
        br(),
        br(),
        DTOutput("tabla_resultado_enir_ps"),
        br(),
        downloadButton("dl_tabla_resultado_enir_ps", "Descargá la tabla")
      )
      
    } else if(input$dimension_enir == "Experiencia migratoria") {
      
      mainPanel(
        tags$style(type = "text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        tags$h3(style="display:inline-block;font-size:18px;",
                uiOutput("title_enir_em")),
        plotOutput("barPlot_enir_em", height = "600px" ),
        br(),
        downloadButton(outputId = "baja_barPlot_enir_em", label = "Descargá el gráfico"),
        br(),
        br(),
        DTOutput("tabla_resultado_enir_em"),
        br(),
        downloadButton("dl_tabla_resultado_enir_em", "Descargá la tabla")
      )
      
    } 
    
  }) 
  

  help <- helpText(HTML("<B><FONT size=1>La información de ECH corresponde a valores anuales salvo para las estimaciones de 2021 donde se reportan valores del segundo semestre. Las variables que permiten construir la condición migratoria se relevaron únicamente en el segundo semestre, y en 2020 la ECH no las incluyó. La información sobre condición migratoria  errores y barras de error que acompañan las representaciones gráficas y de tablas corresponden a un nivel de confianza estadística de 95%. Cuando las barras de error de una y otra categoría se superponen en valores, la comparación entre esas categorías no es significativa y no es posible sacar conclusiones en ese caso. Dado que la muestra de 2021 representa sólo una porción del año, el número de casos es sensiblemente menor en ese año y, consecuentemente, los intervalos de confianza son más amplios.</FONT></B>"))
  
  help2 <- helpText(HTML("<B><FONT size=1>La información sobre condición migratoria  errores y barras de error que acompañan las representaciones gráficas y de tablas corresponden a un nivel de confianza estadística de 95%. Cuando las barras de error de una y otra categoría se superponen en valores, la comparación entre esas categorías no es significativa y no es posible sacar conclusiones en ese caso.</FONT></B>"))
  
  
  output$texto1 <- renderUI({ 
    help
  })
  
  output$texto2 <- renderUI({ 
    help2
  })
  
  output$texto3 <- renderUI({ 
    help2
  })
  
  
  def <-  HTML("<B><FONT size=1><ul><li>Nativo/no migrante: persona nacida en Uruguay que siempre ha vivido en el país</li><li>Inmigrante reciente: persona nacida en el exterior que lleva menos de cinco años de residencia en Uruguay</li><li>Inmigrante antiguo: persona nacida en el exterior que lleva cinco o más años de residencia en Uruguay.</li><li>Retornado reciente: persona nacida en Uruguay que estuvo viviendo en el exterior y regresó al país hace menos de cinco años.</li><li>Retornado antiguo: persona nacida en Uruguay que estuvo viviendo en el exterior y regresó al país hace cinco o más años.</li></ul></FONT></B>")
  
  output$texto4 <- renderUI({ 
    def
  })
  
  output$texto5 <- renderUI({ 
    def
  })
  
  output$texto6 <- renderUI({ 
    def
  })
  
  
  texto_enir <- helpText(HTML("<B><FONT size=1><ul><li>La ENIR estuvo dirigida a informantes de origen cubano, dominicano, peruano y venezolano, mayores de edad, que residían en Montevideo al momento de la encuesta, y para su selección se aplicó un tipo de Muestreo Guiado por el Informante (RDS por su acrónimo en inglés, Respondent Driven Sampling). Esta técnica se sustenta en la implementación de la lógica de “bola de nieve”, la cual se completa con la inclusión de controles de sesgos de selección con el propósito de que las personas con mayor popularidad no queden sobrerrepresentadas en la muestra. Para ello, los microdatos de la ENIR fueron ponderados utilizando un ponderador RDS2, que se calcula como el inverso del tamaño de la red del informante (número de personas a las que podría contactar en 24 horas) <a href='http://www.sverigeisiffror.scb.se/contentassets/ff271eeeca694f47ae99b942de61df83/probability-based-estimation-theory-for-respondent-driven-sampling.pdf'>(Volz y Heckathorn, 2008)</a>.</li><li>Se recomienda leer el <a href='https://lamp.opr.princeton.edu/uruguay/uy_pdf/ENIR%20Methodology.pdf'>documento metodológico de la ENIR</a>, donde se presenta información más detallada.</li></ul></FONT></B>"))
  
  
  output$texto_enir_1 <- renderUI({
    texto_enir
  })
  
  output$texto_enir_2 <- renderUI({
    texto_enir
  })
  
  
  texto_enir_ad <- helpText(HTML("<B><FONT size=1><ul><li>El diseño muestral aplicado en la ENIR no permite realizar inferencias sobre el total de la población inmigrante en Uruguay. Refiere únicamente a las redes sociales de los informantes de cada comunidad.</li><li>El reducido tamaño muestral imposibilita realizar afirmaciones estadísticamente significativas para determinados cruces de variables, motivo por el cual se marcarán los valores que estén asociados a un número de casos menor a 20.</li><li>Los resultados deben analizarse de manera fragmentada por comunidad.</li></ul></FONT></B>"))
  
  
  output$texto_enir_ad_1 <- renderUI({
    texto_enir_ad
  })
  
  output$texto_enir_ad_2 <- renderUI({
    texto_enir_ad
  })
  
  
  
  output$anio_csd <- renderUI({
    
    if(input$indicador_csd == "DISTRIBUCIÓN RELATIVA POR NIVEL EDUCATIVO ALCANZADO SEGÚN CONDICIÓN MIGRATORIA PARA LAS PERSONAS DE 25 AÑOS Y MÁS"
       |input$indicador_csd == "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN DE 0 A 17 AÑOS POR ARREGLOS RESIDENCIALES SEGÚN GENERACIÓN MIGRATORIA") {
      
      selectInput(
        inputId = "anio_csd1",
        label = "Seleccione año:",
        choices = c("2015-2019"))
      
    } else if(input$indicador_csd == "PERSONAS SEGÚN CONDICIÓN MIGRATORIA"
              |input$indicador_csd == "PERSONAS SEGÚN CONDICIÓN MIGRATORIA Y LUGAR DE RESIDENCIA (MONTEVIDEO E INTERIOR)"){
      
      selectInput(
        inputId = "anio_csd2",
        label = "Seleccione año:",
        choices = c("2013", "2014", "2015", "2016", "2017", "2018", "2019","2021", "Evolución"),
        selected = "2021")
      
    }else{
      
      selectInput(
        inputId = "anio_csd2",
        label = "Seleccione año:",
        choices = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "Evolución"),
        selected = "2019")
    }
    
  })
  
  

  
  
  csd <- reactive({
    
    indicadores_csd <- indicadores %>% 
      filter(nomindicador == input$indicador_csd) %>% 
      filter(anio == input$anio_csd2) %>%
      mutate(statusmigratorio = factor(statusmigratorio, levels = c("Nativo/No migrante", 
                                                                    "Inmigrante reciente", 
                                                                    "Inmigrante antiguo",
                                                                    "Retornado reciente", 
                                                                    "Retornado antiguo"))) %>% 
      arrange(statusmigratorio) 
    
  }) 
  
  csd2 <- reactive({
    
    indicadores_csd <- indicadores %>% 
      filter(nomindicador == input$indicador_csd) %>% 
      filter(anio == input$anio_csd1) %>%
      mutate(statusmigratorio = factor(statusmigratorio, levels = c("Nativo/No migrante", 
                                                                    "Inmigrante reciente", 
                                                                    "Inmigrante antiguo",
                                                                    "Retornado reciente", 
                                                                    "Retornado antiguo"))) %>% 
      arrange(statusmigratorio) 
    
  }) 
  
  csd_evolucion <- reactive({
    
    indicadores_csd <- indicadores %>% 
      filter(nomindicador == input$indicador_csd) %>%
      mutate(statusmigratorio = factor(statusmigratorio, levels = c("Nativo/No migrante", 
                                                                    "Inmigrante reciente", 
                                                                    "Inmigrante antiguo",
                                                                    "Retornado reciente", 
                                                                    "Retornado antiguo"))) %>% 
      arrange(statusmigratorio) 
    
  }) 
  
  
  
  
  output$title_csd <- renderUI({
    
    if(input$indicador_csd=="DISTRIBUCIÓN RELATIVA POR NIVEL EDUCATIVO ALCANZADO SEGÚN CONDICIÓN MIGRATORIA PARA LAS PERSONAS DE 25 AÑOS Y MÁS"
       | input$indicador_csd=="DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN DE 0 A 17 AÑOS POR ARREGLOS RESIDENCIALES SEGÚN GENERACIÓN MIGRATORIA"){
      helpText(HTML(paste(input$indicador_csd,"- (",input$anio_csd1,")")))}
    
    else {
      helpText(HTML(paste(input$indicador_csd,"-",input$anio_csd2)))}
  })
  
  
  output$barPlot_csd <- renderPlot({
    
    if(input$indicador_csd == "PERSONAS SEGÚN CONDICIÓN MIGRATORIA" & (input$anio_csd2 %in% c(2013:2021))) {
      
      
      csd <- csd() %>%
        mutate(valor = as.numeric(valor),
               int_inf = as.numeric(int_inf),
               int_sup = as.numeric(int_sup)) %>%
        filter(statusmigratorio != "Nativo/No migrante") %>% 
        ggplot(aes(statusmigratorio, valor)) +
        #       geom_linerange(aes(ymin = int_inf, ymax = int_sup), color = blue_bdd3, size = 2, alpha = .4) +
        #      geom_point(size = 5, color = blue_bdd) +
        geom_bar(stat = "identity", position = "dodge",fill="#7bb7dd", width=0.5)+
        geom_errorbar(aes(ymin = int_inf, ymax = int_sup), width = 0.2,
                      position = position_dodge(0.9))+
        scale_y_continuous(limits = c(0, NA),
                           labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
        labs(x = "",
             y = "")+
        
        theme(legend.position = "none",
              axis.text.x = element_text(size=13,angle = 90),
              plot.title = element_text(vjust=1.5,size = 13))
      
    } else if (input$indicador_csd == "PERSONAS SEGÚN CONDICIÓN MIGRATORIA" & input$anio_csd2 == "Evolución") {
      
      csd <- csd_evolucion() %>% 
        filter(statusmigratorio != "Nativo/No migrante") %>% 
        ggplot(aes(anio, valor, color = statusmigratorio, fill = statusmigratorio, group = statusmigratorio)) +
        geom_line(size = 1.1) +
        geom_ribbon(aes(ymin = int_inf, ymax = int_sup), alpha = .3, colour = NA) +
        scale_y_continuous(limits = c(0, NA),
                           labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
        labs(x = "",
             y = "")+
        
        theme(axis.text.x = element_text(size=13,angle = 90),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))
      
    } else if (input$indicador_csd == "PERSONAS SEGÚN CONDICIÓN MIGRATORIA Y GRUPO DE EDAD" & (input$anio_csd2 %in% c(2013:2019))) {
      csd <- csd() %>%
        mutate(valor = as.numeric(valor),
               int_inf = as.numeric(int_inf),
               int_sup = as.numeric(int_sup)) %>%
        filter(statusmigratorio != "Nativo/No migrante") %>% 
        ggplot(aes(statusmigratorio, valor)) +
        
        geom_bar(stat = "identity", position = "dodge",fill="#7bb7dd", width=0.5)+
        geom_errorbar(aes(ymin = int_inf, ymax = int_sup), width = 0.2,
                      position = position_dodge(0.9))+
        scale_y_continuous(limits = c(0, NA),
                           labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
        facet_wrap(~ grupoedad) +
        theme(axis.text.x = element_text(size=13,angle = 90),
              plot.title = element_text(vjust=1.5,size = 13))+
        labs(x = "",
             y = "")
      
    } else if (input$indicador_csd == "PERSONAS SEGÚN CONDICIÓN MIGRATORIA Y GRUPO DE EDAD" & input$anio_csd2 == "Evolución") {
      
      csd <- csd_evolucion() %>%
        filter(statusmigratorio != "Nativo/No migrante") %>% 
        ggplot(aes(anio, valor, color = statusmigratorio, fill = statusmigratorio, group = statusmigratorio)) +
        geom_line(size = 1.1) +
        geom_ribbon(aes(ymin = int_inf, ymax = int_sup), alpha = .3, colour = NA) +
        scale_y_continuous(limits = c(0, NA),
                           labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
        facet_wrap(~ grupoedad) +
        theme(axis.text.x = element_text(size=13,angle = 90),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))+
        labs(x = "",
             y = "")
      
      
    } else if (input$indicador_csd == "PORCENTAJE DE POBLACIÓN AFRO SEGÚN CONDICIÓN MIGRATORIA" & (input$anio_csd2 %in% c(2013:2019))) {
      csd <- csd() %>%
        mutate(valor = as.numeric(valor),
               int_inf = as.numeric(int_inf),
               int_sup = as.numeric(int_sup)) %>%
        filter(sexo != "No afro") %>% 
        ggplot(aes(statusmigratorio, valor)) +
        geom_bar(stat = "identity", position = "dodge",fill="#7bb7dd", width=0.5)+
        geom_errorbar(aes(ymin = int_inf, ymax = int_sup), width = 0.2,
                      position = position_dodge(0.9))+
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::percent_format(accuracy = 1)) +
        labs(x = "",
             y = "")+
        theme(axis.text.x = element_text(size=13,angle = 90),
              plot.title = element_text(vjust=1.5,size = 13))
    } else if (input$indicador_csd == "PORCENTAJE DE POBLACIÓN AFRO SEGÚN CONDICIÓN MIGRATORIA" & (input$anio_csd2 == "Evolución")) {
      
      csd <- csd_evolucion() %>%
        filter(sexo != "No afro") %>% 
        ggplot(aes(anio, valor, color = statusmigratorio, fill = statusmigratorio, group = statusmigratorio)) +
        geom_line(size = 1.1) +
        geom_ribbon(aes(ymin = int_inf, ymax = int_sup), alpha = .3, colour = NA) +
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::percent_format(accuracy = 1)) +
        theme(axis.text.x = element_text(size=13,angle = 90),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))
      labs(x = "",
           y = "")
    } else if (input$indicador_csd == "PORCENTAJE DE MUJERES SEGÚN CONDICIÓN MIGRATORIA" & (input$anio_csd2 %in% c(2013:2019))){
      
      csd <- csd() %>% 
        mutate(valor = as.numeric(valor),
               int_inf = as.numeric(int_inf),
               int_sup = as.numeric(int_sup)) %>% 
        ggplot(aes(statusmigratorio, valor)) +
        geom_bar(stat = "identity", position = "dodge",fill="#7bb7dd", width=0.5)+
        geom_errorbar(aes(ymin = int_inf, ymax = int_sup), width = 0.2,
                      position = position_dodge(0.9))+
        scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                           limits = c(0, .6)) +
        labs(x = "",
             y = "")+
        theme(axis.text.x = element_text(size=13,angle = 90),
              plot.title = element_text(vjust=1.5,size = 13))
      
    } else if (input$indicador_csd == "PORCENTAJE DE MUJERES SEGÚN CONDICIÓN MIGRATORIA" & (input$anio_csd2 == "Evolución")){
      
      csd <- csd_evolucion() %>% 
        mutate(valor = as.numeric(valor),
               int_inf = as.numeric(int_inf),
               int_sup = as.numeric(int_sup)) %>% 
        ggplot(aes(anio, valor, color = statusmigratorio, fill = statusmigratorio, group = statusmigratorio)) +
        geom_line(size = 1.1) +
        geom_ribbon(aes(ymin = int_inf, ymax = int_sup), alpha = .3, colour = NA) +
        
        scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                           limits = c(0, .6)) +
        theme(axis.text.x = element_text(size=13,angle = 90),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))+
        labs(x = "",
             y = "")
      
    } else if (input$indicador_csd == "DISTRIBUCIÓN RELATIVA DE LA INMIGRACIÓN RECIENTE POR LUGAR DE NACIMIENTO" & 
               (input$anio_csd2 %in% c(2013:2019))) {
      
      csd <- csd() %>% 
        mutate(valor = as.numeric(valor),
               int_inf = as.numeric(int_inf),
               int_sup = as.numeric(int_sup)) %>% 
        ggplot(aes(reorder(lugarnac, valor), valor)) +
        geom_bar(stat = "identity", position = "dodge",fill="#7bb7dd", width=0.5)+
        geom_errorbar(aes(ymin = int_inf, ymax = int_sup), width = 0.2,
                      position = position_dodge(0.9))+
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::percent_format(accuracy = 1)) +
        labs(x = "",
             y = "")+
        theme(axis.text.x = element_text(size=13,angle = 90),
              plot.title = element_text(vjust=1.5,size = 13))
      
    } else if (input$indicador_csd == "DISTRIBUCIÓN RELATIVA DE LA INMIGRACIÓN RECIENTE POR LUGAR DE NACIMIENTO" & 
               (input$anio_csd2 == "Evolución")) {
      
      csd <- csd_evolucion() %>% 
        mutate(valor = as.numeric(valor),
               int_inf = as.numeric(int_inf),
               int_sup = as.numeric(int_sup)) %>% 
        ggplot(aes(anio, valor, color = lugarnac, fill = lugarnac, group = lugarnac)) +
        geom_line(size = 1.1) +
        geom_ribbon(aes(ymin = int_inf, ymax = int_sup), alpha = .3, colour = NA) +
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::percent_format(accuracy = 1)) +
        theme(axis.text.x = element_text(size=13,angle = 90),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))+
        labs(x = "",
             y = "")
      
      
    } else if (input$indicador_csd == "PERSONAS SEGÚN CONDICIÓN MIGRATORIA Y LUGAR DE RESIDENCIA (MONTEVIDEO E INTERIOR)" & 
               (input$anio_csd2 %in% c(2013:2021))) {
      
      csd <- csd() %>% 
        filter(statusmigratorio != "Nativo/No migrante") %>% 
        ggplot(aes(statusmigratorio, valor)) +
        geom_bar(stat = "identity", position = "dodge",fill="#7bb7dd", width=0.5)+
        geom_errorbar(aes(ymin = int_inf, ymax = int_sup), width = 0.2,
                      position = position_dodge(0.9))+
        scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
        facet_wrap(~ depto) + 
        theme(axis.text.x = element_text(size=13,angle = 90),
              plot.title = element_text(vjust=1.5,size = 13))+
        labs(x = "",
             y = "")
      
      
    } else if (input$indicador_csd == "PERSONAS SEGÚN CONDICIÓN MIGRATORIA Y LUGAR DE RESIDENCIA (MONTEVIDEO E INTERIOR)" & 
               input$anio_csd2 == "Evolución") {
      
      csd <- csd_evolucion() %>% 
        filter(statusmigratorio != "Nativo/No migrante") %>% 
        ggplot(aes(anio, valor, color = statusmigratorio, fill = statusmigratorio, group = statusmigratorio)) +
        geom_line(size = 1.1) +
        geom_ribbon(aes(ymin = int_inf, ymax = int_sup), alpha = .3, colour = NA) +
        scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
        theme(axis.text.x = element_text(size=13,angle = 90),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))+
        facet_wrap(~ depto, ncol=2) + 
        labs(x = "",
             y = "")
      
    }  else if (input$indicador_csd == "DISTRIBUCIÓN RELATIVA DEL RETORNO RECIENTE POR LUGAR DE RESIDENCIA CINCO AÑOS ANTES" & 
                (input$anio_csd2 %in% c(2013:2019))) {
      
      csd <- csd() %>% 
        mutate(valor = as.numeric(valor),
               int_inf = as.numeric(int_inf),
               int_sup = as.numeric(int_sup),
               lugarrec = reorder(lugarrec, valor),
               lugarrec = fct_relevel(lugarrec, "Sin dato", after = Inf)) %>% 
        ggplot(aes(lugarrec, valor)) +
        
        geom_bar(stat = "identity", position = "dodge",fill="#7bb7dd", width=0.5)+
        geom_errorbar(aes(ymin = int_inf, ymax = int_sup), width = 0.2,
                      position = position_dodge(0.9))+
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::percent_format(accuracy = 1)) +
        labs(x = "",
             y = "")+
        theme(axis.text.x = element_text(size=13,angle = 90),
              plot.title = element_text(vjust=1.5,size = 13))
    } else if (input$indicador_csd == "DISTRIBUCIÓN RELATIVA DEL RETORNO RECIENTE POR LUGAR DE RESIDENCIA CINCO AÑOS ANTES" & 
               input$anio_csd2 == "Evolución") {
      
      csd <- csd_evolucion() %>% 
        mutate(valor = as.numeric(valor),
               int_inf = as.numeric(int_inf),
               int_sup = as.numeric(int_sup),
               lugarrec = reorder(lugarrec, valor),
               lugarrec = fct_relevel(lugarrec, "Sin dato", after = Inf)) %>% 
        ggplot(aes(anio, valor, fill = lugarrec, color = lugarrec, group = lugarrec)) +
        geom_line(size = 1.1) +
        geom_ribbon(aes(ymin = int_inf, ymax = int_sup), alpha = .3, colour = NA) +
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::percent_format(accuracy = 1)) +
        #guides(color=guide_legend(nrow=2)) +
        theme(axis.text.x = element_text(size=13,angle = 90),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))+
        labs(x = "",
             y = "")
    } else if (input$indicador_csd == "PERSONAS DE 0 A 17 AÑOS VINCULADAS A LA INMIGRACIÓN EXTRANJERA SEGÚN GENERACIÓN MIGRATORIA" & 
               (input$anio_csd2 %in% c(2013:2019))) {
      
      csd <- csd() %>% 
        mutate(valor = as.numeric(valor),
               int_inf = as.numeric(int_inf),
               int_sup = as.numeric(int_sup),
               lugarrec = reorder(generacion, valor),
               lugarrec = fct_relevel(generacion, "Sin dato", after = Inf)) %>% 
        ggplot(aes(generacion, valor)) +
        geom_bar(stat = "identity", position = "dodge",fill="#7bb7dd", width=0.5)+
        geom_errorbar(aes(ymin = int_inf, ymax = int_sup), width = 0.2,
                      position = position_dodge(0.9))+
        scale_y_continuous(limits = c(0, NA),
                           labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
        theme(axis.text.x = element_text(size=13,angle = 90),
              plot.title = element_text(vjust=1.5,size = 13))+
        labs(x = "",
             y = "")
      
    } else if (input$indicador_csd == "PERSONAS DE 0 A 17 AÑOS VINCULADAS A LA INMIGRACIÓN EXTRANJERA SEGÚN GENERACIÓN MIGRATORIA" &
               input$anio_csd2 == "Evolución") {
      
      
      csd <- csd_evolucion() %>% 
        mutate(valor = as.numeric(valor),
               int_inf = as.numeric(int_inf),
               int_sup = as.numeric(int_sup),
               lugarrec = reorder(generacion, valor),
               lugarrec = fct_relevel(generacion, "Sin dato", after = Inf)) %>% 
        ggplot(aes(anio, valor, fill = generacion, color = generacion, group = generacion)) +
        geom_line(size = 1.1) +
        geom_ribbon(aes(ymin = int_inf, ymax = int_sup), alpha = .3, colour = NA) +
        scale_y_continuous(limits = c(0, NA),
                           labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
        theme(axis.text.x = element_text(size=13,angle = 90),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))+
        labs(x = "",
             y = "")
    } else if (input$indicador_csd == "DISTRIBUCIÓN RELATIVA POR NIVEL EDUCATIVO ALCANZADO SEGÚN CONDICIÓN MIGRATORIA PARA LAS PERSONAS DE 25 AÑOS Y MÁS" & (input$anio_csd1 == "2015-2019")) {
      
      csd <- csd2() %>% 
        mutate(niveledu = fct_relevel(niveledu, "Sin dato", after = Inf)) %>% 
        ggplot(aes(niveledu, valor)) +
        geom_bar(stat = "identity", position = "dodge",fill="#7bb7dd", width=0.5)+
        geom_errorbar(aes(ymin = int_inf, ymax = int_sup), width = 0.2,
                      position = position_dodge(0.9))+
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        facet_wrap(~ statusmigratorio, ncol = 2, scales = "free_y") + 
        theme(axis.text.x = element_text(size=9,angle = 90,hjust=0.95,vjust=0.2),
              axis.text.y = element_text(size=10),
              strip.text.x = element_text(size = 9),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))+
        
        labs(x = "",
             y = "")
      
    } else if (input$indicador_csd == "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN DE 0 A 17 AÑOS POR ARREGLOS RESIDENCIALES SEGÚN GENERACIÓN MIGRATORIA" & (input$anio_csd1 == "2015-2019")) {
      
      csd <- csd2() %>% 
        mutate(arreglos = fct_relevel(arreglos, "Sin dato", after = Inf)) %>% 
        ggplot(aes(arreglos, valor)) +
        geom_bar(stat = "identity", position = "dodge",fill="#7bb7dd", width=0.5)+
        geom_errorbar(aes(ymin = int_inf, ymax = int_sup), width = 0.2,
                      position = position_dodge(0.9))+
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        facet_wrap(~ generacion, ncol = 2, scales = "free_y") + 
        theme(axis.text.x = element_text(size=9,angle = 90,hjust=0.95,vjust=0.2),
              axis.text.y = element_text(size=10),
              strip.text.x = element_text(size = 9),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))+
        labs(x = "",
             y = "")
    }
    
    
    print(csd)
    ggsave("www/csd.png")
    
  })
  
  
  
  
  
  output$baja_barPlot_csd <- downloadHandler(
    filename <- function() {
      paste("csd", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/csd.png", file)
    },
    contentType = "www/csd"
    
    
  )
  
  
  
  output$tabla_resultado_csd  <- renderDT({
    
    if(input$indicador_csd == "PERSONAS SEGÚN CONDICIÓN MIGRATORIA"){
      
      if (input$anio_csd2 %in% c(2013:2021)) {
        datatable(csd() %>% 
                    arrange(statusmigratorio) %>% 
                    transmute("Año" = anio, 
                              "Condición migratoria" = statusmigratorio,
                              "Valor" = format(valor, big.mark = "."),
                              "Valor mínimo - IC 95 %" = format(int_inf, big.mark = "."),
                              "Valor máximo - IC 95 %" = format(int_sup, big.mark = ".")), rownames = FALSE, options = list(dom = 't')) 
      } else if (input$anio_csd2 == "Evolución") {
        
        datatable(csd_evolucion() %>% 
                    arrange(statusmigratorio) %>% 
                    transmute("Año" = anio, 
                              "Condición migratoria" = statusmigratorio,
                              "Valor" = format(valor, big.mark = "."),
                              "Valor mínimo - IC 95 %" = format(int_inf, big.mark = "."),
                              "Valor máximo - IC 95 %" = format(int_sup, big.mark = ".")), rownames = FALSE, options = list(dom = 'ltipr'))}
      
      
      
      
    } else if (input$indicador_csd == "PERSONAS SEGÚN CONDICIÓN MIGRATORIA Y GRUPO DE EDAD") {
      
      if (input$anio_csd2 %in% c(2013:2019)) {
        datatable(csd() %>% 
                    arrange(statusmigratorio) %>%
                    transmute("Año" = anio, 
                              "Condición migratoria" = statusmigratorio,
                              "Grupo de edad" = grupoedad,
                              "Valor" = format(valor, big.mark = "."),
                              "Valor mínimo - IC 95 %" = format(int_inf, big.mark = "."),
                              "Valor máximo - IC 95 %" = format(int_sup, big.mark = ".")), rownames = FALSE, options = list(dom = 't')) 
      } else if (input$anio_csd2 == "Evolución") {
        
        datatable(csd_evolucion() %>% 
                    arrange(statusmigratorio) %>%
                    transmute("Año" = anio, 
                              "Condición migratoria" = statusmigratorio,
                              "Grupo de edad" = grupoedad,
                              "Valor" = format(valor, big.mark = "."),
                              "Valor mínimo - IC 95 %" = format(int_inf, big.mark = "."),
                              "Valor máximo - IC 95 %" = format(int_sup, big.mark = ".")), rownames = FALSE, options = list(dom = 'ltipr'))}
      
    } else if (input$indicador_csd == "PERSONAS DE 0 A 17 AÑOS VINCULADAS A LA INMIGRACIÓN EXTRANJERA SEGÚN GENERACIÓN MIGRATORIA") {
      
      if (input$anio_csd2 %in% c(2013:2019)) {
        datatable(csd() %>% 
                    arrange(generacion) %>%
                    transmute("Año" = anio, 
                              "Generación migratoria" = generacion,
                              #"Grupo de edad" = grupoedad,
                              "Valor" = format(valor, big.mark = "."),
                              "Valor mínimo - IC 95 %" = format(int_inf, big.mark = "."),
                              "Valor máximo - IC 95 %" = format(int_sup, big.mark = ".")), rownames = FALSE, options = list(dom = 't')) 
      } else if (input$anio_csd2 == "Evolución") {
        
        datatable(csd_evolucion() %>% 
                    arrange(generacion) %>%
                    transmute("Año" = anio, 
                              "Generación migratoria" = generacion,
                              #"Grupo de edad" = grupoedad,
                              "Valor" = format(valor, big.mark = "."),
                              "Valor mínimo - IC 95 %" = format(int_inf, big.mark = "."),
                              "Valor máximo - IC 95 %" = format(int_sup, big.mark = ".")), rownames = FALSE, options = list(dom = 'ltipr'))}
      
    } else if (input$indicador_csd == "PERSONAS SEGÚN CONDICIÓN MIGRATORIA Y LUGAR DE RESIDENCIA (MONTEVIDEO E INTERIOR)") {
      
      if (input$anio_csd2 %in% c(2013:2021)) {
        datatable(csd() %>% 
                    arrange(statusmigratorio) %>%
                    transmute("Año" = anio, 
                              "Condición migratoria" = statusmigratorio,
                              "Departamento" = depto,
                              "Valor" = format(round(valor), big.mark = "."),
                              "Valor mínimo - IC 95 %" = format(round(int_inf), big.mark = "."),
                              "Valor máximo - IC 95 %" = format(round(int_sup), big.mark = ".")), rownames = FALSE, options = list(dom = 't')) 
      } else if (input$anio_csd2 == "Evolución") {
        
        datatable(csd_evolucion() %>% 
                    arrange(statusmigratorio) %>%
                    transmute("Año" = anio, 
                              "Condición migratoria" = statusmigratorio,
                              "Departamento" = depto,
                              "Valor" = format(round(valor), big.mark = "."),
                              "Valor mínimo - IC 95 %" = format(round(int_inf), big.mark = "."),
                              "Valor máximo - IC 95 %" = format(round(int_sup), big.mark = ".")), rownames = FALSE, options = list(dom = 'ltipr'))}
      
    } else if (input$indicador_csd == "PORCENTAJE DE POBLACIÓN AFRO SEGÚN CONDICIÓN MIGRATORIA") {
      
      if (input$anio_csd2 %in% c(2013:2019)) {
        datatable(csd() %>% 
                    arrange(statusmigratorio) %>%
                    filter(sexo != "No afro") %>% 
                    transmute("Año" = anio, 
                              "Condición migratoria" = statusmigratorio,
                              "Ascendencia étnico-racial" = sexo,
                              "Valor (%)" = round(valor*100, 2),
                              "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                              "Valor máximo - IC 95 %" = round(int_sup*100, 2)), rownames = FALSE, options = list(dom = 't')) 
      } else if (input$anio_csd2 == "Evolución") {
        
        datatable(csd_evolucion() %>% 
                    arrange(statusmigratorio) %>%
                    filter(sexo != "No afro") %>% 
                    transmute("Año" = anio, 
                              "Condición migratoria" = statusmigratorio,
                              "Ascendencia étnico-racial" = sexo,
                              "Valor (%)" = round(valor*100, 2),
                              "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                              "Valor máximo - IC 95 %" = round(int_sup*100, 2)), rownames = FALSE, options = list(dom = 'ltipr'))}
      
    } else if(input$indicador_csd == "PORCENTAJE DE MUJERES SEGÚN CONDICIÓN MIGRATORIA"){
      
      if (input$anio_csd2 %in% c(2013:2019)) {
        datatable(csd() %>% 
                    arrange(statusmigratorio) %>%
                    transmute("Año" = anio, 
                              "Condición migratoria" = statusmigratorio,
                              "Valor (%)" = round(valor*100, 2),
                              "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                              "Valor máximo - IC 95 %" = round(int_sup*100, 2)), rownames = FALSE, options = list(dom = 't')) 
      } else if (input$anio_csd2 == "Evolución") {
        
        datatable(csd_evolucion() %>% 
                    arrange(statusmigratorio) %>%
                    transmute("Año" = anio, 
                              "Condición migratoria" = statusmigratorio,
                              "Valor (%)" = round(valor*100, 2),
                              "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                              "Valor máximo - IC 95 %" = round(int_sup*100, 2)), rownames = FALSE, options = list(dom = 'ltipr'))}
      
      
    } else if(input$indicador_csd == "DISTRIBUCIÓN RELATIVA DE LA INMIGRACIÓN RECIENTE POR LUGAR DE NACIMIENTO"){
      
      if (input$anio_csd2 %in% c(2013:2019)) {
        datatable(csd() %>% 
                    arrange(statusmigratorio) %>%
                    transmute("Año" = anio, 
                              "Lugar de nacimiento" = lugarnac,
                              "Valor (%)" = round(valor*100, 2),
                              "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                              "Valor máximo - IC 95 %" = round(int_sup*100, 2)), rownames = FALSE, options = list(dom = 't')) 
      } else if (input$anio_csd2 == "Evolución") {
        
        datatable(csd_evolucion() %>% 
                    arrange(statusmigratorio) %>%
                    transmute("Año" = anio, 
                              "Lugar de nacimiento" = lugarnac,
                              "Valor (%)" = round(valor*100, 2),
                              "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                              "Valor máximo - IC 95 %" = round(int_sup*100, 2)), rownames = FALSE, options = list(dom = 'ltipr'))}
      
    } else if(input$indicador_csd == "DISTRIBUCIÓN RELATIVA DEL RETORNO RECIENTE POR LUGAR DE RESIDENCIA CINCO AÑOS ANTES"){
      
      
      if (input$anio_csd2 %in% c(2013:2019)) {
        datatable(csd() %>% 
                    arrange(statusmigratorio) %>%
                    transmute("Año" = anio, 
                              "Lugar de residencia" = lugarrec,
                              "Valor (%)" = round(valor*100, 2),
                              "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                              "Valor máximo - IC 95 %" = round(int_sup*100, 2)), rownames = FALSE, options = list(dom = 't')) 
      } else if (input$anio_csd2 == "Evolución") {
        
        datatable(csd_evolucion() %>% 
                    arrange(statusmigratorio) %>%
                    transmute("Año" = anio, 
                              "Lugar de residencia" = lugarrec,
                              "Valor (%)" = round(valor*100, 2),
                              "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                              "Valor máximo - IC 95 %" = round(int_sup*100, 2)), rownames = FALSE, options = list(dom = 'ltipr'))}
      
      
      
    } else if (input$indicador_csd == "DISTRIBUCIÓN RELATIVA POR NIVEL EDUCATIVO ALCANZADO SEGÚN CONDICIÓN MIGRATORIA PARA LAS PERSONAS DE 25 AÑOS Y MÁS") {
      
      datatable(csd2() %>% 
                  arrange(statusmigratorio) %>%
                  transmute("Año" = anio, 
                            "Condición migratoria" = statusmigratorio,
                            "Nivel educativo" = niveledu,
                            "Valor (%)" = round(valor*100, 2),
                            "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                            "Valor máximo - IC 95 %" = round(int_sup*100, 2)), rownames = FALSE, options = list(dom = 't'))
    } else if (input$indicador_csd == "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN DE 0 A 17 AÑOS POR ARREGLOS RESIDENCIALES SEGÚN GENERACIÓN MIGRATORIA") {
      
      datatable(csd2() %>% 
                  arrange(generacion) %>%
                  transmute("Año" = anio, 
                            "Generación migratoria" = generacion,
                            "Arreglos residenciales" = arreglos,
                            "Valor (%)" = round(valor*100, 2),
                            "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                            "Valor máximo - IC 95 %" = round(int_sup*100, 2)), rownames = FALSE, options = list(dom = 't'))
    }
    
  }) 
  
  
  
  
  
  
  output$dl_tabla_resultado_csd <- downloadHandler(
    
    
    filename = function() {
      paste0("resultados- ",input$indicador_csd, ".csv", sep = "")
    },
    content = function(file) {
      
      if(input$indicador_csd == "PERSONAS SEGÚN CONDICIÓN MIGRATORIA"){
        
        if (input$anio_csd2 %in% c(2013:2021)) {
          
          write.csv(csd() %>% 
                      arrange(statusmigratorio) %>% 
                      transmute("Año" = anio, 
                                "Condición migratoria" = statusmigratorio,
                                "Valor" = format(valor, big.mark = "."),
                                "Valor mínimo - IC 95 %" = format(int_inf, big.mark = "."),
                                "Valor máximo - IC 95 %" = format(int_sup, big.mark = ".")), file, row.names = FALSE, fileEncoding = "latin1")
          
          
          
        } else if (input$anio_csd2 == "Evolución") {
          
          write.csv(csd_evolucion() %>% 
                      arrange(statusmigratorio) %>%
                      transmute("Año" = anio, 
                                "Condición migratoria" = statusmigratorio,
                                "Valor" = valor,
                                "Valor mínimo - IC 95 %" = int_inf,
                                "Valor máximo - IC 95 %" = int_sup), file, row.names = FALSE, fileEncoding = "latin1")
          
        }
        
        
        
      } else if (input$indicador_csd == "PERSONAS SEGÚN CONDICIÓN MIGRATORIA Y GRUPO DE EDAD") {
        
        if (input$anio_csd2 %in% c(2013:2019)) {
          
          
          write.csv(csd() %>% 
                      arrange(statusmigratorio) %>%
                      transmute("Año" = anio, 
                                "Condición migratoria" = statusmigratorio,
                                "Grupo de edad" = grupoedad,
                                "Valor" = valor,
                                "Valor mínimo - IC 95 %" = int_inf,
                                "Valor máximo - IC 95 %" = int_sup), file, row.names = FALSE, fileEncoding = "latin1") 
          
          
        } else if (input$anio_csd2 == "Evolución") {
          
          write.csv(csd_evolucion() %>% 
                      arrange(statusmigratorio) %>%
                      transmute("Año" = anio, 
                                "Condición migratoria" = statusmigratorio,
                                "Grupo de edad" = grupoedad,
                                "Valor" = valor,
                                "Valor mínimo - IC 95 %" = int_inf,
                                "Valor máximo - IC 95 %" = int_sup), file, row.names = FALSE, fileEncoding = "latin1") 
          
        }
        
        
      } else if (input$indicador_csd == "PERSONAS SEGÚN CONDICIÓN MIGRATORIA Y LUGAR DE RESIDENCIA (MONTEVIDEO E INTERIOR)") {
        
        if (input$anio_csd2 %in% c(2013:2021)) {
          
          write.csv(csd() %>% 
                      arrange(statusmigratorio) %>%
                      transmute("Año" = anio, 
                                "Condición migratoria" = statusmigratorio,
                                "Departamento" = depto,
                                "Valor" = format(valor, big.mark = "."),
                                "Valor mínimo - IC 95 %" = format(int_inf, big.mark = "."),
                                "Valor máximo - IC 95 %" = format(int_sup, big.mark = ".")), file, row.names = FALSE, fileEncoding = "latin1") 
          
        } else if (input$anio_csd2 == "Evolución") {
          
          write.csv(csd_evolucion() %>% 
                      arrange(statusmigratorio) %>%
                      transmute("Año" = anio, 
                                "Condición migratoria" = statusmigratorio,
                                "Departamento" = depto,
                                "Valor" = format(valor, big.mark = "."),
                                "Valor mínimo - IC 95 %" = format(int_inf, big.mark = "."),
                                "Valor máximo - IC 95 %" = format(int_sup, big.mark = ".")), file, row.names = FALSE, fileEncoding = "latin1") 
        }
        
      } else if (input$indicador_csd == "PORCENTAJE DE POBLACIÓN AFRO SEGÚN CONDICIÓN MIGRATORIA") {
        
        if (input$anio_csd2 %in% c(2013:2019)) {
          write.csv(csd() %>% 
                      arrange(statusmigratorio) %>%
                      filter(sexo != "No afro") %>% 
                      transmute("Año" = anio, 
                                "Condición migratoria" = statusmigratorio,
                                "Ascendencia étnico-racial" = sexo,
                                "Valor (%)" = round(valor*100, 2),
                                "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                                "Valor máximo - IC 95 %" = round(int_sup*100, 2)), file, row.names = FALSE, fileEncoding = "latin1") 
        } else if (input$anio_csd2 == "Evolución") {
          write.csv(csd_evolucion() %>% 
                      arrange(statusmigratorio) %>%
                      filter(sexo != "No afro") %>% 
                      transmute("Año" = anio, 
                                "Condición migratoria" = statusmigratorio,
                                "Ascendencia étnico-racial" = sexo,
                                "Valor (%)" = round(valor*100, 2),
                                "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                                "Valor máximo - IC 95 %" = round(int_sup*100, 2)), file, row.names = FALSE, fileEncoding = "latin1") 
        }
        
        
      } else if(input$indicador_csd == "PORCENTAJE DE MUJERES SEGÚN CONDICIÓN MIGRATORIA"){
        
        if (input$anio_csd2 %in% c(2013:2019)) {
          write.csv(csd() %>% 
                      arrange(statusmigratorio) %>%
                      transmute("Año" = anio, 
                                "Condición migratoria" = statusmigratorio,
                                "Valor (%)" = round(valor*100, 2),
                                "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                                "Valor máximo - IC 95 %" = round(int_sup*100, 2)), file, row.names = FALSE, fileEncoding = "latin1")
        } else if (input$anio_csd2 == "Evolución") {
          write.csv(csd_evolucion() %>% 
                      arrange(statusmigratorio) %>%
                      transmute("Año" = anio, 
                                "Condición migratoria" = statusmigratorio,
                                "Valor (%)" = round(valor*100, 2),
                                "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                                "Valor máximo - IC 95 %" = round(int_sup*100, 2)), file, row.names = FALSE, fileEncoding = "latin1")
        }
        
        
        
      } else if(input$indicador_csd == "PERSONAS DE 0 A 17 AÑOS VINCULADAS A LA INMIGRACIÓN EXTRANJERA SEGÚN GENERACIÓN MIGRATORIA"){
        
        if (input$anio_csd2 %in% c(2013:2019)) {
          write.csv(csd() %>% 
                      arrange(generacion) %>%
                      transmute("Año" = anio, 
                                "Generación migratoria" = generacion,
                                "Valor" = format(valor, big.mark = "."),
                                "Valor mínimo - IC 95 %" = format(int_inf, big.mark = "."),
                                "Valor máximo - IC 95 %" = format(int_sup, big.mark = ".")), file, row.names = FALSE, fileEncoding = "latin1")
        } else if (input$anio_csd2 == "Evolución") {
          write.csv(csd_evolucion() %>% 
                      arrange(generacion) %>%
                      transmute("Año" = anio, 
                                "Generación migratoria" = generacion,
                                "Valor" = format(valor, big.mark = "."),
                                "Valor mínimo - IC 95 %" = format(int_inf, big.mark = "."),
                                "Valor máximo - IC 95 %" = format(int_sup, big.mark = ".")), file, row.names = FALSE, fileEncoding = "latin1")
        }
        
      } else if(input$indicador_csd == "DISTRIBUCIÓN RELATIVA DE LA INMIGRACIÓN RECIENTE POR LUGAR DE NACIMIENTO"){
        
        if (input$anio_csd2 %in% c(2013:2019)) {
          write.csv(csd() %>% 
                      arrange(statusmigratorio) %>%
                      transmute("Año" = anio, 
                                "Condición migratoria" = statusmigratorio,
                                "Lugar de nacimiento" = lugarnac,
                                "Valor (%)" = round(valor*100, 2),
                                "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                                "Valor máximo - IC 95 %" = round(int_sup*100, 2)), file, row.names = FALSE, fileEncoding = "latin1")
        } else if (input$anio_csd2 == "Evolución") {
          write.csv(csd_evolucion() %>% 
                      arrange(statusmigratorio) %>%
                      transmute("Año" = anio, 
                                "Condición migratoria" = statusmigratorio,
                                "Lugar de nacimiento" = lugarnac,
                                "Valor (%)" = round(valor*100, 2),
                                "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                                "Valor máximo - IC 95 %" = round(int_sup*100, 2)), file, row.names = FALSE, fileEncoding = "latin1") 
        }
        
      } else if(input$indicador_csd == "DISTRIBUCIÓN RELATIVA DEL RETORNO RECIENTE POR LUGAR DE RESIDENCIA CINCO AÑOS ANTES"){
        
        if (input$anio_csd2 %in% c(2013:2019)) {
          write.csv(csd() %>% 
                      arrange(statusmigratorio) %>%
                      transmute("Año" = anio, 
                                "Condición migratoria" = statusmigratorio,
                                "Lugar de residencia" = lugarnac,
                                "Valor (%)" = round(valor*100, 2),
                                "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                                "Valor máximo - IC 95 %" = round(int_sup*100, 2)), file, row.names = FALSE, fileEncoding = "latin1")
        } else if (input$anio_csd2 == "Evolución") {
          write.csv(csd_evolucion() %>% 
                      arrange(statusmigratorio) %>%
                      transmute("Año" = anio, 
                                "Condición migratoria" = statusmigratorio,
                                "Lugar de residencia" = lugarnac,
                                "Valor (%)" = round(valor*100, 2),
                                "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                                "Valor máximo - IC 95 %" = round(int_sup*100, 2)), file, row.names = FALSE, fileEncoding = "latin1")
        }
        
        
      } else if (input$indicador_csd == "DISTRIBUCIÓN RELATIVA POR NIVEL EDUCATIVO ALCANZADO SEGÚN CONDICIÓN MIGRATORIA PARA LAS PERSONAS DE 25 AÑOS Y MÁS") {
        
        write.csv(csd2() %>% 
                    arrange(statusmigratorio) %>%
                    transmute("Año" = anio, 
                              "Condición migratoria" = statusmigratorio,
                              "Nivel educativo" = niveledu,
                              "Valor (%)" = round(valor*100, 2),
                              "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                              "Valor máximo - IC 95 %" = round(int_sup*100, 2)), file, row.names = FALSE, fileEncoding = "latin1")
      }
      else if (input$indicador_csd == "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN DE 0 A 17 AÑOS POR ARREGLOS RESIDENCIALES SEGÚN GENERACIÓN MIGRATORIA") {
        
        write.csv(csd2() %>% 
                    arrange(generacion) %>%
                    transmute("Año" = anio, 
                              "Generación migratoria" = generacion,
                              "Arreglos residenciales" = arreglos,
                              "Valor (%)" = round(valor*100, 2),
                              "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                              "Valor máximo - IC 95 %" = round(int_sup*100, 2)), file, row.names = FALSE, fileEncoding = "latin1")
      }
    }
  )
  
  output$anio_mdt <- renderUI({
    
    if(input$indicador_mdt == "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN OCUPADA MAYOR DE 24 AÑOS POR ADECUACIÓN EDUCATIVA SEGÚN CONDICIÓN MIGRATORIA") {
      
      selectInput(
        inputId = "anio_mdt1",
        label = "Seleccione año:",
        choices = c("2015-2019"))
      
    } else if (input$indicador_mdt == "TASA DE ACTIVIDAD SEGÚN CONDICIÓN MIGRATORIA Y SEXO" |
               input$indicador_mdt == "TASA DE OCUPACIÓN SEGÚN CONDICIÓN MIGRATORIA Y SEXO" |
               input$indicador_mdt == "TASA DE DESEMPLEO SEGÚN CONDICIÓN MIGRATORIA Y SEXO" ){
      
      selectInput(
        inputId = "anio_mdt3",
        label = "Seleccione año:",
        choices = c("2013", "2014", "2015", "2016", "2017", "2018", "2019"),
        selected = "2019")
      
    } else { 
      
      selectInput(
        inputId = "anio_mdt2",
        label = "Seleccione año:",
        choices = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "Evolución (2013-2019)"),
        selected = "2019")
      
      
    }
    
  })
  
  mdt <- reactive({
    
    indicadores_mdt <- indicadores %>% 
      filter(nomindicador == input$indicador_mdt) %>% 
      filter(anio == input$anio_mdt2) %>%
      mutate(statusmigratorio = factor(statusmigratorio, levels = c("Nativo/No migrante", 
                                                                    "Inmigrante reciente", 
                                                                    "Inmigrante antiguo",
                                                                    "Retornado reciente", 
                                                                    "Retornado antiguo"))) %>% 
      arrange(statusmigratorio) 
    
  }) 
  
  mdt3 <- reactive({
    
    indicadores_mdt <- indicadores %>% 
      filter(nomindicador == input$indicador_mdt) %>% 
      filter(anio == input$anio_mdt3) %>%
      mutate(statusmigratorio = factor(statusmigratorio, levels = c("Nativo/No migrante", 
                                                                    "Inmigrante reciente", 
                                                                    "Inmigrante antiguo",
                                                                    "Retornado reciente", 
                                                                    "Retornado antiguo"))) %>% 
      arrange(statusmigratorio) 
    
  }) 
  
  mdt2 <- reactive({
    
    indicadores_mdt <- indicadores %>% 
      filter(nomindicador == input$indicador_mdt) %>% 
      filter(anio == input$anio_mdt1) %>%
      mutate(statusmigratorio = factor(statusmigratorio, levels = c("Nativo/No migrante", 
                                                                    "Inmigrante reciente", 
                                                                    "Inmigrante antiguo",
                                                                    "Retornado reciente", 
                                                                    "Retornado antiguo"))) %>% 
      arrange(statusmigratorio) 
    
  }) 
  
  mdt_evolucion <- reactive({
    
    indicadores_mdt <- indicadores %>% 
      filter(nomindicador == input$indicador_mdt) %>%
      mutate(statusmigratorio = factor(statusmigratorio, levels = c("Nativo/No migrante", 
                                                                    "Inmigrante reciente", 
                                                                    "Inmigrante antiguo",
                                                                    "Retornado reciente", 
                                                                    "Retornado antiguo"))) %>% 
      arrange(statusmigratorio) 
    
  }) 
  
  
  output$title_mdt <- renderUI({
    
    if(input$indicador_csd=="DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN OCUPADA MAYOR DE 24 AÑOS POR ADECUACIÓN EDUCATIVA SEGÚN CONDICIÓN MIGRATORIA"){
      helpText(HTML(paste(input$indicador_mdt)))}
    
    else if (input$indicador_mdt == "TASA DE ACTIVIDAD SEGÚN CONDICIÓN MIGRATORIA Y SEXO" |
             input$indicador_mdt == "TASA DE OCUPACIÓN SEGÚN CONDICIÓN MIGRATORIA Y SEXO" |
             input$indicador_mdt == "TASA DE DESEMPLEO SEGÚN CONDICIÓN MIGRATORIA Y SEXO" ){
      helpText(HTML(paste(input$indicador_mdt,"-",input$anio_mdt3)))}
    
    else{ 
      helpText(HTML(paste(input$indicador_mdt,"-",input$anio_mdt2)))
    }
    
  })
  
  
  
  output$barPlot_mdt <- renderPlot({
    
    if((input$indicador_mdt == "TASA DE ACTIVIDAD SEGÚN CONDICIÓN MIGRATORIA Y SEXO" |
        input$indicador_mdt == "TASA DE OCUPACIÓN SEGÚN CONDICIÓN MIGRATORIA Y SEXO" |
        input$indicador_mdt == "TASA DE DESEMPLEO SEGÚN CONDICIÓN MIGRATORIA Y SEXO") & 
       (input$anio_mdt3 %in% c(2013:2019))){
      
      mdt <- ggplot(mdt3(), aes(statusmigratorio, valor)) +
        # geom_linerange(aes(ymin = int_inf, ymax = int_sup), color = blue_bdd3, size = 2, alpha = .4) +
        # geom_point(size = 5, color = blue_bdd) +
        geom_bar(stat = "identity", position = "dodge",fill="#7bb7dd", width=0.5)+
        geom_errorbar(aes(ymin = int_inf, ymax = int_sup), width = 0.2,
                      position = position_dodge(0.9))+
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::percent_format(accuracy = 1)) +
        facet_wrap(~ sexo) + 
        theme(axis.text.x = element_text(size=13,angle = 90),
              plot.title = element_text(vjust=1.5,size = 13))+
        labs(x = "",
             y = "")
      #      title = paste(
      #        input$indicador_mdt,
      #        " (",
      #        input$anio_mdt3,
      #        ")", 
      #        sep = ""
      #      ))
    } else if (input$indicador_mdt == "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN OCUPADA MAYOR DE 24 AÑOS POR ADECUACIÓN EDUCATIVA SEGÚN CONDICIÓN MIGRATORIA") {
      
      mdt <- mdt2() %>% 
        mutate(niveledu = fct_relevel(niveledu, "Sin dato", after = Inf)) %>% 
        ggplot(aes(niveledu, valor)) +
        # geom_linerange(aes(ymin = int_inf, ymax = int_sup), color = blue_bdd3, size = 2, alpha = .4) +
        # geom_point(size = 3, color = blue_bdd) +
        geom_bar(stat = "identity", position = "dodge",fill="#7bb7dd", width=0.5)+
        geom_errorbar(aes(ymin = int_inf, ymax = int_sup), width = 0.2,
                      position = position_dodge(0.9))+
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        facet_wrap(~ statusmigratorio, ncol = 2, scales = "free_y") + 
        theme(axis.text.x = element_text(size=9,angle = 90,hjust=0.95,vjust=0.2),
              axis.text.y = element_text(size=10),
              plot.title = element_text(vjust=1.5,size = 13),
              strip.text.x = element_text(size = 9))+
        labs(x = "",
             y = "")
      
    } else if ((input$indicador_mdt == "PORCENTAJE DE LA POBLACIÓN OCUPADA QUE SE DESEMPEÑA COMO ASALARIADOS PRIVADOS SEGÚN CONDICIÓN MIGRATORIA" |         
                input$indicador_mdt == "PORCENTAJE DE LA POBLACIÓN OCUPADA QUE NO REALIZA APORTES JUBILATORIOS SEGÚN CONDICIÓN MIGRATORIA") & 
               input$anio_mdt2 %in% c(2013:2019)) {
      
      mdt <- ggplot(mdt(), aes(statusmigratorio, valor)) +
        # geom_linerange(aes(ymin = int_inf, ymax = int_sup), color = blue_bdd3, size = 2, alpha = .4) +
        # geom_point(size = 5, color = blue_bdd) +
        geom_bar(stat = "identity", position = "dodge",fill="#7bb7dd", width=0.5)+
        geom_errorbar(aes(ymin = int_inf, ymax = int_sup), width = 0.2,
                      position = position_dodge(0.9))+
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(x = "",
             y = "")+
        #      title = paste(
        #        unique(mdt()$titulo_arreglado),
        #        " (",
        #        input$anio_mdt2,
        #        ")", 
        #        sep = ""
        #      )) +
        theme(axis.text.x = element_text(size=13,angle = 90),
              plot.title = element_text(vjust=1.5,size = 13))
    } else if ((input$indicador_mdt == "PORCENTAJE DE LA POBLACIÓN OCUPADA QUE SE DESEMPEÑA COMO ASALARIADOS PRIVADOS SEGÚN CONDICIÓN MIGRATORIA" |         
                input$indicador_mdt == "PORCENTAJE DE LA POBLACIÓN OCUPADA QUE NO REALIZA APORTES JUBILATORIOS SEGÚN CONDICIÓN MIGRATORIA") & 
               (input$anio_mdt2 == "Evolución (2013-2019)")) {
      
      mdt <- ggplot(mdt_evolucion(), aes(anio, valor, color = statusmigratorio, fill = statusmigratorio, group = statusmigratorio)) +
        geom_line(size = 1.1) +
        geom_ribbon(aes(ymin = int_inf, ymax = int_sup), alpha = .3, colour = NA) +
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::percent_format(accuracy = 1)) +
        theme(axis.text.x = element_text(size=13,angle = 90),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))+
        labs(x = "",
             y = "")
      #      color = "",
      #      fill = "",
      #      title = paste(
      #        input$indicador_mdt,
      #        " (",
      #        "2013-2019",
      #        ")", 
      #        sep = ""
      #      )) 
    }
    
    
    print(mdt)
    ggsave("www/mdt.png")
  })
  
  
  output$baja_barPlot_mdt <- downloadHandler(
    filename <- function() {
      paste("mdt", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/mdt.png", file)
    },
    contentType = "www/mdt"
    
    
  )
  
  
  
  
  
  
  
  
  output$tabla_resultado_mdt  <- renderDT({
    
    if((input$indicador_mdt == "TASA DE ACTIVIDAD SEGÚN CONDICIÓN MIGRATORIA Y SEXO" |
        input$indicador_mdt == "TASA DE OCUPACIÓN SEGÚN CONDICIÓN MIGRATORIA Y SEXO" |
        input$indicador_mdt == "TASA DE DESEMPLEO SEGÚN CONDICIÓN MIGRATORIA Y SEXO")){
      
      if (input$anio_mdt3 %in% c(2013:2019)) { 
        datatable(mdt3() %>% 
                    arrange(statusmigratorio) %>%
                    transmute("Año" = anio, 
                              "Condición migratoria" = statusmigratorio,
                              "Sexo" = sexo,
                              "Valor (%)" = round(valor*100, 2),
                              "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                              "Valor máximo - IC 95 %" = round(int_sup*100, 2)), rownames = FALSE, options = list(dom = 't')) 
      } else if (input$anio_mdt2 == "Evolución (2013-2019)") {
        datatable(mdt_evolucion() %>% 
                    arrange(statusmigratorio) %>%
                    transmute("Año" = anio, 
                              "Condición migratoria" = statusmigratorio,
                              "Sexo" = sexo,
                              "Valor (%)" = round(valor*100, 2),
                              "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                              "Valor máximo - IC 95 %" = round(int_sup*100, 2)), rownames = FALSE, options = list(dom = 'ltipr')) 
      }
      
      
    } else if (input$indicador_mdt == "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN OCUPADA MAYOR DE 24 AÑOS POR ADECUACIÓN EDUCATIVA SEGÚN CONDICIÓN MIGRATORIA") {
      
      datatable(mdt2() %>% 
                  arrange(statusmigratorio) %>%
                  transmute("Año" = anio, 
                            "Condición migratoria" = statusmigratorio,
                            "Adecuación educativa" = niveledu,
                            "Valor (%)" = round(valor*100, 2),
                            "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                            "Valor máximo - IC 95 %" = round(int_sup*100, 2)), rownames = FALSE, options = list(dom = 't'))
    } else if ((input$indicador_mdt == "PORCENTAJE DE LA POBLACIÓN OCUPADA QUE SE DESEMPEÑA COMO ASALARIADOS PRIVADOS SEGÚN CONDICIÓN MIGRATORIA" |         
                input$indicador_mdt == "PORCENTAJE DE LA POBLACIÓN OCUPADA QUE NO REALIZA APORTES JUBILATORIOS SEGÚN CONDICIÓN MIGRATORIA")) {
      
      if (input$anio_mdt2 %in% c(2013:2019)) { 
        datatable(mdt() %>% 
                    arrange(statusmigratorio) %>%
                    transmute("Año" = anio, 
                              "Condición migratoria" = statusmigratorio,
                              "Valor (%)" = round(valor*100, 2),
                              "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                              "Valor máximo - IC 95 %" = round(int_sup*100, 2)), rownames = FALSE, options = list(dom = 't')) 
      } else if (input$anio_mdt2 == "Evolución (2013-2019)") {
        datatable(mdt_evolucion() %>% 
                    arrange(statusmigratorio) %>%
                    transmute("Año" = anio, 
                              "Condición migratoria" = statusmigratorio,
                              "Valor (%)" = round(valor*100, 2),
                              "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                              "Valor máximo - IC 95 %" = round(int_sup*100, 2)), rownames = FALSE, options = list(dom = 'ltipr')) 
      }
    }
    
    
    
    
  }) 
  
  ##de nuevo
  
  output$dl_tabla_resultado_mdt <- downloadHandler(
    
    
    filename = function() {
      paste0("resultados- ", input$indicador_mdt, ".csv", sep = "")
    },
    content = function(file) {
      
      if((input$indicador_mdt == "TASA DE ACTIVIDAD SEGÚN CONDICIÓN MIGRATORIA Y SEXO" |
          input$indicador_mdt == "TASA DE OCUPACIÓN SEGÚN CONDICIÓN MIGRATORIA Y SEXO" |
          input$indicador_mdt == "TASA DE DESEMPLEO SEGÚN CONDICIÓN MIGRATORIA Y SEXO")){
        
        if (input$anio_mdt3 %in% c(2013:2019)) { 
          write.csv(mdt3() %>% 
                      arrange(statusmigratorio) %>%
                      transmute("Año" = anio, 
                                "Condición migratoria" = statusmigratorio,
                                "Sexo" = sexo,
                                "Valor (%)" = round(valor*100, 2),
                                "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                                "Valor máximo - IC 95 %" = round(int_sup*100, 2)), file, row.names = FALSE, fileEncoding = "latin1") 
        } else if (input$anio_mdt2 == "Evolución (2013-2019)") {
          write.csv(mdt_evolucion() %>% 
                      arrange(statusmigratorio) %>%
                      transmute("Año" = anio, 
                                "Condición migratoria" = statusmigratorio,
                                "Sexo" = sexo,
                                "Valor (%)" = round(valor*100, 2),
                                "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                                "Valor máximo - IC 95 %" = round(int_sup*100, 2)), file, row.names = FALSE, fileEncoding = "latin1")
        }
        
        
      } else if (input$indicador_mdt == "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN OCUPADA MAYOR DE 24 AÑOS POR ADECUACIÓN EDUCATIVA SEGÚN CONDICIÓN MIGRATORIA") {
        
        write.csv(mdt2() %>% 
                    arrange(statusmigratorio) %>%
                    transmute("Año" = anio, 
                              "Condición migratoria" = statusmigratorio,
                              "Adecuación educativa" = niveledu,
                              "Valor (%)" = round(valor*100, 2),
                              "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                              "Valor máximo - IC 95 %" = round(int_sup*100, 2)),file, row.names = FALSE, fileEncoding = "latin1")
      } else if ((input$indicador_mdt == "PORCENTAJE DE LA POBLACIÓN OCUPADA QUE SE DESEMPEÑA COMO ASALARIADOS PRIVADOS SEGÚN CONDICIÓN MIGRATORIA" |         
                  input$indicador_mdt == "PORCENTAJE DE LA POBLACIÓN OCUPADA QUE NO REALIZA APORTES JUBILATORIOS SEGÚN CONDICIÓN MIGRATORIA")) {
        
        if (input$anio_mdt2 %in% c(2013:2019)) { 
          write.csv(mdt() %>% 
                      arrange(statusmigratorio) %>%
                      transmute("Año" = anio, 
                                "Condición migratoria" = statusmigratorio,
                                "Valor (%)" = round(valor*100, 2),
                                "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                                "Valor máximo - IC 95 %" = round(int_sup*100, 2)), file, row.names = FALSE, fileEncoding = "latin1") 
        } else if (input$anio_mdt2 == "Evolución (2013-2019)") {
          write.csv(mdt_evolucion() %>% 
                      arrange(statusmigratorio) %>%
                      transmute("Año" = anio, 
                                "Condición migratoria" = statusmigratorio,
                                "Valor (%)" = round(valor*100, 2),
                                "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                                "Valor máximo - IC 95 %" = round(int_sup*100, 2)), file, row.names = FALSE, fileEncoding = "latin1") 
        }
        
        
        
        
        
      } }
  )    
  
  
  
  
  
  
  
  
  pobreza <- reactive({
    
    indicadores_p <- indicadores %>% 
      filter(nomindicador == input$indicador_p) %>%
      arrange(statusmigratorio) 
    
  }) 
  
  
  output$title_p <- renderUI({ 
    helpText(HTML(paste(input$indicador_p,input$anio_p)))
  })
  
  
  
  output$barPlot_p <- renderPlot({
    
    if(input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES EN SITUACIÓN DE POBREZA (% DEL TOTAL DE PERSONAS) (INE)" |                                                 
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES EN SITUACIÓN DE POBREZA (% DEL TOTAL DE PERSONAS) (INE)" |                                                 
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES EN SITUACIÓN DE HACINAMIENTO (% DEL TOTAL DE PERSONAS)" |                                                  
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES EN SITUACIÓN DE HACINAMIENTO (% DEL TOTAL DE PERSONAS)" |                                                  
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES CON PAREDES O TECHOS DE DESECHO O PISO DE TIERRA (% DEL TOTAL DE PERSONAS)" |                              
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES CON PAREDES O TECHOS DE DESECHO O PISO DE TIERRA (% DEL TOTAL DE PERSONAS)" |                              
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES SIN AGUA POTABLE (% DEL TOTAL DE PERSONAS)" |                                                              
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES SIN AGUA POTABLE (% DEL TOTAL DE PERSONAS)" |                                                              
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES SERVICIO HIGIÉNICO DE CALIDAD  (% DEL TOTAL DE PERSONAS)" |                                                
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES SERVICIO HIGIÉNICO DE CALIDAD  (% DEL TOTAL DE PERSONAS)" |                                                
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES SIN ARTEFACTOS BÁSICOS DE CONFORT  (% DEL TOTAL DE PERSONAS)" |                                            
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES SIN ARTEFACTOS BÁSICOS DE CONFORT  (% DEL TOTAL DE PERSONAS)" |                                            
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES EN SITUACIÓN DE POBREZA (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS) (INE)" |                   
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES EN SITUACIÓN DE POBREZA (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS) (INE)" |                   
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES CON NECESIDADES BÁSICAS INSATISFECHAS (MET. 2011) (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS)" |
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES CON NECESIDADES BÁSICAS INSATISFECHAS (MET. 2011) (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS)" |
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES EN SITUACIÓN DE HACINAMIENTO (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |                   
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES EN SITUACIÓN DE HACINAMIENTO (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |                   
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES CON PAREDES O TECHOS DE DESECHO O PISO DE TIERRA (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES CON PAREDES O TECHOS DE DESECHO O PISO DE TIERRA (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS  EN HOGARES SIN AGUA POTABLE (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |                              
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS  EN HOGARES SIN AGUA POTABLE (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |                              
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS  EN HOGARES SERVICIO HIGIÉNICO DE CALIDAD  (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |                
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS  EN HOGARES SERVICIO HIGIÉNICO DE CALIDAD  (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |                
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES SIN ARTEFACTOS BÁSICOS DE CONFORT  (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS)" |              
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES SIN ARTEFACTOS BÁSICOS DE CONFORT  (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS)") {
      
      pobreza <- ggplot(data = pobreza(), aes(statusmigratorio, valor)) +
        # geom_linerange(aes(ymin = int_inf, ymax = int_sup), color = blue_bdd3, size = 2, alpha = .4) +
        # geom_point(size = 5, color = blue_bdd) +
        geom_bar(stat = "identity", position = "dodge",fill="#7bb7dd", width=0.5)+
        geom_errorbar(aes(ymin = int_inf, ymax = int_sup), width = 0.2,
                      position = position_dodge(0.9))+
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        theme(axis.text.x = element_text(size=10,angle = 90),
              plot.title = element_text(vjust=1.5,size = 13))+
        labs(x = "",
             y = "")
      #      title = paste(
      #        unique(pobreza()$titulo_arreglado),
      #        " (",
      #        input$anio_p,
      #        ")", sep = ""
      #      ))
      
    } else if (input$indicador_p == "PROMEDIO DE INGRESO PER-CÁPITA DE LOS HOGARES (CTE. BASE DICIEMBRE 2006)") {
      
      pobreza <- ggplot(data = pobreza(), aes(reorder(statusmig_hogar, valor), valor)) +
        # geom_linerange(aes(ymin = int_inf, ymax = int_sup), color = blue_bdd3, size = 2, alpha = .4) +
        # geom_point(size = 5, color = blue_bdd) + 
        geom_bar(stat = "identity", position = "dodge",fill="#7bb7dd", width=0.5)+
        geom_errorbar(aes(ymin = int_inf, ymax = int_sup), width = 0.2,
                      position = position_dodge(0.9))+
        scale_y_continuous(labels = scales::dollar_format(big.mark = ".", prefix = "$")) +
        labs(x = "",
             y = "")+
        #      title = paste(
        #        unique(pobreza()$titulo_arreglado),
        #        " (",
        #        input$anio_p,
        #        ")", sep = ""
        #      ))+
        theme(axis.text.x = element_text(size=10,angle = 90),
              plot.title = element_text(vjust=1.5,size = 13))
      
    } else if(input$indicador_p == "PORCENTAJE DE HOGARES EN SITUACIÓN DE POBREZA (% DEL TOTAL DE HOGARES) (INE)" |                                        
              input$indicador_p == "PORCENTAJE DE HOGARES EN SITUACIÓN DE POBREZA (% DEL TOTAL DE HOGARES) (INE)" |                                          
              input$indicador_p == "PORCENTAJE DE HOGARES CON JEFATURA MASCULINA EN SITUACIÓN DE POBREZA (% DEL TOTAL DE HOGARES) (INE)" |               
              input$indicador_p == "PORCENTAJE DE HOGARES CON JEFATURA MASCULINA EN SITUACIÓN DE POBREZA (% DEL TOTAL DE HOGARES) (INE)" |                   
              input$indicador_p == "PORCENTAJE DE HOGARES CON JEFATURA FEMENINA EN SITUACIÓN DE POBREZA (% DEL TOTAL DE HOGARES) (INE)" |                    
              input$indicador_p == "PORCENTAJE DE HOGARES CON JEFATURA FEMENINA EN SITUACIÓN DE POBREZA (% DEL TOTAL DE HOGARES) (INE)" |                    
              input$indicador_p == "PORCENTAJE DE HOGARES EN SITUACIÓN DE HACINAMIENTO (% DEL TOTAL DE HOGARES) (INE)" |                                     
              input$indicador_p == "PORCENTAJE DE HOGARES EN SITUACIÓN DE HACINAMIENTO (% DEL TOTAL DE HOGARES) (INE)" |                                     
              input$indicador_p == "PORCENTAJE DE HOGARES CON PAREDES O TECHOS DE DESECHO O PISO DE TIERRA (% DEL TOTAL DE HOGARES)" |                       
              input$indicador_p == "PORCENTAJE DE HOGARES CON PAREDES O TECHOS DE DESECHO O PISO DE TIERRA (% DEL TOTAL DE HOGARES)" |                       
              input$indicador_p == "PORCENTAJE DE HOGARES SIN AGUA POTABLE (% DEL TOTAL DE HOGARES)" |                                                       
              input$indicador_p == "PORCENTAJE DE HOGARES SIN AGUA POTABLE (% DEL TOTAL DE HOGARES)" |                                                       
              input$indicador_p == "PORCENTAJE DE HOGARES SIN SERVICIO HIGIÉNICO DE CALIDAD (% DEL TOTAL DE HOGARES)" |                                      
              input$indicador_p == "PORCENTAJE DE HOGARES SIN SERVICIO HIGIÉNICO DE CALIDAD (% DEL TOTAL DE HOGARES)" |                                      
              input$indicador_p == "PORCENTAJE DE HOGARES SIN ARTEFACTOS BÁSICOS DE CONFORT (% DEL TOTAL DE HOGARES)" |                                      
              input$indicador_p == "PORCENTAJE DE HOGARES SIN ARTEFACTOS BÁSICOS DE CONFORT (% DEL TOTAL DE HOGARES)" |                                      
              input$indicador_p == "PORCENTAJE DE HOGARES QUE TIENEN PERSONAS DE 4 A 17 AÑOS QUE NO ASISTEN A CENTROS EDUCATIVOS (% DEL TOTAL DE HOGARES)" | 
              input$indicador_p == "PORCENTAJE DE HOGARES QUE TIENEN PERSONAS DE 4 A 17 AÑOS QUE NO ASISTEN A CENTROS EDUCATIVOS (% DEL TOTAL DE HOGARES)" | 
              input$indicador_p == "PROMEDIO DE INGRESO PER-CÁPITA DE LOS HOGARES (CTE. BASE DICIEMBRE 2006)" |                                             
              input$indicador_p == "PROMEDIO DE INGRESO PER-CÁPITA DE LOS HOGARES (CTE. BASE DICIEMBRE 2006)"){
      
      pobreza <- ggplot(data = pobreza(), aes(reorder(statusmig_hogar, valor), valor)) +
        # geom_linerange(aes(ymin = int_inf, ymax = int_sup), color = blue_bdd3, size = 2, alpha = .4) +
        # geom_point(size = 5, color = blue_bdd) + 
        geom_bar(stat = "identity", position = "dodge",fill="#7bb7dd", width=0.5)+
        geom_errorbar(aes(ymin = int_inf, ymax = int_sup), width = 0.2,
                      position = position_dodge(0.9))+
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(x = "",
             y = "")+
        #      title = paste(
        #        unique(pobreza()$titulo_arreglado),
        #        " (",
        #        input$anio_p,
        #        ")", sep = ""
        #      ))+
        theme(axis.text.x = element_text(size=10,angle = 90),
              plot.title = element_text(vjust=1.5,size = 13))
      
    } 
    
    print(pobreza)
    ggsave("www/pobreza.png")
  })
  
  output$baja_barPlot_p <- downloadHandler(
    filename <- function() {
      paste("pobreza", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/pobreza.png", file)
    },
    contentType = "www/pobreza"
    
    
  )
  
  output$tabla_resultado_p  <- renderDT({
    
    if(input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES EN SITUACIÓN DE POBREZA (% DEL TOTAL DE PERSONAS) (INE)" |                                                 
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES EN SITUACIÓN DE POBREZA (% DEL TOTAL DE PERSONAS) (INE)" |                                                 
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES EN SITUACIÓN DE HACINAMIENTO (% DEL TOTAL DE PERSONAS)" |                                                  
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES EN SITUACIÓN DE HACINAMIENTO (% DEL TOTAL DE PERSONAS)" |                                                  
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES CON PAREDES O TECHOS DE DESECHO O PISO DE TIERRA (% DEL TOTAL DE PERSONAS)" |                              
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES CON PAREDES O TECHOS DE DESECHO O PISO DE TIERRA (% DEL TOTAL DE PERSONAS)" |                              
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES SIN AGUA POTABLE (% DEL TOTAL DE PERSONAS)" |                                                              
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES SIN AGUA POTABLE (% DEL TOTAL DE PERSONAS)" |                                                              
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES SERVICIO HIGIÉNICO DE CALIDAD  (% DEL TOTAL DE PERSONAS)" |                                                
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES SERVICIO HIGIÉNICO DE CALIDAD  (% DEL TOTAL DE PERSONAS)" |                                                
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES SIN ARTEFACTOS BÁSICOS DE CONFORT  (% DEL TOTAL DE PERSONAS)" |                                            
       input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES SIN ARTEFACTOS BÁSICOS DE CONFORT  (% DEL TOTAL DE PERSONAS)" |                                            
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES EN SITUACIÓN DE POBREZA (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS) (INE)" |                   
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES EN SITUACIÓN DE POBREZA (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS) (INE)" |                   
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES CON NECESIDADES BÁSICAS INSATISFECHAS (MET. 2011) (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS)" |
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES CON NECESIDADES BÁSICAS INSATISFECHAS (MET. 2011) (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS)" |
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES EN SITUACIÓN DE HACINAMIENTO (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |                   
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES EN SITUACIÓN DE HACINAMIENTO (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |                   
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES CON PAREDES O TECHOS DE DESECHO O PISO DE TIERRA (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES CON PAREDES O TECHOS DE DESECHO O PISO DE TIERRA (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS  EN HOGARES SIN AGUA POTABLE (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |                              
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS  EN HOGARES SIN AGUA POTABLE (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |                              
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS  EN HOGARES SERVICIO HIGIÉNICO DE CALIDAD  (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |                
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS  EN HOGARES SERVICIO HIGIÉNICO DE CALIDAD  (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |                
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES SIN ARTEFACTOS BÁSICOS DE CONFORT  (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS)" |              
       input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES SIN ARTEFACTOS BÁSICOS DE CONFORT  (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS)"){
      
      datatable(pobreza() %>% 
                  arrange(statusmigratorio) %>%
                  transmute("Año" = anio, 
                            "Condición migratoria" = statusmigratorio,
                            "Valor (%)" = round(valor*100, 2),
                            "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                            "Valor máximo - IC 95 %" = round(int_sup*100, 2)), rownames = FALSE, options = list(dom = 't')) 
      
    } else if (input$indicador_p == "PROMEDIO DE INGRESO PER-CÁPITA DE LOS HOGARES (CTE. BASE DICIEMBRE 2006)") {
      
      datatable(pobreza() %>% 
                  transmute("Año" = anio, 
                            "Condición migratoria hogar" = statusmig_hogar,
                            "Valor ($)" = format(round(valor), big.mark = "."),
                            "Valor mínimo - IC 95 %" = round(int_inf, 2),
                            "Valor máximo - IC 95 %" = round(int_sup, 2)), rownames = FALSE, options = list(dom = 't')) 
      
    } else if(input$indicador_p == "PORCENTAJE DE HOGARES EN SITUACIÓN DE POBREZA (% DEL TOTAL DE HOGARES) (INE)" |                                        
              input$indicador_p == "PORCENTAJE DE HOGARES EN SITUACIÓN DE POBREZA (% DEL TOTAL DE HOGARES) (INE)" |                                          
              input$indicador_p == "PORCENTAJE DE HOGARES CON JEFATURA MASCULINA EN SITUACIÓN DE POBREZA (% DEL TOTAL DE HOGARES) (INE)" |               
              input$indicador_p == "PORCENTAJE DE HOGARES CON JEFATURA MASCULINA EN SITUACIÓN DE POBREZA (% DEL TOTAL DE HOGARES) (INE)" |                   
              input$indicador_p == "PORCENTAJE DE HOGARES CON JEFATURA FEMENINA EN SITUACIÓN DE POBREZA (% DEL TOTAL DE HOGARES) (INE)" |                    
              input$indicador_p == "PORCENTAJE DE HOGARES CON JEFATURA FEMENINA EN SITUACIÓN DE POBREZA (% DEL TOTAL DE HOGARES) (INE)" |                    
              input$indicador_p == "PORCENTAJE DE HOGARES EN SITUACIÓN DE HACINAMIENTO (% DEL TOTAL DE HOGARES) (INE)" |                                     
              input$indicador_p == "PORCENTAJE DE HOGARES EN SITUACIÓN DE HACINAMIENTO (% DEL TOTAL DE HOGARES) (INE)" |                                     
              input$indicador_p == "PORCENTAJE DE HOGARES CON PAREDES O TECHOS DE DESECHO O PISO DE TIERRA (% DEL TOTAL DE HOGARES)" |                       
              input$indicador_p == "PORCENTAJE DE HOGARES CON PAREDES O TECHOS DE DESECHO O PISO DE TIERRA (% DEL TOTAL DE HOGARES)" |                       
              input$indicador_p == "PORCENTAJE DE HOGARES SIN AGUA POTABLE (% DEL TOTAL DE HOGARES)" |                                                       
              input$indicador_p == "PORCENTAJE DE HOGARES SIN AGUA POTABLE (% DEL TOTAL DE HOGARES)" |                                                       
              input$indicador_p == "PORCENTAJE DE HOGARES SIN SERVICIO HIGIÉNICO DE CALIDAD (% DEL TOTAL DE HOGARES)" |                                      
              input$indicador_p == "PORCENTAJE DE HOGARES SIN SERVICIO HIGIÉNICO DE CALIDAD (% DEL TOTAL DE HOGARES)" |                                      
              input$indicador_p == "PORCENTAJE DE HOGARES SIN ARTEFACTOS BÁSICOS DE CONFORT (% DEL TOTAL DE HOGARES)" |                                      
              input$indicador_p == "PORCENTAJE DE HOGARES SIN ARTEFACTOS BÁSICOS DE CONFORT (% DEL TOTAL DE HOGARES)" |                                      
              input$indicador_p == "PORCENTAJE DE HOGARES QUE TIENEN PERSONAS DE 4 A 17 AÑOS QUE NO ASISTEN A CENTROS EDUCATIVOS (% DEL TOTAL DE HOGARES)" | 
              input$indicador_p == "PORCENTAJE DE HOGARES QUE TIENEN PERSONAS DE 4 A 17 AÑOS QUE NO ASISTEN A CENTROS EDUCATIVOS (% DEL TOTAL DE HOGARES)" | 
              input$indicador_p == "PROMEDIO DE INGRESO PER-CÁPITA DE LOS HOGARES (CTE. BASE DICIEMBRE 2006)" |                                             
              input$indicador_p == "PROMEDIO DE INGRESO PER-CÁPITA DE LOS HOGARES (CTE. BASE DICIEMBRE 2006)"){
      
      datatable(pobreza() %>% 
                  transmute("Año" = anio, 
                            "Condición migratoria hogar" = statusmig_hogar,
                            "Valor (%)" = round(valor*100, 2),
                            "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                            "Valor máximo - IC 95 %" = round(int_sup*100, 2)), rownames = FALSE, options = list(dom = 't')) 
      
    } 
    
  }) 
  
  output$dl_tabla_resultado_p <- downloadHandler(
    
    
    filename = function() {
      paste0("resultados- ", input$indicador_p, ".csv", sep = "")
    },
    content = function(file) {
      
      if(input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES EN SITUACIÓN DE POBREZA (% DEL TOTAL DE PERSONAS) (INE)" |                                                 
         input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES EN SITUACIÓN DE POBREZA (% DEL TOTAL DE PERSONAS) (INE)" |                                                 
         input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES EN SITUACIÓN DE HACINAMIENTO (% DEL TOTAL DE PERSONAS)" |                                                  
         input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES EN SITUACIÓN DE HACINAMIENTO (% DEL TOTAL DE PERSONAS)" |                                                  
         input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES CON PAREDES O TECHOS DE DESECHO O PISO DE TIERRA (% DEL TOTAL DE PERSONAS)" |                              
         input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES CON PAREDES O TECHOS DE DESECHO O PISO DE TIERRA (% DEL TOTAL DE PERSONAS)" |                              
         input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES SIN AGUA POTABLE (% DEL TOTAL DE PERSONAS)" |                                                              
         input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES SIN AGUA POTABLE (% DEL TOTAL DE PERSONAS)" |                                                              
         input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES SERVICIO HIGIÉNICO DE CALIDAD  (% DEL TOTAL DE PERSONAS)" |                                                
         input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES SERVICIO HIGIÉNICO DE CALIDAD  (% DEL TOTAL DE PERSONAS)" |                                                
         input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES SIN ARTEFACTOS BÁSICOS DE CONFORT  (% DEL TOTAL DE PERSONAS)" |                                            
         input$indicador_p == "PORCENTAJE DE PERSONAS EN HOGARES SIN ARTEFACTOS BÁSICOS DE CONFORT  (% DEL TOTAL DE PERSONAS)" |                                            
         input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES EN SITUACIÓN DE POBREZA (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS) (INE)" |                   
         input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES EN SITUACIÓN DE POBREZA (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS) (INE)" |                   
         input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES CON NECESIDADES BÁSICAS INSATISFECHAS (MET. 2011) (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS)" |
         input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES CON NECESIDADES BÁSICAS INSATISFECHAS (MET. 2011) (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS)" |
         input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES EN SITUACIÓN DE HACINAMIENTO (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |                   
         input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES EN SITUACIÓN DE HACINAMIENTO (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |                   
         input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES CON PAREDES O TECHOS DE DESECHO O PISO DE TIERRA (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |
         input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES CON PAREDES O TECHOS DE DESECHO O PISO DE TIERRA (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |
         input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS  EN HOGARES SIN AGUA POTABLE (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |                              
         input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS  EN HOGARES SIN AGUA POTABLE (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |                              
         input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS  EN HOGARES SERVICIO HIGIÉNICO DE CALIDAD  (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |                
         input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS  EN HOGARES SERVICIO HIGIÉNICO DE CALIDAD  (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS )" |                
         input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES SIN ARTEFACTOS BÁSICOS DE CONFORT  (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS)" |              
         input$indicador_p == "PORCENTAJE DE PERSONAS DE 0 A 17 AÑOS EN HOGARES SIN ARTEFACTOS BÁSICOS DE CONFORT  (% DEL TOTAL DE PERSONAS DE 0 A 17 AÑOS)"){
        
        write.csv(pobreza() %>% 
                    arrange(statusmigratorio) %>%
                    transmute("Año" = anio, 
                              "Condición migratoria" = statusmigratorio,
                              "Valor (%)" = round(valor*100, 2),
                              "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                              "Valor máximo - IC 95 %" = round(int_sup*100, 2)), file, row.names = FALSE, fileEncoding = "latin1")
        
      } else if (input$indicador_p == "PROMEDIO DE INGRESO PER-CÁPITA DE LOS HOGARES (CTE. BASE DICIEMBRE 2006)") {
        
        write.csv(pobreza() %>% 
                    transmute("Año" = anio, 
                              "Condición migratoria hogar" = statusmig_hogar,
                              "Valor ($)" = format(round(valor), big.mark = "."),
                              "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                              "Valor máximo - IC 95 %" = round(int_sup*100, 2)), file, row.names = FALSE, fileEncoding = "latin1") 
        
      } else if(input$indicador_p == "PORCENTAJE DE HOGARES EN SITUACIÓN DE POBREZA (% DEL TOTAL DE HOGARES) (INE)" |                                        
                input$indicador_p == "PORCENTAJE DE HOGARES EN SITUACIÓN DE POBREZA (% DEL TOTAL DE HOGARES) (INE)" |                                          
                input$indicador_p == "PORCENTAJE DE HOGARES CON JEFATURA MASCULINA EN SITUACIÓN DE POBREZA (% DEL TOTAL DE HOGARES) (INE)" |               
                input$indicador_p == "PORCENTAJE DE HOGARES CON JEFATURA MASCULINA EN SITUACIÓN DE POBREZA (% DEL TOTAL DE HOGARES) (INE)" |                   
                input$indicador_p == "PORCENTAJE DE HOGARES CON JEFATURA FEMENINA EN SITUACIÓN DE POBREZA (% DEL TOTAL DE HOGARES) (INE)" |                    
                input$indicador_p == "PORCENTAJE DE HOGARES CON JEFATURA FEMENINA EN SITUACIÓN DE POBREZA (% DEL TOTAL DE HOGARES) (INE)" |                    
                input$indicador_p == "PORCENTAJE DE HOGARES EN SITUACIÓN DE HACINAMIENTO (% DEL TOTAL DE HOGARES) (INE)" |                                     
                input$indicador_p == "PORCENTAJE DE HOGARES EN SITUACIÓN DE HACINAMIENTO (% DEL TOTAL DE HOGARES) (INE)" |                                     
                input$indicador_p == "PORCENTAJE DE HOGARES CON PAREDES O TECHOS DE DESECHO O PISO DE TIERRA (% DEL TOTAL DE HOGARES)" |                       
                input$indicador_p == "PORCENTAJE DE HOGARES CON PAREDES O TECHOS DE DESECHO O PISO DE TIERRA (% DEL TOTAL DE HOGARES)" |                       
                input$indicador_p == "PORCENTAJE DE HOGARES SIN AGUA POTABLE (% DEL TOTAL DE HOGARES)" |                                                       
                input$indicador_p == "PORCENTAJE DE HOGARES SIN AGUA POTABLE (% DEL TOTAL DE HOGARES)" |                                                       
                input$indicador_p == "PORCENTAJE DE HOGARES SIN SERVICIO HIGIÉNICO DE CALIDAD (% DEL TOTAL DE HOGARES)" |                                      
                input$indicador_p == "PORCENTAJE DE HOGARES SIN SERVICIO HIGIÉNICO DE CALIDAD (% DEL TOTAL DE HOGARES)" |                                      
                input$indicador_p == "PORCENTAJE DE HOGARES SIN ARTEFACTOS BÁSICOS DE CONFORT (% DEL TOTAL DE HOGARES)" |                                      
                input$indicador_p == "PORCENTAJE DE HOGARES SIN ARTEFACTOS BÁSICOS DE CONFORT (% DEL TOTAL DE HOGARES)" |                                      
                input$indicador_p == "PORCENTAJE DE HOGARES QUE TIENEN PERSONAS DE 4 A 17 AÑOS QUE NO ASISTEN A CENTROS EDUCATIVOS (% DEL TOTAL DE HOGARES)" | 
                input$indicador_p == "PORCENTAJE DE HOGARES QUE TIENEN PERSONAS DE 4 A 17 AÑOS QUE NO ASISTEN A CENTROS EDUCATIVOS (% DEL TOTAL DE HOGARES)" | 
                input$indicador_p == "PROMEDIO DE INGRESO PER-CÁPITA DE LOS HOGARES (CTE. BASE DICIEMBRE 2006)" |                                             
                input$indicador_p == "PROMEDIO DE INGRESO PER-CÁPITA DE LOS HOGARES (CTE. BASE DICIEMBRE 2006)"){
        
        write.csv(pobreza() %>% 
                    transmute("Año" = anio, 
                              "Condición migratoria hogar" = statusmig_hogar,
                              "Valor (%)" = round(valor*100, 2),
                              "Valor mínimo - IC 95 %" = round(int_inf*100, 2),
                              "Valor máximo - IC 95 %" = round(int_sup*100, 2)), file, row.names = FALSE, fileEncoding = "latin1")
        
      } 
      
    }
  )
  
  
  
  
  
  #PRUEBA
  output$anio_censo <- renderUI({
    
    selectInput(
      inputId = "anio_censo_general",
      label = "Seleccione año:",
      choices = c("1963","1975","1985","1996","2011","Evolución (1963-2011)"),
      selected = "Evolución (1963-2011)")
    
  })
  
  
  
  #PRUEBA
  censo <- reactive({
    
    indicadores_c <- indicadores %>%
      filter(fuente == "CENSO")%>%
      mutate(lugarnac = factor(lugarnac, levels = c("Nativos","Nacidos en el exterior","Desconocido","Total")))%>%
      filter(nomindicador == input$indicador_c)%>%
      filter(anio == input$anio_censo_general)
    
  })
  
  
  censo_evolucion <- reactive({
    
    indicadores_c <- indicadores %>%
      filter(fuente == "CENSO")%>%
      mutate(lugarnac = factor(lugarnac, levels = c("Nativos","Nacidos en el exterior","Desconocido","Total")))%>%
      filter(nomindicador == input$indicador_c)
    
    
  })
  
  
  censo2 <- reactive({
    
    indicadores_c <- indicadores %>%
      filter(fuente == "CENSO")%>%
      mutate(lugarnac = factor(lugarnac, levels = c("Nativos","Nacidos en el exterior","Desconocido","Total")))%>%
      filter(nomindicador == input$indicador_c)%>%
      filter(anio == input$anio_censo_general)
    
  })
  
  
  censo_mapa <- reactive({
    
    
    indicadores_mapa <- indicadores %>%
      filter(nomindicador == "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN SEGÚN LUGAR DE NACIMIENTO Y DEPARTAMENTO  (1963-2011)")
    
    indicadores_c = depto %>%
      left_join(indicadores_mapa,by = "nombre")%>%
      mutate(lugarnac = factor(lugarnac, levels = c("Nativos","Nacidos en el exterior","Desconocido","Total")))%>%
      filter(anio == input$anio_censo_general)
    
    
  })
  
  
  
  censo_mapa2 <- reactive({
    
    
    indicadores_mapa <- indicadores %>%
      filter(nomindicador == "PERSONAS SEGÚN LUGAR DE NACIMIENTO POR DEPARTAMENTO Y AÑO (1963-2011)")
    
    indicadores_c = depto %>%
      left_join(indicadores_mapa,by = "nombre")%>%
      mutate(lugarnac = factor(lugarnac, levels = c("Nativos","Nacidos en el exterior","Desconocido","Total")))%>%
      filter(anio == input$anio_censo_general)
    
    
  })
  
  
  output$title_censo <- renderUI({ 
    helpText(HTML(input$indicador_c))
  })  
  
  
  output$barPlot_c <- renderPlot({
    
    if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN SEGÚN LUGAR DE NACIMIENTO (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))) {
      
      
      censo <- ggplot(data = censo(), aes(fill=lugarnac, y=valor, x=anio)) +
        geom_bar(position="fill", stat="identity",width=0.5)  +
        scale_fill_brewer(palette="Blues")+
        scale_y_continuous(labels=scales::percent) +
        #guides(fill=guide_legend(nrow=2)) +
        theme(axis.text.x = element_text(size=13,angle = 90),
              axis.text.y = element_text(size=10),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))+
        labs(x = "",
             y = "")
      
    }
    else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN SEGÚN LUGAR DE NACIMIENTO (1963-2011)"  & (input$anio_censo_general =="Evolución (1963-2011)")) {
      
      censo <- ggplot(data = censo_evolucion(), aes(fill=lugarnac, y=valor, x=anio)) +
        geom_bar(position="fill", stat="identity",width=0.5)  +
        scale_fill_brewer(palette="Blues")+
        scale_y_continuous(labels=scales::percent) +
        #guides(fill=guide_legend(nrow=3)) +
        theme(axis.text.x = element_text(size=13,angle = 90),
              axis.text.y = element_text(size=10),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))+
        labs(x = "",
             y = "")
      
    }
    else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN SEGÚN LUGAR DE NACIMIENTO Y GRANDES GRUPOS DE EDAD (0-17, 18 Y MÁS, TOTAL) (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))) {
      
      censo <- ggplot(data = censo(), aes(fill=lugarnac, y=valor, x=grupoedad)) + 
        geom_bar(position="fill", stat="identity",width=0.5)  +
        scale_fill_brewer(palette="Blues")+
        scale_y_continuous(labels=scales::percent) +
        #guides(fill=guide_legend(nrow=2)) +
        theme(axis.text.x = element_text(size=10,angle = 90,hjust=0.95,vjust=0.2),
              axis.text.y = element_text(size=10),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))+
        labs(x = "",
             y = "")
      
    }
    else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN SEGÚN LUGAR DE NACIMIENTO Y GRANDES GRUPOS DE EDAD (0-17, 18 Y MÁS, TOTAL) (1963-2011)"  & (input$anio_censo_general =="Evolución (1963-2011)")) {
      
      censo <- ggplot(data = censo_evolucion(), aes(fill=lugarnac, y=valor, x=anio)) +
        geom_bar(position="fill", stat="identity",width=0.5)  +
        facet_wrap(~ grupoedad)+
        scale_fill_brewer(palette="Blues")+
        scale_y_continuous(labels=scales::percent) +
        #guides(fill=guide_legend(nrow=2)) +
        theme(axis.text.x = element_text(size=10,angle = 90,hjust=0.95,vjust=0.2),
              axis.text.y = element_text(size=10),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))+
        labs(x = "",
             y = "")
      
    }
    else if(input$indicador_c ==  "PORCENTAJE DE MUJERES SEGÚN LUGAR DE NACIMIENTO (1963 - 2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))) {
      
      censo = ggplot(data = censo(), aes(x=lugarnac,y=valor)) +
        geom_bar(position="dodge", stat="identity",width=0.3,fill="#7bb7dd")  +
        scale_y_continuous(limits=c(0,0.7),labels=scales::percent) +
        theme(axis.text.x = element_text(size=13,angle = 90),
              plot.title = element_text(vjust=1.5,size = 13))+
        labs(x = "",
             y = "")
      
    }
    else if(input$indicador_c ==  "PORCENTAJE DE MUJERES SEGÚN LUGAR DE NACIMIENTO (1963 - 2011)"  & (input$anio_censo_general =="Evolución (1963-2011)")) {
      
      censo <- ggplot(data = censo_evolucion(), aes(x=anio,y=valor,fill=lugarnac,width = 0.6)) +
        geom_bar(position=position_dodge(width = 0.8), stat="identity")  +
        scale_fill_brewer(palette="Blues")+
        scale_y_continuous(limits=c(0,0.7),labels=scales::percent) +
        #guides(fill=guide_legend(nrow=2)) +
        theme(axis.text.x = element_text(size=13,angle = 90),
              axis.text.y = element_text(size=10),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))+
        labs(x = "",
             y = "")
      
    }
    else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA POR NIVEL EDUCATIVO ALCANZADO SEGÚN LUGAR DE NACIMIENTO PARA LAS PERSONAS DE 25 AÑOS Y MÁS (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))) {
      
      censo <- ggplot(data = censo(), aes(fill=niveledu, y=valor, x=lugarnac)) +
        geom_bar(position="fill", stat="identity",width=0.4)  +
        scale_fill_brewer(palette="Blues")+
        scale_y_continuous(labels=scales::percent) +
        #guides(fill=guide_legend(nrow=2)) +
        theme(axis.text.x = element_text(size=10,angle = 90,hjust=0.95,vjust=0.2),
              axis.text.y = element_text(size=10),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))+
        labs(x = "",
             y = "")
      
    }
    else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA POR NIVEL EDUCATIVO ALCANZADO SEGÚN LUGAR DE NACIMIENTO PARA LAS PERSONAS DE 25 AÑOS Y MÁS (1963-2011)"  & (input$anio_censo_general == "Evolución (1963-2011)")) {
      
      censo <- ggplot(data = censo_evolucion(), aes(fill=niveledu, y=valor, x=anio)) +
        geom_bar(position="fill", stat="identity",width=0.5)  +
        facet_wrap(~ lugarnac)+
        scale_fill_brewer(palette="Blues")+
        scale_y_continuous(labels=scales::percent) +
        #guides(fill=guide_legend(nrow=2)) +
        theme(axis.text.x = element_text(size=10,angle = 90,hjust=0.95,vjust=0.2),
              axis.text.y = element_text(size=10),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))+
        labs(x = "",
             y = "")
      
      
      
      
    }
    else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN NACIDA EN EL EXTERIOR POR LUGAR DE NACIMIENTO (ARGENTINA, BRASIL, RESTO DE LATINOAMÉRICA Y CARIBE Y RESTO DEL MUNDO) (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","2011"))) {
      
      
      
      censo <- ggplot(data = censo2(), aes(fill=paisnac, y=valor, x=anio)) +
        geom_bar(position="fill", stat="identity",width=0.4)  +
        scale_fill_brewer(palette="Blues")+
        scale_y_continuous(labels=scales::percent) +
        #guides(fill=guide_legend(nrow=2)) +
        theme(axis.text.x = element_text(size=13,angle = 90),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))+
        labs(x = "",
             y = "")
      
      
    }  else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN NACIDA EN EL EXTERIOR POR LUGAR DE NACIMIENTO (ARGENTINA, BRASIL, RESTO DE LATINOAMÉRICA Y CARIBE Y RESTO DEL MUNDO) (1963-2011)"  & (input$anio_censo_general == "Evolución (1963-2011)")) {
      
      
      censo <- ggplot(data = censo_evolucion(), aes(fill=paisnac, y=valor, x=anio)) +
        geom_bar(position="fill", stat="identity",width=0.4)  +
        #facet_wrap(facet_wrap(~ anio, ncol = 2, scales = "free_y"))+
        scale_fill_brewer(palette="Blues")+
        scale_y_continuous(labels=scales::percent) +
        #guides(fill=guide_legend(nrow=2)) +
        theme(axis.text.x = element_text(size=13,angle = 90),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))+
        labs(x = "",
             y = "")
      
    } else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN SEGÚN LUGAR DE NACIMIENTO Y DEPARTAMENTO  (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))){
      
      
      censo <- ggplot(censo_mapa(),color="#8098c2") + geom_sf() +
        #scale_fill_gradient(low = "#9ECAE1", high = "#08306B")+
        geom_sf_text(aes(label = round(valor*100,1)), colour = "black",size=4,fontface = "bold")+
        facet_wrap(~lugarnac,ncol=2)+
        labs(x = "",
             y = "",
             caption = "Mapas desarrollados con el paquete **geouy** - _github.com/RichDeto/geouy_")+
        theme(
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.text = element_blank(),
          legend.title = element_blank(),
          panel.grid.major = element_line(colour = "transparent"),
          plot.caption = element_markdown(size = 10,hjust = 0))
      
    } 
    else if(input$indicador_c ==  "PERSONAS SEGÚN CONDICIÓN MIGRATORIA (NATIVOS, INMIGRANTES ANTIGUOS Y RECIENTES) (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))) {
      
      
      
      censo <-  ggplot(data = censo(),aes(lugarnac, valor)) +
        geom_bar(stat = "identity", position = "dodge",fill="#7bb7dd", width=0.5)+
        scale_y_continuous(limits = c(0, NA),
                           labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
        labs(x = "",
             y = "")+
        
        theme(legend.position = "none",
              axis.text.x = element_text(size=13,angle = 90),
              plot.title = element_text(vjust=1.5,size = 13))
      
      
      
    } else if(input$indicador_c ==  "PERSONAS SEGÚN CONDICIÓN MIGRATORIA (NATIVOS, INMIGRANTES ANTIGUOS Y RECIENTES) (1963-2011)"  & (input$anio_censo_general =="Evolución (1963-2011)")) {
      
      
      censo <- ggplot(data = censo_evolucion(),aes(anio, valor, color = lugarnac, fill = lugarnac, group = lugarnac)) +
        geom_line(size = 1.1) +
        scale_y_continuous(limits = c(0, NA),
                           labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
        labs(x = "",
             y = "")+
        
        theme(axis.text.x = element_text(size=13,angle = 90),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))
      
      
      
    } else if(input$indicador_c ==  "PERSONAS SEGÚN LUGAR DE NACIMIENTO Y GRANDES GRUPOS DE EDAD (0-17, 18 Y MÁS, TOTAL) (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))) {
      
      
      censo <-   ggplot(data = censo(),aes(lugarnac, valor)) +
        
        geom_bar(stat = "identity", position = "dodge",fill="#7bb7dd", width=0.5)+
        scale_y_continuous(limits = c(0, NA),
                           labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
        facet_wrap(~ grupoedad) +
        theme(axis.text.x = element_text(size=13,angle = 90),
              plot.title = element_text(vjust=1.5,size = 13))+
        labs(x = "",
             y = "")
      
      
      
    } else if(input$indicador_c ==  "PERSONAS SEGÚN LUGAR DE NACIMIENTO Y GRANDES GRUPOS DE EDAD (0-17, 18 Y MÁS, TOTAL) (1963-2011)" & (input$anio_censo_general =="Evolución (1963-2011)")) {
      
      
      censo <-  ggplot(data = censo_evolucion(),aes(anio, valor, color = lugarnac, fill = lugarnac, group = lugarnac)) +
        geom_line(size = 1.1) +
        scale_y_continuous(limits = c(0, NA),
                           labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
        facet_wrap(~ grupoedad) +
        theme(axis.text.x = element_text(size=13,angle = 90),
              legend.title = element_blank(),legend.position = "bottom",
              legend.justification = "left",legend.direction = "vertical",
              legend.key.size = unit(0.5,"cm"))+
        labs(x = "",
             y = "")
      
      
      
      
    }else if(input$indicador_c ==  "PERSONAS SEGÚN LUGAR DE NACIMIENTO POR DEPARTAMENTO Y AÑO (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))) {
      
      
      censo <- ggplot(censo_mapa2(),color="#8098c2") + geom_sf() +
        #scale_fill_gradient(low = "#9ECAE1", high = "#08306B")+
        geom_sf_text(aes(label = valor), colour = "black",size=4,fontface = "bold")+
        facet_wrap(~lugarnac,ncol=2)+
        labs(x = "",
             y = "",
             caption = "Mapas desarrollados con el paquete **geouy** - _github.com/RichDeto/geouy_")+
        theme(
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.text = element_blank(),
          legend.title = element_blank(),
          panel.grid.major = element_line(colour = "transparent"),
          plot.caption = element_markdown(size = 10,hjust = 0))
      
      
      
    } 
    
    print(censo)
    ggsave("www/censo.png")
    
  })
  
  
  
  
  
  
  
  
  
  
  output$baja_barPlot_c <- downloadHandler(
    filename <- function() {
      paste("censo", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/censo.png", file)
    },
    contentType = "www/censo"
    
    
  )
  
  
  output$tabla_resultado_c  <- renderDT({
    
    if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN SEGÚN LUGAR DE NACIMIENTO (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))){
      
      datatable(censo() %>%
                  arrange(lugarnac) %>%
                  transmute("Año" = anio,
                            "Condición migratoria" = lugarnac,
                            "Valor (%)" = round(valor*100, 2)),
                rownames = FALSE, options = list(dom = 'ltipr'))
      
    } else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN SEGÚN LUGAR DE NACIMIENTO (1963-2011)"  & (input$anio_censo_general =="Evolución (1963-2011)")){
      
      datatable(censo_evolucion() %>%
                  arrange(lugarnac) %>%
                  transmute("Año" = anio,
                            "Condición migratoria" = lugarnac,
                            "Valor (%)" = round(valor*100, 2)),
                rownames = FALSE, options = list(dom = 'ltipr'))
      
    }   else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN SEGÚN LUGAR DE NACIMIENTO Y GRANDES GRUPOS DE EDAD (0-17, 18 Y MÁS, TOTAL) (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))){
      
      datatable(censo() %>%
                  arrange(lugarnac) %>%
                  transmute("Año" = anio,
                            "Lugar de nacimiento" = lugarnac,
                            "Grupo de edad" = grupoedad,
                            "Valor (%)" = round(valor*100, 2)),
                rownames = FALSE, options = list(dom = 'ltipr'))
      
    } else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN SEGÚN LUGAR DE NACIMIENTO Y GRANDES GRUPOS DE EDAD (0-17, 18 Y MÁS, TOTAL) (1963-2011)"  & (input$anio_censo_general =="Evolución (1963-2011)")){
      
      datatable(censo_evolucion() %>%
                  arrange(lugarnac) %>%
                  transmute("Año" = anio,
                            "Lugar de nacimiento" = lugarnac,
                            "Grupo de edad" = grupoedad,
                            "Valor (%)" = round(valor*100, 2)),
                rownames = FALSE, options = list(dom = 'ltipr'))
      
    } else if(input$indicador_c ==  "PORCENTAJE DE MUJERES SEGÚN LUGAR DE NACIMIENTO (1963 - 2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))){
      
      datatable(censo() %>%
                  arrange(lugarnac) %>%
                  transmute("Año" = anio,
                            "Lugar de nacimiento" = lugarnac,
                            "Valor (%)" = round(valor*100, 2)),
                rownames = FALSE, options = list(dom = 'ltipr'))
      
    } else if(input$indicador_c ==  "PORCENTAJE DE MUJERES SEGÚN LUGAR DE NACIMIENTO (1963 - 2011)"  & (input$anio_censo_general =="Evolución (1963-2011)")){
      
      datatable(censo_evolucion() %>%
                  arrange(lugarnac) %>%
                  transmute("Año" = anio,
                            "Lugar de nacimiento" = lugarnac,
                            "Valor (%)" = round(valor*100, 2)),
                rownames = FALSE, options = list(dom = 'ltipr'))
      
    }else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN NACIDA EN EL EXTERIOR POR LUGAR DE NACIMIENTO (ARGENTINA, BRASIL, RESTO DE LATINOAMÉRICA Y CARIBE Y RESTO DEL MUNDO) (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","2011"))){
      
      datatable(censo2() %>%
                  transmute("Año" = anio,
                            "País de nacimiento" = paisnac,
                            "Valor (%)" = round(valor*100, 2)),
                rownames = FALSE, options = list(dom = 'ltipr'))
      
    } else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN NACIDA EN EL EXTERIOR POR LUGAR DE NACIMIENTO (ARGENTINA, BRASIL, RESTO DE LATINOAMÉRICA Y CARIBE Y RESTO DEL MUNDO) (1963-2011)"  & (input$anio_censo_general =="Evolución (1963-2011)")){
      
      datatable(censo_evolucion() %>%
                  transmute("Año" = anio,
                            "País de nacimiento" = paisnac,
                            "Valor (%)" = round(valor*100, 2)),
                rownames = FALSE, options = list(dom = 'ltipr'))
      
    }else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA POR NIVEL EDUCATIVO ALCANZADO SEGÚN LUGAR DE NACIMIENTO PARA LAS PERSONAS DE 25 AÑOS Y MÁS (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))){
      
      datatable(censo() %>%
                  arrange(lugarnac) %>%
                  transmute("Año" = anio,
                            "Lugar de nacimiento" = lugarnac,
                            "Nivel educativo" = niveledu,
                            "Valor (%)" = round(valor*100, 2)),
                rownames = FALSE, options = list(dom = 'ltipr'))
      
    } else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA POR NIVEL EDUCATIVO ALCANZADO SEGÚN LUGAR DE NACIMIENTO PARA LAS PERSONAS DE 25 AÑOS Y MÁS (1963-2011)"  & (input$anio_censo_general =="Evolución (1963-2011)")){
      
      datatable(censo_evolucion() %>%
                  arrange(lugarnac) %>%
                  transmute("Año" = anio,
                            "Lugar de nacimiento" = lugarnac,
                            "Nivel educativo" = niveledu,
                            "Valor (%)" = round(valor*100, 2)),
                rownames = FALSE, options = list(dom = 'ltipr'))
      
    } else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN SEGÚN LUGAR DE NACIMIENTO Y DEPARTAMENTO  (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))){
      
      datatable(censo_mapa() %>%
                  as.data.frame() %>%
                  arrange(lugarnac) %>%
                  transmute("Año" = anio,
                            "Lugar de nacimiento" = lugarnac,
                            "Departamento" = depto.y,
                            "Valor (%)" = round(valor*100, 2)),
                rownames = FALSE, options = list(dom = 'ltipr'))
      
    }else if(input$indicador_c ==  "PERSONAS SEGÚN CONDICIÓN MIGRATORIA (NATIVOS, INMIGRANTES ANTIGUOS Y RECIENTES) (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))) {
      
      
      datatable(censo() %>%
                  as.data.frame() %>%
                  arrange(lugarnac) %>%
                  transmute("Año" = anio,
                            "Lugar de nacimiento" = lugarnac,
                            "Valor" = valor),
                rownames = FALSE, options = list(dom = 'ltipr'))
      
      
      
    } else if(input$indicador_c ==  "PERSONAS SEGÚN CONDICIÓN MIGRATORIA (NATIVOS, INMIGRANTES ANTIGUOS Y RECIENTES) (1963-2011)"  & (input$anio_censo_general =="Evolución (1963-2011)")) {
      
      datatable(censo_evolucion() %>%
                  as.data.frame() %>%
                  arrange(lugarnac) %>%
                  transmute("Año" = anio,
                            "Lugar de nacimiento" = lugarnac,
                            "Valor" = valor),
                rownames = FALSE, options = list(dom = 'ltipr'))
      
      
      
    } else if(input$indicador_c ==  "PERSONAS SEGÚN LUGAR DE NACIMIENTO Y GRANDES GRUPOS DE EDAD (0-17, 18 Y MÁS, TOTAL) (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))) {
      
      
      datatable(censo() %>%
                  as.data.frame() %>%
                  arrange(lugarnac) %>%
                  transmute("Año" = anio,
                            "Lugar de nacimiento" = lugarnac,
                            "Grupo de edad" = grupoedad,
                            "Valor" = valor),
                rownames = FALSE, options = list(dom = 'ltipr'))
      
      
      
    } else if(input$indicador_c ==  "PERSONAS SEGÚN LUGAR DE NACIMIENTO Y GRANDES GRUPOS DE EDAD (0-17, 18 Y MÁS, TOTAL) (1963-2011)" & (input$anio_censo_general =="Evolución (1963-2011)")) {
      
      
      datatable(censo_evolucion() %>%
                  as.data.frame() %>%
                  arrange(lugarnac) %>%
                  transmute("Año" = anio,
                            "Lugar de nacimiento" = lugarnac,
                            "Grupo de edad" = grupoedad,
                            "Valor" = valor),
                rownames = FALSE, options = list(dom = 'ltipr'))
      
      
      
      
    } else if(input$indicador_c ==  "PERSONAS SEGÚN LUGAR DE NACIMIENTO POR DEPARTAMENTO Y AÑO (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))) {
      
      
      datatable(censo_mapa2() %>%
                  as.data.frame() %>%
                  arrange(lugarnac) %>%
                  transmute("Año" = anio,
                            "Lugar de nacimiento" = lugarnac,
                            "Departamento" = depto.y,
                            "Valor" = valor),
                rownames = FALSE, options = list(dom = 'ltipr'))
      
      
      
    } 
    
    
  })
  
  
  output$dl_tabla_resultado_c <- downloadHandler(
    
    
    filename = function() {
      paste0("resultados- ",input$indicador_c, ".csv", sep = "")
    },
    content = function(file) {
      
      
      if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN SEGÚN LUGAR DE NACIMIENTO (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))){
        
        write.csv(censo() %>%
                    arrange(lugarnac) %>%
                    transmute("Año" = anio,
                              "Condición migratoria" = lugarnac,
                              "Valor (%)" = round(valor*100, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
        
      } else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN SEGÚN LUGAR DE NACIMIENTO (1963-2011)"  & (input$anio_censo_general =="Evolución (1963-2011)")){
        
        write.csv(censo_evolucion() %>%
                    arrange(lugarnac) %>%
                    transmute("Año" = anio,
                              "Condición migratoria" = lugarnac,
                              "Valor (%)" = round(valor*100, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
      }  else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN SEGÚN LUGAR DE NACIMIENTO Y GRANDES GRUPOS DE EDAD (0-17, 18 Y MÁS, TOTAL) (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))){
        
        write.csv(censo() %>%
                    arrange(lugarnac) %>%
                    transmute("Año" = anio,
                              "Lugar de nacimiento" = lugarnac,
                              "Grupo de edad" = grupoedad,
                              "Valor (%)" = round(valor*100, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
      } else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN SEGÚN LUGAR DE NACIMIENTO Y GRANDES GRUPOS DE EDAD (0-17, 18 Y MÁS, TOTAL) (1963-2011)"  & (input$anio_censo_general =="Evolución (1963-2011)")){
        
        write.csv(censo_evolucion() %>%
                    arrange(lugarnac) %>%
                    transmute("Año" = anio,
                              "Lugar de nacimiento" = lugarnac,
                              "Grupo de edad" = grupoedad,
                              "Valor (%)" = round(valor*100, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
      } else if(input$indicador_c ==  "PORCENTAJE DE MUJERES SEGÚN LUGAR DE NACIMIENTO (1963 - 2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))){
        
        write.csv(censo() %>%
                    arrange(lugarnac) %>%
                    transmute("Año" = anio,
                              "Lugar de nacimiento" = lugarnac,
                              "Valor (%)" = round(valor*100, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
      } else if(input$indicador_c ==  "PORCENTAJE DE MUJERES SEGÚN LUGAR DE NACIMIENTO (1963 - 2011)"  & (input$anio_censo_general =="Evolución (1963-2011)")){
        
        write.csv(censo_evolucion() %>%
                    arrange(lugarnac) %>%
                    transmute("Año" = anio,
                              "Lugar de nacimiento" = lugarnac,
                              "Valor (%)" = round(valor*100, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
      }else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN NACIDA EN EL EXTERIOR POR LUGAR DE NACIMIENTO (ARGENTINA, BRASIL, RESTO DE LATINOAMÉRICA Y CARIBE Y RESTO DEL MUNDO) (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","2011"))){
        
        write.csv(censo2() %>%
                    transmute("Año" = anio,
                              "País de nacimiento" = paisnac,
                              "Valor (%)" = round(valor*100, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
      } else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN NACIDA EN EL EXTERIOR POR LUGAR DE NACIMIENTO (ARGENTINA, BRASIL, RESTO DE LATINOAMÉRICA Y CARIBE Y RESTO DEL MUNDO) (1963-2011)"  & (input$anio_censo_general =="Evolución (1963-2011)")){
        
        write.csv(censo_evolucion() %>%
                    transmute("Año" = anio,
                              "País de nacimiento" = paisnac,
                              "Valor (%)" = round(valor*100, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
      }else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA POR NIVEL EDUCATIVO ALCANZADO SEGÚN LUGAR DE NACIMIENTO PARA LAS PERSONAS DE 25 AÑOS Y MÁS (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))){
        
        write.csv(censo() %>%
                    arrange(lugarnac) %>%
                    transmute("Año" = anio,
                              "Lugar de nacimiento" = lugarnac,
                              "Nivel educativo" = niveledu,
                              "Valor (%)" = round(valor*100, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
      }else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA POR NIVEL EDUCATIVO ALCANZADO SEGÚN LUGAR DE NACIMIENTO PARA LAS PERSONAS DE 25 AÑOS Y MÁS (1963-2011)"  & (input$anio_censo_general =="Evolución (1963-2011)")){
        
        write.csv(censo_evolucion() %>%
                    arrange(lugarnac) %>%
                    transmute("Año" = anio,
                              "Lugar de nacimiento" = lugarnac,
                              "Nivel educativo" = niveledu,
                              "Valor (%)" = round(valor*100, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
      }
      else if(input$indicador_c ==  "DISTRIBUCIÓN RELATIVA DE LA POBLACIÓN SEGÚN LUGAR DE NACIMIENTO Y DEPARTAMENTO  (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))){
        
        write.csv(censo_mapa() %>%
                    as.data.frame()%>%
                    arrange(lugarnac) %>%
                    transmute("Año" = anio,
                              "Lugar de nacimiento" = lugarnac,
                              "Departamento" = depto,
                              "Valor (%)" = round(valor*100, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
      }else if(input$indicador_c ==  "PERSONAS SEGÚN CONDICIÓN MIGRATORIA (NATIVOS, INMIGRANTES ANTIGUOS Y RECIENTES) (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))) {
        
        
        write.csv(censo() %>%
                    as.data.frame() %>%
                    arrange(lugarnac) %>%
                    transmute("Año" = anio,
                              "Lugar de nacimiento" = lugarnac,
                              "Valor" = valor),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
        
        
      } else if(input$indicador_c ==  "PERSONAS SEGÚN CONDICIÓN MIGRATORIA (NATIVOS, INMIGRANTES ANTIGUOS Y RECIENTES) (1963-2011)"  & (input$anio_censo_general =="Evolución (1963-2011)")) {
        
        write.csv(censo_evolucion() %>%
                    as.data.frame() %>%
                    arrange(lugarnac) %>%
                    transmute("Año" = anio,
                              "Lugar de nacimiento" = lugarnac,
                              "Valor" = valor),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
        
        
      } else if(input$indicador_c ==  "PERSONAS SEGÚN LUGAR DE NACIMIENTO Y GRANDES GRUPOS DE EDAD (0-17, 18 Y MÁS, TOTAL) (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))) {
        
        
        write.csv(censo() %>%
                    as.data.frame() %>%
                    arrange(lugarnac) %>%
                    transmute("Año" = anio,
                              "Lugar de nacimiento" = lugarnac,
                              "Grupo de edad" = grupoedad,
                              "Valor" = valor),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
        
        
      } else if(input$indicador_c ==  "PERSONAS SEGÚN LUGAR DE NACIMIENTO Y GRANDES GRUPOS DE EDAD (0-17, 18 Y MÁS, TOTAL) (1963-2011)" & (input$anio_censo_general =="Evolución (1963-2011)")) {
        
        
        write.csv(censo_evolucion() %>%
                    as.data.frame() %>%
                    arrange(lugarnac) %>%
                    transmute("Año" = anio,
                              "Lugar de nacimiento" = lugarnac,
                              "Grupo de edad" = grupoedad,
                              "Valor" = valor),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
        
        
        
      }else if(input$indicador_c ==  "PERSONAS SEGÚN LUGAR DE NACIMIENTO POR DEPARTAMENTO Y AÑO (1963-2011)"  & (input$anio_censo_general %in% c("1963","1975","1985","1996","2011"))) {
        
        
        write.csv(censo_mapa2() %>%
                    as.data.frame() %>%
                    arrange(lugarnac) %>%
                    transmute("Año" = anio,
                              "Lugar de nacimiento" = lugarnac,
                              "Departamento" = depto.y,
                              "Valor" = valor),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
        
        
      } 
      
      
    })
  
  
  
  
  enir_ps <- reactive({
    
    enir_ps <- indicadores %>%
      filter(fuente == "ENIR") %>%
      mutate(sexo = factor(sexo, levels = c("VARONES","MUJERES","TOTAL")))%>%
      filter(nomindicador == input$indicador_enir_ps)%>%
      filter(comunidad == input$comunidad_ps)
    
    
  })
  
  
  enir_ps_todo <- reactive({
    
    enir_ps_todo <- indicadores %>%
      filter(fuente == "ENIR") %>%
      mutate(sexo = factor(sexo, levels = c("VARONES","MUJERES","TOTAL")))%>%
      filter(nomindicador == input$indicador_enir_ps)
    
  })
  
  
  output$title_enir_ps <- renderUI({ 
    helpText(HTML(paste(input$indicador_enir_ps,"(",input$comunidad_ps,")")))
  })  
  
  
  output$barPlot_enir_ps <- renderPlot({
    
    if(input$indicador_enir_ps ==  "PIRÁMIDE POBLACIONAL DE LOS INMIGRANTES QUE RESIDEN EN MONTEVIDEO SEGÚN COMUNIDAD DE ORIGEN"
       & (input$comunidad_ps %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
      
      
      enir <- ggplot(enir_ps(),aes(x = tramos, fill = sexo)) +
        geom_col(data = filter(enir_ps(), 
                               sexo == "VARONES"), 
                 aes(y = round(-1*valor*100,2))) +
        geom_col(data = filter(enir_ps(), 
                               sexo == "MUJERES"), 
                 aes(y = round(valor*100,2))) +  
        geom_text(data = filter(enir_ps(), 
                                sexo == "VARONES"), 
                  aes(y = round(-1*valor*100,2), label = scales::percent(valor,accuracy = 0.1)), 
                  hjust = "outward",size=3) +
        geom_text(data = filter(enir_ps(), 
                                sexo == "MUJERES"), 
                  aes(y = round(valor*100,2), label = scales::percent(valor,accuracy = 0.1)), 
                  hjust = "outward",size=3) +
        expand_limits(y = c(-50, 50)) +
        scale_y_continuous(breaks = seq(-50, 50, by = 10),
                           labels = abs) + 
        scale_fill_manual(name = "Sexo",
                          values = c("VARONES" = "#C6DBEF",
                                     "MUJERES" = "#2171B5")) +
        coord_flip() +
        theme(legend.title = element_blank(),
              legend.position = "bottom",
              axis.text = element_text(size=10),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              strip.text.x = element_text(size = 8),
              plot.title = element_text(size=13),
              plot.caption = element_text(size=9,face = "italic",hjust = 0)
              
        )+labs(
          x = "",
          y = "",
          #      title = paste(
          #        "PIRÁMIDE POBLACIONAL DE LOS INMIGRANTES QUE RESIDEN EN URUGUAY \nSEGÚN COMUNIDAD DE ORIGEN",
          #        " (",
          #        input$comunidad_ps,
          #        ")", sep = ""
          #      ),
          caption = "(*)Cuba: N=227 / República Dominicana: N=312\n(**)Cuba: los tramos de edad 0-17 y 50-64 para \nvarones y mujeres tienen un N<20.\nRepública Dominicana: el tramo de edad de \n50-64 para varones y mujeres tienen un N<20.\nPerú: los tramos de edad 0-17 y 65 y más para \nvarones y mujeres y 50-64 para varones tienen \nun N<20.\nVenezuela: el tramo de edad de 65 y más para \nvarones y mujeres, y 50-64 en varones \ntienen un N<20.")
      
      
      
      
    } else if(input$indicador_enir_ps ==  "PIRÁMIDE POBLACIONAL DE LOS INMIGRANTES QUE RESIDEN EN MONTEVIDEO SEGÚN COMUNIDAD DE ORIGEN"
              & (input$comunidad_ps =="TODAS")){
      
      enir <- ggplot(enir_ps_todo(),aes(x = tramos, fill = sexo)) +
        geom_col(data = filter(enir_ps_todo(), 
                               sexo == "VARONES"), 
                 aes(y = round(-1*valor*100,2))) +
        geom_col(data = filter(enir_ps_todo(), 
                               sexo == "MUJERES"), 
                 aes(y = round(valor*100,2))) +  
        geom_text(data = filter(enir_ps_todo(), 
                                sexo == "VARONES"), 
                  aes(y = round(-1*valor*100,2), label = scales::percent(valor,accuracy = 0.1)), 
                  hjust = "outward",size=3) +
        geom_text(data = filter(enir_ps_todo(), 
                                sexo == "MUJERES"), 
                  aes(y = round(valor*100,2), label = scales::percent(valor,accuracy = 0.1)), 
                  hjust = "outward",size=3) +
        expand_limits(y = c(-50, 50)) +
        scale_y_continuous(breaks = seq(-50, 50, by = 10),
                           labels = abs) + 
        scale_fill_manual(name = "Sexo",
                          values = c("VARONES" = "#C6DBEF",
                                     "MUJERES" = "#2171B5")) +
        coord_flip() +
        facet_wrap(~comunidad)+
        theme(legend.title = element_blank(),
              legend.position = "bottom",
              axis.text = element_text(size=10),
              strip.text.x = element_text(size = 8),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(size=12),
              plot.caption = element_text(size=9,face = "italic",hjust = 0)
        )+
        labs(
          x = "",
          y = "",
          #    title = "PIRÁMIDE POBLACIONAL DE LOS INMIGRANTES QUE RESIDEN EN URUGUAY \nSEGÚN COMUNIDAD DE ORIGEN",
          caption = "(*)Cuba: N=227 / República Dominicana: N=312\n(**)Cuba: los tramos de edad 0-17 y 50-64 para \nvarones y mujeres tienen un N<20.\nRepública Dominicana: el tramo de edad de \n50-64 para varones y mujeres tienen un N<20.\nPerú: los tramos de edad 0-17 y 65 y más para \nvarones y mujeres y 50-64 para varones tienen \nun N<20.\nVenezuela: el tramo de edad de 65 y más para \nvarones y mujeres, y 50-64 en varones \ntienen un N<20.")
      
      
      
    } else if(input$indicador_enir_ps ==  "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 25 AÑOS O MÁS QUE RESIDEN EN MONTEVIDEO POR NIVEL EDUCATIVO SEGÚN COMUNIDAD DE ORIGEN"
              & (input$comunidad_ps %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
      
      
      enir <- ggplot(data = enir_ps(), aes(fill=niveledu, y=valor, x=comunidad)) +
        geom_bar(position="fill", stat="identity",width=0.5)  +
        scale_fill_brewer(palette="Blues")+
        scale_y_continuous(labels=scales::percent) +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(size=9,angle = 90),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",legend.justification = "left",legend.direction = "vertical",legend.key.size = unit(0.5,"cm"),
              legend.text = element_text(size=8),
              plot.caption = element_text(size=10,face = "italic",hjust = 0),
              plot.title = element_text(size=12))+
        labs(
          x = "",
          y = "",
          caption = "(*)Cuba: N=119 / República Dominicana: N=222\n/Perú: N=195 / Venezuela: N=518 \n(**)Cuba: la categoría Menos de secundaria \ncompleta tiene un N<20.\nRepública Dominicana: la categoría \nUniversidad completa tiene un N<20.\nVenezuela: la categoría Menos de secundaria \ncompleta tiene un N<20."
        )
      
      
    } else if(input$indicador_enir_ps ==  "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 25 AÑOS O MÁS QUE RESIDEN EN MONTEVIDEO POR NIVEL EDUCATIVO SEGÚN COMUNIDAD DE ORIGEN"
              & (input$comunidad_ps =="TODAS")){
      
      enir <- ggplot(data = enir_ps_todo(), aes(fill=niveledu, y=valor, x=comunidad)) +
        geom_bar(position="fill", stat="identity",width=0.5)  +
        scale_fill_brewer(palette="Blues")+
        scale_y_continuous(labels=scales::percent) +
        
        theme(legend.title = element_blank(),
              axis.text.x = element_text(size=9,angle = 90),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",legend.justification = "left",legend.direction = "vertical",legend.key.size = unit(0.5,"cm"),
              legend.text = element_text(size=8),
              plot.caption = element_text(size=10,face = "italic",hjust = 0),
              plot.title = element_text(size=12))+
        labs(
          x = "",
          y = "",
          #    title = "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 25 AÑOS O MÁS QUE RESIDEN \nEN URUGUAY POR NIVEL EDUCATIVO SEGÚN COMUNIDAD DE ORIGEN",
          caption = "(*)Cuba: N=119 / República Dominicana: N=222\n/Perú: N=195 / Venezuela: N=518 \n(**)Cuba: la categoría Menos de secundaria \ncompleta tiene un N<20.\nRepública Dominicana: la categoría \nUniversidad completa tiene un N<20.\nVenezuela: la categoría Menos de secundaria \ncompleta tiene un N<20.")
      
    } 
    else if(input$indicador_enir_ps ==  "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 14 AÑOS O MÁS QUE RESIDEN EN MONTEVIDEO POR CONDICIÓN DE ACTIVIDAD AL MOMENTO DE LA ENCUESTA SEGÚN COMUNIDAD DE ORIGEN Y SEXO"
            & (input$comunidad_ps %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
      
      
      enir <- ggplot(data = enir_ps(), aes(fill=catocup, y=valor, x=sexo)) +
        geom_bar(position="fill", stat="identity",width=0.5)  +
        scale_fill_brewer(palette="Blues")+
        scale_y_continuous(labels=scales::percent) +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(size=9,angle = 90),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",legend.justification = "left",legend.direction = "vertical",legend.key.size = unit(0.5,"cm"),
              legend.text = element_text(size=8),
              plot.caption = element_text(size=9,face = "italic",hjust = 0),
              plot.title = element_text(size=12))+
        labs(
          x = "",
          y = "",
          #    title = paste(
          #      "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 14 AÑOS O MÁS QUE RESIDEN EN\n URUGUAY POR CONDICIÓN DE ACTIVIDAD AL MOMENTO DE LA ENCUESTA SEGÚN \nCOMUNIDAD DE ORIGEN Y SEXO",
          #      " (",
          #      input$comunidad_ps,
          #      ")", sep = ""
          #    ),
          caption = "(*)Cuba: N=210 / República Dominicana: N=274 \n/Perú: N=229 / Venezuela: N=639 \n(**)Cuba: la categoría Inactivo tiene un N<20\n para las tres categorías de sexo.\nRepública Dominicana: la categoría Inactivo \npara Varones y Mujeres, y Desocupado para \nMujeres tienen un N<20.\nPerú: las categorías Inactivo y Desocupado \npara Varones, Desocupado para Mujeres \ny el Total tienen un N<20.\nVenezuela: la categoría Inactivo para \nVarones tiene un N<20.")
      
      
    }
    else if(input$indicador_enir_ps ==  "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 14 AÑOS O MÁS QUE RESIDEN EN MONTEVIDEO POR CONDICIÓN DE ACTIVIDAD AL MOMENTO DE LA ENCUESTA SEGÚN COMUNIDAD DE ORIGEN Y SEXO"
            & (input$comunidad_ps =="TODAS")){
      
      enir <- ggplot(data = enir_ps_todo(), aes(fill=catocup, y=valor, x=sexo)) +
        geom_bar(position="fill", stat="identity",width=0.5)  +
        scale_fill_brewer(palette="Blues")+
        facet_wrap(~ comunidad,scales = "free_y")+
        scale_y_continuous(labels=scales::percent) +
        theme(legend.title = element_blank(),
              strip.text.x = element_text(size = 8),
              axis.text.x = element_text(size=9,angle = 90),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",legend.justification = "left",legend.direction = "vertical",legend.key.size = unit(0.5,"cm"),
              legend.text = element_text(size=8),
              plot.caption = element_text(size=9,face = "italic",hjust = 0),
              plot.title = element_text(size=12))+
        labs(
          x = "",
          y = "",
          #    title = "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 14 AÑOS O MÁS QUE RESIDEN EN\n URUGUAY POR CONDICIÓN DE ACTIVIDAD AL MOMENTO DE LA ENCUESTA SEGÚN \nCOMUNIDAD DE ORIGEN Y SEXO",
          caption = "(*)Cuba: N=210 / República Dominicana: N=274 \n/Perú: N=229 / Venezuela: N=639 \n(**)Cuba: la categoría Inactivo tiene un N<20\n para las tres categorías de sexo.\nRepública Dominicana: la categoría Inactivo \npara Varones y Mujeres, y Desocupado para \nMujeres tienen un N<20.\nPerú: las categorías Inactivo y Desocupado \npara Varones, Desocupado para Mujeres \ny el Total tienen un N<20.\nVenezuela: la categoría Inactivo para \nVarones tiene un N<20.")
      
    } else if(input$indicador_enir_ps ==  "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 15 AÑOS O MÁS QUE RESIDEN EN MONTEVIDEO POR SITUACIÓN CONYUGAL AL MOMENTO DE LA ENCUESTA SEGÚN COMUNIDAD DE ORIGEN Y SEXO"
              & (input$comunidad_ps %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
      
      
      enir <- ggplot(data = enir_ps(), aes(fill=sitcony, y=valor, x=sexo)) +
        geom_bar(position="fill", stat="identity",width=0.5)  +
        scale_fill_brewer(palette="Blues")+
        scale_y_continuous(labels=scales::percent) +
        guides(fill=guide_legend(nrow=2)) +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(size=9,angle = 90),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",legend.justification = "left",legend.direction = "vertical",legend.key.size = unit(0.5,"cm"),
              legend.text = element_text(size=8),
              plot.caption = element_text(size=9,face = "italic",hjust = 0),
              plot.title = element_text(size=12))+
        labs(
          x = "",
          y = "",
          caption = "(*)Cuba: N=210 / República Dominicana: N=273\n/ Perú: N=228 / Venezuela: N=637 \n(**)Cuba: las categoría Viudo/a, Divorciado/a \ny Separado/a para Varones, Soltero/a, Viudo/a, \nDivorciado/a y Separado/a para Mujeres \ny Viudo/a, Divorciado/a y Separado/a \npara el Total tienen un N<20.\nRepública Dominicana: las categorías Viudo/a, \nDivorciado/a y Separado/a para Varones, \nViudo/a y Divorciado/a para Mujeres \ny Viudo/a y Divorciado/a para el Total \ntienen un N<20.\nPerú: las categorías Viudo/a, Divorciado/a \ny Separado/a para Varones, Viudo/a y \nDivorciado/a para Mujeres y Viudo/a y \nDivorciado/a para el Total tienen \nun N<20.\nVenezuela: las categorías Viudo/a, \nDivorciado/a y Separado/a para las \ntres categorias de sexo tienen un N<20.")
      
      
    }
    else if(input$indicador_enir_ps ==  "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 15 AÑOS O MÁS QUE RESIDEN EN MONTEVIDEO POR SITUACIÓN CONYUGAL AL MOMENTO DE LA ENCUESTA SEGÚN COMUNIDAD DE ORIGEN Y SEXO"
            & (input$comunidad_ps =="TODAS")){
      
      enir <- ggplot(data = enir_ps_todo(), aes(fill=sitcony, y=valor, x=sexo)) +
        geom_bar(position="fill", stat="identity",width=0.5)  +
        scale_fill_brewer(palette="Blues")+
        facet_wrap(~ comunidad,scales = "free_y")+
        scale_y_continuous(labels=scales::percent) +
        guides(fill=guide_legend(nrow=2)) +
        theme(legend.title = element_blank(),
              strip.text.x = element_text(size = 8),
              axis.text.x = element_text(size=9,angle = 90),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",legend.justification = "left",legend.direction = "vertical",legend.key.size = unit(0.5,"cm"),
              legend.text = element_text(size=8),
              plot.caption = element_text(size=9,face = "italic",hjust = 0),
              plot.title = element_text(size=12))+
        labs(
          x = "",
          y = "",
          #    title = "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 15 AÑOS O MÁS QUE RESIDEN \nEN URUGUAY POR SITUACIÓN CONYUGAL AL MOMENTO DE LA ENCUESTA\n SEGÚN COMUNIDAD DE ORIGEN Y SEXO",
          caption = "(*)Cuba: N=210 / República Dominicana: N=273\n/ Perú: N=228 / Venezuela: N=637 \n(**)Cuba: las categoría Viudo/a, Divorciado/a \ny Separado/a para Varones, Soltero/a, Viudo/a, \nDivorciado/a y Separado/a para Mujeres \ny Viudo/a, Divorciado/a y Separado/a \npara el Total tienen un N<20.\nRepública Dominicana: las categorías Viudo/a, \nDivorciado/a y Separado/a para Varones, \nViudo/a y Divorciado/a para Mujeres \ny Viudo/a y Divorciado/a para el Total \ntienen un N<20.\nPerú: las categorías Viudo/a, Divorciado/a \ny Separado/a para Varones, Viudo/a y \nDivorciado/a para Mujeres y Viudo/a y \nDivorciado/a para el Total tienen \nun N<20.\nVenezuela: las categorías Viudo/a, \nDivorciado/a y Separado/a para las \ntres categorias de sexo tienen un N<20.")
      
    }
    
    print(enir)
    ggsave("www/enir.png")
    
  })
  
  
  output$baja_barPlot_enir_ps <- downloadHandler(
    filename <- function() {
      paste("enir", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/enir.png", file)
    },
    contentType = "www/enir"
    
    
  )
  
  
  
  output$tabla_resultado_enir_ps  <- renderDT({
    if(input$indicador_enir_ps ==  "PIRÁMIDE POBLACIONAL DE LOS INMIGRANTES QUE RESIDEN EN MONTEVIDEO SEGÚN COMUNIDAD DE ORIGEN"
       & (input$comunidad_ps %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
      
      
      datatable(enir_ps() %>% 
                  arrange(comunidad) %>%
                  transmute("Comunidad" = comunidad,
                            "Sexo" = sexo,
                            "Tramo de edad" = tramos,
                            "Valor (%)" = round(valor*100, 1)),
                rownames = FALSE, options = list(dom = 'ltipr')) 
      
      
    } else if(input$indicador_enir_ps ==  "PIRÁMIDE POBLACIONAL DE LOS INMIGRANTES QUE RESIDEN EN MONTEVIDEO SEGÚN COMUNIDAD DE ORIGEN"
              & (input$comunidad_ps =="TODAS")){
      
      datatable(enir_ps_todo() %>% 
                  arrange(comunidad) %>%
                  transmute("Comunidad" = comunidad,
                            "Sexo" = sexo,
                            "Tramo de edad" = tramos,
                            "Valor (%)" = round(valor*100, 1)),
                rownames = FALSE, options = list(dom = 'ltipr')) 
      
    }
    
    else if(input$indicador_enir_ps ==  "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 25 AÑOS O MÁS QUE RESIDEN EN MONTEVIDEO POR NIVEL EDUCATIVO SEGÚN COMUNIDAD DE ORIGEN"
            & (input$comunidad_ps %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
      
      
      datatable(enir_ps() %>% 
                  arrange(comunidad) %>%
                  transmute("Comunidad" = comunidad,
                            "Nivel educativo" = niveledu,
                            "Valor (%)" = round(valor, 2)),
                rownames = FALSE, options = list(dom = 'ltipr')) 
      
      
    } else if(input$indicador_enir_ps ==  "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 25 AÑOS O MÁS QUE RESIDEN EN MONTEVIDEO POR NIVEL EDUCATIVO SEGÚN COMUNIDAD DE ORIGEN"
              & (input$comunidad_ps =="TODAS")){
      
      datatable(enir_ps_todo() %>% 
                  arrange(comunidad) %>%
                  transmute("Comunidad" = comunidad,
                            "Nivel educativo" = niveledu,
                            "Valor (%)" = round(valor, 2)),
                rownames = FALSE, options = list(dom = 'ltipr')) 
      
    } 
    else if(input$indicador_enir_ps ==  "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 14 AÑOS O MÁS QUE RESIDEN EN MONTEVIDEO POR CONDICIÓN DE ACTIVIDAD AL MOMENTO DE LA ENCUESTA SEGÚN COMUNIDAD DE ORIGEN Y SEXO"
            & (input$comunidad_ps %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
      
      
      datatable(enir_ps() %>% 
                  arrange(comunidad) %>%
                  transmute("Comunidad" = comunidad,
                            "Categoría de ocupación" = catocup,
                            "Sexo" = sexo,
                            "Valor (%)" = round(valor, 2)),
                rownames = FALSE, options = list(dom = 'ltipr')) 
      
      
    }
    else if(input$indicador_enir_ps ==  "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 14 AÑOS O MÁS QUE RESIDEN EN MONTEVIDEO POR CONDICIÓN DE ACTIVIDAD AL MOMENTO DE LA ENCUESTA SEGÚN COMUNIDAD DE ORIGEN Y SEXO"
            & (input$comunidad_ps =="TODAS")){
      
      datatable(enir_ps_todo() %>% 
                  arrange(comunidad) %>%
                  transmute("Comunidad" = comunidad,
                            "Categoría de ocupación" = catocup,
                            "Sexo" = sexo,
                            "Valor (%)" = round(valor, 2)),
                rownames = FALSE, options = list(dom = 'ltipr')) 
      
      
    } else if(input$indicador_enir_ps ==  "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 15 AÑOS O MÁS QUE RESIDEN EN MONTEVIDEO POR SITUACIÓN CONYUGAL AL MOMENTO DE LA ENCUESTA SEGÚN COMUNIDAD DE ORIGEN Y SEXO"
              & (input$comunidad_ps %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
      
      
      datatable(enir_ps() %>% 
                  arrange(comunidad) %>%
                  transmute("Comunidad" = comunidad,
                            "Situación conyugal" = sitcony,
                            "Sexo" = sexo,
                            "Valor (%)" = round(valor, 2)),
                rownames = FALSE, options = list(dom = 'ltipr')) 
      
      
    }
    else if(input$indicador_enir_ps ==  "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 15 AÑOS O MÁS QUE RESIDEN EN MONTEVIDEO POR SITUACIÓN CONYUGAL AL MOMENTO DE LA ENCUESTA SEGÚN COMUNIDAD DE ORIGEN Y SEXO"
            & (input$comunidad_ps =="TODAS")){
      
      datatable(enir_ps_todo() %>% 
                  arrange(comunidad) %>%
                  transmute("Comunidad" = comunidad,
                            "Situación conyugal" = sitcony,
                            "Sexo" = sexo,
                            "Valor (%)" = round(valor, 2)),
                rownames = FALSE, options = list(dom = 'ltipr')) 
      
    }
    
  })   
  
  
  output$dl_tabla_resultado_enir_ps <- downloadHandler(
    
    
    filename = function() {
      paste0("resultados- ",input$indicador_enir_ps, ".csv", sep = "")
    },
    content = function(file) {
      
      if(input$indicador_enir_ps ==  "PIRÁMIDE POBLACIONAL DE LOS INMIGRANTES QUE RESIDEN EN MONTEVIDEO SEGÚN COMUNIDAD DE ORIGEN"
         & (input$comunidad_ps %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
        
        
        write.csv(enir_ps() %>% 
                    arrange(comunidad) %>%
                    transmute("Comunidad" = comunidad,
                              "Sexo" = sexo,
                              "Tramo de edad" = tramos,
                              "Valor (%)" = round(valor*100, 1)),
                  file, row.names = FALSE, fileEncoding = "latin1") 
        
        
      } else if(input$indicador_enir_ps ==  "PIRÁMIDE POBLACIONAL DE LOS INMIGRANTES QUE RESIDEN EN MONTEVIDEO SEGÚN COMUNIDAD DE ORIGEN"
                & (input$comunidad_ps =="TODAS")){
        
        write.csv(enir_ps_todo() %>% 
                    arrange(comunidad) %>%
                    transmute("Comunidad" = comunidad,
                              "Sexo" = sexo,
                              "Tramo de edad" = tramos,
                              "Valor (%)" = round(valor*100, 1)),
                  file, row.names = FALSE, fileEncoding = "latin1") 
        
      }
      
      
      else if(input$indicador_enir_ps ==  "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 25 AÑOS O MÁS QUE RESIDEN EN MONTEVIDEO POR NIVEL EDUCATIVO SEGÚN COMUNIDAD DE ORIGEN"
              & (input$comunidad_ps %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
        
        
        write.csv(enir_ps() %>% 
                    arrange(comunidad) %>%
                    transmute("Comunidad" = comunidad,
                              "Nivel educativo" = niveledu,
                              "Valor (%)" = round(valor, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
      } else if(input$indicador_enir_ps ==  "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 25 AÑOS O MÁS QUE RESIDEN EN MONTEVIDEO POR NIVEL EDUCATIVO SEGÚN COMUNIDAD DE ORIGEN"
                & (input$comunidad_ps =="TODAS")){
        
        write.csv(enir_ps_todo() %>% 
                    arrange(comunidad) %>%
                    transmute("Comunidad" = comunidad,
                              "Nivel educativo" = niveledu,
                              "Valor (%)" = round(valor, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1") 
        
      } 
      else if(input$indicador_enir_ps ==  "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 14 AÑOS O MÁS QUE RESIDEN EN MONTEVIDEO POR CONDICIÓN DE ACTIVIDAD AL MOMENTO DE LA ENCUESTA SEGÚN COMUNIDAD DE ORIGEN Y SEXO"
              & (input$comunidad_ps %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
        
        
        write.csv(enir_ps() %>% 
                    arrange(comunidad) %>%
                    transmute("Comunidad" = comunidad,
                              "Categoría de ocupación" = catocup,
                              "Sexo" = sexo,
                              "Valor (%)" = round(valor, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1") 
        
        
      }
      else if(input$indicador_enir_ps ==  "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 14 AÑOS O MÁS QUE RESIDEN EN MONTEVIDEO POR CONDICIÓN DE ACTIVIDAD AL MOMENTO DE LA ENCUESTA SEGÚN COMUNIDAD DE ORIGEN Y SEXO"
              & (input$comunidad_ps =="TODAS")){
        
        write.csv(enir_ps_todo() %>% 
                    arrange(comunidad) %>%
                    transmute("Comunidad" = comunidad,
                              "Categoría de ocupación" = catocup,
                              "Sexo" = sexo,
                              "Valor (%)" = round(valor, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1") 
        
        
      } else if(input$indicador_enir_ps ==  "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 15 AÑOS O MÁS QUE RESIDEN EN MONTEVIDEO POR SITUACIÓN CONYUGAL AL MOMENTO DE LA ENCUESTA SEGÚN COMUNIDAD DE ORIGEN Y SEXO"
                & (input$comunidad_ps %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
        
        
        write.csv(enir_ps() %>% 
                    arrange(comunidad) %>%
                    transmute("Comunidad" = comunidad,
                              "Situación conyugal" = sitcony,
                              "Sexo" = sexo,
                              "Valor (%)" = round(valor, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1") 
        
        
      }
      else if(input$indicador_enir_ps ==  "DISTRIBUCIÓN RELATIVA DE LOS INMIGRANTES CON 15 AÑOS O MÁS QUE RESIDEN EN MONTEVIDEO POR SITUACIÓN CONYUGAL AL MOMENTO DE LA ENCUESTA SEGÚN COMUNIDAD DE ORIGEN Y SEXO"
              & (input$comunidad_ps =="TODAS")){
        
        write.csv(enir_ps_todo() %>% 
                    arrange(comunidad) %>%
                    transmute("Comunidad" = comunidad,
                              "Situación conyugal" = sitcony,
                              "Sexo" = sexo,
                              "Valor (%)" = round(valor, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1") 
        
      }
      
    })
  
  
  
  enir_em <- reactive({
    
    enir_em <- indicadores %>%
      filter(fuente == "ENIR") %>%
      mutate(sexo = factor(sexo, levels = c("VARONES","MUJERES","TOTAL")))%>%
      mutate(futuro = factor(futuro, levels = c("SU PAÍS DE NACIMIENTO","URUGUAY","UN TERCER PAÍS","DESCONOCIDO")))%>%
      filter(nomindicador == input$indicador_enir_em)%>%
      filter(comunidad == input$comunidad_em)
    
    
  })
  
  
  enir_em_todo <- reactive({
    
    enir_em_todo <- indicadores %>%
      filter(fuente == "ENIR") %>%
      mutate(sexo = factor(sexo, levels = c("VARONES","MUJERES","TOTAL")))%>%
      mutate(futuro = factor(futuro, levels = c("SU PAÍS DE NACIMIENTO","URUGUAY","UN TERCER PAÍS","DESCONOCIDO")))%>%
      filter(nomindicador == input$indicador_enir_em)
    
  })
  
  output$title_enir_em <- renderUI({ 
    helpText(HTML(paste(input$indicador_enir_em,"(",input$comunidad_em,")")))
  })    
  
  output$barPlot_enir_em <- renderPlot({
    
    if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR LOS MOTIVOS DE SU ÚLTIMA MIGRACIÓN SEGÚN COMUNIDAD DE ORIGEN"
       & (input$comunidad_em %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
      
      
      enir_em <- ggplot(data = enir_em(), aes(fill=motivos, y=valor, x=comunidad)) +
        geom_bar(position="fill", stat="identity",width=0.5)  +
        scale_fill_brewer(palette="Blues")+
        scale_y_continuous(labels=scales::percent) +
        guides(fill=guide_legend(nrow=5)) +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(size=9,angle = 90),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",legend.justification = "left",
              #legend.direction = "vertical",legend.key.size = unit(0.3,"cm"),
              legend.text = element_text(size=8),
              plot.caption = element_text(size=9,face = "italic",hjust = 0),
              plot.title = element_text(size=12))+
        labs(
          x = "",
          y = "",
          #    title = paste(
          #      "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR LOS MOTIVOS DE SU ÚLTIMA MIGRACIÓN \nSEGÚN COMUNIDAD DE ORIGEN",
          #      " (",
          #      input$comunidad_em,
          #      ")", sep = ""
          #    ),
          caption = "(*)Cuba: N=136 /República Dominicana: N=172\n/ Perú: N=124 / Venezuela: N=371 \n(**)Cuba: las categoría Por motivos familiares \ny Otros tienen un N<20.\nRepública Dominicana: las categorías Violencia, \nPersecución política e Inseguridad, y \nOtros tienen un N<20.\nPerú: las categorías Buscaba mejor calidad \nde vida, Violencia, Persecución política \ne Inseguridad, y Otros tienen un N<20.\nVenezuela: la categoría Otros tiene un N<20.")
      
      
    } else if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR LOS MOTIVOS DE SU ÚLTIMA MIGRACIÓN SEGÚN COMUNIDAD DE ORIGEN"
              & (input$comunidad_em =="TODAS")){
      
      enir_em <- ggplot(data = enir_em_todo(), aes(fill=motivos, y=valor, x=comunidad)) +
        geom_bar(position="fill", stat="identity",width=0.5)  +
        scale_fill_brewer(palette="Blues")+
        scale_y_continuous(labels=scales::percent) +
        guides(fill=guide_legend(nrow=5)) +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(size=9,angle = 90),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",
              legend.justification = "left",
              legend.direction = "vertical",
              #legend.key.size = unit(0.3,"cm"),
              legend.text = element_text(size=8),
              plot.caption = element_text(size=9,face = "italic",hjust = 0),
              plot.title = element_text(size=12))+
        labs(
          x = "",
          y = "",
          #    title = "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR LOS MOTIVOS DE SU ÚLTIMA MIGRACIÓN \nSEGÚN COMUNIDAD DE ORIGEN",
          caption = "(*)Cuba: N=136 /República Dominicana: N=172\n/ Perú: N=124 / Venezuela: N=371 \n(**)Cuba: las categoría Por motivos familiares \ny Otros tienen un N<20.\nRepública Dominicana: las categorías Violencia, \nPersecución política e Inseguridad, y \nOtros tienen un N<20.\nPerú: las categorías Buscaba mejor calidad \nde vida, Violencia, Persecución política \ne Inseguridad, y Otros tienen un N<20.\nVenezuela: la categoría Otros tiene un N<20.")
      
    } else if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL PRINCIPAL MOTIVO POR EL QUE ELIGIÓ URUGUAY COMO DESTINO DE SU ÚLTIMA MIGRACIÓN SEGÚN COMUNIDAD DE ORIGEN"
              & (input$comunidad_em %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
      
      
      enir_em <- ggplot(data = enir_em(), aes(fill=miguy, y=valor, x=comunidad)) +
        geom_bar(position="fill", stat="identity",width=0.5)  +
        scale_fill_brewer(palette="Blues")+
        scale_y_continuous(labels=scales::percent) +
        guides(fill=guide_legend(nrow=4)) +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(size=9,angle = 90),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",
              legend.justification = "left",
              legend.direction = "vertical",
              #legend.key.size = unit(0.3,"cm"),
              legend.text = element_text(size=8),
              plot.caption = element_text(size=9,face = "italic",hjust = 0),
              plot.title = element_text(size=12))+
        labs(
          x = "",
          y = "",
          #    title = paste(
          #      "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL PRINCIPAL MOTIVO POR EL \nQUE ELIGIÓ URUGUAY COMO DESTINO DE SU ÚLTIMA MIGRACIÓN SEGÚN COMUNIDAD \nDE ORIGEN",
          #      " (",
          #      input$comunidad_em,
          #      ")", sep = ""
          #    ),
          caption = "(*)Cuba: N=130 /República Dominicana: N=168 \n/Perú: N=110 / Venezuela: N=371 \n(**)Cuba: las categoría Razones económicas \ny Otros tienen un N<20.\nRepública Dominicana: la categoría Otros \ntiene un N<20.\nPerú: la categoría Otros \ntiene un N<20.\nVenezuela: la categoría Otros tiene \nun N<20.")
      
      
    } else if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL PRINCIPAL MOTIVO POR EL QUE ELIGIÓ URUGUAY COMO DESTINO DE SU ÚLTIMA MIGRACIÓN SEGÚN COMUNIDAD DE ORIGEN"
              & (input$comunidad_em =="TODAS")){
      
      enir_em <- ggplot(data = enir_em_todo(), aes(fill=miguy, y=valor, x=comunidad)) +
        geom_bar(position="fill", stat="identity",width=0.5)  +
        scale_fill_brewer(palette="Blues")+
        scale_y_continuous(labels=scales::percent) +
        guides(fill=guide_legend(nrow=4)) +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(size=9,angle = 90),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",
              legend.justification = "left",
              legend.direction = "vertical",
              #legend.key.size = unit(0.3,"cm"),
              legend.text = element_text(size=8),
              plot.caption = element_text(size=9,face = "italic",hjust = 0),
              plot.title = element_text(size=12))+
        labs(
          x = "",
          y = "",
          #    title = "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL PRINCIPAL MOTIVO POR EL \nQUE ELIGIÓ URUGUAY COMO DESTINO DE SU ÚLTIMA MIGRACIÓN SEGÚN COMUNIDAD \nDE ORIGEN",
          caption = "(*)Cuba: N=130 /República Dominicana: N=168 \n/Perú: N=110 / Venezuela: N=371 \n(**)Cuba: las categoría Razones económicas \ny Otros tienen un N<20.\nRepública Dominicana: la categoría Otros \ntiene un N<20.\nPerú: la categoría Otros \ntiene un N<20.\nVenezuela: la categoría Otros tiene \nun N<20.")
      
    } else if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL LUGAR DONDE LE GUSTARÍA ESTAR VIVIENDO EN TRES AÑOS SEGÚN COMUNIDAD DE ORIGEN"
              & (input$comunidad_em %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
      
      
      enir_em <- ggplot(data = enir_em(), aes(fill=futuro, y=valor, x=comunidad)) +
        geom_bar(position="fill", stat="identity",width=0.5)  +
        scale_fill_brewer(palette="Blues")+
        scale_y_continuous(labels=scales::percent) +
        guides(fill=guide_legend(nrow=4)) +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(size=9,angle = 90),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",
              legend.justification = "left",
              legend.direction = "vertical",
              #legend.key.size = unit(0.3,"cm"),
              legend.text = element_text(size=8),
              plot.caption = element_text(size=9,face = "italic",hjust = 0),
              plot.title = element_text(size=12))+
        labs(
          x = "",
          y = "",
          #    title = paste(
          #      "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL LUGAR DONDE LE GUSTARÍA\n ESTAR VIVIENDO EN TRES AÑOS SEGÚN COMUNIDAD DE ORIGEN",
          #      " (",
          #      input$comunidad_em,
          #      ")", sep = ""
          #    ),
          caption = "(*)Cuba: N=136 /República Dominicana: N=172 \n/Perú: N=124 / Venezuela: N=371 \n(**)Cuba: las categorías Su país de nacimiento \ny Desconocido tienen un N<20.\nRepública Dominicana: la categoría \nDesconocido tiene un N<20.\nPerú: la categoría Desconocido tiene \nun N<20.\nVenezuela: la categoría Desconocido \ntiene un N<20.")
      
      
    } else if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL LUGAR DONDE LE GUSTARÍA ESTAR VIVIENDO EN TRES AÑOS SEGÚN COMUNIDAD DE ORIGEN"
              & (input$comunidad_em =="TODAS")){
      
      enir_em <- ggplot(data = enir_em_todo(), aes(fill=futuro, y=valor, x=comunidad)) +
        geom_bar(position="fill", stat="identity",width=0.5)  +
        scale_fill_brewer(palette="Blues")+
        scale_y_continuous(labels=scales::percent) +
        guides(fill=guide_legend(nrow=4)) +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(size=9,angle = 90),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",
              legend.justification = "left",
              legend.direction = "vertical",
              #legend.key.size = unit(0.3,"cm"),
              legend.text = element_text(size=8),
              plot.caption = element_text(size=9,face = "italic",hjust = 0),
              plot.title = element_text(size=12))+
        labs(
          x = "",
          y = "",
          #    title = "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL LUGAR DONDE LE GUSTARÍA \nESTAR VIVIENDO EN TRES AÑOS SEGÚN COMUNIDAD DE ORIGEN",
          caption = "(*)Cuba: N=136 /República Dominicana: N=172 \n/Perú: N=124 / Venezuela: N=371 \n(**)Cuba: las categorías Su país de nacimiento \ny Desconocido tienen un N<20.\nRepública Dominicana: la categoría \nDesconocido tiene un N<20.\nPerú: la categoría Desconocido tiene \nun N<20.\nVenezuela: la categoría Desconocido \ntiene un N<20.")
      
    }else if(input$indicador_enir_em ==  "PORCENTAJE DE INFORMANTES QUE ENVÍAN REMESAS SEGÚN COMUNIDAD DE ORIGEN Y SEXO"
             & (input$comunidad_em %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
      
      
      enir_em = ggplot(data = enir_em(), aes(x=sexo,y=valor/100)) +
        geom_bar(position="dodge", stat="identity",width=0.3,fill="#7bb7dd")  +
        scale_y_continuous(limits=c(0,0.8),labels=scales::percent) +
        guides(fill=guide_legend(nrow=3)) +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(size=9,angle = 90),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",
              legend.justification = "left",
              legend.direction = "vertical",
              #legend.key.size = unit(0.3,"cm"),
              legend.text = element_text(size=8),
              plot.caption = element_text(size=9,face = "italic",hjust = 0),
              plot.title = element_text(size=12))+
        labs(
          x = "",
          y = "",
          #    title = paste(
          #      "PORCENTAJE DE INFORMANTES QUE ENVÍAN REMESAS SEGÚN \nCOMUNIDAD DE ORIGEN Y SEXO",
          #      " (",
          #      input$comunidad_em,
          #      ")", sep = ""
          #    ),
          caption = "(*)Cuba: N=136 / República Dominicana: N=172 \n/Perú: N=124 / Venezuela: N=371")
      
    } else if(input$indicador_enir_em ==  "PORCENTAJE DE INFORMANTES QUE ENVÍAN REMESAS SEGÚN COMUNIDAD DE ORIGEN Y SEXO"
              & (input$comunidad_em =="TODAS")){
      
      enir_em <- ggplot(data = enir_em_todo(), aes(x=comunidad,y=valor/100,fill=sexo,width = 0.6)) +
        geom_bar(position=position_dodge(width = 0.8), stat="identity")  +
        scale_fill_brewer(palette="Blues")+
        scale_y_continuous(limits=c(0,0.8),labels=scales::percent) +
        guides(fill=guide_legend(nrow=3)) +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(size=9,angle = 90),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",
              legend.justification = "left",
              legend.direction = "vertical",
              #legend.key.size = unit(0.3,"cm"),
              legend.text = element_text(size=8),
              plot.caption = element_text(size=9,face = "italic",hjust = 0),
              plot.title = element_text(size=12))+
        labs(
          x = "",
          y = "",
          #    title = "PORCENTAJE DE INFORMANTES QUE ENVÍAN REMESAS SEGÚN \nCOMUNIDAD DE ORIGEN Y SEXO",
          caption = "(*)Cuba: N=136 / República Dominicana: N=172 /\nPerú: N=124 / Venezuela: N=371")
      
    }
    
    
    print(enir_em)
    ggsave("www/enir_em.png")
    
  })
  
  
  
  # output$barPlot_enir_em <- renderImage({
  #   
  #   outfile <- tempfile(fileext = '.png')
  #   
  #   png(outfile, width = 400, height = 300)
  #   
  #   
  #   
  #   if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR LOS MOTIVOS DE SU ÚLTIMA MIGRACIÓN SEGÚN COMUNIDAD DE ORIGEN"
  #      & (input$comunidad_em %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
  #     
  #     
  #     enir_em <- ggplot(data = enir_em(), aes(fill=motivos, y=valor, x=comunidad)) +
  #       geom_bar(position="fill", stat="identity",width=0.5)  +
  #       scale_fill_brewer(palette="Blues")+
  #       scale_y_continuous(labels=scales::percent) +
  #       theme(legend.title = element_blank(),
  #             legend.text = element_text(size=10),axis.text=element_text(size=10),
  #             plot.caption = element_text(size=10,face = "italic",hjust = 0),
  #             plot.title = element_text(size=18))+
  #       labs(x = "",
  #            y = "",
  #            title = paste(
  #              "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR LOS MOTIVOS DE SU ÚLTIMA MIGRACIÓN \nSEGÚN COMUNIDAD DE ORIGEN",
  #              " (",
  #              input$comunidad_em,
  #              ")", sep = ""
  #            ),
  #            caption = "(*)Cuba: N=136 / República Dominicana: N=172 / Perú: N=124 / Venezuela: N=371 \n(**)Cuba: las categoría Por motivos familiares y Otros tienen un N<20.\nRepública Dominicana: las categorías Violencia, Persecución política e Inseguridad, y Otros tienen un N<20.\nPerú: las categorías Buscaba mejor calidad de vida, Violencia, Persecución política e Inseguridad, y Otros tienen un N<20.\nVenezuela: la categoría Otros tiene un N<20.")
  #     
  #     
  #   } else if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR LOS MOTIVOS DE SU ÚLTIMA MIGRACIÓN SEGÚN COMUNIDAD DE ORIGEN"
  #             & (input$comunidad_em =="TODAS")){
  #     
  #     enir_em <- ggplot(data = enir_em_todo(), aes(fill=motivos, y=valor, x=comunidad)) +
  #       geom_bar(position="fill", stat="identity",width=0.5)  +
  #       scale_fill_brewer(palette="Blues")+
  #       scale_y_continuous(labels=scales::percent) +
  #       guides(fill=guide_legend(nrow=2)) +
  #       theme(legend.title = element_blank(),legend.position = "bottom",
  #             legend.text = element_text(size=10),axis.text=element_text(size=10),
  #             plot.caption = element_text(size=10,face = "italic",hjust = 0),
  #             plot.title = element_text(size=18))+
  #       labs(x = "",
  #            y = "",
  #            title = "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR LOS MOTIVOS DE SU ÚLTIMA MIGRACIÓN \nSEGÚN COMUNIDAD DE ORIGEN",
  #            caption = "(*)Cuba: N=136 / República Dominicana: N=172 / Perú: N=124 / Venezuela: N=371 \n(**)Cuba: las categoría Por motivos familiares y Otros tienen un N<20.\nRepública Dominicana: las categorías Violencia, Persecución política e Inseguridad, y Otros tienen un N<20.\nPerú: las categorías Buscaba mejor calidad de vida, Violencia, Persecución política e Inseguridad, y Otros tienen un N<20.\nVenezuela: la categoría Otros tiene un N<20.")
  #     
  #   } else if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL PRINCIPAL MOTIVO POR EL QUE ELIGIÓ URUGUAY COMO DESTINO DE SU ÚLTIMA MIGRACIÓN SEGÚN COMUNIDAD DE ORIGEN"
  #             & (input$comunidad_em %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
  #     
  #     
  #     enir_em <- ggplot(data = enir_em(), aes(fill=miguy, y=valor, x=comunidad)) +
  #       geom_bar(position="fill", stat="identity",width=0.5)  +
  #       scale_fill_brewer(palette="Blues")+
  #       scale_y_continuous(labels=scales::percent) +
  #       theme(legend.title = element_blank(),
  #             legend.text = element_text(size=10),axis.text=element_text(size=10),
  #             plot.caption = element_text(size=10,face = "italic",hjust = 0),
  #             plot.title = element_text(size=18))+
  #       labs(x = "",
  #            y = "",
  #            title = paste(
  #              "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL PRINCIPAL MOTIVO POR EL \nQUE ELIGIÓ URUGUAY COMO DESTINO DE SU ÚLTIMA MIGRACIÓN SEGÚN COMUNIDAD \nDE ORIGEN",
  #              " (",
  #              input$comunidad_em,
  #              ")", sep = ""
  #            ),
  #            caption = "(*)Cuba: N=130 / República Dominicana: N=168 / Perú: N=110 / Venezuela: N=371 \n(**)Cuba: las categoría Razones económicas y Otros tienen un N<20.\nRepública Dominicana: la categoría Otros tiene un N<20.\nPerú: la categoría Otros tiene un N<20.\nVenezuela: la categoría Otros tiene un N<20.")
  #     
  #     
  #   } else if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL PRINCIPAL MOTIVO POR EL QUE ELIGIÓ URUGUAY COMO DESTINO DE SU ÚLTIMA MIGRACIÓN SEGÚN COMUNIDAD DE ORIGEN"
  #             & (input$comunidad_em =="TODAS")){
  #     
  #     enir_em <- ggplot(data = enir_em_todo(), aes(fill=miguy, y=valor, x=comunidad)) +
  #       geom_bar(position="fill", stat="identity",width=0.5)  +
  #       scale_fill_brewer(palette="Blues")+
  #       scale_y_continuous(labels=scales::percent) +
  #       guides(fill=guide_legend(nrow=2)) +
  #       theme(legend.title = element_blank(),legend.position = "bottom",
  #             legend.text = element_text(size=10),axis.text=element_text(size=10),
  #             plot.caption = element_text(size=10,face = "italic",hjust = 0),
  #             plot.title = element_text(size=18))+
  #       labs(x = "",
  #            y = "",
  #            title = "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL PRINCIPAL MOTIVO POR EL \nQUE ELIGIÓ URUGUAY COMO DESTINO DE SU ÚLTIMA MIGRACIÓN SEGÚN COMUNIDAD \nDE ORIGEN",
  #            caption = "(*)Cuba: N=130 / República Dominicana: N=168 / Perú: N=110 / Venezuela: N=371 \n(**)Cuba: las categoría Razones económicas y Otros tienen un N<20.\nRepública Dominicana: la categoría Otros tiene un N<20.\nPerú: la categoría Otros tiene un N<20.\nVenezuela: la categoría Otros tiene un N<20.")
  #     
  #   } else if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL LUGAR DONDE LE GUSTARÍA ESTAR VIVIENDO EN TRES AÑOS SEGÚN COMUNIDAD DE ORIGEN"
  #             & (input$comunidad_em %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
  #     
  #     
  #     enir_em <- ggplot(data = enir_em(), aes(fill=futuro, y=valor, x=comunidad)) +
  #       geom_bar(position="fill", stat="identity",width=0.5)  +
  #       scale_fill_brewer(palette="Blues")+
  #       scale_y_continuous(labels=scales::percent) +
  #       theme(legend.title = element_blank(),
  #             legend.text = element_text(size=10),axis.text=element_text(size=10),
  #             plot.caption = element_text(size=10,face = "italic",hjust = 0),
  #             plot.title = element_text(size=18))+
  #       labs(x = "",
  #            y = "",
  #            title = paste(
  #              "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL LUGAR DONDE LE GUSTARÍA\n ESTAR VIVIENDO EN TRES AÑOS SEGÚN COMUNIDAD DE ORIGEN",
  #              " (",
  #              input$comunidad_em,
  #              ")", sep = ""
  #            ),
  #            caption = "(*)Cuba: N=136 / República Dominicana: N=172 / Perú: N=124 / Venezuela: N=371 \n(**)Cuba: las categorías Su país de nacimiento y Desconocido tienen un N<20.\nRepública Dominicana: la categoría Desconocido tiene un N<20.\nPerú: la categoría Desconocido tiene un N<20.\nVenezuela: la categoría Desconocido tiene un N<20.")
  #     
  #     
  #   } else if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL LUGAR DONDE LE GUSTARÍA ESTAR VIVIENDO EN TRES AÑOS SEGÚN COMUNIDAD DE ORIGEN"
  #             & (input$comunidad_em =="TODAS")){
  #     
  #     enir_em <- ggplot(data = enir_em_todo(), aes(fill=futuro, y=valor, x=comunidad)) +
  #       geom_bar(position="fill", stat="identity",width=0.5)  +
  #       scale_fill_brewer(palette="Blues")+
  #       scale_y_continuous(labels=scales::percent) +
  #       guides(fill=guide_legend(nrow=2)) +
  #       theme(legend.title = element_blank(),legend.position = "bottom",
  #             legend.text = element_text(size=10),axis.text=element_text(size=10),
  #             plot.caption = element_text(size=10,face = "italic",hjust = 0),
  #             plot.title = element_text(size=18))+
  #       labs(x = "",
  #            y = "",
  #            title = "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL LUGAR DONDE LE GUSTARÍA \nESTAR VIVIENDO EN TRES AÑOS SEGÚN COMUNIDAD DE ORIGEN",
  #            caption = "(*)Cuba: N=136 / República Dominicana: N=172 / Perú: N=124 / Venezuela: N=371 \n(**)Cuba: las categorías Su país de nacimiento y Desconocido tienen un N<20.\nRepública Dominicana: la categoría Desconocido tiene un N<20.\nPerú: la categoría Desconocido tiene un N<20.\nVenezuela: la categoría Desconocido tiene un N<20.")
  #     
  #   }else if(input$indicador_enir_em ==  "PORCENTAJE DE INFORMANTES QUE ENVÍAN REMESAS SEGÚN COMUNIDAD DE ORIGEN Y SEXO"
  #            & (input$comunidad_em %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
  #     
  #     
  #     enir_em = ggplot(data = enir_em(), aes(x=sexo,y=valor/100)) +
  #       geom_bar(position="dodge", stat="identity",width=0.3,fill="#7bb7dd")  +
  #       scale_y_continuous(limits=c(0,0.8),labels=scales::percent) +
  #       theme(legend.title = element_blank(),
  #             legend.text = element_text(size=10),axis.text=element_text(size=10),
  #             plot.title = element_text(size=18),
  #             plot.caption = element_text(size=10,face = "italic",hjust = 0))+
  #       labs(x = "",
  #            y = "",
  #            title = paste(
  #              "PORCENTAJE DE INFORMANTES QUE ENVÍAN REMESAS SEGÚN \nCOMUNIDAD DE ORIGEN Y SEXO",
  #              " (",
  #              input$comunidad_em,
  #              ")", sep = ""
  #            ),caption = "(*)Cuba: N=136 / República Dominicana: N=172 / Perú: N=124 / Venezuela: N=371")
  #     
  #   } else if(input$indicador_enir_em ==  "PORCENTAJE DE INFORMANTES QUE ENVÍAN REMESAS SEGÚN COMUNIDAD DE ORIGEN Y SEXO"
  #             & (input$comunidad_em =="TODAS")){
  #     
  #     enir_em <- ggplot(data = enir_em_todo(), aes(x=comunidad,y=valor/100,fill=sexo,width = 0.6)) +
  #       geom_bar(position=position_dodge(width = 0.8), stat="identity")  +
  #       scale_fill_brewer(palette="Blues")+
  #       scale_y_continuous(limits=c(0,0.8),labels=scales::percent) +
  #       theme(legend.title = element_blank(),
  #             legend.text = element_text(size=10),axis.text=element_text(size=10),
  #             plot.title = element_text(size=18),
  #             plot.caption = element_text(size=10,face = "italic",hjust = 0)) +
  #       labs(x = "",
  #            y = "",
  #            title = "PORCENTAJE DE INFORMANTES QUE ENVÍAN REMESAS SEGÚN \nCOMUNIDAD DE ORIGEN Y SEXO",
  #            caption = "(*)Cuba: N=136 / República Dominicana: N=172 / Perú: N=124 / Venezuela: N=371")
  #     
  #   }
  #   
  #   print(enir_em)
  #   ggsave("www/enir_em.png")
  #   
  #   dev.off()
  #   list(src = outfile,
  #        contentType = 'image/png',
  #        width = 400,
  #        height = 300,
  #        alt = "This is alternate text")
  #   
  #   
  #   
  # }, deleteFile = TRUE)
  
  
  
  
  output$baja_barPlot_enir_em <- downloadHandler(
    filename <- function() {
      paste("enir_em", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/enir_em.png", file)
    },
    contentType = "www/enir_em"
    
    
  )
  
  
  output$tabla_resultado_enir_em  <- renderDT({
    
    if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR LOS MOTIVOS DE SU ÚLTIMA MIGRACIÓN SEGÚN COMUNIDAD DE ORIGEN"
       & (input$comunidad_em %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
      
      
      datatable(enir_em() %>% 
                  arrange(comunidad) %>%
                  transmute("Comunidad" = comunidad,
                            "Motivo" = motivos,
                            "Valor (%)" = round(valor, 2)),
                rownames = FALSE, options = list(dom = 'ltipr')) 
      
      
      
    } else if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR LOS MOTIVOS DE SU ÚLTIMA MIGRACIÓN SEGÚN COMUNIDAD DE ORIGEN"
              & (input$comunidad_em =="TODAS")){
      
      datatable(enir_em_todo() %>% 
                  arrange(comunidad) %>%
                  transmute("Comunidad" = comunidad,
                            "Motivo" = motivos,
                            "Valor (%)" = round(valor, 2)),
                rownames = FALSE, options = list(dom = 'ltipr')) 
      
    } else if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL PRINCIPAL MOTIVO POR EL QUE ELIGIÓ URUGUAY COMO DESTINO DE SU ÚLTIMA MIGRACIÓN SEGÚN COMUNIDAD DE ORIGEN"
              & (input$comunidad_em %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
      
      datatable(enir_em() %>% 
                  arrange(comunidad) %>%
                  transmute("Comunidad" = comunidad,
                            "Motivo" = miguy,
                            "Valor (%)" = round(valor, 2)),
                rownames = FALSE, options = list(dom = 'ltipr')) 
      
      
    } else if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL PRINCIPAL MOTIVO POR EL QUE ELIGIÓ URUGUAY COMO DESTINO DE SU ÚLTIMA MIGRACIÓN SEGÚN COMUNIDAD DE ORIGEN"
              & (input$comunidad_em =="TODAS")){
      
      datatable(enir_em_todo() %>% 
                  arrange(comunidad) %>%
                  transmute("Comunidad" = comunidad,
                            "Motivo" = miguy,
                            "Valor (%)" = round(valor, 2)),
                rownames = FALSE, options = list(dom = 'ltipr'))
      
      
    } else if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL LUGAR DONDE LE GUSTARÍA ESTAR VIVIENDO EN TRES AÑOS SEGÚN COMUNIDAD DE ORIGEN"
              & (input$comunidad_em %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
      
      
      datatable(enir_em() %>% 
                  arrange(comunidad) %>%
                  transmute("Comunidad" = comunidad,
                            "Lugar" = futuro,
                            "Valor (%)" = round(valor, 2)),
                rownames = FALSE, options = list(dom = 'ltipr'))
      
      
    } else if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL LUGAR DONDE LE GUSTARÍA ESTAR VIVIENDO EN TRES AÑOS SEGÚN COMUNIDAD DE ORIGEN"
              & (input$comunidad_em =="TODAS")){
      
      datatable(enir_em_todo() %>% 
                  arrange(comunidad) %>%
                  transmute("Comunidad" = comunidad,
                            "Lugar" = futuro,
                            "Valor (%)" = round(valor, 2)),
                rownames = FALSE, options = list(dom = 'ltipr'))
      
      
    }else if(input$indicador_enir_em ==  "PORCENTAJE DE INFORMANTES QUE ENVÍAN REMESAS SEGÚN COMUNIDAD DE ORIGEN Y SEXO"
             & (input$comunidad_em %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
      
      
      datatable(enir_em() %>% 
                  arrange(comunidad) %>%
                  transmute("Comunidad" = comunidad,
                            "Sexo" = sexo,
                            "Valor (%)" = round(valor, 2)),
                rownames = FALSE, options = list(dom = 'ltipr'))
      
      
    } else if(input$indicador_enir_em ==  "PORCENTAJE DE INFORMANTES QUE ENVÍAN REMESAS SEGÚN COMUNIDAD DE ORIGEN Y SEXO"
              & (input$comunidad_em =="TODAS")){
      
      
      datatable(enir_em_todo() %>% 
                  arrange(comunidad) %>%
                  transmute("Comunidad" = comunidad,
                            "Sexo" = sexo,
                            "Valor (%)" = round(valor, 2)),
                rownames = FALSE, options = list(dom = 'ltipr'))
    }
    
    
  })   
  
  
  output$dl_tabla_resultado_enir_em <- downloadHandler(
    
    
    filename = function() {
      paste0("resultados- ", input$indicador_enir_em,".csv", sep = "")
    },
    content = function(file) {
      
      
      if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR LOS MOTIVOS DE SU ÚLTIMA MIGRACIÓN SEGÚN COMUNIDAD DE ORIGEN"
         & (input$comunidad_em %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
        
        
        write.csv(enir_em() %>% 
                    arrange(comunidad) %>%
                    transmute("Comunidad" = comunidad,
                              "Motivo" = motivos,
                              "Valor (%)" = round(valor, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1") 
        
        
        
      } else if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR LOS MOTIVOS DE SU ÚLTIMA MIGRACIÓN SEGÚN COMUNIDAD DE ORIGEN"
                & (input$comunidad_em =="TODAS")){
        
        write.csv(enir_em_todo() %>% 
                    arrange(comunidad) %>%
                    transmute("Comunidad" = comunidad,
                              "Motivo" = motivos,
                              "Valor (%)" = round(valor, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1") 
        
      } else if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL PRINCIPAL MOTIVO POR EL QUE ELIGIÓ URUGUAY COMO DESTINO DE SU ÚLTIMA MIGRACIÓN SEGÚN COMUNIDAD DE ORIGEN"
                & (input$comunidad_em %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
        
        write.csv(enir_em() %>% 
                    arrange(comunidad) %>%
                    transmute("Comunidad" = comunidad,
                              "Motivo" = miguy,
                              "Valor (%)" = round(valor, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1") 
        
        
      } else if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL PRINCIPAL MOTIVO POR EL QUE ELIGIÓ URUGUAY COMO DESTINO DE SU ÚLTIMA MIGRACIÓN SEGÚN COMUNIDAD DE ORIGEN"
                & (input$comunidad_em =="TODAS")){
        
        write.csv(enir_em_todo() %>% 
                    arrange(comunidad) %>%
                    transmute("Comunidad" = comunidad,
                              "Motivo" = miguy,
                              "Valor (%)" = round(valor, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
        
      } else if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL LUGAR DONDE LE GUSTARÍA ESTAR VIVIENDO EN TRES AÑOS SEGÚN COMUNIDAD DE ORIGEN"
                & (input$comunidad_em %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
        
        
        write.csv(enir_em() %>% 
                    arrange(comunidad) %>%
                    transmute("Comunidad" = comunidad,
                              "Lugar" = futuro,
                              "Valor (%)" = round(valor, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
        
      } else if(input$indicador_enir_em ==  "DISTRIBUCIÓN RELATIVA DE LOS INFORMANTES POR EL LUGAR DONDE LE GUSTARÍA ESTAR VIVIENDO EN TRES AÑOS SEGÚN COMUNIDAD DE ORIGEN"
                & (input$comunidad_em =="TODAS")){
        
        write.csv(enir_em_todo() %>% 
                    arrange(comunidad) %>%
                    transmute("Comunidad" = comunidad,
                              "Lugar" = futuro,
                              "Valor (%)" = round(valor, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
        
      }else if(input$indicador_enir_em ==  "PORCENTAJE DE INFORMANTES QUE ENVÍAN REMESAS SEGÚN COMUNIDAD DE ORIGEN Y SEXO"
               & (input$comunidad_em %in% c("CUBA","REPÚBLICA DOMINICANA","PERÚ","VENEZUELA"))) {
        
        
        write.csv(enir_em() %>% 
                    arrange(comunidad) %>%
                    transmute("Comunidad" = comunidad,
                              "Sexo" = sexo,
                              "Valor (%)" = round(valor, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1")
        
        
      } else if(input$indicador_enir_em ==  "PORCENTAJE DE INFORMANTES QUE ENVÍAN REMESAS SEGÚN COMUNIDAD DE ORIGEN Y SEXO"
                & (input$comunidad_em =="TODAS")){
        
        
        write.csv(enir_em_todo() %>% 
                    arrange(comunidad) %>%
                    transmute("Comunidad" = comunidad,
                              "Sexo" = sexo,
                              "Valor (%)" = round(valor, 2)),
                  file, row.names = FALSE, fileEncoding = "latin1")
      }
      
    })
  
  
 
  
  ##RRAA
  
  
  
  output$dimensiones_rraa <- renderUI({
    
    
    selectInput("dimension_rraa",
                "Seleccione la dimensión:",
                choices = c("Documentación","Educación","Estadísticas vitales",
                            "Ingresos y egresos al territorio nacional",
                            "Inclusión socio laboral","Inclusión social"),selected = "Documentación")
    
    
  })
  
  
  
output$subdimensiones_rraa <- renderUI({
    
  req(input$dimension_rraa)
  
 selectInput("subdimensiones_rraa","Seleccione la subdimensión:",
choices = unique(metadata %>% 
filter(Dimensión == input$dimension_rraa) %>%pull(Subdimensión)))
                   
 
  })
  
  
output$indicadores_rraa <- renderUI({
  
  req(input$subdimensiones_rraa)
    
             selectInput("indicador_rraa","Seleccione el indicador:",
                         choices = unique(metadata %>% 
               filter(Subdimensión == input$subdimensiones_rraa) %>% pull(Nombre)))

    
  
}) 
  
output$anio_rraa <- renderUI({
  
  corte1 <- rlang::sym(rlang::as_string(rraa_filt()$corte1[1]))
  corte2 <- rlang::sym(rlang::as_string(rraa_filt()$corte2[1]))
  corte3 <- rlang::sym(rlang::as_string(rraa_filt()$corte3[1]))
  
  if(corte3 == "movimiento_ineg") {
               
selectInput("anio_rraa","Año:",
choices = c(as.character(rev(as.numeric(unique(rraa %>% 
      filter(nomindicador == input$indicador_rraa) %>% pull(anio))))))

)
  }else if(corte3 != "movimiento_ineg") {
    
    
    selectInput("anio_rraa","Año:",
                choices = c("Evolución",as.character(rev(as.numeric(unique(rraa %>% 
    filter(nomindicador == input$indicador_rraa) %>% pull(anio)))))),selected = "Evolución")  
    
  }
  
}) 





rraa_filt <- reactive({
  
  req(input$indicador_rraa)

  rraa <- rraa %>%
    filter(nomindicador == input$indicador_rraa)


})



output$selec_pais <- renderUI({
  
  corte1 <- rlang::sym(rlang::as_string(rraa_filt()$corte1[1]))
  corte2 <- rlang::sym(rlang::as_string(rraa_filt()$corte2[1]))
  corte3 <- rlang::sym(rlang::as_string(rraa_filt()$corte3[1]))
  
  if(corte1 == "pais" & input$anio_rraa !="Evolución" &corte3 != "movimiento_ineg") {
  
pickerInput("pais", "País:", 
            choices = levels(droplevels(rraa %>% 
              filter(nomindicador == input$indicador_rraa
                     & valor >0 &is.na(pais)==F)%>% filter(anio==input$anio_rraa) %>%select(pais)%>% dplyr::distinct(pais)%>%
              pull(pais))),
            selected = levels(droplevels(rraa %>% 
                                           filter(nomindicador == input$indicador_rraa
                                                  & valor >0 &is.na(pais)==F)%>% filter(anio==input$anio_rraa) %>%select(pais)%>% dplyr::distinct(pais)%>%
                                           pull(pais))),
           options = list(`actions-box` = TRUE,
           `deselect-all-text` = "Borrar todo",
           `select-all-text` = "Seleccionar todo",
           `none-selected-text` = "País",
            `live-search`=TRUE),multiple = T)
    
    
  
  } else if(corte2 == "pais" & corte1 == "pais" & input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg") {
  
    
    pickerInput("pais", "País:",
                choices = levels(droplevels(rraa %>%
                                              filter(nomindicador == input$indicador_rraa
                                                     & valor >0 &is.na(pais)==F)%>% select(pais)%>% dplyr::distinct(pais)%>%
                                              pull(pais))),
                selected = levels(droplevels(rraa %>%
                                               filter(nomindicador == input$indicador_rraa
                                                      & valor >0 &is.na(pais)==F)%>% select(pais)%>% dplyr::distinct(pais)%>%
                                               pull(pais))),
                options = list(`actions-box` = TRUE,
                               `deselect-all-text` = "Borrar todo",
                               `select-all-text` = "Seleccionar todo",
                               `none-selected-text` = "País",
                               `live-search`=TRUE),multiple = T)
    
    
    
    
  } else if(corte2 != "pais" & corte1 == "pais" & input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg") {
  
    pickerInput("pais", "País:", 
                choices = levels(droplevels(rraa %>% 
                                              filter(nomindicador == input$indicador_rraa
                                                     & valor >0 &is.na(pais)==F)%>% select(pais)%>% dplyr::distinct(pais)%>%
                                              pull(pais))),
                selected = "Argentina",multiple = F)

  
  }else if(corte1 != "depto" & corte2 == "depto" & input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg") {
    
    pickerInput("depto", "Departamento:", 
                choices = levels(droplevels(rraa %>% 
                                              filter(nomindicador == input$indicador_rraa
                                                     & valor >0 &is.na(depto)==F)%>% select(depto)%>% dplyr::distinct(depto)%>%
                                              pull(depto))),
                selected = "Artigas",multiple = F)
    
    
  
  }else if(corte1 != "depto" & corte2 == "depto" & input$anio_rraa !="Evolución" & corte3 != "movimiento_ineg") {
    
    pickerInput("depto", "Departamento:", 
                choices = levels(droplevels(rraa %>% 
                                              filter(nomindicador == input$indicador_rraa
                                                     & valor >0 &is.na(depto)==F)%>% select(depto)%>% dplyr::distinct(depto)%>%
                                              pull(depto))),
                selected = levels(droplevels(rraa %>% 
                                               filter(nomindicador == input$indicador_rraa
                                                      & valor >0 &is.na(depto)==F)%>% select(depto)%>% dplyr::distinct(depto)%>%
                                               pull(depto))),
                options = list(`actions-box` = TRUE,
                               `deselect-all-text` = "Borrar todo",
                               `select-all-text` = "Seleccionar todo",
                               `none-selected-text` = "País",
                               `live-search`=TRUE),multiple = T)
    
    
    
  }
  else if(corte2 == "depto" & corte1 == "depto" & input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg" ) {
  
  
    pickerInput("depto", "Departamento:",
                choices = levels(droplevels(rraa %>%
                                              filter(nomindicador == input$indicador_rraa
                                                     & valor >0 &is.na(depto)==F)%>% select(depto)%>% dplyr::distinct(depto)%>%
                                              pull(depto))),
                selected = levels(droplevels(rraa %>%
                                               filter(nomindicador == input$indicador_rraa
                                                      & valor >0 &is.na(depto)==F)%>% select(depto)%>% dplyr::distinct(depto)%>%
                                               pull(depto))),
                options = list(`actions-box` = TRUE,
                               `deselect-all-text` = "Borrar todo",
                               `select-all-text` = "Seleccionar todo",
                               `none-selected-text` = "Departamento",
                               `live-search`=TRUE),multiple = T)
    

    
  
  }else if(corte1 == "pais" & input$anio_rraa !="Evolución" &corte3 == "movimiento_ineg") {
    
    pickerInput("pais", "País:", 
                choices = levels(droplevels(rraa %>% 
                                              filter(nomindicador == input$indicador_rraa
                                                     & valor >0 &is.na(pais)==F)%>% filter(anio==input$anio_rraa) %>% 
                                              select(pais)%>% dplyr::distinct(pais)%>%
                                              pull(pais))),selected = c("Argentina"),multiple = F)
    
    
    
  }else if(corte1 != "depto" & corte1 != "pais") {
    
    
    
  }
}) 






output$title_rraa <- renderUI({
  
  helpText(HTML(paste(input$indicador_rraa,"-",input$anio_rraa)))
  
})
  

output$fuente_rraa <- renderUI({
  
  fuente = unique(metadata %>% 
                    filter(Nombre == input$indicador_rraa) %>%pull(Fuente))
  
  helpText(HTML(paste("Fuente: ",fuente)))
  
})


output$nota_met <- renderUI({
  

  
  nota = unique(metadata %>% 
                    filter(Nombre == input$indicador_rraa) %>%pull(Nota.metodológica))
  
  helpText(HTML(paste("Nota metodológica: ",nota)))

  
  
})




output$barPlot_rraa <- renderPlot({


corte1 <- rlang::sym(rlang::as_string(rraa_filt()$corte1[1]))
corte2 <- rlang::sym(rlang::as_string(rraa_filt()$corte2[1]))
corte3 <- rlang::sym(rlang::as_string(rraa_filt()$corte3[1]))


req(input$indicador_rraa)
req(input$anio_rraa)

if(corte1 == corte2 & corte1 != "pais" & corte2 != "pais" & corte1 != "depto" & corte2 != "depto" 
   & input$anio_rraa !="Evolución" & corte3 != "movimiento_ineg" & input$indicador_rraa%in%excep==F & 
   input$indicador_rraa%in%excep2==F& 
   input$indicador_rraa%in%excep3==F& 
   input$indicador_rraa%in%excep4==F) {
  
  
  rraa = rraa_filt() %>%
    filter(anio == input$anio_rraa & !!corte1 !="Total" )%>%
    ggplot(aes_string(corte1 , "valor",fill=corte1)) +
    geom_bar(stat = "identity", position = "dodge", width=0.3)+
    #geom_text(aes(label = valor))+
    # geom_text(aes(label=valor), size=4,colour="black",position = position_stack(vjust = 0.5)) +

    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                       expand = expansion(mult = c(0, .2))) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size=13),
          strip.text.x = element_text(size = 13),
          legend.title = element_blank(),legend.position = "bottom",
          legend.justification = "left",legend.direction = "vertical",
          legend.key.size = unit(0.5,"cm"))+
    labs(x = "",
         y = "")
  
} else if(corte1 != corte2 & corte1 != "pais" & corte2 != "pais" & corte1 != "depto" & corte2 != "depto" & 
          input$anio_rraa !="Evolución" & corte3 != "movimiento_ineg" & input$indicador_rraa%in%excep==F & 
          input$indicador_rraa%in%excep2==F& 
          input$indicador_rraa%in%excep3==F& 
          input$indicador_rraa%in%excep4==F) {

  
  rraa = rraa_filt() %>%
    filter(anio == input$anio_rraa & !!corte2 !="Total"& !!corte1 !="Total" & is.na(corte1)!=T) %>%
    ggplot(aes_string(corte2 , "valor",fill=corte2)) +
    geom_bar(stat = "identity", position = "dodge", width=0.5)+
    #geom_text(aes(label = valor), size = 3)+
    # geom_text(aes(label=valor), size=4,colour="black",position = position_stack(vjust = 0.5)) +
    
    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                       expand = expansion(mult = c(0, .2))) +
    facet_wrap(vars(!!!corte1), ncol = 2, scales = "free_y") + 
    # force_panelsizes(rows = unit(12 , "cm"), 
    #                  cols = unit(8, "cm"), 
    #                  FALSE)+
    theme(axis.text.x = element_text(size=13,angle = 90,hjust=0.95,vjust=0.2),
          
          axis.text.y = element_text(size=13),
          strip.text.x = element_text(size = 13),
          legend.title = element_blank(),legend.position = "bottom",
          legend.justification = "left",legend.direction = "vertical",
          legend.key.size = unit(0.5,"cm"))+
    labs(x = "",
         y = "") 


} else if(corte1 == "pais" & corte2 != "pais" & corte1 != "depto" & corte2 != "depto" & 
          input$anio_rraa !="Evolución" & corte3 != "movimiento_ineg" & input$indicador_rraa%in%excep==F & 
          input$indicador_rraa%in%excep2==F& 
          input$indicador_rraa%in%excep3==F& 
          input$indicador_rraa%in%excep4==F) {

  
  rraa = rraa_filt() %>%
    filter(anio == input$anio_rraa & !!corte2 !="Total")%>%
    filter(pais %in% input$pais)%>%
    ggplot(aes_string(corte1 , "valor",fill=corte2)) +
    geom_bar(position="stack", stat="identity")+
    # geom_text(aes(label=valor), size=4,colour="black",position = position_stack(vjust = 0.5)) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE))+
    # scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE),
    #                    expand = expansion(mult = .2)) +
    
    theme(axis.text.x = element_text(size=13,angle = 90,hjust=0.95,vjust=0.2),
          axis.text.y = element_text(size=13),
          strip.text.x = element_text(size = 13),
          legend.title = element_blank(),legend.position = "bottom",
          legend.justification = "left",legend.direction = "vertical",
          legend.key.size = unit(0.5,"cm"))+
    labs(x = "",
         y = "") 
  
  
} else if(corte1 == "pais" & corte2 == "pais" & corte1 != "depto" & corte2 != "depto" & 
          input$anio_rraa !="Evolución" & corte3 != "movimiento_ineg"& input$indicador_rraa%in%excep==F & 
          input$indicador_rraa%in%excep2==F& 
          input$indicador_rraa%in%excep3==F& 
          input$indicador_rraa%in%excep4==F) {
  
  rraa = rraa_filt() %>%
    filter(anio == input$anio_rraa & !!corte1 !="Total" )%>%
    filter(pais %in% input$pais)%>%
    ggplot(aes_string(paste0("reorder(",corte1,", -valor)") , "valor")) +
    geom_bar(stat = "identity", position = "dodge",fill="#F8766D")+
    # geom_text(aes(label=valor), size=4,colour="black",position = position_stack(vjust = 0.5))+
    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                       expand = expansion(mult = c(0, .2))) +
    
    theme(axis.text.x = element_text(size=13,angle = 90,hjust=0.95,vjust=0.2),
          axis.text.y = element_text(size=13),
          strip.text.x = element_text(size = 13),
          legend.title = element_blank(),legend.position = "bottom",
          legend.justification = "left",legend.direction = "vertical",
          legend.key.size = unit(0.5,"cm"))+
    labs(x = "",
         y = "")
  
} else if(corte1 != "depto" & corte2 == "depto" & corte1 != "pais" & corte2 != "pais" & 
          input$anio_rraa !="Evolución" & corte3 != "movimiento_ineg"& input$indicador_rraa%in%excep==F & 
          input$indicador_rraa%in%excep2==F& 
          input$indicador_rraa%in%excep3==F& 
          input$indicador_rraa%in%excep4==F) {
  
  
  rraa = rraa_filt() %>%
    filter(anio == input$anio_rraa & !!corte2 !="Total" & !!corte1 !="Total")%>%
    filter(depto %in% input$depto)%>%
    ggplot(aes_string(corte2 , "valor",fill=corte1)) +
    geom_bar(position="stack", stat="identity")+
    # geom_text(aes(label=valor), size=4,colour="black",position = position_stack(vjust = 0.5)) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE))+
    # scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE),
    #                    expand = expansion(mult = .2)) +
    
    theme(axis.text.x = element_text(size=13,angle = 90,hjust=0.95,vjust=0.2),
          axis.text.y = element_text(size=13),
          strip.text.x = element_text(size = 13),
          legend.title = element_blank(),legend.position = "bottom",
          legend.justification = "left",legend.direction = "vertical",
          legend.key.size = unit(0.5,"cm"))+
    labs(x = "",
         y = "") 
  
  
} else if(corte1 == "depto" & corte2 == "depto" & corte1 != "pais" & corte2 != "pais" & 
          input$anio_rraa !="Evolución" & corte3 != "movimiento_ineg" & input$indicador_rraa%in%excep==F & 
          input$indicador_rraa%in%excep2==F& 
          input$indicador_rraa%in%excep3==F& 
          input$indicador_rraa%in%excep4==F) {
  
 
  rraa = rraa_filt() %>%
    filter(anio == input$anio_rraa & !!corte1 !="Total" )%>%
    filter(depto %in% input$depto)%>%
    ggplot(aes_string(paste0("reorder(",corte1,", -valor)") , "valor")) +
    geom_bar(stat = "identity", position = "dodge",fill="#F8766D")+
    # geom_text(aes(label=valor), size=4,colour="black",position = position_stack(vjust = 0.5))+
    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                       expand = expansion(mult = c(0, .2))) +
    
    theme(axis.text.x = element_text(size=13,angle = 90,hjust=0.95,vjust=0.2),
          axis.text.y = element_text(size=13),
          strip.text.x = element_text(size = 13),
          legend.title = element_blank(),legend.position = "bottom",
          legend.justification = "left",legend.direction = "vertical",
          legend.key.size = unit(0.5,"cm"))+
    labs(x = "",
         y = "")
  
  
  
  
} else if(corte1 == corte2 & corte1 != "pais" & corte2 != "pais" & corte1 != "depto" & corte2 != "depto" & 
          input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg" & input$indicador_rraa%in%excep==F & 
          input$indicador_rraa%in%excep2==F& 
          input$indicador_rraa%in%excep3==F& 
          input$indicador_rraa%in%excep4==F) {
  
  
      rraa = rraa_filt() %>%
        filter(!!corte1 !="Total" )%>%
    ggplot(aes_string("anio", "valor", color = corte1, fill = corte1, group = corte1)) +
   geom_bar(position="stack", stat="identity")+
    # geom_text(aes(label=valor), size=4,colour="black",position = position_stack(vjust = 0.5)) +

    scale_y_continuous(
                       labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +

    labs(x = "",
         y = "")+
    
    theme(axis.text.x = element_text(size=13,angle = 90),
          legend.title = element_blank(),legend.position = "bottom",
          legend.justification = "left",legend.direction = "vertical",
          legend.key.size = unit(0.5,"cm"))
      
####pruebo lo de labels
      
} else if(corte1 != corte2 & corte1 != "pais" & corte2 != "pais" & corte1 != "depto" & corte2 != "depto" & 
          input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg" & input$indicador_rraa%in%excep==F & 
          input$indicador_rraa%in%excep2==F& 
          input$indicador_rraa%in%excep3==F& 
          input$indicador_rraa%in%excep4==F) {
  
  
  rraa = rraa_filt() %>%
    filter(!!corte2 !="Total" & !!corte1 !="Total")%>%
    ggplot(aes_string("anio", "valor", color = corte2, fill = corte2, group = corte2)) +
    geom_bar(position="stack", stat="identity")+
    # geom_text(aes(label = ifelse(corte2 %in% c("tramos","tramos_r","tramos_d","tramos_ineg","identidad_gen","tramos_mides","identidad_gen_ce",
    #                                            "tramos_mides_ce","tramos_mig","tramos_inau","tramos_trabajo","tramos_refugiados","tipo_edu",
    #                                            "cat_escuela","nivel_soc","grado","tramos_ced","tramos_egreso"), "", valor)), size=4,colour="black",position = position_stack(vjust = 0.5)) +

    scale_y_continuous(
                       labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +

    labs(x = "",
         y = "")+
    facet_wrap(vars(!!!corte1),ncol = 2, scales = "free_y") +
    
    theme(axis.text.x = element_text(size=13,angle = 90),
          legend.title = element_blank(),legend.position = "bottom",
          legend.justification = "left",legend.direction = "vertical",
          legend.key.size = unit(0.5,"cm"))
  
  
} else if(corte1 == "pais" & corte2 != "pais" & corte1 != "depto" & corte2 != "depto" & 
          input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg" & input$indicador_rraa%in%excep==F & 
          input$indicador_rraa%in%excep2==F& 
          input$indicador_rraa%in%excep3==F& 
          input$indicador_rraa%in%excep4==F) {
  
  
  rraa = rraa_filt() %>%
    filter(!!corte2 !="Total" & !!corte1 !="Total")%>%
    filter(pais %in% input$pais)%>%
    mutate(pais=as.character(pais))%>%
    ggplot(aes_string("anio", "valor", color = corte2, fill = corte2, group = corte2)) +
    geom_bar(position="stack", stat="identity")+
    # geom_text(aes(label=valor), size=4,colour="black",position = position_stack(vjust = 0.5)) +
    
    scale_y_continuous(
      labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    
    labs(x = "",
         y = "")+
    #facet_wrap(~ get(corte2),ncol = 2) +
    
    theme(axis.text.x = element_text(size=13,angle = 90),
          legend.title = element_blank(),legend.position = "bottom",
          legend.justification = "left",legend.direction = "vertical",
          legend.key.size = unit(0.5,"cm"))+
    guides(color=guide_legend(ncol=4))
  
  
} else if(corte1 == "pais" & corte2 == "pais" & corte1 != "depto" & corte2 != "depto" & 
          input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg" & input$indicador_rraa%in%excep==F & 
          input$indicador_rraa%in%excep2==F& 
          input$indicador_rraa%in%excep3==F& 
          input$indicador_rraa%in%excep4==F) {
  

  
  rraa = rraa_filt() %>%
    filter(!!corte1 !="Total" )%>%
    filter(pais %in% input$pais)%>%
    mutate(pais=as.character(pais))%>%
    ggplot(aes_string("anio", "valor", color = corte1, fill = corte1, group = corte1)) +
    geom_bar(position="stack", stat="identity")+
    # geom_text(aes(label=valor), size=4,colour="black",position = position_stack(vjust = 0.5)) +
    
    scale_y_continuous(
      labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    
    labs(x = "",
         y = "")+
    
    theme(axis.text.x = element_text(size=13,angle = 90),
          legend.title = element_blank(),legend.position = "bottom",
          legend.justification = "left",legend.direction = "vertical",
          legend.key.size = unit(0.5,"cm"))+
          guides(color=guide_legend(ncol=4))
  
  
  
  
} else if(corte1 != "depto" & corte2 == "depto" & corte1 != "pais" & corte2 != "pais" & 
          input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg" & input$indicador_rraa%in%excep==F & 
          input$indicador_rraa%in%excep2==F& 
          input$indicador_rraa%in%excep3==F& 
          input$indicador_rraa%in%excep4==F) {
  
  
  rraa = rraa_filt() %>%
    filter(!!corte2 !="Total" & !!corte1 !="Total")%>%
    filter(depto %in% input$depto)%>%
    mutate(depto=as.character(depto))%>%
    ggplot(aes_string("anio", "valor", color = corte1, fill = corte1, group = corte1)) +
    geom_bar(position="stack", stat="identity")+
    # geom_text(aes(label=valor), size=4,colour="black",position = position_stack(vjust = 0.5)) +
    
    scale_y_continuous(
      labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    
    labs(x = "",
         y = "")+
    # facet_wrap(~ get(corte2),ncol = 2) +
    
    theme(axis.text.x = element_text(size=13,angle = 90),
          legend.title = element_blank(),legend.position = "bottom",
          legend.justification = "left",legend.direction = "vertical",
          legend.key.size = unit(0.5,"cm"))+
    guides(color=guide_legend(ncol=4))
  
  
} else if(corte1 == "depto" & corte2 == "depto" & corte1 != "pais" & corte2 != "pais" & 
          input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg" & input$indicador_rraa%in%excep==F & 
          input$indicador_rraa%in%excep2==F& 
          input$indicador_rraa%in%excep3==F& 
          input$indicador_rraa%in%excep4==F) {
  
  rraa = rraa_filt() %>%
    filter(!!corte1 !="Total" & !!corte2 !="Total")%>%
    filter(depto %in% input$depto)%>%
    mutate(depto=as.character(depto))%>%
    ggplot(aes_string("anio", "valor", color = corte1, fill = corte1, group = corte1)) +
    geom_bar(position="stack", stat="identity")+
    # geom_text(aes(label=valor), size=4,colour="black",position = position_stack(vjust = 0.5)) +
    
    scale_y_continuous(
      labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    
    labs(x = "",
         y = "")+
    
    theme(axis.text.x = element_text(size=13,angle = 90),
          legend.title = element_blank(),legend.position = "bottom",
          legend.justification = "left",legend.direction = "vertical",
          legend.key.size = unit(0.5,"cm"))+
    guides(color=guide_legend(ncol=4))
  
}
 else if( corte3 == "movimiento_ineg" & corte1 != "pais" & 
          input$anio_rraa !="Evolución" & input$indicador_rraa%in%excep==F & 
          input$indicador_rraa%in%excep2==F& 
          input$indicador_rraa%in%excep3==F& 
          input$indicador_rraa%in%excep4==F) {
   

#   
#   
#   #corte1=lugar_nac
#   #corte2=punto_control
#   #corte3=movimiento_ineg
#   
  rraa = rraa_filt() %>%
    filter(anio == input$anio_rraa & !!corte1 !="Total" & !!corte2 !="Total" & is.na(corte1)!=T)%>%
    ggplot(aes_string(corte2 , "valor", fill=corte3)) +
    geom_bar(stat = "identity", position = "dodge", width=0.5)+
    facet_wrap(vars(!!!corte1), ncol = 1, scales = "free_y") +
    # geom_text(aes(label=valor), size=4, angle=90, position = position_dodge(width = 0.5), vjust = "inward",
    #           hjust="inward") +
    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                       expand = expansion(mult = c(0, .2))) +
     theme(axis.text.x = element_text(size=13,angle = 90,hjust=0.95,vjust=0.2),
          axis.text.y = element_text(size=13),
          strip.text.x = element_text(size = 13),
          legend.title = element_blank(),legend.position = "bottom",
          legend.justification = "left",legend.direction = "vertical",
          legend.key.size = unit(0.5,"cm"))+
    labs(x = "",
         y = "")



 } else if( corte3 == "movimiento_ineg" & corte1 == "pais" & 
            input$anio_rraa !="Evolución" & input$indicador_rraa%in%excep==F & 
            input$indicador_rraa%in%excep2==F& 
            input$indicador_rraa%in%excep3==F& 
            input$indicador_rraa%in%excep4==F) {
   
   
   rraa = rraa_filt() %>%
     filter(anio == input$anio_rraa & !!corte1 !="Total" & !!corte2 !="Total" & is.na(corte1)!=T)%>%
     filter(pais %in% input$pais)%>%
     ggplot(aes_string(corte2 , "valor", fill=corte3)) +
     geom_bar(stat = "identity", position = "dodge", width=0.5)+
     
     facet_wrap(vars(!!!corte1), ncol = 1, scales = "free_y") +
     # geom_text(aes(label=valor), size=4, angle=90, position = position_dodge(width = 0.5), vjust = "inward",
     #           hjust="inward") +

     scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                        expand = expansion(mult = c(0, .2))) +
     
     guides(color=guide_legend(ncol=4))+
     
  theme(axis.text.x = element_text(size=13,angle = 90,hjust=0.95,vjust=0.2),
           axis.text.y = element_text(size=13),
           strip.text.x = element_text(size = 13),
           legend.title = element_blank(),legend.position = "bottom",
           legend.justification = "left",legend.direction = "vertical",
           legend.key.size = unit(0.5,"cm"))+
     labs(x = "",
          y = "")
   
   
   
 }else if( input$indicador_rraa%in%excep==T & 
           input$anio_rraa !="Evolución" & 
           input$indicador_rraa%in%excep2==F& 
           input$indicador_rraa%in%excep3==F& 
           input$indicador_rraa%in%excep4==F) {
   
   rraa = rraa_filt() %>%
     filter(anio == input$anio_rraa & !!corte1 !="Total" )%>%
     ggplot(aes_string(corte1 , "valor",fill=corte1)) +
     geom_bar(stat = "identity", position = "dodge", width=0.3)+
     #geom_text(aes(label = valor))+
     # geom_text(aes(label=valor), size=4,colour="black",position = position_stack(vjust = 0.5)) +
     
     scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                        expand = expansion(mult = c(0, .2))) +
     guides(color=guide_legend(ncol=4))+
     theme(axis.text.x = element_blank(),
           axis.text.y = element_text(size=13),
           strip.text.x = element_text(size = 13),
           legend.title = element_blank(),legend.position = "bottom",
           legend.justification = "left",legend.direction = "vertical",
           legend.key.size = unit(0.5,"cm"))+
     labs(x = "",
          y = "")
   
   
   
   
   
 }else if( input$indicador_rraa%in%excep==T & 
           input$anio_rraa =="Evolución"& 
           input$indicador_rraa%in%excep2==F& 
           input$indicador_rraa%in%excep3==F& 
           input$indicador_rraa%in%excep4==F &
           corte1 == "depto") {
   
   rraa = rraa_filt() %>%
     filter(!!corte1 !="Total")%>%
     filter(depto%in%input$depto)%>%
     ggplot(aes_string("anio", "valor", color = corte1, fill = corte1, group = corte1)) +
     geom_bar(stat = "identity", position = "dodge")+
     # geom_text(aes(label=valor), size=4,colour="black",position = position_stack(vjust = 0.5)) +
     
     scale_y_continuous(
       labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
     
     labs(x = "",
          y = "")+
     guides(color=guide_legend(ncol=4))+
     theme(axis.text.x = element_text(size=13,angle = 90),
           legend.title = element_blank(),legend.position = "bottom",
           legend.justification = "left",legend.direction = "vertical",
           legend.key.size = unit(0.5,"cm"))
   
   
   
   
   
 }else if( input$indicador_rraa%in%excep==T & 
           input$anio_rraa =="Evolución"& 
           input$indicador_rraa%in%excep2==F& 
           input$indicador_rraa%in%excep3==F& 
           input$indicador_rraa%in%excep4==F &
           corte1 != "depto") {
   
   rraa = rraa_filt() %>%
     filter(!!corte1 !="Total")%>%
     ggplot(aes_string("anio", "valor", color = corte1, fill = corte1, group = corte1)) +
     geom_bar(stat = "identity", position = "dodge")+
     # geom_text(aes(label=valor), size=4,colour="black",position = position_stack(vjust = 0.5)) +
     
     scale_y_continuous(
       labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
     
     labs(x = "",
          y = "")+
     guides(color=guide_legend(ncol=4))+
     theme(axis.text.x = element_text(size=13,angle = 90),
           legend.title = element_blank(),legend.position = "bottom",
           legend.justification = "left",legend.direction = "vertical",
           legend.key.size = unit(0.5,"cm"))
   
   
   
   
   
 }else if( input$indicador_rraa%in%excep==F & 
           input$anio_rraa !="Evolución" & 
           input$indicador_rraa%in%excep2==T& 
           input$indicador_rraa%in%excep3==F& 
           input$indicador_rraa%in%excep4==F) {
   
   rraa = rraa_filt() %>%
     filter(anio == input$anio_rraa & !!corte1 !="Total" )%>%
     ggplot(aes_string(corte1 , "valor",fill=corte1)) +
     geom_bar(stat = "identity", position = "dodge", width=0.3)+
     #geom_text(aes(label = valor))+
     # geom_text(aes(label=valor), size=4,colour="black",position = position_stack(vjust = 0.5)) +
     
     scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                        expand = expansion(mult = c(0, .2))) +
     guides(color=guide_legend(ncol=4))+
     theme(axis.text.x = element_blank(),
           axis.text.y = element_text(size=13),
           strip.text.x = element_text(size = 13),
           legend.title = element_blank(),legend.position = "bottom",
           legend.justification = "left",legend.direction = "vertical",
           legend.key.size = unit(0.5,"cm"))+
     labs(x = "",
          y = "")
   
   
   
   
   
 }else if( input$indicador_rraa%in%excep==F & 
           input$anio_rraa =="Evolución"& 
           input$indicador_rraa%in%excep2==T& 
           input$indicador_rraa%in%excep3==F& 
           input$indicador_rraa%in%excep4==F) {
   
   rraa = rraa_filt() %>%
     filter(!!corte1 !="Total" )%>%
     ggplot(aes_string("anio", "valor", color = corte1, fill = corte1, group = corte1)) +
     geom_bar(stat = "identity", position = "dodge")+
     # geom_text(aes(label=valor), size=4,colour="black",position = position_stack(vjust = 0.5)) +
     
     scale_y_continuous(
       labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
     facet_wrap(vars(!!!corte1), ncol = 1, scales = "free_y") +
     guides(color=guide_legend(ncol=4))+
     
     labs(x = "",
          y = "")+
     
     theme(axis.text.x = element_text(size=13,angle = 90),
           legend.title = element_blank(),legend.position = "bottom",
           legend.justification = "left",legend.direction = "vertical",
           legend.key.size = unit(0.5,"cm"))
   
   
   
   
   
 }else if( input$indicador_rraa%in%excep==F & 
           input$anio_rraa =="Evolución"& 
           input$indicador_rraa%in%excep2==F& 
           input$indicador_rraa%in%excep3==T& 
           input$indicador_rraa%in%excep4==F) {
   
   rraa = rraa_filt() %>%
     filter(!!corte1 !="Total" & !!corte2 !="Total" )%>%
     ggplot(aes_string("anio", "valor", color = corte2, fill = corte2, group = corte2)) +
     geom_bar(stat = "identity", position = "dodge")+
     # geom_text(aes(label=valor), size=4,colour="black",position = position_stack(vjust = 0.5)) +
     
     scale_y_continuous(
       labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
     facet_wrap(vars(!!!corte1), ncol = 1, scales = "free_y") +
     guides(color=guide_legend(ncol=4))+
     
     labs(x = "",
          y = "")+
     
     theme(axis.text.x = element_text(size=13,angle = 90),
           legend.title = element_blank(),legend.position = "bottom",
           legend.justification = "left",legend.direction = "vertical",
           legend.key.size = unit(0.5,"cm"))
   
   
   
   
   
 }else if( input$indicador_rraa%in%excep==F & 
           input$anio_rraa !="Evolución"& 
           input$indicador_rraa%in%excep2==F& 
           input$indicador_rraa%in%excep3==T& 
           input$indicador_rraa%in%excep4==F) {
   
   rraa = rraa_filt() %>%
     filter(anio == input$anio_rraa & !!corte2 !="Total"& !!corte1 !="Total" & is.na(corte1)!=T) %>%
     ggplot(aes_string(corte2 , "valor",fill=corte2)) +
     geom_bar(stat = "identity", position = "dodge", width=0.5)+
     #geom_text(aes(label = valor), size = 3)+
     # geom_text(aes(label=valor), size=4,colour="black",position = position_stack(vjust = 0.5)) +
     
     scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                        expand = expansion(mult = c(0, .2))) +
     facet_wrap(vars(!!!corte1), ncol = 2, scales = "free_y") + 
     # force_panelsizes(rows = unit(12 , "cm"), 
     #                  cols = unit(8, "cm"), 
     #                  FALSE)+
     theme(axis.text.x = element_text(size=13,angle = 90,hjust=0.95,vjust=0.2),
           
           axis.text.y = element_text(size=13),
           strip.text.x = element_text(size = 13),
           legend.title = element_blank(),legend.position = "bottom",
           legend.justification = "left",legend.direction = "vertical",
           legend.key.size = unit(0.5,"cm"))+
     labs(x = "",
          y = "") 
   
   
   
   
   
 }else if( input$indicador_rraa%in%excep==F & 
           input$anio_rraa =="Evolución"& 
           input$indicador_rraa%in%excep2==F& 
           input$indicador_rraa%in%excep3==F& 
           input$indicador_rraa%in%excep4==T) {
   
 
   
   rraa = rraa_filt() %>%
     filter(!!corte2 !="Total" & !!corte1 !="Total")%>%
     filter(pais %in% input$pais)%>%
     mutate(pais=as.character(pais))%>%
     ggplot(aes_string("anio", "valor", color = corte2, fill = corte2, group = corte2)) +
     geom_bar(position="dodge", stat="identity")+
     # geom_text(aes(label=valor), size=4,colour="black",position = position_stack(vjust = 0.5)) +
     
     scale_y_continuous(
       labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
     
     labs(x = "",
          y = "")+
     #facet_wrap(~ get(corte2),ncol = 2) +
     
     theme(axis.text.x = element_text(size=13,angle = 90),
           legend.title = element_blank(),legend.position = "bottom",
           legend.justification = "left",legend.direction = "vertical",
           legend.key.size = unit(0.5,"cm"))+
     guides(color=guide_legend(ncol=4))
   
   
   
 }else if( input$indicador_rraa%in%excep==F & 
           input$anio_rraa !="Evolución"& 
           input$indicador_rraa%in%excep2==F& 
           input$indicador_rraa%in%excep3==F& 
           input$indicador_rraa%in%excep4==T) {
   
    
   rraa = rraa_filt() %>%
     filter(anio == input$anio_rraa & !!corte2 !="Total")%>%
     filter(pais %in% input$pais)%>%
     ggplot(aes_string(corte1 , "valor",fill=corte2)) +
     geom_bar(position="dodge", stat="identity")+
     # geom_text(aes(label=valor), size=4,colour="black",position = position_stack(vjust = 0.5)) +
     scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE))+
     # scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE),
     #                    expand = expansion(mult = .2)) +
     
     theme(axis.text.x = element_text(size=13,angle = 90,hjust=0.95,vjust=0.2),
           axis.text.y = element_text(size=13),
           strip.text.x = element_text(size = 13),
           legend.title = element_blank(),legend.position = "bottom",
           legend.justification = "left",legend.direction = "vertical",
           legend.key.size = unit(0.5,"cm"))+
     labs(x = "",
          y = "") 
   
   
   
   
 }

print(rraa)
ggsave("www/rraa.png",bg = "white")


})


output$baja_barPlot_rraa <- downloadHandler(
  filename <- function() {
    paste0("Gráfico - ",input$indicador_rraa,"-",input$anio_rraa, ".png", sep = "")
  },
  
  content <- function(file) {
    file.copy("www/rraa.png", file)
  },
  contentType = "www/rraa"
  
  
)


output$tabla_resultado_rraa  <- renderDT({
  
  corte1 <- rlang::sym(rlang::as_string(rraa_filt()$corte1[1]))
  corte2 <- rlang::sym(rlang::as_string(rraa_filt()$corte2[1]))
  corte3 <- rlang::sym(rlang::as_string(rraa_filt()$corte3[1]))
  
  
  if(corte1 == corte2 & corte1 != "pais" & corte2 != "pais" & corte1 != "depto" & corte2 != "depto" 
     & input$anio_rraa !="Evolución" & corte3 != "movimiento_ineg") {
    
    
    datatable(rraa_filt() %>%
                filter(anio == input$anio_rraa& !!corte1 !="Total") %>% select(anio,!!corte1,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"
                                       ))), 
              
              rownames = FALSE, options = list(dom = 'Bfrtip',columnDefs = 
                                                 list(list(className = 'dt-center', 
                                                           targets = "_all"))))
    

    
    
  } else if(corte1 != corte2 & corte1 != "pais" & corte2 != "pais" & corte1 != "depto" & corte2 != "depto" & input$anio_rraa !="Evolución" & corte3 != "movimiento_ineg") {
    
    
    datatable(rraa_filt() %>%
                filter(anio == input$anio_rraa& !!corte1 !="Total") %>% select(anio,!!corte1,!!corte2,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              rownames = FALSE, options = list(dom = 'Bfrtip',columnDefs = 
                                                 list(list(className = 'dt-center', 
                                                           targets = "_all"))))
    
    
    
  } else if(corte1 == "pais" & corte2 != "pais" & corte1 != "depto" & corte2 != "depto" & input$anio_rraa !="Evolución" & corte3 != "movimiento_ineg") {
    
    datatable(rraa_filt() %>%
                filter(anio == input$anio_rraa & !!corte2 !="Total" & pais %in% input$pais) %>% 
                select(anio,pais,!!corte1,!!corte2,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              rownames = FALSE, options = list(dom = 'Bfrtip',columnDefs = 
                                                 list(list(className = 'dt-center', 
                                                           targets = "_all"))))
    
    
    
    
  } else if(corte1 == "pais" & corte2 == "pais" & corte1 != "depto" & corte2 != "depto" & input$anio_rraa !="Evolución" & corte3 != "movimiento_ineg") {
    
    datatable(rraa_filt() %>%
                filter(anio == input$anio_rraa & !!corte2 !="Total" & pais %in% input$pais) %>% 
                select(anio,pais,!!corte1,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              rownames = FALSE, options = list(dom = 'Bfrtip',columnDefs = 
                                                 list(list(className = 'dt-center', 
                                                           targets = "_all"))))
    
  } else if(corte1 != "depto" & corte2 == "depto" & corte1 != "pais" & corte2 != "pais" & input$anio_rraa !="Evolución" & corte3 != "movimiento_ineg") {
    
    
    datatable(rraa_filt() %>%
                filter(anio == input$anio_rraa & !!corte2 !="Total" & depto %in% input$depto) %>% 
                select(anio,depto,!!corte1,!!corte2,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              rownames = FALSE, options = list(dom = 'Bfrtip',columnDefs = 
                                                 list(list(className = 'dt-center', 
                                                           targets = "_all"))))
    
    
  } else if(corte1 == "depto" & corte2 == "depto" & corte1 != "pais" & corte2 != "pais" & input$anio_rraa !="Evolución" & corte3 != "movimiento_ineg") {
    
    datatable(rraa_filt() %>%
                filter(anio == input$anio_rraa & !!corte2 !="Total" & depto %in% input$depto) %>% 
                select(anio,depto,!!corte1,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              rownames = FALSE, options = list(dom = 'Bfrtip',columnDefs = 
                                                 list(list(className = 'dt-center', 
                                                           targets = "_all"))))
    
    
  }else if(corte1 == corte2 & corte1 != "pais" & corte2 != "pais" & corte1 != "depto" & corte2 != "depto" & input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg") {
    
    
    datatable(rraa_filt() %>%
                filter(!!corte1 !="Total") %>% select(anio,!!corte1,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              rownames = FALSE, options = list(dom = 'Bfrtip',columnDefs = 
                                                 list(list(className = 'dt-center', 
                                                           targets = "_all"))))
    
    
  } else if(corte1 != corte2 & corte1 != "pais" & corte2 != "pais" & corte1 != "depto" & corte2 != "depto" & input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg") {
    
    
    datatable(rraa_filt() %>%
                filter(!!corte1 !="Total") %>% select(anio,!!corte1,!!corte2,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              rownames = FALSE, options = list(dom = 'Bfrtip',columnDefs = 
                                                 list(list(className = 'dt-center', 
                                                           targets = "_all"))))
    
    
  } else if(corte1 == "pais" & corte2 != "pais" & corte1 != "depto" & corte2 != "depto" & input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg") {
    
    datatable(rraa_filt() %>%
                filter( !!corte2 !="Total" & pais %in% input$pais) %>% 
                select(anio,pais,!!corte1,!!corte2,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              rownames = FALSE, options = list(dom = 'Bfrtip',columnDefs = 
                                                 list(list(className = 'dt-center', 
                                                           targets = "_all"))))
    
    
    
  } else if(corte1 == "pais" & corte2 == "pais" & corte1 != "depto" & corte2 != "depto" & input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg") {
    
    datatable(rraa_filt() %>%
                filter(!!corte2 !="Total" & pais %in% input$pais) %>% 
                select(anio,pais,!!corte1,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              rownames = FALSE, options = list(dom = 'Bfrtip',columnDefs = 
                                                 list(list(className = 'dt-center', 
                                                           targets = "_all"))))
    
  } else if(corte1 != "depto" & corte2 == "depto" & corte1 != "pais" & corte2 != "pais" & input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg") {
    
    
    datatable(rraa_filt() %>%
                filter( !!corte2 !="Total" & depto %in% input$depto) %>% 
                select(anio,depto,!!corte1,!!corte2,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              rownames = FALSE, options = list(dom = 'Bfrtip',columnDefs = 
                                                 list(list(className = 'dt-center', 
                                                           targets = "_all"))))
    
    
  } else if(corte1 == "depto" & corte2 == "depto" & corte1 != "pais" & corte2 != "pais" & input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg") {
    
    datatable(rraa_filt() %>%
                filter(!!corte2 !="Total" & depto %in% input$depto) %>% 
                select(anio,depto,!!corte1,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              rownames = FALSE, options = list(dom = 'Bfrtip',columnDefs = 
                                                 list(list(className = 'dt-center', 
                                                           targets = "_all"))))
    
  }else if( corte3 == "movimiento_ineg" & corte1 != "pais" & input$anio_rraa !="Evolución") {
    
    
    datatable(rraa_filt() %>%
                filter(!!corte2 !="Total") %>% 
                select(anio,!!corte1,!!corte2,!!corte3,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              rownames = FALSE, options = list(dom = 'Bfrtip',columnDefs = 
                                                 list(list(className = 'dt-center', 
                                                           targets = "_all"))))
    
    
  }else if( corte3 == "movimiento_ineg" & corte1 == "pais" & input$anio_rraa !="Evolución") {
    
    datatable(rraa_filt() %>%
                filter(!!corte2 !="Total" & pais %in% input$pais) %>% 
                select(anio,!!corte1,!!corte2,!!corte3,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              rownames = FALSE, options = list(dom = 'Bfrtip',columnDefs = 
                                                 list(list(className = 'dt-center', 
                                                           targets = "_all"))))
    
      
  }
})




output$dl_tabla_resultado_rraa <- downloadHandler(
  
  
  filename = function() {
    paste0(input$indicador_rraa,"-",input$anio_rraa, ".csv", sep = "")
  },
  content = function(file) {
    
    
    corte1 <- rlang::sym(rlang::as_string(rraa_filt()$corte1[1]))
    corte2 <- rlang::sym(rlang::as_string(rraa_filt()$corte2[1]))
    corte3 <- rlang::sym(rlang::as_string(rraa_filt()$corte3[1]))
    
  
  if(corte1 == corte2 & corte1 != "pais" & corte2 != "pais" & corte1 != "depto" & corte2 != "depto" & input$anio_rraa !="Evolución" & corte3 != "movimiento_ineg") {
    
    
    write.csv(rraa_filt() %>%
                filter(anio == input$anio_rraa& !!corte1 !="Total") %>% select(anio,!!corte1,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad","Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              file, row.names = FALSE, fileEncoding = "latin1")
    
    
    
  } else if(corte1 != corte2 & corte1 != "pais" & corte2 != "pais" & corte1 != "depto" & corte2 != "depto" & input$anio_rraa !="Evolución" & corte3 != "movimiento_ineg") {
    
    
    write.csv(rraa_filt() %>%
                filter(anio == input$anio_rraa& !!corte1 !="Total") %>% select(anio,!!corte1,!!corte2,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              file, row.names = FALSE, fileEncoding = "latin1")
    
    
    
  } else if(corte1 == "pais" & corte2 != "pais" & corte1 != "depto" & corte2 != "depto" & input$anio_rraa !="Evolución" & corte3 != "movimiento_ineg") {
    
    write.csv(rraa_filt() %>%
                filter(anio == input$anio_rraa & !!corte2 !="Total" & pais %in% input$pais) %>% 
                select(anio,pais,!!corte1,!!corte2,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              file, row.names = FALSE, fileEncoding = "latin1")
    
    
    
    
  } else if(corte1 == "pais" & corte2 == "pais" & corte1 != "depto" & corte2 != "depto" & input$anio_rraa !="Evolución" & corte3 != "movimiento_ineg") {
    
    write.csv(rraa_filt() %>%
                filter(anio == input$anio_rraa & !!corte2 !="Total" & pais %in% input$pais) %>% 
                select(anio,pais,!!corte1,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              file, row.names = FALSE, fileEncoding = "latin1")
    
  } else if(corte1 != "depto" & corte2 == "depto" & corte1 != "pais" & corte2 != "pais" & input$anio_rraa !="Evolución" & corte3 != "movimiento_ineg") {
    
    
    write.csv(rraa_filt() %>%
                filter(anio == input$anio_rraa & !!corte2 !="Total" & depto %in% input$depto) %>% 
                select(anio,depto,!!corte1,!!corte2,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              file, row.names = FALSE, fileEncoding = "latin1")
    
    
  } else if(corte1 == "depto" & corte2 == "depto" & corte1 != "pais" & corte2 != "pais" & input$anio_rraa !="Evolución" & corte3 != "movimiento_ineg") {
    
    write.csv(rraa_filt() %>%
                filter(anio == input$anio_rraa & !!corte2 !="Total" & depto %in% input$depto) %>% 
                select(anio,depto,!!corte1,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              file, row.names = FALSE, fileEncoding = "latin1")
    
    
  }else if(corte1 == corte2 & corte1 != "pais" & corte2 != "pais" & corte1 != "depto" & corte2 != "depto" & input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg") {
    
    
    write.csv(rraa_filt() %>%
                filter(!!corte1 !="Total") %>% select(anio,!!corte1,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              file, row.names = FALSE, fileEncoding = "latin1")
    
    
  } else if(corte1 != corte2 & corte1 != "pais" & corte2 != "pais" & corte1 != "depto" & corte2 != "depto" & input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg") {
    
    
    write.csv(rraa_filt() %>%
                filter(!!corte1 !="Total") %>% select(anio,!!corte1,!!corte2,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              file, row.names = FALSE, fileEncoding = "latin1")
    
    
  } else if(corte1 == "pais" & corte2 != "pais" & corte1 != "depto" & corte2 != "depto" & input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg") {
    
    write.csv(rraa_filt() %>%
                filter( !!corte2 !="Total" & pais %in% input$pais) %>% 
                select(anio,pais,!!corte1,!!corte2,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              file, row.names = FALSE, fileEncoding = "latin1")
    
    
    
  } else if(corte1 == "pais" & corte2 == "pais" & corte1 != "depto" & corte2 != "depto" & input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg") {
    
    write.csv(rraa_filt() %>%
                filter(!!corte2 !="Total" & pais %in% input$pais) %>% 
                select(anio,pais,!!corte1,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              file, row.names = FALSE, fileEncoding = "latin1")
    
  } else if(corte1 != "depto" & corte2 == "depto" & corte1 != "pais" & corte2 != "pais" & input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg") {
    
    
    write.csv(rraa_filt() %>%
                filter( !!corte2 !="Total" & depto %in% input$depto) %>% 
                select(anio,depto,!!corte1,!!corte2,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              file, row.names = FALSE, fileEncoding = "latin1")
    
    
  } else if(corte1 == "depto" & corte2 == "depto" & corte1 != "pais" & corte2 != "pais" & input$anio_rraa =="Evolución" & corte3 != "movimiento_ineg") {
    
    write.csv(rraa_filt() %>%
                filter(!!corte2 !="Total" & depto %in% input$depto) %>% 
                select(anio,depto,!!corte1,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              file, row.names = FALSE, fileEncoding = "latin1")
    
  }else if( corte3 == "movimiento_ineg" & corte1 != "pais" & input$anio_rraa !="Evolución") {
    
    
    write.csv(rraa_filt() %>%
                filter(!!corte2 !="Total") %>% 
                select(anio,depto,!!corte1,!!corte2,!!corte3,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              file, row.names = FALSE, fileEncoding = "latin1")
    
    
  }else if( corte3 == "movimiento_ineg" & corte1 == "pais" & input$anio_rraa !="Evolución") {
    
    write.csv(rraa_filt() %>%
                filter(!!corte2 !="Total" & pais %in% input$pais) %>% 
                select(anio,depto,!!corte1,!!corte2,!!corte3,valor)%>%
                dplyr::rename(any_of(c("Lugar de nacimiento" = "lugar_nac",
                                       "Año" = "anio",
                                       "Valor" = "valor",
                                       "País" = "pais",
                                       "Sexo" = "sexo",
                                       "Tramos de edad" = "tramos",
                                       "Total" = "total",
                                       "Tramos de edad " = "tramos_r",
                                       "Lugar de nacimiento " = "lugar_nac_d",
                                       "Tramos de edad  " = "tramos_d",
                                       "Departamento" = "depto",
                                       "Movimiento" = "movimiento_ineg",
                                       "Sexo " = "sexo_ineg",
                                       "Tramos de edad   " = "tramos_ineg",
                                       "Punto de control" = "punto_control",
                                       "Identidad de género" = "identidad_gen",
                                       "Tramos de edad    " = "tramos_mides",
                                       "Identidad de género " = "identidad_gen_ce",
                                       "Tramos edad" = "tramos_mides_ce",
                                       "Edad" = "tramos_mig",
                                       "Causal jubilatoria" =  "causal_jub",
                                       "Propuesta" =  "propuesta",
                                       "Edades" =  "tramos_inau",
                                       "Género" =  "identidad_gen_trabajo",
                                       "Tramos" =  "tramos_trabajo",
                                       "Solicitudes" =  "solicitudes",
                                       "Personas" = "personas",
                                       "Tramo de edad" = "tramos_refugiados",
                                       "Tipo de educación"="tipo_edu",
                                       "Categoría de escuela"="cat_escuela",
                                       "Nivel de contexto Sociocultural"="nivel_soc",
                                       "Grado"="grado",
                                       "Tramo de edad "="tramos_ced",
                                       "Tipo de residencia" = "tipo_res",
                                       "Institución otorgante" = "institucion",
                                       "Calidad de residencia" = "calidad",
                                       "Tramos  de edad" ="tramos_egreso",
                                       "Tramos  de  edad" ="tramos_territorio"))), 
              
              file, row.names = FALSE, fileEncoding = "latin1")
    
    
  }
  
  
})




  
  
  
}

shinyApp(ui = ui, server = server)


