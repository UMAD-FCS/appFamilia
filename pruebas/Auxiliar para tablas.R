## Tablas
output$tabla_resultado_CE_tp <- renderDT({


  } else if(substring(input$indicador_CE_tp,1,5) == "Distr" & input$corte1 != "Total") {
    

    dat <- base_CE_tp() %>% filter(NOMINDICADOR == input$indicador_CE_tp & CORTE1_rec == input$corte1)   %>%
      filter(FECHA %in% input$anio1)
    cut1 <- rlang::sym(rlang::as_string(unique(df$CORTE2_rec[df$NOMINDICADOR ==input$indicador_CE_tp])))
    cut2 <- rlang::sym(rlang::as_string(unique(df$CORTE1_rec[df$NOMINDICADOR ==input$indicador_CE_tp&df$CORTE1_rec==input$corte1])))
    cut_nom1 <- quo_name(cut1)
    cut_nom2 <- quo_name(cut2)
    
    datatable(dat %>% 
                filter(NOMINDICADOR==input$indicador_CE_tp & is.na(CORTE1) == T) %>%
                arrange(!!cut)%>%
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
    
    
