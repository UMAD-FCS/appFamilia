dat <- df %>% filter(NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 0 y 17 años de edad" & is.na(CORTE1)==T)

g1 <- ggplot(dat, aes(x = FECHA, y = as.numeric(VALOR), alpha = metodo, color = NOMINDICADOR)) +
    geom_line(dat = dat %>% filter(FECHA <= 2019),
            size = 1, alpha = 0.4) +
    geom_line(dat = dat %>% filter(FECHA >= 2022),
            size = 1, alpha = 0.8) +
    geom_point(size = 2, show.legend = FALSE) +
    theme_minimal() +
    theme(legend.position = "bottom",
        legend.title = element_blank()) +
    scale_alpha_manual(values = c(.4, .8)) +
    scale_color_manual(
    values = c("639")) +
  #ylim(0, 100) +
  #scale_x_continuous(breaks=seq(min(FECHA), max(FECHA), 2)) +
  scale_x_date(date_breaks = "3 years",date_labels  = "%Y")+
  
    labs(
    color = "",
    alpha = "",
    title = "",
    caption = "",
    x = "",
    y = "")


g1



dat <- df %>% filter(NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 0 y 17 años de edad" & CORTE1_rec == "Pobreza")

g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), alpha = metodo, color = POBRE)) +
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
  # scale_color_manual(
  #   values = c("639")) +
  scale_x_date(date_breaks = "3 years",date_labels  = "%Y")
  # labs(
  #   color = "",
  #   alpha = "",
  #   title = input$indicador_CE_tp,
  #   caption = str_wrap(paste("Fuente:",unique(metadata %>% 
  #                                               filter(NOMINDICADOR == input$indicador_CE_tp) %>% pull(FUENTE))), width = 160),
  #   x = "",
  #   y = "")

g1


dat <- df %>% filter(NOMINDICADOR == "Porcentaje de hogares en los que residen menores entre 0 y 17 años de edad" & CORTE1_rec == "Pobreza")
cut <- rlang::sym(dat$CORTE1_rec[1])

g1 <- dat %>% ggplot(aes(x = as.Date(as.character(FECHA), format = "%Y"), y = as.numeric(VALOR), alpha = metodo, color = !!cut)) +
geom_line(dat = dat %>% filter(FECHA <= 2019),
          size = 1, alpha = 0.4) +
  geom_line(dat = dat %>% filter(FECHA >= 2022),
            size = 1, alpha = 0.8) 
