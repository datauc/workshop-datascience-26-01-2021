library(shiny)

shinyServer(function(input, output, session) {

    
    updatePickerInput(session,
                      inputId = "selector_comunas_barras",
                      choices = datos_casen %>% select(comuna) %>% distinct() %>% pull() %>% as.character(),
                      selected = c("La Florida", "Puente Alto", "La Cisterna", "Cerrillos", "Ñuñoa", "Vitacura", "Providencia", "Maipú", "Santiago")
    )
  
  updatePickerInput(session,
                    inputId = "selector_comunas_gse",
                    choices = datos_casen %>% select(comuna) %>% distinct() %>% pull() %>% as.character(),
                    selected = c("Puente Alto", "Providencia", "La Dehesa")
  )

    source("filtrador_casen.R", local = TRUE)

    #Output tabla ----
    output_tabla_casen <- reactive({
        output <- datos_filtrados() %>%
          group_by(comuna) %>%
          count(name="cantidad") %>%
          ungroup() %>%
          #{if (input$selector_grupo_porcentaje == "Comunal") group_by(., comuna) else .} %>%
          mutate(porcentaje = cantidad/sum(cantidad))

      return(output)
    })


    selector_tipo_casen <- renderText({
      as.character(input$selector_tipo_casen)
    })

    #Output barras ----
    output_grafico_casen <- reactive({
      grafico_g <- output_tabla_casen() %>%
        filter(comuna %in% input$selector_comunas_barras) %>%
        ggplot(aes(forcats::fct_reorder(comuna, cantidad, .desc=T),
                   cantidad,
                   fill=cantidad)) +
        geom_col(width=0.4) +
        geom_text(aes(label = stringr::str_trim(format(cantidad, big.mark =  ".", decimal.mark = ","))),
                  vjust=-0.8,
                  size=4) +
        labs(y="Cantidad",
             x="") +
        theme_minimal(base_size = 15) +
        viridis::scale_fill_viridis() +
        scale_y_continuous(expand = expansion(mult=c(0, 0.2)),
                           labels = function(x) stringr::str_trim(format(x, big.mark = ".", decimal.mark = ","))) +
        theme(legend.position = "none",
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.text.x = element_text(angle = 90, vjust = 0.5),
              axis.title.y = element_text(margin=margin(r=10)))

      grafico_g

    })

    output$output_grafico_casen <- renderPlot({
      output_grafico_casen()
    })


    #Output densidad ----
    output_densidad_casen <- reactive({

      if (input$selector_densidad_pobreza_casen==TRUE) {

        densidad_g <- datos_filtrados() %>%
          filter(!is.na(pobreza)) %>%
          ggplot(aes(x = ytotcor,
                     fill = pobreza,
                     col = pobreza)) +
          geom_density(kernel = "gaussian",
                       bw = 50000, n=20000,
                       alpha = 0.9) +
          scale_x_continuous(labels = function(x) stringr::str_trim(format(x, big.mark = ".", decimal.mark=",")),
                             breaks = c(0, 200000, 400000, 600000, 800000, 1000000, 1250000, 1500000, 1750000, 2000000, 2500000, 3000000, 3500000, 4000000, 5000000, 6000000, 7000000, 8000000, 9000000, 10000000)) +
          theme_minimal(base_size=15) +
          scale_fill_viridis_d(aesthetics = c("fill", "col")) +
          coord_cartesian(xlim = c(0, 3000000),
                          clip= "off",
                          #ylim = c(0, 0.000004),
                          expand = FALSE) +
          theme(legend.position = "bottom",
                legend.title = element_blank(),
                legend.margin = margin(0, 20, 20, 0),
                legend.text=element_text(margin=margin(r=10)),
                axis.text.x = element_text(angle=-90, hjust=0,
                                           margin=margin(t=5)),
                axis.text.y = element_blank(),
                axis.title = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major.y = element_blank())

      } else if (input$selector_densidad_pobreza_casen==FALSE) {

        densidad_g <- datos_filtrados() %>%
          filter(!is.na(pobreza)) %>%
          ggplot(aes(x = ytotcor,
                     fill = "pobreza",
                     col = "pobreza")) +
          geom_density(kernel = "gaussian",
                       bw = 50000, n=20000,
                       alpha = 0.9) +
          scale_x_continuous(labels = function(x) stringr::str_trim(format(x, big.mark = ".", decimal.mark=",")),
                             breaks = c(0, 200000, 400000, 600000, 800000, 1000000, 1250000, 1500000, 1750000, 2000000, 2500000, 3000000, 3500000, 4000000, 5000000, 6000000, 7000000, 8000000, 9000000, 10000000)) +
          theme_minimal(base_size=15) +
          scale_fill_viridis_d(aesthetics = c("fill", "col")) +
          coord_cartesian(xlim = c(0, 4000000),
                          #ylim = c(0, 0.000004),
                          expand = FALSE) +
          theme(legend.position = "none",
                legend.title = element_blank(),
                legend.margin = margin(0, 20, 20, 0),
                legend.text=element_text(margin=margin(r=10)),
                axis.text.x = element_text(angle=-90, hjust=0,
                                           margin=margin(t=5)),
                axis.text.y = element_blank(),
                axis.title = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major.y = element_blank())
      }

      if (input$selector_densidad_genero_casen==TRUE) {
        densidad_g <-  densidad_g +
          facet_wrap(~sexo) +
          theme(panel.spacing.x =unit(1, "cm"))
      } else {
        densidad_g <- densidad_g
      }


      densidad_g

    })

    output$output_densidad_casen <- renderPlot({
      output_densidad_casen()
    })



    #Output deciles ----
    
    output$output_grafico_deciles <- renderPlot({
      
      #crear variable que categoriza a las personas en deciles
      deciles_2 <- datos_filtrados() %>%
        select(ytrabajocorh) %>%
        mutate(decil = case_when(ytrabajocorh <= as.numeric(deciles$`Decil 1`) ~ "Decil 1",
                                 ytrabajocorh <= as.numeric(deciles$`Decil 2`) ~ "Decil 2",
                                 ytrabajocorh <= as.numeric(deciles$`Decil 3`) ~ "Decil 3",
                                 ytrabajocorh <= as.numeric(deciles$`Decil 4`) ~ "Decil 4",
                                 ytrabajocorh <= as.numeric(deciles$`Decil 5`) ~ "Decil 5",
                                 ytrabajocorh <= as.numeric(deciles$`Decil 6`) ~ "Decil 6",
                                 ytrabajocorh <= as.numeric(deciles$`Decil 7`) ~ "Decil 7",
                                 ytrabajocorh <= as.numeric(deciles$`Decil 8`) ~ "Decil 8",
                                 ytrabajocorh <= as.numeric(deciles$`Decil 9`) ~ "Decil 9",
                                 ytrabajocorh >  as.numeric(deciles$`Decil 9`) ~ "Decil 10")) %>%
        count(decil, name ="cantidad") %>%
        mutate(porcentaje = cantidad/sum(cantidad))
      
      
      deciles_3 <- deciles_2 %>%
        #crear columna con la cifra de corte de los deciles
        left_join(deciles %>%
                    t() %>%
                    as.data.frame() %>%
                    tibble::rownames_to_column(var = "decil") %>%
                    rename(limite = 2) %>%
                    tibble::as_tibble()) %>%
        mutate(decil = forcats::fct_reorder(decil, limite),
               decil = forcats::fct_relevel(decil, "Decil 10", after = 9)) %>%
        #crear texto que describe los cortes
        mutate(limite_text = case_when(
          decil != "Decil 10" & limite < 1000000 ~ paste("Hasta", round(limite/1000), "mil"),
          decil != "Decil 10" & decil != "Decil 1" & limite >= 1000000 ~ paste("Hasta", round(limite/1000000, 1), "millones"),
          #decil == "Decil 1" ~ paste("Menos de", round(lead(limite)/1000), "mil"),
          decil == "Decil 10" ~ paste("Más de 2.4 millones"))
        )
      
      
      #graficar
      p <- deciles_3 %>%
        ggplot(aes(y = decil, x = porcentaje, fill = decil)) +
        geom_col(width=0.5) +
        geom_text(aes(label = paste(scales::percent(porcentaje, 
                                                    accuracy = 0.1), "-",
                                    limite_text)),
                  hjust = -0.07, #nudge_x = 0.02,
                  size=4) +
        viridis::scale_fill_viridis(discrete = TRUE) +
        scale_y_discrete(drop = FALSE) +
        coord_cartesian(xlim = c(0, 1)) +
        theme_minimal(base_size = 15) +
        theme(legend.position = "none") +
        theme(panel.grid = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_text(margin = margin(l =10, r=-10)))+
        labs(y = "Decil de ingresos",
             x = "Porcentaje de la población")
      
      return(p)
    })
    
    
#Output gse ----
    
    output$output_grafico_gse <- renderPlot({
      datos_filtrados() %>%
      #datos_casen %>%
        filter(comuna %in% input$selector_comunas_gse) %>%
        # filter(comuna  %in% c("La Florida",
        #                       "Cerrillos",
        #                       "Vitacura")) %>%
        mutate(gse = case_when(numper == 1 & ytotcorh <= 66000 ~ "E", #E
                               numper == 2 & ytotcorh <= 134000 ~ "E", 
                               numper == 3 & ytotcorh <= 212000 ~ "E", 
                               numper == 4 & ytotcorh <= 276000 ~ "E", 
                               numper == 5 & ytotcorh <= 325000 ~ "E", 
                               numper >= 6 & ytotcorh <= 382000 ~ "E",
                               #D
                               numper == 1 & ytotcorh <= 134000 ~ "D", 
                               numper == 2 & ytotcorh <= 252000 ~ "D", 
                               numper == 3 & ytotcorh <= 382000 ~ "D", 
                               numper == 4 & ytotcorh <= 479000 ~ "D", 
                               numper == 5 & ytotcorh <= 572000 ~ "D", 
                               numper >= 6 & ytotcorh <= 661000 ~ "D",
                               #C3
                               numper == 1 & ytotcorh <= 258000 ~ "C3", 
                               numper == 2 & ytotcorh <= 463000 ~ "C3", 
                               numper == 3 & ytotcorh <= 663000 ~ "C3", 
                               numper == 4 & ytotcorh <= 830000 ~ "C3", 
                               numper == 5 & ytotcorh <= 984000 ~ "C3", 
                               numper >= 6 & ytotcorh <= 1124000 ~ "C3",
                               #C2
                               numper == 1 & ytotcorh <= 460000 ~ "C2", 
                               numper == 2 & ytotcorh <= 824000 ~ "C2", 
                               numper == 3 & ytotcorh <= 1115000 ~ "C2", 
                               numper == 4 & ytotcorh <= 1384000 ~ "C2", 
                               numper == 5 & ytotcorh <= 1650000 ~ "C2", 
                               numper >= 6 & ytotcorh <= 1750000 ~ "C2",
                               #C1b
                               numper == 1 & ytotcorh <= 807000 ~ "C1b", 
                               numper == 2 & ytotcorh <= 1404000 ~ "C1b", 
                               numper == 3 & ytotcorh <= 1926000 ~ "C1b", 
                               numper == 4 & ytotcorh <= 2311000 ~ "C1b", 
                               numper == 5 & ytotcorh <= 2717000 ~ "C1b", 
                               numper >= 6 & ytotcorh <= 3005000 ~ "C1b",
                               #C1a
                               numper == 1 & ytotcorh <= 1414000 ~ "C1a", 
                               numper == 2 & ytotcorh <= 2350000 ~ "C1a", 
                               numper == 3 & ytotcorh <= 3234000 ~ "C1a", 
                               numper == 4 & ytotcorh <= 3960000 ~ "C1a", 
                               numper == 5 & ytotcorh <= 4656000 ~ "C1a", 
                               numper >= 6 & ytotcorh <= 5428000 ~ "C1a",
                               #AB
                               numper == 1 & ytotcorh >= 1415000 ~ "AB", 
                               numper == 2 & ytotcorh >= 2351000 ~ "AB", 
                               numper == 3 & ytotcorh >= 3235000 ~ "AB", 
                               numper == 4 & ytotcorh >= 3961000 ~ "AB", 
                               numper == 5 & ytotcorh >= 4657000 ~ "AB", 
                               numper >= 6 & ytotcorh >= 5429000 ~ "AB")) %>%
        mutate(gse = as.factor(gse),
               gse = forcats::fct_rev(gse)) %>%
        filter(!is.na(gse)) %>%
        group_by(comuna) %>%
        count(gse, name = "cantidad") %>%
        mutate(porcentaje = cantidad/sum(cantidad)) %>%
        #graficar
        ggplot(aes(x = gse, y = porcentaje)) +
        geom_point(aes(col = comuna,
                       size = cantidad),
                   alpha=0.7) +
        coord_cartesian(clip = "off") +
        scale_size_continuous(range = c(6, 20),
                              labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
        viridis::scale_color_viridis(discrete = TRUE) +
        scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
        labs(y="Porcentaje de cada comuna",
             x="Grupos socioeconómicos\n(del más bajo al más alto)",
             col = "Comunas",
             size = "Población") +
        theme_minimal(base_size = 15) +
        theme(legend.position = "right",
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.text.x = element_text(angle = 90, vjust = 0.5),
              axis.title.y = element_text(margin=margin(r=10))) +
        guides(size = guide_legend(override.aes = list(color="gray60")),
               col = guide_legend(override.aes = list(size=4)))
      
    })



    #Output mapa ----
    output$output_mapa_casen <- renderGirafe({

      #Obtener mapa
      mapa_regional <- chilemapas::mapa_comunas %>%
        left_join(
          chilemapas::codigos_territoriales %>%
            select(matches("comuna"))
        ) %>%
        rename(comuna=nombre_comuna) %>%
        mutate(comuna = recode(comuna,
                               "San Jose de Maipo" = "San José de Maipo",
                               "Alhue" = "Alhué",
                               "Curacavi" = "Curacaví",
                               "Maria Pinto" = "María Pinto",
                               "Nunoa" = "Ñuñoa",
                               "Penalolen" = "Peñalolén",
                               "Maipu" = "Maipú",
                               "Penaflor" = "Peñaflor",
                               "San Ramon" = "San Ramón",
                               "San Joaquin" = "San Joaquín",
                               "Estacion Central" = "Estación Central",
                               "Conchali" = "Conchalí")) %>%
        left_join(output_tabla_casen() ) %>%
        filter(codigo_region=="13") #%>%
        # filter(comuna != "San Jose de Maipo",
        #        comuna != "San Pedro",
        #        comuna != "Alhue")

      #Graficar
      #Frecuencia ----
      if (input$selector_tipo_casen=="Frecuencia") {
        mapa_regional_g <- mapa_regional %>%
          ggplot(aes(geometry = geometry,
                     fill = cantidad )) +
          geom_sf_interactive(col="white",
                              aes(tooltip = paste(comuna, "\n",
                                                  stringr::str_trim(format(cantidad, big.mark =  ".", decimal.mark = ",")),
                                                  "personas"
                                                  ))) +
          viridis::scale_fill_viridis(name="Frecuencia",
                                      labels = function(x) format(x, big.mark =  ".", decimal.mark = ",") ) +
          coord_sf(expand = FALSE) +
          theme_minimal(base_size = 15) +
          theme(legend.position = "right",
                axis.text = element_blank(),
                axis.title = element_blank(),
                panel.grid = element_blank())

        #Porcentaje ----
      } else if (input$selector_tipo_casen=="Porcentaje") {
        mapa_regional_g <- mapa_regional %>%
          ggplot(aes(geometry = geometry,
                     fill = porcentaje )) +
          geom_sf_interactive(col="white",
                              aes(tooltip = paste(comuna, "\n",
                                                  stringr::str_trim(paste0(round(porcentaje*100, digits=1),"%"))
                              ))) +
          viridis::scale_fill_viridis(name="Porcentaje",
                                      labels = function(x) paste0(round(x*100, digits=1),"%") ) +
          coord_sf(expand = FALSE) +
          theme_minimal(base_size = 15) +
          theme(legend.position = "right",
                axis.text = element_blank(),
                axis.title = element_blank(),
                panel.grid = element_blank())
      }


      girafe(ggobj = mapa_regional_g,
             pointsize = 18,
             #bg = color_verde_oficial_medio,
             width_svg = 12, height_svg = 8,
             fonts = list(sans = "Open Sans"),
             options = list(
               opts_sizing(rescale = FALSE, width = 1),
               opts_toolbar(position = "topright", saveaspng = FALSE)))
    })

    # output$output_mapa_casen <- renderPlot({
    #   output_mapa_casen()
    # })
    
  #Texto filtro ----
    
    output$texto_filtro <- renderText({
      
      texto <- vector() #crear vector
      
      
      #pobreza por ingresos
      if (is.null(input$pobreza_ingresos_casen)) { 
        
        } else if (input$pobreza_ingresos_casen == "Pobreza extrema" & input$pobreza_ingresos_casen == "Pobreza no extrema") {
        texto <-  append(texto, "en situación de pobreza extrema")
        
      } else if (input$pobreza_ingresos_casen == "Pobreza no extrema") {
        texto <-  append(texto, "en situación de pobreza no extrema")
        
      } else if (input$pobreza_ingresos_casen == "Pobreza extrema") {
        texto <-  append(texto, "en situación de pobreza extrema")
      
      }
      
      #pobreza multidimensional
      if (!is.null(input$pobreza_multi_casen)) {
        texto <-  append(texto, "en situación de pobreza multidimensional")
      }
      
      #slider percentil
      if (is.null(input$percentil_slider_casen)) { 
        
      } else if (input$percentil_slider_casen != 100) {
       
        texto <-  append(texto, 
                         paste0("perteneciente al ", 
                                input$percentil_slider_casen, "% de los ingresos más bajos"))
      }
      
      #mujeres
      if (!is.null(input$mujeres_casen)) {
        texto <-  append(texto, "de género femenino")
      }
      
      
      if (!is.null(input$migrantes_casen)) {
        texto <-  append(texto, "en condición de migrantes")
      }
      
      
      if (input$edad_slider_casen != 0) {
        texto <-  append(texto, paste0("mayores de ",
                                       input$edad_slider_casen, " años"))
      }
      
      
      if (input$escolaridad_slider_casen < 25) {
        texto <-  append(texto, paste0("con menos de ",
                                       input$escolaridad_slider_casen, " años de educación"))
      }
      
      
      if (!is.null(input$indigena_casen)) {
        texto <-  append(texto, "que pertenecen a un pueblo originario")
      }
      
      
      if (!is.null(input$inactivos_casen)) {
        texto <-  append(texto, "que no integran la fuerza de trabajo (inactividad)")
      }
      
      
      if (!is.null(input$informalidad_casen)) {
        texto <-  append(texto, "que carecen de contrato laboral (informalidad)")
      }
      
      
      if (!is.null(input$trabajo_domestico_casen)) {
        texto <-  append(texto, "que se dedican al trabajo doméstico remunerado (trabajadoras/es de casa particular)")
      }
      
      
      if (is.null(input$zona_casen)) { 
        
      } else if (input$zona_casen == "Rural" & input$zona_casen == "Urbano") {
        texto <- texto
        
      } else if (input$zona_casen == "Urbano") {
        texto <-  append(texto, "que viven en zona urbana")
        
      } else if (input$zona_casen == "Rural") {
        texto <-  append(texto, "que viven en zona rural")
        }
      
      
      if (!is.null(input$hacinamiento_casen)) {
        texto <-  append(texto, "que viven en condición de hacinamiento")
      }
      
      
      if (!is.null(input$servicios_casen)) {
        texto <-  append(texto, "en situación de aceso deficiente a servicios básicos")
      }
      
      
      if (is.null(input$vivienda_calidad_casen)) { 
        
      } else if (input$vivienda_calidad_casen == "Recuperable" & input$vivienda_calidad_casen == "Irrecuperable") {
        texto <- texto
        
      } else if (input$vivienda_calidad_casen == "Recuperable") {
        texto <-  append(texto, "cuya calidad de su vivienda es inaceptable pero recuperable")
      
      } else if (input$vivienda_calidad_casen == "Irrecuperable") {
        texto <-  append(texto, "cuya calidad de su vivienda es inaceptable y considerada irrecuperable")
      }
      
      
      if (!is.null(input$malnutricion_casen)) {
        texto <-  append(texto, "que vive con menores de edad en situación de malnutrición")
      }
      
      
      if (!is.null(input$comida_sana_casen)) {
        texto <-  append(texto, "con dificultades para acceder a alimentación saludable")
      }
      
      
      if (!is.null(input$comida_insuficiencia_casen)) {
        texto <-  append(texto, "que experimenta insuficiencia de alimentos debido a su falta de recursos")
      }
      
      
      texto_listo <- texto
      
      #poner texto por defecto si no se ha seleccionado nada
      if (length(texto) == 0) {
        texto_listo <- append(texto_listo, "mostrando toda la población")
      }
      
      #unir textos de filtros activos y separar con comas
      texto_2 <- paste(texto_listo, collapse = ", ") 
      
      #poner título y punto al final
      if (length(texto) == 0) {
        texto_3 <- paste0("<b>Población filtrada:</b> ", texto_2, ". ") 
      } else {
      texto_3 <- paste0("<b>Población filtrada:</b> personas ", texto_2, ". ") 
      }
      
      return(texto_3)
    })
    
    
    
    
    
    
    
})
