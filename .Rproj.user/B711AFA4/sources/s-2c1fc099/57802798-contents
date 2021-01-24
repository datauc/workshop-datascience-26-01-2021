library(dplyr)
library(tidyr)

#importar datos
casen <- readstata13::read.dta13("~/Casen/Casen 2017.dta") %>% as_tibble()

#filtrar sólo la RM
casen_rm <- casen %>%
  filter(region == "Región Metropolitana de Santiago")

#seleccionar variables
casen_1 <- casen_rm %>% 
  select(comuna,
         expc,                    #factor de expansión comunal
         sexo,                    #género
         esc,                     #años de escolaridad
         edad,                    #edad
         ytotcorh,                #Ingreso total del hogar corregido
         ytotcor,                 #Ingreso total corregido
         yoprcor,                 #Ingreso ocupación principal
         ypc,                     #Ingreso total per cápita del hogar corregido
         ytrabajocor,             #ingreso del trabajo
         ytrabajocorh,            #ingreso del trabajo del hogar
         ypchautcor,              #ingreso autónomo per cápita 
         y26_2c,                  #jubilación o pensión
         numper,                  #numero de personas en el hogar
         s4,                      #hijos vivos
         pco1,                    #jefe de hogar
         activ,                   #actividad
         hacinamiento,            #hacinamiento
         pobreza,                 #pobreza
         pobreza_multi_5d,        #pobreza multidimensional
         r1a,                     #nacionalidad
         r3,                      #pertenencia a pueblos originarios
         v12,                     #metros cuadrados de la casa
         indmat)                  #índice de materialidad de la vivienda

#aplicar factor de expansión
casen_2 <- tidyr::uncount(casen_1, weights = expc)

glimpse(casen_2)

#calcular medidas de tendencia central
casen_promedios <- casen_2 %>%
  group_by(comuna) %>%
  summarize(across(2:13, ~ mean(.x, na.rm = TRUE))) %>%
  mutate(tipo = "promedio")

casen_medianas <- casen_2 %>%
  group_by(comuna) %>%
  summarize(across(2:13, ~ median(.x, na.rm = TRUE))) %>%
  mutate(tipo = "mediana")

#calcular metros cuadrados de la vivienda aproximados
casen_metros_promedio <- casen_2 %>%
  mutate(v12 = recode(v12, "Menos de 30 m2" = "20")) %>%
  mutate(metros = readr::parse_number(as.character(v12))) %>%
  mutate(metros = round(metros, -1)) %>%
  group_by(comuna) %>%
  summarize(metros = mean(metros, na.rm = TRUE)) %>%
  mutate(tipo = "promedio")

casen_metros_mediana <- casen_2 %>%
  mutate(v12 = recode(v12, "Menos de 30 m2" = "20")) %>%
  mutate(metros = readr::parse_number(as.character(v12))) %>%
  mutate(metros = round(metros, -1)) %>%
  group_by(comuna) %>%
  summarize(metros = median(metros, na.rm = TRUE)) %>%
  mutate(tipo = "mediana")


#contar cantidad de personas en ciertas situaciones
casen_conteos <- casen_2 %>%
  group_by(comuna) %>%
  summarize(hacinamiento =  sum(hacinamiento == "Hacinamiento crítico (5 y más)" | hacinamiento == "Hacinamiento medio alto (3,5 a 4,9)" | hacinamiento == "Hacinamiento medio bajo (2,5 a 3,49)", na.rm = TRUE),
            pobreza = sum(pobreza == "Pobres extremos" | pobreza == "Pobres no extremos", na.rm = TRUE),
            originario = sum(r3 != "No pertenece a ningún pueblo indígena" & r3 != "No sabe/no responde", na.rm = TRUE),
            extranjero = sum(r1a == "Otra nacionalidad. Especifique país", na.rm=TRUE),
            inactivos = sum(activ == "Inactivos", na.rm=TRUE),
            desocupados = sum(activ == "Desocupados", na.rm=TRUE),
            pobreza_multi = sum(pobreza_multi_5d == "Pobre", na.rm=TRUE))


#unir datos
casen_datos <- bind_rows(casen_promedios, casen_medianas) %>%
  left_join(bind_rows(casen_metros_promedio,
                      casen_metros_mediana)) %>%
  left_join(casen_conteos %>% mutate(tipo = "promedio")) %>%
  arrange(comuna, tipo)

casen_datos

glimpse(casen_datos)

#limpiar
remove(casen, casen_rm, casen_1, casen_2)

#exportar datos
saveRDS(casen_datos, file = "casen_datos.rds")
