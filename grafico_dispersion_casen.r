#PARTE 2: gráfico de dispersión

#cambiemos el nombre al objeto
casen <- casen_rm2

glimpse(casen)

names(casen)

#calcular resumen de varias variables ----
casen_p <- casen %>%
  group_by(comuna) %>%
  summarize(across(3:13, ~ mean(.x, na.rm = TRUE)))

casen_p <- casen %>%
  group_by(comuna) %>%
  summarize(across(3:13, ~ mean(.x, na.rm = TRUE)),
            hacinamiento =  sum(hacinamiento == "Hacinamiento crítico (5 y más)" | hacinamiento == "Hacinamiento medio alto (3,5 a 4,9)" | hacinamiento == "Hacinamiento medio bajo (2,5 a 3,49)", na.rm = TRUE),
            desocupados = sum(activ == "Desocupados", na.rm=TRUE),
            pobreza = sum(pobreza == "Pobres extremos" | pobreza == "Pobres no extremos", na.rm=T),
            extranjero = sum(r1a == "Otra nacionalidad. Especifique país", na.rm=TRUE)
            )


#graficar ----
library(ggplot2)

casen_p %>%
  filter(comuna %in% c("La Pintana", "La Florida", "Santiago", "Ñuñoa", "Providencia", "Vitacura")) %>%
  ggplot(aes(x = yoprcor,
             y = pobreza, 
             size = esc,
             col = comuna)) +
  geom_point()

casen_p %>%
  filter(comuna %in% c("La Pintana", "La Florida", "Santiago", "Ñuñoa", "Providencia", "Vitacura")) %>%
  ggplot(aes(x = desocupados,
             y = hacinamiento, 
             size = ytotcorh,
             col = comuna)) +
  geom_point() +
  theme_light()


casen_p %>%
  filter(comuna %in% c("La Pintana", "La Florida", "Santiago", "Ñuñoa", "Providencia", "Vitacura")) %>%
  ggplot(aes(x = edad,
             y = esc, 
             size = ytotcorh,
             col = comuna)) +
  geom_point() +
  scale_size_continuous(labels = scales::number) +
  theme_light()


options(scipen=9999)


#añadir datos de covid ----
#descargar los datos de covid: https://github.com/datauc/api-covid19-datauc
covid <- readr::read_csv("https://coronavirus-api.mat.uc.cl/casos_totales_comuna_enriquecido")

#ver datos más actuales
covid %>%
  filter(region == "Metropolitana de Santiago") %>%
  arrange(desc(fecha))

#seleccionar lo que necesitamos
covid_c <- covid %>% 
  filter(region == "Metropolitana de Santiago") %>%
  filter(!is.na(casos_confirmados)) %>%
  filter(fecha == max(fecha)) %>%
  select(comuna, casos_confirmados) %>%
  mutate(casos = as.numeric(casos_confirmados))

casen_p %>%
  left_join(covid_c) %>%
  filter(comuna %in% c("La Pintana", "La Florida", "Santiago", "Ñuñoa", "Providencia", "Vitacura")) %>%
  ggplot(aes(x = esc,
             y = ytotcorh, 
             size = casos,
             col = comuna)) +
  geom_point() +
  scale_size_continuous(range = c(1, 10)) +
  #scale_size_continuous(labels = scales::number) +
  theme_light()
