#livecode <- livecode::serve_file(port = 11111)

#WORKSHOP
#Aplicaciones de Data Science en Ciencias Sociales
#Bastián Olea Herrera  |  baolea@mat.uc.cl  |  @bastimapache  |  http://bastian.olea.biz
#https://github.com/datauc/workshop-datascience-26-01-2021

#descargar e instalar R: https://www.r-project.org
#descargar e instalar RStudio: https://rstudio.com/products/rstudio/


#—----


#instalar paquetes necesarios ----
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("readstata13")

#cargar paquetes ----
library(dplyr)

#descargar casen: http://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen-2017

#cargar base de datos ----

casen <- readstata13::read.dta13("~/Casen/Casen 2017.dta")


#mirar la base  ----

casen
tibble(casen)
names(casen)
glimpse(casen)


#convertir a tibble ----
casen <- as_tibble(casen)


#contar casos de una variable----
casen %>%
  count(e19_2)

#filtrar por regiones ----
casen %>%
  count(region)

casen %>%
  filter(region != "Región de Coquimbo") %>%
  count(pobreza)

#filtrar por otras condiciones ----

casen %>%
  count(pco1)

casen %>%
  filter(pco1 == "Jefe(a) de hogar") %>%
  filter(sexo == "Mujer") %>%
  count(pobreza)

#factor de expansión ----
#necesitamos aplicar factor de expansión...

casen %>%
  select(pco1, expc) %>%
  print(n=500)

#filtrar la región metropolitana
casen_rm <- casen %>%
  filter(region == "Región Metropolitana de Santiago")

#seleccionar variables de interés

casen_rm1 <- casen_rm %>%
  select(comuna,
expc,                    #factor de expansión comunal
expr,                    #factor de expansión regional
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
#hasta acá las numéricas #
pco1,                    #jefe de hogar
activ,                   #actividad
hacinamiento,            #hacinamiento
pobreza,                 #pobreza
pobreza_multi_5d,        #pobreza multidimensional
r1a,                     #nacionalidad
r3,                      #pertenencia a pueblos originarios
v12,                     #metros cuadrados de la casa
indmat)                   #índice de materialidad de la vivienda


#aplicar factor de expansión comunal (expc)
casen_rm2 <- tidyr::uncount(casen_rm1, weights = expc)

#comparar conteos
casen_rm %>%
  count(pco1)

casen_rm2 %>%
  count(pco1)

#resumir valores ----
#ingresos, pensiones 
casen_rm2 %>%
  summarize(mean(ytotcorh))

casen_rm2 %>%
  summarize(mean(y26_2c, na.rm=TRUE))


#agrupar operaciones ----
#sexo, comuna

casen_rm2 %>%
  group_by(sexo) %>%
  summarize(mean(y26_2c, na.rm=TRUE))

casen_rm2 %>%
  group_by(comuna) %>%
  summarize(pension = mean(y26_2c, na.rm=TRUE)) %>%
  arrange(desc(pension))


casen_rm2 %>%
  group_by(comuna)

#crear nuevas variables ----





# ---- break ---- #



#calcular resumen de varias variables ----
casen <- casen_rm2


casen %>%
  group_by(comuna) %>%
  summarize(pension = mean(y26_2c, na.rm=TRUE),
            ingreso_hogar = median(ytotcorh, na.rm=TRUE),
            ingreso_ind = median(ytotcor, na.rm=TRUE))


casen %>%
  glimpse()

names(casen)

casen %>%
  count(pobreza)

casen_p <- casen %>%
  group_by(comuna) %>%
  summarize(across(3:14, ~ mean(.x, na.rm = TRUE)),
            hacinamiento =  sum(hacinamiento == "Hacinamiento crítico (5 y más)" | hacinamiento == "Hacinamiento medio alto (3,5 a 4,9)" | hacinamiento == "Hacinamiento medio bajo (2,5 a 3,49)", na.rm = TRUE),
            desocupados = sum(activ == "Desocupados", na.rm=TRUE),
            pobreza = sum(pobreza == "Pobres extremos" | pobreza == "Pobres no extremos", na.rm=T),
            extranjero = sum(r1a == "Otra nacionalidad. Especifique país", na.rm=TRUE)
            )



#gráfico de dispersión ----
library(ggplot2)

options(scipen=9999)


casen_p %>%
  filter(comuna %in% c("La Florida", "Puente Alto", "Las Condes", "Santiago")) %>%
  ggplot(aes(x = desocupados,
             y = hacinamiento,
             size= ytotcorh,
             col = comuna)) +
  geom_point() +
  theme_light()


casen_p %>%
  filter(comuna %in% c("La Florida", "Puente Alto", 
                       "Las Condes", "Santiago")) %>%
  ggplot(aes(x = desocupados,
             y = hacinamiento,
             size= ytotcorh,
             col = comuna)) +
  geom_point() +
  theme_light()


#
casen_p %>%
  filter(comuna %in% c("La Florida", "Puente Alto", 
                       "Las Condes", "Santiago", "La Pintana",
                       "Cerrillos", "Ñuñoa", "Providencia",
                       "San Bernardo")) %>%
  ggplot(aes(x = pobreza,
             y = yoprcor,
             size = esc,
             alpha = numper,
             col = comuna)) +
  geom_point() +
  theme_light()


#


#



#añadir datos de covid ----
#descargar los datos de covid: https://github.com/datauc/api-covid19-datauc

covid <- readr::read_csv("http://localhost:8080/casos_totales_comuna_enriquecido")

covid %>%
  filter(region == "Metropolitana de Santiago") %>%
  arrange(desc(fecha))

covid_c <- covid %>%
  filter(region == "Metropolitana de Santiago") %>%
  filter(!is.na(casos_confirmados)) %>%
  filter(fecha == max(fecha)) %>%
  select(comuna, casos_confirmados) %>%
  mutate(casos = as.numeric(casos_confirmados))


casen_p %>%
  left_join(covid_c) %>% 
  filter(comuna %in% c("La Florida", "Puente Alto", 
                       "Las Condes", "Santiago", "La Pintana",
                       "Cerrillos", "Ñuñoa", "Providencia",
                       "La Dehesa", "Lo Barnechea",
                       "Estación Central",
                       "San Bernardo")) %>%
  ggplot(aes(x = pobreza,
             y = casos,
             size = esc,
             alpha = numper,
             col = comuna)) +
  geom_point() +
  theme_light() +
  scale_size_continuous(range = c(2, 14)) +
  labs(y = "Casos activos de COVID-19 al 22 de enero de 2012",
       x = "Personas en situación de pobreza",
       size = "Años de escolaridad",
       alpha = "Número de personas por hogar",
       caption = "Fuentes: API COVID-19, Encuesta CASEN 2017")

#mirar app shiny ----
#https://bit.ly/relacionador_casen



#—----


#filtrar datos ----


#gráfico de densidad ----



#


#


#


#mirar app shiny ----
#https://bit.ly/filtrador_casen



#—---

#ver otras aplicaciones shiny más complejas ----
#https://linktr.ee/datauc
