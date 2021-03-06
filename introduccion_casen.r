#PARTE 1: introducción a la Casen

#instalar paquetes si es necesario
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("readstata13")

#cargar paquetes
library(dplyr)

#descargar casen: http://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen-2017

#cargar base de datos ----
casen <- readstata13::read.dta13("~/Casen/Casen 2017.dta")


#mirar la base  ----
casen
names(casen)
tibble(casen)
glimpse(casen)


#convertir a tibble ----
casen <- as_tibble(casen)


#contar casos de una variable----
casen %>%
  count(pobreza)

#filtrar por regiones ----
casen %>%
  count(region)

casen %>% 
  filter(region != "Región de La Araucanía")

#filtrar por otras condiciones ----
casen %>%
  count(pco1)

casen %>%
  filter(pco1 == "Jefe(a) de hogar",
         sexo == "Mujer")


#factor de expansión ----
#necesitamos aplicar factor de expansión...

nrow(casen)

#filtrar la región metropolitana

casen_rm <- casen %>%
  filter(region == "Región Metropolitana de Santiago")

casen_rm

nrow(casen_rm)

#seleccionar variables de interés
casen_rm

casen_rm %>%
  select(region, edad, sexo, pco1)


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
         #hasta acá las numéricas
         pco1,                    #jefe de hogar
         activ,                   #actividad
         hacinamiento,            #hacinamiento
         pobreza,                 #pobreza
         pobreza_multi_5d,        #pobreza multidimensional
         r1a,                     #nacionalidad
         r3,                      #pertenencia a pueblos originarios
         v12,                     #metros cuadrados de la casa
         indmat)                  #índice de materialidad de la vivienda


#expansión ----
#aplicar factor de expansión comunal (expc)
casen_rm2 <- tidyr::uncount(casen_rm1, weights = expc)

#comparar conteos
casen_rm %>%
  filter(pco1 == "Jefe(a) de hogar") %>%
  count(sexo)

casen_rm2 %>%
  filter(pco1 == "Jefe(a) de hogar") %>%
  count(sexo)

#resumir valores ----

#resumir valores: ingresos 
casen_rm %>%
  summarize(mean(ytrabajocorh, na.rm=T))

casen_rm2 %>%
  summarize(mean(ytrabajocorh, na.rm=T))

#resumir valores: ingresos 
casen_rm2 %>%
  summarize(mean(y26_2c, na.rm=T))

#agrupar operaciones ----
casen_rm2 %>%
  group_by(sexo) %>%
  summarize(mean(ytrabajocorh, na.rm=T))

casen_rm2 %>%
  group_by(comuna) %>%
  summarize(mean(ytrabajocorh, na.rm=T))

#crear nuevas variables ----
casen_rm2 %>%
  filter(pco1 == "Jefe(a) de hogar") %>%
  count(sexo, name = "cantidad") %>%
  mutate(porcentaje = cantidad/sum(cantidad))


