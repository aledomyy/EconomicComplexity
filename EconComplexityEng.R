#rm(list=ls())

# Opening Libraries -------------------------------------------------------------
library(tidyverse)
library(pivottabler)
library(economiccomplexity)
library(readr)


#Open data from Economic Census 2019, which has location per rows and
#economic activities per column, showing aggregate value in each cell. 
datos <- read_csv("vaprom_2019.csv")

#Must do pivot to meet requirements of economiccomplexity package
datos_p <- datos %>%
  pivot_longer(!`Row Labels`,names_to = "Producto",values_to = "VA")

#Change al NA's for 0's
datos_p[is.na(datos_p)] <- 0


# Balassa Index -------------------------------------------------------

#Usage of Balassa Index function of 'economiccomplexity'
bal <- balassa_index(datos_p,country = "Row Labels",
                     product = "Producto",
                     value = "VA")


#Convert from matrix to df, to then export as csv
bal_datos <- as.data.frame(as.matrix(bal)) %>%
  rownames_to_column()


#pivot 
datos_bal <- bal_datos %>%
  pivot_longer(!rowname, names_to = "Clase", values_to = "VC")

#Writing CSV 
write_csv(datos_bal,"balassa.csv")


# Complexity measures -------------------------------------------------
#Run the complexity measure
complejidad <- complexity_measures(bal)


#Obtain complexity index per location 
#This shows how complex is every location, with and standardized index
complej_municipio <- as.data.frame(complejidad$complexity_index_country)


#Write as csv
write_csv(complej_municipio, "complejidad_municipios.csv")


#Find complexity per economic activity
#Shows how complex is an economic activity, after standardizing
complej_prod <- as.data.frame(complejidad$complexity_index_product) %>%
  rownames_to_column("Producto") %>%
  mutate(comp_prod = complejidad$complexity_index_product) %>%
  select(!`complejidad$complexity_index_product`)
write_csv(complej_prod,"complejidad_ae.csv")



# Proximities ------------------------------------------------------------


#Calculating proximity measures
proximidad <- proximity(bal,"both")
prox_muni <- proximidad$proximity_country
prox_prod <- proximidad$proximity_product

#From matrix to data frame
proximidad_productos <- as.data.frame((as.matrix(prox_prod)))
proximidad_municipios <- as.data.frame(as.matrix(prox_muni))


# Complexity Outlook Gain/Index -------------------------------------------


#Find the complexity outlook (unexploited export opportunities)
#This gives both the Outlook Gain as the Outlook Index
outlook <- complexity_outlook(bal,prox_prod,complejidad$complexity_index_product)

#Data frame with the outlook gain
outlook_gain_df <- as.data.frame(as.matrix(outlook$complexity_outlook_gain)) %>%
  rownames_to_column()
#Pivot to create csv
ogi_df <- outlook_gain_df %>%
  pivot_longer(!rowname, names_to = "Clase", values_to = "Gain Index") 
write_csv(ogi_df,"outlook_gain_index.csv")

#data frame with the outlook index
coi<- as.data.frame(as.matrix(outlook$complexity_outlook_index)) %>%
  rownames_to_column()
write_csv(coi, "coi.csv")

