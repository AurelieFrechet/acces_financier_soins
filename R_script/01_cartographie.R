
## 00 - Chargement des packages

library(readxl)
library(tidyverse)
library(maps)
library(ggplot2)
library(pipeR)
library(highcharter)
library(plotly)

## 01 - Chargement des données

carte_france <- map_data("world") %>% filter(region == "France")
Delai_rdv_ophtalmos_2012 <-
  readRDS("~/UnJourUneViz/Accès financier aux soins/data/Delai_rdv_ophtalmos_2012.RDS")
Delai_rdv_pediatres_2012 <-
  readRDS("~/UnJourUneViz/Accès financier aux soins/data/Delai_rdv_pediatres_2012.RDS")
Delai_rdv_gynecos_2012 <-
  readRDS("~/UnJourUneViz/Accès financier aux soins/data/Delai_rdv_gynecos_2012.RDS")
Delai_rdv_agreg<-
  readRDS("~/UnJourUneViz/acces_financier_soins/data/Delai_rdv_agreg.RDS"
)






