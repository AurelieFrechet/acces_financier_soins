## 00 - Chargement des packages

library(readxl)
library(tidyverse)
library(maps)
library(ggplot2)
library(pipeR)

## 01 - Chargement des données

carte_france <- map_data("world") %>% filter(region == "France")
Delai_rdv_ophtalmos_2012 <-
  readRDS("~/UnJourUneViz/Accès financier aux soins/data/Delai_rdv_ophtalmos_2012.RDS")
Delai_rdv_pediatres_2012 <-
  readRDS("~/UnJourUneViz/Accès financier aux soins/data/Delai_rdv_pediatres_2012.RDS")
Delai_rdv_gynecos_2012 <-
  readRDS("~/UnJourUneViz/Accès financier aux soins/data/Delai_rdv_gynecos_2012.RDS")

## 02 - Carte de France gyneco

ggplot() +
  geom_polygon(
    data = carte_france,
    aes(x = long,
        y = lat,
        group = group),
    fill = "grey70",
    alpha = 0.7
  ) +
  geom_point(
    data = Delai_rdv_gynecos_2012,
    aes(
      x = long,
      y = lat,
      color = delai_rdv2,
      size = depassement_pct
    )
  ) +
  scale_radius(name = "Depassement moy.en en %",
               range = c(2, 14)) +
  scale_color_brewer(palette = "RdPu")+
  # scale_color_continuous(
  #   name = "Délai moy. en jours",
  #   # breaks = NULL,
  #   limits = c(0, 365),
  #   low = "maroon",
  #   high = "black",
  #   space = "Lab",
  #   guide = "colourbar"
  # ) +
  guides(size = guide_legend(override.aes = list(colour = "grey70")),
         shape = guide_legend(override.aes = list(colour = "grey70"))) +
  theme_void() + coord_map()


## 03 - Distribution délai / dépassement
ggplot(data = Delai_rdv_gynecos_2012,
       aes(x = delai_rdv_sem,
           y = depassement_pct,
           color = delai_rdv2)) +
  geom_point()

ggplot(data = Delai_rdv_gynecos_2012)+
  geom_bar(aes(x=delai_rdv2))

quantile(Delai_rdv_gynecos_2012$delai_rdv)

##• Top dépassement - delai
head(unique(Delai_rdv_gynecos_2012[order(Delai_rdv_gynecos_2012$depassement, decreasing = T),
                                   c("ville", "tarif")]))

head(unique(Delai_rdv_gynecos_2012[order(Delai_rdv_gynecos_2012$delai_rdv, decreasing = T),
                                   c("ville", "delai_rdv")]))
