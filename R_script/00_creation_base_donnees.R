





## 00 - Chargement des packages de fonctions----

library(readxl)
library(data.table)
library(dplyr)

# Découpage du délai en modalités ordonnées
tranche_delai <- function(delai) {
  resultat <-
    factor(
      ifelse(
        delai < 7,
        "Dans la semaine",
        ifelse(
          delai < 14,
          "Dans les 2 semaines",
          ifelse(
            delai < 21,
            "Dans les 3 semaines",
            ifelse(
              delai < 30,
              "Dans le mois",
              ifelse(
                delai < 60,
                "Dans les 2 mois",
                ifelse(
                  delai < 90,
                  "Dans les 3 mois",
                  ifelse(delai < 180,
                         "Dans les 6 mois",
                         "Plus de 6 mois")
                )
              )
            )
          )
        )
      ),
      levels = c(
        "Dans la semaine",
        "Dans les 2 semaines",
        "Dans les 3 semaines",
        "Dans le mois",
        "Dans les 2 mois",
        "Dans les 3 mois",
        "Dans les 6 mois",
        "Plus de 6 mois"
      )
    )
  return(resultat)
}

# Nettoyage du nom de la ville pour une correspondance avec la table INSEE
nettoyage_nom_ville <- function(ville){
  ville <- gsub(" CEDEX", "", ville)
  ville <- gsub(" [[:digit:]]", "", ville)
  ville <- ifelse(ville == "PARIS", "PARIS 01", ville)
  ville <- ifelse(ville == "MARSEILLE", "MARSEILLE 01", ville)
  ville <- ifelse(ville == "ROMORANTIN", "ROMORANTIN LANTHENAY", ville)
  ville
}
## Exemple 
# nettoyage_nom_ville(c("THIAIS CEDEX 3", "MARSEILLE"))



## 01 - Chargement des données ----

## 01.A - Données géographiques ----

# souce : https://www.data.gouv.fr/fr/datasets/base-officielle-des-codes-postaux/
villes_france <-
  read.csv(
    file = "~/UnJourUneViz/acces_financier_soins/data/laposte_hexasmal.csv",
    header = TRUE,
    sep = ";",
    stringsAsFactors = FALSE
  )

str(villes_france)
list_coord <- strsplit(villes_france$coordonnees_gps, ",")
villes_france$lat <- as.numeric(sapply(
  X = 1:nrow(villes_france),
  FUN = function(x) {
    list_coord[[x]][1]
  }
))
villes_france$long <- as.numeric(sapply(
  X = 1:nrow(villes_france),
  FUN = function(x) {
    list_coord[[x]][2]
  }
))


saveRDS(villes_france,
        "~/UnJourUneViz/acces_financier_soins/data/villes_france.RDS")

## 01.B - Données gynécos 2012 ----

# source : https://www.data.gouv.fr/fr/datasets/delai-d-attente-rendez-vous-gynecologue-0/
Delai_rdv_gynecos_2012 <-
  read_excel("~/UnJourUneViz/acces_financier_soins/data/Delai_rdv_gynecos_2012.xlsx")
str(Delai_rdv_gynecos_2012)
colnames(Delai_rdv_gynecos_2012) <- c("ville",
                                      "date_appel",
                                      "date_rdv",
                                      "delai_rdv",
                                      "tarif",
                                      "depassement")
Delai_rdv_gynecos_2012$Nom_commune  <- nettoyage_nom_ville(Delai_rdv_gynecos_2012$ville)

Delai_rdv_gynecos_2012 <-
  left_join(x = Delai_rdv_gynecos_2012[, c("Nom_commune",
                                           "delai_rdv",
                                           "tarif",
                                           "depassement")],
            y = villes_france[, c("Nom_commune", "lat", "long")],
            by = "Nom_commune")

# Verif que toutes les villes sont trouvées
setdiff(Delai_rdv_gynecos_2012$Nom_commune, villes_france$Nom_commune)

# Dépassement en pourcentage
Delai_rdv_gynecos_2012$depassement_pct <-
  round(Delai_rdv_gynecos_2012$depassement / 28 * 100, 2)

# Var binaire : dépassement, pas de dépassement
Delai_rdv_gynecos_2012$pas_depassement <-
  ifelse(Delai_rdv_gynecos_2012$depassement == 0,
         "Tarif secteur 1 : 28€",
         "Dépassement")

# Délai du rdv en semaines
Delai_rdv_gynecos_2012$delai_rdv2 <-
  tranche_delai(Delai_rdv_gynecos_2012$delai_rdv)


saveRDS(
  Delai_rdv_gynecos_2012,
  "~/UnJourUneViz/acces_financier_soins/data/Delai_rdv_gynecos_2012.RDS"
)

## 01.C - Données ophtalmos 2012 ----

# source : https://www.data.gouv.fr/fr/datasets/delai-d-attente-rendez-vous-ophtalmologiste-0/
Delai_rdv_ophtalmos_2012 <-
  read_excel("~/UnJourUneViz/acces_financier_soins/data/Delai_rdv_ophtalmos_2012.xlsx")
str(Delai_rdv_ophtalmos_2012)
colnames(Delai_rdv_ophtalmos_2012) <- c("ville",
                                        "date_appel",
                                        "date_rdv",
                                        "delai_rdv",
                                        "tarif",
                                        "depassement")
Delai_rdv_ophtalmos_2012$Nom_commune  <- nettoyage_nom_ville(Delai_rdv_ophtalmos_2012$ville)

Delai_rdv_ophtalmos_2012 <-
  left_join(x = Delai_rdv_ophtalmos_2012[, c("Nom_commune",
                                             "delai_rdv",
                                             "tarif",
                                             "depassement")],
            y = villes_france[, c("Nom_commune", "lat", "long")],
            by = "Nom_commune")

# Verif que toutes les villes sont trouvées
setdiff(Delai_rdv_ophtalmos_2012$Nom_commune, villes_france$Nom_commune)

# Dépassement en pourcentage
Delai_rdv_ophtalmos_2012$depassement_pct <-
  round(Delai_rdv_ophtalmos_2012$depassement / 28 * 100, 2)

# Var binaire : dépassement, pas de dépassement
Delai_rdv_ophtalmos_2012$pas_depassement <-
  ifelse(Delai_rdv_ophtalmos_2012$depassement == 0,
         "Tarif secteur 1 : 28€",
         "Dépassement")

# Délai du rdv en semaines
Delai_rdv_ophtalmos_2012$delai_rdv2 <-
  tranche_delai(Delai_rdv_ophtalmos_2012$delai_rdv)


saveRDS(
  Delai_rdv_ophtalmos_2012,
  "~/UnJourUneViz/acces_financier_soins/data/Delai_rdv_ophtalmos_2012.RDS"
)


## 01.D - Données pédiatres 2012 ----

# source : https://www.data.gouv.fr/fr/datasets/delai-d-attente-rendez-vous-pediatre-0/
Delai_rdv_pediatres_2012 <-
  read_excel("~/UnJourUneViz/acces_financier_soins/data/Delai_rdv_pediatres_2012.xlsx")
str(Delai_rdv_pediatres_2012)
colnames(Delai_rdv_pediatres_2012) <- c("ville",
                                        "date_appel",
                                        "date_rdv",
                                        "delai_rdv",
                                        "tarif",
                                        "depassement")
Delai_rdv_pediatres_2012$Nom_commune  <- nettoyage_nom_ville(Delai_rdv_pediatres_2012$ville)

Delai_rdv_pediatres_2012 <-
  left_join(x = Delai_rdv_pediatres_2012[, c("Nom_commune",
                                             "delai_rdv",
                                             "tarif",
                                             "depassement")],
            y = villes_france[, c("Nom_commune", "lat", "long")],
            by = "Nom_commune")

# Verif que toutes les villes sont trouvées
setdiff(Delai_rdv_pediatres_2012$Nom_commune, villes_france$Nom_commune)

# Dépassement en pourcentage
Delai_rdv_pediatres_2012$depassement_pct <-
  round(Delai_rdv_pediatres_2012$depassement / 28 * 100, 2)

# Var binaire : dépassement, pas de dépassement
Delai_rdv_pediatres_2012$pas_depassement <-
  ifelse(Delai_rdv_pediatres_2012$depassement == 0,
         "Tarif secteur 1 : 28€",
         "Dépassement")

# Délai du rdv en semaines
Delai_rdv_pediatres_2012$delai_rdv2 <-
  tranche_delai(Delai_rdv_pediatres_2012$delai_rdv)


saveRDS(
  Delai_rdv_pediatres_2012,
  "~/UnJourUneViz/acces_financier_soins/data/Delai_rdv_pediatres_2012.RDS"
)

## 02 - Agregagtions des tables ----

## 02.A - Concaténation des tables ----
Delai_rdv_gynecos_2012$specialiste <- "Gynécolgues"
Delai_rdv_ophtalmos_2012$specialiste <- "Opthalmologues"
Delai_rdv_pediatres_2012$specialiste <- "Pédiatres"

Delai_rdv_concat <- as.data.table(
  rbind(
    Delai_rdv_gynecos_2012,
    Delai_rdv_ophtalmos_2012,
    Delai_rdv_pediatres_2012
  )
)

Delai_rdv_agreg <-
  Delai_rdv_concat[, .(
    depassement_moy = mean(depassement),
    depassement_med = median(depassement),
    delai_moy = mean(delai_rdv),
    delai_med = median(delai_rdv)
  ),
  by = .(specialiste, Nom_commune, lat, long)]
Delai_rdv_agreg[, c("depassement_moy_pct",
                    "depassement_med_pct",
                    "delai_moy_fct",
                    "delai_med_fct",
                    "mytext") := .(
                      round(depassement_moy / 28 * 100, 2),
                      round(depassement_med / 28 * 100, 2),
                      tranche_delai(delai_moy),
                      tranche_delai(delai_med),
                      paste("Ville :", Nom_commune,
                            "\n Délais moyen :", floor(delai_moy),
                            "jours \n Dépassement moyen :",round(depassement_moy / 28 * 100, 2),
                            "%")
                    )]

saveRDS(
  Delai_rdv_agreg,
  "~/UnJourUneViz/acces_financier_soins/data/Delai_rdv_agreg.RDS"
)

# Etude complete : https://www.quechoisir.org/action-ufc-que-choisir-acces-aux-soins-en-france-la-fracture-s-aggrave-n21799/

