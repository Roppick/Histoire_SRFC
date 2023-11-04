# --------------------- Chargement des librairies ------------------------------
library(worldfootballR)
library(tidyverse)
library(dplyr)
library(ggh4x) # pour colorier le delta entre les deux coubres
library(lubridate) # pour transfomer les dates
library(zoo) # pour les moyennes glissantes
library(showtext) # pour importer les polices de caractères
library(ggpmisc) # pour faire des zones d'ombrage en fond
library(gginnards) #pour gérer les couches
library(scales)  # pour les chiffres significatifs des axes
library(magick) #lire les images
library(grid) #Ajouter les images aux bonnes positions

# ---------------------- Import des Données ------------------------------------
SRFC_url <- c("https://fbref.com/en/squads/b3072e00/2023-2024/all_comps/Rennes-Stats-All-Competitions",
              "https://fbref.com/en/squads/b3072e00/2022-2023/all_comps/Rennes-Stats-All-Competitions",
              "https://fbref.com/en/squads/b3072e00/2021-2022/all_comps/Rennes-Stats-All-Competitions",
              "https://fbref.com/en/squads/b3072e00/2020-2021/all_comps/Rennes-Stats-All-Competitions",
              "https://fbref.com/en/squads/b3072e00/2019-2020/all_comps/Rennes-Stats-All-Competitions",
              "https://fbref.com/en/squads/b3072e00/2018-2019/all_comps/Rennes-Stats-All-Competitions"
              #"https://fbref.com/en/squads/b3072e00/2017-2018/all_comps/Rennes-Stats-All-Competitions"
)
match_log_SRFC <- fb_team_match_log_stats(team_urls = SRFC_url, stat_type = "shooting")
match_log_SRFC_xG_pour <- match_log_SRFC[match_log_SRFC$ForAgainst == "For",]
match_log_SRFC_xG_pour_nettoyé <- match_log_SRFC_xG_pour[,c("Date","Opponent","xG_Expected")]
names(match_log_SRFC_xG_pour_nettoyé)[names(match_log_SRFC_xG_pour_nettoyé) == "xG_Expected"] <- "xG"

match_log_SRFC_xG_contre <- match_log_SRFC[match_log_SRFC$ForAgainst == "Against",]
match_log_SRFC_xG_contre_nettoyé <- match_log_SRFC_xG_contre[,c("Date","xG_Expected")]
names(match_log_SRFC_xG_contre_nettoyé)[names(match_log_SRFC_xG_contre_nettoyé) == "xG_Expected"] <- "xGA"

regroupement_xG_xGA <- cbind(match_log_SRFC_xG_pour_nettoyé,match_log_SRFC_xG_contre_nettoyé)
regroupement_xG_xGA_nettoyé <- regroupement_xG_xGA[,c("Date","xG","xGA")]

# Supprimer les lignes contenants des NA
# regroupement_xG_xGA_nettoyé[!grepl("NA", regroupement_xG_xGA_nettoyé$xG),]
regroupement_xG_xGA_nettoyé_sans_NA <- regroupement_xG_xGA_nettoyé %>% 
                                      filter(!is.na(xG))

# Ranger le tableau dans l'ordre des date
regroupement_xG_xGA_rangé_sans_NA <- regroupement_xG_xGA_nettoyé_sans_NA[order(regroupement_xG_xGA_nettoyé_sans_NA$Date),]
# Extraire juste les dates pour les passer au bon format
regroupement_xG_xGA_date <- ymd(regroupement_xG_xGA_rangé_sans_NA$Date)
# Remettre les dates au bon format dans le tableau
regroupement_xG_xGA_rangé_date_au_bon_format <- cbind(regroupement_xG_xGA_rangé_sans_NA,regroupement_xG_xGA_date)
# Vérifier le format du tableau, notamment des dates
# str(regroupement_xG_xGA_rangé_date_au_bon_format)
# Supprimer la première colonne avec les mauvaises dates
regroupement_xG_xGA_rangé_date_au_bon_format <- regroupement_xG_xGA_rangé_date_au_bon_format[,-1]

#Renommer la nouvelle colonne de date
names(regroupement_xG_xGA_rangé_date_au_bon_format)[names(regroupement_xG_xGA_rangé_date_au_bon_format) == "regroupement_xG_xGA_date"] <- "Dates"

# duppliquer les données pour en faire des moyennes glissantes
regroupement_xG_xGA_rangé_date_au_bon_format$Moyenne_xG <- regroupement_xG_xGA_rangé_date_au_bon_format$xG
regroupement_xG_xGA_rangé_date_au_bon_format$Moyenne_xGA <- regroupement_xG_xGA_rangé_date_au_bon_format$xGA

# Nouveau nom court pour le tableau
xG_xGA_SRFC <- regroupement_xG_xGA_rangé_date_au_bon_format

# Calcul des moyennes glissantes
xG_xGA_SRFC <- xG_xGA_SRFC %>%
  dplyr::mutate(Moyenne_xG = zoo::rollmean(Moyenne_xG, k = 9, fill = NA)) %>%
  dplyr::mutate(Moyenne_xGA = zoo::rollmean(Moyenne_xGA, k = 9, fill = NA))

# Suppression des NA en début et fin + ajouter les n° de ligne
xG_xGA_SRFC_sans_NA <- xG_xGA_SRFC %>%
                      filter(!is.na(Moyenne_xG)) %>%
                      mutate(Matchs=row_number())

# Supprimer pour ne plus avoir que les moyennes glissantes
xG_xGA_SRFC_ac_juste_moyennes <- select(xG_xGA_SRFC_sans_NA, -c(xG,xGA))

### -------------------------Ajout des entraineurs-------------------------------
# Ajout des colonnes pour les entraineurs
# On créé des vecteurs de la bonne taille
Lamouchi <- rep(NA, nrow(xG_xGA_SRFC_ac_juste_moyennes))
Stéphan <- rep(NA, nrow(xG_xGA_SRFC_ac_juste_moyennes))
Génésio <- rep(NA, nrow(xG_xGA_SRFC_ac_juste_moyennes))

# Dates pour les coachs
# Lamouchi  début:18/11/2017  fin:02/12/2018
# Stéphan   début:05/12/2018  fin:26/02/2021
# Génésio   début:03/03/2021  fin:

xG_xGA_SRFC_avec_coachs <- cbind(xG_xGA_SRFC_ac_juste_moyennes,Lamouchi,Stéphan,Génésio)

date_fin_lamouchi <- as.Date("2018-12-02")
ligne_fin_lamouchi <- which(grepl(date_fin_lamouchi, xG_xGA_SRFC_avec_coachs$Dates))
colonne_lamouchi <- which(colnames(xG_xGA_SRFC_avec_coachs) == "Lamouchi")
xG_xGA_SRFC_avec_coachs[ligne_fin_lamouchi, colonne_lamouchi] <- 0.2
xG_xGA_SRFC_avec_coachs <- xG_xGA_SRFC_avec_coachs %>% fill(Lamouchi, .direction = 'up')

date_début_Stéphan <- as.Date("2018-12-05")
date_fin_Stéphan <- as.Date("2021-02-26")
ligne_début_Stéphan <- which(grepl(date_début_Stéphan, xG_xGA_SRFC_avec_coachs$Dates))
ligne_fin_Stéphan <- which(grepl(date_fin_Stéphan, xG_xGA_SRFC_avec_coachs$Dates))
colonne_Stéphan <- which(colnames(xG_xGA_SRFC_avec_coachs) == "Stéphan")
xG_xGA_SRFC_avec_coachs[ligne_début_Stéphan-1, colonne_Stéphan] <- 0
xG_xGA_SRFC_avec_coachs[ligne_début_Stéphan, colonne_Stéphan] <- 0.2
xG_xGA_SRFC_avec_coachs[ligne_fin_Stéphan, colonne_Stéphan] <- 0.2
xG_xGA_SRFC_avec_coachs <- xG_xGA_SRFC_avec_coachs %>% fill(Stéphan, .direction = 'up')
xG_xGA_SRFC_avec_coachs[xG_xGA_SRFC_avec_coachs == 0] = NA

date_début_Génésio <- as.Date("2021-03-03")
ligne_début_Génésio <- which(grepl(date_début_Génésio, xG_xGA_SRFC_avec_coachs$Dates))
colonne_Génésio <- which(colnames(xG_xGA_SRFC_avec_coachs) == "Génésio")
xG_xGA_SRFC_avec_coachs[ligne_début_Génésio, colonne_Génésio] <- 0.2
xG_xGA_SRFC_avec_coachs <- xG_xGA_SRFC_avec_coachs %>% fill(Génésio, .direction = 'down')


xG_xGA_SRFC_final <- xG_xGA_SRFC_avec_coachs
names(xG_xGA_SRFC_final)[names(xG_xGA_SRFC_final) == "Moyenne_xG"] <- "xG"
names(xG_xGA_SRFC_final)[names(xG_xGA_SRFC_final) == "Moyenne_xGA"] <- "xGA"

## -------------------------Ajout des saisons----------------------------------
# 18-19 début 2018-09-14  fin 2019-05-24
# 19-20 début 2019-08-10  fin 2020-03-08
# 20-21 début 2020-08-22  fin 2021-05-23
# 21-22 début 2021-08-08  fin 2022-05-21
# 22-23 début 2022-08-07  fin 2023-06-03
# 23-24 début 2023-08-13  fin

ligne_début_18_19 <- which(grepl("2018-09-14", xG_xGA_SRFC_final$Dates))
ligne_fin_18_19 <- which(grepl("2019-05-24", xG_xGA_SRFC_final$Dates))
ligne_début_19_20 <- which(grepl("2019-08-10", xG_xGA_SRFC_final$Dates))
ligne_fin_19_20 <- which(grepl("2020-03-08", xG_xGA_SRFC_final$Dates))
ligne_début_20_21 <- which(grepl("2020-08-22", xG_xGA_SRFC_final$Dates))
ligne_fin_20_21 <- which(grepl("2021-05-23", xG_xGA_SRFC_final$Dates))
ligne_début_22_23 <- which(grepl("2022-08-07", xG_xGA_SRFC_final$Dates))
ligne_fin_22_23 <- which(grepl("2023-06-03", xG_xGA_SRFC_final$Dates))
# shade <- data.frame(Matchs = c(ligne_début_18_19, ligne_fin_18_19),
#                     # x2 = c(ligne_début_19_20, ligne_fin_19_20),
#                     min = c(-Inf, -Inf), max = c(Inf, Inf))

## -------------------------Dates importantes----------------------------------
date_Leicester <- as.Date("2022-03-10")
ligne_Leicester <- which(grepl(date_Leicester, xG_xGA_SRFC_avec_coachs$Dates))
date_LDC <- as.Date("2020-10-20")
ligne_LDC <- which(grepl(date_LDC, xG_xGA_SRFC_avec_coachs$Dates))
date_CdF <- as.Date("2019-04-19")
ligne_CdF <- which(grepl(date_CdF, xG_xGA_SRFC_avec_coachs$Dates))
date_Arsenal <- as.Date("2019-03-07")
ligne_Arsenal <- which(grepl(date_Arsenal, xG_xGA_SRFC_avec_coachs$Dates))
date_Shakhtar <- as.Date("2023-02-23")
ligne_Shakhtar <- which(grepl(date_Shakhtar, xG_xGA_SRFC_avec_coachs$Dates))
date_Terrier <- as.Date("2023-01-02")
ligne_Terrier <- which(grepl(date_Terrier, xG_xGA_SRFC_avec_coachs$Dates))

## ------------------------Importer la police de charactere-------------------
font_add(family = "CMU Serif", 
         regular = "cmunrm.otf",
         bold = "cmunbx.otf")
showtext_auto()

## --------------------- Importer les logos -----------------------------------
# Twitter
logotwitter <- image_read("~/Documents/Stade Rennais/Ressources/Twitter_gris.png")
image_scale(logotwitter, "600") # échelle
image_trim(logotwitter) #supprimer le fond pour un png

#Github
logogithub <- image_read("~/Documents/Stade Rennais/Ressources/Github_gris.png")
image_scale(logogithub, "600") # échelle
image_trim(logogithub) #supprimer le fond pour un png

#Bluesky
logobluesky <- image_read("~/Documents/Stade Rennais/Ressources/Bluesky_gris.png")
image_scale(logobluesky, "600") # échelle
image_trim(logobluesky) #supprimer le fond pour un png

#Fbref
logofbref <- image_read("~/Documents/Stade Rennais/Ressources/Fbref.png")
image_scale(logofbref, "600") # échelle
image_trim(logofbref) #supprimer le fond pour un png

#Leicester
logoLeicester <- image_read("~/Documents/Stade Rennais/Ressources/Leicester.png")
image_scale(logoLeicester, "600") # échelle
image_trim(logoLeicester) #supprimer le fond pour un png
logoLeicester_final <- rasterGrob(logoLeicester, interpolate=TRUE)

#Arsenal
logoArsenal <- image_read("~/Documents/Stade Rennais/Ressources/Arsenal.png")
image_scale(logoArsenal, "600") # échelle
image_trim(logoArsenal) #supprimer le fond pour un png
logoArsenal_final <- rasterGrob(logoArsenal, interpolate=TRUE)

#LDC
logoLDC <- image_read("~/Documents/Stade Rennais/Ressources/LDC.png")
image_scale(logoLDC, "600") # échelle
image_trim(logoLDC) #supprimer le fond pour un png
logoLDC_final <- rasterGrob(logoLDC, interpolate=TRUE)

#CdF
logoCdF <- image_read("~/Documents/Stade Rennais/Ressources/CdF.png")
image_scale(logoCdF, "600") # échelle
image_trim(logoCdF) #supprimer le fond pour un png
logoCdF_final <- rasterGrob(logoCdF, interpolate=TRUE)

#Shakhtar
logoShakhtar <- image_read("~/Documents/Stade Rennais/Ressources/Shakhtar.png")
image_scale(logoShakhtar, "600") # échelle
image_trim(logoShakhtar) #supprimer le fond pour un png
logoShakhtar_final <- rasterGrob(logoShakhtar, interpolate=TRUE)

#Ambulance
logoAmbulance <- image_read("~/Documents/Stade Rennais/Ressources/Ambulance.png")
image_scale(logoAmbulance, "600") # échelle
image_trim(logoAmbulance) #supprimer le fond pour un png
logoAmbulance_final <- rasterGrob(logoAmbulance, interpolate=TRUE)

# --------------------------Représentation Graphique---------------------------

## ------------------------Définition du titre et sous-titre------------------
mon_titre = "Evolution des xG pour et contre le SRFC depuis la saison 2018-2019"
mon_sous_titre = "Graphique généré sur la base des données du site Fbref.com. La courbe est lissées sur la base d'une moyenne glissante sur 9 matchs. Les données ne sont pas disponibles pour l'ensemble des matchs\nnotamment pour la coupe de France. De ce cas, les matchs ont été ignorés pour ne pas fausser les moyennes glissantes."
monbasdegraph = "@Roppick            Roppick            @roppick.bsky.social"
## ------------------------Définition du graphique---------------------------

p <- ggplot(xG_xGA_SRFC_final, aes(x = Matchs, xmin=4, xmax=120, ymin=0, ymax=3)) +
### --------------------------Ajouter les saisons--------------------------------
  annotate("rect", fill = "#D4CEC2", alpha = 0.2, 
           xmin = ligne_début_18_19, xmax = ligne_fin_18_19,
           ymin = 0, ymax = 3) +
  annotate(geom="text", x=22, y=2.875, label="2018-2019",
           color="white", size = 15, family = "CMU Serif", fontface =2) +
  annotate(geom="text", x=62, y=2.875, label="2019-2020",
           color="#D4CEC2", size = 15, family = "CMU Serif", fontface =2) +
  annotate("rect", fill = "#D4CEC2", alpha = 0.2, 
           xmin = ligne_début_20_21, xmax = ligne_fin_20_21,
           ymin = 0, ymax = 3) +
  annotate(geom="text", x=100.5, y=2.875, label="2020-2021",
           color="white", size = 15, family = "CMU Serif", fontface =2) +
  annotate(geom="text", x=145, y=2.875, label="2021-2022",
           color="#D4CEC2", size = 15, family = "CMU Serif", fontface =2) +
  annotate("rect", fill = "#D4CEC2", alpha = 0.2, 
           xmin = ligne_début_22_23, xmax = ligne_fin_22_23,
           ymin = 0, ymax = 3) +
  annotate(geom="text", x=190.5, y=2.875, label="2022-2023",
           color="white", size = 15, family = "CMU Serif", fontface =2) +
  annotate(geom="text", x=220, y=2.875, label="2023\n2024",
           color="#D4CEC2", size = 15, family = "CMU Serif", fontface =2) +
  # Une ligne pour les xG
  geom_line(aes(y = xG),
            color = "#c00000",
            linewidth = 2,
            lineend = "round",
            show.legend = FALSE) +
  # Une ligne pour les xGA
  geom_line(aes(y = xGA),
            color = "#000000",
            linewidth = 1.8,
            lineend = "round",
            show.legend = FALSE) +
  
  ### ------------------- Ajout des Entraineurs -------------------------------
  
  # Une ligne pour Lamouchi
  geom_line(aes(y = Lamouchi),
            color = "#A6A6A6",
            linewidth = 3.8,
            lineend = "round",
            show.legend = FALSE) +
  # Une ligne pour Stéphan
  geom_line(aes(y = Stéphan),
            color = "#7F7F7F",
            linewidth = 3.8,
            lineend = "round",
            show.legend = FALSE) +
  # Une ligne pour Génésio
  geom_line(aes(y = Génésio),
            color = "#404040",
            linewidth = 3.8,
            lineend = "round",
            show.legend = FALSE) +
  # Ajouter le nom des entraineurs
  annotate(geom="text", x=8.5, y=0.15, label="Lamouchi",
           color="#A6A6A6", size = 10, family = "CMU Serif", fontface =2) +
  annotate(geom="text", x=63.5, y=0.15, label="Stéphan",
           color="#7F7F7F", size = 10, family = "CMU Serif", fontface =2) +
  annotate(geom="text", x=166.5, y=0.15, label="Génésio",
           color="#404040", size = 10, family = "CMU Serif", fontface =2) +
  
  # Colorier la différence entre les deux 
  stat_difference(aes(ymin = xG, ymax = xGA),
                  levels = c("plus de xGA","plus de xG"),
                  alpha = 0.3,#permet d'ajuster la transparence
                  show.legend = FALSE 
                  ) + 
   # Ajouter un titre
  ggtitle(mon_titre) +
  # AJouter un secondaire axe y, permet aussi d'ajuster la taille des incréments
  scale_y_continuous("xG",
                     breaks = seq(0,3,by = 0.5),
                     sec.axis = sec_axis(
                       ~ . * 1.0,
                       name = "xGA",
                       labels = number_format(accuracy = 0.1),
                       breaks = seq(0,3,by = 0.5))) +
                     
  # ajuster les couleurs de remplissages des trucs
  scale_fill_manual(
    values = c(
      colorspace::lighten("#000000"),
      colorspace::lighten("#c00000"),
      "grey60"
    )) +
  
### ----------------- Ajouter des dates importantes ----------------------------
#### ---------------- Lignes ---------------------------------------------------
# Leicester  
  annotate(
    "segment",
    x = ligne_Leicester,
    xend = ligne_Leicester,
    y = 0.75,
    yend = 2.25,
    colour = "#BF9000",
    linetype = 2,
    linewidth = 1.5
  ) +
  # LDC
  annotate(
    "segment",
    x = ligne_LDC,
    xend = ligne_LDC,
    y = 0.75,
    yend = 2.25,
    colour = "#BF9000",
    linetype = 2,
    linewidth = 1.5
  ) +
  # CdF
  annotate(
    "segment",
    x = ligne_CdF,
    xend = ligne_CdF,
    y = 0.875,
    yend = 2.375,
    colour = "#BF9000",
    linetype = 2,
    linewidth = 1.5
  ) +
  # Arsenal
  annotate(
    "segment",
    x = ligne_Arsenal,
    xend = ligne_Arsenal,
    y = 0.75,
    yend = 2.25,
    colour = "#BF9000",
    linetype = 2,
    linewidth = 1.5
  ) +
  # Shakhtar
  annotate(
    "segment",
    x = ligne_Shakhtar,
    xend = ligne_Shakhtar,
    y = 0.875,
    yend = 2.375,
    colour = "#BF9000",
    linetype = 2,
    linewidth = 1.5
  ) +
  # Terrier
  annotate(
    "segment",
    x = ligne_Terrier,
    xend = ligne_Terrier,
    y = 0.75,
    yend = 2.25,
    colour = "#BF9000",
    linetype = 2,
    linewidth = 1.5
  ) +
  
#### ---------------- Logos ---------------------------------------------------
  annotation_custom(logoLeicester_final, xmin=ligne_Leicester-4, xmax=ligne_Leicester+4, ymin=1.55) +
  annotation_custom(logoArsenal_final, xmin=ligne_Arsenal-3, xmax=ligne_Arsenal+3, ymin=1.55) +
  annotation_custom(logoLDC_final, xmin=ligne_LDC-4, xmax=ligne_LDC+4, ymin=1.55) +
  annotation_custom(logoCdF_final, xmin=ligne_CdF-4, xmax=ligne_CdF+4, ymin=1.75) +
  annotation_custom(logoShakhtar_final, xmin=ligne_Shakhtar-3, xmax=ligne_Shakhtar+3, ymin=1.75) +
  annotation_custom(logoAmbulance_final, xmin=ligne_Terrier-3, xmax=ligne_Terrier+3, ymin=1.45) +
  
  ## ---------------- Ajout d'un sous-Titre-------------------------------------
  labs(subtitle = mon_sous_titre, caption = monbasdegraph) +
  
  ## ---------------- Options du graphique -------------------------------------
  theme(
  # CHoix de la police pour tous les textes
  text = element_text(family = "CMU Serif", size = 12), 
  # choix de la police pour le titre seulement,
  plot.title = element_text(family = "CMU Serif", face = "bold", size = 60, hjust = 0.5, vjust = 0.5)
  , plot.subtitle = element_text(family = "CMU Serif", face = "bold", size = 25, hjust = 0.5, vjust = -4, lineheight = 1.5, colour = "#747474")
  , plot.caption = element_text(family = "CMU Serif", face = "bold", size = 30, hjust = 0.5, vjust = -18, lineheight = 1.5, colour = "#747474")
  # Choix de la couleur pour le fond de la zone de graphique
  , panel.background = element_rect("#f8f7f5")
  , plot.background = element_rect("#f8f7f5")
  , plot.margin = unit(c(6,8,6,8), "cm") #top, right, bottom, and left
  # Ajuster les axes horizontales et verticales pour qu'on ne les voit pas
  , axis.line = element_line(color = "#f8f7f5")
  , axis.title = element_text(size = 35, face = "bold") #taille des titres d'axe
  , axis.title.x.bottom = element_text(vjust = -2)
  , axis.title.y.left = element_text(vjust = 5, colour = "#c00000")
  , axis.title.y.right = element_text(vjust = 5)
  , axis.text = element_text(size = 35) #taille des textes de graduations
  , axis.text.y.left = element_text(hjust = -2, colour = "#c00000")
  , axis.text.y.right = element_text(hjust = 2)
  , panel.grid.major.x = element_line(colour = NA)
  , panel.grid.minor.x = element_line(colour = NA)
  , panel.grid.major.y = element_line(color = "#D9D9D9",
                                      linewidth = 1 ,
                                      linetype = "dashed")
  , panel.grid.minor.y = element_line(color = "#D9D9D9",
                                      linewidth = 1 ,
                                      linetype = "dashed")
  , axis.ticks = element_line(colour = NA)
  , panel.ontop = FALSE
  ) +
  
  ## ----------------------- Ajout des flèches pour le classement -------------
  #Fin 18-19
  annotate(
    geom = "curve",
    x = ligne_début_19_20+5,
    y = xG_xGA_SRFC_final$xGA[xG_xGA_SRFC_final$Matchs == ligne_début_19_20]+0.2,
    xend = ligne_début_19_20-1,
    yend = xG_xGA_SRFC_final$xGA[xG_xGA_SRFC_final$Matchs == ligne_début_19_20-1]+0.01, 
    curvature = .3, arrow = arrow(length = unit(4, "mm"), type = "closed"),
    colour = "#747474"
  ) +
  annotate(geom="text",
           x=ligne_début_19_20+6,
           y=xG_xGA_SRFC_final$xGA[xG_xGA_SRFC_final$Matchs == ligne_début_19_20]+0.23,
           label="10e de L1",
           color="#747474", size = 10, family = "CMU Serif", fontface =2) +
  
  #Fin 19-20
  annotate(
    geom = "curve",
    x = ligne_début_20_21-4,
    y = xG_xGA_SRFC_final$xG[xG_xGA_SRFC_final$Matchs == ligne_début_20_21]+0.2,
    xend = ligne_début_20_21-1,
    yend = xG_xGA_SRFC_final$xG[xG_xGA_SRFC_final$Matchs == ligne_début_20_21-1]+0.01, 
    curvature = .2, arrow = arrow(length = unit(4, "mm"), type = "closed"),
    colour = "#747474"
  ) +
  annotate(geom="text",
           x=ligne_début_20_21-4,
           y=xG_xGA_SRFC_final$xG[xG_xGA_SRFC_final$Matchs == ligne_début_20_21]+0.23,
           label="3e de L1",
           color="#747474", size = 10, family = "CMU Serif", fontface =2) +
  
  #Fin 20-21
  annotate(
    geom = "curve",
    x = ligne_fin_20_21-5,
    y = xG_xGA_SRFC_final$xGA[xG_xGA_SRFC_final$Matchs == ligne_fin_20_21]+0.2,
    xend = ligne_fin_20_21,
    yend = xG_xGA_SRFC_final$xGA[xG_xGA_SRFC_final$Matchs == ligne_fin_20_21]+0.01, 
    curvature = -.3, arrow = arrow(length = unit(4, "mm"), type = "closed"),
    colour = "#747474"
  ) +
  annotate(geom="text",
           x=ligne_fin_20_21-6,
           y=xG_xGA_SRFC_final$xGA[xG_xGA_SRFC_final$Matchs == ligne_fin_20_21]+0.23,
           label="6e de L1",
           color="#747474", size = 10, family = "CMU Serif", fontface =2) +
  
  #Fin 21-22
  annotate(
    geom = "curve",
    x = ligne_début_22_23-5,
    y = xG_xGA_SRFC_final$xG[xG_xGA_SRFC_final$Matchs == ligne_début_22_23]+0.3,
    xend = ligne_début_22_23,
    yend = xG_xGA_SRFC_final$xG[xG_xGA_SRFC_final$Matchs == ligne_début_22_23]+0.01, 
    curvature = -.2, arrow = arrow(length = unit(4, "mm"), type = "closed"),
    colour = "#747474"
  ) +
  annotate(geom="text",
           x=ligne_début_22_23-5,
           y=xG_xGA_SRFC_final$xG[xG_xGA_SRFC_final$Matchs == ligne_début_22_23]+0.33,
           label="4e de L1",
           color="#747474", size = 10, family = "CMU Serif", fontface =2) +
  
  #Fin 22-23
  annotate(
    geom = "curve",
    x = ligne_fin_22_23+6,
    y = xG_xGA_SRFC_final$xG[xG_xGA_SRFC_final$Matchs == ligne_fin_22_23]+0.2,
    xend = ligne_fin_22_23,
    yend = xG_xGA_SRFC_final$xG[xG_xGA_SRFC_final$Matchs == ligne_fin_22_23]+0.01, 
    curvature = -.3, arrow = arrow(length = unit(4, "mm"), type = "closed"),
    colour = "#747474"
  ) +
  annotate(geom="text",
           x=ligne_fin_22_23+6,
           y=xG_xGA_SRFC_final$xG[xG_xGA_SRFC_final$Matchs == ligne_fin_22_23]+0.23,
           label="4e de L1",
           color="#747474", size = 10, family = "CMU Serif", fontface =2) +

#---------------------- Export du graphique ----------------------------------
png(filename = "~/Documents/Stade Rennais/02_Code R/Histoire_SRFC/23-11-04_xG&xGA_SRFC_18-19_23-24.png",
    width = 5000,
    height = 3000)

p

## ---------------------- Ajout des logos RS -----------------------------------
grid.raster(logotwitter,
            x = 0.375, # latéral, augmenter pour aller à droite
            y = 0.015, # hauteur
            width = unit(75, "points"))

grid.raster(logogithub,
            x = 0.45, # latéral, augmenter pour aller à droite
            y = 0.015, # hauteur
            width = unit(75, "points"))

grid.raster(logobluesky,
            x = 0.52, # latéral, augmenter pour aller à droite
            y = 0.015, # hauteur
            width = unit(66, "points"))

grid.raster(logofbref,
            x = 0.95, # latéral, augmenter pour aller à droite
            y = 0.015, # hauteur
            width = unit(200, "points"))

dev.off() # Finally, close the “device”, or file