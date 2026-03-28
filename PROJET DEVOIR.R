# 1/Chargement des librairies
library(readxl)
library(ggplot2)
# 2/Chargement de la BD
data <- read_excel("C:/Almamy/projet_env_thies_qualite_air. Projet Devoir xlsx.xlsx")
View(data)
str(data)
summary(data)
#Commenataire:Le summary(data) montre une forte variabilité des polluants atmosphériques, en particulier du PM2.5, ainsi qu’une diversité des niveaux de bruit et des conditions environnementales. Les valeurs observées sont cohérentes avec le contexte local et confirment la pertinence des analyses univariées et bivariées menées par la suite.
##2)TRAVAIL DEMANDER
#A.Nettoyage et preparation
 #1.calcul du taux de NA par variable+visualiser
na_pct <- sapply(data, function(x) mean(is.na(x)) * 100)
na_pct
barplot(
  na_pct,
  las = 2,
  col = "steelblue",
  main = "Pourcentage de valeurs manquantes par variable",
  ylab = "Pourcentage (%)"
)
#commentaire:Certaines variables présentent des valeurs manquantes, ce qui nécessite un traitement avant l’analyse statistique afin d’éviter des biais.

 #2.Traiter  les NA(au choix) 
 #Imputation simple(mediane/mode)
data$PM25_ugm3[is.na(data$PM25_ugm3)] <- median(data$PM25_ugm3, na.rm = TRUE)
data$NO2_ppb[is.na(data$NO2_ppb)]     <- median(data$NO2_ppb, na.rm = TRUE)
data$Bruit_dB[is.na(data$Bruit_dB)]   <- median(data$Bruit_dB, na.rm = TRUE)
data$NDVI[is.na(data$NDVI)]           <- median(data$NDVI, na.rm = TRUE)
data$Temperature_C[is.na(data$Temperature_C)] <- median(data$Temperature_C, na.rm = TRUE)
data$Humidite_pct[is.na(data$Humidite_pct)]   <- median(data$Humidite_pct, na.rm = TRUE)
data$Pluie_mm[is.na(data$Pluie_mm)]           <- median(data$Pluie_mm, na.rm = TRUE)
data$Vent_mps[is.na(data$Vent_mps)]           <- median(data$Vent_mps, na.rm = TRUE)
data$Perception_air_1a5[is.na(data$Perception_air_1a5)] <-
  median(data$Perception_air_1a5, na.rm = TRUE)

data$Gene_bruit_1a5[is.na(data$Gene_bruit_1a5)] <-
  median(data$Gene_bruit_1a5, na.rm = TRUE)

data$Brulage_dechets[is.na(data$Brulage_dechets)] <- "Non"
data$Commentaire <- NULL
table(data$Qualite_capteur)
data$Qualite_capteur[is.na(data$Qualite_capteur)] <- "Bonne"
#Les valeurs manquantes de la variable Qualite_capteur ont été remplacées par la modalité la plus fréquente (mode).
colSums(is.na(data))
#commentaire:Les valeurs manquantes ont été traitées par imputation 
#médiane pour les variables quantitatives et ordinales, et mode pour les variables qualitatives. Les variables non utilisées dans les analyses ont été supprimées. Ce choix permet de conserver l’ensemble des observations tout en limitant les biais liés aux valeurs extrêmes.
 #3/Detecter les valeurs aberrantes(IQR/Boxplot/z-score)
PM2.5
boxplot(data$PM25_ugm3,
        main = "Distribution du PM2.5",
        ylab = "PM2.5 (µg/m³)")
#commentaire:Le boxplot du PM2.5 montre une distribution asymétrique avec la présence de valeurs élevées situées au-delà de l’intervalle interquartile.Ces valeurs aberrantes traduisent des épisodes ponctuels de forte pollution atmosphérique, possiblement liés au trafic, au brûlage de déchets ou aux conditions météorologiques. Elles ont été conservées car elles représentent des situations environnementales réelles.
NO₂
boxplot(data$NO2_ppb,
        main = "Distribution du NO2",
        ylab = "NO2 (ppb)")
#commentaire:Le boxplot du NO₂ met en évidence une variabilité modérée des concentrations, avec quelques valeurs extrêmes observées. Ces valeurs peuvent correspondre à des zones à forte influence anthropique, notamment le trafic routier ou certaines activités urbaines. Leur conservation permet de mieux représenter la réalité de l’exposition à la pollution gazeuse.
Bruit
boxplot(data$Bruit_dB,
        main = "Distribution du bruit",
        ylab = "Bruit (dB)")
#commentaire:Le boxplot du bruit révèle une dispersion importante des niveaux sonores ainsi que la présence de valeurs élevées. Ces valeurs extrêmes traduisent probablement des environnements très bruyants, tels que les zones de circulation intense ou industrielles. Elles sont cohérentes avec la nature du phénomène étudié et ont donc été conservées.
NDVI
boxplot(data$NDVI,
        main = "Distribution du NDVI",
        ylab = "NDVI")
#commentaire:Le boxplot du NDVI montre une distribution plus resserrée, avec quelques valeurs faibles ou élevées. Les valeurs faibles correspondent à des zones peu végétalisées ou urbanisées, tandis que les valeurs plus élevées reflètent une couverture végétale plus dense. Les valeurs identifiées ne sont pas considérées comme aberrantes mais comme représentatives de la diversité des usages du sol.
 #methode IQR
outliers_iqr <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)
}
sum(outliers_iqr(data$PM25_ugm3))
sum(outliers_iqr(data$NO2_ppb))
sum(outliers_iqr(data$Bruit_dB))
sum(outliers_iqr(data$NDVI))
#Les valeurs aberrantes ont d’abord été identifiées visuellement à l’aide de boxplots.
#La méthode de l’intervalle interquartile (IQR) a ensuite été utilisée pour confirmer statistiquement la présence de ces valeurs.
#Les valeurs aberrantes ont été conservées, car elles correspondent probablement à des événements environnementaux réels et non à des erreurs de mesure.


###B/SUPPRESSION DE COLONNES
library(dplyr)
data <- data %>%
  select(-any_of(c("Agent_collecteur", "Numero_serie_capteur", "Commentaire")))
names(data)
#commentaire:Les variables Agent_collecteur, Numero_serie_capteur et Commentaire ont été supprimées car elles ne sont pas pertinentes pour l’analyse environnementale et ont été incluses uniquement pour simuler un fichier de terrain réel.
#C/AJOUT DE NOUVELLE COLONNES
#1)Saison a partir de la dat
library(lubridate)
data$Date <- as.Date(data$Date)
data$Saison <- ifelse(
  month(data$Date) %in% c(6, 7, 8, 9, 10),
  "Pluie",
  "Seche"
)
#2)Dépassement PM2.5
data$Depassement_PM25 <- ifelse(
  data$PM25_ugm3 > 55,
  "Oui",
  "Non"
)
#3)PM25_log 
data$PM25_log <- log(data$PM25_ugm3)
#4)Indice de confort
data$Indice_confort <- data$Temperature_C + 0.1 * data$Humidite_pct
head(data)
#commentaire:Quatre nouvelles variables ont été ajoutées au jeu de données : la saison (pluvieuse ou sèche), le dépassement du seuil de PM2.5, la transformation logarithmique du PM2.5 et un indice de confort thermique combinant température et humidité. Ces variables permettent d’enrichir l’analyse environnementale et statistique.

#D/VISUALISATION
#Histogrammes + boxplots (PM2.5, NO₂, Bruit)
 #PM2.5
hist(data$PM25_ugm3,
     main = "Histogramme du PM2.5",
     xlab = "PM2.5 (µg/m³)")

boxplot(data$PM25_ugm3,
        main = "Boxplot du PM2.5",
        ylab = "PM2.5 (µg/m³)")
#Interpretation:La distribution du PM2.5 est asymétrique avec des valeurs élevées ponctuelles, traduisant des épisodes de pollution.
 #NO₂
hist(data$NO2_ppb,
     main = "Histogramme du NO2",
     xlab = "NO2 (ppb)")

boxplot(data$NO2_ppb,
        main = "Boxplot du NO2",
        ylab = "NO2 (ppb)")
#Interpretation:Le NO₂ présente une variabilité modérée avec quelques valeurs élevées, probablement liées au trafic routier.
 #Bruit

hist(data$Bruit_dB,
     main = "Histogramme du bruit",
     xlab = "Bruit (dB)")

boxplot(data$Bruit_dB,
        main = "Boxplot du bruit",
        ylab = "Bruit (dB)")
#Interpretation:Les niveaux de bruit sont très dispersés, reflétant la diversité des environnements étudiés

#Boxplot du PM2.5 par catégories
 #Par type de zone
boxplot(PM25_ugm3 ~ Type_zone,
        data = data,
        main = "PM2.5 selon le type de zone",
        ylab = "PM2.5 (µg/m³)")
 #Interpretation:Les concentrations de PM2.5 sont globalement plus élevées en zone urbaine que rurale.
 #Par usage du sol
boxplot(PM25_ugm3 ~ Usage_sol,
        data = data,
        main = "PM2.5 selon l’usage du sol",
        ylab = "PM2.5 (µg/m³)")

 #Interpretation:Le PM2.5 varie selon l’usage du sol, avec des niveaux plus élevés dans les zones industrielles ou urbanisées.

#Scatter plots (relations)
 #PM2.5 vs NO₂ (avec tendance)
plot(data$NO2_ppb, data$PM25_ugm3,
     xlab = "NO2 (ppb)",
     ylab = "PM2.5 (µg/m³)",
     main = "Relation PM2.5 – NO2")

abline(lm(PM25_ugm3 ~ NO2_ppb, data = data))
 #Interpretation:Une relation positive est observée entre le PM2.5 et le NO₂, suggérant des sources communes.

 #PM2.5 vs NDVI
plot(data$NDVI, data$PM25_ugm3,
     xlab = "NDVI",
     ylab = "PM2.5 (µg/m³)",
     main = "PM2.5 et NDVI")
 #Interpretation:Le PM2.5 tend à diminuer lorsque le NDVI augmente, suggérant un rôle atténuateur de la
 
#Série temporelle : PM2.5 par mois
data$Mois <- as.Date(format(data$Date, "%Y-%m-01"))

pm_mois <- aggregate(PM25_ugm3 ~ Mois, data = data, mean, na.rm = TRUE)



plot(pm_mois$Mois, pm_mois$PM25_ugm3,
     type = "l",
     xlab = "Mois",
     ylab = "PM2.5 moyen (µg/m³)",
     main = "Évolution mensuelle du PM2.5")

 #Interpretation: Le PM2.5 présente des variations mensuelles, probablement liées à la saisonnalité et aux activités humaines.
#Corrélogramme (variables quantitatives)
library(corrplot)

quanti <- data[, c("PM25_ugm3", "NO2_ppb", "Bruit_dB",
                   "Temperature_C", "Humidite_pct",
                   "Vent_mps", "NDVI")]

cor_mat <- cor(quanti, use = "complete.obs")

corrplot(cor_mat, method = "circle")

 #Interpretation:Le corrélogramme met en évidence des corrélations positives entre certains polluants et une corrélation négative entre le PM2.5 et le NDVI.

#3/Test1 — Quali × Quali
# Question :Le dépassement du PM2.5 dépend-il du type de zone ?
#Tableau de contingence
tab1 <- table(data$Depassement_PM25, data$Type_zone)
tab1
#Choix du test : Chi-deux ou Fisher ?
test1 <- chisq.test(tab1)
test1
test1 <- fisher.test(tab1)
test1
#V de Cramer
library(lsr)
cramersV(tab1)
#H0 :Le dépassement du PM2.5 est indépendant du type de zone.
#H1 :Le dépassement du PM2.5 dépend du type de zone.
#Commentaire:Le test du Chi-deux met en évidence une dépendance statistiquement significative entre le dépassement du seuil de PM2.5 et le type de zone (p = 7,392 × 10⁻⁵).
#Cela signifie que le dépassement du PM2.5 varie selon le type de zone.
#La taille d’effet mesurée par le V de Cramer (V = 0,36) indique une association modérée entre ces deux variables.
 
#TEST 2 — Quali × Quali Association terrain
#Le brûlage de déchets dépend-il du type de zone ? 
#Tableau de contingence
tab2 <- table(data$Brulage_dechets, data$Type_zone)
tab2
#statistique (Chi-deux ou Fisher)
test2 <- chisq.test(tab2)
test2
test2 <- fisher.test(tab2)
test2
#V de Cramer
cramersV(tab2)
#H0 :Le brûlage de déchets est indépendant du type de zone.
#H1 :Le brûlage de déchets dépend du type de zone.
#Commentaire:Le test du Chi-deux ne met pas en évidence de dépendance statistiquement significative entre le brûlage de déchets et le type de zone (p = 0,5404).
#Ainsi, les données ne permettent pas de conclure que la pratique du brûlage de déchets varie selon le type de zone.
#La taille d’effet mesurée par le V de Cramer (V = 0,09) indique une association très faible, confirmant l’absence de relation notable entre ces deux variables.
 

#TEST 3 —  Quanti_quanti Corrélation PM2.5 et NO₂(Pearson vs Spearman)
#Question: PM2.5 est il correle au NO₂ ? 
#Vérification visuelle
plot(data$PM25_ugm3, data$NO2_ppb,
     xlab = "PM2.5 (µg/m³)",
     ylab = "NO₂ (ppb)",
     main = "Relation entre PM2.5 et NO₂")
#Commentaire:Le nuage de points suggère une relation positive entre le PM2.5 et le NO₂, avec une dispersion modérée, indiquant une possible influence de sources communes de pollution atmosphérique.

#Choix du test
#En environnement, on choisit Spearman par défaut
#Test de corrélation (Spearman)
test3 <- cor.test(data$PM25_ugm3, data$NO2_ppb,
                  method = "spearman",
                  use = "complete.obs")
test3
#Hypothèses
#H0 :Il n’existe pas de relation entre la concentration de PM2.5 et celle du NO₂.
#H1 :Il n’existe pas de relation entre la concentration de PM2.5 et celle du NO₂.
#Commentaire:Le test de corrélation de Spearman met en évidence une relation statistiquement significative entre les concentrations de PM2.5 et de NO₂ (p = 7,81 × 10⁻¹⁴).
#Le coefficient de corrélation (ρ = 0,57) indique une corrélation positive de force modérée, ce qui signifie que lorsque la concentration de PM2.5 augmente, celle du NO₂ tend également à augmenter.
#Cette relation suggère l’existence de sources communes de pollution atmosphérique, telles que le trafic routier et les activités industrielles.

#Test 4 Quanti_Quanti NDVI et pollution
# Question:La végétation (NDVI) est-elle associée à la pollution (PM2.5) ?
#Visualisation 
plot(data$NDVI, data$PM25_ugm3,
     xlab = "NDVI (indice de végétation)",
     ylab = "PM2.5 (µg/m³)",
     main = "Relation entre NDVI et PM2.5")
#Commentaire :Le nuage de points suggère une relation globale entre le NDVI et la concentration de PM2.5. Une tendance décroissante indiquerait que les zones plus végétalisées présentent généralement des niveaux de pollution particulaire plus faibles.

#Choix du test
#Données environnementales + pas forcément linéaires
#corrélation de Spearman
#Test de corrélation (Spearman)
test4 <- cor.test(data$NDVI, data$PM25_ugm3,
                  method = "spearman",
                  use = "complete.obs")
test4
Hypothèses

#H0 :Il n’existe pas d’association entre le NDVI et la concentration de PM2.5.

#H1 :Il existe une association entre le NDVI et la concentration de PM2.5.
#Commentaire:#Le test de corrélation de Spearman met en évidence une association statistiquement significative et négative entre le niveau de végétation (NDVI) et la concentration de PM2.5 (p = 1,237 × 10⁻¹⁴).
#Le coefficient de corrélation (ρ = −0,59) indique une relation de force modérée à relativement forte, ce qui signifie que les zones plus végétalisées présentent généralement des concentrations de PM2.5 plus faibles.
#Ces résultats suggèrent que la végétation joue un rôle important dans la réduction de la pollution particulaire et l’amélioration de la qualité de l’air.

#Test_5— Quanti × Quali (2 groupes)
#Question:Le PM2.5 est-il différent quand il y a brûlage ?
#Visualisation 
boxplot(PM25_ugm3 ~ Brulage_dechets,
        data = data,
        xlab = "Brûlage de déchets",
        ylab = "PM2.5 (µg/m³)",
        main = "PM2.5 selon la présence de brûlage")
#Commentaire:Le boxplot suggère une différence des concentrations de PM2.5 entre les situations avec et sans brûlage de déchets, avec des valeurs généralement plus élevées lorsque le brûlage est présent.
#Test de normalité (Shapiro)
shapiro.test(data$PM25_ugm3[data$Brulage_dechets == "Oui"])
shapiro.test(data$PM25_ugm3[data$Brulage_dechets == "Non"])
#Commentaire:il n'y pas de normalite entre le PM2.5 et le brûlage 
#Test d’égalité des variances (Levene)
library(car)
leveneTest(PM25_ugm3 ~ Brulage_dechets, data = data)
#Test statistique final (Mann–Whitney)
test5 <- wilcox.test(PM25_ugm3 ~ Brulage_dechets,
                     data = data,
                     exact = FALSE)
test5
#Hypothèses
#H0 :La concentration de PM2.5 est la même en présence ou en absence de brûlage de déchets.
#H1 :La concentration de PM2.5 est différente selon la présence de brûlage de déchets.
#Commentaire:Le test de Mann–Whitney ne met pas en évidence de différence statistiquement significative des concentrations de PM2.5 entre les situations avec et sans brûlage de déchets (p = 0,4591).
#Ainsi, les données analysées ne permettent pas de conclure que la présence de brûlage de déchets entraîne une variation significative des niveaux de PM2.5.


#Test 6 _Bruit et zone industrielle
#QUESTION 6 :Le niveau de bruit est-il plus élevé en zone industrielle ?
bruit_indus <- data$Bruit_dB[data$Usage_sol == "Industriel"]
bruit_non_indus <- data$Bruit_dB[data$Usage_sol != "Industriel"]

#Supprimer les NA
bruit_indus <- bruit_indus[!is.na(bruit_indus)]
bruit_non_indus <- bruit_non_indus[!is.na(bruit_non_indus)]
length(bruit_indus)
length(bruit_non_indus)

 
#Visualisation 
boxplot(bruit_indus, bruit_non_indus,
        names = c("Industriel", "Non industriel"),
        ylab = "Niveau de bruit (dB)",
        main = "Niveau de bruit selon le type de zone")

#Commentaire :Le boxplot montre que les niveaux de bruit observés en zone industrielle sont globalement plus élevés que ceux des zones non industrielles. La médiane du bruit est plus élevée en zone industrielle et l’ensemble de la distribution est décalé vers des valeurs plus importantes Le boxplot permet de comparer les niveaux de bruit entre les zones industrielles et les autres zones. Une différence visuelle peut être observée, mais une analyse statistique est nécessaire pour déterminer si cette différence est significative. 

#Choix du test
#Les différences observées visuellement sur le boxplot suggèrent un niveau de bruit plus élevé en zone industrielle. Afin de vérifier si cette différence est statistiquement significative et non due au hasard, un test de Mann–Whitney a été réalisé.
#Test de Mann–Whitney (Wilcoxon)
test6 <- wilcox.test(bruit_indus, bruit_non_indus, exact = FALSE)
test6
#Hypothèses
#H0 :Le niveau de bruit est le même en zone industrielle et dans les autres zones.
#H1 :Le niveau de bruit est différent selon le type de zone.
#Commentaire:Le test fournit une p-value = 2,158 × 10⁻⁶, largement inférieure au seuil de 5 %.
#La p-value très faible conduit au rejet de l’hypothèse nulle.
#Les niveaux de bruit observés en zone industrielle sont significativement plus élevés que ceux des zones non industrielles.Le test de Mann–Whitney donne une p-value = 2,158 × 10⁻⁶, inférieure au seuil de significativité de 5 %.
# Alors on rejette l’hypothèse nulle.
#Il existe donc une différence statistiquement significative des niveaux de bruit entre les zones industrielles et les zones non industrielles.


#Test 7_Bruit et type de zone
 #Question:Le bruit diffère-t-il selon le type de zone ?
Graphique : boxplot 
boxplot(Bruit_dB ~ Type_zone,
        data = data,
        ylab = "Niveau de bruit (dB)",
        xlab = "Type de zone",
        main = "Niveau de bruit selon le type de zone")
#Commentaire:Le boxplot montre des différences dans la distribution des niveaux de bruit selon le type de zone. Les zones urbaines présentent globalement des niveaux de bruit plus élevés, tandis que les zones périurbaines affichent des valeurs intermédiaires. Les zones rurales enregistrent les niveaux de bruit les plus faibles.
#Test statistique : Kruskal–Wallis
test7 <- kruskal.test(Bruit_dB ~ Type_zone, data = data)
test7
#Hypothèses
#H0 : le niveau de bruit est identique quel que soit le type de zone.
#H1 : le niveau de bruit diffère selon le type de zone.
#Commentaie:La p-value extrêmement faible conduit au rejet de l’hypothèse nulle. Il existe donc une différence statistiquement significative des niveaux de bruit selon le type de zone.

#Conclusion:Le niveau de bruit diffère significativement entre les zones urbaines, périurbaines et rurales, avec des niveaux plus élevés observés en zone urbaine et plus faibles en zone rurale.



# TEST 8 : PM2.5 ~ Usage_sol
TEST 8 — Quanti × Quali (>2 groupes)

PM2.5 selon l’usage du sol
# TEST 8 : PM2.5 ~ Usage_sol
# Question : Le PM2.5 diffère-t-il selon l’usage du sol ?
#Visualisation (boxplot)
boxplot(PM25_ugm3 ~ Usage_sol,
        data = data,
        xlab = "Usage du sol",
        ylab = "PM2.5 (µg/m³)",
        main = "PM2.5 selon l’usage du sol")
#Commentaire:
#Le boxplot met en évidence des différences notables des concentrations de PM2.5 selon l’usage du sol.
#Les zones industrielles et mixtes présentent des niveaux de PM2.5 plus élevés, tandis que les zones agricoles et résidentielles affichent des concentrations plus faibles. Cette variabilité suggère une influence des activités humaines liées à l’usage du sol sur la pollution particulaire.
#Choix du test

#Les distributions n’étant pas nécessairement normales et l’usage du sol comportant plus de deux modalités, un test non paramétrique de Kruskal–Wallis est retenu.
test8 <- kruskal.test(PM25_ugm3 ~ Usage_sol, data = data)
test8
#Hypothèses
#H0 : le PM2.5 est identique pour tous les usages du sol
#H1 : le PM2.5 diffère selon l’usage du sol

#Commentaire
#La p-value obtenue est très inférieure au seuil de 5 %, ce qui conduit au rejet de l’hypothèse nulle.
#Il existe donc une différence statistiquement significative des concentrations de PM2.5 selon l’usage du sol.
#Ces résultats confirment l’impact des activités industrielles et urbaines sur la dégradation de la qualité de l’air.


#TEST 9 — Ordinale × Ordinale_Perception de la qualité de l’air et gêne du bruit
# TEST 9 : Perception_air_1a5 ~ Gene_bruit_1a5
# Question : La perception de la qualité de l’air est-elle liée à la gêne du bruit ?
#Visualisation (nuage de points avec jitter)
plot(jitter(data$Perception_air_1a5),
     jitter(data$Gene_bruit_1a5),
     xlab = "Perception de la qualité de l’air (1 = mauvaise, 5 = bonne)",
     ylab = "Gêne du bruit (1 = faible, 5 = forte)",
     main = "Relation entre perception de l’air et gêne du bruit")
#Commentaire:Le nuage de points suggère une relation monotone entre la perception de la qualité de l’air et la gêne du bruit.
#Lorsque la gêne du bruit augmente, la perception de la qualité de l’air tend à se dégrader, ce qui indique une possible association négative entre ces deux perceptions environnementales.
#Choix du test

#Les deux variables étant ordinales, le taux de Kendall est utilisé.
test9 <- cor.test(data$Perception_air_1a5,
                  data$Gene_bruit_1a5,
                  method = "kendall",
                  use = "complete.obs")
test9
#Hypothèses
#H0 : aucune association entre perception de l’air et gêne du bruit
#H1 : il existe une association entre les deux variables

#Commentaire:
#Le test de Kendall met en évidence une association statistiquement significative entre la perception de la qualité de l’air et la gêne du bruit.
#Le signe négatif du coefficient indique que plus la gêne du bruit est forte, plus la perception de la qualité de l’air est mauvaise.
#Ces résultats soulignent l’importance des nuisances sonores dans l’évaluation globale de la qualité de l’environnement urbain.


#TEST 10 — Données appariées
#Effet de la campagne poussière sur le PM2.5
# TEST 10 : PM2.5 avant / après campagne poussière
# Question : La campagne poussière a-t-elle réduit le PM2.5 ?

# Sélection des données de la campagne poussière
data_poussiere <- subset(data, Campagne == "Campagne_poussiere")

avant <- subset(data_poussiere, Phase_intervention == "Avant")
apres <- subset(data_poussiere, Phase_intervention %in% c("Après", "Apres"))
data_avant_apres <- merge(
  avant[, c("Station", "PM25_ugm3")],
  apres[, c("Station", "PM25_ugm3")],
  by = "Station",
  suffixes = c(".Avant", ".Apres")
)
names(data_avant_apres)
# Axe X
x <- c(1, 2)

# Initialisation du graphique (vide)
plot(x,
     range(c(data_avant_apres$PM25_ugm3.Avant,
             data_avant_apres$PM25_ugm3.Apres)),
     type = "n",
     xaxt = "n",
     xlab = "Phase de la campagne",
     ylab = "PM2.5 (µg/m³)",
     main = "Évolution du PM2.5 avant et après la campagne poussière")

# Axe X personnalisé
axis(1, at = c(1, 2), labels = c("Avant", "Après"))

# Tracer une ligne par station
for(i in 1:nrow(data_avant_apres)) {
  lines(x,
        c(data_avant_apres$PM25_ugm3.Avant[i],
          data_avant_apres$PM25_ugm3.Apres[i]),
        type = "b",
        col = "gray")
}
#Commentaire:
#Le graphique apparié montre que, pour la majorité des stations, les concentrations de PM2.5 diminuent entre la phase « Avant » et la phase « Après ».
#La tendance globale observée suggère une amélioration de la qualité de l’air suite à la campagne poussière.
#Test statiatique
test10 <- wilcox.test(data_avant_apres$PM25_ugm3.Avant,
                      data_avant_apres$PM25_ugm3.Apres,
                      paired = TRUE)
test10
#Commentaire:
#Le test de Wilcoxon apparié met en évidence une différence statistiquement significative des concentrations de PM2.5 avant et après la campagne poussière. La diminution observée du PM2.5 après l’intervention suggère un effet positif de la campagne sur la qualité de l’air. 

