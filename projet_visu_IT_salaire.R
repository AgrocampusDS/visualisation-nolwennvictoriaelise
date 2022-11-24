#Packages
library(ggplot2)
library("rnaturalearth")
library("rnaturalearthdata")
library(tidyverse)
library(maps)
library(ggrepel)
library(ggpubr)
library(scatterpie)
library(colorspace)
library(grid)
library(plyr)


# Palette de couleurs utilisée dans les graphiques
pal = c(sequential_hcl(3, palette = "Purp"),sequential_hcl(3, palette = "Mint"))

#Importations des données

data2019 <- read.csv("~/GitHub/visualisation-nolwennvictoriaelise/archive/T Salary Survey EU 2019.csv", 
                     header = TRUE, 
                     sep = ";", 
                     encoding = "latin1")
data2019$City <- as.factor(data2019$City)
data2019$Gender <- as.factor(data2019$Gender)

#Mise au propre du jeu de données
#Mise au propre des noms des villes

library(stringr)
data2019$City = str_replace_all(data2019$City, pattern = "Braunschweig", replacement = "Brunswick")
data2019$City = str_replace_all(data2019$City, pattern = "DÃ¼sseldorf", replacement = "Dusseldorf")
data2019$City = str_replace_all(data2019$City, pattern = "Hannover", replacement = "Hanover")
data2019$City = str_replace_all(data2019$City, pattern = "Kassel ", replacement = "Kassel")
data2019$City = str_replace_all(data2019$City, pattern = "Marburg", replacement = "Magdeburg")
data2019$City = str_replace_all(data2019$City, pattern = "WÃ¼rzburg ", replacement = "Wurzburg")

#Conserver que les villes allemandes
villes = c("Berlin", "Bielefeld", "Brunswick", "Bremen", "Cologne", "Darmstadt",
           "Dusseldorf", "Dresden", "Frankfurt", "Freiburg", "Hamburg",
           "Hanover", "Heidelberg","Ingolstadt", "Kaiserslautern",
           "Karlsruhe", "Kassel", "Koblenz", "Leipzig", "Lingen",
           "Magdeburg", "Munich", "Nuremberg", "Pforzheim", "Stuttgart", 
           "Wurzburg","Walldorf", "Wolfsburg")

length(villes)

data2019_Allemagne <- subset(data2019, City %in% villes) # on conserve seulement les villes en allemagne
dta <- subset(data2019_Allemagne, select=c("Age","Gender","City",
                                           "Seniority.level",
                                           "Position..without.seniority.",
                                           "Years.of.experience",
                                           "Yearly.brutto.salary..without.bonus.and.stocks."))
colnames(dta) <- c("Age","Gender","City",
                   "Seniority_level",
                   "Position",
                   "Years_of_experience",
                   "Yearly_salary")

###### Carte
# dta carte

dta$City = as.factor(dta$City)
female = data.frame(table(dta$City[dta$Gender == "Female"]))
male = data.frame(table(dta$City[dta$Gender == "Male"]))
sum = female$Freq + male$Freq
dta_graph = data.frame(Cities_allemagne, Female = female$Freq, Male = male$Freq, sum = sum)

Cities_allemagne <- Cities %>% filter(name %in% villes)
Cities_allemagne <- Cities_allemagne[-10,]

#Carte

ggplot() + 
  geom_polygon(data = Germany, aes(x = long, y = lat, group = group), 
               fill="grey",color="black", alpha = 0.5)+
  geom_scatterpie(data = dta_graph, 
                  aes(x = long, y = lat,group = name, r = log(sum)/8),
                  alpha =.5, cols = c("Female","Male"), color = "Black")+
  scale_fill_manual(name = "Gender", values = c('#645A9F','#005D67'))+
  geom_scatterpie_legend(radius = log(dta_graph$sum)/8, x = 4, y = 47.5, 
                         labeller = function(x) round(exp(x*8), 0))+
  geom_text_repel(data = dta_graph, aes(x = long, y = lat, label = name), size = 3,   col = "black", family = "sans")+
  geom_point(data=dta_graph, aes(x = long, y = lat), color = "black", size = 1)+
  theme_void()+ 
  labs(title = "Repartition géographique des postes des participants de l'enquête",
       caption = "Source : Viktor Shcherban and Ksenia Legostay")+
  theme(
    plot.title = element_text(color = "black", size = 12, hjust = 0.5),
    plot.caption = element_text(color = "black", face = "italic", hjust = 1)
  )

##### Grahique 2 et 3
# Pré-traitement du jeu de données
# Changement des noms des jobs

dta$Position[242] = "NA" # la personne 242 n'a pas renseigné son job
dta$Position = as.factor(dta$Position)

a <- as.data.frame(summary(dta$Position))
colnames(a) <- "count"
a$count = as.numeric(a$count)

nvelles_positions = c()
for (i in 1:nrow(a)){
  if (a[i,1] < 10){
    nvelles_positions = c(nvelles_positions,"Other professions")
  }
  else if(a[i,1] %in% c("Data Scientist","Data Analyst","Data Engineer")){
    nvelles_positions=c(nvelles_positions,"Data Scientist")
  }
  else{
    nvelles_positions=c(nvelles_positions,rownames(a)[i])
  }
}

b <- data.frame(rownames(a),nvelles_positions)

# Création d'une nouvelle colonne dans dta contenant les nouveaux noms des jobs

Position2 = c()
for (i in 1:nrow(dta)){
  Position2=c(Position2,b[which(b[,1] == dta$Position[i]),2])
}

dta <- data.frame(dta,Position2)

###### Graph 2

# On crée un jeu de données contenant le nombre de personnes par position et par genre

positions_femmes = sapply(unique(nvelles_positions),function(i){
  dta_femmes = subset(dta,Gender == "Female")
  return(length(which(dta_femmes$Position2 == i)))
})

positions_hommes = sapply(unique(nvelles_positions),function(i){
  dta_hommes = subset(dta,Gender == "Male")
  return(length(which(dta_hommes$Position2 == i)))
})

salaries_femmes = sapply(unique(nvelles_positions),function(i){
  dta_femmes = subset(dta,Gender == "Female")
  return(mean(dta_femmes$Yearly_salary[which(dta_femmes$Position2 == i)],na.rm=T))
})

salaries_hommes = sapply(unique(nvelles_positions),function(i){
  dta_hommes = subset(dta,Gender == "Male")
  return(mean(dta_hommes$Yearly_salary[which(dta_hommes$Position2 == i)],na.rm=T))
})


dta_graph1<- rbind(data.frame("Position" = unique(nvelles_positions),
                              "Gender" = c("Female"),
                              "Count" = positions_femmes,
                              "Salaries" = salaries_femmes),
                   data.frame("Position" = unique(nvelles_positions),
                              "Gender" = c("Male"),
                              "Count" = positions_hommes,
                              "Salaries" = salaries_hommes))

dta_graph1$Position2 <- ifelse(dta_graph1$Gender == "Male",
                               -1*dta_graph1$Count,
                               dta_graph1$Count)


difference_salaire <- sapply(unique(nvelles_positions),function(i){
  dta_hommes = subset(dta, Gender == "Male" & Position2 == i)
  dta_femmes = subset(dta, Gender == "Female" & Position2 == i)
  #diff <- dta_hommes$Yearly_salary[which(dta_hommes$Position2 == i)] - dta_femmes$Yearly_salary[which(dta_hommes$Position2 == i)]
  diff <- mean(dta_hommes$Yearly_salary, na.rm = TRUE) - mean(dta_femmes$Yearly_salary, na.rm = TRUE)
  return(diff)
})

dta_difference <- data.frame(cbind(unique(nvelles_positions)), round_any(difference_salaire, 500))
colnames(dta_difference)[2] <- "difference_salaire"
colnames(dta_difference)[1] <- "Position"
dta_difference <- dta_difference[-which(dta_difference$Position == "DevOps"),] 
dta_difference$Position[which(dta_difference$Position == "QA")] <- "Quality Assurance"
#on enlève la ligne du métier correspondant à DevOps car pas de données pour les femmes

# salaire moyen des hommes et des femmes pour tous métiers confondus
salaire_moy_hommes_femmes <- sapply(unique(dta$Gender), function(i){
  dta_genre = subset(dta, Gender == i)
  moy_genre <- mean(dta_genre$Yearly_salary, na.rm = TRUE) 
  return(moy_genre)
})



#on arrondi les salaires aux 5000 euros près
library(plyr)
library(scales)
dta_graph1$Salaries <- round_any(dta_graph1$Salaries, 5000)
format(dta_graph1$Salaries, scientific = TRUE, )


##### Graph2 : la DIFFERENCE de salaire


ggplot(dta_difference, aes(x = reorder(Position, difference_salaire),y = difference_salaire, fill = )) +
  geom_bar(aes(fill = 'Surplus salarial gagné\npar les hommes (en milliers d\'euros)'), 
           stat = "identity",
           position = "identity") +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K", accuracy = 1),
                     breaks = seq(0, 50000, 5000)) +
  ylab("") +
  labs(fill = "Légende", title = "Différence salariale annuelle entre les hommes et les femmes\n selon les différents métiers de l'IT") +
  scale_fill_manual(values = c('#005D67')) +
  geom_hline(yintercept = salaire_moy_hommes_femmes[1] - salaire_moy_hommes_femmes[2] + 500, color = 'red', linewidth = 1) +
  geom_text( x = 4, y = salaire_moy_hommes_femmes[1] - salaire_moy_hommes_femmes[2], vjust = -1, color = "red",size = 4.5, label = "Différence de salaire moyen entre les hommes et les femmes") +
  geom_text(aes(x = Position, y = difference_salaire, label = paste0(difference_salaire * 1e-3, "K")), family = "sans", color = "white", fontface = "bold", 
            cex = 4.5, vjust = 1.4) +
  annotation_custom(grob = linesGrob(), xmin = 2.6, xmax = 2.75, ymin = 80, ymax = 80) +
  theme(panel.background = element_rect(fill= "white", colour = "white"),
        axis.line.y = element_line(linewidth = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(family = "sans", size = 20, vjust = -3),
        axis.text.x = element_text(angle= 60,family="sans",size=12, vjust = 1, hjust = 1, face = "bold", color = "black"), #gras/taille/couleur à changer
        axis.text.y = element_text(family="sans",size=13, hjust = 1, margin = margin(40, 40, 40, 40), face = "bold", color = "black"),
        axis.ticks=element_blank(),
        legend.text = element_text(family = "sans", size = 13),
        legend.title = element_text(family = "sans", size = 15, vjust = 1.5, face = "bold"),
        legend.title.align = 0.3,
        legend.margin = margin(0,1,0,0, "cm"),
        legend.position = c(0.6,0.8),
        plot.margin = margin(1,2,0.5,0.3, "cm"),
        plot.title = element_text(hjust = 0.5, vjust = 6, family = "sans", color = "black", face = "bold", size = 25),
        plot.subtitle = element_text(hjust = 0.5, vjust = 6, family = "sans", color = "black", size = 15))



# ##ajout d'une colonne moyenne salaire dans dta_graph1
# 
# library(tidyr)
# library(dplyr)
# 
# dta_graph1_female <- dta_graph1[which(dta_graph1$Gender == "Female"),]
# tot_moy_female <- c()
# for (position in dta_graph1_female$Position2){
#   #moy <- c(dta$Yearly_salary[(which(dta$Gender == 'Female') %in% which(dta$Position2 == position))])
#   moy <- subset(dta,Gender=="Female" & Position2 == position)$Yearly_salary
#   moy <- mean(moy, na.rm = TRUE)
#   tot_moy_female <- c(tot_moy_female, moy)
# }
# 
# dta_graph1_male <- dta_graph1[which(dta_graph1$Gender == "Male"),]
# tot_moy_male <- c()
# for (position in dta_graph1_male$Position){
#   moy <- c(dta$Yearly_salary[(which(dta$Gender == 'Male') %in% (which(dta$Position2 == position)))])
#   moy <- mean(moy, na.rm = TRUE)
#   tot_moy_male <- c(tot_moy_male, moy)
# }



###### Graph 3

# on garde 3 classes de seniority : junior, middle et senior

dta$Seniority_level = as.factor(dta$Seniority_level)
summary(dta$Seniority_level)

Seniority_level2 = unlist(lapply(dta$Seniority_level,function(i){
  if (i =="Junior"){
    return("Junior")
  }
  else if (i =="Middle"){
    return("Middle")
  }
  else if (i == "Senior"){
    return("Senior")
  }
  else {
    return("NA")
  }
}))

Seniority_level2
dta$Seniority_level2 = Seniority_level2

# on calcule le nombre de personnes par genre, age et par catégorie de seniority

list_age = sort(unique(dta$Age))

junior_age_f = sapply(list_age,function(i){
  dta_age = subset(dta,Age == i & Gender == "Female")
  return(length(which(dta_age$Seniority_level2 == "Junior")))
})

middle_age_f = sapply(list_age,function(i){
  dta_age = subset(dta,Age == i & Gender == "Female")
  return(length(which(dta_age$Seniority_level2=="Middle")))
})

senior_age_f = sapply(list_age,function(i){
  dta_age = subset(dta,Age==i & Gender == "Female")
  return(length(which(dta_age$Seniority_level2=="Senior")))
})

junior_age_h = sapply(list_age,function(i){
  dta_age = subset(dta,Age == i & Gender == "Male")
  return(length(which(dta_age$Seniority_level2 == "Junior")))
})

middle_age_h = sapply(list_age,function(i){
  dta_age = subset(dta,Age == i & Gender == "Male")
  return(length(which(dta_age$Seniority_level2=="Middle")))
})

senior_age_h = sapply(list_age,function(i){
  dta_age = subset(dta,Age==i & Gender == "Male")
  return(length(which(dta_age$Seniority_level2=="Senior")))
})

# on fait les classes

nb_senior = data.frame(list_age,senior_age_f,senior_age_h)

senior_f_cum = c()
senior_h_cum = c()

for (i in 1:nrow(nb_senior)){
  senior_f_cum = c(senior_f_cum,sum(nb_senior$senior_age_f[1:i]))
  senior_h_cum = c(senior_h_cum,sum(nb_senior$senior_age_h[1:i]))
}
senior_f_cum = senior_f_cum/nrow(subset(dta,Gender == "Female"))
senior_h_cum = senior_h_cum/nrow(subset(dta,Gender == "Male"))
senior_dif_cum = senior_h_cum - senior_f_cum

dta_graph3 = rbind(data.frame(list_age,"Gender" = c("Female"), "prop" = senior_f_cum),
                     data.frame(list_age,"Gender" = c("Male"),"prop" = senior_h_cum))

dta_graph3 = rbind(data.frame(list_age,"Gender" = c("Male"), "prop" = senior_h_cum),
                   data.frame(list_age,"Gender" = c("Female"),"prop" = senior_f_cum))


graph3 <- ggplot(data = dta_graph3,aes(x = list_age, 
                                       y = prop, 
                                       color = Gender,
                                       group=Gender)) +
  geom_line(linewidth=1.3) +
  labs(title = "",
       subtitle ="") +
  ylab("Nombre cumulé de seniors rappoté au nombre total de personnes du même genre") +
  xlab("Age (années)") +
  scale_x_continuous(breaks=seq(20,54,2)) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 1, colour = "black"),
        panel.grid = element_line(color = "grey"))

graph3

