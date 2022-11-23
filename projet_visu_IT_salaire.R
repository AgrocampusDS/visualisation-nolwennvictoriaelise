#Carte
library(ggplot2)
library("rnaturalearth")
library("rnaturalearthdata")
library(tidyverse)
library(maps)
library(ggrepel)
library(ggpubr)
library(scatterpie)
library(sf)
library(colorspace)

# A faire : ordonner les parties (j'ai l'impression que la partie de la carte
# est mise avant la partie de l'importation de données, c'est bizarre non ?)

# Palette de couleurs utilisée dans les graphiques
pal = c(sequential_hcl(3, palette = "Purp"),sequential_hcl(3, palette = "Mint"))

# Carte
Germany <- map_data("world") %>% filter(region=="Germany")
Cities <- world.cities %>% filter(country.etc=="Germany")
villes= c("Berlin", "Bielefeld", "Brunswick", "Bremen", "Cologne", "Dusseldorf",
               "Darmstadt","Dresden", "Frankfurt", "Freiburg", "Hamburg",
               "Hanover", "Heidelberg","Ingolstadt", "Kaiserslautern",
               "Karlsruhe", "Kassel", "Koblenz", "Leipzig", "Lingen",
               "Magdeburg", "Munich", "Nuremberg", "Pforzheim", "Stuttgart", 
               "Wurzburg","Walldorf", "Wolfsburg")

Cities_allemagne <- Cities %>% filter(name %in% villes)
Cities_allemagne <- Cities_allemagne[-10,]

ggplot() + 
  geom_polygon(data = Germany, aes(x=long, y = lat, group = group), 
               fill="grey",color="black", alpha=0.5)+
  geom_scatterpie(data = dta_graph, 
                  aes( x=long, y = lat,group = name,r=log(sum)/8),
                  alpha =.6, cols = c("Female","Male"), color = NA)+
  scale_fill_manual(name = "Gender",values = c('#645A9F','#005D67'))+
  geom_scatterpie_legend(radius = log(dta_graph$sum)/8, x=4, y=47.5, 
                         labeller = function(x) round(exp(x*8),0))+
  geom_text_repel( data= dta_graph , aes(x=long, y=lat, label=name), size=3, col = "black", family = "sans")+
  geom_point( data=dta_graph , aes(x=long, y=lat), color="black", size=1)+
  coord_map() + theme_void()+ 
  labs(title = "People working IT positions in different cities in 2019",
     subtitle ="From a salary survey conducted among German IT specialists. This year 825 respondents participated in the survey.",
     caption = "Source : Viktor Shcherban and Ksenia Legostay, www.")+
  theme(
    plot.title = element_text(color = "black", size = 15, hjust = 0.5),
    plot.subtitle = element_text(color = "black", hjust = 0),
    plot.caption = element_text(color = "black", face = "italic")
  )



#Importations des données
data2018 <- read.csv("~/GitHub/projet_visu_trains/archive/IT Salary Survey EU 2018.csv", 
                       header = TRUE, 
                       sep = ";", 
                       encoding = "latin1")
data2018$City <- as.factor(data2018$City)
data2018$Gender <- as.factor(data2018$Gender)
levels(data2018$City)


data2019 <- read.csv("T Salary Survey EU 2019.csv", 
                     header = TRUE, 
                     sep = ";", 
                     encoding = "latin1")
data2019$City <- as.factor(data2019$City)
data2019$Gender <- as.factor(data2019$Gender)

data2020 <- read.csv("~/GitHub/visu_femmes_IT/archive/IT Salary Survey EU  2020.csv", 
                       header = TRUE, 
                       sep = ";", 
                       encoding = "latin1")
data2020$City <- as.factor(data2020$City)

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
    nvelles_positions = c(nvelles_positions,"Other")
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

####### Graph 1

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

#dta_graph1$Salaries <- ifelse(dta_graph1$Gender == "Male",
                              # round(-1*dta_graph1$Salaries),
                              # round(dta_graph1$Salaries))

#on arrondi les salaires aux 5000 euros près
library(plyr)
dta_graph1$Salaries <- round_any(dta_graph1$Salaries, 5000)
format(dta_graph1$Salaries, scientific = TRUE, )



#graphique avec le count
ggplot(dta_graph1) +
  geom_bar(aes(x = Position, y = Position2, fill = Gender),
           stat = "identity",
           position = "identity") +
  geom_text(aes(x = Position, y = Position2, label = abs(Position2)),
            vjust = ifelse(dta_graph1$Position2 >= 0, 0, 1)) +
  scale_y_continuous(labels = abs,breaks = seq(-200, 200, 20)) + 
  labs(title = "People working in different IT positions in 2019",
       subtitle ="From a salary survey conducted among German IT specialists. This year 825 respondents participated in the survey.",
       caption = "Source : Viktor Shcherban and Ksenia Legostay, www.") +
  theme(panel.background = element_rect(fill= "white", colour = "white"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=90),
        plot.margin = margin(1.5,0,1,1, "cm")
  )

#graphique avec le salaire
dta_graph1_bis <- data.frame(apply(dta_graph1, 2, rev))
dta_graph1_bis$Position <- as.factor(dta_graph1_bis$Position)
dta_graph1_bis$Gender <- as.factor(dta_graph1_bis$Gender)
dta_graph1_bis$Salaries <- as.numeric(dta_graph1_bis$Salaries)


# ggplot(dta_graph1) +
#   geom_bar(aes(x = Position, y = Salaries, fill = Gender),
#            stat = "identity",
#            position = "identity") +
#   geom_text(aes(x = Position, y = Salaries, label = Salaries),
#             vjust = ifelse(dta_graph1$Position2 >= 0, 0, 1)) +
#   scale_y_continuous(labels = abs,breaks = seq(-80000, 80000, 20000)) + 
#   labs(title = "Yearly salary in different IT positions in 2019",
#        subtitle ="From a salary survey conducted among German IT specialists. This year 825 respondents participated in the survey.",
#        caption = "Source : Viktor Shcherban and Ksenia Legostay, www.") +
#   theme(panel.background = element_rect(fill= "white", colour = "white"),
#         axis.title.x = element_blank(),
#         axis.text.x = element_text(angle=90),
#         plot.margin = margin(1.5,0,1,1, "cm")
#   )

library(scales)

position <- reorder(dta_graph1_bis$Position[1:14], dta_graph1_bis$Salaries[1:14])


ggplot(dta_graph1_bis) +
  geom_bar(aes(x = reorder(Position, Salaries), y = Salaries, fill = Gender),
           stat = "identity",
           position = "identity") +
  geom_text(aes(x = Position, y = Salaries, label = paste0(Salaries * 1e-3, "K")), family = "sans", color = "white", fontface = "bold", cex = 3.5,
            vjust = ifelse(dta_graph1_bis$Gender == 'Male', 1.2, 1.8)) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K", accuracy = 1),
                     breaks = seq(0, 90000, 15000)) + 
  labs(title = "Yearly salary in different IT positions in 2019",
       subtitle ="From a salary survey conducted among German IT specialists. This year 825 respondents participated in the survey.",
       caption = "Source : Viktor Shcherban and Ksenia Legostay, www.") +
  scale_fill_manual(values = c("#645A9F",'#005D67')) +
  theme(panel.background = element_rect(fill= "white", colour = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(family = "sans", size = 20, vjust = -3),
        axis.text.x = element_text(angle= 60,family="sans",size=12, vjust = 0.9, hjust = 0.9, face = "bold", color = "black"), #gras/taille/couleur à changer
        axis.text.y = element_text(family="sans",size=13, hjust = 1, margin = margin(40, 40, 40, 40), face = "bold", color = "black"),
        axis.ticks=element_blank(),
        legend.text = element_text(family = "sans", size = 13),
        legend.title = element_text(family = "sans", size = 15),
        legend.margin = margin(1,1,1,1, "cm"),
        plot.margin = margin(1,1,1,0.3, "cm"),
        plot.title = element_text(hjust = 0.5, vjust = 6, family = "sans", color = "black", face = "bold", size = 25),
        plot.subtitle = element_text(hjust = 0.5, vjust = 6, family = "sans", color = "black", size = 15)
        )
  
#en blanc/ et gras données salaires des bar
#inverser légende gender male/female
#police sans
#txt en noir
#Titre, sous titre centré
#ajouter axe ordonnées
#other tout à droite
#trier salaire en fonction des femmes

##ajout d'une colonne moyenne salaire dans dta_graph1

library(tidyr)
library(dplyr)

dta_graph1_female <- dta_graph1[which(dta_graph1$Gender == "Female"),]
tot_moy_female <- c()
for (position in dta_graph1_female$Position2){
  #moy <- c(dta$Yearly_salary[(which(dta$Gender == 'Female') %in% which(dta$Position2 == position))])
  moy <- subset(dta,Gender=="Female" & Position2 == position)$Yearly_salary
  moy <- mean(moy, na.rm = TRUE)
  tot_moy_female <- c(tot_moy_female, moy)
}

dta_graph1_male <- dta_graph1[which(dta_graph1$Gender == "Male"),]
tot_moy_male <- c()
for (position in dta_graph1_male$Position){
  moy <- c(dta$Yearly_salary[(which(dta$Gender == 'Male') %in% (which(dta$Position2 == position)))])
  moy <- mean(moy, na.rm = TRUE)
  tot_moy_male <- c(tot_moy_male, moy)
}

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

classes_age = c("23-25","26-28", "29-31", "32-34", "35-37", "38-40", "41-43", 
                "44-46", "47-49","50-54")

nb_senior = data.frame(list_age,senior_age_f,senior_age_h)

nb_seniors_h_classes = c(sum(senior_age_h[1:3])/(sum(senior_age_h[1:3])+sum(senior_age_f[1:3])),
                         sum(senior_age_h[4:6])/(sum(senior_age_h[4:6])+sum(senior_age_f[4:6])),
                         sum(senior_age_h[7:9])/(sum(senior_age_h[7:9])+sum(senior_age_f[7:9])),
                         sum(senior_age_h[10:12])/(sum(senior_age_h[10:12])+sum(senior_age_f[10:12])),
                         sum(senior_age_h[13:15])/(sum(senior_age_h[13:15])+sum(senior_age_f[13:15])),
                         sum(senior_age_h[16:18])/(sum(senior_age_h[16:18])+sum(senior_age_f[16:18])),
                         sum(senior_age_h[19:21])/(sum(senior_age_h[19:21])+sum(senior_age_f[19:21])),
                         sum(senior_age_h[22:24])/(sum(senior_age_h[22:24])+sum(senior_age_f[22:24])),
                         sum(senior_age_h[25:27])/(sum(senior_age_h[25:27])+sum(senior_age_f[25:28])),
                         sum(senior_age_h[28:30])/(sum(senior_age_h[28:30])+sum(senior_age_f[28:30])))

nb_seniors_h_classes = c(sum(senior_age_h[1:3])/sum(senior_age_h+senior_age_f),
                         sum(senior_age_h[4:6])/sum(senior_age_h+senior_age_f),
                         sum(senior_age_h[7:9])/sum(senior_age_h+senior_age_f),
                         sum(senior_age_h[10:12])/sum(senior_age_h+senior_age_f),
                         sum(senior_age_h[13:15])/sum(senior_age_h+senior_age_f),
                         sum(senior_age_h[16:18])/sum(senior_age_h+senior_age_f),
                         sum(senior_age_h[19:21])/sum(senior_age_h+senior_age_f),
                         sum(senior_age_h[22:24])/sum(senior_age_h+senior_age_f),
                         sum(senior_age_h[25:27])/sum(senior_age_h+senior_age_f),
                         sum(senior_age_h[28:30])/sum(senior_age_h+senior_age_f))


nb_seniors_f_classes = c(sum(senior_age_f[1:3])/(sum(senior_age_h[1:3])+sum(senior_age_f[1:3])),
                         sum(senior_age_f[4:6])/(sum(senior_age_h[4:6])+sum(senior_age_f[4:6])),
                         sum(senior_age_f[7:9])/(sum(senior_age_h[7:9])+sum(senior_age_f[7:9])),
                         sum(senior_age_f[10:12])/(sum(senior_age_h[10:12])+sum(senior_age_f[10:12])),
                         sum(senior_age_f[13:15])/(sum(senior_age_h[13:15])+sum(senior_age_f[13:15])),
                         sum(senior_age_f[16:18])/(sum(senior_age_h[16:18])+sum(senior_age_f[16:18])),
                         sum(senior_age_f[19:21])/(sum(senior_age_h[19:21])+sum(senior_age_f[19:21])),
                         sum(senior_age_f[22:24])/(sum(senior_age_h[22:24])+sum(senior_age_f[22:24])),
                         sum(senior_age_f[25:28])/(sum(senior_age_h[25:28])+sum(senior_age_f[25:28])),
                         sum(senior_age_f[28:30])/(sum(senior_age_h[28:30])+sum(senior_age_f[28:30])))

nb_seniors_f_classes = c(sum(senior_age_f[1:3])/sum(senior_age_h+senior_age_f),
                         sum(senior_age_f[4:6])/sum(senior_age_h+senior_age_f),
                         sum(senior_age_f[7:9])/sum(senior_age_h+senior_age_f),
                         sum(senior_age_f[10:12])/sum(senior_age_h+senior_age_f),
                         sum(senior_age_f[13:15])/sum(senior_age_h+senior_age_f),
                         sum(senior_age_f[16:18])/sum(senior_age_h+senior_age_f),
                         sum(senior_age_f[19:21])/sum(senior_age_h+senior_age_f),
                         sum(senior_age_f[22:24])/sum(senior_age_h+senior_age_f),
                         sum(senior_age_f[25:28])/sum(senior_age_h+senior_age_f),
                         sum(senior_age_f[28:30])/sum(senior_age_h+senior_age_f))
dta_graph_prop = data.frame(classes_age, nb_seniors_f_classes, nb_seniors_h_classes)

dta_graph_prop = rbind(data.frame(classes_age,"Gender"= c("Female"), "prop"=nb_seniors_f_classes),
                       data.frame(classes_age,"Gender" = c("Male"),"prop"=nb_seniors_h_classes))


graph_prop <- ggplot(data= dta_graph_prop,aes(x=classes_age, y=prop, group=Gender, color=Gender)) +
  geom_line()

prop_senior_f = (senior_age_f)/(senior_age_f+middle_age_f+junior_age_f+senior_age_h+middle_age_h+junior_age_h)
prop_senior_f = sapply(prop_senior_f, function(i){
  if(is.nan(i)){return (0)}
    else{return(i)}
})
prop_senior_h = (senior_age_h)/(senior_age_f+middle_age_f+junior_age_f+senior_age_h+middle_age_h+junior_age_h)

dta_graph_prop = data.frame(list_age, prop_senior_f, prop_senior_f)

dta_graph_prop = rbind(data.frame(list_age,"Gender"= c("Female"), "prop"=prop_senior_f),
                   data.frame(list_age,"Gender" = c("Male"),"prop"=prop_senior_h))


graph_prop <- ggplot(data= dta_graph_prop,aes(x=list_age, y=prop, group=Gender, color=Gender)) +
              geom_line()

dta_graph3 = data.frame(list_age,
                        junior_age_f,
                        middle_age_f,
                        senior_age_f,
                        junior_age_h,
                        middle_age_h,
                        senior_age_h)

dta_graph3 = rbind(data.frame(list_age,"position"= c("Junior Women"), "count"=junior_age_f),
                   data.frame(list_age,"position" = c("Middle Women"),"count"=middle_age_f),
                   data.frame(list_age,"position" = c("Senior Women"),"count"=senior_age_f),
                   data.frame(list_age,"position"= c("Junior Men"), "count"=junior_age_h),
                   data.frame(list_age,"position" = c("Middle Men"),"count"=middle_age_h),
                   data.frame(list_age,"position" = c("Senior Men"),"count"=senior_age_h))

dta_graph3$count=ifelse(dta_graph3$position %in%c("Junior Men","Middle Men","Senior Men"),
                        -1*dta_graph3$count,
                        dta_graph3$count)

dta_graph3$position = factor(dta_graph3$position,
                             levels = c("Senior Women","Middle Women", "Junior Women",
                                        "Senior Men","Middle Men", "Junior Men"))

graph3 <- ggplot(dta_graph3) + 
  geom_area(aes(x = list_age, y = count, fill = position),
            stat = "identity") +
  scale_y_continuous(labels = abs,breaks = seq(-60, 60, 5)) +
  scale_x_continuous(breaks = seq(20, 54, 1)) +
  scale_fill_manual(values=pal) +
  xlab("Age of participants (years)") +
  ylab("Number of participants") +
  theme(panel.background=element_rect(fill="white"))
graph3
  
# A régler : pb de blanc vers 41 ans chez les hommes
# Mettre une grille pour que ce soit plus lisible
# Changer le nom de la légende

#dta pies

dta$City = as.factor(dta$City)

female = data.frame(table(dta$City[dta$Gender == "Female"]))
male = data.frame(table(dta$City[dta$Gender == "Male"]))
sum = female$Freq + male$Freq
dta_graph = data.frame(Cities_allemagne, Female = female$Freq, Male = male$Freq, sum = sum)



