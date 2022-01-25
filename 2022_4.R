#Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!
install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2022-01-25')
tuesdata <- tidytuesdayR::tt_load(2022, week = 4)

ratings <- tuesdata$ratings
details<-tuesdata$details

# lien entre l'année de sortie et le rang dans le classement ? Devrait faire apparaître les classiques 
summary(ratings$year)
#3500 ??
a=subset(ratings,year<1900)
a
ratings2=subset(ratings, year<=2022)

library(ggplot2)
g=ggplot(data=ratings2, aes(x=rank,y=year))+
  geom_point()
g


g=ggplot(data=ratings2, aes(x=average,y=year))+
  geom_point()
g

#fusionner les deux jeux 
total=merge(details,ratings,by="id")
total=subset(total,year<=2022) #enlève les rangs pour les valeurs d'années supérieures à 2022

g=ggplot(data=total, aes(x=average,y=minage))+
  geom_point()
g

#diviser les âges en catégories
library(dplyr)
total=mutate(total, age_cat=case_when(
  minage<6~"Tout public",
  minage>=6 & minage<12 ~"7/12 ans",
  minage>=12 ~ "Dès 12 ans"))
total$age_cat



#pour chaque âge, trouver les jeux les mieux notés en moyenne

h_toutpub=total%>% select(age_cat,name,average)%>%filter(age_cat=="Tout public")%>%
  arrange(desc(average))%>%
  slice(1:5)
h_toutpub  

h_7_12=total%>% select(age_cat,name,average)%>%filter(age_cat=="7/12 ans")%>%
  arrange(desc(average))%>%
  slice(1:5)

h  

h_12=total%>% select(age_cat,name,average)%>%filter(age_cat=="Dès 12 ans")%>%
  arrange(desc(average))%>%
  slice(1:5)
h

h=total%>% select(name,average)%>% #jeux les mieux notés toutes catégories confondues
  arrange(desc(average))%>%
  slice(1:5)
h

#Création du graphique
library(forcats) #réordonner les facteurs
# h$average=factor(h$average)
# h$name=factor(h$name) #NON

g1=ggplot(h, aes(x=fct_reorder(name,average),y=average,fill=average))+
  geom_bar(stat="identity")+
  scale_fill_gradient(low = "#FCD21C", high = "#DFAF2C")
  
g1
g1=g1+coord_flip()

g2=ggplot(h_toutpub, aes(x=fct_reorder(name,average),y=average, fill=average))+
  geom_bar(stat="identity") + #width=0.5
  scale_fill_gradient(low = "#42E695", high = "#3BB2B8")+
  geom_text(aes(y=3,label=paste(format(name))))+
  labs(title="Jeux les mieux notés par tranche d'âge minimum pour y jouer", subtitle="Tout public", fill="Note moyenne")+
  scale_y_discrete(name ="")+
  theme(
     plot.title = element_text(size=16, face="bold",hjust=0.5 ),
     axis.text.y = element_blank(),
     axis.ticks=element_blank(),
     axis.title.y = element_blank(),
     
  )

  
g2=g2+coord_flip()
g2 

g3=ggplot(h_7_12, aes(x=fct_reorder(name,average),y=average,fill=average))+
  geom_bar(stat="identity")+
  scale_fill_gradient(low = "#3BB2B8", high = "#4390CB")+
  geom_text(aes(y=3,label=paste(format(name))))+
  labs( subtitle="7/12 ans", fill="Note moyenne")+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks=element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
    
  )
g3=g3+coord_flip()
g3

g4=ggplot(h_12, aes(x=fct_reorder(name,average),y=average, fill=average))+
  geom_bar(stat="identity")+
  scale_fill_gradient(low = "#4390CB", high = "#7773D0")+
  geom_text(aes(y=3,label=paste(format(name))))+
  scale_y_discrete(name ="Note moyenne")+
  labs( subtitle="Dès 12 ans", fill="Note moyenne")+
  theme(
    
    axis.text.y = element_blank(),
    axis.ticks=element_blank(),
    
    axis.title.y = element_blank()
    
  )+
  labs(
      caption= "Tidy Tuesday Semaine 4 | Data:Board Game ")
g4=g4+coord_flip()
g4
#regrouper les plots dans un même 
library(gridExtra)
g_f=grid.arrange(g2,g3,g4, ncol=1, nrow=3)

#enregistrer le graph
ggsave("TTSem4.pdf",plot=g_f) #pas la bonne taille
dev.copy2pdf(file="TTSem4.pdf") # non plus, débordement des titres des barres

# labs(title="Jeux les mieux notés par tranche d'âge minimum pour y jouer",
#      caption= "Tidy Tuesday Semaine 4 | Data:Board Game ")+
#   theme(
#     plot.title = element_text(size=16, face="bold")
#   )