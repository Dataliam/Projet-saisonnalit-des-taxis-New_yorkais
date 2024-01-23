## L3 MIASHS
## Projet AS - Taxis New York

################################################################################################

## Repertoire de travail
setwd("C:/Users/lebre/OneDrive/Bureau/AS/Projet AS")
getwd()

## Packages
library(lubridate)
library(ggplot2)
library(timeDate)
library(car)

################################################################################################
# I-Analyse descriptive ########################################################################
################################################################################################

df=read.csv(file = "new_york_taxis_2014-2015.csv",sep = ",",row.names=1)
nrow(df)
# conversion de timestamp en date + heure
df$timestamp <- ymd_hms(as.character(df$timestamp))

# on garde seulement le mois de janvier 2015
df <- df[(df$timestamp>="2015-01-01 00:00:00"),]

nrow(df)
summary(df)

### Graphiques

# graphique des données brutes 
ggplot(df, aes(x=timestamp, y=value)) +   
  geom_line(size=0.5,color="darkblue") +
  theme_light()+
  # légende
  labs(title = "Evolution du nombre de passagers de taxi à New York ",
       subtitle = "Janvier 2015",
       caption = "Données : The NYC Taxi and Limousine Commission",
       x = "Date", y = "Nombre total de passagers de taxi par tranche de 30 min")

# Régression linéaire : Nombre de passagers x Temps
reg <- lm(df$value~df$timestamp)
summary(reg) # p-value = 0.90 donc il n'y a pas de tendance linéaire notable au cours du mois

# graphique des données brutes en log 
ggplot(df, aes(x=timestamp, y=log(value))) +   
  geom_line(size=0.5,color="darkred") +
  theme_light()+
  # légende
  labs(title = "Evolution du nombre de passagers de taxi à New York ",
       subtitle = "Janvier 2015",
       caption = "Données : The NYC Taxi and Limousine Commission",
       x = "Date", y = "Logarithme du nombre de passagers par tranche de 30 min")

# création d'une colonne jour
df$jour <- strftime(df$timestamp, "%d")

# création d'une colonne jour de la semaine (weekday)
df$weekday <- weekdays(df$timestamp)
df$weekday <- factor(df$weekday, levels = c('lundi', 'mardi', 'mercredi', 'jeudi','vendredi','samedi','dimanche'))

# création d'une colonne semaine/week end
df$swe <-isWeekend(df$timestamp)
df$swe <- factor(df$swe)
levels(df$swe)[levels(df$swe)==TRUE] <- "Week-end"
levels(df$swe)[levels(df$swe)==FALSE] <- "Semaine"

# création d'une colonne heure
df$heure <- strftime(df$timestamp, "%H")
# création d'une colonne jour + heure
df$jour_heure <- strftime(df$timestamp, "%Y-%m-%d %H")


# agrégation des données par jour de la semaine (weekday) (moyenne)
df_weekday <- aggregate(value ~ weekday,df,FUN = mean)
# agrégation des données par semaine/week-end (sum)
df_swe <- aggregate(value ~ swe,df,FUN = sum)
# agrégation des données par heure (moyenne)
df_heure <- aggregate(value ~ heure,df,FUN = mean)
# agrégation des données par jour + heure (somme)
df_jour_heure <- aggregate(value ~ jour_heure,df,FUN = sum)
df_jour_heure$jour_heure <- ymd_h(as.character(df_jour_heure$jour_heure))
df_jour_heure$heure <- strftime(df_jour_heure$jour_heure, "%H")
df_jour_heure$jour <- strftime(df_jour_heure$jour_heure, "%d")
df_jour_heure$weekday <- weekdays(df_jour_heure$jour_heure)
df_jour_heure$weekday <- factor(df_jour_heure$weekday, levels =c('dimanche','samedi','vendredi','jeudi','mercredi','mardi','lundi'))
df_jour_heure$swe <-isWeekend(df_jour_heure$jour_heure)
df_jour_heure$swe <- factor(df_jour_heure$swe)
levels(df_jour_heure$swe)[levels(df_jour_heure$swe)==TRUE] <- "Week-end"
levels(df_jour_heure$swe)[levels(df_jour_heure$swe)==FALSE] <- "Semaine"
# graphique des données par jour + heure
ggplot(df_jour_heure, aes(x=jour_heure, y=value)) + 
  geom_line(size=0.7,color="blue") +
  theme_light()+
  # légende
  labs(title = "Evolution du nombre de passagers de taxi à New York ",
       subtitle = "Janvier 2015",
       caption = "Données : The NYC Taxi and Limousine Commission",
       x = "Date", y = "Nombre total de passagers de taxi par heure")

# graphique des données par jour
ggplot(df, aes(x=jour,y=value,group=jour)) + 
  geom_bar(stat = "identity",fill="purple") +
  theme_light()+
  # légende
  labs(title = "Evolution du nombre de passagers de taxi à New York ",
       subtitle = "Janvier 2015",
       caption = "Données : The NYC Taxi and Limousine Commission",
       x = "Jour du mois de Janvier", y = "Nombre total de passagers de taxi par jour")

# graphique des données par heure
ggplot(df_weekday, aes(x=weekday, y=value)) + 
  geom_bar(stat = "identity",fill="darkblue") +
  theme_light()+
  # légende
  labs(title = "Evolution du nombre de passagers de taxi à New York ",
       subtitle = "Janvier 2015",
       caption = "Données : The NYC Taxi and Limousine Commission",
       x = "Jour de la semaine", y = "Nombre moyen de passagers de taxi par jour de la semaine")

# graphique des données par heure
ggplot(df_heure, aes(x=heure, y=value)) + 
  geom_bar(stat = "identity",fill="darkred") +
  theme_light()+
  # légende
  labs(title = "Evolution du nombre de passagers de taxi à New York ",
       subtitle = "Janvier 2015",
       caption = "Données : The NYC Taxi and Limousine Commission",
       x = "Heure de la journée", y = "Nombre moyen de passagers de taxi par heure")


# pieplot Week-end/Semaine
df_swe$pourcentage=round(df_swe$value/sum(df_swe$value)*100,2)
ggplot(df_swe,aes(x="", y=value, fill=swe)) +
  geom_bar(stat="identity", width=1)+
  geom_text(aes(label = paste(pourcentage,"%",sep ="")),
            position = position_stack(vjust = 0.5)) +
  coord_polar("y", start=0)

# graphique des données par heure + jour de la semaine
ggplot(df_jour_heure, aes(x=heure,y=value,group=weekday,fill=weekday)) + 
  geom_bar(stat = "identity",position="dodge") +
  theme_light()+
  # légende
  labs(title = "Evolution du nombre de passagers de taxi à New York ",
       subtitle = "Janvier 2015",
       caption = "Données : The NYC Taxi and Limousine Commission",
       x = "Heure", y = "Nombre total de passagers de taxi par heure")

# graphique des données par heure + semaine ou week-end
ggplot(df_jour_heure, aes(x=heure,y=value,group=swe,fill=swe)) + 
  geom_bar(stat = "identity",position="dodge") +
  theme_light()+
  # légende
  labs(title = "Evolution du nombre de passagers de taxi à New York",
       subtitle = "Janvier 2015",
       caption = "Données : The NYC Taxi and Limousine Commission",
       x = "Heure", y = "Nombre total de passagers de taxi par heure")

# heatmaps : Jour x Heure
ggplot(df_jour_heure,aes(jour,heure,fill=value)) +
  geom_tile()+
  theme_classic() +
  scale_fill_viridis(name="Nombre de passagers par heure",option ="viridis") +
  labs(title = "Evolution du nombre de passagers de taxi à New York",
       subtitle = "Janvier 2015",
       caption = "Données : The NYC Taxi and Limousine Commission",
       x = "Jour", y = "Heure")

# heatmaps : Weekday x Jour
ggplot(df_jour_heure,aes(heure,weekday,fill=value)) +
  geom_tile()+
  theme_classic() +
  scale_fill_viridis(name="Nombre de passagers moyen par heure",option ="plasma") +
  labs(title = "Evolution du nombre de passagers de taxi à New York",
       subtitle = "Janvier 2015",
       caption = "Données : The NYC Taxi and Limousine Commission",
       x = "Jour du mois", y = "Jour de la semaine")


################################################################################################
# II-Moyennes mobiles ##########################################################################
################################################################################################

View(df)
df_periode<- ts(data = df$value,start = 01+(0/48),end = 31+(48/48),frequency = 48)
length(df_periode)
plot(df_periode,type="l")

# methode du profil
mat=matrix(data=df_periode[1:1488],nrow=31,ncol=48,byrow=TRUE)
ymin=min(mat[1:31,])
ymax=max(mat[1:31,])
plot(mat[1,],type="l",col=1,ylim=c(ymin,ymax))
for(i in 2:31) {lines(mat[i,],type="l",col=i,lwd=2)}

#on effectue un test de buys ballot pour determiner le modele
#calcul des moyenne par jour pour chaque observation
aggmean<- aggregate(df$value,list(jour=df$jour),mean)
#calcul des ecarts types par jour pour chaque observation
aggsd<-aggregate(df$value,list(jour=df$jour),FUN="sd")
#on effectue une regression lineaire 
buys_ballot<- lm(aggsd$x~aggmean$x)
summary(buys_ballot)
plot(aggsd$x~aggmean$x)

#on rejette l'hypothese nulle. Donc le modele est multiplicatif
# Transformation de la série : transformation de BOX-COX
lambda <- powerTransform(df$value)$lambda
df$mod <- ((df$value^lambda)-1)/lambda
plot(df$mod,type="l")

################################################################################################
# III-Lissages #################################################################################
################################################################################################






