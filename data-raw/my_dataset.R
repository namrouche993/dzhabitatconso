## code to prepare `my_dataset` dataset goes here
library(tidyverse)
library(readxl)
library(geojsonio)
library(rmapshaper)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)

donnees_consomation <- read_excel(paste0(getwd(),"/data-raw/donnees_stat.xlsx"),sheet="Consommation")
dd0=donnees_consomation
dd0=dd0 %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`)
dd0d =donnees_consomation %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`)
for(i in 1:50){
  for(j in 2:11){
    dd0[i,j]=as.numeric(format(round(dd0[i,j]/ 1e9, 1), trim = TRUE))
  }
}
dd16=c("16-ALGER (DEB+BMR+HD)",0,0,0,0,0,0,0,0,0)
dd16
for(i in 2:11){
  dd16[i]=sum(dd0[16:18,i])
}

dd160=data.frame(t(dd16))
colnames(dd160)=c("OPGI","2013","2014","2015","2016","2017","2018","2019","2020","2021","Total")
dd48=rbind(dd0[1:15,],dd160,dd0[19:50,])
dd48=data.frame(dd48)
for(i in 1:48){
  for(j in 2:11){
    dd48[i,j]=as.numeric(dd48[i,j])
  }
}
colnames(dd48)=c("OPGI","2013","2014","2015","2016","2017","2018","2019","2020","2021","Total")
dd=donnees_consomation %>% gather("Annee","consomation",2:10)




######################## consomation mensuelle 2021
#conso_21 <- read_excel(paste0(getwd(),"/data-raw/conso.xlsx"))
dco21 <- read_excel(paste0(getwd(),"/data-raw/donnees_stat.xlsx"),sheet="consomation21")
dd_21=dco21

dco21=dco21 %>% select(-1) %>% mutate(Prevtotal=Prev1+Prev2+Prev3+Prev4+Prev5+Prev6+Prev7+Prev8+Prev9,
                                      Realtotal=Real1+Real2+Real3+Real4+Real5+Real6+Real7+Real8+Real9,
                                      tauxtotal=Realtotal/Prevtotal
)

#dd0_21=dd0_21 %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`)
#dd0d =donnees_consomation %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`)


dco48_21 <- read_excel(paste0(getwd(),"/data-raw/donnees_stat.xlsx"),sheet="consomation21M")
#dd_21=donnees_consomation %>% gather("Annee","consomation",2:10)

######################### consomation mensuelle 2021 f



######################## consomation mensuelle 2021
#conso_21 <- read_excel(paste0(getwd(),"/data-raw/conso.xlsx"))
donnees_consomation_21 <- read_excel(paste0(getwd(),"/data-raw/donnees_stat.xlsx"),sheet="consomation_mensuelles_2021")
dd0_21=donnees_consomation_21
#dd0_21=dd0_21 %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`)
#dd0d =donnees_consomation %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`)


donnees_consomation48_21 <- read_excel(paste0(getwd(),"/data-raw/donnees_stat.xlsx"),sheet="consomationd48_mensuelles_2021")
#dd_21=donnees_consomation %>% gather("Annee","consomation",2:10)

consommation2021_data_hchart <- read_excel(paste0(getwd(),"/data-raw/donnees_stat.xlsx"),sheet="Consommation2021")

consommation2021_data_distplot2 <- read_excel(paste0(getwd(),"/data-raw/donnees_stat.xlsx"),sheet="consomationd48_mensuelles_2021")

consommation2021_data_distplot2=
  consommation2021_data_distplot2 %>%
  mutate(Prevision_m=as.numeric(round(Prevision/1e6, 1)),
         Consommations_m=as.numeric(round(Realisation/1e6, 1)),
         Taux=Taux*100
  )
######################### consomation mensuelle 2021 f






####################################### Livraison #####################
#donnees_consomation <- read_excel(paste0(getwd(),"/data-raw/donnees_stat.xlsx"),sheet="Consommation")


donnees_livraison <- read_excel(paste0(getwd(),"/data-raw/donnees_stat.xlsx"),sheet="Livraison")
dd0_livraison=donnees_livraison
dd0_livraison=dd0_livraison %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`)
dd0d_livraison =donnees_livraison %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`)
for(i in 1:48){
  for(j in 2:11){
    dd0_livraison[i,j]=as.numeric(dd0_livraison[i,j])
  }
}
dd48_livraison=dd0_livraison

colnames(dd48_livraison)=c("Wilayas","2013","2014","2015","2016","2017","2018","2019","2020","2021","Total")
dd_livraison=donnees_livraison %>% gather("Annee","Livraison",2:10)
####################################### Livraison #####################




####################################### Encours #####################
#donnees_consomation <- read_excel(paste0(getwd(),"/data-raw/donnees_stat.xlsx"),sheet="En Cours")


donnees_encours <- read_excel(paste0(getwd(),"/data-raw/donnees_stat.xlsx"),sheet="En cours")
dd0_encours=donnees_encours
dd0_encours=dd0_encours %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`)
dd0d_encours =donnees_encours %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`)
for(i in 1:48){
  for(j in 2:10){
    dd0_encours[i,j]=as.numeric(dd0_encours[i,j])
  }
}
dd48_encours=dd0_encours
colnames(dd48_encours)=c("Wilayas","2013","2014","2015","2016","2017","2018","2019","2020","Total")
dd_encours=donnees_encours %>% gather("Annee","EnCours",2:9)
####################################### Livraison #####################




####################################### Non Lances #####################
#donnees_consomation <- read_excel(paste0(getwd(),"/data-raw/donnees_stat.xlsx"),sheet="Non Lances")


donnees_nonlances <- read_excel(paste0(getwd(),"/data-raw/donnees_stat.xlsx"),sheet="Non lances")
dd0_nonlances=donnees_nonlances
dd0_nonlances=dd0_nonlances %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`)
dd0d_nonlances =donnees_nonlances %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`)
for(i in 1:48){
  for(j in 2:10){
    dd0_nonlances[i,j]=as.numeric(dd0_nonlances[i,j])
  }
}
dd48_nonlances=dd0_nonlances
colnames(dd48_nonlances)=c("Wilayas","2013","2014","2015","2016","2017","2018","2019","2020","Total")
dd_nonlances=donnees_nonlances %>% gather("Annee","NonLances",2:9)


#m6=readxl::read_excel(paste0(getwd(),"/data-raw/before2000.xlsx"))

######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################


usethis::use_data(donnees_consomation,overwrite = FALSE)
usethis::use_data(donnees_consomation_21,overwrite = FALSE)
usethis::use_data(donnees_consomation48_21,overwrite = FALSE)
usethis::use_data(donnees_encours,overwrite = FALSE)
usethis::use_data(donnees_livraison,overwrite = FALSE)
usethis::use_data(donnees_nonlances,overwrite = FALSE)


usethis::use_data(dd48_nonlances,
                  dd48_livraison,
                  dd48_encours,
                  dd48,
                  dd160,
                  dd0d_nonlances,
                  dd0d_livraison,
                  dd0d_encours,
                  dd0d,
                  dd0_nonlances,
                  dd0_livraison,
                  dd0_encours,
                  dd0_21,
                  dd0,
                  dd_nonlances,
                  dd_livraison,
                  dd_encours,
                  dd_21,
                  dd0,
                  dd_nonlances,
                  dd_livraison,
                  dd_encours,
                  dd_21,
                  dd,
                  dco48_21,
                  dco21,
                  consommation2021_data_hchart,
                  consommation2021_data_distplot2,


                  overwrite = TRUE,internal = TRUE)

countries <- geojsonio::geojson_read(paste0(getwd(),"/data-raw/polbnda_dza.json"), what = "sp")
algeria <- rmapshaper::ms_simplify(countries, keep = 0.05, keep_shapes = TRUE)



id_wilaya=c(27,31,29,22,46,13,20,15,6,35,16,42,9,10,2,19,26,44,34,28,38,48,17,14,5,7,21,23,36,18,24,43,25,41,4,12,40,8,32,45,1,3,47,30,39,33,37,11)
algeria@data$id_wilaya=id_wilaya
algeria@data=algeria@data[1:96,]
algeria@data=algeria@data[1:48,]
gps=data.frame(longitude=rep(0,48),latitude=rep(0,48))
gps=data.frame(longitude=rep(0,48),latitude=rep(0,48))
for(i in 1:48){
  gps[i,]=algeria@polygons[[i]]@labpt
}
algeria@data$longitude=gps$longitude
algeria@data$latitude=gps$latitude
algeria@data$wilayas=unique(dco48_21$OPGI)[id_wilaya]

mapdz=leaflet(algeria)%>%
  setView(lng = 3.03333 , lat = 28.6167, zoom = 5)%>%
  addProviderTiles("OpenStreetMap.BZH",
                   options = leafletOptions(minZoom = 5, maxZoom = 10,dragging = TRUE))%>%
  setMapWidgetStyle(list(background= "#ffffff"))


usethis::use_data(mapdz,algeria, overwrite = FALSE)



mapdz2=leaflet(algeria)%>%
  setView(lng = 1.63333 , lat = 28.3667, zoom = 5)%>%
  addProviderTiles("OpenStreetMap.BZH") %>%
  setMapWidgetStyle(list(background= "#ffffff"))


mapdz3=leaflet(dzhabitatconso::algeria)%>%
  setView(lng = 1.63333 , lat = 28.3667, zoom = 5)%>%
  addProviderTiles("OpenStreetMap.BZH") %>%
  setMapWidgetStyle(list(background= "#ffffff"))

usethis::use_data(mapdz2,mapdz3, overwrite = FALSE)
