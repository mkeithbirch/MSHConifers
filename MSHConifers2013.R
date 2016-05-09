require(readxl)
require(tidyr)
require(dplyr)
conifersxl<-read_excel("conifers2013 updated 2014.xlsx","forR")
conifers2013.tbl<-tbl_df(read_excel("conifers2013 updated 2014.xlsx","forR"))

#Calculate conifer abundance by site ("transect.point") for 2013.
conifers2013.tbl %>%
  group_by(transect.point) %>%
  summarise(
    Total13 = n()
  )
#Calculate conifer abundance by species and site for 2013.
spp.abund13<-conifers2013.tbl %>%
  group_by(transect.point, species = factor(species)) %>%
  summarise(
  count2013 = n()
  )
#create variables for 2013 species abundance by site (separate variables for each species)
  spp.abund13 <- spread(spp.abund13,species,count2013)
#Rename columns as "species code" + "13"
    colnames(spp.abund13)<-paste(colnames(spp.abund13),"13",sep="")
    spp.abund13