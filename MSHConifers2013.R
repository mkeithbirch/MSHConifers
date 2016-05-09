library(readxl)
library(tidyr)
library(dplyr)
library(stargazer)
conifersxl<-read_excel("conifers2013 updated 2014.xlsx","forR")
conifers2013.tbl<-tbl_df(read_excel("conifers2013 updated 2014.xlsx","forR"))

#Calculate conifer abundance by species and site for 2013.
spp.abund13 <- conifers2013.tbl %>%
  group_by(transect.point, species = factor(species)) %>%
  summarise(
  count2013 = n()
  )
#create variables for 2013 species abundance by site (separate variables for each species)
  spp.abund13 <- spread(spp.abund13,species,count2013, fill = 0) 
#Rename columns as "species code" + "13"
    colnames(spp.abund13)<-paste(colnames(spp.abund13),"13",sep="")
    spp.abund13
#Add new variable "Total13", total conifer count by site
  spp.abund13 <- spp.abund13 %>% mutate(Total13 = sum(c(ABPR13, PICO13, PIMO13, PSME13, THPL13, TSHE13, UNKNOWN13), na.rm = TRUE))

#load in topography data (includes 2010 conifer counts)
topo.cnt <- tbl_df(read.csv("topo.count.csv"))
#Join 2013 data with topo.cnt
topo.cnt.all <- full_join(topo.cnt, spp.abund13, by = c("t.pt" = "transect.point13"))
#Create new table from topo.cnt.all that includes only the data we want to use in the analysis
topo.cnt.analyze <- topo.cnt.all %>% 
  #Filter data to exlcude sites without heat load (or other spatial) data
  filter(!is.na(hl.3)) %>% 
  #Exclude sites noted to be recently disturbed in 2010 data
  filter(disturb == "N") %>% 
#Assuming all sites surveyed in 2010 were also surveyed in 2013, we can replace "NA" values in 2013 abundance with 0.
  replace_na(list(Total13 = 0, ABPR13 = 0, PICO13 = 0, PIMO13 = 0,
                  PSME13 = 0, THPL13 = 0, TSHE13 = 0, UNKNOWN13 = 0)) 

#2013 conifer data spatial models:
#all conifers model for heat load (hl.3) used in thesis, quasipoisson error distribution
qglm.hl13<-glm(Total13~hl.3+rgh.mu.mean,data=topo.cnt.analyze,quasipoisson) 
summary(qglm.hl13)
#all conifers model for incident radiation (ir.3) used in thesis, quasipoisson error distribution
qglm.ir13<-glm(Total13~ir.3+rgh.mu.mean,data=topo.cnt.analyze,quasipoisson)
summary(qglm.ir13)
#ABPR model for incident radiation (ir.3)
qglm.abpr.ir13<-glm(ABPR13~ir.3+rgh.mu.mean,data=topo.cnt.analyze,quasipoisson)
summary(qglm.abpr.ir13)
#model for abpr using heat load (hl.3)
qglm.abpr.hl13<-glm(ABPR13~hl.3+rgh.mu.mean,data=topo.cnt.analyze,quasipoisson)
summary(qglm.abpr.hl13)
#incident radiation model for psme
qglm.psme.ir13<-glm(PSME13~ir.3+rgh.mu.mean,data=topo.cnt.analyze,quasipoisson)
summary(qglm.psme.ir13)
#heat load model for psme
qglm.psme.hl13<-glm(PSME13~hl.3+rgh.mu.mean,data=topo.cnt.analyze,quasipoisson)
summary(qglm.psme.hl13)
#heat load model for tshe
qglm.tshe.hl13<-glm(TSHE13~hl.3+rgh.mu.mean,data=topo.cnt.analyze,quasipoisson)
summary(qglm.tshe.hl13)
#incident radiation model for tshe
qglm.tshe.ir13<-glm(TSHE13~ir.3+rgh.mu.mean,data=topo.cnt.analyze,quasipoisson)
summary(qglm.tshe.ir13)

#2010 conifer data spatial models:
#all conifers model for heat load (hl.3) used in thesis, quasipoisson error distribution
qglm.hl10<-glm(Total10~hl.3+rgh.mu.mean,data=topo.cnt.analyze,quasipoisson)
summary(qglm.hl10)
#all conifers model for incident radiation (ir.3) used in thesis, quasipoisson error distribution
qglm.ir10<-glm(Total10~ir.3+rgh.mu.mean,data=topo.cnt.analyze,quasipoisson)
summary(qglm.ir10)
#ABPR model for incident radiation (ir.3)
qglm.abpr.ir10<-glm(ABPR10~ir.3+rgh.mu.mean,data=topo.cnt.analyze,quasipoisson)
summary(qglm.abpr.ir10)
#model for abpr using heat load (hl.3)
qglm.abpr.hl10<-glm(ABPR10~hl.3+rgh.mu.mean,data=topo.cnt.analyze,quasipoisson)
summary(qglm.abpr.hl10)
#incident radiation model for psme
qglm.psme.ir10<-glm(PSME10~ir.3+rgh.mu.mean,data=topo.cnt.analyze,quasipoisson)
summary(qglm.psme.ir10)
#heat load model for psme
qglm.psme.hl10<-glm(PSME10~hl.3+rgh.mu.mean,data=topo.cnt.analyze,quasipoisson)
summary(qglm.psme.hl10)
#heat load model for tshe
qglm.tshe.hl10<-glm(TSHE10~hl.3+rgh.mu.mean,data=topo.cnt.analyze,quasipoisson)
summary(qglm.tshe.hl10)
#incident radiation model for tshe
qglm.tshe.ir10<-glm(TSHE10~ir.3+rgh.mu.mean,data=topo.cnt.analyze,quasipoisson)
summary(qglm.tshe.ir10)