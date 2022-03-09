
##---Script 1:
################################################################################
## Title: Community characteristics of COVID-19 vaccine hesitancy in England: ## 
##                  A nationwide cross-sectional study                        ##
##                                                                            ##
## Authors: Bucyibaruta, G. - Blangiardo, M. -  Konstantinoudis, G.           ##
##                                                                            ##
##                                                                            ##
##                                                                            ##
################################################################################

#--This script covers the following sections

    ##----Loading required data (cleaned data)-----##
#       * Shapefile for MSOAs in England
#       * Vaccine uptake  data at MSOA level
#       * Covariates in orginal scale
#       * vaccine access data
#       * Spatial distribution of Vaccination sites

    ##-----Data exploration----------##

#       * Visualizing spatial distribution of covariates
#       * Assessing Correlation between covariates
#       *
#----------------------------------------------------------------------


# Loading packages

library(readODS);library(sf);library(leaflet);library(RColorBrewer);
library(tidyverse);library(xtable);library(classInt);library(viridis);
library(gridExtra);library(Hmisc);library(ggplot2);library(INLA);
library(spdep);library(ggplot2);library(patchwork);library(ghibli);
library(ggplotify);library(ggtext)
#--------------------------------------------------------------

###--------- Get MSOA boundaries -----------------###
#------- Download the MSOA Shapefile (polygon layer)-------#

url.shp <- "https://borders.ukdataservice.ac.uk/ukborders/easy_download/prebuilt/shape/England_msoa_2011_sgen_clipped.zip"
download.file(url.shp, destfile = "temp.zip", method = "auto" , quiet=FALSE)
unzip("temp.zip")
# Delete the original zip file
unlink("temp.zip")

# Read vac_msoa
msoa_poly <- st_read("england_msoa_2011_sgen_clipped.shp")
# Fix projection
msoa_poly <- st_transform(msoa_poly, 4326)
sf::sf_use_s2(FALSE)


###-------------- Loading  the vaccine uptake  data at MSOA level------------###

data_Dec <- read.csv("msoa_2022-01-02.csv",header = T)


names(data_Dec)[11] <- "PopVac"
names(data_Dec)[12] <- "Cum_firstDose"
names(data_Dec)[13] <- "First_dosePct"

##---Linking vaccine data with shapefile
vac_poly_Dec <- merge(msoa_poly, data_Dec[,c(7,11,12,13)],  by.x = "code", by.y = "areaCode", all.x = T)

data_Vaccine <- read.csv("msoa_2022-01-02.csv",header = T)


names(data_Vaccine)[6] <- "PopVac"
names(data_Vaccine)[7] <- "Cum_firstDose"
names(data_Vaccine)[8] <- "First_dosePct"

vac_poly_Dec <- merge(msoa_poly, data_Vaccine[,c(3,6,7,8)],  by.x = "code", by.y = "areaCode", all.x = T)

##################-- Loading Covariate in orginal scale--------#################

datav_og <- read.csv("DataVac_OG.csv", header = T)

datav_og <- datav_og[,-c(1,3,4,5,6,12,16,24,29,30)]

###-----Calculating min, quintiles and max 

round(quantile(datav_og$bme,probs = c(0,0.2, 0.4, 0.6,0.8,1)),2)
round(quantile(datav_og$pctYoungAge,probs = c(0,0.2, 0.4, 0.6,0.8,1)),2)
round(quantile(datav_og$PctOld,probs = c(0,0.2, 0.4, 0.6,0.8,1)),2)
round(quantile(datav_og$Pct_LAB,probs = c(0,0.2, 0.4, 0.6,0.8,1)),2)
round(quantile(datav_og$Pct_con,probs = c(0,0.2, 0.4, 0.6,0.8,1)),2)
round(quantile(datav_og$Pct_Leave,probs = c(0,0.2, 0.4, 0.6,0.8,1)),2)
round(quantile(datav_og$DeathRates,probs = c(0,0.2, 0.4, 0.6,0.8,1)),2)
round(quantile(datav_og$Asthma,probs = c(0,0.2, 0.4, 0.6,0.8,1)),2)
round(quantile(datav_og$blood_pre,probs = c(0,0.2, 0.4, 0.6,0.8,1)),2)
round(quantile(datav_og$Diabetes,probs = c(0,0.2, 0.4, 0.6,0.8,1)),2)
round(quantile(datav_og$Depression,probs = c(0,0.2, 0.4, 0.6,0.8,1)),2)



datav_og$rateCat <- factor(datav_og$rateCat)
datav_og$rateCat <- relevel(datav_og$rateCat,ref = "low")
datav_og$imd_decile_msoaLast <- factor(datav_og$imd_decile_msoaLast)
datav_og$B_RUC11 <- factor(datav_og$B_RUC11)
datav_og$B_RUC11 <- relevel(datav_og$B_RUC11,ref = "PU")
datav_og <- fastDummies::dummy_cols(datav_og,
                                    select_columns = "B_RUC11",
                                    remove_first_dummy = TRUE)
vac_poly_Dec_og <- merge(vac_poly_Dec, datav_og,  by.x = "code", by.y = "areaCode", all.x = T)


######-----Convert covariates into categories-------######

vac_poly_Dec_og$bmeG <- cut2(vac_poly_Dec_og$bme,g=5)
levels(vac_poly_Dec_og$bmeG) <- c("1(low)", "2","3","4","5")

vac_poly_Dec_og$leave <- cut2(vac_poly_Dec_og$Pct_Leave,g=5)
levels(vac_poly_Dec_og$leave) <- c("1(low)", "2","3","4","5")

vac_poly_Dec_og$asthma <- cut2(vac_poly_Dec_og$Asthma,g=5)
levels(vac_poly_Dec_og$asthma) <- c("1(low)", "2","3","4","5")

vac_poly_Dec_og$diabetes <- cut2(vac_poly_Dec_og$Diabetes,g=5)
levels(vac_poly_Dec_og$diabetes) <- c("1(low)", "2","3","4","5")

vac_poly_Dec_og$bp <- cut2(vac_poly_Dec_og$blood_pre,g=5)
levels(vac_poly_Dec_og$bp) <- c("1(low)", "2","3","4","5")

vac_poly_Dec_og$depression <- cut2(vac_poly_Dec_og$Depression,g=5)
levels(vac_poly_Dec_og$depression) <- c("1(low)", "2","3","4","5")

vac_poly_Dec_og$Cons <- cut2(vac_poly_Dec_og$Pct_con,g=5)
levels(vac_poly_Dec_og$Cons) <- c("1(low)", "2","3","4","5")

vac_poly_Dec_og$LAB <- cut2(vac_poly_Dec_og$Pct_LAB,g=5)
levels(vac_poly_Dec_og$LAB) <- c("1(low)", "2","3","4","5")

vac_poly_Dec_og$death <- cut2(vac_poly_Dec_og$rates,g=5)
levels(vac_poly_Dec_og$death) <- c("1(low)", "2","3","4","5")

vac_poly_Dec_og$young <- cut2(vac_poly_Dec_og$pctYoungAge,g=5)
levels(vac_poly_Dec_og$young) <- c("1(low)", "2","3","4","5")

vac_poly_Dec_og$old <- cut2(vac_poly_Dec_og$PctOld,g=5)
levels(vac_poly_Dec_og$old) <- c("1(low)", "2","3","4","5")

vac_poly_Dec_og$imd_rank_msoaLast <-round(rank(-vac_poly_Dec_og$imd_medianS_msoa))
vac_poly_Dec_og$imd_quintile <- cut2(vac_poly_Dec_og$imd_rank_msoaLast, g=5)
levels(vac_poly_Dec_og$imd_quintile) <- 
  c("low", "second","third","fourth","high")
vac_poly_Dec_og$imd_quintile <- relevel(vac_poly_Dec_og$imd_quintile,ref = "high")


vac_poly_Dec_og$urbanicity <- vac_poly_Dec_og$B_RUC11

levels(vac_poly_Category$urbanicity)<  c("Urban","Rural","Semi")
#vac_poly_Category <- vac_poly_Dec_og

##--------- Subseting  categorical data to use in model------------##

vac_poly_Category <-vac_poly_Dec_og[,c(1,2,3,4,5,46:60)]

##---Reverse the order of levels in IMD
vac_poly_Category$imd_quintileNew <- ifelse(vac_poly_Category$imd_quintile=="low", "5",
                                            ifelse(vac_poly_Category$imd_quintile=="second", "4",
                                                   ifelse(vac_poly_Category$imd_quintile=="third","3",
                                                          ifelse(vac_poly_Category$imd_quintile=="fourth","2",
                                                                 "1(least)"))))
vac_poly_Category$imd_quintileNew <- factor(vac_poly_Category$imd_quintileNew, levels = c("1(least)", "2","3","4","5"))


####------ Laoding vaccine access data-------------######

Vac_Acces <- read.csv("Weighted_VAc_Acc_Data.csv")

round(quantile(Vac_Acces$WeightVacAcc,probs = c(0,0.2, 0.4, 0.6,0.8,1)),2)

vac_poly_Category <- merge(vac_poly_Category, Vac_Acces[,c(2,7,8)],  by.x = "code", by.y = "code", all.x = T)

vac_poly_Category$WVA <- cut2(vac_poly_Category$WeightVacAcc, g=5)
levels(vac_poly_Category$WVA) <- c("1(low)", "2","3","4","5")



############################################################################################
###############################----Data exploration----------#############################
###########################################################################################


vac_poly_Category %>% 
  ggplot() + 
  geom_sf(aes(fill = bmeG), color = NA, show.legend = TRUE) +
  theme_void() +
  #scale_fill_viridis_d(name = "", alpha = 0.8,begin = 0,
  #end = 1,
  #direction = 1,
  #option = "D",
  #aesthetics = "colour")  +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.5,end=1)+
  theme(legend.position = c(0.92, 0.85),
        legend.title =  element_blank(),
        plot.title = element_text(size=9, face = "bold",hjust = 0.5))+
  ggtitle("BME")->Bme
#--------------
vac_poly_Category %>% 
  ggplot() + 
  geom_sf(aes(fill = imd_quintileNew), color = NA, show.legend = TRUE) +
  theme_void() +
  #scale_fill_viridis_d(name = "", alpha = 0.8,begin = 0,
  #end = 1,
  #direction = 1,
  #option = "D",
  #aesthetics = "colour")  +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.5,end=1)+
  theme(legend.position = c(0.92, 0.85),
        legend.title = element_text(face="bold"),
        plot.title = element_text(size=9, face = "bold",hjust = 0.5))+
  labs(fill='IMD')+
  ggtitle("IMD")->imd
#-------------------
vac_poly_Category %>% 
  ggplot() + 
  geom_sf(aes(fill = young), color = NA, show.legend = TRUE) +
  theme_void() +
  #scale_fill_viridis_d(name = "", alpha = 0.8,begin = 0,
  #end = 1,
  #direction = 1,
  #option = "D",
  #aesthetics = "colour")  +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.5,end=1)+
  theme(legend.position = c(0.92, 0.85),
        legend.title =  element_blank(),
        plot.title = element_text(size=9, face = "bold",hjust = 0.5))+
  ggtitle("Young")->Young

#---------------------

vac_poly_Category %>% 
  ggplot() + 
  geom_sf(aes(fill = old), color = NA, show.legend = TRUE) +
  theme_void() +
  #scale_fill_viridis_d(name = "", alpha = 0.8,begin = 0,
  #end = 1,
  #direction = 1,
  #option = "D",
  #aesthetics = "colour")  +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.5,end=1)+
  theme(legend.position = c(0.92, 0.85),
        legend.title =  element_blank(),
        plot.title = element_text(size=9, face = "bold",hjust = 0.5))+
  ggtitle("Old")->Old

#---------
vac_poly_Category %>% 
  ggplot() + 
  geom_sf(aes(fill = death), color = NA, show.legend = TRUE) +
  theme_void() +
  #scale_fill_viridis_d(name = "", alpha = 0.8,begin = 0,
  #end = 1,
  #direction = 1,
  #option = "D",
  #aesthetics = "colour")  +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.5,end=1)+
  theme(legend.position = c(0.92, 0.85),
        legend.title =  element_blank(),
        plot.title = element_text(size=9, face = "bold",hjust = 0.5))+
  ggtitle("Mortality")->Death

#------------
vac_poly_Category %>% 
  ggplot() + 
  geom_sf(aes(fill = urbanicity), color = NA, show.legend = TRUE) +
  theme_void() +
  #scale_fill_viridis_d(name = "", alpha = 0.8,begin = 0,
  #end = 1,
  #direction = 1,
  #option = "D",
  #aesthetics = "colour")  +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.5,end=1)+
  theme(legend.position = c(0.92, 0.85),
        legend.title =  element_blank(),
        plot.title = element_text(size=9, face = "bold",hjust = 0.5))+
  ggtitle("Urbanicity")->Urban

#----------
vac_poly_Category %>% 
  ggplot() + 
  geom_sf(aes(fill = LAB), color = NA, show.legend = TRUE) +
  theme_void() +
  #scale_fill_viridis_d(name = "", alpha = 0.8,begin = 0,
  #end = 1,
  #direction = 1,
  #option = "D",
  #aesthetics = "colour")  +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.5,end=1)+
  theme(legend.position = c(0.92, 0.85),
        legend.title =  element_blank(),
        plot.title = element_text(size=9, face = "bold",hjust = 0.5))+
  ggtitle("Labour")->lab

#----------
vac_poly_Category %>% 
  ggplot() + 
  geom_sf(aes(fill = Cons), color = NA, show.legend = TRUE) +
  theme_void() +
  #scale_fill_viridis_d(name = "", alpha = 0.8,begin = 0,
  #end = 1,
  #direction = 1,
  #option = "D",
  #aesthetics = "colour")  +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.5,end=1)+
  theme(legend.position = c(0.92, 0.85),
        legend.title =  element_blank(),
        plot.title = element_text(size=9, face = "bold",hjust = 0.5))+
  ggtitle("Conservatives")->cons

#------------------
vac_poly_Category %>% 
  ggplot() + 
  geom_sf(aes(fill = leave), color = NA, show.legend = TRUE) +
  theme_void() +
  #scale_fill_viridis_d(name = "", alpha = 0.8,begin = 0,
  #end = 1,
  #direction = 1,
  #option = "D",
  #aesthetics = "colour")  +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.5,end=1)+
  theme(legend.position = c(0.92, 0.85),
        legend.title =  element_blank(),
        plot.title = element_text(size=9, face = "bold",hjust = 0.5))+
  ggtitle("Brexit")->Leave

#----------------------
vac_poly_Category %>% 
  ggplot() + 
  geom_sf(aes(fill = asthma), color = NA, show.legend = TRUE) +
  theme_void() +
  #scale_fill_viridis_d(name = "", alpha = 0.8,begin = 0,
  #end = 1,
  #direction = 1,
  #option = "D",
  #aesthetics = "colour")  +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.5,end=1)+
  theme(legend.position = c(0.92, 0.85),
        legend.title =  element_blank(),
        plot.title = element_text(size=9, face = "bold",hjust = 0.5))+
  ggtitle("Asthma")->Asthma

#------------
vac_poly_Category %>% 
  ggplot() + 
  geom_sf(aes(fill = bp), color = NA, show.legend = TRUE) +
  theme_void() +
  #scale_fill_viridis_d(name = "", alpha = 0.8,begin = 0,
  #end = 1,
  #direction = 1,
  #option = "D",
  #aesthetics = "colour")  +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.5,end=1)+
  theme(legend.position = c(0.92, 0.85),
        legend.title =  element_blank(),
        plot.title = element_text(size=9, face = "bold",hjust = 0.5))+
  ggtitle("Blood Pressure")->BP

#--------------
vac_poly_Category %>% 
  ggplot() + 
  geom_sf(aes(fill = diabetes), color = NA, show.legend = TRUE) +
  theme_void() +
  #scale_fill_viridis_d(name = "", alpha = 0.8,begin = 0,
  #end = 1,
  #direction = 1,
  #option = "D",
  #aesthetics = "colour")  +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.5,end=1)+
  theme(legend.position = c(0.92, 0.85),
        legend.title =  element_blank(),
        plot.title = element_text(size=9, face = "bold",hjust = 0.5))+
  ggtitle("Diabetes")->Diab

#----------------
vac_poly_Category %>% 
  ggplot() + 
  geom_sf(aes(fill = depression), color = NA, show.legend = TRUE) +
  theme_void() +
  #scale_fill_viridis_d(name = "", alpha = 0.8,begin = 0,
  #end = 1,
  #direction = 1,
  #option = "D",
  #aesthetics = "colour")  +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.5,end=1)+
  theme(legend.position = c(0.92, 0.85),
        legend.title =  element_blank(),
        plot.title = element_text(size=9, face = "bold",hjust = 0.5))+
  ggtitle("Depression")->Depres

#-----------
vac_poly_Category %>% 
  ggplot() + 
  geom_sf(aes(fill = WVA), color = NA, show.legend = TRUE) +
  theme_void() +
  #scale_fill_viridis_d(name = "", alpha = 0.8,begin = 0,
  #end = 1,
  #direction = 1,
  #option = "D",
  #aesthetics = "colour")  +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.5,end=1)+
  theme(legend.position = c(0.92, 0.85),
        legend.title =  element_blank(),
        plot.title = element_text(size=9, face = "bold",hjust = 0.5))+
  ggtitle("Access")->access

Socio=((Bme|Old|Young)/(imd|Urban))

x11()

Socio +plot_layout(guides = "collect")+
  plot_annotation(title = "Socio-Demographics and urbanicity",
                  theme = theme(plot.title = element_text(face = "bold",size = 14)))

ggsave("Socio-Demographics.pdf",width=10,height =8)
#-----------------------------

poli_acc=((lab|cons)/(Leave|access))

x11()
poli_acc +plot_layout(guides = "collect")+
  plot_annotation(title = "Politics view and Access",
                  theme = theme(plot.title = element_text(face = "bold",size = 14)))

ggsave("Politics and Access.pdf",width=10,height =8)

#-----------------------------

Health=((Death|Asthma|BP)/(Diab|Depres))

x11()
Health +plot_layout(guides = "collect")+
  plot_annotation(title = "COVID-19 awareness and targeting of high risk groups",
                  theme = theme(plot.title = element_text(face = "bold",size = 14)))

ggsave("health-conditions.pdf",width=10,height =8)


##########################################################################
###-----------Correlation between covariates---------------##############
#########################################################################


DD <- data.frame(vac_poly_Category$bmeG,
                 vac_poly_Category$imd_quintileNew,
                 vac_poly_Category$young,
                 vac_poly_Category$old,
                 vac_poly_Category$death,
                 vac_poly_Category$urbanicity,
                 vac_poly_Category$LAB,
                 vac_poly_Category$Cons,
                 vac_poly_Category$leave,
                 vac_poly_Category$asthma,
                 vac_poly_Category$bp,
                 vac_poly_Category$diabetes,
                 vac_poly_Category$depression,
                 vac_poly_Category$WVA)

colnames(DD) <- c("BME", "IMD", "Young",
                  "Old", "Mortality", "Urbanicity",
                  "Labour", "Conservatives", "Brexit",
                  "Asthma", "Bood pressure","Diabetes",
                  "Depression", "Access")

levels(DD$BME) <- c(1,2,3,4,5)
levels(DD$IMD) <- c(1,2,3,4,5)
levels(DD$Young) <- c(1,2,3,4,5)
levels(DD$ Old) <- c(1,2,3,4,5)
levels(DD$Mortality) <- c(1,2,3,4,5)
levels(DD$Urbanicity) <- c(1,2,3)
levels(DD$Labour) <- c(1,2,3,4,5)
levels(DD$Conservatives) <- c(1,2,3,4,5)
levels(DD$Brexit) <- c(1,2,3,4,5)
levels(DD$Asthma) <- c(1,2,3,4,5)
levels(DD$`Bood pressure`) <- c(1,2,3,4,5)
levels(DD$Diabetes) <- c(1,2,3,4,5)
levels(DD$Depression) <- c(1,2,3,4,5)
levels(DD$Access) <- c(1,2,3,4,5)

library("varhandle")
DD=unfactor(DD)

cov_cor=round(cor(DD, method="kendall", use="pairwise"),2)
cov_cor

#----Using ggcorrplot (plot to save)

library(ggcorrplot)

# plotting corr heatmap
x11()
ggcorrplot(cov_cor,lab = TRUE,colors = c("red", "white", "blue"))

ggsave("Heatmap_cor3.pdf",width = 10,height = 10)

########################################################################
###--------Vaccination centers and vaccine access-------------##########
#######################################################################

vw=vac_poly_Category %>% 
  ggplot() + 
  geom_sf(aes(fill = AccWeighted), color = NA, show.legend = TRUE)+
  scale_fill_manual(values = c("#FC4E07","#0072B2" , 
                               "#00AFBB","#F0E442", "#CC79A7","#999999" )) +
  theme_void() +
  theme(legend.title = element_blank(),
        plot.title = element_text(size=9,face = "bold",hjust = 0.5))+
  ggtitle(" Vaccination Access ")

x11()
vw
ggsave("weighted_Vac_Acc.tiff",width = 14,height = 10)


va=ggplot() + 
  geom_sf(data = msoa_poly, fill = NA) +
  geom_point(data = vac_access,
             aes(x=longitude,y=latitude,colour= site),size=1)+
  
  scale_colour_manual(values = c("#FC4E07","#0072B2" , "#F0E442","#CC79A7" ))+
  theme_void() +
  theme(legend.title =  element_blank(),
        plot.title = element_text(size=9, face = "bold",hjust = 0.5))+
  ggtitle("Viccination sites")
va
ggsave("vac_AccessSites.pdf",width = 10,height = 12)

acces=va|vw

x11()

acces +plot_layout(guides = "collect")+
  plot_annotation(title = "Vaccine Accessibility",
                  theme = theme(plot.title = element_text(face = "bold",size = 14)))

ggsave("VacAccesNew.pdf",width=12,height =14)

