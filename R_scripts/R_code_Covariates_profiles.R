
##---Script 3:
################################################################################
## Title: Community characteristics of COVID-19 vaccine hesitancy in England: ## 
##                  A nationwide cross-sectional study                        ##
##                                                                            ##
## Authors: Bucyibaruta, G. - Blangiardo, M. -  Konstantinoudis, G.           ##
##                                                                            ##
##                                                                            ##
##                                                                            ##
################################################################################


##########################################################################
###########----Heatmap of profiles and Poesterior probabilities-----#####
#########################################################################

data1<- vac_poly_Category
data1$pred <- pred
data1$predrnk <- predrnk
data1$low <- low


data1$bmeG <- as.numeric(data1$bmeG)
data1$imd_quintile <- as.numeric(data1$imd_quintile)
data1$deathNew <- as.numeric(data1$deathNew)
data1$young <- as.numeric(data1$young)
data1$old <- as.numeric(data1$old)
data1$depression <- as.numeric(data1$depression)
data1$urbanicity <- as.numeric(data1$urbanicity)
data1$Cons <- as.numeric(data1$Cons)
data1$LAB <- as.numeric(data1$LAB)
data1$leave <- as.numeric(data1$leave)
data1$asthma <- as.numeric(data1$asthma)
data1$diabetes <- as.numeric(data1$diabetes)
data1$bp <- as.numeric(data1$bp)
data1$WVacAcc <- as.numeric(data1$WVacAcc)

data1$groupPred <- cut2(data1$pred,g=5)


data1$geometry <- NULL
library(tibble)
library(dplyr)
library(ggplot2)

data1<-as_tibble(data1)
data1 <- data1[,-11]

data1$imd_quintileNew <- ifelse(data1$imd_quintile==1, 5,
                                ifelse(data1$imd_quintile==2, 4,
                                       ifelse(data1$imd_quintile==3,3,
                                              ifelse(data1$imd_quintile==2,4,
                                                     1))))

Data2<-data1 %>%
  group_by(bmeG,imd_quintileNew,deathNew,young,old,urbanicity,
           LAB,Cons,leave,asthma, bp,depression, diabetes,WVacAcc) %>%
  summarise(mean=mean(pred),sd=sd(pred),
            q1=mean(low,0.025),
            q3=mean(high,0.975),
            n=n())%>%
  arrange(mean)%>% 
  rename(bme=bmeG,IMD=imd_quintileNew, Mortality=deathNew,YoungAge=young
         ,OldAge=old,urbanicity=urbanicity,LAB=LAB,
         Cons=Cons,EURef=leave,asthma=asthma, bloodPressure=bp,depression=depression,
         diabetes=diabetes,vac_accessibility=WVacAcc
  ) %>%
  ungroup %>%   
  mutate(ID=row_number()) %>%
  tidyr::gather(Covariates,Value,1:13) %>%
  arrange(mean)



## Selecting MASOAs with lower and high probabilities

LH_msoaData <- data1[data1$groupPred == "[0.376,0.724)" | data1$groupPred == "[0.894,0.940]",]

LH_msoaDataLow <- data1[data1$pred <0.724,]

LH_msoaDatahigh <- data1[data1$pred >= 0.894,]

##-----All profiles
Data2<-LH_msoaData %>%
  group_by(bmeG,imd_quintileNew,young,old,deathNew,urbanicity,
           LAB,Cons,leave,asthma, bp, diabetes,depression,WVacAcc) %>%
  summarise(mean=mean(pred),sd=sd(pred),
            q1=mean(low,0.025),
            q3=mean(high,0.975),
            n=n())%>%
  arrange(mean)%>% 
  rename(bme=bmeG,IMD=imd_quintileNew, Mortality=deathNew,YoungAge=young
         ,OldAge=old,urbanicity=urbanicity,LAB=LAB,
         Cons=Cons,EURef=leave,asthma=asthma, bloodPressure=bp,depression=depression,
         diabetes=diabetes,vac_accessibility=WVacAcc
  ) %>%
  ungroup %>%   
  mutate(ID=row_number()) %>%
  tidyr::gather(Covariates,Value,1:14) %>%
  arrange(mean)

###------Low profiles
Data2low<-LH_msoaDataLow %>%
  group_by(bmeG,imd_quintileNew,young,old,deathNew,urbanicity,
           LAB,Cons,leave,asthma, bp, diabetes,depression,WVacAcc) %>%
  summarise(mean=mean(pred),sd=sd(pred),
            q1=mean(low,0.025),
            q3=mean(high,0.975),
            n=n())%>%
  arrange(mean)%>% 
  rename(bme=bmeG,IMD=imd_quintileNew, Mortality=deathNew,YoungAge=young
         ,OldAge=old,urbanicity=urbanicity,LAB=LAB,
         Cons=Cons,EURef=leave,asthma=asthma, bloodPressure=bp,depression=depression,
         diabetes=diabetes,vac_accessibility=WVacAcc
  ) %>%
  ungroup %>%   
  mutate(ID=row_number()) %>%
  tidyr::gather(Covariates,Value,1:14) %>%
  arrange(mean)

####-----High profiles
Data2high<-LH_msoaDatahigh %>%
  group_by(bmeG,imd_quintileNew,young,old,deathNew,urbanicity,
           LAB,Cons,leave,asthma, bp, diabetes,depression,WVacAcc) %>%
  summarise(mean=mean(pred),sd=sd(pred),
            q1=mean(low,0.025),
            q3=mean(high,0.975),
            n=n())%>%
  arrange(mean)%>% 
  rename(BME=bmeG,IMD=imd_quintileNew, Young= young, Mortality=deathNew
         ,OldAge=old,urbanicity=urbanicity,LAB=LAB,
         Cons=Cons,EURef=leave,asthma=asthma, bloodPressure=bp,depression=depression,
         diabetes=diabetes,vac_accessibility=WVacAcc
  ) %>%
  ungroup %>%   
  mutate(ID=row_number()) %>%
  tidyr::gather(Covariates,Value,1:14)%>% 
  arrange(mean)


library(patchwork)
library(forcats)
heatmap<- ggplot(Data2,aes(x=fct_inorder(Covariates),y=ID,fill=Value))+
  geom_tile()+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank()) +
  ylab(label = "MSOA") +
  xlab(label = "Profile")+
  scale_fill_gradient(low="#0073C2FF", high="#CD534CFF")+
  scale_x_discrete(
    label = c("BME", "IMD", "Young",
              "Old", "Mortality", "Urbanicity",
              "Labour", "Conservatives", "Brexit",
              "Asthma", "Bood pressure","Diabetes",
              "Depression", "Access"))
x11()
heatmap
ggsave("heatmap.tiff",width = 12,height = 8)

Prob_CI<-ggplot(Data2,aes(x=mean,y=ID)) +
  geom_linerange(aes(xmin=q1  , xmax=q3)) +
  geom_point(aes(x=mean,y=ID)) +
  ylab(label = "") + 
  scale_fill_gradient(low="#56B1F7", high="#132B43")+
  xlab(label = "Vaccine uptake") + 
  theme(legend.position="none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background =  element_blank())
Prob_CI

x11()
heatmap|Prob_CI

ggsave("Profile_heatMap.pdf",width = 12,height = 10)

###----Separating Profiles with low probabilities and ones with high probability

##----Low probabilities
heatmaplow<- ggplot(Data2low,aes(x=fct_inorder(Covariates),y=ID,fill=factor(Value)))+
  geom_tile()+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank()) +
  ylab(label = "Area") +
  xlab(label = "Profile")+
  #scale_fill_gradient(low="#868686FF" , high="#CD534CFF")+
  #scale_colour_gradientn(colours = terrain.colors(5))+
  #scale_fill_gradient(low = "white", high = "blue")+
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.2,end = 1)+
  scale_x_discrete(
    label = c("BME", "IMD", "Young",
              "Old", "Mortality", "Urbanicity",
              "Labour", "Conservatives", "Brexit",
              "Asthma", "Bood pressure","Diabetes",
              "Depression", "Access"))

x11()
heatmaplow

Prob_CIlow<-ggplot(Data2low,aes(x=mean,y=ID)) +
  geom_linerange(aes(xmin=q1  , xmax=q3)) +
  geom_point(aes(x=mean,y=ID)) +
  ylab(label = "") + 
  #scale_fill_gradient(low="#56B1F7", high="#132B43")+
  #scale_colour_gradientn(colours = terrain.colors(5))+
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.2,end = 1)+
  xlab(label = "Vaccine uptake") + 
  theme(legend.position="none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background =  element_blank())

x11()
Prob_CIlow

heatmaplow|Prob_CIlow
ggsave("heatMap_LowerProbN.jpeg",width = 10,height = 8)

#-----Higher probabilities

heatmaphigh<- ggplot(Data2high,aes(x=fct_inorder(Covariates),y=ID,fill=factor(Value)))+
  geom_tile()+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background =  element_blank()) +
  ylab(label = "Area") +
  xlab(label = "Profile")+
  #scale_fill_gradient(low="#56B1F7", high="#132B43")
  #scale_fill_gradient(low = "white", high = "black")+
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.2,end = 1)+
  scale_x_discrete(
    label = c("BME", "IMD", "Young",
              "Old", "Mortality", "Urbanicity",
              "Labour", "Conservatives", "Brexit",
              "Asthma", "Bood pressure","Diabetes",
              "Depression", "Access"))

x11()
heatmaphigh

Prob_CIhigh<-ggplot(Data2high,aes(x=mean,y=ID)) +
  geom_linerange(aes(xmin=q1, xmax=q3)) +
  geom_point(aes(x=mean,y=ID)) +
  ylab(label = "") + 
  scale_fill_gradient(low="#56B1F7", high="#132B43")+
  xlab(label = "Vaccine uptake") + 
  theme(legend.position="none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background =  element_blank())

x11()
Prob_CIhigh

heatmaphigh|Prob_CIhigh

ggsave("heatmap_HigherProbN.jpeg",width = 10,height = 8)
