
##---Script 2:
################################################################################
## Title: Community characteristics of COVID-19 vaccine hesitancy in England: ## 
##                  A nationwide cross-sectional study                        ##
##                                                                            ##
## Authors: Bucyibaruta, G. - Blangiardo, M. -  Konstantinoudis, G.           ##
##                                                                            ##
##                                                                            ##
##                                                                            ##
################################################################################


#########################################################################
###-------------Fitting model in INLA--------------------------#########
########################################################################

##----This script covers:

    ##--------Fitting model in INLA---------##
#        * Model parameter estimates
#        * Odds ratio
#        * Plots of Posterior median and Relative residuals


vac_poly_Category$reid <- 1:nrow(vac_poly_Category)

nb <- poly2nb(vac_poly_Category)

nb2INLA("map_adj",nb)

# Graph
g <- inla.read.graph(filename="map_adj")

# With spatial (all categorical)
formulaNewFirstG <- Cum_firstDose~ 1+bmeG+imd_quintileNew+young+old+
  urbanicity +LAB+Cons+leave+death+
  asthma+bp+diabetes+depression+WVA+
  f(reid,model="bym2",graph = g,scale.model =TRUE )


vac_ful3New2FirstG = inla(formulaNewFirstG, family = "binomial", 
                          data = vac_poly_Category, Ntrials=PopVac,
                          control.predictor = list(compute=TRUE),
                          control.compute=list(dic=TRUE,cpo = TRUE,waic = TRUE,config = TRUE))

summary(vac_ful3New2FirstG)

##-----------Extract Odd Ratios ------------------------##

tab_spatial <- exp(vac_ful3New2FirstG$summary.fixed[
  c("bmeG2","bmeG3","bmeG4","bme5",
    "imd_quintileNew2","imd_quintileNe3","imd_quintileNew4","imd_quintileNew5",
    "young2","young3","young4","young5",
    "old2","old3","old4","old5",
    "death2","death3","death4","death5",
    "urbanicityPR","urbanicityUR",
    "LAB2","LAB3","LAB4","LAB5",
    "Cons2","Cons3","Cons4","Cons5",
    "leave2","leave3","leave4","leave5",
    "asthma2","asthma3","asthma4","asthma5",
    "bp2","bp3","bp4","bp5",
    "diabetes2","diabetes3","diabetes4","diabetes5",
    "depression2","depression3","depression4","depression5",
    "WVA2","WVA3","WVA4", "WVA5"
  ),
  c("0.5quant", "0.025quant", "0.975quant")])
tab_spatial <- round(tab_spatial, digits = 3)

##----------------------------------------------------------------------

########################################################################
###-------Customizing Caterpillar plots for OR-----------------#########
########################################################################


New= c("1 (low)","2","3","4","5",
       "1 (least)","2","3","4","5",
       "1 (low)","2","3","4","5",
       "1 (low)","2","3","4","5",
       "Urban","Rural","Semi",
       "1 (low)","2","3","4","5",
       "1 (low)","2","3","4","5",
       "1 (low)","2","3","4","5",
       "1 (low)","2","3","4","5",
       "1 (low)","2","3","4","5",
       "1 (low)","2","3","4","5",
       "1 (low)","2","3","4","5",
       "1 (low)","2","3","4","5",
       "1 (low)","2","3","4","5"
)
A <- c(1,6,11,16,21,24,29,34,39,44,49,54,59,64)
B <- c(2:5,7:10,12:15,17:20,22:23,25:28,30:33,35:38,40:43,45:48,50:53,55:58,60:63,65:68)
N<- rep(1,14)
M= exp(vac_ful3New2FirstG$summary.fixed$`0.5quant`)[-1]
Lower = exp(vac_ful3New2FirstG$summary.fixed$`0.025quant`)[-1]
Upper= exp(vac_ful3New2FirstG$summary.fixed$`0.975quant`)[-1]
MN <- c()
MN[A]=N
MN[B]=M
LN <- c()
LN[A]=N
LN[B]=Lower
UN <- c()
UN[A]=N
UN[B]=Upper
df <- data.frame(index=c(1:23, 25:39, 41:65, 67:71),variable=New,Mean=MN,l=LN,u=UN)
length(c(1:28, 30:45, 47:67, 69:76))
df$class <- ifelse(df$index <24, "Socio_demographic and urbanicity",
                   ifelse(df$index >= 24 & df$index <= 40, "Political_belief",
                          ifelse(df$index > 40 & df$index <= 65,
                                 "COVID-19 awareness and targeting of high risk groups",
                                 "Vaccine_Accessibility")))


df$class <- factor(df$class,levels = c("Socio_demographic and urbanicity","Political_belief",
                                       "COVID-19 awareness and targeting of high risk groups",
                                       "Vaccine_Accessibility" ))

breaks = c(0.5,0.6,0.7,0.8,0.9,1,1.1, 1.2,1.3,1.4, 1.5)
labels = as.character(breaks)


pm <- ggplot(data=df)+
  geom_vline(xintercept=1, linetype="dashed") +
  geom_errorbar(aes(y=index, x=Mean, xmin=l, xmax=u))+
  geom_point(aes(x=Mean,y=index))+
  scale_y_continuous(name = "", breaks=c(1:23, 25:39, 41:65, 67:71),
                     labels = df$variable, trans="reverse",
                     expand = c(0, 0)) +
  ggtitle("")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_markdown(size = rel(1.2)),
    axis.title.y = element_text(size = rel(1.8) ),
    panel.spacing.y  = unit(2, "lines"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(0, 1, 0, 1), "cm")
  ) +
  xlab("Odds ratio")  +
  geom_hline(yintercept = c(5.5, 10.5, 15.5, 20.5, 24,
                            29.5, 34.5, 40, 45.5,
                            50.5, 55.5, 60.5, 66), col = "grey85")+
  geom_hline(yintercept = c(24, 40, 66), col = "black") +
  geom_vline(xintercept = breaks,
             col = "grey85",linetype="dashed") +
  scale_x_continuous(labels = labels,breaks = breaks)+
  annotate("text",
           x = rep(0.339, lenght.out = 14),angle = 0,
           y=c(3,8,13,18,22,27,32,37,43,48,53,58,63,69),
           label = c("BME", "IMD", "Young",
                     "Old", "Urbanicity",
                     "Labour", "Conservatives", "Brexit", "Mortality",
                     "Asthma", "Bood pressure","Diabetes",
                     "Depression", "Access")) +
  annotate("text",
           x = rep(1.60, lenght.out =3 ),angle = 3*90,
           y = c(10, 33, 53, 69),
           label = c("Socio-demographics\n and urbanicity",
                     "Political opinions",
                     "COVID-19 awareness and \n targeting of high risk groups",
                     "Access"), fontface = "bold", size =4) +
  geom_vline(xintercept=1, linetype="dashed",col="red") +
  coord_cartesian(xlim = c(0.5, 1.52), clip = "off")
x11()
pm

ggsave("CovariatesEstimatesNew5.pdf", width = 10, height = 14)

#########################################################################
###----Mapping posterior median and spatial residuals (relative risk)--##
########################################################################

pred <-vac_ful3New2FirstG$summary.fitted.values$`0.5quant`
predrnk <- rank(pred)
low <- vac_ful3New2FirstG$summary.fitted.values$`0.025quant`
high <- vac_ful3New2FirstG$summary.fitted.values$`0.975quant`

vac_poly_Category$predictor<- pred*100

vac_poly_Category$pred_low<- low*100
vac_poly_Category$pred_upper<- high*100
vac_poly_Category$group <- cut2(vac_poly_Category$predictor,g=5)

Q_prob <- quantile(vac_poly_Category$predictor,probs = c(0.2,0.4,0.6,0.8))

######-----vaccine Uptake and large cities--------------------##########
cities <- read_sf("Major_Towns_and_Cities_(December_2015)_Boundaries_V2.shp")
cities <- st_transform(cities, 4326)

London <- cities[cities$TCITY15NM %in% "London",]           
Birmingham <- cities[cities$TCITY15NM %in% "Birmingham",]   
Liverpool <- cities[cities$TCITY15NM %in% "Liverpool",]    
Bristol <- cities[cities$TCITY15NM %in% "Bristol",]   

################################################################
######------------Posterior Median---------------------##########
###############################################################

Bris <- st_intersection(vac_poly_Category, cities[cities$TCITY15NM %in% "Bristol",]) %>% 
  ggplot() + 
  geom_sf(aes(fill = group), col = NA, show.legend = F) +
  geom_sf(data = Bristol, fill = NA,col=NA) +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.2,end=1) +
  theme_void() +
  theme(legend.title =  element_blank(),
        plot.title = element_text(hjust = 0.5,face = "bold"))+
  ggtitle("Bristol")

L <- st_intersection(vac_poly_Category, cities[cities$TCITY15NM %in% "London",]) %>% 
  ggplot() + 
  geom_sf(aes(fill = group), col = NA, show.legend = F) +
  geom_sf(data = London, fill = NA,col=NA) +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.2,end=1) +
  theme_void() +
  theme(legend.title =  element_blank(),
        plot.title = element_text(hjust = 0.5,face = "bold"))+
  ggtitle("London")

Bir <- st_intersection(vac_poly_Category, cities[cities$TCITY15NM %in% "Birmingham",]) %>% 
  ggplot() + 
  geom_sf(aes(fill = group), col = NA, show.legend = F) +
  geom_sf(data = Birmingham, fill = NA,col=NA) +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.2,end=1) +
  theme_void() +
  theme(legend.title =  element_blank(),
        plot.title = element_text(hjust = 0.5,face = "bold"))+
  ggtitle("Birmingham")

Liv <- st_intersection(vac_poly_Category, cities[cities$TCITY15NM %in% "Liverpool",]) %>% 
  ggplot() + 
  geom_sf(aes(fill = group), col = NA, show.legend = F) +
  geom_sf(data = Liverpool, fill = NA,col=NA) +
  scale_fill_viridis_d(option="viridis",direction = -1,begin = 0.2,end = 1) +
  theme_void() +
  theme(legend.title =  element_blank(),
        plot.title = element_text(hjust = 0.5,face = "bold"))+
  ggtitle("Liverpool")

vac_poly_Category %>% 
  ggplot() + 
  geom_sf(aes(fill = group), color = NA, show.legend = TRUE) +
  geom_sf(data = London, colour = "red", fill = NA, size = .1) + 
  geom_sf(data = Birmingham, colour = "red", fill = NA, size = .1) + 
  geom_sf(data = Liverpool, colour = "red", fill = NA, size = .1) + 
  geom_sf(data = Bristol, colour = "red", fill = NA, size = .1) + 
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.2,end=1)+
  theme_void() +
  theme(legend.position = c(0.92, 0.85),
        legend.title =  element_blank(),
        legend.text = element_text(size=14),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0) # Left margin
        ,plot.title = element_text(hjust = 0.5,face = "bold"))+
  ggtitle("England") -> a

p_all=a|((L|Bir)/(Liv|Bris))
p_all +plot_layout(guides = "collect")+
  plot_annotation(title = "")

ggsave("VaccUptake_CitiesViridisN.pdf",width=10,height =8)

################################################################
######------------Relative ratio---------------------##########
###############################################################
ff = function(x){exp(x)}
vac_poly_Category$bym2RR <- round(ff(vac_ful3New2FirstG$summary.random$reid$'0.5quant'[1:6791]),2)
vac_poly_Category$bym2GR <-cut2(vac_poly_Category$bym2RR,g=6)


L_rr <- st_intersection(vac_poly_Category, cities[cities$TCITY15NM %in% "London",]) %>% 
  ggplot() + 
  geom_sf(aes(fill = bym2GR), col = NA, show.legend = F) +
  geom_sf(data = London, fill = NA,col=NA) +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.2,end = 1)+
  theme_void() +
  theme(legend.title =  element_blank(),
        plot.title = element_text(hjust = 0.5,face = "bold"))+
  ggtitle("London")
L_rr

Bir_rr <- st_intersection(vac_poly_Category, cities[cities$TCITY15NM %in% "Birmingham",]) %>% 
  ggplot() + 
  geom_sf(aes(fill = bym2GR), col = NA, show.legend = F) +
  geom_sf(data = Birmingham, fill = NA,col=NA) +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.2,end = 1)+
  theme_void() +
  theme(legend.title =  element_blank(),
        plot.title = element_text(hjust = 0.5,face = "bold"))+
  ggtitle("Birmingham")
Bir_rr


Liv_rr <- st_intersection(vac_poly_Category, cities[cities$TCITY15NM %in% "Liverpool",]) %>% 
  ggplot() + 
  geom_sf(aes(fill = bym2GR), col = NA, show.legend = F) +
  geom_sf(data = Liverpool, fill = NA,col=NA) +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.2,end = 1)+
  theme_void() +
  theme(legend.title =  element_blank(),
        plot.title = element_text(hjust = 0.5,face = "bold"))+
  ggtitle("Liverpool")
Liv_rr

Bris_rr <- st_intersection(vac_poly_Category, cities[cities$TCITY15NM %in% "Bristol",]) %>% 
  ggplot() + 
  geom_sf(aes(fill = bym2GR), col = NA, show.legend = F) +
  geom_sf(data = Bristol, fill = NA,col=NA) +
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.2,end = 1)+
  theme_void() +
  theme(legend.title =  element_blank(),
        plot.title = element_text(hjust = 0.5,face = "bold"))+
  ggtitle("Bristol")
Bris_rr

vac_poly_Category %>% 
  ggplot() + 
  geom_sf(aes(fill = bym2GR), color = NA, show.legend = TRUE) +
  geom_sf(data = London, colour = "red", fill = NA, size = .1) + 
  geom_sf(data = Birmingham, colour = "red", fill = NA, size = .1) + 
  geom_sf(data = Liverpool, colour = "red", fill = NA, size = .1) + 
  geom_sf(data = Bristol, colour = "red", fill = NA, size = .1) + 
  scale_fill_viridis_d(option = "viridis",direction = -1,begin = 0.2,end = 1)+
  theme_void() +
  theme(legend.position = c(0.92, 0.85),
        legend.title =  element_blank(),
        legend.text = element_text(size=14),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0) # Left margin
        ,plot.title = element_text(hjust = 0.5,face = "bold"))+
  ggtitle("England") -> a_rr
a_rr


p_all_rr=a_rr|((L_rr|Bir_rr)/(Liv_rr|Bris_rr))

x11()

p_all_rr +plot_layout(guides = "collect")+
  plot_annotation(title = "")

ggsave("RelativeRatio_CitiesVirN.pdf",width=10,height = 8)
