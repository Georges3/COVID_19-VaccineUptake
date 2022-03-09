
##---Script 4:
################################################################################
## Title: Community characteristics of COVID-19 vaccine hesitancy in England: ## 
##                  A nationwide cross-sectional study                        ##
##                                                                            ##
## Authors: Bucyibaruta, G. - Blangiardo, M. -  Konstantinoudis, G.           ##
##                                                                            ##
##                                                                            ##
##                                                                            ##
################################################################################

###########################################################################
############----Fitting Univariate (unadjusted) model-------##############
##########################################################################

#-------------------Univariate model--------------------------#
# With spatial
##---mbe------
formulaNewFirstGbme <- Cum_firstDose~ 1+bmeG+
  f(reid,model="bym2",graph = g,scale.model =TRUE )


vac_ful3New2FirstGbme = inla(formulaNewFirstGbme, family = "binomial", data = vac_poly_Category, Ntrials=PopVac,
                             control.predictor = list(compute=TRUE),
                             control.compute=list(dic=TRUE,cpo = TRUE,waic = TRUE,config = TRUE))

df_bme <-data.frame(
  M= exp(vac_ful3New2FirstGbme$summary.fixed$`0.5quant`)[-1],
  Lower = exp(vac_ful3New2FirstGbme$summary.fixed$`0.025quant`)[-1],
  Upper= exp(vac_ful3New2FirstGbme$summary.fixed$`0.975quant`)[-1]
)

#----------------------------
# imd
formulaNewFirstGimd <- Cum_firstDose~ 1+imd_quintileNew+
  f(reid,model="bym2",graph = g,scale.model =TRUE )

vac_ful3New2FirstGimd = inla(formulaNewFirstGimd, family = "binomial", data = vac_poly_Category, Ntrials=PopVac,
                             control.predictor = list(compute=TRUE),
                             control.compute=list(dic=TRUE,cpo = TRUE,waic = TRUE,config = TRUE))

df_imd <-data.frame(
  M= exp(vac_ful3New2FirstGimd$summary.fixed$`0.5quant`)[-1],
  Lower = exp(vac_ful3New2FirstGimd$summary.fixed$`0.025quant`)[-1],
  Upper= exp(vac_ful3New2FirstGimd$summary.fixed$`0.975quant`)[-1]
)
#------------------------
# leave
formulaNewFirstGleave <- Cum_firstDose~ 1+leave+
  f(reid,model="bym2",graph = g,scale.model =TRUE )


vac_ful3New2FirstGleave = inla(formulaNewFirstGleave, family = "binomial", data = vac_poly_Category, Ntrials=PopVac,
                               control.predictor = list(compute=TRUE),
                               control.compute=list(dic=TRUE,cpo = TRUE,waic = TRUE,config = TRUE))

df_leave <-data.frame(
  M= exp(vac_ful3New2FirstGleave$summary.fixed$`0.5quant`)[-1],
  Lower = exp(vac_ful3New2FirstGleave$summary.fixed$`0.025quant`)[-1],
  Upper= exp(vac_ful3New2FirstGleave$summary.fixed$`0.975quant`)[-1]
)
#--------------------------------------------
# asthma
formulaNewFirstGasthma <- Cum_firstDose~ 1+asthma+
  f(reid,model="bym2",graph = g,scale.model =TRUE )


vac_ful3New2FirstGasthma = inla(formulaNewFirstGasthma, family = "binomial", data = vac_poly_Category, Ntrials=PopVac,
                                control.predictor = list(compute=TRUE),
                                control.compute=list(dic=TRUE,cpo = TRUE,waic = TRUE,config = TRUE))

df_asthma <-data.frame(
  M= exp(vac_ful3New2FirstGasthma$summary.fixed$`0.5quant`)[-1],
  Lower = exp(vac_ful3New2FirstGasthma$summary.fixed$`0.025quant`)[-1],
  Upper= exp(vac_ful3New2FirstGasthma$summary.fixed$`0.975quant`)[-1]
)

#----------------------------------
# diabetes
formulaNewFirstGdiabetes <- Cum_firstDose~ 1+diabetes+
  f(reid,model="bym2",graph = g,scale.model =TRUE )


vac_ful3New2FirstGdiabetes = inla(formulaNewFirstGdiabetes, family = "binomial", data = vac_poly_Category, Ntrials=PopVac,
                                  control.predictor = list(compute=TRUE),
                                  control.compute=list(dic=TRUE,cpo = TRUE,waic = TRUE,config = TRUE))

df_diabetes <-data.frame(
  M= exp(vac_ful3New2FirstGdiabetes$summary.fixed$`0.5quant`)[-1],
  Lower = exp(vac_ful3New2FirstGdiabetes$summary.fixed$`0.025quant`)[-1],
  Upper= exp(vac_ful3New2FirstGdiabetes$summary.fixed$`0.975quant`)[-1]
)

#--------------------------------------
# bp
formulaNewFirstGbp <- Cum_firstDose~ 1+bp+
  f(reid,model="bym2",graph = g,scale.model =TRUE )


vac_ful3New2FirstGbp = inla(formulaNewFirstGbp, family = "binomial", data = vac_poly_Category, Ntrials=PopVac,
                            control.predictor = list(compute=TRUE),
                            control.compute=list(dic=TRUE,cpo = TRUE,waic = TRUE,config = TRUE))

df_bp <-data.frame(
  M= exp(vac_ful3New2FirstGbp$summary.fixed$`0.5quant`)[-1],
  Lower = exp(vac_ful3New2FirstGbp$summary.fixed$`0.025quant`)[-1],
  Upper= exp(vac_ful3New2FirstGbp$summary.fixed$`0.975quant`)[-1]
)

#-------------------------------------------
# depression
formulaNewFirstGdepression <- Cum_firstDose~ 1+depression+
  f(reid,model="bym2",graph = g,scale.model =TRUE )


vac_ful3New2FirstGdepression = inla(formulaNewFirstGdepression, family = "binomial", data = vac_poly_Category, Ntrials=PopVac,
                                    control.predictor = list(compute=TRUE),
                                    control.compute=list(dic=TRUE,cpo = TRUE,waic = TRUE,config = TRUE))

df_depression <-data.frame(
  M= exp(vac_ful3New2FirstGdepression$summary.fixed$`0.5quant`)[-1],
  Lower = exp(vac_ful3New2FirstGdepression$summary.fixed$`0.025quant`)[-1],
  Upper= exp(vac_ful3New2FirstGdepression$summary.fixed$`0.975quant`)[-1]
)
#-----------------------------------------
# Cons
formulaNewFirstGcons <- Cum_firstDose~ 1+Cons+
  f(reid,model="bym2",graph = g,scale.model =TRUE )


vac_ful3New2FirstGcons = inla(formulaNewFirstGcons, family = "binomial", data = vac_poly_Category, Ntrials=PopVac,
                              control.predictor = list(compute=TRUE),
                              control.compute=list(dic=TRUE,cpo = TRUE,waic = TRUE,config = TRUE))

df_cons <-data.frame(
  M= exp(vac_ful3New2FirstGcons$summary.fixed$`0.5quant`)[-1],
  Lower = exp(vac_ful3New2FirstGcons$summary.fixed$`0.025quant`)[-1],
  Upper= exp(vac_ful3New2FirstGcons$summary.fixed$`0.975quant`)[-1]
)

#--------------------------------------
# LAB
formulaNewFirstGlab <- Cum_firstDose~ 1+LAB+
  f(reid,model="bym2",graph = g,scale.model =TRUE )


vac_ful3New2FirstGlab = inla(formulaNewFirstGlab, family = "binomial", data = vac_poly_Category, Ntrials=PopVac,
                             control.predictor = list(compute=TRUE),
                             control.compute=list(dic=TRUE,cpo = TRUE,waic = TRUE,config = TRUE))

df_lab <-data.frame(
  M= exp(vac_ful3New2FirstGlab$summary.fixed$`0.5quant`)[-1],
  Lower = exp(vac_ful3New2FirstGlab$summary.fixed$`0.025quant`)[-1],
  Upper= exp(vac_ful3New2FirstGlab$summary.fixed$`0.975quant`)[-1]
)
#---------------------------------------
# death
formulaNewFirstGdeath <- Cum_firstDose~ 1+death+
  f(reid,model="bym2",graph = g,scale.model =TRUE )


vac_ful3New2FirstGdeath = inla(formulaNewFirstGdeath, family = "binomial", data = vac_poly_Category, Ntrials=PopVac,
                               control.predictor = list(compute=TRUE),
                               control.compute=list(dic=TRUE,cpo = TRUE,waic = TRUE,config = TRUE))

df_death <-data.frame(
  M= exp(vac_ful3New2FirstGdeath$summary.fixed$`0.5quant`)[-1],
  Lower = exp(vac_ful3New2FirstGdeath$summary.fixed$`0.025quant`)[-1],
  Upper= exp(vac_ful3New2FirstGdeath$summary.fixed$`0.975quant`)[-1]
)

#-------------------------------------------
# young
formulaNewFirstGyoung <- Cum_firstDose~ 1+young+
  f(reid,model="bym2",graph = g,scale.model =TRUE )


vac_ful3New2FirstGyoung = inla(formulaNewFirstGyoung, family = "binomial", data = vac_poly_Category, Ntrials=PopVac,
                               control.predictor = list(compute=TRUE),
                               control.compute=list(dic=TRUE,cpo = TRUE,waic = TRUE,config = TRUE))

df_young <-data.frame(
  M= exp(vac_ful3New2FirstGyoung$summary.fixed$`0.5quant`)[-1],
  Lower = exp(vac_ful3New2FirstGyoung$summary.fixed$`0.025quant`)[-1],
  Upper= exp(vac_ful3New2FirstGyoung$summary.fixed$`0.975quant`)[-1]
)
#----------------------------------------
# urbanicity
formulaNewFirstGur <- Cum_firstDose~ 1+urbanicity+
  f(reid,model="bym2",graph = g,scale.model =TRUE )


vac_ful3New2FirstGur = inla(formulaNewFirstGur, family = "binomial", data = vac_poly_Category, Ntrials=PopVac,
                            control.predictor = list(compute=TRUE),
                            control.compute=list(dic=TRUE,cpo = TRUE,waic = TRUE,config = TRUE))

df_ur <-data.frame(
  M= exp(vac_ful3New2FirstGur$summary.fixed$`0.5quant`)[-1],
  Lower = exp(vac_ful3New2FirstGur$summary.fixed$`0.025quant`)[-1],
  Upper= exp(vac_ful3New2FirstGur$summary.fixed$`0.975quant`)[-1]
)
#---------------------------
# old
formulaNewFirstGold <- Cum_firstDose~ 1+old+
  f(reid,model="bym2",graph = g,scale.model =TRUE )


vac_ful3New2FirstGold = inla(formulaNewFirstGold, family = "binomial", data = vac_poly_Category, Ntrials=PopVac,
                             control.predictor = list(compute=TRUE),
                             control.compute=list(dic=TRUE,cpo = TRUE,waic = TRUE,config = TRUE))

df_old <-data.frame(
  M= exp(vac_ful3New2FirstGold$summary.fixed$`0.5quant`)[-1],
  Lower = exp(vac_ful3New2FirstGold$summary.fixed$`0.025quant`)[-1],
  Upper= exp(vac_ful3New2FirstGold$summary.fixed$`0.975quant`)[-1]
)
#-----------------------
## WVA

formulaNewFirstGWVA <- Cum_firstDose~ 1+WVA+
  f(reid,model="bym2",graph = g,scale.model =TRUE )


vac_ful3New2FirstGWVA = inla(formulaNewFirstGWVA, family = "binomial", data = vac_poly_Category, Ntrials=PopVac,
                             control.predictor = list(compute=TRUE),
                             control.compute=list(dic=TRUE,cpo = TRUE,waic = TRUE,config = TRUE))

df_WVA <-data.frame(
  M= exp(vac_ful3New2FirstGWVA$summary.fixed$`0.5quant`)[-1],
  Lower = exp(vac_ful3New2FirstGWVA$summary.fixed$`0.025quant`)[-1],
  Upper= exp(vac_ful3New2FirstGWVA$summary.fixed$`0.975quant`)[-1]
)


df_univ <- rbind(df_bme,df_imd,df_young,df_old,df_ur,df_lab,df_cons,
                 df_leave,df_death,df_asthma,df_bp,df_diabetes,
                 df_depression,df_WVA)

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

Mu <- c()
Mu[A]<- N
Mu[B] <- df_univ$M

Lu <- c()
Lu[A]<- N
Lu[B] <- df_univ$Lower

Uu <- c()
Uu[A]<- N
Uu[B] <- df_univ$Upper

df_univNew <- data.frame(index=c(1:23, 25:39, 41:65, 67:71),variable=New,M=Mu,Lower=Lu,Upper=Uu)



df_univNew$class <- ifelse(df_univNew$index <24, "Socio_demographic and urbanicity",
                           ifelse(df_univNew$index >= 24 & df_univNew$index <= 40, "Political_belief",
                                  ifelse(df_univNew$index > 40 & df_univNew$index <= 65,
                                         "COVID-19 awareness and targeting of high risk groups",
                                         "Vaccine_Accessibility")))


df_univNew$class <- factor(df_univNew$class,levels = c("Socio_demographic and urbanicity","Political_belief",
                                                       "COVID-19 awareness and targeting of high risk groups",
                                                       "Vaccine_Accessibility" ))

breaks= seq(0.3,2.8,by=0.1)

labels = as.character(breaks)

pmu <- ggplot(data=df_univNew)+
  geom_vline(xintercept=1, linetype="dashed") +
  geom_errorbar(aes(y=index, x=M, xmin=Lower, xmax=Upper))+
  geom_point(aes(x=M,y=index))+
  #adds the CIs
  scale_y_continuous(name = "", breaks=c(1:23, 25:39, 41:65, 67:71),
                     labels = df_univNew$variable, trans="reverse",
                     expand = c(0, 0)) +
  ggtitle("")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_markdown(size = rel(1.2)),
    axis.title.y = element_text(size = rel(2) ),
    panel.spacing.y  = unit(2, "lines"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(0,1, 0, 1), "cm")
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
           x = rep(-0.09, lenght.out = 14),angle = 0,
           y=c(3,8,13,18,22,27,32,37,43,48,53,58,63,69),
           label = c("BME", "IMD", "Young",
                     "Old", "Urbanicity",
                     "Labour", "Conservatives", "Brexit", "Mortality",
                     "Asthma", "Bood pressure","Diabetes",
                     "Depression", "Access")) +
  annotate("text",
           x = rep(2.99, lenght.out =4 ),angle = 3*90,
           y = c(10, 33, 53, 69),
           label = c("Socio-demographics\n and urbanicity",
                     "Political opinions",
                     "COVID-19 awareness and \n targeting of high risk groups",
                     "Access"), fontface = "bold", size =3.5
           #,colour=c( "#CC79A7","#009E73","#0072B2","#D55E00")
  ) +
  geom_vline(xintercept=1, linetype="dashed",col="red") +
  coord_cartesian(xlim = c(0.28, 2.8), clip = "off")

x11()
pmu

ggsave("CovariatesEstimatesUni3.pdf", width = 11, height = 14)


##--------------End--------------------------------------------#
