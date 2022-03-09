# COVID_19-VaccineUptake
To ensure reproducibility of our results, the code  and data used in analysis are available here

All data and R-scripts used in our analysis were provided. To facilitate the reproducibility of our results, the  R-script folder can be used in the following order:
 * 1) R_code_Data_exploratory_analysis for 
   ##----Loading required data (cleaned data)-----##
   
#        Shapefile for MSOAs in England
#       * Vaccine uptake  data at MSOA level
#       * Covariates in orginal scale
#       * vaccine access data
#       * Spatial distribution of Vaccination sites

    ##-----Data exploration----------##

#       * Visualizing spatial distribution of covariates
#       * Assessing Correlation between covariates

 2) R_code_multivariate model for

     ##--------Fitting model in INLA---------##
#        * Model parameter estimates
#        * Odds ratio
#        * Plots of Posterior median and Relative residuals

3) R_code_Covariates_ profiles for

#----Heatmap of profiles and Poesterior probabilities

4) R_code_univariate_ model for 

# -----Fitting Univariate (unadjusted) model
