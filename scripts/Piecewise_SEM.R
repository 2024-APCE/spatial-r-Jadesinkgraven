# Piecewise SEM
rm(list = ls()) # clear environment

library(piecewiseSEM)
#library for readcsv
library(readr)
#library for ggplot
library(ggplot2)

# read the pointdata
pointdata_init<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRq0HTbPC8h6MHiLJfmOUxRJmal8gH00_ftIfQjuKPdtzClFpDY4IlAM6EnZlHz4dgZZS3wsIQtnEIw/pub?gid=2033923084&single=true&output=csv")
pointdata <- pointdata_init |> # Remove rows with missing values
  na.omit() |>   # keep complete cases
  dplyr:: filter(woody>0, woody<20)   # remove 2 extreme values and avoid interpolated negative values

# note that you should not standardize your data for a PicewiseSEM as then eg logistic regression cannot be used

# Check for missing values
sum(is.na(pointdata))
colSums(is.na(pointdata))

  
psych::pairs.panels(pointdata,stars = T, ellipses = F)


# Define the models
# I started from this initially hypothesized causal scheme, my model 1)
browseURL("https://docs.google.com/presentation/d/1PB8rhbswyPew-FYULsw1pIl8Jyb1FFElKPf34DZrEY8/edit?usp=sharing")

# Model 1: woody predicted by burnfreq and rainfall
model_woody <- lm(woody ~  cec  + rainfall + dist2river + elevation, 
             data = pointdata)
summary(model_woody)
p1<-ggplot(data=pointdata,aes(x=burnfreq,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p1
p2<-ggplot(data=pointdata,aes(x=cec,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
#              method.args=list(family=Gamma(link="log")),
              formula= y~x,
              se=T) 
p2

p11 <- ggplot(data=pointdata,aes(x=dist2river,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p11

p12 <- ggplot(data=pointdata,aes(x=elevation,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p12



# it seems like the two variables are correlated


# Model_burnfreq: burning frequency predicted by Core Protected Areas and Rainfall
model_hills_init <- glm(hills ~  rainfall, 
              family=binomial, 
              data = pointdata)
# Calculate dispersion statistic
dispersion_stat <- summary(model_hills_init)$deviance / summary(model_hills_init)$df.residual
dispersion_stat
# If 𝜙≈1 : No evidence of overdispersion → Poisson is appropriate. (mean≈variance)
# If 𝜙>1 : Overdispersion is present → Consider quasi-Poisson or negative binomial.
# If 𝜙<1 : Underdispersion (less common) → Investigate the data further.
library(MASS)
model_hills <- MASS::glm.nb(hills ~ elevation, 
              data = pointdata)
summary(model_hills)

p3<-ggplot(data=pointdata,aes(y=hills,x=elevation))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),  # close to glm.nb
              formula= y~x,
              se=T)
p3
p4<-ggplot(data=pointdata,aes(y=burnfreq,x=rainfall))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),
              formula= y~x,
              se=T)
p4

# model_cec: predicted by rainfall

model_cec <- glm(cec ~ rainfall + burnfreq + elevation, 
                      data = pointdata)
summary(model_cec)

p5<-ggplot(data=pointdata,aes(y=cec,x=rainfall))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p5

p6<-ggplot(data=pointdata,aes(y=cec,x=burnfreq))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p6

p7 <- ggplot(data=pointdata,aes(y=cec,x=elevation))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p7



# model_rainfall: rainfall predicted by elevation
model_rainfall <- glm(rainfall ~ elevation, 
              data = pointdata)
summary(model_rainfall)

p8<-ggplot(data=pointdata,aes(y=rainfall,x=elevation))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p8

model_dist2river <- glm(dist2river ~ elevation , 
              data = pointdata)
summary(model_dist2river)

p9 <- ggplot(data=pointdata,aes(y=dist2river,x=elevation))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p9

p10 <- ggplot(data=pointdata,aes(y=dist2river,x=burnfreq))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p10

# combine the figures
library(patchwork)
allplots<-p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+p11+p12+
  patchwork::plot_layout(ncol=3) +
  patchwork::plot_annotation(title="Relations in model 1")
allplots

####### Combine all models into a single piecewise SEM
psem_model <- piecewiseSEM::psem(model_woody,
                                 model_hills,
                                 model_cec,
                                 model_rainfall,
                                 model_dist2river)

# Summarize the SEM results
summary(psem_model)

#make a scheme of the model
plot(psem_model)

#plot a readable scheme of the model






#i want woody at the right of the plot, and the plot to read from left to right
plot(psem_model, direction = "right")




lavaanPlot::lavaanPlot(psem_model, 
                       coefs = TRUE, 
                       stand = TRUE,
                       graph_options=list(rankdir="LR"),
                       stars="regress")



       
library(lavaanPlot)

# plot the model using lavaan plot
lavaanPlot::lavaanPlot(psem_model, 
                       coefs = TRUE, 
                       stand = TRUE,
                       graph_options=list(rankdir="LR"),
                       stars="regress")




# a Significant (P<0.05) global goodness of fit means that your model does not fit well, 
# indicating potential problems like missing paths, mis-specfied relations, 
# or unaccounted-for correlations

# update the model based on your results
# significant tests of directed separation could mean a missing direct effect between the variables

# Best Practices:
# - Hypothesize Carefully:
#   Construct the initial model based on theoretical or empirical understanding.
# - Evaluate d-Separation Results:
#   Add or adjust paths based on significant independence test results.
# - Refit and Validate:
#   Rerun the model after adjustments and recheck the Fisher’s C statistic and independence claims.
# - Avoid Overfitting:
#   Add paths only when justified by theory or strong evidence, not purely to improve fit.
# Common pitfall: 
# - ignofing significant d-separation tests and failing to modify the model
# - adding too many variables and pathways, leading to overfitting and loss of parsimony

