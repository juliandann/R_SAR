library(mgcv)
library(SemiPar)
library(nlme)
library("ggpubr")
library(spm)

df <- read.csv('Z:/AKSeward/2017_SAR/ABoVE_Soil_Moisture_Products/JBD_Products/above_final_wout_water_100000.csv', header = TRUE)
df_large <- read.csv('Z:/AKSeward/2017_SAR/ABoVE_Soil_Moisture_Products/JBD_Products/above_final_wout_water.csv', header = TRUE)
df$NLCD <- as.factor(df$NLCD)
df$gaplandfire_val <- as.factor(df$gaplandfire_val)

df_train <- df[c('NLCD','slope','curvature','aspect','prox_water','lbc_0.2_aug')]
df_test <- df_large[c('NLCD','slope','curvature','aspect','prox_water','lbc_0.2_aug')]


attach(df)
# Fit a GAM =
gam_fit <- gam(lbc_0.06_aug ~ s(lon,lat,k=200)+s(slope,k=15)+ NLCD+ gaplandfire_val+ s(aspect,k=12)+s(fa,k=12)+s(NDVI_30m,k=12)+s(prox_water), data = df,method = "REML")
plot(gam_fit, all.terms = TRUE,pages = 1,residuals=TRUE,pch=1,cex=1,shade=TRUE,seWithMean=TRUE,shift= coef(gam_fit)[1])
summary(gam_fit)

#factor smoothing for categorical variables
gam_fit_factor <- gam(lbc_0.06_aug ~ s(slope,NLCD,bs="fs",k=15), data = df,method = "REML")

#multiscale factors with tensors
gam_fit_2 <- gam(lbc_0.2_aug ~ s(slope)+NLCD+s(prox_water)+s(curvature)+s(NDVI_30m)+s(fa)+s(aspect),data=df,method="REML")
gam_fit_3 <- gam(lbc_0.2_aug ~ s(aspect,k=8)+s(slope,k=9)+gaplandfire_val+s(fa,k=6)+s(prox_water,k=7)+NLCD+s(curvature,k=7)+s(NDVI_30m,k=8),data=df,method="REML")
#gam_fit_tensor <- gam(lbc_0.2_aug ~ s(lon,lat)+s(slope)+NLCD+s(aspect)+s(fa)+s(NDVI_30m)+s(prox_water)+ti(lat,lon,slope,aspect,fa,NDVI_30m,prox_water),data=df,method="REML")
#gam_fit_tensor <- gam(lbc_0.2_aug ~ s(lon,lat)+s(slope)+NLCD+s(aspect)+s(fa)+s(NDVI_30m)+s(prox_water)+ti(lat,lon,slope,aspect),data=df,method="REML")

s(curvature,k=7)
s(prox_water,k=7)
s(NDVI_30m,k=8)
s(fa,k=6)
s(slope,k=9)
s(aspect,k=8)
s(lon,lat,k=50)

#checking the gam
gam.check(gam_fit)


#perform an anova for model comparison
anova(gam_fit_2,gam_fit_3,test="Chisq")

#checking for concurvity if worst case is high (>0.80) then do next step
concurvity(gam_fit,full=TRUE)

#detailed  tells me that slope and elevation are highly correlated ~0.51 so I will keep only slope in the model
concurvity(gam_fit,full=FALSE)

#SemiPar Attempts
attach(df_train)
fit <-spm(lbc_0.2_aug~f(slope))
plot(fit)
summary(fit)

#testing correlations between variables
x = aspect
y = NDVI_30m
x_name = 'aspect'
y_name = 'NDVI_30m'

cor.test(aspect, NDVI_30m, method=c("pearson", "kendall", "spearman"))

#visualize correlation
par(mfrow=c(1,1))
ggscatter(df, x = x_name, y = y_name, 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = x_name, ylab = y_name)

#Multiple variables

fit <- spm(lbc_0.2_aug ~ f(slope)+
             f(curvature,k=10) +
             f(aspect )+
             f(NDVI_30m) +
             f(ifsar_5m) +
             f(fa) +
             NLCD +
             gaplandfire_val+
             f(prox_water),omit.missing=TRUE)
fit <- spm(lbc_0.2_aug ~ f(slope)+
             f(curvature)+
             f(aspect)+
             f(prox_water)) 
par(mfrow=c(3,2))

plot(fit)
points(slope,lbc_0.2_aug)
summary(fit)

attach(df_test)

#predict that shit!
preds <- predict(fit,df_test[c('slope','curvature','aspect','prox_water')],se=TRUE)

#averaged variable importance based on random forest
train_x <- df_train[c('slope','curvature','aspect','prox_water')]
train_y <-df_train['lbc_0.2_aug'] 
avi_res <-avi(train_x,unlist(train_y))


