# sjSDM::install_sjSDM()
# install_sjSDM(version = c("gpu"))
# vignette("Dependencies", package = "sjSDM")
# sjSDM::install_diagnostic()
library(dplyr)
library(sjSDM)

# reticulate::conda_remove('r-reticulate')
# reticulate::conda_remove('r-sjsdm')
# reticulate::conda_create('r-sjsdm', python_version='3.9')
# reticulate::conda_install('r-sjsdm',packages = c("pytorch", "torchvision" ,"torchaudio", "cudatoolkit=11.3", "-c", "pytorch"))
# reticulate::conda_install('r-sjsdm',packages = c("pyro-ppl", "torch_optimizer", "madgrad"), pip=TRUE)

##############################################################
##### Import Data

# 1 - Survey Data - Counts & Environment
df = read.csv("Data/XY_No_Satellite_Gola.csv", row.names = 1)
names(df)

# Create Count Data
y = df[,c(19:ncol(df))]
y[y>0]=1
table(colSums(y))
table(rowSums(y))

# As per Pichler 2021 - Remove Sites with fewer than 4 OTU's
# Remove OTU's with fewer than 3 occurrences
df1 = df
r = which(rowSums(y)>3) # Rows to keep
df1 = df1[c(r),]
c = which(colSums(y)>2) # Columns to keep
c = c+18
df1 = df1[,c(1:18,c)]

# Repeat from here
y = df1[,c(19:ncol(df1))]
y[y>0]=1
r = which(rowSums(y)>3) # Rows to keep
df1 = df1[c(r),]
c = which(colSums(y)>2) # Columns to keep
c = c+18
df1 = df1[,c(1:18,c)]
# Repeat Above until it stops reducing


# Create Environmental data and put it into numerical or ordered factoral structure and turn percentages into decimal
names(df1)
env = df1[,c(2:16)]
str(env)
fac = c("Surveyor", "SITE_ID")
env[fac] <- lapply(env[fac], as.factor)

env$bare_ground_pct = ifelse(env$bare_ground_pct >1, env$bare_ground_pct/100, env$bare_ground_pct)

env$agric_pct
env$agric_pct = ifelse(env$agric_pct==">60%",60,env$agric_pct)
env$agric_pct = as.numeric(env$agric_pct)
env$agric_pct = ifelse(env$agric_pct >1, env$agric_pct/100, env$agric_pct)

env$oil_palm_y_n = ifelse(env$oil_palm_y_n == "Yes",1,0)
env$stumps_gt_30cm_y_n = ifelse(env$stumps_gt_30cm_y_n == "Yes",1,0)
env$stumps_gt_10cm__y_n = ifelse(env$stumps_gt_10cm__y_n == "Yes",1,0)
env$recent_burning_y_n = ifelse(env$recent_burning_y_n == "Yes",1,0)

env$forest_pct = ifelse(env$forest_pct == "5-20",12.5,env$forest_pct)
env$forest_pct = ifelse(env$forest_pct=="20-40",30,env$forest_pct)
env$forest_pct = ifelse(env$forest_pct=="40-60",50,env$forest_pct)
env$forest_pct = as.numeric(env$forest_pct)
env$forest_pct = ifelse(env$forest_pct >1, env$forest_pct/100, env$forest_pct)

env$liana_pct
env$liana_pct = ifelse(env$liana_pct == "5-10",7.5,env$liana_pct)
env$liana_pct = ifelse(env$liana_pct == "5-20",12.5,env$liana_pct)
env$liana_pct = ifelse(env$liana_pct == "10-20",15,env$liana_pct)
env$liana_pct = ifelse(env$liana_pct=="20-40",30,env$liana_pct)
env$liana_pct = ifelse(env$liana_pct=="40-60",50,env$liana_pct)
env$liana_pct = as.numeric(env$liana_pct)
env$liana_pct = ifelse(env$liana_pct >1, env$liana_pct/100, env$liana_pct)

env$cocoa_pct
env$cocoa_pct = ifelse(env$cocoa_pct == "5-20",12.5,env$cocoa_pct)
env$cocoa_pct = as.numeric(env$cocoa_pct)
env$cocoa_pct = ifelse(env$cocoa_pct >1, env$cocoa_pct/100, env$cocoa_pct)

env$grass_pct
env$grass_pct = ifelse(env$grass_pct == "5-20",12.5,env$grass_pct)
env$grass_pct = as.numeric(env$grass_pct)
env$grass_pct = ifelse(env$grass_pct >1, env$grass_pct/100, env$grass_pct)

env$canopyHt_m
env$canopyHt_m = ifelse(env$canopyHt_m == "10-20",15,env$canopyHt_m)
env$canopyHt_m = ifelse(env$canopyHt_m == "20-30",25,env$canopyHt_m)
env$canopyHt_m = ifelse(env$canopyHt_m == "30-40",35,env$canopyHt_m)
env$canopyHt_m = ifelse(env$canopyHt_m=="5-10",7.5,env$canopyHt_m)
env$canopyHt_m = ifelse(env$canopyHt_m=="2-5",3.5,env$canopyHt_m)
env$canopyHt_m = ifelse(env$canopyHt_m=="1-2",1.5,env$canopyHt_m)
env$canopyHt_m = as.numeric(env$canopyHt_m)
env$canopyHt_m = ifelse(env$canopyHt_m >1, env$canopyHt_m/100, env$canopyHt_m)

# ----------

# 2 - Remote Sensed Data
rs = read.csv("satellite_data/golaTrapPoints_15m_Sent12_q1_2022_popDens_hansYr_GLCM7_8.csv", row.names = 1)
names(rs)

df2 = merge(df1, rs, by = "SITE_ID")
# names(df2)
df2 = select(df2, 1:18, 302:ncol(df2), 19:301)
# names(df2)
y2 = df2[,c(118:ncol(df2))]
y2[y2>0]=1

# As per Pichler 2021 - Remove Sites with fewer than 4 OTU's
# Remove OTU's with fewer than 3 occurrences
df3 = df2
r2 = which(rowSums(y2)>3) # Rows to keep
df3 = df3[c(r2),]
c2 = which(colSums(y2)>2) # Columns to keep
c2 = c2+117
df3 = df3[,c(1:117,c2)]

# Repeat from here 
y2 = df3[,c(118:ncol(df3))]
y2[y2>0]=1
r2 = which(rowSums(y2)>3) # Rows to keep
df3 = df3[c(r2),]
c2 = which(colSums(y2)>2) # Columns to keep
c2 = c2+117
df3 = df3[,c(1:117,c2)]
# Repeat Above until it stops reducing

# names(df3)
env_rs = df3[,18:60]
# names(env_rs)

##########################################################################
#### --- Fit Models --- ####

# Basic workflow: 
## fit model: # increase iterations for your own data
# c = as.matrix(y)
# c2 = as.matrix(y2)
e = scale(env[, c(5:15)])
eo = scale(select(env_rs, 3:4, 14:22, 42:43)) # Radar SD, Indices means, min hansen & pop dens



# Spatial terms: for linear spatial model
XY = df1[,c(3:4)]
names(XY) = c("X1", "X2")

XY2 = df3[,c(3:4)]
names(XY2) = c("X1", "X2")



mEnv = sjSDM(Y = as.matrix(y), env = as.matrix(e), iter = 200, se = T, device = "gpu")
# mix of lasso and ridge, we do the same for the species-species associations
mEnvReg = sjSDM(Y = as.matrix(y), env = linear(as.matrix(e), lambda = 0.01, alpha = 0.5),
                biotic = bioticStruct(lambda = 0.01, alpha = 0.5), se = T, iter = 1000)
                
r2EV = Rsquared(mEnv, method = "McFadden") # ~0.35
save(mEnv, r2EV, file = "Model_Env.Rdata")

r2EVreg = Rsquared(mEnvReg, method = "McFadden") # ~0.
save(mEnvReg, r2EVreg, file = "Model_EnvReg.Rdata")

print(mEnv)
plot(mEnv)
summary(mEnv)
coef(mEnv)
getCov(mEnv)
# we usually normalize the covariance matrix to the correlation matrix:
cov2cor(getCov(mEnv))

imp = importance(mEnv)
print(imp)
plot(imp)

anEnv = anova(mEnv)
plot(anEnv, internal=TRUE)
plot(anEnv)
print(anEnv)
save(anEnv, file = "Anonva_Env.Rdata")
load("Anonva_Env.Rdata") # Loads as variable "an"
anEnv = an

anEnvReg = anova(mEnvReg)


# --
# If the Environmental variables are scaled, then so must the Spatial variables be
mEnvSp = sjSDM(Y = as.matrix(y), env = as.matrix(e), spatial = linear(scale(XY), ~0+X1:X2), se = T, iter = 200, device = "gpu")
# Using a trend surface model (which includes interactiosn between X&Y) as per help vignette improves R2 due to more flexibility
# ~0+X1+X2:X1:X2+I(X1^2)+I(X2^2) below can be replaced with ~0+poly(X1, X2, degree = 2)
mEnvSp = sjSDM(Y = as.matrix(y), env = as.matrix(e), spatial = linear(scale(XY), ~0+X1+X2:X1:X2+I(X1^2)+I(X2^2)), se = T, iter = 200, device = "gpu")


r2EVSp = Rsquared(mEnvSp, method = "McFadden") # ~0.22
save(mEnvSp, r2EVSp, file = "Model_EnvSpatial.Rdata")

print(mEnvSp)
plot(mEnvSp)
summary(mEnvSp)
coef(mEnvSp)
getCov(mEnvSp)
# we usually normalize the covariance matrix to the correlation matrix:
cov2cor(getCov(mEnvSp))

imp = importance(mEnvSp)
print(imp)
plot(imp)

anEnvSp = anova(mEnvSp, device = "gpu")
plot(anEnvSp, internal=TRUE)
plot(anEnvSp)
print(anEnvSp)
save(anEnvSp, file = "Anonva_EnvSpatial.Rdata")
load("Anonva_EnvSpatial.Rdata") # Loads as "an"
anEnvSp = an

# --
mEO = sjSDM(Y = as.matrix(y2), env = as.matrix(eo), se = T, iter = 1000)
r2Eo = Rsquared(mEO, method = "McFadden") # ~0.20 0.22
save(mEO, r2Eo, file = "Model_EO.Rdata")

print(mEO)
plot(mEO)
summary(mEO)
coef(mEO)
getCov(mEO)
# we usually normalize the covariance matrix to the correlation matrix:
cov2cor(getCov(mEO))

imp = importance(mEO)
print(imp)
plot(imp)

anEO = anova(mEO)
plot(anEO, internal=TRUE)
plot(anEO)
print(anEO)
save(anEO, file = "Anonva_EO.Rdata")
load("Anonva_EO.Rdata") # Loads as "an"
anEO = an

# --
mEOSp = sjSDM(Y = as.matrix(y2), env = as.matrix(eo), spatial = linear(XY2, ~0+X1:X2), se = T, iter = 1000)
# mix of lasso and ridge, we do the same for the species-species associations
mEOSpReg = sjSDM(Y = as.matrix(y2), env = linear(as.matrix(eo), lambda = 0.01, alpha = 0.5),
                biotic = bioticStruct(lambda = 0.01, alpha = 0.5), spatial = linear(XY2, ~0+X1:X2),
                se = T, iter = 1000)

r2EOSp = Rsquared(mEOSp, method = "McFadden") # ~0.15
save(mEOSp, r2EOSp, file = "Model_EOspatial.Rdata")

r2EOSpReg = Rsquared(mEOSp, method = "McFadden") # ~0.15
save(mEOSpReg, r2EOSpReg, file = "Model_EOspatial.Rdata")


print(mEOSp)
plot(mEOSp)
summary(mEOSp)
coef(mEOSp)
getCov(mEOSp)
# we usually normalize the covariance matrix to the correlation matrix:
cov2cor(getCov(mEOSp))

imp = importance(mEOSp)
print(imp)
plot(imp)

anEOsp = anova(mEOSp)
plot(anEOsp, internal=TRUE)
plot(anEOsp)
print(anEOsp)
save(anEOsp, file = "Anonva_EOspatial.Rdata")
load("Anonva_EOspatial.Rdata") # Laads as "an"
anEOsp = an


###################

model_env = sjSDM(Y = as.matrix(y),
              env = linear(data = e), # formula = ~X1:X2 + X3),
              spatial = linear(XY, ~0+X1:X2),
              # se = TRUE,
              family = binomial("probit"),
              iter = 50)

model_EO = sjSDM(Y = c2,
                  env = linear(data = eo), # formula = ~X1:X2 + X3),
                  spatial = linear(XY2, ~0+X1:X2),
                  # se = TRUE,
                  # family = binomial("probit"),
                  iter = 50)

## without intercept:
model2 = update(model, env_formula = ~0+.)

R2 = Rsquared(model)
print(R2)

R2 = Rsquared(model_EO)
print(R2)

coef(model)
summary(model)
getCov(model)

# Variance partitioning between Environment, Biotic (Species Interactions) & Spatial
imp = importance(model)
print(imp)
plot(imp)

## Visualize internal meta-community structure
an = anova(model)
plot(an, internal=TRUE)
print(an)

## plot results
plot(model)
# species=c("sp1","sp2","sp3","sp4","sp5","sp6","sp7")
# group=c("mammal","bird","fish","fish","mammal","amphibian","amphibian")
# group = data.frame(species=species,group=group)
# plot(model,group=group)

# Regularization
## lambda is the regularization strength
## alpha weights the lasso or ridge penalty:
## - alpha = 0 --> pure lasso
## - alpha = 1.0 --> pure ridge

#### TUNE THE MODEL TO GET BEST LAMBDA and ALPHA
tune = sjSDM_cv(env = e, Y = as.matrix(y),
                # learning_rate = 0.005, iter = 100L, CV = 5,
                # tune_steps = 40,
                # lambda_cov = seq(0, 1, 0.1),
                # lambda_coef = seq(0, 0.1, 0.001),
                # alpha_cov = seq(0, 1, 0.05),
                # alpha_coef = seq(0, 1, 0.05),
                # alpha_spatial = seq(0, 1, 0.05),
                # sampling = 100L,
                # biotic = bioticStruct(df=dim(Occ)[2]),
                spatial = linear(XY, ~0+X1:X2),
                # lambda_spatial = 2^seq(-10, -0.5, length.out = 20),
                # step_size = 5L,
                # blocks = 3L,
                # family=poisson("log"),
                device = "gpu")
                # n_cores = 8)

best = head(tune$short_summary[order(tune$short_summary$logLik),])[1,]

model_tuned <- sjSDM(Y = as.matrix(y),
               env = linear(as.matrix(e), lambda = best[["lambda_coef"]], alpha = best[["alpha_coef"]]),
               spatial = linear(scale(XY), ~0+., lambda = best[["lambda_spatial"]], alpha = best[["alpha_spatial"]]),
               biotic = bioticStruct(lambda = best[["lambda_cov"]], alpha = best[["alpha_cov"]], df = dim(y)[2]),
               iter = 1000, learning_rate = 0.005, device = "gpu")
r2Tuned = Rsquared(model_tuned, method = "McFadden")


model2 = sjSDM(Y = c, 
              # mix of lasso and ridge
              env = linear(e, lambda = 0.01, alpha = 0.5), 
              # we can do the same for the species-species associations
              biotic = bioticStruct(lambda = 0.01, alpha = 0.5),
              spatial = linear(XY, ~0+X1:X2),
              # se = TRUE,
              family = binomial("probit"),
              iter = 50L) # increase iter for your own data

model2 = sjSDM(Y = c2, 
               # mix of lasso and ridge
               env = linear(eo, lambda = 0.01, alpha = 0.5), 
               # we can do the same for the species-species associations
               biotic = bioticStruct(lambda = 0.01, alpha = 0.5),
               spatial = linear(XY2, ~0+X1:X2),
               # se = TRUE,
               family = binomial("probit"),
               iter = 50L) # increase iter for your own data

# Variance partitioning between Environment, Biotic (Species Interactions) & Spatial
imp2 = importance(model2)
print(imp2)
plot(imp2)

## Visualize internal meta-community structure
an2 = anova(model2)
plot(an2, internal=TRUE)
plot(an2)
print(an2)

r2 = Rsquared(model = model2, method = "McFadden")
r2

## extract weights
weights = getWeights(model)

## Make Predictions
# predict(model, newdata = NEWenv, SP = newXY)