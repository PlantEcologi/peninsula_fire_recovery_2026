##################################################
##data prep and model fitting for Slingsby, Moncrieff and Wilson 2020
##################################################

####################
##setup
####################
#you only need to run this once
#renv::init()

### Load libraries
libs=c(
  "doParallel",
  #"rasterVis",
  #"rgdal",
  #"reshape2",
  "sf",
  "knitr",
  "rmarkdown",
  "ggplot2",
  "tidyr",
  "tidyverse",
  #"minpack.lm",
  #"maptools",
  "lubridate",
  "rjags",
  "dclone",
  #"raster",
  "terra")
lapply(libs, require, character.only=T)

#file locations and names
mdatwd <- "data/"
mname <- "peninsulaJune2022" #model name for file naming

# Calculate the number of cores
no_cores <- detectCores() - 1

# # Initiate cluster
cl <- makeCluster(no_cores, type = "FORK")
registerDoParallel(cl)

###########################################################
###Load data
###########################################################

#set extent and projection
cp <- ext(18.37, 18.48, -34.362, -34.19)
projection <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#modis ndvi from MODIS/061/MOD13Q1
NDVI <- rast("data/NDVI_stack_2001_2026.tif") #new NDVI data
crs(NDVI) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m"
NAflag(NDVI) <- -9999
NDVI <- trim(NDVI)
NDVI <- project(NDVI, y = crs(projection)) #Make sure it's the same projection
NDVI <- crop(NDVI, cp)
NDVIdates <- read.csv("data/NDVI_dates_2001_2026.csv") # Load dates
time(NDVI) <- as.Date(NDVIdates$date) # Assign time
NDVI <- NDVI[[time(NDVI) < as.Date("2022-06-01")]] # Select only NDVI within the fire observation period

#spatial covariates
#load(paste0(mdatwd,"inputData.RData"))

vegtype <- vect("data/veg/Vegetation_Indigenous_Remnants.shp") |> 
  project(crs(projection)) |> 
  crop(cp)
vegtype$National_ <- str_replace_all(vegtype$National_,
                                    c("Beach" = "beach", 
                                      "Cape Flats Dune Strandveld - False Bay" = "dune", 
                                      "Cape Lowland Freshwater Wetlands" = "wetland", 
                                      "Hangklip Sand Fynbos" = "sand",
                                      "Peninsula Granite Fynbos - South" = "granite",
                                      "Peninsula Sandstone Fynbos" = "sandstone"))
vegtype <- rasterize(vegtype, NDVI, field="National_", background=NA, na.rm = T)
names(vegtype) <- "vegtype"

sta <- rast("data/STATIC_stack.tif")
NAflag(sta) <- -9999
sta <- trim(sta)
crs(sta) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m"
sta <- project(sta, NDVI) #Make sure it's the same projection
#sta <- crop(sta, cp)
sta$northness <- cos(sta$aspect*pi/180)
sta$eastness <- sin(sta$aspect*pi/180)
sta$vegtype <- vegtype
sta$prec_jan <- sta$prec_jan/10
sta$prec_jul <- sta$prec_jul/10
sta$tmax_jan <- sta$tmax_jan/100
sta$tmin_jul <- sta$tmin_jul/100

#vegetation age
rfi <- rast("data/veldage2022.tif") #new fire age data
rfi <- project(rfi, NDVI)

#NDVI <- stack(paste0(mdatwd,"NDVI"))
#QA <- stack(paste0(mdatwd,"QA")) #no QA data as this step was done in the new GEE script
#NDVI <- project(NDVI, rfi, method = "near")

#mask data to where we have fire data
NDVI <- mask(NDVI, rfi[[1]]) #note use of rfi - already cropped by LC
sta <- mask(sta, rfi[[1]])

###########################################################
###Convert raster data to df
###########################################################

# Time-series
ndat <- as.data.frame(NDVI, xy=TRUE, na.rm=F) |>
  pivot_longer(cols = -c(x,y), names_to = "Date", values_to = "NDVI") |>
  mutate(Date = as.Date(substr(Date,1,10), format = "%Y_%m_%d"))
fdat <- as.data.frame(rfi, xy=TRUE, na.rm=F) |>
  pivot_longer(cols = -c(x,y), names_to = "Date", values_to = "Age") |>
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
cdat <- inner_join(na.omit(ndat), na.omit(fdat), by=c("x","y","Date")) |> 
  #filter(complete.cases(.)) |> 
  mutate(UI = paste(x, y, sep = "_")) |>
  mutate(UIJ = paste(round(x, 3), round(y, 3), sep = "_")) #replace unique identifier rounded to coords with 3 decimal places (that matches temporal data))

# Covariates
cov <- as.data.frame(sta, xy=TRUE, na.rm=F) |>
  na.omit() |>
  mutate(UI = paste(x, y, sep = "_")) |>
  mutate(UIJ = paste(round(x, 3), round(y, 3), sep = "_")) #replace unique identifier rounded to coords with 3 decimal places (that matches temporal data))

# #NDVI
# ndat <- as.data.frame(NDVI, xy=TRUE, na.rm=TRUE) #as.data.frame(rasterToPoints(NDVI))
# datXY <- dat[,1:2]
# dat <- as.data.frame(t(dat[,3:ncol(dat)])) #transpose data.frame
# dat$Date <- as.Date(rownames(dat), format = "X%Y.%m.%d")
# dat <- melt(dat, id=c("Date"))
# datXY$variable <- paste0("V", rownames(datXY))
# dat <- merge(dat, datXY)
# names(dat)[which(names(dat)=="value")] <- "NDVI"
# 
# #QA data
# qdat <- as.data.frame(rasterToPoints(QA))
# qdatXY <- qdat[,1:2]
# qdat <- as.data.frame(t(qdat[,3:ncol(qdat)])) #transpose data.frame
# qdat$Date <- as.Date(rownames(qdat), format = "X%Y.%m.%d")
# qdat <- melt(qdat, id=c("Date"))
# qdatXY$variable <- paste0("V", rownames(qdatXY))
# qdat <- merge(qdat, qdatXY)
# names(qdat)[which(names(qdat)=="value")] <- "QA"
# 
# #Age (rfi) data
# adat <- as.data.frame(rasterToPoints(rfi))
# adatXY <- adat[,1:2]
# adat <- as.data.frame(t(adat[,3:ncol(adat)])) #transpose data.frame
# adat$Date <- as.Date(rownames(adat), format = "X%Y.%m.%d")
# adat <- melt(adat, id=c("Date"))
# adatXY$variable <- paste0("V", rownames(adatXY))
# adat <- merge(adat, adatXY)
# names(adat)[which(names(adat)=="value")] <- "Age"

# #Merge the three
# cdat <- merge(qdat, dat, by = c("Date", "x", "y"))
# cdat <- merge(cdat, adat, by = c("Date", "x", "y"))
# cdat <- cdat[,c("Date", "x", "y", "Age", "NDVI", "QA")] #drop unwanted columns
# cdat$UI <- paste(cdat$x, cdat$y, sep = "_") #add unique identifier not rounded
# cdat$UIJ <- paste(round(cdat$x, 3), round(cdat$y, 3), sep = "_") #add unique identifier rounded to coords with 3 decimal places (that matches covariates)

###########################################################
###Filter by a few NB criteria and trim covariates to match
###########################################################

#drop NDVI values < 0 or pixels with <50 NDVI observations
cdat <- cdat |> filter(NDVI > 0) |>
  group_by(UIJ) |> 
  filter(n() >= 50) |>
  ungroup()

#trim covariates and temporal data to match
cov <- cov |> 
  filter(complete.cases(cov)) |> 
  filter(vegtype %in% c("dune", "granite", "sandstone", "sand")) |> #drop beach and wetland veg types with few observations
  filter(UIJ %in% cdat$UIJ) |>
  droplevels()

cdat <- filter(cdat, UIJ %in% cov$UIJ)

###########################################################
###Quick data check and clear unwanted objects from memory
###########################################################

rm(list = c("NDVI", "NDVIdates", "sta", "rfi", "vegtype", "libs", "ndat", "fdat")); gc()

###########################################################
###Add columns for month of fire and age in years
###########################################################

#create new variable indicating month of fire
cdat <- cdat %>% mutate(firemonth=month(Date-Age))

#create age column in years
cdat$DA <- cdat$Age/365.25

###########################################################
###Have a look at some raw data
###########################################################

cdat |>
  filter(UIJ %in% sample(unique(UIJ), 9)) |> #== "18.383_-34.199") |>
  ggplot(aes(x = DA, y = NDVI)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = T) +
  facet_wrap(~UIJ)
# #+ geom_vline(aes(xintercept = as.numeric(Fire)))


###########################################################
### Create dummy variables for veg type and select and scale environmental data
###########################################################

#create dummy vars for veg type
#cov$vegnut <- as.factor(cov$geo)
tveg <- as.numeric(cov$vegtype) - 1
dummies <- model.matrix(~as.factor(tveg))
dummies <- dummies[,-1]

#select vars and scale data
envars <- c("elevation", "slope", "tpi", "northness", "eastness") 
scaled <- scale(as.matrix(cov[,envars]))
env <- as.data.frame(cbind(intercept=1, scaled, tveg))
env$UI <- cov$UI
env$UIJ <- cov$UIJ
env <- cbind(env,dummies)

#have a look at collinearity
# require(GGally)
# ggpairs(as.data.frame(env_full))

#save the scaling parameters to convert fitted coefficients back to metric units later
beta.mu=c(intercept=0,attr(scaled,"scaled:center")) #,rep(0,ncol(tveg)))
beta.sd=c(intercept=1,attr(scaled,"scaled:scale")) #,rep(1,ncol(tveg)))
rm(scaled)  #drop the scaled data

###########################################################
###Format data for JAGS
###########################################################

tdat <- cdat

# #set up cross-validation dataset?
# holdout <- 0.00
# set.seed(111)
# s <- sort(sample(unique(cov$UI),round(length(unique(cov$UI))*(1-holdout)))); length(s)
# #NA sites in cv set
# tdat$NDIN=tdat$NDVI
# tdat$NDIN[!(tdat$UI%in%s)]=NA; gc() 

tdat$NDIN <- tdat$NDVI
#if we want to predict NDVI for date beyond 2014-05-31
#tdat$NDIN[(tdat$Date>as.Date("2014-05-31"))]=NA; gc() 

#create new id that goes from 1 to nGrid (to order env and tdat in the same way)
env$jag_id <- as.integer(as.factor(env$UIJ))
jtab <- data.frame(UIJ=env$UIJ,jag_id=env$jag_id, stringsAsFactors=F)
tdat <- left_join(tdat, jtab, by='UIJ')

#make sure tdat and env have matching sites
env_sites <- unique(env$jag_id)
tdat_sites <- unique(tdat$jag_id)

tdat <- tdat %>%
  filter(jag_id %in% env_sites)
env <- env %>%
  filter(jag_id %in% tdat_sites)

#arrange temporal and env data into same order
drop.cols <- c('jag_id', 'UI','UIJ','tveg')
env <- env[order(env$jag_id),] 
save(env,file=paste(mdatwd,mname,"_envdata.Rdata", Sys.Date(),sep="")) #save env for analysing results
env <- env %>% dplyr::select(-one_of(drop.cols))
env <- as.matrix(env)
tdat <- tdat[order(tdat$jag_id),]

#final check
if(length(unique(tdat$jag_id)) != nrow(env))  print("sites not matching between spatial and temporal data!")

#save tdat with all dates
tdat_full <- tdat
#otherwise only send data up to 2014-05-31 to jags
#tdat <- tdat[(tdat$Date<=as.Date("2014-05-31")),]; gc() 

#for alter analysis
save(tdat,tdat_full, file=paste(mdatwd,mname, Sys.Date(),"_inputdata_small.Rdata",sep="")) #save env for analysing results
###########################################################
###Prep JAGS inputs
###########################################################

#get counts
nGrid=length(unique(tdat$jag_id))       ;nGrid
nTime=length(unique(tdat$Date))          ;nTime
nBeta=ncol(env)                          ;nBeta

#write data object
data=list(
  age=tdat$DA,
  ndvi=tdat$NDIN, 
  id=tdat$jag_id,
  firemonth=tdat$firemonth,
  nObs=nrow(tdat),
  env=env,
  nGrid=nGrid,
  nBeta=nBeta
)

#function to generate initial values
gen.inits=function(nGrid,nBeta) { list(
  ## spatial terms
  alpha=runif(nGrid,0.1,0.5),
  gamma=runif(nGrid,0.1,.9),
  A=runif(nGrid,0.1,.9),
  lambda=runif(nGrid,0.2,1),
  ## spatial means
  alpha.mu=runif(1,0.1,0.2),
  ## priors  
  gamma.beta=runif(nBeta,0,1),
  gamma.tau=runif(1,1,5),
  alpha.tau=runif(1,1,5),
  lambda.beta=runif(nBeta,0,2),
  lambda.tau=runif(1,0,2),
  A.beta=runif(nBeta,0,1),
  A.tau=runif(1,1,5),
  tau=runif(1,0,2)
)
}

#list of parameters to monitor (save)
params=c("phi","gamma.beta","gamma.sigma","A.beta","A.sigma","alpha","gamma","lambda","A",
         "alpha.mu","alpha.sigma","lambda.beta","lambda.sigma","sigma")

#params=c("gamma.beta")
###########################################################
###Save all data into Rdata object for model fitting
###########################################################

#save.image(file=paste(mdatwd,mname,"_inputdata.Rdata",sep="")) 

rm(list = ls()[-which(ls() %in% c("mdat", "mname", "data", "params", "cl", "mdatwd", "gen.inits"))])
gc()

###########################################################
###Run JAGS
###########################################################

foutput=paste0(mdatwd, mname, Sys.Date(), "_modeloutput.Rdata")


m <- jags.parfit(cl = cl, #runs chains in parallel with library(dclone)
                 data = data, 
                 params = params, 
                 model = "Model.R", 
                 inits = gen.inits(data$nGrid,data$nBeta), 
                 n.chains = 3,
                 n.adapt=10000,n.update=10000,
                 thin = 5, n.iter = 10000
)

# m <- jags.fit(data = data, 
#                  params = params, 
#                  model = "Model_nc.R", 
#                  inits = gen.inits(data$nGrid,data$nBeta), 
#                  n.chains = 1,
#                  n.adapt=100,n.update=100,
#                  thin = 1, n.iter = 200
# )


save(m,file=foutput)

