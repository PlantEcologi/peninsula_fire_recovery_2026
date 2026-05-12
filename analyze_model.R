#################################
######Analysis of results and creation of rasters for upload to GEE
#################################
#only run if you have not before
#renv::init()

libs=c(
  "tidyverse",
  #"tidyr",
  "terra",
  "coda",
  "rjags")
lapply(libs, require, character.only=T)



#file locations and names
mdatwd <- "data/"
mname <- "peninsulaJune20222026-04-16" #model name for file naming

# #download the results if you did not create them in fit_model.R:
# download.file('https://storage.googleapis.com/data-sharing-gmoncrieff/peninsulaDec2019_modeloutput.Rdata', destfile = paste0(mdatwd, mname, "_modeloutput.Rdata"))
# download.file('https://storage.googleapis.com/data-sharing-gmoncrieff/peninsulaDec2019_envdata.Rdata', destfile = paste0(mdatwd, mname, "_envdata.Rdata"))
# download.file('https://storage.googleapis.com/data-sharing-gmoncrieff/peninsulaDec2019_inputdata_small.Rdata', destfile = paste0(mdatwd, mname, "_inputdata_small.Rdata"))

#load results
foutput <- paste0(mdatwd, mname, "_modeloutput.Rdata")
envdata <- paste0(mdatwd,mname,"_envdata.Rdata")

#load model results
#load env data
load(foutput)
load(envdata)

#check model fitting - select a parameter
par <- m[,"phi"]
plot(par)

#extract parameter summaries
res <- as.data.frame(summary(m)$statistics)

#summarise non spatial pars
phi <- res %>%
  mutate(pname = rownames(res)) %>%
  filter(pname =='phi')

sig <- res %>%
  mutate(pname = rownames(res)) %>%
  filter(pname =='sigma')

#summarise spatial pars
alphas <- res %>%
  mutate(par = rownames(res)) %>%
  filter(str_detect(par, "alpha")) %>%
  filter(str_detect(par, "\\[")) %>%
  mutate(parstr = str_sub(par,1,5)) %>%
  mutate(parnum = str_remove(par,"alpha")) %>%
  mutate(parnum = str_sub(parnum,2,-2)) %>%
  select(c("Mean","SD","parstr","parnum"))

lambdas <- res %>%
  mutate(par = rownames(res)) %>%
  filter(str_detect(par, "lambda")) %>%
  filter(str_detect(par, "\\[")) %>%
  filter(str_detect(par, "beta",negate=TRUE)) %>%
  mutate(parstr = str_sub(par,1,6)) %>%
  mutate(parnum = str_remove(par,"lambda")) %>%
  mutate(parnum = str_sub(parnum,2,-2)) %>%
  select(c("Mean","SD","parstr","parnum"))

gammas <- res %>%
  mutate(par = rownames(res)) %>%
  filter(str_detect(par, "gamma")) %>%
  filter(str_detect(par, "\\[")) %>%
  filter(str_detect(par, "beta",negate=TRUE)) %>%
  mutate(parstr = str_sub(par,1,5)) %>%
  mutate(parnum = str_remove(par,"gamma")) %>%
  mutate(parnum = str_sub(parnum,2,-2)) %>%
  select(c("Mean","SD","parstr","parnum"))

As <- res %>%
  mutate(par = rownames(res)) %>%
  filter(str_detect(par, "A")) %>%
  filter(str_detect(par, "\\[")) %>%
  filter(str_detect(par, "beta",negate=TRUE)) %>%
  mutate(parstr = str_sub(par,1,2)) %>%
  mutate(parnum = str_remove(par,"A")) %>%
  mutate(parnum = str_sub(parnum,2,-2)) %>%
  select(c("Mean","SD","parstr","parnum"))

######################################
####convert to rasters
#####################################

env = env |>
  separate_wider_delim(cols = UI, names = c("lon","lat"), delim = "_") |>
  mutate(across(c("lon","lat"), as.numeric))

#rasterise - could try making on df and seeing if rast() would make a stack?
#grd <- rast(paste0(mdatwd,"NDVI_input_grid.tif"))

gammaM <- data.frame(lon = env$lon, lat = env$lat, gammas$Mean)
gammaSD <- data.frame(lon = env$lon, lat = env$lat, gammas$SD)
alphaM <- data.frame(lon = env$lon, lat = env$lat, alphas$Mean)
alphaSD <- data.frame(lon = env$lon, lat = env$lat, alphas$SD)
lambdaM <- data.frame(lon = env$lon, lat = env$lat, lambdas$Mean)
lambdaSD <- data.frame(lon = env$lon, lat = env$lat, lambdas$SD)
AM <- data.frame(lon = env$lon, lat = env$lat, As$Mean)
ASD <- data.frame(lon = env$lon, lat = env$lat, As$SD)

gammaMras <- rast(gammaM, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
gammaSDras <- rast(gammaSD, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
alphaMras <- rast(alphaM, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
alphaSDras <- rast(alphaSD, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
lambdaMras <- rast(lambdaM, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
lambdaSDras <- rast(lambdaSD, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
AMras <- rast(AM, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
ASDras <- rast(ASD, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

#write to disk
writeRaster(gammaMras,paste0(mdatwd, "/output_parameters/gammaM.tif"))
writeRaster(gammaSDras,paste0(mdatwd, "/output_parameters/gammaSD.tif"))
writeRaster(lambdaMras,paste0(mdatwd, "/output_parameters/lambdaM.tif"))
writeRaster(lambdaSDras,paste0(mdatwd, "/output_parameters/lambdaSD.tif"))
writeRaster(alphaMras,paste0(mdatwd, "/output_parameters/alphaM.tif"))
writeRaster(alphaSDras,paste0(mdatwd, "/output_parameters/alphaSD.tif"))
writeRaster(AMras,paste0(mdatwd, "/output_parameters/AM.tif"))
writeRaster(ASDras,paste0(mdatwd, "/output_parameters/ASD.tif"))
