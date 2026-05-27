###########################################################
###Script to generate plots for Slingsby, Moncrieff and Wilson 2020
###########################################################


#only run if you have not before
#renv::init()

libs=c("tidyverse",
       "terra",
       "coda",
       "rjags",
  #     "tictoc",
       "readxl",
       "scales",
       "sf",
       "cowplot",
       "stringr",
       "ggridges",
       "viridis",
       "ggforce")
lapply(libs, require, character.only=T)

#file locations and names
mdatwd <- "data/"
mname <- "peninsulaLandsatMay20262026-05-26" #model name for file naming

###########################################################
###Get exceedance rasters (GEE output) and plot
###NOTE: further plotting based on JAGS outputs below
###Hash out this section if you don't have GEE outputs yet
###########################################################

# Get data and wrangle for plotting
exceed <- stack("data/exceed_below.tif", "data/exceed_above.tif")
names(exceed) <- c("below", "above")
exceed <- projectRaster(exceed, crs = CRS("+proj=merc +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +units=m +no_defs"))
edat <- as.data.frame(rasterToPoints(exceed, spatial = F))
edat <- melt(edat, id = c("x", "y"))
edat <- fortify(edat)

# Get a pretty coastline for plotting
coast <- st_read("Data/coastline") #, layer = "coastline")
coast <- fortify(coast)

# Plot

g <- ggplot() +
  geom_sf(data = coast, fill = "skyblue1") +
  geom_tile(data = edat, aes(x, y, fill = value)) + 
  scale_fill_gradient(low = "#FFF5F0", high = "#67000D", na.value = "transparent") +
  facet_wrap(~variable) +
  theme_void() +
  labs(fill="Deviance") + 
  theme(legend.position=c(.1,.25)) + 
  annotate("rect", xmin = 2045607, xmax = 2054169, ymin = -4045661, ymax = -4040601, fill = "transparent", colour = "grey30") + 
  annotate("text", label = "Silvermine", x = 2050000, y = -4047000, colour = "grey30") +
  annotate("rect", xmin = 2037356, xmax = 2042970, ymin = -4037517, ymax = -4031284, fill = "transparent", colour = "grey30") + 
  annotate("text", label = "Karbonkelberg", x = 2040000, y = -4038500, colour = "grey30") +
  annotate("rect", xmin = 2050000, xmax = 2052500, ymin = -4072500, ymax = -4069250, fill = "transparent", colour = "grey30") + 
  annotate("text", label = "Cape of Good Hope", x = 2050000, y = -4074000, colour = "grey30") +
  annotate("rect", xmin = 2053500, xmax = 2056500, ymin = -4061000, ymax = -4058500, fill = "transparent", colour = "grey30") + 
  annotate("text", label = "Miller's Point", x = 2055500, y = -4062500, colour = "grey30")

ggsave(filename = "figures/exceedmap.png", plot = g, device = NULL, path = NULL, scale = 1, width = 18, height = 18, units = "cm", dpi = 300, limitsize = TRUE)

###########################################################
###Get model data and prep for model prediction and plotting
###########################################################

# #download the results if you did not create them in fit_model.R:
# download.file('https://storage.googleapis.com/data-sharing-gmoncrieff/peninsulaDec2019_modeloutput.Rdata', destfile = paste0(mdatwd, mname, "_modeloutput.Rdata"))
# download.file('https://storage.googleapis.com/data-sharing-gmoncrieff/peninsulaDec2019_envdata.Rdata', destfile = paste0(mdatwd, mname, "_envdata.Rdata"))
# download.file('https://storage.googleapis.com/data-sharing-gmoncrieff/peninsulaDec2019_inputdata_small.Rdata', destfile = paste0(mdatwd, mname, "_inputdata_small.Rdata"))

#load results
foutput <- paste0(mdatwd, mname, "_modeloutput.Rdata")
envdata <- paste0(mdatwd,mname,"_envdata.Rdata")
inputdata <- paste0(mdatwd,mname,"_inputdata_small.Rdata")

#load model results
#load env data
load(foutput)
load(envdata)
load(inputdata)

#add columns for results
tdat <- tdat_full
tdat$mean <- NA
tdat$upper <- NA
tdat$lower <- NA
tdat$lq <- NA
tdat$uq <- NA

#all results
res <- as.data.frame(summary(m)$statistics)

#firemonth
phi <- res %>%
  mutate(pname = rownames(res)) %>%
  filter(pname =='phi')

#model sd
sig <- res %>%
  mutate(pname = rownames(res)) %>%
  filter(pname =='sigma')

#extract spatial pars
alphas <- res %>%
  mutate(par = rownames(res)) %>%
  filter(str_detect(par, "alpha")) %>%
  filter(str_detect(par, "\\[")) %>%
  mutate(parstr = str_sub(par,1,5)) %>%
  mutate(parnum = str_remove(par,"alpha")) %>%
  mutate(parnum = str_sub(parnum,2,-2)) %>%
  dplyr::select(c("Mean","SD","parstr","parnum"))

lambdas <- res %>%
  mutate(par = rownames(res)) %>%
  filter(str_detect(par, "lambda")) %>%
  filter(str_detect(par, "\\[")) %>%
  filter(str_detect(par, "beta",negate=TRUE)) %>%
  mutate(parstr = str_sub(par,1,6)) %>%
  mutate(parnum = str_remove(par,"lambda")) %>%
  mutate(parnum = str_sub(parnum,2,-2)) %>%
  dplyr::select(c("Mean","SD","parstr","parnum"))

gammas <- res %>%
  mutate(par = rownames(res)) %>%
  filter(str_detect(par, "gamma")) %>%
  filter(str_detect(par, "\\[")) %>%
  filter(str_detect(par, "beta",negate=TRUE)) %>%
  mutate(parstr = str_sub(par,1,5)) %>%
  mutate(parnum = str_remove(par,"gamma")) %>%
  mutate(parnum = str_sub(parnum,2,-2)) %>%
  dplyr::select(c("Mean","SD","parstr","parnum"))

As <- res %>%
  mutate(par = rownames(res)) %>%
  filter(str_detect(par, "A")) %>%
  filter(str_detect(par, "\\[")) %>%
  filter(str_detect(par, "beta",negate=TRUE)) %>%
  mutate(parstr = str_sub(par,1,1)) %>%
  mutate(parnum = str_remove(par,"A")) %>%
  mutate(parnum = str_sub(parnum,2,-2)) %>%
  dplyr::select(c("Mean","SD","parstr","parnum"))


#to points layer of ids:
env <- env |> st_set_geometry(env$UI) |> 
  st_as_sf(crs = 4326)

#  separate(UI,c("lon","lat"),sep="_")
jag_id <- env |> select("jag_id", "UI")
#jag_id <- data.frame(lon = env$lon, lat = env$lat, env$jag_id)
#jag_id_ras<-rast(jag_id,crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

###########################################################
###Plot maps of parameters and boxplot of regression coefficients
###########################################################

# Parameter coefficients

gcols <- grep("gamma.beta", colnames(m[[1]]))
gamma.coef <- data.frame(variable = rep(sort(rep(colnames(m[[1]])[gcols],1000)),3),
                         value = as.vector(sapply(m, FUN = function(x){x[,gcols]})),
                         stringsAsFactors = F)

lcols <- grep("lambda.beta", colnames(m[[1]]))
lambda.coef <- data.frame(variable = rep(sort(rep(colnames(m[[1]])[lcols],1000)),3),
                          value = as.vector(sapply(m, FUN = function(x){x[,lcols]})),
                          stringsAsFactors = F)

Acols <- grep("A.beta", colnames(m[[1]]))
A.coef <- data.frame(variable = rep(sort(rep(colnames(m[[1]])[Acols],1000)),3),
                     value = as.vector(sapply(m, FUN = function(x){x[,Acols]})),
                     stringsAsFactors = F)

betas <- bind_rows(lambda.coef, gamma.coef, A.coef)
betas$covariate <- str_sub(betas$variable, -7, -1)
betas$variable <- str_sub(betas$variable, 1, -9)
betas <- filter(betas, !covariate == "beta[1]")
betas$covariate <- recode(betas$covariate,  # see names(env) for names and order of covariates
                          'beta[2]' = "elevation", 
                          'beta[3]' = "slope",
                          'beta[4]' = "TPI",
                          'beta[5]' = "northness",
                          'beta[6]' = "eastness")
                        #  'beta[7]' = "granite", 
                        #  'beta[8]' = "sandstone",
                        #  'beta[9]' = "sand")

b <- ggplot(betas) +
  geom_boxplot(aes(x = covariate, y = value)) +
  facet_wrap(.~variable, scales = "free_x") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_hline(aes(yintercept = 0), colour = "gray50") +
  coord_flip()

# Maps

dat <- bind_rows(lambdas, gammas, alphas, As) |>
  mutate(across(c("parnum"), as.numeric))
dat <- left_join(dat, jag_id, by = c("parnum" = "jag_id")) #"gammas.parnum"))
dat$lon <- as.numeric(as.character(dat$lon))
dat$lat <- as.numeric(as.character(dat$lat))
dat <- fortify(dat)

###Get coastline for pretty plotting

coast <- st_read("Data/coastline") #, layer = "coastline")
coast <- st_transform(coast, '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
coast <- st_crop(coast, jag_id_ras)
coast <- fortify(coast)

###Plot

# Means

l <- ggplot() +
  geom_sf(data = coast, fill = "skyblue1") +
  geom_tile(data = filter(dat, parstr == "lambda"), aes(lon, lat, fill = log(Mean))) + 
  scale_fill_gradient(low = "#FFF5F0", high = "#67000D", na.value = "transparent") +
  theme_void() +
  labs(fill="ln(lambda)")

g <- ggplot() +
  geom_sf(data = coast, fill = "skyblue1") +
  geom_tile(data = filter(dat, parstr == "gamma"), aes(lon, lat, fill = log(Mean))) + 
  scale_fill_gradient(low = "steelblue4", high = "white", na.value = "transparent") +
  theme_void() +
  labs(fill="ln(gamma)")

a <- ggplot() +
  geom_sf(data = coast, fill = "skyblue1") +
  geom_tile(data = filter(dat, parstr == "alpha"), aes(lon, lat, fill = Mean)) + 
  scale_fill_gradient(low = "#EDFAED", high = "#228B22", na.value = "transparent") +
  theme_void() +
  labs(fill="alpha")

A <- ggplot() +
  geom_sf(data = coast, fill = "skyblue1") +
  geom_tile(data = filter(dat, parstr == "A"), aes(lon, lat, fill = Mean)) + 
  scale_fill_gradient(low = "#FAF3E3", high = "#B8860B", na.value = "transparent") +
  theme_void() +
  labs(fill="Alpha")

parmeans <- ggdraw() +
  draw_plot(l + theme(legend.position=c(.25,.25), legend.key.size=unit(.5, "cm")), x = 0, y = 0, width = .25, height = 1) +
  draw_plot(g + theme(legend.position=c(.25,.25), legend.key.size=unit(.5, "cm")), x = .25, y = 0, width = .25, height = 1) +
  draw_plot(A + theme(legend.position=c(.25,.25), legend.key.size=unit(.5, "cm")), x = .5, y = 0, width = .25, height = 1) +
  draw_plot(a + theme(legend.position=c(.25,.25), legend.key.size=unit(.5, "cm")), x = .75, y = 0, width = .25, height = 1)

ggsave(filename = "figures/CP_parametermap_means.png", plot = parmeans, device = NULL, path = NULL, scale = 1, width = 26, height = 14, units = "cm", dpi = 300, limitsize = TRUE)

pars <- ggdraw() +
  draw_plot(A + theme(legend.position=c(.25,.25), legend.key.size=unit(.5, "cm")), x = 0, y = .4, width = .33, height = .6) +
  draw_plot(g + theme(legend.position=c(.25,.25), legend.key.size=unit(.5, "cm")), x = .33, y = .4, width = .33, height = .6) +
  draw_plot(l + theme(legend.position=c(.25,.25), legend.key.size=unit(.5, "cm")), x = .66, y = .4, width = .33, height = .6) +
  draw_plot(b + theme(legend.position=c(.25,.25), legend.key.size=unit(.5, "cm")), x = 0, y = 0, width = .9, height = .4) +
  draw_plot_label(label = c("(a)", "(b)"), size = 15, x = c(0.025, 0.025), y = c(.965, .4), fontface = "bold")

ggsave(filename = "CP_parametermap.png", plot = pars, device = NULL, path = NULL, scale = 1, width = 16, height = 20, units = "cm", dpi = 300, limitsize = TRUE)


# Standard Deviations

l <- ggplot() +
  geom_sf(data = coast, fill = "skyblue1") +
  geom_tile(data = filter(dat, parstr == "lambda"), aes(lon, lat, fill = log(SD))) + 
  scale_fill_gradient(low = "#FFF5F0", high = "#67000D", na.value = "transparent") +
  theme_void() +
  labs(fill="ln(lambda)")

g <- ggplot() +
  geom_sf(data = coast, fill = "skyblue1") +
  geom_tile(data = filter(dat, parstr == "gamma"), aes(lon, lat, fill = log(SD))) + 
  scale_fill_gradient(low = "steelblue4", high = "white", na.value = "transparent") +
  theme_void() +
  labs(fill="ln(gamma)")

a <- ggplot() +
  geom_sf(data = coast, fill = "skyblue1") +
  geom_tile(data = filter(dat, parstr == "alpha"), aes(lon, lat, fill = SD)) + 
  scale_fill_gradient(low = "#EDFAED", high = "#228B22", na.value = "transparent") +
  theme_void() +
  labs(fill="alpha")

A <- ggplot() +
  geom_sf(data = coast, fill = "skyblue1") +
  geom_tile(data = filter(dat, parstr == "A"), aes(lon, lat, fill = SD)) + 
  scale_fill_gradient(low = "#FAF3E3", high = "#B8860B", na.value = "transparent") +
  theme_void() +
  labs(fill="Alpha")

parsd <- ggdraw() +
  draw_plot(l + theme(legend.position=c(.25,.25), legend.key.size=unit(.5, "cm")), x = 0, y = 0, width = .25, height = 1) +
  draw_plot(g + theme(legend.position=c(.25,.25), legend.key.size=unit(.5, "cm")), x = .25, y = 0, width = .25, height = 1) +
  draw_plot(A + theme(legend.position=c(.25,.25), legend.key.size=unit(.5, "cm")), x = .5, y = 0, width = .25, height = 1) +
  draw_plot(a + theme(legend.position=c(.25,.25), legend.key.size=unit(.5, "cm")), x = .75, y = 0, width = .25, height = 1)

ggsave(filename = "figures/CP_parametermap_SD.png", plot = parsd, device = NULL, path = NULL, scale = 1, width = 26, height = 14, units = "cm", dpi = 300, limitsize = TRUE)


###########################################################
###Plot observed NDVI time-series with model predictions for points of interest
###########################################################

# Get points of interest and extract "id" by intersecting with "jag_id_ras" raster

pts <- read.csv("data/focal_plots_n50_coords.csv") |> 
  mutate(geometry = str_remove_all(geometry, pattern = "[c()]")) |>
  separate_wider_delim(cols = geometry, names = c("Longitude", "Latitude"), delim = ",") |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>
  rename(Site = name)

# st_write(pts, "data/focal_plots_n50_coords.kml")

# pts <- data.frame(
#   Site = c("Miller's Point: Alien clearing", "Cape of Good Hope: Drought"),
#   Latitude = c(-34.225279, -34.316068),
#   Longitude = c(18.463551, 18.428294)
# )
# pts <- st_as_sf(pts, coords = c("Longitude", "Latitude"), crs = 4326) 

ids <- extract(jag_id_ras, pts)

###NOTE: if you dont want to do this for all pixels it will take a long time!!!###

# Filter on the the jag_id column of tdat and env
tdat <- tdat %>% filter(jag_id %in% ids[,2])
env <- env %>% filter(jag_id %in% ids[,2])

#set number of samples
nsamp <- 1000

#sample global parameters
phi_par <- sample(unlist(m[,"phi"]),nsamp)
#phi_par <- rnorm(nsamp, phi$Mean,phi$SD)
sigma_par <- sample(unlist(m[,"sigma"]),nsamp)
#sigma_par <- rnorm(nsamp, sig$Mean,sig$SD)

#make an empty df to which we can write results
new_dat = tdat[FALSE,]

# This loop models ndvi using the estimated parameters and covariate data for the points of interest

##time
#tic()

for (j in 1:nrow(env)){
  #pixel_id
  pid <- env$jag_id[j]
  
  tdat_temp <- tdat %>% filter(jag_id == pid)
  
  #get pixel pars
  alpha_temp <- unlist(m[,paste0("alpha[",pid,"]")])
  alpha <- sample(alpha_temp,nsamp)
  
  lambda_temp <- unlist(m[,paste0("lambda[",pid,"]")])
  lambda <- sample(lambda_temp,nsamp)
  
  A_temp <- unlist(m[,paste0("A[",pid,"]")])
  A <- sample(A_temp,nsamp)
  
  gamma_temp <- unlist(m[,paste0("gamma[",pid,"]")])
  gamma <- sample(gamma_temp,nsamp)
  
  # alpha_m <- alphas$Mean[which(alphas$parnum==pid)]
  # alpha_sd <- alphas$SD[which(alphas$parnum==pid)]
  # 
  # lambda_m <- lambdas$Mean[which(lambdas$parnum==pid)]
  # lambda_sd <- lambdas$SD[which(lambdas$parnum==pid)]
  # 
  # A_m <- As$Mean[which(As$parnum==pid)]
  # A_sd <- As$SD[which(As$parnum==pid)]
  # 
  # gamma_m <- gammas$Mean[which(gammas$parnum==pid)]
  # gamma_sd <- gammas$SD[which(gammas$parnum==pid)]
  # 
  # #sample
  # alpha <- pmax(0,rnorm(nsamp, alpha_m,alpha_sd))
  # lambda <- pmax(0,rnorm(nsamp, lambda_m,lambda_sd))
  # gamma <- pmax(0,rnorm(nsamp, gamma_m,gamma_sd))
  # A <- pmax(0,rnorm(nsamp, A_m,A_sd))
  
  for (i in 1:nrow(tdat_temp)){
    #sample loop
    mu <- numeric(nsamp)
    ndvi <- numeric(nsamp)
    
    for (k in 1:nsamp){
      mu[k] <- alpha[k]+gamma[k]-gamma[k]*exp(-(tdat_temp$DA[i]/lambda[k]))+
        sin((phi_par[k]+((tdat_temp$firemonth[i]-1)*3.141593/6))+6.283185*tdat_temp$DA[i])*A[k]
      
      ndvi[k] <- rnorm(1,mu[k], sigma_par[k])
    }
    #summarize samples
    upper <- quantile(ndvi,probs=0.975)
    uq <- quantile(ndvi,probs=0.75) 
    mean <- quantile(ndvi,probs=0.5)
    lq <- quantile(ndvi,probs=0.25)
    lower <- quantile(ndvi,probs=0.025)
    #write to df
    tdat_temp$mean[i] <- mean
    tdat_temp$upper[i] <- upper
    tdat_temp$lower[i] <- lower
    tdat_temp$uq[i] <- uq
    tdat_temp$lq[i] <- lq
  }
  
  #output the final results data frame
  new_dat <- bind_rows(new_dat,tdat_temp)
}

#toc()

### Plot

#Fix names
nms <- data.frame(jag_id = ids[,2], Name = pts$Site)
new_dat <- merge(new_dat, nms, by.x = "jag_id", by.y = "jag_id")

#cape point
Pcp <- ggplot(data=new_dat, aes(x=Date,y=NDVI)) +
  #geom_point() +
  geom_line(color="blue") +
  geom_ribbon(aes(ymin=lower,ymax=upper,alpha=0.1))+
  geom_ribbon(aes(ymin=lq,ymax=uq,alpha=0.1))+
  scale_x_date(date_breaks = "5 year",
               labels=date_format("%Y"),
               limits = as.Date(c('1984-01-01','2022-06-01'))) +
 # scale_y_continuous(limits=c(0,1)) +
  facet_wrap(~UIJ) +
  xlab("Date") +
  ylab("NDVI") +
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Model fit and forecast for points of interest")
#  geom_vline(xintercept = as.Date("2014-05-31")) +
#  annotate("text", label = "Fit", x = as.Date("2013-03-15"), y = 0.1) +
#  annotate("text", label = "Forecast", x = as.Date("2015-06-01"), y = 0.1)

ggsave(filename = "figures/CP_postfire_curves_points_of_interest_Lansdat.png", plot = Pcp, device = NULL, path = NULL, scale = 1, width = 36, height = 24, units = "cm", dpi = 300, limitsize = TRUE)

# Plot map
library(rosm)
library(ggspatial)

grid <- st_as_sf(as.polygons(jag_id_ras, aggregate=FALSE))
st_write(grid, "data/jag_id_grid.kml")

ggplot() +
  annotation_map_tile(type = "cartolight", progress = "none") +
  geom_sf(data = grid) +
  geom_sf(data = pts) +
  geom_sf_text(data = pts, aes(label = Site), nudge_y = -0.0025)

ggsave(filename = "figures/CP_plot_map.png", device = NULL, path = NULL, scale = 1, width = 12, height = 20, units = "cm", dpi = 300, limitsize = TRUE)

mapview::mapview(pts, zcol = "Site") +
  mapview::mapview(grid)

### Calculate Continuous Rank Probability Score (CRPS) and Mean Forecast Error (MFE) for model evaluation

library(scoringRules)
library(slider)

# hmm <- new_dat |> # This is a test to check the CRPS calculation for one point and date
#   filter(jag_id ==4, Date == as.Date("2003-04-07")) |>
#   mutate(sd = (upper - lower) / 3.92 ) |> # Convert 95% CI to SD
#   mutate(crps = crps_norm(y = NDVI, mean = mean, sd = sd))

# For each time step
crps_dat <- new_dat |>
  group_by(UIJ, Date)  |>
  mutate(sd = (upper - lower) / 3.92 ) |> # Convert 95% CI to SD
  mutate(crps = crps_norm(y = NDVI, mean = mean, sd = sd)) |> # Calculate CRPS for each prediction
  mutate(crps_cum_avg = cummean(crps)) |> # Cumulative average of CRPS over time
  mutate(mfe = NDVI - mean) |> # Mean Forecast Error (MFE) for each prediction
  mutate(mfe_cum_avg = cummean(mfe)) |> # Cumulative average of MFE over time
  mutate(mfe_slide_avg = slide_mean(mfe, before = 3, after = 0, step = 1))

# Annual values
crps_year <- crps_dat |>
  group_by(UIJ, Year = lubridate::year(Date)) |>
  summarise(crps = mean(crps), mfe = mean(mfe))

### Plot CRPS and MFE by year

#mfe
crps_year |> 
  ggplot(aes(x = Year, y = mfe)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  # ylim(0, 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~UIJ) +
  labs(title = "Average Mean Forecast Error (MFE: NDVI - mean) by Year")

ggsave(filename = "figures/CP_MFE_annual_Landsat.png", device = NULL, path = NULL, scale = 1, width = 36, height = 24, units = "cm", dpi = 300, limitsize = TRUE)

#crps
crps_year |> 
  ggplot(aes(x = Year, y = crps)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  # ylim(0, 0.2) +
  # geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~UIJ) +
  labs(title = "Average Continuous Rank Probability Score (CRPS) by Year")

ggsave(filename = "figures/CP_CRPS_annual_Landsat.png", device = NULL, path = NULL, scale = 1, width = 36, height = 24, units = "cm", dpi = 300, limitsize = TRUE)

### Plot CRPS and MFE over time for all timesteps

## Line graph

#mfe by date
crps_dat |> 
  ggplot(aes(x = Date, y = mfe)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
 # ylim(0, 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~UIJ) +
  labs(title = "Mean Forecast Error (MFE: NDVI - mean) for all timesteps")

ggsave(filename = "figures/CP_MFE_timestep_Landsat.png", device = NULL, path = NULL, scale = 1, width = 36, height = 24, units = "cm", dpi = 300, limitsize = TRUE)

#mfe density plots by year
crps_dat |> 
  mutate(Year = factor(year(Date), levels = 1984:2022)) |>
  ggplot(aes(x = mfe, y = Year, fill = ..x..)) +
  geom_density_ridges_gradient(quantile_lines = T, quantiles = 2) + #quantile_lines = T) +
  scale_fill_viridis_c(name = "mfe", option = "C") +
  xlim(-0.15,0.15) +
  geom_vline(xintercept = 0, linetype = "dashed") 

ggsave(filename = "figures/CP_MFE_ridges_all_Landsat.png", device = NULL, path = NULL, scale = 1, width = 10, height = 24, units = "cm", dpi = 300, limitsize = TRUE)


ridges <- crps_dat |> 
  mutate(Year = factor(year(Date), levels = 1984:2022)) |>
  ggplot(aes(x = mfe, y = Year, fill = ..x..)) +
  geom_density_ridges_gradient(quantile_lines = T, quantiles = 2) + #quantile_lines = T) +
  scale_fill_viridis_c(name = "mfe", option = "C") +
  xlim(-0.15,0.15) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #facet_wrap(~ UIJ, ncol = 10, nrow = 5, page = 10)
  facet_wrap(~ UIJ, ncol = 10, nrow = 5)
  
ggsave(filename = "figures/CP_MFE_ridges_plots_Landsat.png", device = NULL, path = NULL, scale = 1, width = 36, height = 50, units = "cm", dpi = 300, limitsize = TRUE)

#+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  # ylim(0, 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~UIJ) +
  labs(title = "Mean Forecast Error (MFE: NDVI - mean) for all timesteps")

ggsave(filename = "figures/CP_MFE_timestep_Landsat.png", device = NULL, path = NULL, scale = 1, width = 36, height = 24, units = "cm", dpi = 300, limitsize = TRUE)


#mfe cumulative average by date
crps_dat |> 
  ggplot(aes(x = Date, y = mfe_cum_avg)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  # ylim(0, 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~UIJ)

#mfe sliding average (3 previous timesteps) by date
crps_dat |> 
  ggplot(aes(x = Date, y = mfe_slide_avg)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  # ylim(0, 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~UIJ)

#mfe by age
crps_dat |> 
  ggplot(aes(x = Age, y = mfe)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~UIJ)
  #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  # ylim(0, 0.2) +
  #geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  #facet_wrap(~Name)

# crps by date
crps_dat |> 
  ggplot(aes(x = Date, y = crps)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ylim(0, 0.2) +
  #geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~Name) +
  labs(title = "Continuous Rank Probability Score (CRPS) for all timesteps")

ggsave(filename = "figures/CP_CRPS_timestep.png", device = NULL, path = NULL, scale = 1, width = 36, height = 24, units = "cm", dpi = 300, limitsize = TRUE)

# crps cumulative average by date
crps_dat |> 
  ggplot(aes(x = Date, y = crps_cum_avg)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ylim(0, 0.2) +
  #geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~Name)


# Boxplot by year
crps_dat |> 
  mutate(Year = lubridate::year(Date)) |>
  ggplot(aes(group = Year, y = crps)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  facet_wrap(~Name)
  