# download and process climate data
# use geoknife, guide found here: https://cran.r-project.org/web/packages/geoknife/vignettes/geoknife.html
# another good guide with thorough explanations is found here: https://owi.usgs.gov/R/training-curriculum/usgs-packages/geoknife-intro/index.html

library("geoknife")
library("sf")
library("sp")
library("tidyverse")

#shapefile for study area
#Here, I import a shapefile to use as the study area
#isro_boundary <- sf::st_read("./Parameterization/Parameterization data/isle_royale_boundary_buffer/isle_royale_boundary_buffer.shp") %>%
#  sf::st_buffer(dist = 10000) %>% #the study area was too small to calculate variance! 
#  # Try adding a buffer if you're getting NaN values for variance or st_dev
#  sf::st_transform(crs = "+proj=longlat +datum=WGS84") #reproject to CRS that geoknife needs

#or you can draw it using basemaps
study_area <- basemaps::draw_ext()

#"stencil" is what geoknife uses for the extent of the data
stencil <- simplegeom(as(study_area, Class = "Spatial"))

# the "fabric" is what gets "cut out" in the stencil area, in this case a raster of climate data.
# find the URL for the data from THREDDS catalog: https://cida.usgs.gov/thredds/catalog.html
# find the data you want, then find the URL under the OPENDAP link
# or you can use webdatasets = query('webdata') to get a list of all the datasets, and filter from there

# e.g.
# webdatasets = query('webdata')
# grep("prism", webdatasets@group)
#here's some PRISM data for baseline climate
# webdatasets[1]
#we can assign that directly to the fabric
# fabric <- webdata(webdatasets[1])

# here's the MACAv2-METDATA downscaled climate data, URL from the catalog
fabric <- webdata(url='https://cida.usgs.gov/thredds/dodsC/macav2metdata_daily_future')

#here's the U of Idaho METDATA historic data
# fabric <- webdata(url = 'https://cida.usgs.gov/thredds/dodsC/UofIMETDATA')

# see what variables are available -- 
# for MACA data, variables are identified by a concatenation of 
# climate variable, model, and rcp
query(fabric, 'variables')

# what climate variables do we need? Only Precipitation (pr), min temperature (tasmin), and 
# max temperature (tasmas) are needed by default
vars <- c("pr", "tasmax", "tasmin", "uas", "vas")

#which model?
model_name <- "MIROC-ESM-CHEM"
#which RCP?
rcp <- "rcp85"

#what statistics do we want? These three are what we need for the LANDIS Climate Library
summary_stats <- c("MEAN", "VARIANCE", "STD_DEV")

#find the right variables, using grep() so we can include partial matches (e.g., all the HadGEM2 models)
varList <- query(fabric, 'variables') %>%
  `[`(grep(model_name, .)) %>% #what model to use?
  `[`(grep(rcp, .)) %>% # what RCP to use?
  `[`(grep(paste(vars, collapse = "|"), .))

#we can see what times are available, but it's per variable
variables(fabric) <- varList[1]
query(fabric, 'times') #what times are available?

# set up the "knife" which tells the GeoData Portal what to do with the 
# subset data. We want the mean, variance, and std_dev (specified above), 
# averaged across the study area, 
# and there are a few other arguments to give to the remote server, specified 
# by the "knife" object:
# wait = TRUE has R wait while job is processed
# email =TRUE emails you when process is done. 
knife <- webprocess(wait = TRUE, email = "your.email@gmail.com")
query(knife, 'algorithms') #what algorithms are available?

# area grid statistics are the default, but we can change it if we  (we don't)
algorithm(knife) <- list('Area Grid Statistics (weighted)' = 
                           "gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")

#I think this is the best way to set it? The geoknife documentation isn't clear on
# if there's a better way; might be a feature in development
knife@processInputs$STATISTICS <- summary_stats #what statistics do we want?

#-------------------------------------------------------------------------------
#run a test job to see if things are working, using just one variable
variables(fabric) <- varList[1]

testjob <- geoknife(stencil, fabric, knife)

#get information on our job in process
check(testjob) #is the job happening?
running(testjob)
error(testjob)
successful(testjob)

# cancel the job if we need to
testjob <- cancel(testjob)

#extract the data from our completed job
test <- result(testjob)
head(test)
tail(test)

#-------------------------------------------------------------------------------
# now to run it for the full set of data that we need
# we just have one ecoregion for now, TODO figure out how to do several ecoregions

# I had the job fail when I did all variables at once, so let's split them up into separate jobs
# we can't submit several jobs at the same time, so we'll put them in a loop and
# wait until one job is done before starting the next one.
# This takes a variable amount of time -- sometimes 10 minutes or sometimes an hour-ish

knife@email <- "your.address@email.com" #I just replaced this so I don't get emails about other people's data

  job_results <- list()

  for(i in 1:length(varList)){
    #set the fabric for a new variable, but keep everything else the same (i.e. the stencil and knife)
    variables(fabric) <- varList[i]
    print(varList[i])
    job <- geoknife(stencil, fabric, knife)
    if(error(job)){
      break
      check(job)
    }

    job_results[[i]] <- result(job)
  }

  
#save your work!
saveRDS(job_results, file = "climate_raw.RDS")

#The data are in a long format -- not quite what we want
str(job_results[[1]]) 

#check on one of our datasets
ppt <- job_results[[1]] #ppt is in mm

#reshape the data into the format we need
job_results_reform <- job_results %>% 
  #widen data format
  map(., function(x) tidyr::pivot_wider(data = x,
                                names_from = "statistic",
                                values_from = "1")) %>%
  #create a TIMESTEP column and add some formatting junk to the end of the date. 
  # TODO figure out a better way to do this
  map(., function(x) dplyr::mutate(x, TIMESTEP = paste0(as.character(DateTime), "T00:00:00Z")))

job_results_reform[[2]]$MEAN <- job_results_reform[[2]]$MEAN - 273.15 #convert from kelvin to celsius
job_results_reform[[3]]$MEAN <- job_results_reform[[3]]$MEAN - 273.15 #convert from kelvin to celsius


#now we need to wrangle this list of data into the format needed by the climate library
# I haven't figured this out in an elegant way yet TODO


vars #remind us what the original var names were

#rewrite variables in the format the climate library needs
# this is sort of difficult using data.frames or tibbles, because 
# there are different kinds of data in each column -- so we'll do everything
# as character vectors then glue it together at the end.
var_rows <- c("#ppt",
                 "#Tmax",
                 "#Tmin",
                 "#wind_easting",
                 "#wind_northing")
                 
units_means <- c("mm/d",
           "C",
           "C",
           "m/s",
           "m/s")
units_variance <- c("mm/d^2",
                 "C^2",
                 "C^2",
                 "m/s^2",
                 "m/s^2")


ppt <- job_results_reform[[1]]

# sorry, you're on your own from here. 
# TODO automate this better
TIMESTEP <- character()
means <- character()
variances <- character()
stdev <- character()
for(i in 1:length(var_rows)){
  #the first column has timesteps but also the headers for each variable followed by a blank cell
  TIMESTEP <- c(TIMESTEP, var_rows[i], "", "TIMESTEP", job_results_reform[[i]]$TIMESTEP)
  
  means <- c(means, "", "eco1", paste0("MEAN(", units_means[i], ")"), job_results_reform[[i]]$MEAN)
  variances <- c(variances, "", "eco1", paste0("VARIANCE(", units_variance[i], ")"), job_results_reform[[i]]$VARIANCE)
  stdev <- c(stdev, "", "eco1", paste0("STD_DEV(", units_means[i], ")"), job_results_reform[[i]]$STD_DEV)
}

output_data <- cbind(TIMESTEP, means, variances, stdev)

write.table(output_data,               # Write CSV file without header
            "./LANDIS inputs/NECN files/MACA_MIROC_rcp8_5.csv",
            sep = ",",
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE) # quote = false is important! Otherwise the CL can't read the file, 
                          # but it won't be apparent looking at the data in Excel