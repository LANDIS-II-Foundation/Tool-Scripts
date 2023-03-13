# create one-cell landscape with specified characteristics
# This script will write a one-cell raster with the provided values,
# allowing for flexible creation of one-cell test landscapes to evaluate
# different initial communities, input raster values, etc.

# at the moment, the script doesn't touch the NECN_scenario.txt script
# so you'll have to check that the names match what's written in the scenario

# Future features should include:
# create a new directory each time the script is run (optionally)
# create a new NECN_succession.txt, species traits, etc (optionally)
# allow for custom climate to be set

library("raster")
library("tidyverse")


# this script needs there to be an NECN_succession script, a climate file,
# and a scenario script
# in the working directory. Then you can either keep everything in one directory
# or make a subdirectory to put the new model files (new_folder == TRUE). 
# The script will make all the files in that folder, copy over your succession
# file and your scenario file, and create a batchfile to run the new model.


#helper functions
#function to write one-cell rasters (or potentially larger rasters, I guess!)
write_cell <- function(name){
  if(new_folder){
    writeRaster(setValues(template, name), paste0("./", new_folder_name, raster_folder, deparse(substitute(name)), ext),
                overwrite = TRUE)} else{
                  writeRaster(setValues(template, name), 
                              paste0("./", raster_folder, deparse(substitute(name)), ext),
                              format = "GTiff",
                              overwrite = TRUE,
                              datatype = "FLT4S", 
                              NAvalue = 0)
                }
  
}

#some parameters for the script
#set WD if you want to move this to your LANDIS folder
# setwd("./Models/landis_test/")

#should we write a little readme text file to explain what the model is for? What should it say?
#consider renaming the folder as well (new_folder_name) below

write_readme <- TRUE
readme <- "upland site (shallow soil, high drainage)"


#what is the original scenario file name? (should be in the working directory)
#there will be an error about incomplete final lines, don't worry about it
scenario <- readLines("./inputs/Scenario1.txt")
batch <- readLines("./inputs/Scenario1.bat")

#what is the NECN succession file name? (should be in the working directory)


#what subfolder should the newly created rasters go in?
raster_folder <- "rasters/"
ext <- ".tif"

new_folder <- TRUE
new_folder_name <- "upland/"
if(!new_folder) new_folder_name <- ""

#make the folders if we want to
if(new_folder){
  if(dir.exists(new_folder_name)) print("Writing to existing folder. Pick a new folder name if you want a new folder.") else{
    dir.create(new_folder_name)
    dir.create(paste0(new_folder_name, raster_folder))
  }
}

#change to TRUE if you want to use a cell from an existing initial communities file
use_existing_ic <- FALSE

#ecoregion map
#just one ecoregion for one cell -- make sure that the climate you're using matches!
template <- raster(nrows = 1, ncols = 1, vals = 1) #vals should match the climate region you want to use
if(new_folder){
  writeRaster(template, 
            paste0("./", new_folder_name, raster_folder, "ecoregions", ext), 
            overwrite = TRUE,
            datatype = "INT4S",
            NAvalue = 0)
}

#variables we need:
#Total soil C
#TODO warn or adjust if soil C or soil N compartments will be too large

SOMtot <- 10000
SOM1surfC <- 0.01 * SOMtot
write_cell(SOM1surfC)
SOM1soilC <- 0.02 * SOMtot
write_cell(SOM1soilC)
SOM2C <- 0.59 * SOMtot
write_cell(SOM2C)
SOM3C <- 0.38 * SOMtot
write_cell(SOM3C)

#TODO figure out how to write all the rasters at once
#lapply(list(SOM1surfC, SOM1soilC, SOM2C, SOM3C), FUN = write_cell)

#Total soil N (or C:N ratio)
SOM1surfN <- 0.1 * SOM1surfC
SOM1surfN <- 0.5 * SOM1surfN
write_cell(SOM1surfN)
SOM1soilN <- 0.1 * SOM1soilC
SOM1soilN <- 0.5 * SOM1soilN
write_cell(SOM1soilN)
SOM2N <- 0.04 * SOM2C
SOM2N <- 0.5 * SOM2N
write_cell(SOM2N)
SOM3N <- 0.118 * SOM3C
SOM3N <- 0.5 * SOM3N
write_cell(SOM3N)

#Soil Drain
soil_drain <- 1
write_cell(soil_drain)

#Soil Depth
soil_depth <- 30
write_cell(soil_depth)

#Field capacity
field_capacity <- 0.15
write_cell(field_capacity)

#permanent wilt point
wilt_point <- 0.05
write_cell(wilt_point)

#percent sand
sand <- 0.55
write_cell(sand)

#percent clay
clay <- 0.05
write_cell(clay)

#baseflow
baseflow <- .5
write_cell(baseflow)

#stormflow
stormflow <- .9
write_cell(stormflow)



# species biomass cohorts (initial communities)
# you can put your own initial communities here, to calibrate species parameters,
# test against plantation data, etc. You can add multiple cohorts or just use one cohort.
#Just make sure there's only one MapCode for all the cohorts
init_comm <- data.frame(MapCode = 1,
                        SpeciesName = c("ABBA", "POTR5"),
                        CohortAge = c(15, 5),
                        CohortBiomass = c(2000, 3480))

#or import data from a spreadsheet
#replace the csv filename with an already existing NECN initial communities file
if(use_existing_ic){
  init_comm <- read.csv("./initial_communities_update.csv") %>%
    filter(MapCode == 29613) %>%
    select(!X)
  init_comm$MapCode <- 1
}

#write initial communities
write.csv(init_comm, paste0("./", new_folder_name, "init_comm.csv"))

initial_communities <- init_comm$MapCode[1]
if(new_folder){
  initial_comm_raster <- setValues(template, initial_communities)
  writeRaster(template, 
              paste0("./", new_folder_name, raster_folder, "initial_communities", ext), 
              overwrite = TRUE,
              datatype = "INT4S",
              NAvalue = 0)
}

#   * Dead Wood on the Surface ^3^
dead_wood <- sum(init_comm$CohortBiomass) * 0.37 #"equilibrium" ratio from long LANDIS run
write_cell(dead_wood)

#   * Dead Wood of Coarse Roots ^3^
coarse_roots <- 0.33 * dead_wood #suggestion from Zachary's scripts
write_cell(coarse_roots)

#import the text files needed to run NECN

NECN_succession <- readLines("./inputs/NECN_succession.txt")

#change/write NECN variables
#make sure the rows line up; here they're rows 30-45. 
NECN_succession[30:45] <- c("CalibrateMode yes",
                              "SmokeModelOutputs no",
                              "Version_Henne_SoilWater no",
                              "WaterDecayFunction Ratio <<Linear or Ratio",
                              "",
                              "ProbabilityEstablishAdjust 	1.0",
                              "InitialMineralN			1.0",
                              "InitialFineFuels		0.75",
                              "AtmosphericNSlope		-0.000109",
                              "AtmosphericNIntercept		0.0589",
                              "Latitude			48",
                              "DenitrificationRate		0.25 <<was 0.5",
                              "DecayRateSurf			0.88",
                              "DecayRateSOM1			0.95",
                              "DecayRateSOM2			0.02",
                              "DecayRateSOM3			0.0002")
  
write(NECN_succession, file = paste0("./", new_folder_name, "NECN_succession.txt"))
  
#make the initial communities pointer file -- make sure this matches the name 
#of the initial communities csv file
#TODO have script check the NECN_succession file to make sure it knows the name of this text file
initial_communities_pointer <- c("LandisData	\"Initial Communities\"",
                                  "CSV_File \"init_comm.csv\"")
write(initial_communities_pointer, file = paste0("./", new_folder_name, "initial_communities_pointer.txt"))

#should climate data be copied to new directory?
copy_climate <- FALSE

#TODO fix this -- when climate is getting copied, something isn't working properly
climate_filename <- "test_climate.csv"
if(new_folder & copy_climate){
  climate <- read.csv(climate_filename)
  write.table(climate,  
            paste0("./", new_folder_name, climate_filename),
            sep = ",",
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE)
}

climate_generator_filename <- "climate-generator-gridmet.txt"
climate_generator <- readLines(climate_generator_filename)
if(new_folder & copy_climate){
  climate_generator[4] <- paste0("ClimateFile \"", climate_filename, "\"")
  climate_generator[8] <- paste0("SpinUpClimateFile \"", climate_filename, "\"")
  write(climate_generator, file = paste0("./", new_folder_name, climate_generator_filename))
} else if(new_folder & !copy_climate){
  # assuming the actual climate file stays in the main working directory, not copied
  # to one level lower
  climate_generator[4] <- paste0("ClimateFile \"../", climate_filename, "\"")
  climate_generator[8] <- paste0("SpinUpClimateFile \"../", climate_filename, "\"")
  write(climate_generator, file = paste0("./", new_folder_name, climate_generator_filename))
}

#should species information be copied over?
copy_species <- TRUE
species_filename <- "./inputs/species.txt"
necn_species_filename <- "./inputs/NECN_Spp_Table.csv"
functional_filename <- "./inputs/NECN_Functional_Table.csv"
# species
if(new_folder){
  spp <- readLines(species_filename)
  necn_spp <- read.csv(necn_species_filename)
  func <- read.csv(functional_filename)
  write(spp, file = paste0("./", new_folder_name, basename(species_filename)), sep = "tab")
  write.csv(necn_spp, paste0("./", new_folder_name, basename(necn_species_filename)))
  write.csv(func, paste0("./", new_folder_name, basename(functional_filename)))
}

#copy over the scenario file
#you might have to change where this points for some things
#TODO update scenario file to point to where new files are written
writeLines(scenario, paste0("./", new_folder_name,"Scenario1.txt"))

#copy over the ecoregions file
#only one ecoregion, ecoregion 1
ecoregions <- c("LandisData	\"Ecoregions\"",		
                "yes	1	eco1		\"active\"")
writeLines(ecoregions, paste0("./", new_folder_name,"ecoregions.txt"))

#write readme (see top of script)
write(readme, file = paste0("./", new_folder_name, "readme.txt"))


#make the batchfile and optionally run it
batch <- c("call landis-ii-7 Scenario1.txt", "pause")
write(batch, file = paste0("./", new_folder_name, "Scenario1.bat"))

# shell.exec(paste0(getwd(), "/", new_folder_name, "Scenario1.bat"))