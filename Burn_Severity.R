<<<<<<< HEAD
##################################################
########## Step 1:Installing packages ############
##################################################
install.packages("maptools")       # Package of tools for reading and handling spatial objects
install.packages("rgdal")          # Package for bindings for the Geospatial Data Abstraction Library
install.packages("raster")         # Package for geographic data analysis and modeling
install.packages("rgeos")          # Interface to Geometry Engine - Open Source (GEOS)
install.packages("rasterVis")      # Package for visualization methods for raster data
install.packages("RCurl")          # Provides functions to allow one to compose general HTTP requests and to fetch URIs
install.packages("devtools")       # Package tools to make Developing R Packages easier

#################################################################################
###### Step 2: Attaching packages into library and making them ready for use #### 
#################################################################################

library(maptools)		# Loads the package "maptools"
library(rgdal)			# Loads the package "rgdal"
library(raster)			# Loads the package "raster"
library(rgeos)			# Loads the package "rgeos"
library(rasterVis)		# Loads the package "rasterVis"
library(RCurl)			# Loads the package "RCurl"
library(devtools)		# Loads the package "devtools"
install_url("https://github.com/Terradue/rLandsat8/releases/download/v0.1-SNAPSHOT/rLandsat8_0.1.0.tar.gz") 
# Installs the package from the URL for Landsat8
library(rLandsat8)		# Loads the package "rLandsat8"

################################################
###### Step 3: Set your working directory ######
################################################

setwd("G:/landsat 08")

#################################################
### Step 4: Load the pre- and post-fire images###
#################################################

prefire<- "LC80010852017008LGN00" 	# assigns 'LC80010852017008LGN00' as 'prefire' in R 
postfire<- "LC80010852017056LGN00" 	# assigns 'LC80010852017056LGN00' as 'postfire' in R

lspre<- ReadLandsat8(prefire) 		# reads all images pre-fire
lspost<- ReadLandsat8(postfire) 	# reads all images post-fire

#######################################################################
#### Step 5: Correct images for Top of Atmosphere (TOA) reflectance####
#######################################################################

reflectance.jan08NIR <- ToTOAReflectance(landsat8=lspre, band="nir") 	  # corrects the NIR band from the 08 Jan 2017 and assigns the name reflectance.jan08NIR     
reflectance.jan08SWIR2 <- ToTOAReflectance(landsat8=lspre, band="swir2")  # corrects the SWIR2 band from the 08 Jan 2017 and assigns the name reflectance.jan08SWIR2     

reflectance.feb25NIR  <- ToTOAReflectance(landsat8=lspost, band="nir") 	  # corrects the NIR band from the 25 Feb 2017 and assigns the name reflectance.feb25NIR     
reflectance.feb25SWIR2 <- ToTOAReflectance(landsat8=lspost, band="swir2") # corrects the SWIR2 band from the 25 Feb 2017 and assigns the name reflectance.feb25SWIR2     

plot(reflectance.jan08NIR)  # plots the 8th Jan corrected image (band 5 - NIR)    
plot(reflectance.jan08SWIR2) 	# plots the 8th Jan corrected image (band 7 - SWIR2)
plot(reflectance.feb25NIR)	# plots the 25th Feb corrected image (band 5 - NIR)
plot(reflectance.feb25SWIR2)	# plots the 25th Feb corrected image (band 7 - SWIR2)

############################################################################################
#### Step 6: Read shapefile into r and calculate NBR for the pre- and post-fire images #####
############################################################################################

Empedrado <- readOGR("Empedrado.shp") 		# loads the shapefile
# change the coordinate system to match the same from the images #
monchique <- spTransform(Empedrado, CRS("+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# calculates pre-fire NBR
pre_fire_NBR<-(reflectance.jan08NIR-reflectance.jan08SWIR2)/(reflectance.jan08NIR+reflectance.jan08SWIR2)
pre_fire_NBR<-mask(pre_fire_NBR,monchique)# This could be done to avoid resampling
pre_fire_NBR<-crop(pre_fire_NBR,monchique)# This could be done to avoid resampling
# calculates post-fire NBR
post_fire_NBR<-(reflectance.feb25NIR-reflectance.feb25SWIR2)/(reflectance.feb25NIR+reflectance.feb25SWIR2)
post_fire_NBR<-mask(post_fire_NBR,monchique)# This could be done to avoid resampling
post_fire_NBR<-crop(post_fire_NBR,monchique)# This could be done to avoid resampling

############################################################################
################ Step 7: Calculate dNBR (delta NBR) ########################
############################################################################

# dNBR is calculated through the difference of pre and post-fire NBR
dNBR <- (pre_fire_NBR) - (post_fire_NBR)
dNBR_cropped<-dNBR

############################################################################################
##########Step 8: Cropping the image if not done in Step 6. ################################
############################################################################################
##################### Optional steps for the cropping and masking ##########################
# masks the image to the extent of the shapefile (product to be masked, shapefile used)  ###
#dNBR_masked <- mask(dNBR,monchique) # if not already done				 ###
											 ###
# crops the image to the extent of the shapefile (product to be masked, shapefile used)	 ###
dNBR_cropped <- crop(dNBR_masked,monchique) # if not already done			 ###
############################################################################################

# plots the cropped image
plot(dNBR_cropped) 

############################################################################
######### Step 9: Classify the Burn Severity map ###########################
############################################################################

# scales the dNBR map by 10^3
dNBR_scaled <- 1000*dNBR_cropped 

# sets the ranges that will be used to classify dNBR information about the ranges used, please see the NBR section on the recommended practice
NBR_ranges <- c(-Inf, -500, -1, -500, -251, 1, -251, -101, 2, -101, 99, 3, 99, 269, 4, 269, 439, 5, 439, 659, 6, 659, 1300, 7, 1300, +Inf, -1) 

# sets a classification matrix
class.matrix <- matrix(NBR_ranges, ncol = 3, byrow = TRUE)

# classification matrix is used to classify dNBR_scaled
dNBR_reclass <- reclassify(dNBR_scaled, class.matrix, right=NA)

####################################################################
############# Step 10:Building the legend ##########################
####################################################################

# builds the attribute table for the legend 
dNBR_reclass <- ratify(dNBR_reclass) 
rat <- levels(dNBR_reclass)[[1]]

# creates the text that will be on the legend
rat$legend  <- c("NA", "Enhanced Regrowth, High", "Enhanced Regrowth, Low", "Unburned", "Low Severity", "Moderate-low Severity", "Moderate-high Severity", "High Severity") 
levels(dNBR_reclass) <- rat 

#############################################################
####### Step 11:  Changing the plotted colours ##############
#############################################################

# setting the colors for the severity map
my_col=c("white", "darkolivegreen","darkolivegreen4","limegreen", "yellow2", "orange2", "red", "purple")


# plots the burn severity map with title in two lines; first line: 'Burn Severity'; second line: 'Empedrado, Chile'.
plot(dNBR_reclass,col=my_col,legend=F,box=F,axes=F, main="Burn Severity \nMonchique, Portugal \n21 August 2018 ") 

x11()

# plots the legend on the right side of the burn severity map
legend(x='bottomright', legend =rat$legend, fill = my_col, y='bottomright') 

########################################################################################
######## Estimate of the hectares of land destroyed by fire in each severity class #####
########################################################################################

####### Estimating the Area of Every burnt class in Hectares or other units when required
v<-extract(dNBR_reclass, Empedrado_shp) #Extract raster values to polygons 
v.counts <- lapply(v,table) # Get class counts for each polygon depending on the number of polygons
class.df <- as.data.frame(t(sapply(v.counts,'[',1:length(unique(dNBR_reclass)))))  # Create a data.frame where missing classes are NA
Empedrado@data<- data.frame(Empedrado@data, class.df)# Add back to polygon data
Empedrado_Attributes<-monchique@data
Empedrado_Burn_Severity<-subset(Empedrado_Attributes, select = c(NAME_0, NAME_2, X.1, X1, X2, X3, X4, X5, X6, X7))
##### The size of a landsat pixel is 30m X30m, and the area is 900sqm= 0.9 hectare ####
Empedrado_Burn_Severity$"No Data"<-Empedrado_Burn_Severity$ X.1*0.09 # multiply to get ha
Empedrado_Burn_Severity$"High Enhanced Regrowth hectares"<-Empedrado_Burn_Severity$X1*0.09
Empedrado_Burn_Severity$"Low Enhanced Regrowth hectares"<-Empedrado_Burn_Severity$X2*0.09
Empedrado_Burn_Severity$"Unburned hectares"<-Empedrado_Burn_Severity$X3*0.09
Empedrado_Burn_Severity$"Low Severity hectares"<-Empedrado_Burn_Severity$X4*0.09
Empedrado_Burn_Severity$"Moderate-Low Severity hectares"<-Empedrado_Burn_Severity$X5*0.09
Empedrado_Burn_Severity$"Moderate-High Severity hectares"<-Empedrado_Burn_Severity$X6*0.09
Empedrado_Burn_Severity$"High Severity hectares"<-Empedrado_Burn_Severity$X7*0.09

### Removing unwanted columns and saving information both as csv and as shapefile ###
Empedrado_Burn_Severity<- subset(Empedrado_Burn_Severity, select = -c(X.1, X1, X2, X3, X4, X5, X6, X7)) # To remove unwanted columns 
write.csv(Empedrado_Burn_Severity,"G:/landsat 08/Empedrado_Burn_Severity_21_Aug_2018.csv") # write the csv with values                      
monchiqueBurntHectares<-merge(Empedrado,Empedrado_Burn_Severity,by=("NAME_2")) # merge csv to shapefile
writeOGR(EmpedradoBurntHectares,"EmpedradoBurntHectares_21_Aug_2018.shp", "EmpedradoBurntHectares_21_Aug_2018", driver = "ESRI Shapefile" ) # write shapefile with burnt classes estimated in hectares
=======
##################################################
########## Step 1:Installing packages ############
##################################################
install.packages("maptools")       # Package of tools for reading and handling spatial objects
install.packages("rgdal")          # Package for bindings for the Geospatial Data Abstraction Library
install.packages("raster")         # Package for geographic data analysis and modeling
install.packages("rgeos")          # Interface to Geometry Engine - Open Source (GEOS)
install.packages("rasterVis")      # Package for visualization methods for raster data
install.packages("RCurl")          # Provides functions to allow one to compose general HTTP requests and to fetch URIs
install.packages("devtools")       # Package tools to make Developing R Packages easier

#################################################################################
###### Step 2: Attaching packages into library and making them ready for use #### 
#################################################################################

library(maptools)		# Loads the package "maptools"
library(rgdal)			# Loads the package "rgdal"
library(raster)			# Loads the package "raster"
library(rgeos)			# Loads the package "rgeos"
library(rasterVis)		# Loads the package "rasterVis"
library(RCurl)			# Loads the package "RCurl"
library(devtools)		# Loads the package "devtools"
install_url("https://github.com/Terradue/rLandsat8/releases/download/v0.1-SNAPSHOT/rLandsat8_0.1.0.tar.gz") 
# Installs the package from the URL for Landsat8
library(rLandsat8)		# Loads the package "rLandsat8"

################################################
###### Step 3: Set your working directory ######
################################################

setwd("G:/landsat 08")

#################################################
### Step 4: Load the pre- and post-fire images###
#################################################

prefire<- "LC80010852017008LGN00" 	# assigns 'LC80010852017008LGN00' as 'prefire' in R 
postfire<- "LC80010852017056LGN00" 	# assigns 'LC80010852017056LGN00' as 'postfire' in R

lspre<- ReadLandsat8(prefire) 		# reads all images pre-fire
lspost<- ReadLandsat8(postfire) 	# reads all images post-fire

#######################################################################
#### Step 5: Correct images for Top of Atmosphere (TOA) reflectance####
#######################################################################

reflectance.jan08NIR <- ToTOAReflectance(landsat8=lspre, band="nir") 	  # corrects the NIR band from the 08 Jan 2017 and assigns the name reflectance.jan08NIR     
reflectance.jan08SWIR2 <- ToTOAReflectance(landsat8=lspre, band="swir2")  # corrects the SWIR2 band from the 08 Jan 2017 and assigns the name reflectance.jan08SWIR2     

reflectance.feb25NIR  <- ToTOAReflectance(landsat8=lspost, band="nir") 	  # corrects the NIR band from the 25 Feb 2017 and assigns the name reflectance.feb25NIR     
reflectance.feb25SWIR2 <- ToTOAReflectance(landsat8=lspost, band="swir2") # corrects the SWIR2 band from the 25 Feb 2017 and assigns the name reflectance.feb25SWIR2     

plot(reflectance.jan08NIR)  # plots the 8th Jan corrected image (band 5 - NIR)    
plot(reflectance.jan08SWIR2) 	# plots the 8th Jan corrected image (band 7 - SWIR2)
plot(reflectance.feb25NIR)	# plots the 25th Feb corrected image (band 5 - NIR)
plot(reflectance.feb25SWIR2)	# plots the 25th Feb corrected image (band 7 - SWIR2)

############################################################################################
#### Step 6: Read shapefile into r and calculate NBR for the pre- and post-fire images #####
############################################################################################

Empedrado <- readOGR("Empedrado.shp") 		# loads the shapefile
# change the coordinate system to match the same from the images #
monchique <- spTransform(Empedrado, CRS("+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# calculates pre-fire NBR
pre_fire_NBR<-(reflectance.jan08NIR-reflectance.jan08SWIR2)/(reflectance.jan08NIR+reflectance.jan08SWIR2)
pre_fire_NBR<-mask(pre_fire_NBR,monchique)# This could be done to avoid resampling
pre_fire_NBR<-crop(pre_fire_NBR,monchique)# This could be done to avoid resampling
# calculates post-fire NBR
post_fire_NBR<-(reflectance.feb25NIR-reflectance.feb25SWIR2)/(reflectance.feb25NIR+reflectance.feb25SWIR2)
post_fire_NBR<-mask(post_fire_NBR,monchique)# This could be done to avoid resampling
post_fire_NBR<-crop(post_fire_NBR,monchique)# This could be done to avoid resampling

############################################################################
################ Step 7: Calculate dNBR (delta NBR) ########################
############################################################################

# dNBR is calculated through the difference of pre and post-fire NBR
dNBR <- (pre_fire_NBR) - (post_fire_NBR)
dNBR_cropped<-dNBR

############################################################################################
##########Step 8: Cropping the image if not done in Step 6. ################################
############################################################################################
##################### Optional steps for the cropping and masking ##########################
# masks the image to the extent of the shapefile (product to be masked, shapefile used)  ###
#dNBR_masked <- mask(dNBR,monchique) # if not already done				 ###
											 ###
# crops the image to the extent of the shapefile (product to be masked, shapefile used)	 ###
dNBR_cropped <- crop(dNBR_masked,monchique) # if not already done			 ###
############################################################################################

# plots the cropped image
plot(dNBR_cropped) 

############################################################################
######### Step 9: Classify the Burn Severity map ###########################
############################################################################

# scales the dNBR map by 10^3
dNBR_scaled <- 1000*dNBR_cropped 

# sets the ranges that will be used to classify dNBR information about the ranges used, please see the NBR section on the recommended practice
NBR_ranges <- c(-Inf, -500, -1, -500, -251, 1, -251, -101, 2, -101, 99, 3, 99, 269, 4, 269, 439, 5, 439, 659, 6, 659, 1300, 7, 1300, +Inf, -1) 

# sets a classification matrix
class.matrix <- matrix(NBR_ranges, ncol = 3, byrow = TRUE)

# classification matrix is used to classify dNBR_scaled
dNBR_reclass <- reclassify(dNBR_scaled, class.matrix, right=NA)

####################################################################
############# Step 10:Building the legend ##########################
####################################################################

# builds the attribute table for the legend 
dNBR_reclass <- ratify(dNBR_reclass) 
rat <- levels(dNBR_reclass)[[1]]

# creates the text that will be on the legend
rat$legend  <- c("NA", "Enhanced Regrowth, High", "Enhanced Regrowth, Low", "Unburned", "Low Severity", "Moderate-low Severity", "Moderate-high Severity", "High Severity") 
levels(dNBR_reclass) <- rat 

#############################################################
####### Step 11:  Changing the plotted colours ##############
#############################################################

# setting the colors for the severity map
my_col=c("white", "darkolivegreen","darkolivegreen4","limegreen", "yellow2", "orange2", "red", "purple")


# plots the burn severity map with title in two lines; first line: 'Burn Severity'; second line: 'Empedrado, Chile'.
plot(dNBR_reclass,col=my_col,legend=F,box=F,axes=F, main="Burn Severity \nMonchique, Portugal \n21 August 2018 ") 

x11()

# plots the legend on the right side of the burn severity map
legend(x='bottomright', legend =rat$legend, fill = my_col, y='bottomright') 

########################################################################################
######## Estimate of the hectares of land destroyed by fire in each severity class #####
########################################################################################

####### Estimating the Area of Every burnt class in Hectares or other units when required
v<-extract(dNBR_reclass, Empedrado_shp) #Extract raster values to polygons 
v.counts <- lapply(v,table) # Get class counts for each polygon depending on the number of polygons
class.df <- as.data.frame(t(sapply(v.counts,'[',1:length(unique(dNBR_reclass)))))  # Create a data.frame where missing classes are NA
Empedrado@data<- data.frame(Empedrado@data, class.df)# Add back to polygon data
Empedrado_Attributes<-monchique@data
Empedrado_Burn_Severity<-subset(Empedrado_Attributes, select = c(NAME_0, NAME_2, X.1, X1, X2, X3, X4, X5, X6, X7))
##### The size of a landsat pixel is 30m X30m, and the area is 900sqm= 0.9 hectare ####
Empedrado_Burn_Severity$"No Data"<-Empedrado_Burn_Severity$ X.1*0.09 # multiply to get ha
Empedrado_Burn_Severity$"High Enhanced Regrowth hectares"<-Empedrado_Burn_Severity$X1*0.09
Empedrado_Burn_Severity$"Low Enhanced Regrowth hectares"<-Empedrado_Burn_Severity$X2*0.09
Empedrado_Burn_Severity$"Unburned hectares"<-Empedrado_Burn_Severity$X3*0.09
Empedrado_Burn_Severity$"Low Severity hectares"<-Empedrado_Burn_Severity$X4*0.09
Empedrado_Burn_Severity$"Moderate-Low Severity hectares"<-Empedrado_Burn_Severity$X5*0.09
Empedrado_Burn_Severity$"Moderate-High Severity hectares"<-Empedrado_Burn_Severity$X6*0.09
Empedrado_Burn_Severity$"High Severity hectares"<-Empedrado_Burn_Severity$X7*0.09

### Removing unwanted columns and saving information both as csv and as shapefile ###
Empedrado_Burn_Severity<- subset(Empedrado_Burn_Severity, select = -c(X.1, X1, X2, X3, X4, X5, X6, X7)) # To remove unwanted columns 
write.csv(Empedrado_Burn_Severity,"G:/landsat 08/Empedrado_Burn_Severity_21_Aug_2018.csv") # write the csv with values                      
monchiqueBurntHectares<-merge(Empedrado,Empedrado_Burn_Severity,by=("NAME_2")) # merge csv to shapefile
writeOGR(EmpedradoBurntHectares,"EmpedradoBurntHectares_21_Aug_2018.shp", "EmpedradoBurntHectares_21_Aug_2018", driver = "ESRI Shapefile" ) # write shapefile with burnt classes estimated in hectares
>>>>>>> 1903c5fe845011d166b80bef2c1c966c5cefc03c
plot(Empedrado_Burn_Severity)