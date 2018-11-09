# ToDo (data preparation): 

# 1. Download MOD13Q1 data at https://lpdaacsvc.cr.usgs.gov/appeears/
# 2. Create two folders, where you will store all data, one for the EVI files and one for the Pixel Reliability files,
# e.g. C:/Data_EVI and C:/Data_Pixel_Reliability.
# 3. In both folders for each day of the year (DOY) create one subfolder. The name of each folder must start with "DOY_", e.g. DOY_033.
# 4. Rename the files by addding a prefix following the pattern DOY_YYYY_, e.g. 033_2001 or 001_2005. 
# Total Commander is a useful tool to rename multiple files. (Download Link: http://www.ghisler.com/index.htm)
# Renaming the files is important to automatize the filenames and the titles of the resulting maps.
# 6. Sort the EVI and Pixel Reliability data according to the DOY in the respective folders
# 7. Create another subfolder within the EVI data folder called "shape". Store the shapefile with the country border here.
# 8. Adjust the capture of the resulting jpg map to fit your area of interest in line 116

# installing relevant packages
install.packages("raster") # you only have to do this once
install.packages("rgdal")  # you only have to do this once
install.packages("sp")     # you only have to do this once

library(raster)
library(rgdal)
library(sp)

# load borders 
border <- shapefile("C:/User/MOD13Q1/Data/shape/gadm36_GTM_0.shp") # ToDo: insert link to the shapefile with the country borders
# download country borders as shapefiles: http://www.gadm.org/download

path <- "C:/User/MOD13Q1/Data/Data_EVI" # ToDo: enter link to the folder where you have stored the MODIS EVI data
dlist <- dir(path,pattern="DOY")

path_c <- "C:/User/MOD13Q1/Data/Data_Pixel_Reliability" # ToDo: enter link to the folder where you have stored the MODIS Pixel Reliability data
dlist_c <- dir(path_c,pattern="DOY")

path_jpg <- "C:/User/MOD13Q1/Data/SVI_Maps_Guatemala_jpg" # ToDo: enter the link to the folder where you want to store the resulting .jpg-images.
path_tif <- "C:/User/MOD13Q1/Data/SVI_Maps_Guatemala_jpg" # ToDo: enter the link to the folder where you want to store the resulting .tif-files.

pb <- txtProgressBar (min=0, max=length(dlist), style=1) # this creates a progress bar in the Console, 
# which ends at the end of the loop. The proegress bar looks like this: =========
setTxtProgressBar (pb, 0)

for (i in 1:length(dlist)) {                   # start of the outer for-loop
  fold <- paste(path,dlist[i],sep="/")         # the respective DOY folder of the Data_EVI folder
  fls <- dir(fold,pattern=".tif")              # all files that are available in the respective EVI DOY folder
  flsp <-paste(fold,fls,sep="/")               # all files that are available in the respective EVI DOY folder with complete path name
  
  evistack <- stack(flsp)                      # creates a layer stack of all files within the EVI DOY folder
  eviresize<- crop(evistack,border)            # resizes the EVI layer stack to the rectangular extent of the border shapefile
  evimask<-mask(eviresize,border)              # masks the EVI layer stack using the border shapefile
  evi<-evimask*0.0001                          # rescaling of MODIS EVI data
  evi[evi==-0.3]<-NA                           # EVI fill value(-0,3) in NA 
  evi[evi<(0)]<-NA                             # as we are only interested in vegetation valid EVI range is 0 to 1 and all EVI values smaller than 0 set to NA
  
  fold_c <- paste(path_c,dlist_c[i],sep="/")   # the respective DOY folder of the Data_Pixel_Reliability folder
  fls_c <- dir(fold_c,pattern=".tif")          # all files that are available in the respective Pixel Reliability DOY folder
  flsp_c <-paste(fold_c,fls_c,sep="/")         # all files that are available in the respective Pixel Reliability DOY folder with complete path name
  
  cloudstack <- stack(flsp_c)                  # creates a layer stack of all files within the Pixel Relaibility DOY folder
  cloudresize<- crop(cloudstack,border)        # resizes the Pixel Reliability layer stack to the rectangular extent of the border shapefile
  cloudmask<-mask(cloudresize,border)          # masks the Pixel Reliability layer stack using the border shapefile
  cloudmask[cloudmask==(3)]<-NA                # Pixel Reliability rank 3 pixels (cloudy) set to NA
  cloudmask[cloudmask==(2)]<-NA                # Pixel Reliability rank 2 pixels (snow&ice) set to NA  
  cloudmask[cloudmask==(0)]<-1                 # Pixel Reliability rank 0 pixels (good quality) set to 1
  cloudmask[cloudmask>(3)]<-NA                 # as valid Pixel Reliability range is -1 to 3, all Pixel Reliability values >3 set to NA
  # (as -1 rank pixels show value 255)
  
  evi_c=evi*cloudmask                          # multiplying the EVI layer stack by the Pixel Reliability layer stack
  # to get one single layer stack with applied cloud mask
  
  # extracting mean and standard deviation for each pixel
  evimean <- stackApply (evi_c, rep (1, nlayers (evi)), mean, na.rm=T) #calculating the mean for the layer stack for each individual pixel
  evisd <- stackApply (evi_c, rep (1, nlayers (evi)), sd, na.rm=T) #calculating the standard deviation for the layer stack for each individual pixel
  
  
  # if na.rm is TRUE NA values are ignored, otherwise an NA value in any of the arguments will cause a value of NA to be returned,
  # https://stat.ethz.ch/R-manual/R-devel/library/base/html/Extremes.html
  
  SVI_all <- ((evi_c-evimean)/evisd) #calculating SVI
  
  
  # define breaks and color scheme
  
  stdev <- cellStats(SVI_all, stat='sd')                              # calculating the standard deviation for each layer of the raster data,
  # returning as many values as number of input layers
  stdev2 <- 2*stdev                                                   # 2 standard deviations, returning as many values as number of input layers
  stdev15 <- 1.5*stdev                                                # 1.5 standard deviations, returning as many values as number of input layers
  
  breaksSVI <- c(-4, -stdev2, -stdev15, -stdev, stdev, stdev15, stdev2, 4) # definition of the breaks of the resulting maps based on statistics
  # (standard deviation), returning as many values as number of input layers
  # times 6 (i.e. 6 values for each of the 15 layers) plus 2
  # (the -4 in the beginning and 4 in the end)
  
  l <- nlayers(SVI_all)
  x <- seq(2, 6*l+1, l)
  
  colorsSVI<-c('#4C0E0D', '#E72223', '#F19069', '#F9F6C6', '#64B14B', '#04984A', '#00320E') # definition of the color scheme of the resulting maps
  
  fold_jpg <- paste(path_jpg)                                         # the respective folder where you want to store the resulting .jpg-images.
  fold_tif <- paste(path_tif)                                         # the respective folder where you want to store the resulting .tif-files.
  
  
  for (k in 1:nlayers(SVI_all)) {     # start of the inner for-loop
    
    
    year <- substr(fls[k],5,8)        # extracting the fifth to eigths letter of the filename, which is the year (cf. data preparation above)
    doy <- substr(fls[k],1,3)         # extracting the first to third letter of the filename, which is the DOY (cf. data preparation above)
    
    
    # writeRaster(evi[[k]], filename=paste(fold,"/",doy,"_",year,sep=""), format="ENVI", datatype='FLT4S', overwrite=TRUE)
    # in case you would like to have Envi files (Attention: note the datatype)
    
    jpeg(filename=paste(fold_jpg,"/",doy,"_",year,".jpg",sep=""), quality = 100) 
    # writes the jpg maps and names the files autmatically according to the pattern DOY_YYYY
    
    
    plot(SVI_all[[k]], # zlim=c(0,100), 
         breaks=c(-4, breaksSVI[x], 4),                                # sets the breaks as defined above
         col=colorsSVI,                                                # sets the colors as defined above
         main=paste("SVI"," (EVI)"," sample ",doy," ",year,sep="")) # automizes the title of the plot.
    # ToDo: Adjust the file naming according to the data you are processing!
    # E.g. if you base your SVI on EVI data, write (EVI)
    
    
    dev.off()
    
    
    writeRaster(SVI_all[[k]], filename=paste(fold_tif,"/",doy,"_",year,".tif",sep=""), format="GTiff", overwrite=TRUE)
    # writes the geotiff and automizes the file naming according to the pattern DOY_YYYY
    
  } # end of the inner for-loop
  
  
  setTxtProgressBar (pb, i)
  
}   # end of the outer for-loop