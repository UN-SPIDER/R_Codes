######l VCI computation using chunking for large countries 

#Enter the path to the a folder where you have enough free space to store your MODIS data and the resulting products
#Please do _not_ use backslashes ('\').
dataPath <- "C:/Users/Desktop/Peru"

# automatic download
downloadList<-"C:/Users/Desktop/Peru/download-list.txt"

#Quality masking: replace the '0' with a '1' to apply MODIS pixel reliability data. If you are not sure whether you should enable this option, it is recommended to leave it as is. Check the "in detail" page for more information.
cloudmask <- 1

#Change the capture of the VCI maps (jpg) according to your study area in line 224

#Setting VCI image colours 

my_palette <- colorRampPalette(c('#8B0000', '#FF4500', '#FFFF00', '#9ACD32', '#008000'))

#No need to enter more - mark the whole code (ctrl + a) and click on 'run' (or press ctrl + enter)

#############################################################
#############################################################
#############################################################
#############################################################
#############################################################

####Installing all needed packages

install.packages("zoo")
install.packages("raster")
install.packages("curl")
install.packages("rgdal")

# calling packages for use

library(zoo)
library(raster)
library(curl)
library(rgdal)

#Downloading Files (except 'VI quality control' data as they are not needed)
#If you downloaded your data from appEEARS beforehand, comment out the following seven lines 
filesl <- scan(downloadList, what='list', sep='\n')
for (d in 1:length(filesl)){
  if (grepl("_VI_Quality_",filesl[d]) == F){
    curl_download(url=filesl[d], destfile=paste0(dataPath,"/",basename(filesl[d])),quiet = T, handle = new_handle())
  }
  print(paste0('Downloading source data (Step 1 of 4): ',round(d / length(filesl) * 100, digits=2),'%'))
}


#Creating a temp-directory and setting memory limit
dir.create(paste0(dataPath,'/temp/'))
rasterOptions(tmpdir=paste0(dataPath,'/temp/'))
rasterOptions(tolerance=1)
memory.limit(80000)

#Listing up all downloaded TIF-files in the data folder
rasterData <- list.files(path=dataPath, pattern='.tif$', recursive=F, ignore.case=T, full.names=T)
rasterFiles <- basename(rasterData)

#Chunk-size for processing (in Pixel)
#You can reduce these numbers if you run into RAM-Problems early on:
#Try reducing the numbers to 1000. If you still run into RAM-problems, reduce to 500 or 250.
#Only reduce the numbers if you have memory issues ("Error: cannot allocate vector of size [...]")
chwidth <- 1500
chheight <- 1500

##################STARTING EVI/VCI
##################
##################
##################

#Listing all EVI rasters and their corresponding pixel reliability data
EVIrasterData <- rasterData[grepl('EVI',rasterData)] 
EVIqc <- rasterData[grepl('pixel_reliability',rasterData)] 


#(Automatically adjusting chunk size for small scenes if needed)
initial <- raster(EVIrasterData[1])
if (as.numeric(ncol(initial)) <= chwidth || as.numeric(nrow(initial)) <= chheight){
  chwidth <- ceiling(ncol(initial)/2)
}

#Parsing all DOY and Years from the filenames
DOYs <- unique(substr(basename(EVIrasterData),38,40))
YEARs <- unique(substr(basename(EVIrasterData),34,37))

##VCI: chunkwise calculation
#Creating output folders
dir.create(paste0(dataPath,'/VCI'))
dir.create(paste0(dataPath,'/VCIjpg'))
dir.create(paste0(dataPath,'/EVI'))

#Determining chunk-shapefile

#Loading example image from the downloaded data
exRST <- raster(EVIrasterData[1])

#Determining chunks
h        <- ceiling(ncol(exRST)/ceiling(ncol(exRST) / chwidth))
v        <- ceiling(nrow(exRST)/ceiling(nrow(exRST) / chheight)) 

#Creating shapefile for each chunk
split      <- aggregate(exRST,fact=c(h,v))
split[]    <- 1:ncell(split)
splitshp <- rasterToPolygons(split)
names(splitshp) <- 'shapes'
notiles <- ncell(split)

#Filtervalues for quality masking: "0" and "1" in the pixel reliability bands are accepted as sufficient quality
goodValues <- c(0,1)

#Masking clouds/snow; Splitting data into chunks
for (d in 1:length(DOYs)){
  #Filtering all relevant data for this DOY
  vrasterData <- EVIrasterData[grepl(paste0(DOYs[d],'_aid'),EVIrasterData)]
  #..and their corresponding pixel reliability data
  vQC <- EVIqc[grepl(paste0(DOYs[d],'_aid'),EVIqc)]
  #Reading years of available data
  vYear <- substr(basename(vrasterData),34,37)
  for (y in 1:length(vYear)){
    viPRE <- raster(vrasterData[y])
    #Applying quality mask to each image (if masking was activated)
    if (cloudmask == 1){
      qc <- raster(vQC[y])
      viPRE <- overlay(viPRE, qc, fun = function(x, y) {
        x[!(y %in% goodValues)] <- NA
        return(x)
      })
    }
    #####Splitting (masked) data into Chunks 
    for(i in 1:ncell(split)){
      ex          <- extent(splitshp[splitshp$shapes==i,])
      exx         <- crop(viPRE,ex)
      writeRaster(exx,filename=paste0(dataPath,'/temp/',DOYs[d],'_',vYear[y],'_EVICHUNK',formatC(i, width=3, flag='0')),format='GTiff', overwrite=TRUE)
    }
  }
  #(Progress report)
  print(paste0('Data preparation (VCI) & masking (Step 2 of 4): ',round(d / length(DOYs)  * 100, digits=2),'%'))
}

#Applying VCI calculation for each chunk
#List all chunks
EVIchunks <- list.files(path=paste0(dataPath,'/temp/'), pattern='_EVICHUNK', recursive=F, ignore.case=T, full.names=T)
for (d in 1:length(DOYs)){
  for (t in 1:notiles){
    #Filtering relevant chunks for this specific date
    sEVIchunks <- EVIchunks[grepl(paste0(DOYs[d],'_'),EVIchunks)] 
    sEVIchunks <- sEVIchunks[grepl(paste0('_EVICHUNK',formatC(t, width=3, flag='0')),sEVIchunks)] 
    #Listing years of data available for each DOY
    vvYear <- substr(basename(sEVIchunks),5,8)
    
    if (length(sEVIchunks) > 0){
      sT <- stack(sEVIchunks)
      
      #Removing filler-values from EVI data
      #The fill value for MODIS images is -3000  
      sT[sT<(-2999)] <- NA
      
      #VCI formula application
      vimax <- stackApply(sT , rep(1, nlayers (sT)), max, na.rm=T)
      vimin <- stackApply(sT , rep(1, nlayers (sT)), min, na.rm=T)
      z <- vimax - vimin
      VCI <- ((sT -vimin)/z)*100
      
      #Writing VCI-chunks for each available year
      for (y in 1:length(vvYear)){
        writeRaster(VCI[[y]],filename=paste0(dataPath,'/temp/',DOYs[d],'_',vvYear[y],'_VCICHUNK',formatC(t, width=3, flag='0')),format='GTiff', overwrite=TRUE)
      }
    }
  }
  #(Progress report)
  print(paste0('VCI processing (Step 3 of 4): ',round(d / length(DOYs)  * 100, digits=2),'%'))
}

##VCI chunk merging and output 
#Listing all created chunks
VCIchunks <- list.files(path=paste0(dataPath,'/temp/'), pattern='_VCICHUNK', recursive=F, ignore.case=T, full.names=T)
#Looping through each available DOY_YEAR combination
for (y in 1:length(YEARs)){
  for (d in 1:length(DOYs)){
    #Listing relevant chunks (EVI, VCI) for this specific date
    sVCIchunks <- VCIchunks[grepl(paste0(DOYs[d],'_',YEARs[y],'_'),VCIchunks)]
    sEVIchunks <- EVIchunks[grepl(paste0(DOYs[d],'_',YEARs[y],'_'),EVIchunks)] 
    #Creating raster-list of the EVI chunks
    if (length(sEVIchunks) > 0){
      if (length(sEVIchunks) > 1){
        sMos <- list()
        for (o in 1:length(sEVIchunks)){
          sMos <- append(sMos, raster(sEVIchunks[o]))
        }
        #Mosaicking the EVI chunks
        sMos$fun = mean
        Mos <- do.call(mosaic, sMos)
        Mos <- Mos*0.0001
        #Output of mosaicked, scaled EVI image
        writeRaster(Mos,filename=paste0(dataPath,'/EVI/',DOYs[d],'_',YEARs[y],'_EVI'),format='GTiff', overwrite=TRUE)
      }else{
        #Fail-safe in case only one chunk is available
        exp <- raster(sEVIchunks[1])
        exp <- exp*0.0001
        writeRaster(exp,filename=paste0(dataPath,'/EVI/',DOYs[d],'_',YEARs[y],'_EVI'),format='GTiff', overwrite=TRUE)
      }
    }
    #Creating raster-list of the VCI chunks
    if (length(sVCIchunks) > 0){
      if (length(sVCIchunks) > 1){
        sMos <- list()
        for (o in 1:length(sVCIchunks)){
          sMos <- append(sMos, raster(sVCIchunks[o]))
        }
        #Mosaicking the VCI chunks
        sMos$fun = mean
        Mos <- do.call(mosaic, sMos)
        
        #Plotting JPG of VCI
        
        jpeg(filename=paste0(dataPath,'/VCIjpg/',DOYs[d],"_",YEARs[y],".jpg",sep=""), quality = 100)
        
        plot(Mos,
             zlim=c(0,100),
             col=my_palette(101),                                           # sets the colors as defined above
             main=paste("VCI"," (EVI)"," sample ",DOYs[d]," ",YEARs[y],sep=""))  # automizes the title of the plot.
        
        dev.off()
        
        #Output: final VCI mosaic
        writeRaster(Mos,filename=paste0(dataPath,'/VCI/',DOYs[d],'_',YEARs[y],'_VCI'),format='GTiff', overwrite=TRUE)
      }else{
        #Fail-safe in case only one chunk is available
        writeRaster(raster(sVCIchunks[1]),filename=paste0(dataPath,'/VCI/',DOYs[d],'_',YEARs[y],'_VCI'),format='GTiff', overwrite=TRUE)
      }
    }
    print(paste0('EVI output & VCI output (Step 4 of 4): ',round((d+(length(DOYs)*(y-1)))/(length(DOYs)*length(YEARs))*100, digits=2),'%')) 
  }
}

#Removing temp files to free space
#tmp <- list.files(path=paste0(dataPath,'/temp/'), recursive=F, ignore.case=T, full.names=T)
#file.remove(tmp)
