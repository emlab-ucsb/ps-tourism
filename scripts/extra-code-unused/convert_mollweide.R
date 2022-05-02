#Note that there are two functions here

#Function for converting a dataframe into mollweide
#Author: Reniel Cabral
#Jan 30, 2021

ConvertMollweide <- function(eez, reference) {
  
  #convert coordinate system to mollweide
  eez$probability<-1
  raster_test <- eez %>% 
    dplyr::select(lon, lat, probability)%>% 
    raster::rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") %>% 
    raster::projectRaster(crs = "+proj=moll") 
  mollfile_1<-raster_test %>% 
    as.data.frame(xy = T) %>% 
    filter(!is.na(probability)) %>%
    set_names(c("lon", "lat", "probability")) 
  
  highreseezgrid2<-left_join(reference,mollfile_1,by=c("lat","lon"))
  highreseezgrid2$probability[is.na(highreseezgrid2$probability)] <- 0
  
  #if 0, then they are not aligning
  if (sum(highreseezgrid2$probability)>0){
    return(mollfile_1)
  } else {
    #if not alligning, MATCH highreseezgrid2 to our working coordinates
    # Latest version: Assign closest points from a second point set
    CleanCoordmegacell_mollweide<-add_rownames(reference, var = "rowname") %>% as.data.frame()
    CleanCoordmegacell_mollweide2<-CleanCoordmegacell_mollweide
    #head(CleanCoordmegacell_mollweide2)
    #require(sp)
    MPA_coord_mollweide2<-mollfile_1
    
    # promote the input lists to SpatialPointsDataFrames
    coordinates(CleanCoordmegacell_mollweide2) <- c("lon", "lat")
    coordinates(MPA_coord_mollweide2) <- c("lon", "lat")
    
    #  Define these vectors, used in the loop.
    closestSiteVec <- vector(mode = "numeric",length = nrow(MPA_coord_mollweide2))
    minDistVec     <- vector(mode = "numeric",length = nrow(MPA_coord_mollweide2))
    
    for (i in 1 : nrow(MPA_coord_mollweide2))
    {
      distVec <- spDistsN1(CleanCoordmegacell_mollweide2,MPA_coord_mollweide2[i,],longlat = FALSE)
      minDistVec[i] <- min(distVec)
      closestSiteVec[i] <- which.min(distVec)
    }
    rowname<- CleanCoordmegacell_mollweide2[closestSiteVec,]$rowname
    FinalTable = data.frame(coordinates(MPA_coord_mollweide2),MPA_coord_mollweide2$probability,
                            closestSiteVec,minDistVec,rowname)
    #head(FinalTable)
    names(FinalTable) <- c("lon","lat","probability","CloseCoordIndex","Distance","rowname")
    #dim(FinalTable)
    MPA_coord_moll_Matched<-left_join(FinalTable,CleanCoordmegacell_mollweide,by="rowname")
    MPA_coord_moll_Matched <- as.data.frame(MPA_coord_moll_Matched)
    #head(MPA_coord_moll_Matched)
    #dim(MPA_coord_moll_Matched)
    
    MPA_test<-MPA_coord_moll_Matched %>% dplyr::select(lon.y,lat.y)
    colnames(MPA_test)<-c("lon","lat")
    #head(MPA_test)
    #MPA_test$MPA<-1

    #head(MPA_test)
    #dim(MPA_test)
    MPA_test_UNIQUE<-unique(MPA_test[c("lat", "lon")])
    #dim(MPA_test_UNIQUE) #great!
    MPA_test_UNIQUE$probability<-1
    
    # #Merge with our working coordinates
    # head(CleanCoordmegacell_mollweide)
    # dim(CleanCoordmegacell_mollweide)
    # CleanCoordmegacell_MPA<-left_join(CleanCoordmegacell_mollweide,MPA_test_UNIQUE,by=c("lon","lat"))
    # head(CleanCoordmegacell_MPA)
    # dim(CleanCoordmegacell_MPA)
    # sum(CleanCoordmegacell_MPA$MPA,na.rm=T)
    # 
    # #saveRDS(MPA_test_UNIQUE, file = "/Users/ren/Documents/CODES/FoodProvision/MPA_coord_mollweide.rds")
    # 
    # #EEZs_coord<-readRDS(file="/Users/ren/Documents/CODES/FoodProvision/EEZs_coord_mollweide.rds")
    # #head(EEZs_coord) #not this
    return(MPA_test_UNIQUE)
  }}  


AlignCoordinates <- function(eez, reference) {
  
  #This is the content of the allign function
  # Latest version: Assign closest points from a second point set
  CleanCoordmegacell_mollweide<-add_rownames(CleanCoordmegacell, var = "rowname") %>% as.data.frame()
  CleanCoordmegacell_mollweide2<-CleanCoordmegacell_mollweide
  #head(CleanCoordmegacell_mollweide2)
  #require(sp)
  MPA_coord_mollweide2<-eezccoord2
  
  # promote the input lists to SpatialPointsDataFrames
  coordinates(CleanCoordmegacell_mollweide2) <- c("lon", "lat")
  coordinates(MPA_coord_mollweide2) <- c("lon", "lat")
  
  #  Define these vectors, used in the loop.
  closestSiteVec <- vector(mode = "numeric",length = nrow(MPA_coord_mollweide2))
  minDistVec     <- vector(mode = "numeric",length = nrow(MPA_coord_mollweide2))
  
  for (i in 1 : nrow(MPA_coord_mollweide2))
  {
    distVec <- spDistsN1(CleanCoordmegacell_mollweide2,MPA_coord_mollweide2[i,],longlat = FALSE)
    minDistVec[i] <- min(distVec)
    closestSiteVec[i] <- which.min(distVec)
  }
  rowname<- CleanCoordmegacell_mollweide2[closestSiteVec,]$rowname
  FinalTable = data.frame(coordinates(MPA_coord_mollweide2),#MPA_coord_mollweide2$probability,
                          closestSiteVec,minDistVec,rowname)
  #head(FinalTable)
  names(FinalTable) <- c("lon","lat","CloseCoordIndex","Distance","rowname")
  head(FinalTable)
  dim(FinalTable)
  
  head(eezccoord2)
  dim(eezccoord2)
  
  #add country code in the Final Table File
  FinalTable$CountryCode <- eezccoord2$CountryCode
  
  MPA_coord_moll_Matched<-left_join(FinalTable,CleanCoordmegacell_mollweide,by="rowname")
  MPA_coord_moll_Matched <- as.data.frame(MPA_coord_moll_Matched)
  head(MPA_coord_moll_Matched)
  #dim(MPA_coord_moll_Matched)
  
  MPA_test<-MPA_coord_moll_Matched %>% dplyr::select(Distance, CountryCode, lon.y,lat.y)
  colnames(MPA_test)<-c("Distance", "CountryCode", "lon","lat")
  dim(MPA_test)
  head(MPA_test)
  
  #select pixel with lower distance
  Final_EEZ_file<-MPA_test %>% 
    group_by(lon, lat) %>% 
    filter(Distance == min(Distance))
  
  return(Final_EEZ_file)
}