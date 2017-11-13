#' modis_grid: shapefiles of modis grid.
#' reg_outline: file name or sf object of processing region with extent
#' Outputs: list of modis tiles in the hxxvxx format eg h09v06
get_modis_tiles_list <-function(modis_grid,reg_outline){
  
  #load shapefile determining regions
  if((class(reg_outline)[1]!="sf")){
    reg_outline <- st_read(reg_outline)
  }
  
  #load modis grid
  modis_grid<-st_read(infile_modis_grid)
  
  #make sure they are in the same projection
  reg_outline_sin <- st_transform(reg_outline,st_crs(modis_grid)$proj4string)
  
  #intersect and convert to data frame
  l_poly <- st_intersects(reg_outline_sin,modis_grid) #intersected poly
  modis_grid_selected <- modis_grid[unlist(l_poly),]
  df_tmp <- as.data.frame(modis_grid_selected)
  
  #now format...
  #format()
  tiles_modis <- paste(sprintf("h%02d", df_tmp$h),sprintf("v%02d", df_tmp$v),sep="")
  tiles_modis <- paste(tiles_modis,collapse=",")
  
  return(tiles_modis)
}


#' modis_product A character string listing the modis product
#' tile_list A character vector of the spatial tiles from MODIS to be downloaded
#' start_date A character string, in the form of YYYY.MM.DD
#' end_date Character string. from of YYYY.MM.DD
#' output_path file.path. 
get_modis_links = function(modis_product, tile_list, start_date, end_date, output_path, redownload = F){
  
  #determine the time frame
  st <- as.Date(start_date,format="%Y.%m.%d") #start date
  en <- as.Date(end_date,format="%Y.%m.%d") #end date
  ll <- seq.Date(st, en, by="1 day") #sequence of dates
  dates_queried <- format(ll,"%Y.%m.%d") #formatting queried dates
  
  #extract the list of folders
  modis_web = paste("https://e4ftl01.cr.usgs.gov/MOLT/",MODIS_product,"/",sep="") #URL is a constant...
  fffs = extractFolders(modis_web)
  
  #keep those within the desired range
  good_dates <- intersect(as.character(dates_queried), as.character(fffs))
  
  #url folders
  web_fld = paste0(modis_web, good_dates,'/')
  
  #for each folder, get the corrosponding list of links
  list_links = lapply(web_fld, extractFiles)
  
  
  
}

extractFiles = function(url,modis_product, extension = c('.hdf','.xml')){
  link = getURL(url)
  
  
  
  
}

extractFolders=function(urlString) {
  htmlString=getURL(urlString)
  ret=gsub("]", "", str_replace_all(str_extract_all(htmlString, paste('DIR',".([^]]+).", '/\">',sep=""))[[1]], "[a-zA-Z\"= <>/]", ""))
  return(ret[which(nchar(ret)>0)])
}
