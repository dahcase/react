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
get_modis_links = function(modis_product, tile_list, start_date, end_date, extensions = c('.hdf','.xml'), read_local = F, verbose = F){
  
  #determine the time frame
  st <- as.Date(start_date,format="%Y.%m.%d") #start date
  en <- as.Date(end_date,format="%Y.%m.%d") #end date
  ll <- seq.Date(st, en, by="1 day") #sequence of dates
  dates_queried <- format(ll,"%Y.%m.%d") #formatting queried dates
  
  #extract the list of folders
  modis_web = paste("https://e4ftl01.cr.usgs.gov/MOLT/",modis_product,"/",sep="") #URL is a constant...
  fffs = extractFolders(modis_web)
  
  #keep those within the desired range
  good_dates <- intersect(as.character(dates_queried), as.character(fffs))
  
  #url folders
  web_fld = paste0(modis_web, good_dates,'/')
  
  #for each folder, get the corrosponding list of links
  list_links = lapply(web_fld, function(x) paste0(x,extractFiles(x, tile_list, extensions, read_local = read_local, verbose)))
  
  #unlist them and return
  return(unlist(list_links))
}

#' urls: A list of urls to download, assumes modis data (e.g. something with the h##v## tile notation)
#' output_folder: base folder. New folders will be created in the format of output_folder/tile id
#' redownload: logical. Indicates whether or not a file should be redownloaded even if it already exists
#' 
download_modis = function(urls, output_folder, redownload = F){
  
  #build folder structure
  #get all the tiles
  tiles = unique(stringr::str_extract(urls, 'h[0-9][0-9]v[0-9][0-9]'))
  new_folders = file.path(output_folder, tiles)
  catcher = lapply(new_folders, function(x) dir.create(path = x, recursive = T))
  
  #create a download path
  dl_path = file.path(output_folder, stringr::str_extract(urls, 'h[0-9][0-9]v[0-9][0-9]'), basename(urls) )
  
  if(!redownload){
    dl_me = which(!file.exists(dl_path))
  }else{
    dl_me = 1:length(urls)
  }
  
  #download the files
  if(length(dl_me)>0){
    for(iii in dl_me){
      #authentication comes from the global environment
      httr::GET(urls[iii], authenticate(username, pass), write_disk(dl_path[iii]), overwrite = T)
    }
  }
  
  invisible()
}



extractFiles = function(url, tiles, extension = c('.hdf','.xml'), read_local = F, verbose = F){
  if(verbose) print(url)
  
  #load html file
  if(read_local){
    tmp = tempfile()
    download.file(url, destfile = tmp, quiet = T)
    link = read_html(tmp)
  } else{
    link = read_html(url)
  }
  
  
  
  #sanitize extensions
  extension = gsub(".", "", extension, fixed = T)
  
  #get all the a hrefs
  hrefs = html_attr(html_nodes(link, "a"), "href")
  
  #keep only those with the file extension we want
  #get a list of indicies
  exts = tools::file_ext(hrefs)
  exts = exts %in% extension
  
  hrefs = hrefs[exts]
  
  #keep only hrefs with the right tiles
  good_files = unique(unlist(lapply(tiles, function(x) grep(x, hrefs, fixed = T))))
  hrefs = hrefs[good_files]
  
  return(hrefs)
  
}

extractFolders=function(urlString) {
  htmlString=getURL(urlString)
  ret=gsub("]", "", str_replace_all(str_extract_all(htmlString, paste('DIR',".([^]]+).", '/\">',sep=""))[[1]], "[a-zA-Z\"= <>/]", ""))
  return(ret[which(nchar(ret)>0)])
}