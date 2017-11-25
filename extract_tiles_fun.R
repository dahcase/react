#extract layers
#' extract_layer
#' @param hdf_file hard drive location of the modis hdf file
#' @param slices vector. Indices or names of the slices to pull out of the hdf
#' @return A list of raster objects corrosponding to the slices
extract_layer = function(hdf_file, slices){
  
  #Get the metadata from the file
  metadat = MODIS::getSds(hdf_file)
  
  #convert names to slices
  if(class(slices)=='character'){
    slices = which(metadat$SDSnames %in% slices)
  }
  
  #extract the layers
  layers = lapply(metadat$SDS4gdal[slices], raster)
  names(layers) = metadat$SDSnames[slices]
  
  return(layers)
  
}

#' apply_qa
#' @param val_layer Raster*. Rasterlayer containing the data raster
#' @param qa_layer Raster*. Rasterlayer where the values are QA bits
#' @param qa_mask numeric. Which values in the QA layer should be applied to the val_layer
#' @param inverse logical. If F, qa_mask values are set to NA. If T, all values not in qa_mask are set to NA
#' @return A rasterlayer with masked for qa
apply_qa = function(val_layer, qa_layer, qa_mask, inverse = F){
  
  #make sure they are compatible
  stopifnot(raster::compareRaster(val_layer, qa_layer))
  
  if(!inverse){
    val_layer[which(as.vector(qa_layer) %in% qa_mask)] <- NA
  } else{
    val_layer[which(!as.vector(qa_layer) %in% qa_mask)] <- NA
  }
  
  return(val_layer)
  
}

#' process image
#' @param hdf_file File path to a modis hdf file
#' @param val_layer_id Numeric or character vector. If numeric, slice id of the value layer extract; otherwise, the name of the slice
#' @param qa_layer_id Numeric or character vector. If numeric, slice id of the qa layer extract; otherwise, the name of the slice
#' @param qa_mask numeric. Which values in the QA layer should be applied to the val_layer
#' @param inverse logical. If F, qa_mask values are set to NA. If T, all values not in qa_mask are set to NA
#' @param scaling_factors numeric. set up as slope (a) and intercept (b), if NULL, no scaling done,
#' @param output_projection crs. Destination projection
#' @param output_filepath file path. If !NULL, where should the output raster be saved
#' @param return_raster logical. If output_filepath is !NULL, should the raster that was saved also be returned in R
#' 
process_image = function(hdf_file, val_layer_id, qa_layer_id = NULL, qa_mask = NULL, inverse = F, scaling_factors = NULL, output_projection = NULL, output_filepath = NULL, return_raster = F){
  
  #input checks
  if(!is.null(qa_layer_id) & is.null(qa_mask)){
    stop('Provided qa_layer_id, but no guidence on how to apply QA')
  }
  
  if(!file.exists(hdf_file)){
    stop(paste(hdf_file, "doesn't exist"))
  }
  
  #open hdf file and extract layers
  if(!is.null(qa_layer_id)){
    slices = c(val_layer_id, qa_layer_id)
  }else{
    slices = val_layer_id
  }
  ras = extract_layer(hdf_file, slices = slices)
  
  
  #apply qa if requested
  if(!is.null(qa_layer_id)){
    ras = apply_qa(val_layer = ras[[val_layer_id]], qa_layer = ras[[qa_layer_id]], qa_mask = qa_mask, inverse = inverse)
  }else{
    ras = ras[[1]] #get it out of the higher list
  }
  
  #if scaling is requestion
  if(!is.null(scaling_factors)){
    ras = scaling_factors[2] + scaling_factors[1] * ras
  }
  
  #reproject if requested
  if(!is.null(output_projection)){
    ras = raster::projectRaster(ras, crs = output_projection)
  }
  
  #save if requested
  if(!is.null(output_filepath)){
    writeRaster(ras, filename = output_filepath, overwrite = T)
    
    if(return_raster){
      return(ras)
    } else{
      return(invisible())
    }
    
  }
  
  return(ras)
  
}


build_MODIS_QC_table <-function(table = 'LST'){
  #Function to generate MODIS QC  flag table
  #Author: Benoit Parmentier (with some lines from S.Mosher)
  #Date: 09/16/2013
  #Some of the inspiration and code originates from Steve Mosher' s blog:
  #http://stevemosher.wordpress.com/2012/12/05/modis-qc-bits/
  
  
  ## PRODUCT 1: LST
  #This can be seen from table defined at LPDAAC: https://lpdaac.usgs.gov/products/modis_products_table/mod11a2
  #LST MOD11A2 has 4 levels/indicators of QA:
  
  ## Generate product table
  if (table =='LST'){
    QC_Data <- data.frame(Integer_Value = 0:255,
                          QA_word1 = NA,QA_word2 = NA,QA_word3 = NA,QA_word4 = NA)
    
    #convert i vals to bits and store in the data frame
    bits = lapply(QC_Data$Integer_Value, function(x) as.integer(intToBits(x)[8:1]))
    bits = data.table::data.table(do.call(what = rbind, bits))
    names(bits) = paste0('Bit', 7:0)
    bits = bits[,names(bits) := lapply(bits, as.integer)]
    
    QC_Data = cbind(QC_Data, bits)
    
    #Level 1: Overal MODIS Quality which is common to all MODIS product
    QC_Data$QA_word1[QC_Data$Bit1 == 0 & QC_Data$Bit0==0] <- "LST Good Quality"    #(0-0)
    QC_Data$QA_word1[QC_Data$Bit1 == 0 & QC_Data$Bit0==1] <- "LST Produced,Check QA"
    QC_Data$QA_word1[QC_Data$Bit1 == 1 & QC_Data$Bit0==0] <- "Not Produced,clouds"
    QC_Data$QA_word1[QC_Data$Bit1 == 1 & QC_Data$Bit0==1] <- "No Produced, check Other QA"
    
    #Level 2: Information on quality of product (i.e. LST produced, Check QA) for LST
    QC_Data$QA_word2[QC_Data$Bit3 == 0 & QC_Data$Bit2==0] <- "Good Data"
    QC_Data$QA_word2[QC_Data$Bit3 == 0 & QC_Data$Bit2==1] <- "Other Quality"
    QC_Data$QA_word2[QC_Data$Bit3 == 1 & QC_Data$Bit2==0] <- "TBD"
    QC_Data$QA_word2[QC_Data$Bit3 == 1 & QC_Data$Bit2==1] <- "TBD"
    
    #Level 3: Information on quality of of emissitivity 
    QC_Data$QA_word3[QC_Data$Bit5 == 0 & QC_Data$Bit4==0] <- "Emiss Error <= .01"
    QC_Data$QA_word3[QC_Data$Bit5 == 0 & QC_Data$Bit4==1] <- "Emiss Err >.01 <=.02"
    QC_Data$QA_word3[QC_Data$Bit5 == 1 & QC_Data$Bit4==0] <- "Emiss Err >.02 <=.04"
    QC_Data$QA_word3[QC_Data$Bit5 == 1 & QC_Data$Bit4==1] <- "Emiss Err > .04"
    
    #Level 4: Uncertaing for LST error
    QC_Data$QA_word4[QC_Data$Bit7 == 0 & QC_Data$Bit6==0] <- "LST Err <= 1"
    QC_Data$QA_word4[QC_Data$Bit7 == 0 & QC_Data$Bit6==1] <- "LST Err > 2 LST Err <= 3"
    QC_Data$QA_word4[QC_Data$Bit7 == 1 & QC_Data$Bit6==0] <- "LST Err > 1 LST Err <= 2"
    QC_Data$QA_word4[QC_Data$Bit7 == 1 & QC_Data$Bit6==1] <- "LST Err > 4"
    
    return(QC_Data)
  }
  
  ## PRODUCT 2: NDVI
  #This can be seen from table defined at LPDAAC: https://lpdaac.usgs.gov/products/modis_products_table/mod11a2
  
  if(table == 'NDVI'){
    
    
    QC_Data <- data.frame(Integer_Value = 0:65535,
                          QA_word1 = NA,QA_word2 = NA,QA_word3 = NA,QA_word4 = NA,
                          QA_word5 = NA,QA_word6 = NA,QA_word7 = NA,QA_word8 = NA,
                          QA_word9 = NA)
    
    #convert i vals to bits and store in the data frame
    bits = lapply(QC_Data$Integer_Value, function(x) as.integer(intToBits(x)[16:1]))
    bits = data.table::data.table(do.call(what = rbind, bits))
    names(bits) = paste0('Bit', 15:0)
    bits = bits[,names(bits) := lapply(bits, as.integer)]
    
    QC_Data = cbind(QC_Data, bits)
    
    
    #Level 1: Overal MODIS Quality which is common to all MODIS product
    QC_Data$QA_word1[QC_Data$Bit1 == 0 & QC_Data$Bit0==0] <- "VI Good Quality"    #(0-0)
    QC_Data$QA_word1[QC_Data$Bit1 == 0 & QC_Data$Bit0==1] <- "VI Produced,check QA"
    QC_Data$QA_word1[QC_Data$Bit1 == 1 & QC_Data$Bit0==0] <- "Not Produced,because of clouds"
    QC_Data$QA_word1[QC_Data$Bit1 == 1 & QC_Data$Bit0==1] <- "Not Produced, other reasons"
    
    #Level 2: VI usefulness (read from right to left)
    QC_Data$QA_word2[QC_Data$Bit5 == 0 & QC_Data$Bit4==0 & QC_Data$Bit3 == 0 & QC_Data$Bit2==0] <- "Highest quality, 1"
    QC_Data$QA_word2[QC_Data$Bit5 == 0 & QC_Data$Bit4==0 & QC_Data$Bit3 == 0 & QC_Data$Bit2==1] <- "Lower quality, 2"
    QC_Data$QA_word2[QC_Data$Bit5 == 0 & QC_Data$Bit4==0 & QC_Data$Bit3 == 1 & QC_Data$Bit2==0] <- "Decreasing quality, 3 "
    QC_Data$QA_word2[QC_Data$Bit5 == 0 & QC_Data$Bit4==0 & QC_Data$Bit3 == 1 & QC_Data$Bit2==1] <- "Decreasing quality, 4"
    QC_Data$QA_word2[QC_Data$Bit5 == 0 & QC_Data$Bit4==1 & QC_Data$Bit3 == 0 & QC_Data$Bit2==0] <- "Decreasing quality, 5"
    QC_Data$QA_word2[QC_Data$Bit5 == 0 & QC_Data$Bit4==1 & QC_Data$Bit3 == 0 & QC_Data$Bit2==1] <- "Decreasing quality, 6"
    QC_Data$QA_word2[QC_Data$Bit5 == 0 & QC_Data$Bit4==1 & QC_Data$Bit3 == 1 & QC_Data$Bit2==0] <- "Decreasing quality, 7"
    QC_Data$QA_word2[QC_Data$Bit5 == 0 & QC_Data$Bit4==1 & QC_Data$Bit3 == 1 & QC_Data$Bit2==1] <- "Decreasing quality, 8"
    QC_Data$QA_word2[QC_Data$Bit5 == 1 & QC_Data$Bit4==0 & QC_Data$Bit3 == 0 & QC_Data$Bit2==0] <- "Decreasing quality, 9"
    QC_Data$QA_word2[QC_Data$Bit5 == 1 & QC_Data$Bit4==0 & QC_Data$Bit3 == 0 & QC_Data$Bit2==1] <- "Decreasing quality, 10"
    QC_Data$QA_word2[QC_Data$Bit5 == 1 & QC_Data$Bit4==0 & QC_Data$Bit3 == 1 & QC_Data$Bit2==0] <- "Decreasing quality, 11"
    QC_Data$QA_word2[QC_Data$Bit5 == 1 & QC_Data$Bit4==0 & QC_Data$Bit3 == 1 & QC_Data$Bit2==1] <- "Decreasing quality, 12"
    QC_Data$QA_word2[QC_Data$Bit5 == 1 & QC_Data$Bit4==1 & QC_Data$Bit3 == 0 & QC_Data$Bit2==0] <- "Lowest quality, 13"
    QC_Data$QA_word2[QC_Data$Bit5 == 1 & QC_Data$Bit4==1 & QC_Data$Bit3 == 0 & QC_Data$Bit2==1] <- "Quality so low that not useful, 14"
    QC_Data$QA_word2[QC_Data$Bit5 == 1 & QC_Data$Bit4==1 & QC_Data$Bit3 == 1 & QC_Data$Bit2==0] <- "L1B data faulty, 15"
    QC_Data$QA_word2[QC_Data$Bit5 == 1 & QC_Data$Bit4==1 & QC_Data$Bit3 == 1 & QC_Data$Bit2==1] <- "Not useful/not processed, 16"
    
    # Level 3: Aerosol quantity 
    QC_Data$QA_word3[QC_Data$Bit7 == 0 & QC_Data$Bit6==0] <- "Climatology"
    QC_Data$QA_word3[QC_Data$Bit7 == 0 & QC_Data$Bit6==1] <- "Low"
    QC_Data$QA_word3[QC_Data$Bit7 == 1 & QC_Data$Bit6==0] <- "Average"
    QC_Data$QA_word3[QC_Data$Bit7 == 1 & QC_Data$Bit6==1] <- "High"
    
    # Level 4: Adjacent cloud detected
    QC_Data$QA_word4[QC_Data$Bit8==0] <- "No"
    QC_Data$QA_word4[QC_Data$Bit8==1] <- "Yes"
    
    # Level 5: Atmosphere BRDF correction performed
    QC_Data$QA_word5[QC_Data$Bit9 == 0] <- "No"
    QC_Data$QA_word5[QC_Data$Bit9 == 1] <- "Yes"
    
    # Level 6: Mixed Clouds
    QC_Data$QA_word6[QC_Data$Bit10 == 0] <- "No"
    QC_Data$QA_word6[QC_Data$Bit10 == 1] <- "Yes"
    
    #Level 7: Land/Water Flag (read from right to left)
    QC_Data$QA_word7[QC_Data$Bit13==0 & QC_Data$Bit12 == 0 & QC_Data$Bit11==0] <- "Shallow Ocean"
    QC_Data$QA_word7[QC_Data$Bit13==0 & QC_Data$Bit12 == 0 & QC_Data$Bit11==1] <- "Land"
    QC_Data$QA_word7[QC_Data$Bit13==0 & QC_Data$Bit12 == 1 & QC_Data$Bit11==0] <- "Ocean coastlines and lake shorelines"
    QC_Data$QA_word7[QC_Data$Bit13==0 & QC_Data$Bit12 == 1 & QC_Data$Bit11==1] <- "Shallow inland water"
    QC_Data$QA_word7[QC_Data$Bit13==1 & QC_Data$Bit12 == 0 & QC_Data$Bit11==0] <- "Ephemeral water"
    QC_Data$QA_word7[QC_Data$Bit13==1 & QC_Data$Bit12 == 0 & QC_Data$Bit11==1] <- "Deep inland water"
    QC_Data$QA_word7[QC_Data$Bit13==1 & QC_Data$Bit12 == 1 & QC_Data$Bit11==0] <- "Moderate or continental water"
    QC_Data$QA_word7[QC_Data$Bit13==1 & QC_Data$Bit12 == 1 & QC_Data$Bit11==1] <- "Deep ocean"
    
    # Level 8: Possible snow/ice
    QC_Data$QA_word8[QC_Data$Bit14 == 0] <- "No"
    QC_Data$QA_word8[QC_Data$Bit14 == 1] <- "Yes"
    
    # Level 9: Possible shadow
    QC_Data$QA_word9[QC_Data$Bit15 == 0] <- "No"
    QC_Data$QA_word9[QC_Data$Bit15 == 1] <- "Yes"
    
    return(QC_Data)
  }
  
  
  return(NULL)
}
