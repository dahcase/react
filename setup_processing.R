setup = function(product = 'lst'){
  if(product == 'lst'){
    var = 'LST_Day'
    modis_base = 'MOD11A2'
    scaling_factors <- c(1,-273.15) #set up as slope (a) and intercept (b), if NULL, no scaling done, setting for LST 
    
    val_layer = 1
    qa_layer = 2
    
    #qa
    qc_table = build_MODIS_QC_table('LST')
    qc_table <- subset(x=qc_table,QA_word1 == "LST Good Quality" | QA_word1 =="LST Produced,Check QA")
    #Select level 2:
    qc_table <- subset(x=qc_table,QA_word2 == "Good Data" | QA_word2 =="Other Quality")
    qa_codes = qc_table$Integer_Value
    
    md_start = '01.01'
    md_end = '12.26'
    temporal_resolution = 8
  }
  
  if(product == 'ndvi'){
    var = 'NDVI'
    modis_base = 'MOD13A2'
    scaling_factors <- c(0.0001,0) #set up as slope (a) and intercept (b), if NULL, no scaling done, setting for LST 
    
    val_layer = "1 km 16 days NDVI"
    qa_layer = "1 km 16 days VI Quality"
    
    #qa
    qc_table = build_MODIS_QC_table('NDVI')
    qc_table <- subset(x=qc_table,QA_word1 == "VI Good Quality" | QA_word1 =="VI Produced,check QA")
    #Select level 2:
    qc_table <- subset(x=qc_table,QA_word2 %in% unique(qc_table$QA_word2)[1:8]) #"Highest quality, 1","Lower quality, 2","Decreasing quality, 3",...,"Decreasing quality, 8" 
    
    qa_codes = qc_table$Integer_Value
    
    md_start = '01.01'
    md_end = '12.18'
    temporal_resolution = 16
  }
  
  assign('md_start', md_start, envir = .GlobalEnv)
  assign('md_end', md_end, envir = .GlobalEnv)
  assign('temporal_resolution', temporal_resolution, envir = .GlobalEnv)
  assign('qa_codes', qa_codes, envir = .GlobalEnv)
  assign('variable', var, envir = .GlobalEnv)
  assign('modis_base', modis_base, envir = .GlobalEnv)
  assign('scaling_factors', scaling_factors, envir = .GlobalEnv)
  assign('val_layer', val_layer, envir = .GlobalEnv)
  assign('qa_layer', qa_layer, envir = .GlobalEnv)
  
  
}

#generate possible slices by year
expand_yearlist = function(md_start, md_end, year , bydays = '8 day'){
  mds = as.Date(paste0(year,'.',md_start), format="%Y.%m.%d")
  mde = as.Date(paste0(year,'.',md_end), format="%Y.%m.%d")
  yearlist = seq.Date(mds, mde, by = bydays)
  return(yearlist)
}
