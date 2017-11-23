setup_lstday = function(){
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
  
  assign('qa_codes', qa_codes, envir = .GlobalEnv)
  assign('variable', var, envir = .GlobalEnv)
  assign('modis_base', modis_base, envir = .GlobalEnv)
  assign('scaling_factors', scaling_factors, envir = .GlobalEnv)
  assign('val_layer', val_layer, envir = .GlobalEnv)
  assign('qa_layer', qa_layer, envir = .GlobalEnv)
  
  
}
