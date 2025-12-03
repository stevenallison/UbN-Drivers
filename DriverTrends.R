

wood_weights <- drive_get("Wood_weights_native") %>% 
  read_sheet(sheet = "wtf_native_harvest", col_names=T, trim_ws = T, 
             na = c("","#N/A", "NA", "-", " ", NULL, "NULL"),
             col_types = c("cnncccnDDLLLLLncLLnLncDLLLLLLLLLcLLLLLccLcLL")) # read in class for each column but will have to change as no option for factor and many columns are lists
