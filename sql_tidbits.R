df_to_table <- 
  function(df_name, 
           table_name = NULL,
           overwrite = FALSE, 
           append = TRUE) {
    
    ### error checking
    if(overwrite == append)  stop("You can not append and overwrite at the same time")
    if(missing(df_name)) stop("You must specify a dataframe")
    if(!exists(deparse(substitute(df_name))))  stop("Can not find that dataframe")
    if(is.null(table_name)) stop("You must specify a table in the database")
    
    ### main code
    epi_connect <- DBI::dbConnect(odbc::odbc(), "epicenter")
    
    # build the SQL string
    fqdbn <- paste0("DPH_COVID_IMPORT.dbo.", table_name)
    
    DBI::dbWriteTable(epi_connect, 
                      SQL(fqdbn), 
                      df_name, 
                      append = append, 
                      overwrite = overwrite)
    
    odbc::dbDisconnect(epi_connect)
  }


df_to_table(gary_state_file, "ODP_state_result_new", overwrite = TRUE, append = FALSE)
