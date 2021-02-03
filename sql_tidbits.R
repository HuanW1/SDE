df_to_table <- 
  function(df_name, 
           table_name = NULL,
           overwrite = FALSE, 
           append = TRUE) {
    
    ### error checking
    if(missing(df_name)) stop("You must specify a dataframe")
    if(!exists(deparse(substitute(df_name))))  stop("Can not find that dataframe")
    if(is.null(table_name)) stop("You must specify a table in the database")
    
    ### main code
    epi_connect <- DBI::dbConnect(odbc::odbc(), "epicenter")
    # table_id <- DBI::Id(catalog = "DPH_COVID_IMPORT", 
    #                     schema = "dbo", 
    #                     table = table_name)
    # DBI::dbWriteTable(epi_connect, 
    #                   table_id, 
    #                   df_name, 
    #                   overwrite = overwrite, 
    #                   append = append)
    DBI::dbWriteTable(epi_connect, SQL("DPH_COVID_IMPORT.dbo.ODP_state_Result"), df_name, append = TRUE)
    odbc::dbDisconnect(epi_connect)
  }


df_to_table(gary_state_file, "ODP_state_result_new", overwrite = TRUE, append = FALSE)
