if(!dir.exists("L:/")) message("You need to have L drive mapped")

DPH_packages <- c("rmarkdown", "kableExtra", "tidyverse", "lubridate", "sf",
                   "DBI", "odbc", "formatR", "knitr", "MMWRweek",
                  "scales", "english", "flextable", "slider",
                  "stringdist", "knitr", "rmarkdown", "yaml")

quiet_load <- function(x) {
  suppressPackageStartupMessages(library(x,
                                         lib.loc = "l:/newlib/",
                                         logical.return = TRUE,
                                         character.only = TRUE,
                                         warn.conflicts = FALSE,
                                         quietly = TRUE,
                                         attach.required = TRUE))
}

sapply(DPH_packages, quiet_load)

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

table_to_df <-
  function(table_name = NULL) {

    ### error checking
    if(is.null(table_name)) stop("You must specify a table in the database")
    sql_table_name <- SQL("DPH_COVID_IMPORT.dbo.report_summary")

    ### main code
    epi_connect <- DBI::dbConnect(odbc::odbc(), "epicenter")

    # build the SQL string
    fqdbn <- paste0("select * FROM [DPH_COVID_IMPORT].[dbo].[",
                    table_name,
                    "] ")

    results <- DBI::dbGetQuery(epi_connect,
                               statement = fqdbn)

    odbc::dbDisconnect(epi_connect)
    return(results)
  }


message("All set! The usual packages and functions are loaded")
