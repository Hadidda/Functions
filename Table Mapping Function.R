#Map tables from sharepoint to database ####

#Variables requried:

#Con - Connection to database
#excel_location - folder location
#Add Schema name to this formula
#current_branch - pull current git branch

mapping_tables <- function(x) {
  
  tryCatch(
    
    {
      
      if(current_branch == "main"){
        
        #read in table to load
        
        tables_to_load  <- read_excel(excel_location,
                                      sheet = x$SHEET_NAME)
        # Remove all rows
        dbGetQuery(con,
                   paste0("TRUNCATE TABLE .",
                          x$TABLE_NAME))
        
        # Append the new values
        write_orcl_db_w_dates(con,
                              tables_to_load,
                              x$TABLE_NAME,
                              append_table = T
        )
        
        message(paste0(x$TABLE_NAME," data appended"))
        
        #If the queries run successfully create a data frame output with them being successful
        
        data.frame(
          TABLE = x$TABLE_NAME, 
          Status = "Successful", 
          Error = "successful"
        )
        
        #End mapping table updates
      } else { 
        
        
        message("Not on main no table updates")
        
        data.frame(
          TABLE = "Not on main", 
          Status = "Not on main", 
          Error = "Not on main"
        )
      } #End IF
    } #End non error
    ,
    
    #Create error handling procedure####
    
    error = function(e){
      
      
      #Message Error
      
      message(paste0(e))
      
      #On error create data frame with file name,status,error, and date 
      
      data.frame(
        TABLE = x$TABLE_NAME,
        Status = "Failed",
        Erorr = paste0(e)
      )
      
    }
  ) #End Try Catch
  
} #End Table Mapping