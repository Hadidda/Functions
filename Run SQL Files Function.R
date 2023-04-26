#Create Loop to run SQL scripts ####

#Variables required####

#current_branch - pull current github branch
#con - database connection


#Input x is table Requirements####
#FILE - File name ex. Test.sql
#Location - file folder location

#Recommended Items ####
#PROD Order - order to run files in
#If on dev branch DEV Order - Order to run files in
#REPO Subfolder - used to concatenate subfolder in repo onto repo folder/file name to get full location


QUERY_RUNNING <- function(x) {
  
  
  tryCatch(
    {
      
      message(paste0("Starting",x$FILE,"at",Sys.time()))  
      
      #Check if prod or dev
      
      if(current_branch == "main")
        
      { #Start Prod Processing####
        
        #Capture starting time of each script
        start_time <- Sys.time()
        
        #Check if table exists
        
        if(dbExistsTable(con,((read_file(x$Location) %>%
                                str_split(.,"(?<=;)"))[[1]][1] %>%
                               str_split(.,"(?<=[.])")) [[1]][2] %>%
                         str_remove(., ";")
        ) 
        == TRUE) #End Query
          
        {
          
          #Run truncate/append if table exists
          
          
          #Truncate Data
          
          dbSendQuery(con,
                      (read_file(x$Location) %>%
                         str_split(.,"(?<=;)"))[[1]][1] %>%
                        str_remove(., ";")
          ) #End Query
          
          #Append Data
          dbSendQuery(con,
                      (read_file(x$Location) %>%
                         str_split(.,"(?<=;)"))[[1]][2] %>%
                        str_remove(., ";")
          ) #End Query
          
          #If the queries run successfully create a data frame output with them being successful
          
          data.frame(
            File = x$FILE, 
            Run_Time = paste0(round(Sys.time() - start_time,2),"Min(s)"), #Calculate the run time of each script 
            Status = "Successful", 
            Error = "successful", 
            Finished_Time = paste0(Sys.time())
          )
          
          #End truncate/append step
          
        }else{   #Start table does not exist section
          
          data.frame(
            File = x$FILE, 
            Run_Time = paste0(round(Sys.time() - start_time,2),"Min(s)"), #Calculate the run time of each script 
            Status = "Table Doesn't exist", 
            Error = "Table Doesn't exist", 
            Finished_Time = paste0(Sys.time())
          )
          
        }
        
        message(paste0(x$FILE," ended ","at ",Sys.time()))
        
        #End Prod processing
        
      }else{  
        
        #Start DEV Processing####
        
        #Capture starting time of each script
        start_time <- Sys.time()
        
        #Check if table exists
        
        if(dbExistsTable(con,
                         #Add Dev to the front of the table name
                         paste0("DEV_", 
                                #Add Ending of current branch name to table name
                                na.omit(current_branch %>% 
                                        str_sub(.,-3-1))%>%
                                str_remove(.,"-"),#Remove Dash from branch name
                                #Adding Table Name
                                ((read_file(x$Location) %>%
                                  str_split(.,"(?<=;)"))[[1]][1] %>%
                                 str_split(.,"(?<=[.])")) [[1]][2] %>%
                                str_remove(., ";"))
        ) #End dbexist check
        == TRUE) #End If Check
        
        {
          
          #Run Trunc/Append if table exists
          
          dbSendQuery(con,
                      paste0(((
                        #Pull drop table XABBOTT_RPT.
                        read_file(x$Location) %>%
                          str_split(.,"(?<=;)"))[[1]][1] %>%
                          str_split(.,"(?<=[.])")) [[1]][1], 
                        #Append DEV_ to the front of table name
                        "DEV_",  
                        #Pull Table name
                        ((read_file(x$Location) %>%
                            str_split(.,"(?<=;)"))[[1]][1] %>%
                           str_split(.,"(?<=[.])")) [[1]][2] %>%
                          str_remove(., ";") 
                      ) 
          ) #End Trunc Query
          
          dbSendQuery(con,
                      paste0(((read_file(x$Location) %>%
                                 str_split(.,"(?<=;)"))[[1]][2] %>%
                                str_split(.,"(?<=[.])", n =2)) [[1]][1] %>%
                               str_remove(., ";"), #Pull Create table XABBOTT_RPT.
                             "DEV_", #Append DEV_ in front of table name
                             #pull table name/rest of query
                             ((read_file(x$Location) %>%
                                 str_split(.,"(?<=;)"))[[1]][2] %>%
                                str_split(.,"(?<=[.])", n =2)) [[1]][2] %>%
                               str_remove(., ";") 
                      ) 
          ) #End Append Query 
          
          #If the queries run successfully create a data frame output with them being successful
          
          data.frame(
            File = x$FILE, 
            Run_Time = paste0(round(Sys.time() - start_time,2),"Min(s)"), #Calculate the run time of each script
            Status = "Successful", 
            Error = "successful", 
            Finished_Time = paste0(Sys.time())
          )
          #End Trunc/Append step
          
        }else{   #Start table doesn't exist
          
          data.frame(
            File = x$FILE, 
            Run_Time = paste0(round(Sys.time() - start_time,2), "Min(s)"), #Calculate the run time of each script
            Status = "Table Doesn't Exist", 
            Error = "Table Doesn't Exist", 
            Finished_Time = paste0(Sys.time())
          )
          
        } #End Create Step
        
        message(paste0(x$FILE," ended ","at ",Sys.time()))
        
        
      } #End Prod/Dev processing
      
    } #End try catch non error section
    ,
    
    #Create error handling procedure####
    
    error = function(e){
      
      
      #Message Error
      
      message(paste0(e))
      
      #On error create data frame with file name,status,error, and date 
      
      data.frame(
        File = x$FILE,
        Run_Time = paste0(round(Sys.time() - start_time,2),"Min(s)"), #Calculate the run time of each script
        Status = "Failed",
        Erorr = paste0(e),
        Finished_Time = paste0(Sys.time()))
      
    }) #End Try Catch
  
}