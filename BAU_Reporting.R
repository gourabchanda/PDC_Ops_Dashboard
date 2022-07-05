#-------------------------------------------- PDC Distribution list --------------------------------------------------

#check if the package is available in the session if not install and load
#Load the required packages into the project 

req_packages <- c(
     #library load
     "pacman","fs",
     #Data read package for reading and writing flat files
     "readxl","data.table","readr","writexl","openxlsx",
     #Data wrangling package 
     "magrittr","tidyverse","dplyr","Hmisc","hablar","lubridate","tidyr","janitor",
     #Data Visualization Package
     "ggplot2","scales",
     #EDA package
     "skimr","DataExplorer"
)

new.packages <- req_packages[!(req_packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

sapply(req_packages,library,character.only=T)

base_dir <- "C:/Users/Lenovo/Box/Unilever_Ops_Projects/Ops_BAU_Project/"
input_files_dir <- str_glue({base_dir},"Input_Files")
output_files_dir <- str_glue({base_dir},"Output_Files")


#------------------------------------------ Dataset and File Read --------------------------------------------------------

BAU_Ops_file <- readxl::read_excel(path = str_glue({input_files_dir},"/BAU_Export.xls"))

#------------------------------------------ Data Preparation and Analysis-------------------------------------------------

#select the required columns for the further processing
# @columns - {Title,Status,Assigned to,Start Date,End Date,Description,Ops Request type}

ops_data_prep <- BAU_Ops_file%>%
     select(Title,Status,`Assigned To`,`Start Date`,`End Date`,Description,`Operation Request Type`,Duration,`PDC Pillars for Ops`,`Select SBA squad`)

ops_data_prep <- ops_data_prep%>%
     clean_names(.)

#create the UID for each requests and extract the name from the Assigned_to
#convert the description column data to lower case
ops_data_prep <- ops_data_prep%>%
     separate(.,col = title,into = c("Request_ID"),sep = ":",remove = T)%>%
     mutate(Analyst_Name = assigned_to%>%
                 str_remove_all(.,pattern="\\b[-A-Za-z0-9_.%]+\\@[-A-Za-z0-9_.%]+\\.[A-Za-z]+"))%>%
     mutate(Analyst_Name = Analyst_Name%>%
                 str_to_title(.)%>%
                 str_remove_all(pattern = "<>"))%>%
     mutate(description=description%>%str_to_lower(.))


#-------------------------------------------- Feature Engineering -----------------------------------------------------

#Extract the features from the description column 
# @features to be extracted are - {Pillar of the pdc}

pdc_pillars <- c("cec","ddm","sba","cross pillar","cmi or others")

ops_data_prep <- ops_data_prep%>%
     mutate(pdc_pillar= case_when(description%>%str_detect(coll("cec",ignore_case = T,))~"CEC",
                                  description%>%str_detect(coll("ddm",ignore_case = T))~"DDM",
                                  description%>%str_detect(coll("cmi",ignore_case = T))~"CMI or Others",
                                  description%>%str_detect(coll("sba",ignore_case = T))~"SBA",
                                  description%>%str_detect(coll("cross pillar",ignore_case = T))~"Cross pillar ( CD, E-Commerce, DMPT)",
                                  T~"Others"))




ops_data_prep <- ops_data_prep%>%
     dplyr::mutate(pdc_pillars_for_ops = case_when((is.na(pdc_pillars_for_ops))~pdc_pillar,
                                                   T~pdc_pillars_for_ops))

ops_data_prep <- ops_data_prep%>%
        dplyr::mutate(start_date =case_when((is.na(start_date))~as.POSIXct(Sys.Date()),
                                            T~start_date))

ops_data_prep[which(is.na(ops_data_prep$operation_request_type)),"operation_request_type"] <- "Not Applicable"

#extract the date components from the start date 

ops_data_prep <- ops_data_prep%>%
        mutate(Request_Month = month(start_date,label = T,abbr = F),
               Request_year = year(start_date))

ops_data_prep$Request_Day <- weekdays(x = ops_data_prep$start_date)

write_xlsx(x = ops_data_prep,path = paste0(output_files_dir,"/OPS_data.xlsx"),format_headers = T)
