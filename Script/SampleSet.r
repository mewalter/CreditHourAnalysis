
library("tidyverse")
library("readxl")
#library("openxlsx")    # library("readxl") # this is the tidyverse installed package
library("scales")
library("lubridate")
library("googledrive")
library("googlesheets4")

#library("pastecs")
#library("anytime")

#source("./Script/Functions/ShowWorkMeans.r")
#col_names <- names(read_csv("./Data/Professor of Teaching Effort Distribution and Support Survey_February 24, 2021_21.37_NumChoice.csv", n_max = 0))

path <- "./Data/codebook.xlsx"
Codebook <- path %>% excel_sheets() %>% set_names() %>% map(read_excel, path = path)

AllDataSampleRaw <- read_excel("./Data/MAE transcript pulls_sample.xlsx")      # load all data

#CodebookMajors <- read_excel("./Data/codebook.xlsx", sheet="majors", skip=2)  # load MAJOR_CODE AND MAJOR_TITLE
CodebookMajors <- Codebook$majors %>% slice(2:n())
names(CodebookMajors) <- CodebookMajors %>% slice(1) %>% unlist()
CodebookMajors <- CodebookMajors %>% slice(-1)

CodebookTerms <- Codebook$terms %>% slice(2:n())
names(CodebookTerms) <- CodebookTerms %>% slice(1) %>% unlist()
CodebookTerms <- CodebookTerms %>% slice(2:(n() - 2)) #%>% 
#  remove_rownames %>% column_to_rownames(var="CODE")
QtrCodes <- as.list( setNames( CodebookTerms[[2]], CodebookTerms[[1]] ) )

AllDataSample <- AllDataSampleRaw %>% 
  left_join(CodebookMajors, by=c("majcd" = "MAJOR_CODE")) %>%
  relocate(major1=MAJOR_TITLE, .after = majcd) %>%
  mutate(across(majcd2,as.character)) %>%
  left_join(CodebookMajors, by=c("majcd2"="MAJOR_CODE")) %>%
  relocate(major2=MAJOR_TITLE, .after = majcd2) %>%
  left_join(CodebookMajors, by=c("degmajor"="MAJOR_CODE")) %>% rename(degmajcd=MAJOR_TITLE)

# Parse Degree Term

AllDataSample$first_year <- str_sub(AllDataSample$firstregtermcd,1,4)
AllDataSample$c_year <- str_sub(AllDataSample$term,1,4)
AllDataSample$grad_year <- str_sub(AllDataSample$degterm,1,4)

AllDataSample$first_qtr <- dplyr::recode(str_sub(AllDataSample$firstregtermcd,5,6), !!!QtrCodes)
AllDataSample$c_qtr <- dplyr::recode(str_sub(AllDataSample$term,5,6), !!!QtrCodes)
AllDataSample$grad_qtr <- dplyr::recode(str_sub(AllDataSample$degterm,5,6), !!!QtrCodes)





