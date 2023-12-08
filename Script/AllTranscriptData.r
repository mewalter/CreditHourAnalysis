
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

# Load Codebook sheets into list 
path <- "./Data/codebook.xlsx"
Codebook <- path %>% excel_sheets() %>% set_names() %>% map(read_excel, path = path)

# Load raw data
#AllTransDataV2Raw <- read_excel("./Data/MAE transcript pulls_sample.xlsx")      # load all data
AllRawDataV2 <- read_csv("./Data/MAE transcript pulls_v2.csv")      # load all data

# Extract codes for majors from list
#CodebookMajors <- read_excel("./Data/codebook.xlsx", sheet="majors", skip=2)  # load MAJOR_CODE AND MAJOR_TITLE
CodebookMajors <- Codebook$majors %>% slice(2:n())
names(CodebookMajors) <- CodebookMajors %>% slice(1) %>% unlist()
CodebookMajors <- CodebookMajors %>% slice(-1)

# Extract codes for terms from list
CodebookTerms <- Codebook$terms %>% slice(2:n())
names(CodebookTerms) <- CodebookTerms %>% slice(1) %>% unlist()
CodebookTerms <- CodebookTerms %>% slice(2:(n() - 2)) #%>% remove_rownames %>% column_to_rownames(var="CODE")
# Turn codes into list so that it can be used as a lookup table
QtrCodes <- as.list( setNames( CodebookTerms[[2]], CodebookTerms[[1]] ) )

AllTranscriptDataV2 <- AllRawDataV2 %>% 
  left_join(CodebookMajors, by=c("majcd" = "MAJOR_CODE")) %>%
  relocate(major1=MAJOR_TITLE, .after = majcd) %>%
  mutate(across(majcd2,as.character)) %>%
  left_join(CodebookMajors, by=c("majcd2"="MAJOR_CODE")) %>%
  relocate(major2=MAJOR_TITLE, .after = majcd2) %>%
  left_join(CodebookMajors, by=c("degmajor"="MAJOR_CODE")) %>% 
  relocate(degmajcd=MAJOR_TITLE, .after = degmajor) 

AllTranscriptDataV2 <- AllTranscriptDataV2 %>%
  add_column(first_year=str_sub(AllRawDataV2$firstregtermcd,1,4), .after="firstregtermcd") %>%
  add_column(first_qtr=recode(str_sub(AllTranscriptDataV2$firstregtermcd,5,6), !!!QtrCodes), .after="first_year") %>%
  add_column(c_year=str_sub(AllRawDataV2$term,1,4), .after="term") %>%
  add_column(c_qtr=recode(str_sub(AllTranscriptDataV2$term,5,6), !!!QtrCodes), .after="c_year") %>%
  add_column(grad_year=str_sub(AllRawDataV2$degterm,1,4), .after="degterm") %>%
  add_column(grad_qtr=recode(str_sub(AllTranscriptDataV2$degterm,5,6), !!!QtrCodes), .after="grad_year") %>%
  unite(course, deptname, coursenumber, sep="", na.rm = TRUE, remove=FALSE)


# Get only Aero and ME Majors
# Aero: c("03D", "279", "281", "282", "332", "712", "751")
# Mech: c("02S", "032", "073", "277", "283", "295", "2B7", "2C0", "314", "330", "331", "332", "650", "712", "751", "954")
# c("Toyota", "my TOYOTA", "your Subaru") %>% str_detect( "(?i)toyota" )

# TE lists ... first easy way to enter them
# 
# txt <- "A B C D"
# txt4 <- scan(text=txt, what="")
# this does not work
# txt3 <- txt %>% str_split("\\s") %>% `[[`(1) %>% str_replace_all("\\b", "'") %>% str_flatten(collapse = ",")

NonMAETELump <- "ENGR7A ENGR7B CEE125 CEE160 CEE162 CEE173 EECS152A EECS152B CBEMS110
                     CBEMS190 BME111 BME120 BME121 Stats67 COMPSCI131 MATH112A MATH112B MATH112C 
                     MATH114A PHYSICS111A PHYSICS111B PHYSICS112A PHYSICS112B PHYSICS106"
NonMAETEStrings <- scan(text=NonMAETELump, what="")
# need to subtract at least 8 units (155/156/157) and (112/120)
METELump <- "ENGRMAE155 ENGRMAE156 ENGRMAE157 ENGRMAE112 ENGRMAE115 ENGRMAE110 ENGRMAE113 ENGRMAE114 ENGRMAE108
              ENGRMAE117 ENGRMAE118 ENGRMAE119 ENGRMAE132 ENGRMAE135 ENGRMAE136 ENGRMAE146 ENGRMAE152 ENGRMAE153
              ENGRMAE158 ENGRMAE159 ENGRMAE164 ENGRMAE165 ENGRMAE171 ENGRMAE172 ENGRMAE175 ENGRMAE182  
              ENGRMAE183 ENGRMAE184 ENGRMAE185 ENGRMAE199 ENGRMAE195 ENGRMAE188 ENGRMAE189 ENGRMAE193"
METEStrings <- scan(text=METELump, what="")
TEStrings <- c(NonMAETEStrings,METEStrings)

# Issues: ME only
# anything more than one of (155, 156, 157) is TE   --> just subtract 4 units
# anything more than 3 units of 189 or 188 is TE    --> just subtract 3 units
# anything more than one of (112,115) is TE         --> just subtract 4 units

# is 108 a TE for MEs?
# NOT accounting for double majors
# 



# Filter out MAE Data :: MAE189/188 Data :: MAE TE Data

AllMAEData <- AllTranscriptDataV2 %>% filter(str_detect(degmajcd,regex("aero|mech", ignore_case = TRUE)))

AllMAE189Data <- AllTranscriptDataV2 %>% 
  filter(str_detect(degmajcd,regex("aero|mech", ignore_case = TRUE)) &
           (course %in% "ENGRMAE189")) %>% arrange(id)

AllMAE189188Data <- AllTranscriptDataV2 %>% 
  filter(str_detect(degmajcd,regex("aero|mech", ignore_case = TRUE)) &
           (course %in% c("ENGRMAE189","ENGRMAE188"))) %>% arrange(id)

AllMAE93Data <- AllTranscriptDataV2 %>% filter(course %in% c("ENGRMAE93")) %>% arrange(id)
#  filter(str_detect(degmajcd,regex("aero|mech", ignore_case = TRUE)) &
#           (course %in% c("ENGRMAE93"))) %>% arrange(id)

AllMAE_TEData <- AllTranscriptDataV2 %>% 
  filter(str_detect(degmajcd,regex("aero|mech", ignore_case = TRUE)) &
           str_detect(course,paste(TEStrings, collapse = "|"))) %>% arrange(id)

AllME_TEData <- AllTranscriptDataV2 %>% 
  filter(str_detect(degmajcd,regex("mech", ignore_case = TRUE)) &
           str_detect(course,paste(TEStrings, collapse = "|"))) %>% arrange(id)

# Eventually might need to drop everyone not yet graduating current quarter         
#AllMAE189GradData <- AllMAE189Data %>% drop_na(grad_year)
#AllMAE189188GradData <- AllMAE189Data %>% drop_na(grad_year)
# Drop everyone with no firstregtermcd

# add units for each unique ID report 1 row with ID, fixed columns, total units, number of rows (189s taken)
# Add up total units of 189 taken by all MAE majors
SumMAE189GradData <- AllMAE189Data %>% group_by(id,grad_year,degmajcd,first_year,pascd) %>% 
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(id, .keep_all=TRUE) %>% arrange(totalunits)

# Add up total units of 189+188 taken by all MAE majors
SumMAE189188GradData <- AllMAE189188Data %>% group_by(id,grad_year,degmajcd,first_year,pascd) %>% 
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(id, .keep_all=TRUE) %>% arrange(totalunits)

# Add up total units of 189+188 taken by all MAE majors by year
SumMAE189188YearData <- AllMAE189188Data %>% group_by(c_year) %>% 
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(c_year, .keep_all=TRUE) %>% arrange(c_year)

# Add up total units of 93 taken by all UCI students
SumMAE93YearData <- AllMAE93Data %>% group_by(c_year) %>% 
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(c_year, .keep_all=TRUE) %>% arrange(totalunits)

# Add up total units of 93 taken by all UCI students by year
SumMAE93GradData <- AllMAE93Data %>% group_by(id,grad_year,degmajcd,first_year,pascd) %>% 
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(id, .keep_all=TRUE) %>% arrange(totalunits)

# Add up total TE units taken by ME degmajor students (adjusted by -11)
SumME_TEGradDataAdj <- AllME_TEData %>% group_by(id,grad_year,degmajcd,first_year,pascd) %>% 
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(id, .keep_all=TRUE) %>% mutate(totalunitsadj=totalunits-11) %>% arrange(totalunitsadj)

# Add up total TE units taken by ME degmajor students (adjusted by -11 AND filtered out outliers)
SumME_TEGradDataAdjFilt <- AllME_TEData %>% group_by(id,grad_year,degmajcd,first_year,pascd) %>% 
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(id, .keep_all=TRUE) %>% mutate(totalunitsadj=totalunits-11) %>% 
  filter(totalunitsadj %in% (0:40)) %>% arrange(totalunitsadj)


ggplot(SumMAE189188GradData, aes(x=totalunits, fill=grad_year, color=grad_year)) + 
  geom_histogram(binwidth = 1, center=0) +
  geom_vline(aes(xintercept=mean(totalunits)),color="blue", linetype="dashed", size=1) +
  scale_color_brewer(palette="Dark2") +
  #scale_x_continuous(breaks=seq(0,34,2)) +
  ggtitle("Total 189 Units Taken by MAEs \n (missing 195 at the moment)") +
  xlab("Total Units") + ylab("Number of Unique Students")

ggplot(SumME_TEGradDataAdj, aes(x=totalunitsadj, fill=grad_year, color=grad_year)) + 
  geom_histogram(binwidth = 1, center=0) +
  geom_vline(aes(xintercept=mean(totalunitsadj)),color="blue", linetype="dashed", size=1) +
  scale_color_brewer(palette="Dark2") +
  # scale_x_continuous(breaks=seq(0,34,2)) +
  ggtitle("Total TE Units Taken by MEs \n (subtracted 11 units for MEs)") +
  xlab("Total Units (Adjusted)") + ylab("Number of Unique Students")

ggplot(SumME_TEGradDataAdjFilt, aes(x=totalunitsadj, fill=grad_year, color=grad_year)) + 
  geom_histogram(binwidth = 1, center=0) +
  geom_vline(aes(xintercept=mean(totalunitsadj)),color="blue", linetype="dashed", size=1) +
  scale_color_brewer(palette="Dark2") +
  scale_x_continuous(breaks=seq(0,40,2)) +
  ggtitle("Total TE Units Taken by MEs \n (subtracted 11 units for MEs)") +
  xlab("Total Units (Adjusted & Filtered)") + ylab("Number of Unique Students")

ggplot(SumMAE93GradData, aes(x=totalunits, fill=grad_year, color=grad_year)) + 
  geom_histogram(binwidth = 1, center=0) +
  geom_vline(aes(xintercept=mean(totalunits)),color="blue", linetype="dashed", size=1) +
  scale_color_brewer(palette="Dark2") +
  scale_x_continuous(breaks=seq(0,40,2)) +
  ggtitle("Total 93 Units Taken by all UCI Students") +
  xlab("Total Units") + ylab("Number of Unique Students")


# How many times did some of these people take 189?
AllMAERepeat189Data <- AllMAE189Data %>% group_by(id) %>% summarise(n = n()) %>% arrange(n)
# This person took 189 21 times
Interesting <- AllTranscriptDataV2 %>% filter(str_detect(id,"15355")) %>% arrange(coursenumber)

Interesting3 <- AllTranscriptDataV2 %>% filter(str_detect(id,"3203")) %>% arrange(coursenumber)


Interesting4 <- AllTranscriptDataV2 %>% filter(str_detect(id,"1784")) %>% arrange(coursenumber)





# Filter out all non-MEAE people in 189  ..... 188  .... our TEs


AllNonMAE189Data <- AllTranscriptDataV2 %>% 
  filter(str_detect(AllTranscriptDataV2$degmajcd, regex("aero|mech", ignore_case = TRUE),negate=TRUE) &
           str_detect(AllTranscriptDataV2$course,"ENGRMAE189")) %>% arrange(id)

AllNonMAE189BData <- AllNonMAE189Data %>% group_by(degmajcd) %>% summarise(n = n()) %>% arrange(n)

AllNonMAERepeat189Data <- AllNonMAE189Data %>% group_by(id) %>% summarise(n = n()) %>% arrange(n)

# This person (Electrical Engineer) took 189 10(!) times
Interesting2 <- AllTranscriptDataV2 %>% filter(str_detect(id,"17389"))


