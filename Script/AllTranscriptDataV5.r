
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

############################## Get Codes for Sanitizing Data ################################

# Load Codebook sheets into list 
path <- "./Data/codebook.xlsx"
Codebook <- path %>% excel_sheets() %>% set_names() %>% map(read_excel, path = path)

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
QtrCodes1 <- as.list( setNames( CodebookTerms[[2]], CodebookTerms[[1]] ) )
QtrCodes2 <- as.list( setNames( CodebookTerms[[3]], CodebookTerms[[1]] ) )


## Add Academic Year Breakdown
# if 202092 or 202103 or 202114 or 202125 or 202176 or 202139 or 202151 then AY20-21 
AY_Table <- tibble(quarter = character(), ay = character())
for (i in 1965:2050){
  A <- c(paste0(as.character(i),"92"), paste0(as.character(i+1),"03"), paste0(as.character(i+1),"14"), 
         paste0(as.character(i+1),"25"), paste0(as.character(i+1),"76"), paste0(as.character(i+1),"39"), 
         paste0(as.character(i+1),"51"))
  B <- paste0("AY",str_sub(as.character(i),3,4),"-",str_sub(as.character(i+1),3,4))
  temp <- bind_cols(quarter=A, ay=B)
  AY_Table <- bind_rows(AY_Table,temp)
}
remove(temp,A,B)
AY_Table <- AY_Table %>% mutate(across(quarter, as.integer))

### Letter to Number Grade
Letters <- c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "D-", "F","P","W","UR","I","IP", "NP", "NR")
Nums <- c(4, 4,3.666667, 3.333333, 3, 2.66667, 2.333333, 2, 1.666667, 1.333333, 1, 0.666667, 0, 0, 0, 0, 0, 0, 0, 0)
Numgrade <- bind_cols(letters=Letters, nums=Nums)

### Find Check-ins


###################################### Load Raw Data and Sanitize ####################################

#AllTransDataV3Raw <- read_excel("./Data/MAE transcript pulls_sample.xlsx")      # load
AllRawDataV3 <- read_csv("./Data/MAE transcript pulls_v3.csv", col_types = cols(majcd3=col_character()))     # load all data


AllTranscriptDataV3 <- AllRawDataV3 %>% 
  left_join(CodebookMajors, by=c("majcd" = "MAJOR_CODE")) %>%
  relocate(major1=MAJOR_TITLE, .after = majcd) %>%
  mutate(across(majcd2,as.character)) %>%
  left_join(CodebookMajors, by=c("majcd2"="MAJOR_CODE")) %>%
  relocate(major2=MAJOR_TITLE, .after = majcd2) %>%
  left_join(CodebookMajors, by=c("majcd3"="MAJOR_CODE")) %>%
  relocate(major3=MAJOR_TITLE, .after = majcd3) %>%
  left_join(CodebookMajors, by=c("degmajor"="MAJOR_CODE")) %>% 
  relocate(degmajcd=MAJOR_TITLE, .after = degmajor) %>%
  left_join(AY_Table, by=c("term"="quarter")) %>%
  relocate(c_ay=ay, .after = term) %>%
  left_join(AY_Table, by=c("degterm"="quarter")) %>%
  relocate(grad_ay=ay, .after = degterm) %>%
  left_join(Numgrade, by=c("grade"="letters")) %>%
  relocate(numgrade=nums, .after = grade)

AllTranscriptDataV3 <- AllTranscriptDataV3 %>%
  #add_column(first_year=str_sub(AllRawDataV3$firstregtermcd,1,4), .after="firstregtermcd") %>%
  #add_column(first_qtr=recode(str_sub(AllTranscriptDataV3$firstregtermcd,5,6), !!!QtrCodes1), .after="first_year") %>%
  add_column(first_qtr=paste0(str_sub(AllRawDataV3$firstregtermcd,1,4),
                             recode(str_sub(AllTranscriptDataV3$firstregtermcd,5,6), !!!QtrCodes2)),
                             .after="firstregtermcd") %>%
  #add_column(c_year=str_sub(AllRawDataV3$term,1,4), .after="term") %>%
  #add_column(c_qtr=recode(str_sub(AllTranscriptDataV3$term,5,6), !!!QtrCodes1), .after="c_year") %>%
  add_column(term_qtr=paste0(str_sub(AllRawDataV3$term,1,4),
                             recode(str_sub(AllTranscriptDataV3$term,5,6), !!!QtrCodes2)),
             .after="term") %>%
  #add_column(grad_year=str_sub(AllRawDataV3$degterm,1,4), .after="degterm") %>%
  #add_column(deg_qtr=recode(str_sub(AllTranscriptDataV3$degterm,5,6), !!!QtrCodes1), .after="grad_year") %>%
  add_column(deg_qtr=paste0(str_sub(AllRawDataV3$degterm,1,4),
                             recode(str_sub(AllTranscriptDataV3$degterm,5,6), !!!QtrCodes2)),
             .after="degterm") %>%
  unite(course, deptname, coursenumber, sep="", na.rm = TRUE, remove=FALSE) %>%
  arrange(degterm)


#CheckNA <- AllTranscriptDataV3 %>% filter(is.na(numgrade)) 
#CheckYear <- AllTranscriptDataV3 %>% arrange(firstregtermcd)
# head(arrange(Forbes2000,desc(profits)), n = 50)

# All_93Data <- AllTranscriptDataV3 %>% filter(course =="ENGRMAE93") %>% arrange(term)

remove(path, Codebook, CodebookMajors, CodebookTerms, QtrCodes1, QtrCodes2, 
       i, AY_Table, Letters, Nums, Numgrade, AllRawDataV3)


########################################### Tech Elective Lists #################################################

# TE lists ... first easy way to enter them
# 
# txt <- "A B C D"
# txt4 <- scan(text=txt, what="")
# this does not work
# txt3 <- txt %>% str_split("\\s") %>% `[[`(1) %>% str_replace_all("\\b", "'") %>% str_flatten(collapse = ",")

NonMAETELump <- "ENGR7A ENGR7B CEE125 CEE160 CEE162 CEE173 EECS152A EECS152B CBEMS110
                     CBEMS190 BME111 BME120 BME121 STATS67 COMPSCI131 MATH112A MATH112B MATH112C 
                     MATH114A PHYSICS111A PHYSICS111B PHYSICS112A PHYSICS112B PHYSICS106"
NonMAETEStrings <- scan(text=NonMAETELump, what="")
# need to subtract at least 8 units (155/156/157) and (112/120)
METELump <- "ENGRMAE155 ENGRMAE156 ENGRMAE157 ENGRMAE112 ENGRMAE115 ENGRMAE110 ENGRMAE113 ENGRMAE114 ENGRMAE108
              ENGRMAE117 ENGRMAE118 ENGRMAE119 ENGRMAE132 ENGRMAE135 ENGRMAE136 ENGRMAE146 ENGRMAE152 ENGRMAE153
              ENGRMAE158 ENGRMAE159 ENGRMAE164 ENGRMAE165 ENGRMAE171 ENGRMAE172 ENGRMAE175 ENGRMAE182  
              ENGRMAE183 ENGRMAE184 ENGRMAE185 ENGRMAE199 ENGRMAE195 ENGRMAE188 ENGRMAE189 ENGRMAE193"
METEStrings <- scan(text=METELump, what="")
TEStrings <- c(NonMAETEStrings,METEStrings)

remove(METELump, METEStrings, NonMAETELump, NonMAETEStrings)

# Issues: ME only
# anything more than one of (155, 156, 157) is TE   --> just subtract 4 units
# anything more than 3 units of 189 or 188 is TE    --> just subtract 3 units
# anything more than one of (112,115) is TE         --> just subtract 4 units

# is 108 a TE for MEs?
# NOT accounting for double majors
# 

# 195 class codes
# ENGRMAE195 (did not exist in AY18-19 ... did it by hand with signup genius)

# filter((course == "ENGRMAE195") & (coursecode == 18500))    ## F19
# filter((course == "ENGRMAE195") & (coursecode == 18560))    ## W20
# filter((course == "ENGRMAE195") & (coursecode == 18660))    ## Sp20
# filter((course == "ENGRMAE195") & (coursecode == 18500))    ## F20
# filter((course == "ENGRMAE195") & (coursecode == 18610))    ## W21
# filter((course == "ENGRMAE195") & (coursecode == 18610))    ## Sp21
CourseCode195 <- c(18500,18560,18660,18610)
#
#Check195 <- AllTranscriptDataV3 %>% filter((course == "ENGRMAE195") & (coursecode %in% CourseCode195)) 
#
#
#
#
#


#################################### Filter by Major ##################################################

# Get only Aero and ME Majors
# Aero: c("03D", "279", "281", "282", "332", "712", "751")
# Mech: c("02S", "032", "073", "277", "283", "295", "2B7", "2C0", "314", "330", "331", "332", "650", "712", "751", "954")
# c("Toyota", "my TOYOTA", "your Subaru") %>% str_detect( "(?i)toyota" )


########## Only students who took MAE151 and are graduating by Sp21 and are degree major 277
#  filter(str_detect(degmajcd,regex("aero|mech", ignore_case = TRUE)) &


#AllIDs_MAE151_Sp21_277 <- AllTranscriptDataV3 %>% mutate(degterm=as.numeric(degterm)) %>% 
#  filter((course == "ENGRMAE151") & (degterm <= 202114) & (degmajor == "277")) %>%
#  arrange(id)
#AllMEFinished <- AllTranscriptDataV3 %>% filter(id %in% AllIDs_MAE151_Sp21_277$id) %>% arrange(id)

## based on fact that 151 is MAE required and offered Winter quarters
AllIDs_MAE151_F14Su20_277 <- AllTranscriptDataV3 %>% mutate(degterm=as.numeric(degterm)) %>% 
  filter((course == "ENGRMAE151") & (degterm <= 202076) & (degterm >= 201414) & (degmajor == "277")) %>%
  arrange(id)
AllMEFinished <- AllTranscriptDataV3 %>% filter(id %in% AllIDs_MAE151_F14Su20_277$id) %>% arrange(id)

## this is "stable majors" only ... if someone took 150 before being MAE-277 they are not in this list.
## similarly, we will some who might switch out of MAE-277 before graduating
AllIDs_MAE150_F14XX_277 <- AllTranscriptDataV3 %>% mutate(degterm=as.numeric(degterm)) %>% 
  filter((course == "ENGRMAE150") & (degterm >= 201414) & (degmajor == "277")) %>%
  arrange(id)
AllME <- AllTranscriptDataV3 %>% filter(id %in% AllIDs_MAE150_F14XX_277$id) %>% arrange(degterm)

#CheckYear <- AllMEFinished %>% arrange(firstregtermcd)
# earliest first term enrollment 1999
#CheckYear <- AllMEFinished %>% arrange(degterm)   
# earliest graduation is 2009

AllMEFinished_TEData <- AllMEFinished %>% 
  filter(str_detect(course,paste(TEStrings, collapse = "|"))) %>% arrange(id)
AllME_TEData <- AllME %>% 
  filter(str_detect(course,paste(TEStrings, collapse = "|"))) %>% arrange(id)


################## Aero Majors ############################

####### All AE students (based on fact that MAE158 is required and is offered winter)
AllIDs_MAE158_F14Su20_279 <- AllTranscriptDataV3 %>% mutate(degterm=as.numeric(degterm)) %>% 
  filter((course == "ENGRMAE158") & (degterm <= 202076) & (degterm >= 201414) & (degmajor == "279")) %>%
  arrange(id)
AllAEFinished <- AllTranscriptDataV3 %>% filter(id %in% AllIDs_MAE158_F14Su20_279$id) %>% arrange(id)

## this is "stable majors" only ... if someone took 150 before being MAE-279 they are not in this list.
## similarly, we will some who might switch out of MAE-279 before graduating
AllIDs_MAE150_F14Su20_279 <- AllTranscriptDataV3 %>% mutate(degterm=as.numeric(degterm)) %>% 
  filter((course == "ENGRMAE150") & (degterm <= 202076) & (degterm >= 201414) & (degmajor == "279")) %>%
  arrange(id)
AllAE <- AllTranscriptDataV3 %>% filter(id %in% AllIDs_MAE150_F14Su20_279$id) %>% arrange(id)

## Just to see how many Aero Majors are taking 151 and how many take 159
AllAEFinished_151Data <- AllAEFinished %>% filter(course =="ENGRMAE151") %>% arrange(c_ay)
AllAEFinished_159Data <- AllAEFinished %>% filter(course =="ENGRMAE159") %>% arrange(c_ay)



######### All current and finished MEs and AEs and ALL current STUDENTS with their 189 and 93 units ############ 


AllMEFinished_UDProjData <- AllMEFinished %>% 
  filter((course %in% c("ENGRMAE189","ENGRMAE188","ENGRMAE193")) | 
           ((course == "ENGRMAE195") & (coursecode %in% CourseCode195))) %>% arrange(course)
AllME_UDProjData <- AllME %>% 
  filter((course %in% c("ENGRMAE189","ENGRMAE188","ENGRMAE193"))  | 
           ((course == "ENGRMAE195") & (coursecode %in% CourseCode195))) %>% arrange(id)

AllMEFinished_93Data <- AllMEFinished %>% filter(course =="ENGRMAE93") %>% arrange(term)
AllME_93Data <- AllME %>% filter(course =="ENGRMAE93") %>% arrange(term)

AllAEFinished_UDProjData <- AllAEFinished %>% 
  filter((course %in% c("ENGRMAE189","ENGRMAE188","ENGRMAE193"))  | 
           ((course == "ENGRMAE195") & (coursecode %in% CourseCode195))) %>% arrange(id)
AllAE_UDProjData <- AllAE %>% 
  filter((course %in% c("ENGRMAE189","ENGRMAE188","ENGRMAE193"))  | 
           ((course == "ENGRMAE195") & (coursecode %in% CourseCode195))) %>% arrange(id)

AllAEFinished_93Data <- AllAEFinished %>% filter(course =="ENGRMAE93") %>% arrange(id)
AllAE_93Data <- AllAE %>% filter(course =="ENGRMAE93") %>% arrange(id)

AllStudents_UDProjData <- AllTranscriptDataV3 %>% 
  filter((course %in% c("ENGRMAE189","ENGRMAE188","ENGRMAE193"))  | 
            ((course == "ENGRMAE195") & (coursecode %in% CourseCode195))) %>% arrange(id)
AllStudents_93Data <- AllTranscriptDataV3 %>% filter(course =="ENGRMAE93") %>% arrange(id)

AllNonMAEStudents_UDProjData <- AllTranscriptDataV3 %>% 
  filter( ((course %in% c("ENGRMAE189","ENGRMAE188","ENGRMAE193")) | 
             ((course == "ENGRMAE195") & (coursecode %in% CourseCode195))) & ((degmajor != "279") & (degmajor != "277"))) %>% 
  arrange(coursecode)

AllNonMAEStudents_93Data <-AllTranscriptDataV3 %>% 
  filter((course =="ENGRMAE93") & ((degmajor != "279") & (degmajor != "277")) ) %>% 
  arrange(id)

# Eventually might need to drop everyone not yet graduating current quarter         
#AllMAE189GradData <- AllMAE189Data %>% drop_na(deg_qtr)
#AllMAEUDProjGradData <- AllMAE189Data %>% drop_na(deg_qtr)
# Drop everyone with no firstregtermcd


############# Start Summations

# add units for each unique ID report 1 row with ID, fixed columns, total units, number of rows (189s taken)
# ME Majors

##
## Add up total units of 189/188/93 taken by all ME majors
#SumMEFinished_189Data <- AllMEFinished_189Data %>% group_by(id) %>% 
#  summarize(totalunits = sum(units), n=n(), deg_qtr=deg_qtr, pascd=pascd) %>% 
#  #group_by(id,deg_qtr,degmajcd,first_year,pascd) %>% 
# #summarize(totalunits = sum(units), n=n()) %>% 
#  distinct(id, .keep_all=TRUE) %>% arrange(totalunits)

# Add up total units of 189+188 and 93 taken by all FINISHED ME majors
SumMEFinished_UDProjData <- AllMEFinished_UDProjData %>% group_by(id) %>% 
  summarize(totalunits = sum(units), n=n(), deg_qtr=deg_qtr, pascd=pascd) %>% 
  distinct(id, .keep_all=TRUE) %>% arrange(totalunits)

SumMEFinished_UDProjTermData <- AllMEFinished_UDProjData %>% group_by(term) %>%   ## Finished ME by term
  summarize(totalunits = sum(units), n=n(), deg_qtr=deg_qtr, grad_ay=grad_ay, pascd=pascd, term_qtr=term_qtr) %>% 
  distinct(term, .keep_all=TRUE) %>% arrange(term)

SumMEFinished_UDProjYearData <- AllMEFinished_UDProjData %>% group_by(c_ay) %>%  ## finished ME by AY
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(c_ay, .keep_all=TRUE) %>% arrange(c_ay)
SumMEFinished_UDProjYearData

SumMEFinished_93YearData <- AllMEFinished_93Data %>% group_by(c_ay) %>%      # finished ME 93 by AY
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(c_ay, .keep_all=TRUE) %>% arrange(c_ay)
SumMEFinished_93YearData

SumMEFinished_93TermData <- AllMEFinished_93Data %>% group_by(term) %>%      # finished ME 93 by term
  summarize(totalunits = sum(units), n=n(), deg_qtr=deg_qtr, grad_ay=grad_ay, pascd=pascd, term_qtr=term_qtr) %>% 
  distinct(term, .keep_all=TRUE) %>% arrange(term)
SumMEFinished_93TermData

# Add up total units of 189+188 taken by all All FINISHED AND CURRENT ME majors
SumAllME_UDProjData <- AllME_UDProjData %>% group_by(id) %>% 
  summarize(totalunits = sum(units), n=n(), deg_qtr=deg_qtr, pascd=pascd) %>% 
  distinct(id, .keep_all=TRUE) %>% arrange(totalunits)

SumAllME_UDProjTermData <- AllME_UDProjData %>% group_by(term) %>%               ## all ME by term
  summarize(totalunits = sum(units), n=n(), deg_qtr=deg_qtr, grad_ay=grad_ay, pascd=pascd, term_qtr=term_qtr) %>% 
  distinct(term, .keep_all=TRUE) %>% arrange(term)

SumAllME_UDProjYearData <- AllME_UDProjData %>% group_by(c_ay) %>%               ## All ME by AY
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(c_ay, .keep_all=TRUE) %>% arrange(c_ay)
SumAllME_UDProjYearData
 
SumAllME_93YearData <- AllME_93Data %>% group_by(c_ay) %>%                        ## All ME 93 by AY
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(c_ay, .keep_all=TRUE) %>% arrange(c_ay)
SumAllME_93YearData

SumAllME_93TermData <- AllME_93Data %>% group_by(term) %>%                        ## All ME 93 by Term
  summarize(totalunits = sum(units), n=n(), deg_qtr=deg_qtr, grad_ay=grad_ay, pascd=pascd, term_qtr=term_qtr) %>% 
  distinct(term, .keep_all=TRUE) %>% arrange(term)
SumAllME_93TermData




# Add up total units of 93 taken by all ME majors by term
#SumMEFinished_93TermData <- AllMEFinished_93Data %>% group_by(term) %>% 
#    summarize(totalunits = sum(units), n=n(), deg_qtr=deg_qtr, pascd=pascd, term_qtr=term_qtr) %>% 
#  distinct(term, .keep_all=TRUE) %>% arrange(term)
#SumMEFinished_93TermData

##
## Add up total units of 189/188/93 taken by all FINISHED AE majors

# Add up total units of 189+188 taken by all AE majors
SumAEFinished_UDProjData <- AllAEFinished_UDProjData %>% group_by(id) %>% 
  summarize(totalunits = sum(units), n=n(), deg_qtr=deg_qtr, grad_ay=grad_ay, pascd=pascd) %>% 
  distinct(id, .keep_all=TRUE) %>% arrange(totalunits)

SumAEFinished_UDProjTermData <- AllAEFinished_UDProjData %>% group_by(term) %>% 
  summarize(totalunits = sum(units), n=n(), deg_qtr=deg_qtr, grad_ay=grad_ay, pascd=pascd, term_qtr=term_qtr) %>% 
  distinct(term, .keep_all=TRUE) %>% arrange(term)

# Add up total units of 189+188 taken by all ME majors by year
SumAEFinished_UDProjYearData <- AllAEFinished_UDProjData %>% group_by(c_ay) %>% 
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(c_ay, .keep_all=TRUE) %>% arrange(c_ay)
SumAEFinished_UDProjYearData

# Add up total units of 93 taken by all ME majors
SumAEFinished_93YearData <- AllAEFinished_93Data %>% group_by(c_ay) %>% 
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(c_ay, .keep_all=TRUE) %>% arrange(c_ay)
SumAEFinished_93YearData

SumAEFinished_93TermData <- AllAEFinished_93Data %>% group_by(term) %>% 
  summarize(totalunits = sum(units), n=n(), deg_qtr=deg_qtr, grad_ay=grad_ay, pascd=pascd, term_qtr=term_qtr) %>% 
  distinct(term, .keep_all=TRUE) %>% arrange(term)
SumAEFinished_93TermData


# Add up total units of 189+188 taken by all All FINISHED AND CURRENT AE majors
SumAllAE_UDProjData <- AllAE_UDProjData %>% group_by(id) %>% 
  summarize(totalunits = sum(units), n=n(), deg_qtr=deg_qtr, pascd=pascd) %>% 
  distinct(id, .keep_all=TRUE) %>% arrange(totalunits)

SumAllAE_UDProjTermData <- AllAE_UDProjData %>% group_by(term) %>%               ## all AE by term
  summarize(totalunits = sum(units), n=n(), deg_qtr=deg_qtr, grad_ay=grad_ay, pascd=pascd, term_qtr=term_qtr) %>% 
  distinct(term, .keep_all=TRUE) %>% arrange(term)

SumAllAE_UDProjYearData <- AllAE_UDProjData %>% group_by(c_ay) %>%               ## All AE by AY
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(c_ay, .keep_all=TRUE) %>% arrange(c_ay)
SumAllAE_UDProjYearData

SumAllAE_93YearData <- AllAE_93Data %>% group_by(c_ay) %>%                        ## All AE 93 by AY
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(c_ay, .keep_all=TRUE) %>% arrange(c_ay)
SumAllAE_93YearData

SumAllAE_93TermData <- AllAE_93Data %>% group_by(term) %>%                        ## All AE 93 by Term
  summarize(totalunits = sum(units), n=n(), deg_qtr=deg_qtr, grad_ay=grad_ay, pascd=pascd, term_qtr=term_qtr) %>% 
  distinct(term, .keep_all=TRUE) %>% arrange(term)
SumAllAE_93TermData


##
## Add up total units of 189/188/193/93 taken by all FINISHED AND CURRENT majors AND Non-MAE students
######### Remove 195 enrollments .... DO NOT look at totalunits

SumAllStudents_UDProjData <- AllStudents_UDProjData %>%  
  filter(!course=="ENGRMAE195") %>% group_by(id) %>% 
  summarize(totalunits = sum(units), n=n(), deg_qtr=deg_qtr, grad_ay=grad_ay, pascd=pascd, term_qtr=term_qtr) %>% 
  distinct(id, .keep_all=TRUE) %>% arrange(totalunits)

SumAllStudents_UDProjYearData <- AllStudents_UDProjData %>%  
  filter(!course=="ENGRMAE195") %>% group_by(c_ay) %>% 
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(c_ay, .keep_all=TRUE) %>% add_column(class="MAE188-189") %>% arrange(c_ay)
# add the data for this quarter
SumAllStudents_UDProjYearData[12,3] <- 583
SumAllStudents_UDProjYearData

SumAllStudents_UDProjTermData <- AllStudents_UDProjData %>% 
  filter(!course=="ENGRMAE195") %>% group_by(term) %>% 
  summarize(totalunits = sum(units), n=n(), term_qtr=term_qtr, c_ay=c_ay) %>% 
  distinct(term, .keep_all=TRUE) %>% ungroup() %>%
  # add the data for this quarter
    add_row(term=202114,totalunits=0,n=156,term_qtr="2021Sp",c_ay="AY20-21") %>% group_by(term) %>% 
  add_column(class="MAE188-189") %>% arrange(term)
SumAllStudents_UDProjTermData 

SumAllStudents_93YearData <- AllStudents_93Data %>% group_by(c_ay) %>% 
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(c_ay, .keep_all=TRUE) %>% add_column(class="MAE93") %>% arrange(c_ay)
SumAllStudents_93YearData[4,3] <- 789
SumAllStudents_93YearData

SumAllStudents_93TermData <- AllStudents_93Data %>% group_by(term) %>% 
  summarize(totalunits = sum(units), n=n(), term_qtr=term_qtr, c_ay=c_ay) %>% 
  distinct(term, .keep_all=TRUE) %>% ungroup() %>%
  # add the data for this quarter
  add_row(term=202114,totalunits=0,n=290,term_qtr="2021Sp",c_ay="AY20-21") %>% group_by(term) %>% 
  add_column(class="MAE93") %>% arrange(term)
SumAllStudents_93TermData <- SumAllStudents_93TermData %>% ungroup() %>%
  add_row(term=200992,totalunits=0,n=0,term_qtr="2009F",c_ay="AY09-10",class="MAE93") %>%
  add_row(term=201003,totalunits=0,n=0,term_qtr="2010W",c_ay="AY09-10",class="MAE93") %>%
  add_row(term=201014,totalunits=0,n=0,term_qtr="2010Sp",c_ay="AY09-10",class="MAE93") %>%
  add_row(term=201092,totalunits=0,n=0,term_qtr="2010F",c_ay="AY10-11",class="MAE93") %>%
  add_row(term=201103,totalunits=0,n=0,term_qtr="2011W",c_ay="AY10-11",class="MAE93") %>%
  add_row(term=201114,totalunits=0,n=0,term_qtr="2011Sp",c_ay="AY10-11",class="MAE93") %>%
  add_row(term=201192,totalunits=0,n=0,term_qtr="2011F",c_ay="AY11-12",class="MAE93") %>%
  add_row(term=201203,totalunits=0,n=0,term_qtr="2012W",c_ay="AY11-12",class="MAE93") %>%
  add_row(term=2012114,totalunits=0,n=0,term_qtr="2012Sp",c_ay="AY11-12",class="MAE93") %>%
  add_row(term=201292,totalunits=0,n=0,term_qtr="2012F",c_ay="AY12-13",class="MAE93") %>%
  add_row(term=201303,totalunits=0,n=0,term_qtr="2013W",c_ay="AY12-13",class="MAE93") %>%
  add_row(term=201314,totalunits=0,n=0,term_qtr="2013Sp",c_ay="AY12-13",class="MAE93") %>%
  add_row(term=201392,totalunits=0,n=0,term_qtr="2013F",c_ay="AY13-14",class="MAE93") %>%
  add_row(term=201403,totalunits=0,n=0,term_qtr="2014W",c_ay="AY13-14",class="MAE93") %>%
  add_row(term=201414,totalunits=0,n=0,term_qtr="2014Sp",c_ay="AY13-14",class="MAE93") %>%
  add_row(term=201492,totalunits=0,n=0,term_qtr="2014F",c_ay="AY14-15",class="MAE93") %>%
  add_row(term=201503,totalunits=0,n=0,term_qtr="2015W",c_ay="AY14-15",class="MAE93") %>%
  add_row(term=201514,totalunits=0,n=0,term_qtr="2015Sp",c_ay="AY14-15",class="MAE93") %>%
  add_row(term=201592,totalunits=0,n=0,term_qtr="2015F",c_ay="AY15-16",class="MAE93") %>%
  add_row(term=201603,totalunits=0,n=0,term_qtr="2016W",c_ay="AY15-16",class="MAE93") %>%
  add_row(term=201614,totalunits=0,n=0,term_qtr="2016Sp",c_ay="AY15-16",class="MAE93") %>%
  add_row(term=201692,totalunits=0,n=0,term_qtr="2016F",c_ay="AY16-17",class="MAE93") %>%
  add_row(term=201703,totalunits=0,n=0,term_qtr="2017W",c_ay="AY16-17",class="MAE93") %>%
  add_row(term=201714,totalunits=0,n=0,term_qtr="2017Sp",c_ay="AY16-17",class="MAE93")

SumAllNonMAEStudents_UDProjData <- AllNonMAEStudents_UDProjData %>% group_by(id) %>% 
  summarize(totalunits = sum(units), n=n(), deg_qtr=deg_qtr, grad_ay=grad_ay, pascd=pascd, term_qtr=term_qtr) %>% 
  distinct(id, .keep_all=TRUE) %>% arrange(totalunits)

SumAllNonMAEStudents_93Data <- AllNonMAEStudents_93Data %>% group_by(id) %>% 
  summarize(totalunits = sum(units), n=n(), deg_qtr=deg_qtr, grad_ay=grad_ay, pascd=pascd, term_qtr=term_qtr) %>% 
  distinct(id, .keep_all=TRUE) %>% arrange(totalunits)


################ show what is happening with UDProj and 93 enrollments
AllUDProj93 <- bind_rows(SumAllStudents_UDProjTermData,SumAllStudents_93TermData) 
AllUDProj93 <- AllUDProj93 %>% filter(!grepl('Su',term_qtr)) # %>% spread(c_ay,n)


ggplot(AllUDProj93, aes(x= factor(term_qtr,levels=unique(term_qtr)), y=n, 
                        fill=class)) + 
  geom_col(position=position_dodge(width=1,preserve="single"))+
  facet_wrap(~c_ay, strip.position="bottom", scales="free_x") +
  theme(panel.spacing = unit(1, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside")+
  geom_text(aes(label=paste0(n)),vjust=2,
            position=position_dodge2(width=1),size=3)+
  #geom_text(position=position_dodge2(width=0.9,preserve="single"),angle=0,vjust=0.25) +
  #geom_bar(stat="identity", position=position_dodge()) +
  # scale_color_brewer(palette="Dark2") +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  #scale_x_continuous(breaks=seq(0,34,2)) +
  ggtitle("188+189+193 and 93 Enrollments") +
  xlab("Quarter") + ylab("Number of Enrollments")



########################### average # UDproj units taken by finished MEs


SumMEFinished_UDProjData <- AllMEFinished_UDProjData %>% group_by(id) %>% 
  summarize(totalunits = sum(units), n=n(), deg_qtr=deg_qtr, grad_ay=grad_ay, pascd=pascd) %>% 
  distinct(id, .keep_all=TRUE) %>% arrange(id)
SumMEFinished_UDProjTEData <- SumMEFinished_UDProjData %>% mutate(totalteunits=totalunits-3)

SumMEFinished_UDProjTermData <- AllMEFinished_UDProjData %>% group_by(term) %>%   ## Finished ME by term
  summarize(totalunits = sum(units), n=n(), deg_qtr=deg_qtr, grad_ay=grad_ay, pascd=pascd, term_qtr=term_qtr) %>% 
  distinct(term, .keep_all=TRUE) %>% arrange(term)

SumMEFinished_UDProjYearData <- AllMEFinished_UDProjData %>% group_by(c_ay) %>%  ## finished ME by AY
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(c_ay, .keep_all=TRUE) %>% arrange(c_ay)
SumMEFinished_UDProjYearData

AvgUDProjUnitsMEFinished_YearData <- SumMEFinished_UDProjData %>% group_by(grad_ay) %>%
  summarize(avgudprojunits = mean(totalunits), n=n()) %>%
  distinct(grad_ay, .keep_all=TRUE) %>% arrange(grad_ay)

AvgUDProjTEUnitsMEFinished_YearData <- SumMEFinished_UDProjTEData %>% group_by(grad_ay) %>%
  summarize(avgudprojteunits = mean(totalteunits), n=n()) %>%
  distinct(grad_ay, .keep_all=TRUE) %>% arrange(grad_ay)

## Tabulate year data

AllYearlyUDProjProjectData <- SumAllME_UDProjYearData %>% 
  left_join(SumAllAE_UDProjYearData, by="c_ay") %>%
  left_join(SumAllStudents_UDProjYearData, by="c_ay") %>%
  rename(year=c_ay,ME_Units=totalunits.x,ME_Students=n.x,AE_Units=totalunits.y,AE_Students=n.y,All_Units=totalunits,All_Students=n)
AllYearlyUDProjProjectData

AllYearly93ProjectData <- SumMEFinished_93YearData %>% 
  left_join(SumAEFinished_93YearData, by="c_ay") %>%
  left_join(SumAllStudents_93YearData, by="c_ay") %>%
  rename(year=c_ay,ME_Units=totalunits.x,ME_Students=n.x,AE_Units=totalunits.y,AE_Students=n.y,All_Units=totalunits,All_Students=n)
AllYearly93ProjectData





####### TE Data


# Add up total TE units taken by ME degmajor students (adjusted by -11)
#SumMEFinished_TEDataAdj <- AllMEFinished_TEData %>% group_by(id,deg_qtr,degmajcd,first_year,pascd) %>% 
#  summarize(totalunits = sum(units), n=n()) %>% 
#  distinct(id, .keep_all=TRUE) %>% mutate(totalunitsadj=totalunits-11) %>% arrange(totalunitsadj)

SumMEFinished_TEDataAdj <- AllMEFinished_TEData %>% group_by(id) %>% 
  summarize(totalunits = sum(units), n=n(), deg_qtr=deg_qtr, grad_ay=grad_ay, pascd=pascd) %>% 
  mutate(totalunitsadj=totalunits-11) %>% 
  distinct(id, .keep_all=TRUE) %>% 
  arrange(totalunitsadj)

Interesting3a <- AllTranscriptDataV3 %>% filter(str_detect(id,"12547")) %>% arrange(coursenumber)
Interesting3b <- AllMEFinished_TEData %>% filter(str_detect(id,"12547")) %>% arrange(id)
Interesting3a <- AllTranscriptDataV3 %>% filter(str_detect(id,"\\b821\\b")) %>% arrange(coursenumber)
Interesting3b <- AllMEFinished_TEData %>% filter(str_detect(id,"\\b821\\b")) %>% arrange(id)
Interesting3a <- AllTranscriptDataV3 %>% filter(str_detect(id,"\\b8024\\b")) %>% arrange(coursenumber)
Interesting3b <- AllMEFinished_TEData %>% filter(str_detect(id,"\\b8024\\b")) %>% arrange(id)
Interesting3a <- AllTranscriptDataV3 %>% filter(str_detect(id,"\\b9965\\b")) %>% arrange(coursenumber)
Interesting3b <- AllMEFinished_TEData %>% filter(str_detect(id,"\\b9965\\b")) %>% arrange(id)
Interesting3a <- AllTranscriptDataV3 %>% filter(str_detect(id,"\\b2118\\b")) %>% arrange(coursenumber)
Interesting3b <- AllMEFinished_TEData %>% filter(str_detect(id,"\\b2118\\b")) %>% arrange(id)
Interesting3a <- AllTranscriptDataV3 %>% filter(str_detect(id,"\\b15718\\b")) %>% arrange(coursenumber)
Interesting3b <- AllMEFinished_TEData %>% filter(str_detect(id,"\\b15718\\b")) %>% arrange(id)

# Add up total TE units taken by ME degmajor students (adjusted by -11 AND filtered out outliers)
#SumMEFinished_TEDataAdjFilt <- AllMEFinished_TEData %>% group_by(id,deg_qtr,degmajcd,first_year,pascd) %>% 
#  summarize(totalunits = sum(units), n=n()) %>% 
#  distinct(id, .keep_all=TRUE) %>% mutate(totalunitsadj=totalunits-11) %>% 
#  filter(totalunitsadj %in% (0:40)) %>% arrange(totalunitsadj)



###### Total TE Units to 189 Units



JoinMEFinished_TEUDProjData <- SumMEFinished_TEDataAdj %>% left_join(SumMEFinished_UDProjData,by="id") %>% drop_na() %>%
  mutate(ratio=((totalunits.y-3)/totalunitsadj)*100)





ggplot(SumMEFinished_UDProjData, aes(x=totalunits, fill=grad_ay, color=grad_ay)) + 
  geom_histogram(binwidth = 1, center=0) +
  geom_vline(aes(xintercept=mean(totalunits)),color="blue", linetype="dashed", size=1) +
  scale_color_brewer(palette="Dark2") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  #scale_x_continuous(breaks=seq(0,34,2)) +
  ggtitle("Total 189+188 Units Taken by MAEs \n (missing 195 at the moment)") +
  xlab("Total Units") + ylab("Number of Unique Students")

ggplot(SumMEFinished_TEDataAdj, aes(x=totalunitsadj, fill=grad_ay, color=grad_ay)) + 
  geom_histogram(binwidth = 1, center=0) +
  geom_vline(aes(xintercept=mean(totalunitsadj)),color="blue", linetype="dashed", size=1) +
  scale_color_brewer(palette="Dark2") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  # scale_x_continuous(breaks = round(seq(min(SumMEFinished_TEDataAdj$x), max(SumMEFinished_TEDataAdj$x), by = 0.5),1))+
  # scale_x_continuous(breaks=seq(0,34,2)) +
  ggtitle("Total TE Units Taken by MEs \n (subtracted 11 units for MEs)") +
  xlab("Total Units (Adjusted)") + ylab("Number of Unique Students")


ggplot(JoinMEFinished_TEUDProjData, aes(x=ratio, fill=grad_ay.x, color=grad_ay.x)) + 
  geom_histogram(binwidth = 1, center=0) +
  geom_vline(aes(xintercept=mean(ratio)),color="blue", linetype="dashed", size=1) +
  scale_color_brewer(palette="Dark2") +
 # scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  # scale_x_continuous(breaks = round(seq(min(SumMEFinished_TEDataAdj$x), max(SumMEFinished_TEDataAdj$x), by = 0.5),1))+
  # scale_x_continuous(breaks=seq(0,34,2)) +
  ggtitle("Fraction of 189 Units in Total TE for MEs \n (subtracted 11 TE units and 3 189 units)") +
  xlab("Total Units (Adjusted)") + ylab("Number of Unique Students")


ggplot(JoinMEFinished_TEUDProjData, aes(x=ratio, fill=grad_ay.x, color=grad_ay.x)) + 
  geom_histogram(binwidth = 1, center=0) +
  #geom_vline(aes(xintercept=mean(ratio)),color="blue", linetype="dashed", size=1) +
  scale_color_brewer(palette="Dark2") +
  # scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  # scale_x_continuous(breaks = round(seq(min(SumMEFinished_TEDataAdj$x), max(SumMEFinished_TEDataAdj$x), by = 0.5),1))+
  scale_x_continuous(breaks=seq(0,40,2),limits=c(0,40)) +
  ggtitle("Fraction of 189 Units in Total TE for MEs \n (subtracted 11 TE units and 3 189 units)") +
  xlab("Total Units (Adjusted)") + ylab("Number of Unique Students")



ggplot(JoinMEFinished_TEUDProjData, aes(x=ratio, fill=grad_ay.x, color=grad_ay.x)) + 
  geom_histogram(binwidth = 1, center=0) +
  #geom_vline(aes(xintercept=mean(ratio)),color="blue", linetype="dashed", size=1) +
  scale_color_brewer(palette="Dark2") +
  # scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  # scale_x_continuous(breaks = round(seq(min(SumMEFinished_TEDataAdj$x), max(SumMEFinished_TEDataAdj$x), by = 0.5),1))+
  scale_x_continuous(breaks=seq(0,40,2),limits=c(0,40)) +
  ggtitle("Ratio of Total TE units by 189 units Taken by MEs \n (subtracted 11 TE units and 3 189 units)") +
  xlab("Total Units (Adjusted)") + ylab("Number of Unique Students")










ggplot(SumME_TEGradDataAdjFilt, aes(x=totalunitsadj, fill=grad_ay, color=grad_ay)) + 
  geom_histogram(binwidth = 1, center=0) +
  geom_vline(aes(xintercept=mean(totalunitsadj)),color="blue", linetype="dashed", size=1) +
  scale_color_brewer(palette="Dark2") +
  scale_x_continuous(breaks=seq(0,40,2)) +
  ggtitle("Total TE Units Taken by MEs \n (subtracted 11 units for MEs)") +
  xlab("Total Units (Adjusted & Filtered)") + ylab("Number of Unique Students")

ggplot(SumMAE93GradData, aes(x=totalunits, fill=grad_ay, color=grad_ay)) + 
  geom_histogram(binwidth = 1, center=0) +
  geom_vline(aes(xintercept=mean(totalunits)),color="blue", linetype="dashed", size=1) +
  scale_color_brewer(palette="Dark2") +
  scale_x_continuous(breaks=seq(0,40,2)) +
  ggtitle("Total 93 Units Taken by all UCI Students") +
  xlab("Total Units") + ylab("Number of Unique Students")





# How many times did some of these people take 189?
AllMEFinished_Repeat189Data <- AllMEFinished_UDProjData %>% group_by(id) %>% summarise(n = n()) %>% arrange(n)
# This person took 189 21 times

Interesting1 <- AllTranscriptDataV3 %>% filter(str_detect(id,"15355")) %>% arrange(coursenumber)
# took 189 21 times for 27 units (usually had 3 different 189 projects per quarter)




Interesting4 <- AllTranscriptDataV3 %>% filter(str_detect(id,"181")) %>% arrange(id)
Interesting5 <- AllMEFinished_TEData %>% filter(str_detect(id,"181")) %>% arrange(id)



# Filter out all non-MEAE people in 189  ..... 188  .... our TEs


AllNonMAE189Data <- AllTranscriptDataV3 %>% 
  filter(str_detect(AllTranscriptDataV3$degmajcd, regex("aero|mech", ignore_case = TRUE),negate=TRUE) &
           str_detect(AllTranscriptDataV3$course,"ENGRMAE189")) %>% arrange(id)

AllNonMAE189BData <- AllNonMAE189Data %>% group_by(degmajcd) %>% summarise(n = n()) %>% arrange(n)

AllNonMAERepeat189Data <- AllNonMAE189Data %>% group_by(id) %>% summarise(n = n()) %>% arrange(n)

# This person (Electrical Engineer) took 189 10(!) times
Interesting2 <- AllTranscriptDataV3 %>% filter(str_detect(id,"17389"))


