
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
QtrCodes <- as.list( setNames( CodebookTerms[[2]], CodebookTerms[[1]] ) )

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
remove(temp)
AY_Table <- AY_Table %>% mutate(across(quarter, as.integer))


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
  relocate(grad_ay=ay, .after = degterm)

AllTranscriptDataV3 <- AllTranscriptDataV3 %>%
  add_column(first_year=str_sub(AllRawDataV3$firstregtermcd,1,4), .after="firstregtermcd") %>%
  add_column(first_qtr=recode(str_sub(AllTranscriptDataV3$firstregtermcd,5,6), !!!QtrCodes), .after="first_year") %>%
  add_column(c_year=str_sub(AllRawDataV3$term,1,4), .after="term") %>%
  add_column(c_qtr=recode(str_sub(AllTranscriptDataV3$term,5,6), !!!QtrCodes), .after="c_year") %>%
  add_column(grad_year=str_sub(AllRawDataV3$degterm,1,4), .after="degterm") %>%
  add_column(grad_qtr=recode(str_sub(AllTranscriptDataV3$degterm,5,6), !!!QtrCodes), .after="grad_year") %>%
  unite(course, deptname, coursenumber, sep="", na.rm = TRUE, remove=FALSE) %>%
  arrange(degterm)

#CheckYear <- AllTranscriptDataV3 %>% arrange(firstregtermcd)
# head(arrange(Forbes2000,desc(profits)), n = 50)

# All_93Data <- AllTranscriptDataV3 %>% filter(course =="ENGRMAE93") %>% arrange(term)



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

# Issues: ME only
# anything more than one of (155, 156, 157) is TE   --> just subtract 4 units
# anything more than 3 units of 189 or 188 is TE    --> just subtract 3 units
# anything more than one of (112,115) is TE         --> just subtract 4 units

# is 108 a TE for MEs?
# NOT accounting for double majors
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

#### Filter out MAE189/188/93 Data for ME, AE, and All UCI

AllMEFinished_189Data <- AllMEFinished %>% filter((course == "ENGRMAE189")) %>% arrange(id)
AllME_189Data <- AllME %>% filter((course == "ENGRMAE189")) %>% arrange(id)
# plot grades?

AllMEFinished_189188Data <- AllMEFinished %>% 
  filter((course %in% c("ENGRMAE189","ENGRMAE188"))) %>% arrange(id)
AllME_189188Data <- AllME %>% 
  filter((course %in% c("ENGRMAE189","ENGRMAE188"))) %>% arrange(id)

AllMEFinished_93Data <- AllMEFinished %>% filter(course =="ENGRMAE93") %>% arrange(term)
AllME_93Data <- AllME %>% filter(course =="ENGRMAE93") %>% arrange(term)

AllAEFinished_189Data <- AllAEFinished %>% filter((course == "ENGRMAE189")) %>% arrange(id)
AllAE_189Data <- AllAE %>% filter((course == "ENGRMAE189")) %>% arrange(id)

AllAEFinished_189188Data <- AllAEFinished %>% 
  filter((course %in% c("ENGRMAE189","ENGRMAE188"))) %>% arrange(id)
AllAEd_189188Data <- AllAE %>% 
  filter((course %in% c("ENGRMAE189","ENGRMAE188"))) %>% arrange(id)

AllAEFinished_93Data <- AllAEFinished %>% filter(course =="ENGRMAE93") %>% arrange(id)
AllAE_93Data <- AllAE %>% filter(course =="ENGRMAE93") %>% arrange(id)

## Just to see how many Aero Majors are taking 151 and how many take 159
AllAEFinished_151Data <- AllAEFinished %>% filter(course =="ENGRMAE151") %>% arrange(c_ay)
AllAEFinished_159Data <- AllAEFinished %>% filter(course =="ENGRMAE159") %>% arrange(c_ay)


# all students

AllStudentsYoI_189Data <- AllTranscriptDataV3 %>% filter((course == "ENGRMAE189") & (degterm <= 202076) & (degterm >= 201414)) %>% 
  arrange(id)
AllStudents_189Data <- AllTranscriptDataV3 %>% filter((course == "ENGRMAE189")) %>% arrange(id)


AllStudentsYoI_189188Data <- AllTranscriptDataV3 %>% filter((course %in% c("ENGRMAE189","ENGRMAE188")) & 
                                                           (degterm <= 202076) & (degterm >= 201414)) %>% 
  arrange(id)
AllStudents_189188Data <- AllTranscriptDataV3 %>% filter((course %in% c("ENGRMAE189","ENGRMAE188"))) %>% arrange(id)

AllStudentsYoI_93Data <- AllTranscriptDataV3 %>% filter((course =="ENGRMAE93") & (degterm <= 202076) & (degterm >= 201414)) %>% 
  arrange(id)
AllStudents_93Data <- AllTranscriptDataV3 %>% filter(course =="ENGRMAE93") %>% arrange(id)


# Eventually might need to drop everyone not yet graduating current quarter         
#AllMAE189GradData <- AllMAE189Data %>% drop_na(grad_year)
#AllMAE189188GradData <- AllMAE189Data %>% drop_na(grad_year)
# Drop everyone with no firstregtermcd


############# Start Summations

# add units for each unique ID report 1 row with ID, fixed columns, total units, number of rows (189s taken)
# ME Majors

##
## Add up total units of 189/188/93 taken by all ME majors
#SumMEFinished_189Data <- AllMEFinished_189Data %>% group_by(id) %>% 
#  summarize(totalunits = sum(units), n=n(), grad_year=grad_year, pascd=pascd) %>% 
#  #group_by(id,grad_year,degmajcd,first_year,pascd) %>% 
# #summarize(totalunits = sum(units), n=n()) %>% 
#  distinct(id, .keep_all=TRUE) %>% arrange(totalunits)

# Add up total units of 189+188 taken by all finished ME majors
SumMEFinished_189188Data <- AllMEFinished_189188Data %>% group_by(id) %>% 
  summarize(totalunits = sum(units), n=n(), grad_year=grad_year, pascd=pascd) %>% 
  distinct(id, .keep_all=TRUE) %>% arrange(totalunits)

SumMEFinished_189188TermData <- AllMEFinished_189188Data %>% group_by(term) %>% 
  summarize(totalunits = sum(units), n=n(), grad_year=grad_year, pascd=pascd, qtr=c_qtr) %>% 
  distinct(term, .keep_all=TRUE) %>% arrange(term)

# Add up total units of 189+188 taken by all finished ME majors by year
SumMEFinished_189188YearData <- AllMEFinished_189188Data %>% group_by(c_ay) %>% 
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(c_ay, .keep_all=TRUE) %>% arrange(c_ay)
SumMEFinished_189188YearData

# Add up total units of 93 taken by all finished ME majors by year
SumMEFinished_93YearData <- AllMEFinished_93Data %>% group_by(c_ay) %>% 
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(c_ay, .keep_all=TRUE) %>% arrange(c_ay)
SumMEFinished_93YearData

# Add up total units of 93 taken by all ME majors by term
#SumMEFinished_93TermData <- AllMEFinished_93Data %>% group_by(term) %>% 
#    summarize(totalunits = sum(units), n=n(), grad_year=grad_year, pascd=pascd, qtr=c_qtr) %>% 
#  distinct(term, .keep_all=TRUE) %>% arrange(term)
#SumMEFinished_93TermData

##
## Add up total units of 189/188/93 taken by all AE majors
SumAEFinished_189Data <- AllAEFinished_189Data %>% group_by(id) %>% 
  summarize(totalunits = sum(units), n=n(), grad_year=grad_year, pascd=pascd) %>% 
  distinct(id, .keep_all=TRUE) %>% arrange(totalunits)

# Add up total units of 189+188 taken by all ME majors
SumAEFinished_189188Data <- AllAEFinished_189188Data %>% group_by(id) %>% 
  summarize(totalunits = sum(units), n=n(), grad_year=grad_year, pascd=pascd) %>% 
  distinct(id, .keep_all=TRUE) %>% arrange(totalunits)

SumAEFinished_189188TermData <- AllAEFinished_189188Data %>% group_by(term) %>% 
  summarize(totalunits = sum(units), n=n(), grad_year=grad_year, pascd=pascd, qtr=c_qtr) %>% 
  distinct(term, .keep_all=TRUE) %>% arrange(term)

# Add up total units of 189+188 taken by all ME majors by year
SumAEFinished_189188YearData <- AllAEFinished_189188Data %>% group_by(c_ay) %>% 
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(c_ay, .keep_all=TRUE) %>% arrange(c_ay)
SumAEFinished_189188YearData

# Add up total units of 93 taken by all ME majors
SumAEFinished_93YearData <- AllAEFinished_93Data %>% group_by(c_ay) %>% 
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(c_ay, .keep_all=TRUE) %>% arrange(c_ay)
SumAEFinished_93YearData

SumAEFinished_93TermData <- AllAEFinished_93Data %>% group_by(term) %>% 
  summarize(totalunits = sum(units), n=n(), grad_year=grad_year, pascd=pascd, qtr=c_qtr) %>% 
  distinct(term, .keep_all=TRUE) %>% arrange(term)
SumAEFinished_93TermData

##
## Add up total units of 189/188/93 taken by all AE majors
SumAllStudentsYoI_189Data <- AllStudentsYoI_189Data %>% group_by(id) %>% 
  summarize(totalunits = sum(units), n=n(), grad_year=grad_year, pascd=pascd) %>% 
  distinct(id, .keep_all=TRUE) %>% arrange(totalunits)

SumAllStudentsYoI_189188Data <- AllStudentsYoI_189188Data %>% group_by(id) %>% 
  summarize(totalunits = sum(units), n=n(), grad_year=grad_year, pascd=pascd) %>% 
  distinct(id, .keep_all=TRUE) %>% arrange(totalunits)

SumAllStudentsYoI_189188TermData <- AllStudentsYoI_189188Data %>% group_by(term) %>% 
  summarize(totalunits = sum(units), n=n(), grad_year=grad_year, pascd=pascd, qtr=c_qtr) %>% 
  distinct(term, .keep_all=TRUE) %>% arrange(totalunits)
SumAllStudentsYoI_189188TermData

SumAllStudentsYoI_189188YearData <- AllStudentsYoI_189188Data %>% group_by(c_ay) %>% 
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(c_ay, .keep_all=TRUE) %>% arrange(c_ay)
SumAllStudentsYoI_189188YearData

SumAllStudents_189188YearData <- AllStudents_189188Data %>% group_by(c_ay) %>% 
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(c_ay, .keep_all=TRUE) %>% arrange(c_ay)
SumAllStudents_189188YearData

SumAllStudents_189188TermData <- AllStudents_189188Data %>% group_by(term) %>% 
  summarize(totalunits = sum(units), n=n(), grad_year=grad_year, pascd=pascd, qtr=c_qtr) %>% 
  distinct(term, .keep_all=TRUE) %>% arrange(term)
SumAllStudents_189188TermData

SumAllStudentsYoI_93YearData <- AllStudentsYoI_93Data %>% group_by(c_ay) %>% 
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(c_ay, .keep_all=TRUE) %>% arrange(c_ay)
SumAllStudentsYoI_93YearData

SumAllStudents_93TermData <- AllStudents_93Data %>% group_by(term) %>% 
  summarize(totalunits = sum(units), n=n(), grad_year=grad_year, pascd=pascd, qtr=c_qtr) %>% 
  distinct(term, .keep_all=TRUE) %>% arrange(term)
SumAllStudents_93TermData


## Tabulate year data

AllYearly189188ProjectData <- SumMEFinished_189188YearData %>% 
  left_join(SumAEFinished_189188YearData, by="c_ay") %>%
  left_join(SumAllStudentsYoI_189188YearData, by="c_ay") %>%
  rename(year=c_ay,ME_Units=totalunits.x,ME_Students=n.x,AE_Units=totalunits.y,AE_Students=n.y,All_Units=totalunits,All_Students=n)
AllYearly189188ProjectData

AllYearly93ProjectData <- SumMEFinished_93YearData %>% 
  left_join(SumAEFinished_93YearData, by="c_ay") %>%
  left_join(SumAllStudentsYoI_93YearData, by="c_ay") %>%
  rename(year=c_ay,ME_Units=totalunits.x,ME_Students=n.x,AE_Units=totalunits.y,AE_Students=n.y,All_Units=totalunits,All_Students=n)
AllYearly93ProjectData


Check <- AllTranscriptDataV3 %>% filter((course=="ENGRMAE93") & (degmajor == "279")) %>% arrange(term)



####### TE Data


# Add up total TE units taken by ME degmajor students (adjusted by -11)
#SumMEFinished_TEDataAdj <- AllMEFinished_TEData %>% group_by(id,grad_year,degmajcd,first_year,pascd) %>% 
#  summarize(totalunits = sum(units), n=n()) %>% 
#  distinct(id, .keep_all=TRUE) %>% mutate(totalunitsadj=totalunits-11) %>% arrange(totalunitsadj)

SumMEFinished_TEDataAdj <- AllMEFinished_TEData %>% group_by(id) %>% 
  summarize(totalunits = sum(units), n=n(), grad_year=grad_year, pascd=pascd) %>% 
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
SumMEFinished_TEDataAdjFilt <- AllMEFinished_TEData %>% group_by(id,grad_year,degmajcd,first_year,pascd) %>% 
  summarize(totalunits = sum(units), n=n()) %>% 
  distinct(id, .keep_all=TRUE) %>% mutate(totalunitsadj=totalunits-11) %>% 
  filter(totalunitsadj %in% (0:40)) %>% arrange(totalunitsadj)



###### Total TE Units to 189 Units



JoinMEFinished_TE189188Data <- SumMEFinished_TEDataAdj %>% left_join(SumMEFinished_189188Data,by="id") %>% drop_na() %>%
  mutate(ratio=((totalunits.y-3)/totalunitsadj)*100)





ggplot(SumMEFinished_189188Data, aes(x=totalunits, fill=grad_year, color=grad_year)) + 
  geom_histogram(binwidth = 1, center=0) +
  geom_vline(aes(xintercept=mean(totalunits)),color="blue", linetype="dashed", size=1) +
  scale_color_brewer(palette="Dark2") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  #scale_x_continuous(breaks=seq(0,34,2)) +
  ggtitle("Total 189+188 Units Taken by MAEs \n (missing 195 at the moment)") +
  xlab("Total Units") + ylab("Number of Unique Students")

ggplot(SumMEFinished_TEDataAdj, aes(x=totalunitsadj, fill=grad_year, color=grad_year)) + 
  geom_histogram(binwidth = 1, center=0) +
  geom_vline(aes(xintercept=mean(totalunitsadj)),color="blue", linetype="dashed", size=1) +
  scale_color_brewer(palette="Dark2") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  # scale_x_continuous(breaks = round(seq(min(SumMEFinished_TEDataAdj$x), max(SumMEFinished_TEDataAdj$x), by = 0.5),1))+
  # scale_x_continuous(breaks=seq(0,34,2)) +
  ggtitle("Total TE Units Taken by MEs \n (subtracted 11 units for MEs)") +
  xlab("Total Units (Adjusted)") + ylab("Number of Unique Students")


ggplot(JoinMEFinished_TE189188Data, aes(x=ratio, fill=grad_year.x, color=grad_year.x)) + 
  geom_histogram(binwidth = 1, center=0) +
  geom_vline(aes(xintercept=mean(ratio)),color="blue", linetype="dashed", size=1) +
  scale_color_brewer(palette="Dark2") +
 # scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  # scale_x_continuous(breaks = round(seq(min(SumMEFinished_TEDataAdj$x), max(SumMEFinished_TEDataAdj$x), by = 0.5),1))+
  # scale_x_continuous(breaks=seq(0,34,2)) +
  ggtitle("Fraction of 189 Units in Total TE for MEs \n (subtracted 11 TE units and 3 189 units)") +
  xlab("Total Units (Adjusted)") + ylab("Number of Unique Students")


ggplot(JoinMEFinished_TE189188Data, aes(x=ratio, fill=grad_year.x, color=grad_year.x)) + 
  geom_histogram(binwidth = 1, center=0) +
  #geom_vline(aes(xintercept=mean(ratio)),color="blue", linetype="dashed", size=1) +
  scale_color_brewer(palette="Dark2") +
  # scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  # scale_x_continuous(breaks = round(seq(min(SumMEFinished_TEDataAdj$x), max(SumMEFinished_TEDataAdj$x), by = 0.5),1))+
  scale_x_continuous(breaks=seq(0,40,2),limits=c(0,40)) +
  ggtitle("Fraction of 189 Units in Total TE for MEs \n (subtracted 11 TE units and 3 189 units)") +
  xlab("Total Units (Adjusted)") + ylab("Number of Unique Students")



ggplot(JoinMEFinished_TE189188Data, aes(x=ratio, fill=grad_year.x, color=grad_year.x)) + 
  geom_histogram(binwidth = 1, center=0) +
  #geom_vline(aes(xintercept=mean(ratio)),color="blue", linetype="dashed", size=1) +
  scale_color_brewer(palette="Dark2") +
  # scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  # scale_x_continuous(breaks = round(seq(min(SumMEFinished_TEDataAdj$x), max(SumMEFinished_TEDataAdj$x), by = 0.5),1))+
  scale_x_continuous(breaks=seq(0,40,2),limits=c(0,40)) +
  ggtitle("Ratio of Total TE units by 189 units Taken by MEs \n (subtracted 11 TE units and 3 189 units)") +
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
AllMEFinished_Repeat189Data <- AllMEFinished_189188Data %>% group_by(id) %>% summarise(n = n()) %>% arrange(n)
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


