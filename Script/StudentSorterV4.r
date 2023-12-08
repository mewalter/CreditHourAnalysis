
library("tidyverse")
library("openxlsx")    # library("readxl") # this is the tidyverse installed package
library("scales")
library("lubridate")
library("googledrive")
library("googlesheets4")

#library("pastecs")
#library("anytime")

path <- "H:/Shared drives/ME Senior Design/AY2020_21/Winter21/DataReduction"
if( .Platform$OS.type == "unix" )
  path <- "~/GoogleSDP/AY2020_21/Winter21/DataReduction"
setwd(path)

# get course codes and advisor names and put into a lookup file
gs4_get("1sed-Ui_KYcmvXd5hdVl626hFkeCyIMAs10mJqUKfG70")    # get sheet info 
RawProjInfo <- read_sheet("1sed-Ui_KYcmvXd5hdVl626hFkeCyIMAs10mJqUKfG70", 
                          sheet="W21ProjectInfo")
Lookup189 <- na.omit(RawProjInfo  %>% select(c(1,2,4)) )
Lookup93 <- na.omit(RawProjInfo  %>% select(c(1,3,4)) )


# get 189 students with their projects and advisors (from merged WebRoster on googlesheets)
gs4_get("1T3_0jpyVuysKq0CPBdFk__czEjP1jjl5rXviT8RNjEM")    # get sheet info
FileData189 <- read_sheet("1T3_0jpyVuysKq0CPBdFk__czEjP1jjl5rXviT8RNjEM", 
                          sheet="W21Current189", col_types = "c")
Complete189 <- FileData189  %>% select(1:5) %>% add_column(Class = "MAE189", .before = "Email") # %>% slice(2:n()) # careful with the slice (2vs3)
%names(Complete189)[names(Complete189) == "Course\nCode" "Class"] <- '189CC'
names(Complete189)[3] <- '189CC'
Complete189 <- Complete189 %>% left_join(Lookup189, by="189CC")  # does NOT remove duplicates
FileData189$Name[duplicated(FileData189$Name)]  # find duplicates


# get 93 students with their projects and advisors (from merged WebRoster on googlesheets)
gs4_get("1T0IgwubsqR-X2bIYPgKr-bWK-H5WEmi4lx0Mx9uxogo")    # get sheet info
FileData93 <- read_sheet("1T0IgwubsqR-X2bIYPgKr-bWK-H5WEmi4lx0Mx9uxogo", 
                         sheet="W21Current93", col_types = "c")
Complete93 <- FileData93  %>% select(1:5) %>% add_column(Class = "MAE93", .before = "Email") # %>% slice(2:n())  # careful with the slice (2vs3)
%names(Complete93)[names(Complete93) == 'Course'] <- '93CC'
names(Complete93)[3] <- '93CC'
# Complete93 <- Complete93 %>% drop_na %>% unique %>% left_join(Lookup93, by="93CC") # removes duplicates
Complete93 <- Complete93 %>% drop_na %>% left_join(Lookup93, by="93CC") # does NOT remove duplicates
FileData93$Name[duplicated(FileData93$Name)] # find duplicates


write.csv(Complete189, file = "Complete189.csv",row.names=FALSE)
write.csv(Complete93, file = "Complete93.csv",row.names=FALSE)




############ Rosters


# get 195 rosters with discussion section codes and project/adviser from canvas grades export
All195Grades <- read.csv("2020-12-01T0856_Grades-MAE189_and_MAE195_Fall20.csv",stringsAsFactors=FALSE)    # load 195 canvas
All195Grades$DiscSec <- str_extract(All195Grades$Section,"A[0-9]+")                              # extract discussion section
All195Grades$X195CC <- str_extract(All195Grades$Section, "(?<=\\().*?(?=\\))")      # looks behind for an open parenthesis ?<=\\(, looks ahead for a closing parenthesis ?=\\), and grabs everything in the middle (lazily) .+?, in other words (?<=\\().+?(?=\\))
names(All195Grades)[names(All195Grades) == "SIS.Login.ID"] <- "UCInetID"             # need to re-name column
All195Grades$Student <- str_to_upper(All195Grades$Student)                         # change to all upper-case
All195Grades$UCInetID <- str_to_upper(All195Grades$UCInetID)
Associate195Proj <- Complete189 %>% select(UCInetID,ProjectName,Advisors)      # get selected colums from lookup table
All195Grades <- All195Grades %>% left_join(Associate195Proj,by="UCInetID")        # execute the lookup

All195Grades[is.na(All195Grades$ProjectName),]           # any missing? 

Final195Roster <- All195Grades %>% 
  select(Student,UCInetID,DiscSec,ProjectName,Advisors) %>% 
  arrange(desc(ProjectName))
write.csv(Final195Roster, file = "Final195Roster.csv",row.names=FALSE)

# get 93 rosters with project/adviser from canvas grades export
All93Grades <- read.csv("2020-05-02T0055_Grades-MAE93_Projects.csv",stringsAsFactors=FALSE)    # load 93 canvas
names(All93Grades)[names(All93Grades) == "SIS.Login.ID"] <- "UCInetID"             # need to re-name column
All93Grades$Student <- str_to_upper(All93Grades$Student)                         # change to all upper-case
All93Grades$UCInetID <- str_to_upper(All93Grades$UCInetID)
Associate93Proj <- Complete93 %>% select(UCInetID,ProjectName,Advisors)      # get selected colums from lookup table
All93Grades <- All93Grades %>% left_join(Associate93Proj,by="UCInetID")        # execute the lookup

All93Grades[is.na(All93Grades$ProjectName),]         # Any missing? 
Final93Roster <- All93Grades %>% select(Student,UCInetID,ProjectName,Advisors) %>% arrange(desc(ProjectName))
write.csv(Final93Roster, file = "Final93Roster.csv",row.names=FALSE)




write.csv(Complete189, file = "Complete189.csv",row.names=FALSE)
write.csv(Complete93, file = "Complete93.csv",row.names=FALSE)














############ Bar Plots

plotting_df <-
  bind_rows(Complete189,Complete93) %>% select(7,4) %>%  
  group_by(Class, ProjectName) %>% 
  summarise(Freq = n())
#order by number of 189 studetns
temp_df <-
  plotting_df %>% 
  filter(Class == "MAE189") %>% 
  arrange(Freq)
the_order <- temp_df$ProjectName
# 3 projects have no 189 students
the_order <- prepend(the_order,c("ROCKET PROJ I","Rocket Project Solid","UAV FORGE I"))

# side-by-side (horizontal)
p1 <- 
  plotting_df %>% 
  ggplot(aes(x = ProjectName, weight = Freq, fill = Class)) +
  geom_bar(position = "dodge", width = 0.75) +
  # NB: use the above to plot bars in the certain order
  coord_flip() +
  scale_x_discrete(limits = the_order) +
  labs(x = "", y = "Number of Students") +
  scale_fill_brewer(breaks=c("MAE93", "MAE189"), palette = "Set1") +
  # NB: use the above to change the default order and color of legend
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14), 
        plot.title = element_text(size=14),
        legend.text = element_text(size=9),
        panel.background = element_rect(fill =  "grey90"))
print(p1)

# stacked (horizontal)
p2 <- 
  plotting_df %>% 
  ggplot(aes(x = ProjectName, y = Freq, group = Class, fill = Class)) +
  # NB: use "y = Freq" instead of "weight = Freq"
  geom_bar(stat = "identity", position = "stack", width = 0.75) +
  coord_flip() +
  scale_x_discrete(limits = the_order) +
  # NB: use the above to plot the bars in order
  labs(x = "", y = "Number of Students") +
  scale_fill_brewer(breaks=c("MAE93", "MAE189"), palette = "Set1") +
  # NB: use the above to change the default order and color of legend
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14), 
        plot.title = element_text(size=14),
        legend.text = element_text(size=9),
        panel.background = element_rect(fill =  "grey90"))
print(p2)

# back-to-back plot
plottingb2b_df <- as_factor(plotting_df) %>% mutate(Freq = if_else(Class == "MAE189", -Freq, Freq))
p <- 
  plottingb2b_df %>% 
  ggplot(aes(x = ProjectName, y = Freq, group = Class, fill = Class)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  scale_x_discrete(limits = the_order) +
  # another trick!
  scale_y_continuous(breaks = seq(-75, 75, 5), 
                     labels = abs(seq(-75, 75, 5))) +
  labs(x = "", y = "Count", title = "Project Enrollments") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill =  "grey90")) +
  # reverse the order of items in legend
  # guides(fill = guide_legend(reverse = TRUE)) +
  # change the default colors of bars
  scale_fill_manual(values=c("red", "blue"),
                    name="",
                    breaks=c("MAE93", "MAE189"),
                    labels=c("MAE93", "MAE189")) 
print(p)







# add project and advisor to the list of students who are missing an assignment
All189Missing <- read.csv("Grades-MAE189+195_Fall_2019_UCLC.csv",stringsAsFactors=FALSE,header=FALSE)
All189Missing$Student <- str_to_upper(sub("\\s(Remove).*","",All189Missing$V1))
Search189 <- All195Grades %>% select(Student,Project.Name,UCInetID,Advisor.s.)
# Search189$Student <- sub("(\\w+),\\s(\\w+)","\\2 \\1", Search189$Student)
All189Missing <- All189Missing %>% left_join(Search189,by="Student")        # execute the lookup
All189Missing$UCInetID <- paste(All189Missing$UCInetID, "@UCI.EDU", sep="") 
write.csv(All189Missing, file = "temp1.csv",row.names=FALSE)


All93Missing <- read.csv("Grades-MAE93_Projects_UCLC.csv",stringsAsFactors=FALSE,header=FALSE)
All93Missing$Student <- str_to_upper(sub("\\s(Remove).*","",All93Missing$V1))
Search93 <- All93Grades %>% select(Student,Project.Name,UCInetID,Advisor.s.)
# Search189$Student <- sub("(\\w+),\\s(\\w+)","\\2 \\1", Search189$Student)
All93Missing <- All93Missing %>% left_join(Search93,by="Student")        # execute the lookup
All93Missing$UCInetID <- paste(All93Missing$UCInetID, "@UCI.EDU", sep="") 
write.csv(All93Missing, file = "temp2.csv",row.names=FALSE)












Tabulated189Enroll$Project.Name <- RelevantColumns[match(Tabulated189Enroll$189CC, RelevantColumns$189CC),"Project.Name"]
Tabulated189Enroll$Advisors <- RelevantColumns[match(Tabulated189Enroll$189CC, RelevantColumns$189CC),"Advisor.s."]


#split Name into two columns
splits <- str_split_fixed(df$Name, ", ", 2)
#now merge these two columns the other way round
df$Name <- paste(splits[,2], splits[,1], sep = ' ')




All189Grades$Project.Name <- NA

All189Grades %>%
  select(-Project.Name) %>%
  left_join(RelevantColumns, by="189CC")


alldata$market<-
  
  with(RelevantColumns, Project.Name[match(RelevantColumns$Project.Name, 189CC)])





All93Grades <- read.csv("2019-10-26T2040_Grades-MAE93_Projects.csv")

All189Missing <- read.csv("2019-10-26T2042_Grades-MAE189+195_Fall_2019_Waiver.csv",stringsAsFactors=FALSE,header=FALSE)
All189MissingNames <- sub("\\s(Remove).*","",All189Missing$V1)



str_split_fixed(All189Missing,"Remove",n=1)
All93Missing <- read.csv("2019-10-26T2040_Grades-MAE93_Projects_Waiver.csv",header=FALSE)





All93Lines$X1 <- str_replace_all(All93Lines$X1,"[[:space:]]","_")
Project93Lines <- na.omit(All93Lines %>% extract(X1, c("ProjName", "Code"), regex = "^(.*?)\\_____Course_Code_([[:alnum:]]+)"))




df <- data.frame(x = c(NA, "a-b", "a-d", "b-c", "d-e"))
df %>% extract(x, "A")




AllNames <- RelevantCols %>% 
  select(X5) %>% 
  bind_rows(
    RelevantCols %>% 
      transmute(X5 = X13)
  )


AllNames %>%
  group_by_all() %>%
  filter(n() == 1)


data.frame(UniqueNames=union(RelevantCols$X5,RelevantCols$X13))

























# start a loop through all files 
for (files in file_list[1:length(file_list)]) {
  FileData <- read.xlsx(files, colNames = FALSE)
  if (!(FileData[1, 3] %in% TeamMembers[,2]))  {FileData[1, 3] <- NA}  # review name invalid)
  # cat("Working on file:", files, "completed by", FileData[1, 3], "\n")
  if (is.na(FileData[1, 3])) {
    cat("**** The",files,"from",FileData[1,8],"file has no reviewer name:",FileData[1, 3],"\n")  # Warn about blank reviewer
    next
  }
  
  if (is.na(FileData[1,1])|(FileData[1,1] != "Sp19f")) {
    cat("**** The",files,"from",FileData[1,8]," is not from the current template zip:",FileData[1, 3],"\n")  # Warn about blank reviewer
    next
  }
  
  TempData <- FileData %>% select(c(1:13)) %>%    # 13 columns including comments
    slice(4:n()) %>%                          # remove 4 rows
    add_column(Reviewer=FileData[1,3])        # reviewer is in row 1, column 4
  TempData[is.na(TempData)] = 1               # convert NA to 1
  TempData <- TempData[TempData[,4] != 1,]    # delete all rows with 1 in 4th column
  # Check for all 5s (just columns 3:11 in Data)
  NumEntries <- dim(TempData[,5:12])[1]*dim(TempData[,5:12])[2]
  Num5s <- sum(TempData[5:12]==5)
  Percent5s <- Num5s/NumEntries*100
  if (Percent5s == 100) {
    cat("**** ",FileData[1,3],"from",FileData[1,8],"gave all 5s. Shame on him/her. Dropping them!","\n")  # Bad reviewer. All 5s
    next
  }
  if (ifelse(any(ls() %in% "Data"),is.data.frame(get("Data")),FALSE)) {
    Data <- rbind(Data, TempData)
  } else {Data <- TempData}
  rm(TempData)
}

# convert columns to numerical values
Data[c(3:13)] <- sapply(Data[c(3:13)],as.numeric)

FinalData <- Data %>% group_by(X2) %>% summarise_all(funs(mean))
FinalData$Reviewer <- NULL
FinalData$X1 <- NULL
FinalData$X13 <- NULL
FinalData$AVG <- rowMeans(FinalData[,4:11])    # means for Q3 to Q10

FinalData$RevGiv <- 0
FinalData$RevRec <- 0
i <- 1
for (n in FinalData$X2) {
  FinalData$RevGiv[i] <- length(grep(n, Data[,14],fixed=TRUE)) # ???????????????????
  FinalData$RevRec[i] <- length(grep(n, Data[,2],fixed=TRUE))
  i <- i+1
}
# change received and given to integers
FinalData[c("RevGiv","RevRec")] <- sapply(FinalData[c("RevGiv","RevRec")],as.integer)

FinalData <- FinalData[c(1,12,14,13,4,5,6,7,8,9,10,11,2,3)]
names(FinalData) <- c("Name","Avg Score","Received","Given","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","PersonalFriend","ProjFamiliar")

#  rm(Data,FileData,FinalData)

############# Check if multiple files were submitted by the same person 
multi <- which(FinalData$Given > length(TeamMembers[,1]))
if (length(multi)) {
  cat("**********","Multiple submissions by:",FinalData$Name[multi],"**********",sep="\n")
}

# round data
FinalData <- data.frame(lapply(FinalData, function(y) if(is.numeric(y)) round(y, 2) else y)) 

AllStudentPeerAss <- rbind(AllStudentPeerAss,FinalData)
AllPeerAssTeamMembers <- rbind(AllPeerAssTeamMembers,TeamMembers)

write.csv(FinalData, file = "SUMMARYPeerAssessData.csv",row.names=FALSE)

remove(Data,FileData,FinalData,file_list)
remove(files,i,n,TeamMembers)
}

setwd(BaseDir)
write.csv(AllStudentPeerAss, file = "AllStudentPeerAss.csv",row.names=FALSE)
write.csv(AllPeerAssTeamMembers, file = "AllPeerAssTeamMembers.csv",row.names=FALSE)























#-------------------- Conditional formatting with xlsx package ---------------

cols <- sample(c(1:5), 1) # number of columns to vary to mimic this unknown
label <- rep(paste0("label ", seq(from=1, to=10)))
mydata <- data.frame(label)
for (i in 1:cols) {
  mydata[,i+1] <- sample(c(1:10), 10)
}
# exporting data.frame to excel is easy with xlsx package
sheetname <- "mysheet"
write.xlsx(mydata, "mydata.xlsx", sheetName=sheetname)
file <- "mydata.xlsx"
# but we want to highlight cells if value greater than or equal to 5
wb <- loadWorkbook(file)              # load workbook
fo <- Fill(foregroundColor="yellow")  # create fill object
cs <- CellStyle(wb, fill=fo)          # create cell style
sheets <- getSheets(wb)               # get all sheets
sheet <- sheets[[sheetname]]          # get specific sheet
rows <- getRows(sheet, rowIndex=2:(nrow(mydata)+1)     # get rows
                # 1st row is headers
                cells <- getCells(rows, colIndex = 3:(cols+3))       # get cells
                # in the wb I import with loadWorkbook, numeric data starts in column 3
                # and the first two columns are row number and label number
                
                values <- lapply(cells, getCellValue) # extract the values
                
                # find cells meeting conditional criteria 
                highlight <- "test"
                for (i in names(values)) {
                  x <- as.numeric(values[i])
                  if (x>=5 & !is.na(x)) {
                    highlight <- c(highlight, i)
                  }    
                }
                highlight <- highlight[-1]
                
                lapply(names(cells[highlight]),
                       function(ii)setCellStyle(cells[[ii]],cs))
                
                saveWorkbook(wb, file)
                
                
                
                
                
                grades <- read.csv(file="GradeEssentials.csv",skip=0,head=TRUE,sep=",")
                names(grades) <- sub(" ", ".", names(grades))
                
                FinalStats <- stat.desc(grades$Final.Final.Score, basic=F)
                FinalMean <- paste("Final Exam Mean:",round(FinalStats[2],digits=1))
                FinalSTD <- paste("Final Exam S.D.:",round(FinalStats[6],digits=1))
                TotalStats <- stat.desc(grades$Total.Points, basic=F)
                TotalMean <- paste("Total Points Mean:",round(TotalStats[2],digits=1))
                TotalSTD <- paste("Total Points S.D.:",round(TotalStats[6],digits=1))
                
                hist(grades$Final.Final.Score, breaks = 100, xaxp=c(0,100,50),main="Final Exam Mean: 67.6")
                text(20,8,"S.D. = 16.8%")
                #axis(side=1, at=seq(0,100,10),labels)
                hist(grades$Total.Points, breaks = 100, xaxp=c(0,100,50),main="Total Points Mean: 67.6")
                text(20,8,"S.D. = 13.7")
                #axis(side=1, at=seq(40,100,10),labels)
                
                ggplot(data=grades, aes(x=grades$Total.Points)) + 
                  geom_histogram(breaks=seq(40, 100, by=1),
                                 col="black", 
                                 fill="white", 
                                 alpha = .2) + 
                  labs(title="Histogram Total Points", x="Total Points", y="Count") +
                  scale_x_continuous(breaks = pretty(grades$Total.Points, n = 60)) +
                  theme(axis.text.x = element_text(size=8, angle=90, vjust = 0.5, hjust=1)) +
                  annotate("text", label = TotalMean, x = 50, y = 12) +
                  annotate("text", label = TotalSTD, x = 90, y = 12)
                
                
                # plot
                ggplot(grades, aes(x=grades$exam, y=grades$Midterm, fill=grades$exam)) +
                  geom_boxplot(alpha=0.4) +
                  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
                  theme(legend.position="none") +
                  scale_fill_brewer(palette="Set3") +
                  geom_jitter(width = 0.25)
                
                