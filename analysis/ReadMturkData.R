getStudyData <- function() {
Studyraw <- read.csv("~/Documents/Lab/Gureckis/initial_accuracyXSL/initial-accuracy-exp/analysis/data/initial_accuracy1_study_data.csv",header = TRUE)
Testraw <- read.csv("~/Documents/Lab/Gureckis/initial_accuracyXSL/initial-accuracy-exp/analysis/data/initial_accuracy1_test_data.csv", header = TRUE)

testSs = unique(Testraw$uniqueId)


# Add column to track trial number
addTrialIndex <- function(Studyraw) {
   studSs = unique(Studyraw$uniqueId)
   Studyraw$trial = NA
   for(id in studSs) {
     studInd = which(Studyraw$uniqueId==id)
     trNum = 1
     for(i in 1:length(studInd)) {
       Studyraw[studInd[i],]$trial = trNum
       if(i%%2==0) trNum = trNum + 1
     }
   }
   return(Studyraw)
}

Studyraw = addTrialIndex(Studyraw)

# Join stimulus sets so each row is one trial
stim1 <- seq(1, nrow(Studyraw), 2) # Indecies for first stimulus set
stim2 <- seq(2, nrow(Studyraw), 2) # Indecies for second stimulus set
StudyDat <- Studyraw[-(stim2),] 
StudyDat$obj1 = Studyraw$obj[stim1]
StudyDat$obj2 = Studyraw$obj[stim2]
StudyDat$word1 = Studyraw$word[stim1]
StudyDat$word2 = Studyraw$word[stim2]
StudyDat <- subset(StudyDat, uniqueId%in%testSs) # Remove subjects who didnt finish

# Add original indecies for each subjects stimuli
for(i in 1:nrow(StudyDat)){
  sub <- StudyDat$uniqueId[i]
  subTestDat <- subset(Testraw, uniqueId%in%sub) 
  #subStudyDat <- subset(StudyDat, uniqueId%in%sub)
  StudyDat$o1ind[i] <- subTestDat$corr_obj_ind[which(subTestDat$correctAns==StudyDat$obj1[i])]
  StudyDat$o2ind[i] <- subTestDat$corr_obj_ind[which(subTestDat$correctAns==StudyDat$obj2[i])]
  StudyDat$w1ind[i] <- subTestDat$word_ind[which(subTestDat$word==StudyDat$word1[i])]  
  StudyDat$w2ind[i] <- subTestDat$word_ind[which(subTestDat$word==StudyDat$word2[i])]  
}

StudyDat <- StudyDat[,c("condition", "uniqueId", "obj1", 
                        "obj2", "word1", "word2","trial","o1ind","o2ind","w1ind","w2ind")]
return(StudyDat)
}


study = getStudyData()

# simulateSubject <- function(par, sstud) {
#   for(t in trials) {
#     
#   }
# }
# 
# simulateAllSubjects <- function() {
#   for(s in subjects) {
#     simulateSubject
#   }
# }
