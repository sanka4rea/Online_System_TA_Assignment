library(openxlsx)

fo_ta <- read.xlsx("./TA working file_20210803_to.xlsx", sheet = 1,colNames = T)
# test_ta <- read.xlsx("~/shiny/Format_used.xlsx",sheet = 1,colNames = T)
test_ta <- openxlsx::read.xlsx("./info/Course Requirement Info_20210721_v2.xlsx", sheet = 1, rows = c(2:500), colNames = T)
  # read.xlsx("~/shiny/test_ues_TA_assignment (5).xlsx",sheet = 2,colNames = T)
test_ta <- test_ta[,1:4]
final_ta <- test_ta

for (i in 1:25) {  #semA
  course_name <- test_ta[i,2]
  allSemA <- fo_ta[,"Sem.A"]
  allSemA <- lapply(allSemA, function(xxx){
    tmp <- str_split(xxx,", ")[[1]]
    return(tmp)
  })
  index <- lapply(1:length(allSemA), function(xxx){
    tmp <- grep(course_name,allSemA[[xxx]])
    if(length(tmp)>0){
      return(xxx)
    }else{
      return(NULL)
    }
  })
  index <- do.call(c,index)
  # index <- which(fo_ta[,"Sem.A"]==course_name )
  tmp <- NULL
  for (j in index) {
    tmp <- paste0(tmp,fo_ta[j,1],sep=", ")
  }
  studentNo <- substr(tmp, 1,(nchar(tmp)-2))
  
  tmp <- NULL
  for (z in index) {
    tmp <- paste0(tmp,fo_ta[z,2],sep=", ")
  }
  studentName <- substr(tmp, 1,(nchar(tmp)-2))
  
  final_ta[i,5] <- studentNo
  final_ta[i,6] <- studentName
  
}

titles <- c("Year/.Semester","Course.Code","Course.Title","Course.Leader","TA","Student")
colnames(final_ta) <- titles
rm(studentNo,studentName,allSemA)

for (i in 26:nrow(test_ta)) {  #semB
  course_name <- test_ta[i,2]
  allSemB <- fo_ta[,"Sem.B"]
  allSemB <- lapply(allSemB, function(xxx){
    tmp <- str_split(xxx,", ")[[1]]
    return(tmp)
  })
  index <- lapply(1:length(allSemB), function(xxx){
    tmp <- grep(course_name,allSemB[[xxx]])
    if(length(tmp)>0){
      return(xxx)
    }else{
      return(NULL)
    }
  })
  index <- do.call(c,index)
  #index <- which(fo_ta[,"Sem.B"]==course_name )
  tmp <- NULL
  for (j in index) {
    tmp <- paste0(tmp,fo_ta[j,1],sep=", ")
  }
  studentNo <- substr(tmp, 1,(nchar(tmp)-2))
  
  tmp <- NULL
  for (z in index) {
    tmp <- paste0(tmp,fo_ta[z,2],sep=", ")
  }
  studentName <- substr(tmp, 1,(nchar(tmp)-2))
  
  final_ta[i,5] <- studentNo
  final_ta[i,6] <- studentName
  
}

write.xlsx(final_ta,file = "./final_assign_from_GO_20210804.xlsx")
