library(shiny)
library(openxlsx)

ini.global <- function() {
  logged = FALSE
  unlogged = FALSE
  return(list(logged=logged, unlogged=unlogged))
}

##A. Initiation for student module
ini.student <- function(globalvar) {
  studentvar <- list()
  
  studentvar$course_info <- read.xlsx(file.path("info", "Course Requirement Info_20200708_used.xlsx"), 1, sheet = 1, rows = c(2:106),colNames = T) #3:107 
  studentvar$student_info <- read.xlsx(file.path("info", "TA Info_20200708.xlsx"), 1, sheet = 1, rows = c(3:101), colNames = T) #3:110  rows = c(3:110),
  studentvar$course_names <- studentvar$course_info[, "Course.Code"]
  
  return(studentvar)
}


##B. Initiation for admin module
ini.admin <- function(globalvar, studentvar) {
  adminvar <- list()
  return(adminvar)
}

##C. Initiation for CLs module
ini.CLs <- function(globalvar, studentvar) {
  CLsvar <- list()
  return(CLsvar)
}
