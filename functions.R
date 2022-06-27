library(shiny)
library(shinythemes)
library(shinysky)
library(mailR)
library(DT)


generate.stupw <- function() {
  student_info <- openxlsx::read.xlsx(file.path("info", "TA Info_20200708.xlsx"), sheet = 1, rows = c(3:101), colNames = T)
  ##return a data frame
  passgen.multi <- function(vec, passwidth=8) {
    ##password generator
    passgen <- function(passwidth=8) {
      samp<-c(0:9,letters,LETTERS,"!", "@", "#", "%", "&", "(", ")", "*")
      paste(sample(samp,passwidth),collapse="")
    }
    
    data.frame(username=vec, password=sapply(1:length(vec), function(x) passgen(passwidth)), stringsAsFactors = F)
  }
  
  style <- "notification"
  ## Generate passwd
  withProgress(message = 'And this also', detail = "This other thing",
               style = style, value = NULL, {
                 studentPasswd <- passgen.multi(student_info[, "Student.No"])
                 save(studentPasswd, file="studentPasswd.RData")
               })
  

  ## send email notification
  withProgress(message = 'Sending emails', detail = "begins", value = 0, {
    for(i in 1:nrow(studentPasswd)) {
      email.address <- student_info[student_info[, "Student.No"] == studentPasswd[i, "username"], "Email"]
      student.name <- student_info[student_info[, "Student.No"] == studentPasswd[i, "username"], "Name"]
      student.username <- studentPasswd[i, "username"]
      student.passwd <- studentPasswd[i, "password"]
      
      ## send emails
      if(!is.na(email.address)) {
        
        knitr::knit2html(file.path("email", "email_logininfo.Rmd"), output = "email_content.html", options = "")
        mailR::send.mail(from = "bmstas@cityu.edu.hk",
                         to = c(email.address,"bmstas@cityu.edu.hk"),
                         subject = "Username and password for BMS online TA system",
                         body = "email_content.html",
                         html = TRUE,
                         inline = TRUE,
                         smtp = list(host.name = "outlook-apaccentral.office365.com", port = 587,
                                     user.name = "bmstas@um.cityu.edu.hk", passwd = "XXXXXX", tls = TRUE),
                         authenticate = TRUE,
                         send = TRUE)
        #Sys.sleep(5)
        write(paste("Finished sending email no.", i, Sys.time()), file=file.path("email", "email_tracking_passwd.log"), append = T)
        incProgress(1/nrow(studentPasswd), detail = paste("finished no.", i,  "to", student.name))
      }
    }
  })  
  
  
}

##administrator, update student information
update.stutab <- function(globalvar, studentvar) {
  stutab <- list()
  stuinfo <- studentvar$student_info
  rownames(stuinfo) <- as.character(stuinfo[, "Student.No"])
  stu.ids <- as.character(stuinfo[, "Student.No"])

  tmp <- list.files(file.path("data", "student_final"), full.names = T)
  if(length(tmp) == 0) {
    return(NA)
  } else {
    ids <- do.call(rbind, strsplit(tmp, split = "[/.]"))
    ids <- ids[, ncol(ids)-1] #student.No
    names(tmp) <- ids

    if(! all(names(tmp)%in%stu.ids)) {
      return(NA)
    } else {
      for(stui in stu.ids) {
        if(stui %in% names(tmp)) {
          load(tmp[stui])
          stutab[[stui]] <- tmpL$choices
        } else {
          stutab[[stui]] <- c(choice1="", choice2="", choice3="", comment1="", comment2="", comment3="")
        }
      }
      if(length(stutab) > 0) {
        stutab <- do.call(rbind, stutab)
        stutab <- data.frame(stuinfo, stutab[rownames(stuinfo), ], check.names=F, stringsAsFactors = F)
        
      }
      return(stutab)
     
    }
  }
}

##optimize TA assignment
optimTAAssign <- function(globalvar) {

  stuproved <- file.exists(file.path("data", "admin_final", "student_submission.RData"))

  if(!stuproved) {
    return(NULL)
  }

  ## load student submission and course info
  load(file.path("data", "admin_final", "student_submission.RData"))
  assign_info <- read.xlsx(file.path("info", "Course Requirement Info_20200708_used.xlsx"), 1, sheet = 1, rows = c(2:106),colNames = T) 
  assign_info <- assign_info[, c("Year/.Semester", "Course.Code", "Course.Title", "Course.Leader", "No.of.TAs.required","Preferred.TA")]# ,"all.hours",
  assign_info <- assign_info[!is.na(assign_info[, "No.of.TAs.required"]), ]
  student_info <- read.xlsx(file.path("info", "TA Info_20200708.xlsx"), 1, sheet = 1, rows = c(3:101), colNames = T)
  
  ## init assign, match research area, average_load
  
  
  assign_info[, "TA.lacked"] <- assign_info[, "No.of.TAs.required"]
  assign_info[,"averageload"] <- assign_info$all.hours/assign_info$No.of.TAs.required
  student_submission$limit <- average_num <- ceiling(sum(assign_info$`No.of.TAs.required`)/nrow(student_submission))
  TA <- rep(list(NULL), nrow(assign_info)) #each course have a list
  names(TA) <- assign_info[, "Course.Code"]
  student_names <- rep(list(NULL), nrow(assign_info))
  names(student_names) <- assign_info[, "Course.Code"]
  
  student_submission$time <- average_load <-  ceiling(sum(assign_info$all.hours)/nrow(student_submission))
  
  ##assign preferred.  1st place
  prefer_index <- which(!is.na(assign_info[,"Preferred.TA"]))
  for (i in prefer_index) {
    if(length(str_split(assign_info[i,"Preferred.TA"],", ")[[1]])==assign_info[i,"No.of.TAs.required"]){
      TA[[i]] <- str_split(assign_info[i,"Preferred.TA"],", ")[[1]]
      #student_names[[i]] <-student_info[match(TA[[i]],student_info$Student.No),"Name"] 
        
      assign_info[i, "TA.lacked"] <- 0
    }else{
      TA[[i]] <- str_split(assign_info[i,"Preferred.TA"],", ")[[1]]
      #student_names[[i]] <-student_info[match(TA[[i]],student_info$Student.No),"Name"]
      assign_info[i, "TA.lacked"] <- assign_info[i,"No.of.TAs.required"]-length(str_split(assign_info[i,"Preferred.TA"],", ")[[1]])
    }
    pre_stu <-apply(as.data.frame(TA[[i]]) ,1,function(x){which(student_submission[,"Student.No"] == x) })
    student_submission[pre_stu,"limit"] <- student_submission[pre_stu,"limit"]-1
    student_submission[pre_stu,"time"] <- average_load - (assign_info[i,"all.hours"]/assign_info[i,"No.of.TAs.required"])
  }

  ## assign student choice. 2nd place  
  student_id <-student_submission$Student.No
  for(choice in c("choice1", "choice2", "choice3")) {
    ## assign students by their choices
    ## assign students to those courses which are not full
    for(idx in 1:nrow(assign_info)) {
      ta_num <- assign_info[idx, "TA.lacked"]
      if(ta_num  > 0) {
        candidate <- student_id[which(student_submission[ , "limit"] ==average_num)]

        candidate <- candidate[which(student_submission[candidate, choice] == assign_info[idx, "Course.Code"])]
       
        if(length(candidate) > 0) {
          ## assign candidates
          set.seed(123)
          if(ta_num < length(candidate)) { candidate <- sample(candidate, ta_num,replace = F) }
          TA[[idx]] <- c(TA[[idx]], candidate)
          #student_names[[idx]] <-c(student_names[[idx]],) student_info[match(TA[[i]],student_info$Student.No),"Name"]
          ## update limits,workload
          assign_info[idx, "TA.lacked"] <- assign_info[idx, "TA.lacked"] - length(candidate)
          student_submission[candidate , "limit"] <- student_submission[candidate , "limit"] - 1
          # for student who has been assigned
          student_submission[candidate , "time"] <- student_submission[candidate , "time"]-(assign_info[idx,"all.hours"]/assign_info[idx,"No.of.TAs.required"])
        }
      }
    }
  }

  ##for students who still no assignment   further modification with matched research area()
  ##more loads than the rest
  for(restidx in 1:nrow(assign_info)) {
    ta_num <- assign_info[restidx, "TA.lacked"]
    if(ta_num  > 0) {
      therest <- student_submission$Student.No[which(student_submission[ , "limit"] == average_num)]
      
      if(length(therest) > 0) {
        ## assign candidates
        set.seed(123)
        if(ta_num < length(therest)) { therest <- sample(therest, ta_num,replace = F) }
        TA[[restidx]] <- c(TA[[restidx]], therest)
        
        ## update limits,workload
        assign_info[restidx, "TA.lacked"] <- assign_info[restidx, "TA.lacked"] - length(therest)
        student_submission[therest , "limit"] <- student_submission[therest , "limit"] - 1
        student_submission[therest , "time"] <- student_submission[therest , "time"]-(assign_info[restidx,"all.hours"]/assign_info[restidx,"No.of.TAs.required"])
      }
    }
  }
  
  ##for the last course(s) which still has/have surplus
  sortstudent <- student_submission$Student.No[order(student_submission[, "time"],decreasing = T)] #student_no
  restcourse <-  which(assign_info[, "TA.lacked"] > 0) #index
  sortcourse <- restcourse[order(assign_info[restcourse,"averageload"],decreasing = T)]   #sorted index     
  for (restidx in sortcourse) {
    ta_num <- assign_info[restidx, "TA.lacked"]
    ind <- setdiff(sortstudent[1:9],TA[[restidx]]) #most =8,so 8+1 to make sure. ind=student_No
    therest <- ind[1:ta_num]
    TA[[restidx]] <- c(TA[[restidx]], therest)
    
    #update
    sortstudent <- sortstudent[-(match(ind,sortstudent[1:9]))]
  }
  #finish
  
  ##integrate
  for (index in names(TA)) {
    student_names[[index]] <-student_info[match(TA[[index]],student_info$Student.No),"Name"] 
  }
  
  
  assign_info$TA <- sapply(TA, function(x) paste(x, collapse = ", "))
  assign_info$Student <- sapply(student_names, function(x) paste(x, collapse = ", "))

  return(assign_info)
}

# send preliminary emails to course leaders
send_pre_email2cl <- function() {
  load(file=file.path("data", "admin_final", "TA_assignment.RData")) #opttab
  newcourse <- openxlsx::read.xlsx(file.path("info", "Course Requirement Info_20200708_used.xlsx"), 1, sheet = 1, rows = c(2:106), colNames = T)
  student_info <- openxlsx::read.xlsx(file.path("info", "TA Info_20200708.xlsx"), 1, sheet = 1, rows = c(3:101), colNames = T) #3:111
  teacher_info <- openxlsx::read.xlsx(file.path("info", "Course Leader Info_20200708.xlsx"), 1, sheet = 1, rows = c(3:35), colNames = T) #3:32
  
  ## send email notification
  withProgress(message = 'Sending emails', detail = "begins", value = 0, { 
    length_record <- 0
    
    for (dim1 in 1:nrow(newcourse)) {
      if(length(unlist(str_split(newcourse[dim1,"Course.Leader.Email"],", ") )) >1){
        tmp <- 2
      }else{
        tmp <- 1
      }
      length_record <- c(length_record,tmp)
    }
    length_record <- length_record[-1]
    index_two <- which(length_record>1)
    if(length(index_two)>0){
      aaa <- unique(c(newcourse[-index_two,"Course.Leader.Email"],unlist(str_split(newcourse[index_two,"Course.Leader.Email"],", ")) ) )
    }else{
      aaa <- unique(newcourse[,"Course.Leader.Email"] ) 
    }
    
    
    for(i in aaa[20:28]) { #aaa[3:length(aaa)]
      cls_id <- i
      
      idx <- grep(cls_id, newcourse[, "Course.Leader.Email"]) #index
      
      res_leader <- newcourse[idx, 1:3]#contain all course for a leader
      email.address <- teacher_info[which(teacher_info[, "Email"] == cls_id), "Email"]  #"linqi23-c@my.cityu.edu.hk" 
      if(any(idx==index_two)){
        name_tmp <- unlist(str_split(newcourse[index_two,"Course.Leader"],", "))
        cl.name <-name_tmp[grep(cls_id, unlist(str_split(newcourse[index_two,"Course.Leader.Email"],", ")))]
      }else{
        cl.name <-newcourse[newcourse[, "Course.Leader.Email"] == cls_id, "Course.Leader"][1]
      }
      xxx <- 1
      for (j in 1:nrow(res_leader)) {
        
        student.id <- unlist(strsplit(as.character(opttab[idx[xxx], "TA"]), split=", "))
        #student.name <- unlist(strsplit(as.character(opttab[i, "Student"]), split=", "))
        # student.id <- unlist(strsplit(stuid, split=" ")) 
        res_t <- student_info[student_info[, "Student.No"] %in% student.id,c("Student.No","Name","Supervisor") ] #,"Research.Area"
        if(nrow(res_t) > 0) {
          res_t <- data.frame(res_leader[j, 1:3], res_t, check.names = F) #rep less = more
        }
        
        if(j==1){
          res <- res_t
        }else{
          res <- rbind(res,res_t) 
        }
        xxx <- xxx+1
        rm(res_t)
      }
      
      ## send emails
      if(!is.na(email.address)){
        knitr::knit2html(file.path("email", "email2cls_template.Rmd"), output = "email2cls_content.html", options = "")
        mailR::send.mail(from = "bmstas@cityu.edu.hk",
                         to = c(email.address,"bmstas@cityu.edu.hk"),  # c(email.address,"bmstas@cityu.edu.hk"), 
                         subject = "Notification of TA assignment results for course",
                         body = "email2cls_content.html",
                         html = TRUE,
                         inline = TRUE,
                         smtp = list(host.name = "outlook-apaccentral.office365.com", port = 587,
                                     user.name = "bmstas@um.cityu.edu.hk", passwd = "XXXXXXXX", tls = TRUE),
                         authenticate = TRUE,
                         send = TRUE)
        #Sys.sleep(5)
        write(paste("Finished sending email No.", cl.name, Sys.time()), file=file.path("email", "email_tracking_cls.log"), append = T)
        #incProgress(1/nrow(student_info), detail = paste("finished no.", i,  "to", student.name))
      }else{
        write(paste("Skipped sending email no.", cl.name, Sys.time()), file=file.path("email", "email_tracking_cls.log"), append = T)
        #incProgress(1/nrow(student_info), detail = paste("Skipped no.", i,  "to", student.name))
      }
      rm(res)
    }
  })
}


# send final emails to course leaders
send_finalemail2cl <- function() {
  load(file=file.path("data", "admin_final", "TA_assignment.RData")) #opttab
  newcourse <- openxlsx::read.xlsx(file.path("info", "Course Requirement Info_20200708_used.xlsx"), 1, sheet = 1, rows = c(2:106), colNames = T)
  student_info <- openxlsx::read.xlsx(file.path("info", "TA Info_20200708.xlsx"), 1, sheet = 1, rows = c(3:101), colNames = T) #3:111
  teacher_info <- openxlsx::read.xlsx(file.path("info", "Course Leader Info_20200708.xlsx"), 1, sheet = 1, rows = c(3:35), colNames = T) #3:32
  
  ## send email notification
  withProgress(message = 'Sending emails', detail = "begins", value = 0, { 
    length_record <- 0
    
    for (dim1 in 1:nrow(newcourse)) {
      if(length(unlist(str_split(newcourse[dim1,"Course.Leader.Email"],", ") )) >1){
        tmp <- 2
      }else{
        tmp <- 1
      }
      length_record <- c(length_record,tmp)
    }
    length_record <- length_record[-1]
    index_two <- which(length_record>1)
    
    if(length(index_two)>0){
      aaa <- unique(c(newcourse[-index_two,"Course.Leader.Email"],unlist(str_split(newcourse[index_two,"Course.Leader.Email"],", ")) ) )
    }else{
      aaa <- unique(newcourse[,"Course.Leader.Email"] ) 
    }
    
    for(i in aaa) {
      cls_id <- i
      
      idx <- grep(cls_id, newcourse[, "Course.Leader.Email"]) #index
      
      res_leader <- newcourse[idx, 1:3]#contain all course for a leader
      email.address <- teacher_info[which(teacher_info[, "Email"] == cls_id), "Email"]
      if(any(idx==index_two)){
        name_tmp <- unlist(str_split(newcourse[index_two,"Course.Leader"],", "))
        cl.name <-name_tmp[grep(cls_id, unlist(str_split(newcourse[index_two,"Course.Leader.Email"],", ")))]
      }else{
        cl.name <-newcourse[newcourse[, "Course.Leader.Email"] == cls_id, "Course.Leader"][1]
      }
      xxx <- 1
      for (j in 1:nrow(res_leader)) {
        
        student.id <- unlist(strsplit(as.character(opttab[idx[xxx], "TA"]), split=", "))
        #student.name <- unlist(strsplit(as.character(opttab[i, "Student"]), split=", "))
        # student.id <- unlist(strsplit(stuid, split=" ")) 
        res_t <- student_info[student_info[, "Student.No"] %in% student.id,c("Student.No","Name","Supervisor") ]
        if(nrow(res_t) > 0) {
          res_t <- data.frame(res_leader[j, 1:3], res_t, check.names = F) #rep less = more
        }
        
        if(j==1){
          res <- res_t
        }else{
          res <- rbind(res,res_t) 
        }
        xxx <- xxx+1
      }
        
        ## send emails
        if(!is.na(email.address)){
          knitr::knit2html(file.path("email", "email2cls_final_template.Rmd"), output = "email2cls_final_content.html", options = "")
          mailR::send.mail(from = "bmstas@cityu.edu.hk",
                           to = c("bmstas@cityu.edu.hk"), # c(email.address,"bmstas@cityu.edu.hk"),
                           subject = "Notification of TA assignment results for course",
                           body = "email2cls_final_content.html",
                           html = TRUE,
                           inline = TRUE,
                           smtp = list(host.name = "outlook-apaccentral.office365.com", port = 587,
                                       user.name = "bmstas@um.cityu.edu.hk", passwd = "XXXXXXX", tls = TRUE),
                           authenticate = TRUE,
                           send = TRUE)
          #Sys.sleep(5)
          write(paste("Finished sending email No.", cl.name, Sys.time()), file=file.path("email", "email_tracking_cls.log"), append = T)
          #incProgress(1/nrow(student_info), detail = paste("finished no.", i,  "to", student.name))
        }else{
          write(paste("Skipped sending email no.", cl.name, Sys.time()), file=file.path("email", "email_tracking_cls.log"), append = T)
          #incProgress(1/nrow(student_info), detail = paste("Skipped no.", i,  "to", student.name))
        }
      rm(res,res_t)
    }
    })
}
    

# send final emails to students
send_finalemail2student <- function() {
  
  load(file=file.path("data", "admin_final", "TA_assignment.RData"))
 
  student_info <- openxlsx::read.xlsx(file.path("info", "TA Info_20200708.xlsx"), 1, sheet = 1, rows = c(3:101), colNames = T)
  course_info <- openxlsx::read.xlsx(file.path("info", "Course Requirement Info_20200708_used.xlsx"), 1, sheet = 1, rows = c(2:106), colNames = T)
  ## send email notification
  withProgress(message = 'Sending emails', detail = "begins", value = 0, {
    for(i in 3:nrow(student_info)) {
      stu_id <- student_info$`Student.No`[i]
      
      idx <- grep(stu_id, opttab[, "TA"]) #index
      if(length(idx) > 0) {
        res <- course_info[idx, 1:5]
        
        email.address <- student_info[student_info[, "Student.No"] == stu_id, "Email"]  #   "linqi23-c@my.cityu.edu.hk"
        student.name <-student_info[student_info[, "Student.No"] == stu_id, "Name"]
        
        ## send emails
        if(!is.na(email.address)){
          knitr::knit2html(file.path("email", "email2student_content.Rmd"), output = "email2student_content.html", options = "")
          mailR::send.mail(from = "bmstas@cityu.edu.hk",
                           to = c(email.address,"bmstas@cityu.edu.hk"),
                           subject = "Notification of TA assignment results",
                           body = "email2student_content.html",
                           html = TRUE,
                           inline = TRUE,
                           smtp = list(host.name = "outlook-apaccentral.office365.com", port = 587,
                                       user.name = "bmstas@um.cityu.edu.hk", passwd = "XXXXXXXX", tls = TRUE),
                           authenticate = TRUE,
                           send = TRUE)
          #Sys.sleep(5)
          write(paste("Finished sending email no.", i, Sys.time()), file=file.path("email", "email_tracking_student.log"), append = T)
          #incProgress(1/nrow(student_info), detail = paste("finished no.", i,  "to", student.name))
        }else{
          write(paste("Skipped sending email no.", i, Sys.time()), file=file.path("email", "email_tracking_student.log"), append = T)
          #incProgress(1/nrow(student_info), detail = paste("Skipped no.", i,  "to", student.name))
        }
        
      } else {
        # write(paste("Skipped sending email no.", i, Sys.time()), file=file.path("email", "email_tracking_student.log"), append = T)
        # incProgress(1/nrow(student_info), detail = paste("Skipped no.", i,  "to", student.name))
      }

    }
  }) 
 
}
