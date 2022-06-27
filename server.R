
rm(list = ls())
library(shiny)
library(DT)
library(shinythemes)
library(shinysky)
library(mailR)

library(stringr)
#source("sql.R")

source("functions.R")
source("init.R")
source("ui_login.R")
source("ui_student.R")
source("ui_admin.R")

##1. Global setup
thistheme <- c("cerulean", "flatly", "readable", "spacelab", "united")[3]
globalvar <- ini.global()
studentvar <- list()
adminvar <- list()
CLsvar <- list()

##3. server setup
server = (function(input, output, session) {
  ##login status
  USER <- reactiveValues(logged = globalvar$logged, unlogged = globalvar$unlogged)
  
  ##1. login check
  observeEvent(input$Login, {
    logrole <- isolate(input$role)
    Username <- isolate(input$userName)
    Password <- isolate(input$passwd)
    
    ##
    if(logrole=="Student") {
      load("studentPasswd.RData")
      Id.username <- which(as.character(studentPasswd[, "username"]) == Username)
      Id.password <- which(as.character(studentPasswd[, "password"]) == Password)      
    }
    if(logrole=="Administrator") {
      load("adminPasswd.RData")
      Id.username <- which(as.character(adminPasswd[, "username"]) == Username)
      Id.password <- which(as.character(adminPasswd[, "password"]) == Password)                 
    }

    
    if (length(Id.username) > 0 & length(Id.password) > 0) {
      if (Id.username %in% Id.password) {
        USER$logged <- TRUE
        USER$unlogged <- FALSE
      } else {
        session$sendCustomMessage(type = 'loginmessage',
          message = "Username and password do not match!")
      }
    } else {
      session$sendCustomMessage(type = 'loginmessage',
        message = "Username or password does not exist!")
    }
    
    ##Initiate data
    if(logrole=="Student") {
      studentvar <<- ini.student(globalvar)      
    }
    if(logrole=="Administrator") {
      studentvar <<- ini.student(globalvar)
      adminvar <<- ini.admin(globalvar, studentvar)         
    }

  })  
  

  ##2. redirecting to student/course leader/general officer
  observe({
    logrole <- isolate(input$role)
    if (USER$logged == FALSE) {
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c(theme = shinytheme(thistheme), ui_login())))
      })
    } else if(USER$unlogged == FALSE) {
      output$stu_coursetab = DT::renderDataTable(  # colnames(course_info)
        datatable(studentvar$course_info[,c("Year/.Semester","Course.Code","Course.Title","Course.Leader","Course.Leader.Email","Ext.","Course.Abstract",
                                            "No.of.TAs.required","TA.1","TA.2","TA.3","TA.4","TA.5","TA.6","TA.7","Special.Requirements")], 
                  options = list(columnDefs = list(list( 
          #course_info[,-c(5,6,14,15,17)]  #show less info about course
          targets = 7, #5th col of the table
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 100 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
            "}")
        ))), callback = JS('table.page(0).draw(false);')  ))

      ##direct to student page
      if(logrole == "Student") {
        output$page <- renderUI({
          div(class="outer", do.call(fluidPage, c(theme = shinytheme(thistheme), ui_student(studentvar$course_names))))
        })
      }
      
      ##direct to admin page
      if(logrole == "Administrator") {
        ##update information
        ##update student submission table
        
        output$page <- renderUI({
          div(class="outer", do.call(fluidPage, c(theme = shinytheme(thistheme), ui_admin())))
        })
      }  

    }
  })

  
  ##3. student page event
  observeEvent(input$stu_submit, {
    if(input$stu_agree1 && input$stu_agree2 && input$stu_agree3 && input$stu_agree4) {
      
      tmpL <- list(
        ID=input$userName,
        choices=c(choice1=input$stu_choice1, choice2=input$stu_choice2, choice3=input$stu_choice3, 
                  comment1=input$stu_com1, comment2=input$stu_com2, comment3=input$stu_com3)
      )
      tmpfile <- paste(input$userName, Sys.time(), "RData", sep=".")
      tmpfile2 <- paste(input$userName, "RData", sep=".")
      
      #save student files
      save(tmpL, file=file.path("data", "student_cache", tmpfile))
      save(tmpL, file=file.path("data", "student_final", tmpfile2))
      
      session$sendCustomMessage(type = 'testmessage',
                                message = "Thank you submitting your TA choices!")
      USER$logged <- FALSE
      USER$unlogged <- TRUE
    } else {
      session$sendCustomMessage(type = 'testmessage',
                                message = "Please read all related information and tick all checkboxes!")
    }
    
  })
  
  ##3.1 student text overlimit alerts
  observeEvent(input$stu_com1, {
    if(is.null(input$stu_com1))
      return()

    if (nchar(input$stu_com1) >= 200) {
      showshinyalert(session, "stu_com_alert", paste("Enter < 200 characters"), 
                     styleclass = "danger")
      stu_com1_text <- isolate(input$stu_com1)
      updateTextInput(session, inputId = "stu_com1", value = substr(stu_com1_text, 1, 200))
    }
    output$stu_com1_help <- renderUI({
      helpText(paste("(", 200-nchar(input$stu_com1), " characters left)"))
    })   
  })

  observeEvent(input$stu_com2, {
    if(is.null(input$stu_com2))
      return()
    if (nchar(input$stu_com2) >= 200) {
      showshinyalert(session, "stu_com_alert", paste("Enter < 200 characters"), 
                     styleclass = "danger")
      stu_com2_text <- isolate(input$stu_com2)
      updateTextInput(session, inputId = "stu_com2", value = substr(stu_com2_text, 1, 200))
    }    
    output$stu_com2_help <- renderUI({
      helpText(paste("(", 200-nchar(input$stu_com2), " characters left)"))
    })   
  })

  observeEvent(input$stu_com3, {  
    if(is.null(input$stu_com3))
      return()
    if (nchar(input$stu_com3) >= 200) {
      showshinyalert(session, "stu_com_alert", paste("Enter < 200 characters"), 
                     styleclass = "danger")
      stu_com3_text <- isolate(input$stu_com3)
      updateTextInput(session, inputId = "stu_com3", value = substr(stu_com3_text, 1, 200))
    }
    output$stu_com3_help <- renderUI({
      helpText(paste("(", 200-nchar(input$stu_com3), " characters left)"))
    })   
  })
  
  ##3.2 student remove selected course
  observeEvent(input$stu_choice1, {
    if(!is.null(input$stu_choice1)){
        new_stu_choice <- c(None='', studentvar$course_names[studentvar$course_names != input$stu_choice1])
        updateSelectInput(session, "stu_choice2", selected=ifelse(input$stu_choice1==input$stu_choice2, '', input$stu_choice2), choices=new_stu_choice)
        updateSelectInput(session, "stu_choice3", selected=ifelse(input$stu_choice1==input$stu_choice3, '', input$stu_choice3), choices=new_stu_choice)
    }

  })
  
  observeEvent(input$stu_choice2, {
    if(!is.null(input$stu_choice2)){
      new_stu_choice <- c(None='', studentvar$course_names[!studentvar$course_names %in% c(input$stu_choice1, input$stu_choice2)])
      updateSelectInput(session, "stu_choice3", selected=ifelse(input$stu_choice2==input$stu_choice3, '', input$stu_choice3), choices=new_stu_choice)
    }
  })

  ##4. admin event
  ##4.1 manage student submission
  ##(0)
  observeEvent(input$admin_gen_stupw, {
    if(!file.exists("studentPasswd.RData")) { generate.stupw() }
    else {
      session$sendCustomMessage(type = 'adminstumessage',
                                message = "Students' accounts exist!")
    }
  })
  
  ##(a) update student submission
  observeEvent(input$admin_update_stutab, {
    adminvar$stutab <<- update.stutab(globalvar, studentvar)
    tmpTab <- isolate(adminvar$stutab)
    
    if(length(tmpTab) == 0) {
      session$sendCustomMessage(type = 'adminstumessage',
                                message = "No Student submissions!")
    } else {
      output$admin_stutab = DT::renderDataTable(
        datatable(tmpTab, options = list(columnDefs = list(list(
          targets = (ncol(tmpTab)-2):ncol(tmpTab),
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 10 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 10) + '...</span>' : data;",
            "}")
        ))), callback = JS('table.page(0).draw(false);')))
      
      session$sendCustomMessage(type = 'adminstumessage',
                                message = "Student submissions updated!")
    }
  })    
  
  
  ##(b) save student submission  named:"Confirm"
  observeEvent(input$admin_save_stutab, {
    student_submission <- adminvar$stutab
    tmpTab2 <- isolate(adminvar$stutab)
    if(length(tmpTab2) == 0) { ##!!
      session$sendCustomMessage(type = 'adminstumessage',
                                message = "No Student submissions!")
    }else{
      tmpfile <- paste("student_submission", Sys.time(), "RData", sep=".")
      tmpfile2 <- paste("student_submission", "RData", sep=".") 
      
      ##save admin files
      save(student_submission, file=file.path("data", "admin_cache", tmpfile))
      save(student_submission, file=file.path("data", "admin_final", tmpfile2))
      
      session$sendCustomMessage(type = 'adminstumessage',
                                message = "Student submissions confirmed and saved!")

    }
    
  })     
  ##(c) export student submission named "download"
  output$admin_export_stutab <- downloadHandler(
    filename = function() { paste('student_submission.xlsx', sep='') },
    content = function(file) {
      student_submission <- adminvar$stutab
      openxlsx::write.xlsx(student_submission, file,asTable=FALSE)
    }
  )
  
  ##4.3 optimize TA assignment
  ##(a) optimization
  observeEvent(input$optimize_assign_tab, {
    adminvar$opttab <<- optimTAAssign(globalvar) #dbReadtable(db,"assign")
  
    if(is.null(adminvar$opttab)) {
      session$sendCustomMessage(type = 'adminoptmessage',
                                message = "Students' and course leaders' submissions are not confirmed yet!")
    }
    output$admin_ta_assign_tab = DT::renderDataTable(datatable(adminvar$opttab[,c(-6,-8,-9)]))
  
  })
  ##(b) save TA assignment table
  observeEvent(input$save_assign_tab, {
    opttab <-adminvar$opttab
    tmpfile <- paste("TA_assignment", Sys.time(), "RData", sep=".")
    tmpfile2 <- paste("TA_assignment", "RData", sep=".")  
    
    ##save admin files
    save(opttab, file=file.path("data", "admin_cache", tmpfile))
    save(opttab, file=file.path("data", "admin_final", tmpfile2))
    
    session$sendCustomMessage(type = 'adminoptmessage',
                              message = "Final TA Assignment confirmed and saved!")
  })   
  ##(c) export TA assignment table
  output$export_assign_tab <- downloadHandler(
    filename = function() { paste('TA_assignment.xlsx', sep='') },
    content = function(file) {
      opttab <- adminvar$opttab
      opttab_out <- opttab[,c(-6,-8,-9)] #workload, average time, ta lack
      #opttab <- adminvar$opttab
      openxlsx::write.xlsx(opttab_out, file, asTable=FALSE) #check
    }
  )
  
  ##(d) upload new ta assignment
  observeEvent(input$ta_assignment, {
    ta_assignment <- openxlsx::read.xlsx(input$ta_assignment$datapath, 1)
    adminvar$opttab <<- ta_assignment
    output$admin_ta_assign_tab2 = DT::renderDataTable(datatable(adminvar$opttab))
  })
  
  ##(e) save TA assignment table
  observeEvent(input$save_assign_tab2, {
    opttab <- adminvar$opttab
    tmpfile <- paste("TA_assignment", Sys.time(), "RData", sep=".")
    tmpfile2 <- paste("TA_assignment", "RData", sep=".")  
    
    ##save admin files
    save(opttab, file=file.path("data", "admin_cache", tmpfile))
    save(opttab, file=file.path("data", "admin_final", tmpfile2))
    
    session$sendCustomMessage(type = 'adminoptmessage',
                              message = "Final TA Assignment confirmed and saved!")
  })   
  
  ## send emails of TA assignment
  ##(0) preliminary result to CLs
  observeEvent(input$send_pre_rlst2cl, {
    send_pre_email2cl()
  })  
  ##(1) final results to CLs
  observeEvent(input$send_rlst2cl, {
    send_finalemail2cl()
  })   
  ##(2) final results to students
  observeEvent(input$send_rlst2student, {
    send_finalemail2student()
  })  
  
  
})

