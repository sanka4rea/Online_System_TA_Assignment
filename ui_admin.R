library(shiny)
library(shinysky)
#library(DT)

ui_admin <- function() {
  tagList(
    ##0. Welcome information
    h1("Welcome to BMS Online System for TA Assignment!", align = "center"),
    h1("(2020 ~ 2021)", align = "center"),
    br(),
    
    ##1. section I: course information
    h3("  I. Course Information"), 
    hr(),
    fluidRow(
      column(12, 
             DT::dataTableOutput('stu_coursetab')
      )
    ),
    
    ##2. section II: manage student submission
    h3("  II. Manage student submission"),
    hr(),
    fluidRow(
      wellPanel(style = "border: 0px; background-color: #ffffff;",
               actionButton("admin_gen_stupw", "(0) Generate students' accounts and send emails"),
               actionButton("admin_update_stutab", "(1) Update student submission"), 
               actionButton("admin_save_stutab", "(2) Confirm all student submission"), 
               downloadButton('admin_export_stutab', '(3) Export submission'),
        singleton(
          tags$head(tags$script('Shiny.addCustomMessageHandler("adminstumessage",
                                function(message) {
                                alert(message);
                                }
          );')))        
      )
    ),    
    br(),

    fluidRow(
      column(12, 
             DT::dataTableOutput('admin_stutab')
      )
    ),

    
    ##3. section III: Optimize TA assignment
    h3("  III. Optimize TA assignment"),
    hr(),
    fluidRow(
      wellPanel(style = "border: 0px; background-color: #ffffff;",
                actionButton("optimize_assign_tab", "(1) Optimization for assignment"), 
                actionButton("save_assign_tab", "(2) Confirm and save the assignment result"), 
                downloadButton('export_assign_tab', "(3) Export the assignment result"),
                singleton(
                  tags$head(tags$script('Shiny.addCustomMessageHandler("adminoptmessage",
                                function(message) {
                                alert(message);
                                }
          );')))        
      )
    ),    
    br(),
    
    fluidRow(
      column(12, 
             DT::dataTableOutput('admin_ta_assign_tab')
      )
    ),    
    
    
    ##4. section IV: Manual adjust
    h3("  IV. Upload new TA assignment to replace the current one"),
    hr(),
    fluidRow(
      wellPanel(style = "border: 0px; background-color: #ffffff;",
                fileInput('ta_assignment', 'Upload new TA assignment file',
                          accept=c('text/xlsx', 
                                   'text/comma-separated-values,text/plain', 
                                   '.xlsx'), width = "30%"),
                actionButton("save_assign_tab2", "Confirm new TA assignment"),
                actionButton("send_pre_rlst2cl", "Send Preliminary Results Email to CLs"),
                singleton(
                  tags$head(tags$script('Shiny.addCustomMessageHandler("adminoptmessage",
                                function(message) {
                                alert(message);
                                }
          );')))        
      )
    ),    
    br(),
    fluidRow(
      column(12, 
             DT::dataTableOutput('admin_ta_assign_tab2')
      )
    ),  
    
    ##5. section V: Email
    h3("  V. Notification"),
    hr(),
    fluidRow(
      wellPanel(style = "border: 0px; background-color: #ffffff;",
                actionButton("send_rlst2cl", "Send final results  to CLs"), 
                actionButton("send_rlst2student", "Send final results to students"), 
                singleton(
                  tags$head(tags$script('Shiny.addCustomMessageHandler("adminoptmessage",
                                function(message) {
                                alert(message);
                                }
          );')))        
      )
    ),    
    
    
    ##5. section 5: contact information
    h3("  Help & Contact Information"),
    hr(),
    fluidRow(
      
      column(6,
             strong(h4("General Enquiry")),
             br(),
             h6("Mr ", align="left"),
             h6("Email: c@", align = "left"),
             h6("Phone: (+852) ", align = "left"),
             h6("Office: TYB", align = "left")
      ),
      
      column(6,
             strong(h4("Online System Enquiry")),
             br(),
             h6("Mr ", align="left"),
             h6("Email: l", align = "left"),
             h6("Phone: (+852) ", align = "left")
      )
    ),
    hr(),
    h6("Department of, Kowloon Tong, Hong Kong", align = "right"),
    div(id="admin_busyx",
        class = "busy",
        conditionalPanel(
          condition="$('html').hasClass('shiny-busy')",
          img(src="loading.gif")
        )
    ),
    tags$style(type="text/css", "#admin_busyx {position:fixed;top: 50%;left: 55%;margin-top: -100px;margin-left: -150px;}")
  )
}