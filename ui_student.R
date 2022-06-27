library(shiny)
library(shinysky)

ui_student <- function(course_names) {

  tagList(
    ##0. Welcome information
    h1("Online System for TA Assignment!", align = "center"),
    h1("(2020 ~ 2021)", align = "center"),
    br(),
    ##1. section I: TA guideline information
    div(id='div_stu_sec1', h3("I. Please read the following information before submission")), 
    hr(),
    fluidRow(
      column(12,
             includeHTML("TA_Guide.html")
      )
    ),
    
    ##2. section II: course information
    h3("  II. Course Information"), 
    hr(),
    fluidRow(
      column(12, 
             DT::dataTableOutput('stu_coursetab')
      )
    ),
    
    ##3. section III: select and submit choices
    h3("  III. Select Course and Submit"),
    hr(),
    fluidRow(
      column(6, 
             #selectInput('stu_role', 'Perferred TA role', c(None='', "Practical","Tutorial"), width='100%'),
             helpText("(Note: The system will take Course Leaders' preference first. More details could be found in Part I.)"),
             
             selectInput('stu_choice1', '1st Choice', c(None='', course_names), width='100%'),
             tags$style(type="text/css", "textarea {width:100%;margin-top:-18px;margin-bottom:-10px}"),
             tags$textarea(id = 'stu_com1', placeholder = 'Optional: Enter here any additional supporting comments such as your background and strength (< 200 characters) ', rows = 2, ""),
             div(id = 'stu_com1_help_div', uiOutput('stu_com1_help')),
             
             selectInput('stu_choice2', '2nd Choice', c(None='', course_names), width='100%'),
             tags$style(type="text/css", "textarea {width:100%;margin-top:-18px;margin-bottom:-10px}"),
             tags$textarea(id = 'stu_com2', placeholder = 'Optional: Enter here any additional supporting comments such as your background and strength (< 200 characters) ', rows = 2, ""),
             div(id = 'stu_com2_help_div', uiOutput('stu_com2_help')),
             
             selectInput('stu_choice3', '3rd Choice', c(None='', course_names), width='100%'),
             tags$style(type="text/css", "textarea {width:100%;margin-top:-18px;margin-bottom:-10px}"),
             tags$textarea(id = 'stu_com3', placeholder = 'Optional: Enter here any additional supporting comments such as your background and strength (< 200 characters) ', rows = 2, ""),
             div(id = 'stu_com3_help_div', uiOutput('stu_com3_help')),
             
             shinyalert("stu_com_alert", FALSE, auto.close.after = 5)
      ),
      
      column(6, 
             br(),
             wellPanel(
               helpText("(Note: Make sure you have read all relevant documents and ticked the following checkboxes before you click the 'Submit' button)"),
               checkboxInput('stu_agree1', h5('I have read the "', 
                                             a("Framework for Teaching Assistant (TA) Assignments for Research Degree Students", 
                                               href="http://www.sgs.cityu.edu.hk/staff/memo/?viewAs=getFile&id=102", target="_blank"), '"'), 
                             width='100%'),
               checkboxInput('stu_agree2', h5('I have read the "', 
                                             a("University Assessment Policy and Principles for Taught Programme",
                                               href="http://www.cityu.edu.hk/qac/assessment_policy/Assessment_Policy_revised_%20June_2014_WD_definitions_updated.pdf", target="_blank"), '"'), width='100%'),
               checkboxInput('stu_agree3', h5('I have read the "', 
                                             a("Supplementary Practical Guideline for Teaching Assistant Assignment", 
                                               href="#div_stu_sec1", target="_blank"), '"'), width='100%'),
               checkboxInput('stu_agree4', 'I understand the final selection will be based on mutual selections between students and 
                           course leaders, and if not selected by the course leader, there is a possibility to be randomly 
                             assigned to other courses', width='100%'),
               actionButton("stu_submit", "Submit"), 
               singleton(
                 tags$head(tags$script('Shiny.addCustomMessageHandler("testmessage",
                function(message) {
                  alert(message);
                }
              );'))))
      )
    ),
    
    ##4. section 4: contact information
    h3("  Help & Contact Information"),
    hr(),
    fluidRow(
      
      column(6,
             strong(h4("General Enquiry")),
             br(),
             h6("Mr ", align="left"),
             h6("Email: ", align = "left"),
             h6("Phone: (+852)", align = "left"),
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
    h6("Department of Biomedical Sciences, 1/F, Block 1, To Yuen Building, City University 
       of Hong Kong, 31 To Yuen Street, Kowloon Tong, Hong Kong", align = "right"),
    div(id="stu_busyx",
        class = "busy",
        conditionalPanel(
          condition="$('html').hasClass('shiny-busy')",
          img(src="loading.gif")
        )
    ),
    tags$style(type="text/css", "#stu_com1_help_div {margin-top:0px;margin-bottom:20px;} 
        #stu_com2_help_div {margin-top:0px;margin-bottom:20px;} 
        #stu_com3_help_div {margin-top:0px;margin-bottom:20px;} 
        #stu_busyx {position:fixed;top: 50%;left: 55%;margin-top: -100px;margin-left: -150px;}")
  )
}
