library(shiny)
library(shinysky)
ui_login <- function(){

  tagList(
    div(id = "login",
        wellPanel(
          div(
            id = "loginA",
            h2("Online System"),
            h2("for TA Assignment"),
            hr()
          ),
          div(
            id = "loginB",
            textInput("userName", "Username"),
            passwordInput("passwd", "Password"),
            selectInput('role', 'Select Your Role', c("Student", "Administrator"), width='100%'),
            br(),
            actionButton("Login", "Log in")
          )
        ),
        singleton(
          tags$head(tags$script('Shiny.addCustomMessageHandler("loginmessage",
                function(message) {
                  alert(message);
                });'))
        )
    ),
    div(id="login_busyx",
        class = "busy",
        conditionalPanel(
          condition="$('html').hasClass('shiny-busy')",
          img(src="loading.gif")
        )
    ),
    tags$style(type="text/css", "#loginA {text-align: center} #login {font-size:14px; 
               text-align: left;position:absolute;top: 30%;left: 45%;margin-top: -100px;margin-left: -150px;}
               #login_busyx {position:fixed;top: 50%;left: 60%;margin-top: -100px;margin-left: -150px;}")
  )
}
