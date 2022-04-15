library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

####### R stuff ##########
df = read.csv('./finals/survey_cleaned.csv', header = TRUE)

vals = c("Have you sought treatment for a mental health condition?",
                 "If you have a mental health condition, do you feel that it interferes with your work?",
                 "Does your employer provide mental health benefits?",
                 "How easy is it for you to take medical leave for a mental health condition?",
         "Does your employer provide resources to learn more about mental health issues and how to seek help?")

key = c("treatment",
            "work_interfere",
            "benefits ",
            "leave",
        "seek_help")

dict = setNames(key, vals)

##### UI Stuff ###########

ui = dashboardPage(
  
  dashboardHeader(
    title = "Mental Health In Tech",
    titleWidth = 350
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Age", tabName = "age"),
      menuItem("Gender", tabName = "gender"),
      menuItem("Country", tabName = "country")
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(
        tabName = "home",
        
        h2("Home"),
        p("Aanlysing trends in mental health in the workplace for the year of 2014. This dataset is from a 2014 survey that measures attitudes towards mental health and frequency of mental health disorders in the tech workplace"),
        p("We have analysed the data based on gender, age, country and Employee Size."),
        p(HTML('&nbsp;')),
      ),
      
      tabItem(
        tabName = "age",
        fluidPage(
          h2("Survey Results by Age"),
          p("Select the dropdown to know the results of the survey based on age."),
          
          fluidRow(
            column(6,
                   selectizeInput(inputId = "select_age",
                                  "Compare results by age:",
                                  choices = setNames(key, vals)),
                   plotOutput(outputId = "age_bar_graph")
            )
          ))
      ),
      
      tabItem(
        tabName = "gender",
        fluidPage(
          h2("Survey Results by gender"),
          p("Select the dropdown to know the results of the survey based on age."),
          
          fluidRow(
            column(6,
                   selectizeInput(inputId = "select_gender",
                                  "Compare results by gender:",
                                  choices = setNames(key, vals)),
                   plotOutput(outputId = "gender_bar_graph")
            )
          ))
      ),
      
      tabItem(
        tabName = "country",
        fluidPage(
          h2("Survey Results by country"),
          p("Select the dropdown to know the results of the survey based on age."),
          
          fluidRow(
            column(6,
                   selectizeInput(inputId = "select_country",
                                  "Compare results by country:",
                                  choices = setNames(key, vals)),
                   plotOutput(outputId = "country_bar_graph")
            )
          ))
      )
      
    )
  )
)

str(df)

######## Server stuff ########

server = function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$n))
  })
  
  output$age_bar_graph <- renderPlot(
    df %>% 
      ggplot(aes_string(x="Age", fill=input$select_age)) 
    + geom_bar(position="stack") + labs(x = 'Age', y = 'Count', title ="testing", fill = 'Responses:') + theme(plot.title = element_text(face = 'bold'))
  )
  
  output$gender_bar_graph <- renderPlot(
    df %>% 
      ggplot(aes_string(x="Gender", fill=input$select_gender)) 
    + geom_bar(position="stack") + labs(x = 'Gender', y = 'Count', title ="testing", fill = 'Responses:') + theme(plot.title = element_text(face = 'bold'))
  )
  
  output$country_bar_graph <- renderPlot(
    df %>% 
      ggplot(aes_string(x="Country", fill=input$select_country)) 
    + geom_bar(position="stack") + labs(x = 'Country', y = 'Count', title ="testing", fill = 'Responses:') + theme(plot.title = element_text(face = 'bold'))
  )
  
}


######## App #############
shinyApp(ui = ui, server = server)