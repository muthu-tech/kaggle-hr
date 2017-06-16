#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(readr)
library(tidyr)
library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(
      tags$style(HTML("
        body{max-width:1200px; margin:auto auto;}
        .shiny-plot-output {margin: 100px 0px;}
  
      "))
    ),
   # Application title
   titlePanel("Company Dashboard"),
   p('This dashboard helps understanding satisfaction level of employess in dependence of different factors. 
      All charts are drawn using the information from just current employed staff (not those who are left). The charts are
      represented by violin diagram overlapped with boxplot charts. 
     '),
   p(strong('*RED AREA on the chart shows last evaluation results.')),
   p('So one can see the dynamics of satisfaction level comparing both red and orange areas (orange on stands for currently employed staff.'),
   p(strong('*RED LINE on the chart shows the average level of satisfaction among left employees.')),
   p('This needs to understand a caution level of satisfaction below which the staff are going to leave the company.'),
   hr(),
   
   sidebarLayout(
     sidebarPanel(
         textOutput('size'),
         hr(),
         radioButtons('accident', "Accident happend", c("Yes"=1, "No"=0, "Both"=-1), selected=-1),
         radioButtons('promotion', "Promotion within last 5 years", c("Yes"=1, "No"=0, "Both"=-1), selected=-1),
         uiOutput('departments'),
         uiOutput('salary'),
         uiOutput('nprojects'),
         uiOutput('monthly_hours'),
         uiOutput('spend_company')
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h3('Exploring satisfaction Level'),
        tabsetPanel(
          tabPanel('Satisfaction VS Department', plotOutput('plt_satisfaction')),
          tabPanel('Satisfaction VS Number of Projects',plotOutput('plt_satisfaction_nprojects')),
          tabPanel('Satisfaction VS Salary',plotOutput('plt_satisfaction_salary'))
        )
      )
   ),
   hr(),
   p('The dataset for the analysis is hosted by Kaggle:',
      a('www.kaggle.com/ludobenistant/hr-analytics', href='https://www.kaggle.com/ludobenistant/hr-analytics'))
)

server <- function(input, output, session) {
   base_data <- read_csv('HR_comma_sep.csv')
   
   current_dataset <- reactive({
     dataset <- base_data
     
     if (input$inp_department != 'all')
       dataset <- dataset %>% 
         filter(sales==input$inp_department)
     
     if (input$accident != -1)
       dataset <- dataset %>% 
         filter(Work_accident==input$accident)
     
     if (input$promotion != -1)
       dataset <- dataset %>% 
         filter(promotion_last_5years==input$promotion)
     
     if (input$inp_salary != 'all')
       dataset <- dataset %>% 
         filter(salary==input$inp_salary)
      
     if (length(input$inp_nprojects)>0){
       nprojects <- input$inp_nprojects
       dataset <- dataset %>% 
         filter(number_project>=nprojects[1], number_project<=nprojects[2])
     }
     
     if (length(input$time_spend_company)>0){
       tmspd <- input$time_spend_company
       dataset <- dataset %>% 
         filter(time_spend_company>=tmspd[1], time_spend_company<=tmspd[2])
     }
     
     if (length(input$avg_monthly_hours)>0){
       monthly_hours <- input$avg_monthly_hours
       dataset <- dataset %>% 
         filter(average_montly_hours>=monthly_hours[1], average_montly_hours<=monthly_hours[2])
     }
      
     dataset
   })
   
   react_satisfaction <- reactive({
     
     dataset <- current_dataset()
     working <- dataset %>% filter(left==0)
     left <- dataset %>% filter(left==1)
     
     left_line <- mean(left$satisfaction_level)
     
     ggplot(working) + 
       ggtitle("Comparing Satisfaction Level across Departments") +
       geom_violin(aes(x=sales, y=last_evaluation), color="grey", fill="red") +
       geom_violin(aes(x=sales, y=satisfaction_level), fill="#ffb300") +
       geom_boxplot(aes(x=sales, y=satisfaction_level), fill='black', color=alpha('black', 0.1), alpha=0.1) +
       ylim(0,1) +
       ylab('Satisfaction level of employed staff') +
       xlab('Department') +
       geom_hline(yintercept=left_line, color="red") +
       theme_light()
     
   })
   
   react_satisfaction_nprojects <- reactive({
     
     dataset <- current_dataset() %>% mutate(number_project=as.factor(number_project))
     working <- dataset %>% filter(left==0)
     left <- dataset %>% filter(left==1)
     
     left_line <- mean(left$satisfaction_level)
     
     ggplot(working) + 
       ggtitle("Comparing Satisfaction Level and Number of Projects") +
       geom_violin(aes(x=number_project, y=last_evaluation), color="grey", fill="red") +
       geom_violin(aes(x=number_project, y=satisfaction_level), fill="#ffb300") +
       geom_boxplot(aes(x=number_project, y=satisfaction_level), fill='black', color=alpha('black', 0.1), alpha=0.1) +
       ylim(0,1) +
       ylab('Satisfaction level of employed staff') +
       xlab('Number of projects') +
       geom_hline(yintercept=left_line, color="red") +
       theme_light()
     
   })
   
   react_satisfaction_salary <- reactive({
     
     dataset <- current_dataset() %>% mutate(salary=as.factor(salary))
     working <- dataset %>% filter(left==0)
     left <- dataset %>% filter(left==1)
     
     left_line <- mean(left$satisfaction_level)
     
     ggplot(working) + 
       ggtitle("Comparing Satisfaction Level and Salary") +
       geom_violin(aes(x=salary, y=last_evaluation), color="grey", fill="red") +
       geom_violin(aes(x=salary, y=satisfaction_level), fill="#ffb300") +
       geom_boxplot(aes(x=salary, y=satisfaction_level), fill='#ffb300', color=alpha('black', 0.1), alpha=0.1) +
       ylim(0,1) +
       ylab('Satisfaction level of employed staff') +
       xlab('Salary') +
       geom_hline(yintercept=left_line, color="red") +
       theme_light()
     
   })
   
   # Rendering input select group for departments list
   output$departments <- renderUI({
      deps <- unique(base_data$sales) %>% c('all')
      selectInput('inp_department', 'Choose department', choices=deps, selected='all')
   })
   
   # Rendering input select group for departments list
   output$salary <- renderUI({
     sal_opt <- unique(base_data$salary) %>% c('all')
     selectInput('inp_salary', 'Choose salary', choices=sal_opt, selected='all')
   })
   
   # Rendering slider for number of projects
   output$nprojects <- renderUI({
     dataset <- base_data
     sliderInput('inp_nprojects', 'Number of projects', 
                 min=min(dataset$number_project), 
                 max=max(dataset$number_project), 
                 value=c(min(dataset$number_project), max(dataset$number_project)), step=1)
   })
   
   # Rendering slider for avg_monthly_hours
   output$monthly_hours <- renderUI({
     dataset <- base_data
     sliderInput('avg_monthly_hours', 'Average Monthly Hours', 
                 min=min(dataset$average_montly_hours), 
                 max=max(dataset$average_montly_hours), 
                 value=c(min(dataset$average_montly_hours), max(dataset$average_montly_hours)), step=1)
   })
   
   output$spend_company <- renderUI({
     dataset <- base_data
     sliderInput('time_spend_company', 'Time Spent at the Company', 
                 min=min(dataset$time_spend_company), 
                 max=max(dataset$time_spend_company), 
                 value=c(min(dataset$time_spend_company), max(dataset$time_spend_company)), step=1)
   })
   
   output$size <- renderText({
     sprintf('Amount of staff: %d', nrow(current_dataset()))
   })
   
   output$plt_satisfaction <- renderPlot({
     react_satisfaction()
   })
   
   output$plt_satisfaction_nprojects <- renderPlot({
     react_satisfaction_nprojects()
   })
   
   output$plt_satisfaction_salary <- renderPlot({
     react_satisfaction_salary()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

