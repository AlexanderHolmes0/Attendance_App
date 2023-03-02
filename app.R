
library(shiny)
library(fpp3)
library(plotly)
library(shinydashboard)
library(shinydashboardPlus)
library(waiter)
library(zoo)

gsheets_url <- "https://docs.google.com/spreadsheets/d/1mPyOeN9xKyYe07gy1D12JKYtY1s_gdF7iBLhFwqJeks/export?format=csv"
class_attend <- read.csv(gsheets_url)
class_attend <- class_attend %>% 
  select(date,class, section, attendance)


ui <- dashboardPage(title="Adam Dash",

  dashboardHeader(title = "Adam Dash"),
  skin = 'green',
  dashboardSidebar(minified = F, collapsed = T,tags$img(src='Adam.jpg', width = "230px",height='230px'),
                   textOutput('text')),
  dashboardBody(fluidRow(valueBoxOutput('overall'), valueBoxOutput('overallbas475'), valueBoxOutput('overallbas479')),
    fluidRow(
    box(plotlyOutput("Bas475"),  title = "BAS 475 Section: 1", background = 'maroon', collapsible = T),
    box(plotlyOutput("Bas4791"), title = "BAS 479 Section: 1", background = 'blue', collapsible = T)),
    fluidRow(
    box(plotlyOutput("Bas4794"), title = "BAS 479 Section: 4", background = 'blue', collapsible = T),
    box(plotlyOutput("Bas4795"), title = "BAS 479 Section: 5", background = 'blue', collapsible = T)
  )
))


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$text <- renderText({
    paste0("Wizard")
  })
  output$overall <- renderValueBox({
    valueBox(
      paste0(round(mean(class_attend$attendance, na.rm = T))), "Avg. Overall Attendance", icon = icon("list"),
      color = "purple"
    )
    
  })
  output$overallbas475 <- renderValueBox({
    means475 <- class_attend %>% 
      filter(class=="BAS475") %>% 
      summarise(avg = mean(attendance, na.rm = T))
    
    valueBox(
      paste0(round(means475)), "Avg. 475 Attendance", icon = icon("clock"),
      color = "maroon"
    )
  })
  
  output$overallbas479 <- renderValueBox({
    means479 <- class_attend %>% 
      filter(class=="BAS479") %>% 
      summarise(avg = mean(attendance, na.rm = T))
    
    valueBox(
      paste0(round(means479)), "Avg. 479 Attendance", icon = icon("cubes"),
      color = "blue"
    )
  })

  output$Bas475 <- renderPlotly({
   data <- class_attend %>%
      filter(class == "BAS475") %>%
      mutate(date = mdy(date)) %>%
      tsibble(index = date) %>%
      fill_gaps() %>% 
     update_tsibble(regular = T) %>% 
      mutate(attendance = na.approx(attendance, na.rm=F))
   
      autoplot(data,attendance)+
     geom_hline(yintercept = mean(data$attendance,na.rm = T), color = 'red',alpha=.7)+
        labs(y="Attendance", title = "BAS 475 Section 1")+
        ylim(0, NA)

  })
  
  output$Bas4791 <- renderPlotly({
    data <- class_attend %>%
      filter(class == "BAS479" & section == 1) %>%
      mutate(date = mdy(date)) %>%
      tsibble(index = date) %>%
      fill_gaps() %>% 
      update_tsibble(regular = T) %>% 
      mutate(attendance = na.approx(attendance, na.rm = F))

    autoplot(data,attendance)+
      geom_hline(yintercept = mean(data$attendance, na.rm = T), color = 'red',alpha=.7)+
      labs(y="Attendance", title = "BAS 479 Section 1")+
      ylim(0, NA)
    
  })
  output$Bas4794 <- renderPlotly({
    data <- class_attend %>%
      filter(class == "BAS479" &
               section == 4) %>%
      mutate(date = mdy(date)) %>%
      as_tsibble(index = date) %>%
      fill_gaps() %>% 
      mutate(attendance = na.approx(attendance,na.rm=F))
    
    autoplot(data,attendance)+
      geom_hline(yintercept = mean(data$attendance, na.rm = T), color = 'red',alpha=.7)+
      labs(y="Attendance", title = "BAS 479 Section 4")+
      ylim(0, NA)
    
  })
  output$Bas4795 <- renderPlotly({
    data <- class_attend %>%
      filter(class == "BAS479" &
               section == 5) %>%
      mutate(date = mdy(date)) %>%
      as_tsibble(index = date) %>%
      fill_gaps() %>% 
      mutate(attendance = na.approx(attendance,na.rm=F))
    
    autoplot(data,attendance)+
      geom_hline(yintercept = mean(data$attendance, na.rm = T), color = 'red',alpha=.7) +
      labs(y="Attendance", title = "BAS 479 Section 5")+
      ylim(0, NA)
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
