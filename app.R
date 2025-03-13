#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(tidyverse)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(rsconnect)


# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "Eduvos IT Graduates Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Employment Rate", tabName = "employment", icon = icon("dashboard")),
      menuItem("Top Tools", tabName = "tools", icon = icon("tools")),
      menuItem("Industries", tabName = "industries", icon = icon("briefcase")),
      menuItem("Job Roles", tabName = "roles", icon = icon("users")),
      menuItem("Extra Visualization", tabName = "visuals", icon = icon("chart-bar")),
      selectInput("studyField", "Select Study Field:", choices = c("All", "IT", "Data Science", "Computer Science"), selected = "All"),
      selectInput("toolType", "Select Tool Type:", choices = c("Programming Languages", "Databases", "Web Frameworks", "Platforms","AISearch", "AITool" ))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "employment",
              fluidRow(
                box(title = "Employment Rate by Study Field", width = 12, status = "primary", solidHeader = TRUE, plotOutput("employmentPlot"))
              )
      ),
      
      tabItem(tabName = "tools",
              fluidRow(
                box(title = "Top Tech Tools Used by Graduates", width = 12, status = "info", solidHeader = TRUE, plotOutput("toolsPlot"))
              )
      ),
      
      tabItem(tabName = "industries",
              fluidRow(
                box(title = "Industries Graduates Work In", width = 12, status = "warning", solidHeader = TRUE, plotOutput("industryPlot"))
              )
      ),
      
      tabItem(tabName = "roles",
              fluidRow(
                box(title = "Top Job Roles of Graduates", width = 12, status = "success", solidHeader = TRUE, plotOutput("jobRolePlot"))
              )
      ),
      tabItem(tabName = "visuals",
              fluidRow(
                box(title = "Distribution of Programming Languages", width = 12, plotOutput("proLangPlot")),
                box(title = "Students per Campus per Degree", width = 12, plotOutput("campusVdegreePlot")) 
              )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  df <- read.csv("preprocessed_graduate_survey.csv")
  
  # Filter data based on selected Study Field
  filtered_data <- reactive({
    if (input$studyField == "All") {
      return(df)  # Return entire dataset if "All" is selected
    } else {
      return(df %>% filter(StudyField == input$studyField))
    }
  })
  
  # Employment Rate Plot (Employed vs Not Employed, Percentage on Y-axis)
  output$employmentPlot <- renderPlot({
    employment_data <- filtered_data() %>%
      mutate(EmploymentStatus = ifelse(grepl("Employed", Employment), "Employed", "Unemployed")) %>%
      count(EmploymentStatus) %>%
      mutate(Percentage = n / sum(n) * 100)
    
    ggplot(employment_data, aes(x = EmploymentStatus, y = Percentage, fill = EmploymentStatus)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Employment Rate for", input$studyField, "Graduates"),
           x = "Employment Status", y = "Percentage") +
      theme_minimal() +
      theme(legend.position = "none")
    
  })
  
  
  
  # Top Tools Plot (Programming Languages, Databases, etc.)
  output$toolsPlot <- renderPlot({
    tool_column <- switch(input$toolType,
                          "Programming Languages" = "ProgLang",
                          "Databases" = "Databases",
                          "Web Frameworks" = "WebFramework",
                          "Platforms" = "Platform",
                          "AISearch" = "AISearch",
                          "AITool" = "AITool")
    
    # Split tools by ';' and count frequency of each tool
    tool_data <- filtered_data() %>%
      separate_rows(!!sym(tool_column), sep = ";") %>%  # Split values
      count(!!sym(tool_column), name = "n") %>%
      arrange(desc(n)) %>%
      head(10)  # Get the top 10 tools
    
    ggplot(tool_data, aes(x = reorder(!!sym(tool_column), n), y = n, fill = !!sym(tool_column))) +
      geom_bar(stat = "identity") +
      labs(title = paste("Top 10", input$toolType, "Used by", input$studyField, "Graduates"),
           x = input$toolType, y = "Count") +
      coord_flip() +  # Flip for better readability
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Filtered Job Roles Plot (Top 10 per Study Field, No Legend)
  # Job Roles Plot (Filtered by StudyField)
  output$jobRolePlot <- renderPlot({
    job_roles_data <- filtered_data() %>%
      count(Role) %>%
      arrange(desc(n)) %>%
      head(10)  # Get the top 10 job roles
    
    ggplot(job_roles_data, aes(x = reorder(Role, n), y = n, fill = Role)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Top Job Roles for", input$studyField, "Graduates"),
           x = "Job Role", y = "Count") +
      coord_flip() +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Filtered Industry Plot (Top 10 per Study Field, No Legend)
  output$industryPlot <- renderPlot({
    
    # Separate each industry by ';' and count frequency
    industry_data <- filtered_data() %>%
      mutate(Industry = str_split(Industry, ";")) %>%
      unnest(Industry) %>%
      count(Industry) %>%
      arrange(desc(n)) %>%
      head(10)  # Get the top 10 industries
    
    ggplot(industry_data, aes(x = reorder(Industry, n), y = n, fill = Industry)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Top 10 Industries for", input$studyField, "Graduates"),
           x = "Industry", y = "Count") +
      coord_flip() +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$proLangPlot <- renderPlot({
    proLang_data <- filtered_data()%>%
      mutate(ProgLang = str_split(ProgLang, ";")) %>%
      unnest(ProgLang) %>%
      count(ProgLang) %>%
      arrange(desc(n)) %>%
      head(10)  # Get the top 10 languages
    
    ggplot(proLang_data, aes(x = "", y = n, fill = ProgLang)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_minimal() +
      theme(axis.text.x = element_blank()) +
      labs(title = "Top 10 Programming Languages Distribution", fill = "Language")
  })
  
  
  output$campusVdegreePlot <- renderPlot({
    ggplot(df, aes(x = Campus, fill = StudyField)) +
      geom_bar(position = "dodge") +
      theme_minimal() +
      labs(title = "Number of Students per Campus/Study l", 
           x = "Campus", y = "Student Count", fill = "Study Field")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
