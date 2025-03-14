#Importing all necessary libraries
library(tidyverse)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(rsconnect)


# Define UI for application
ui <- dashboardPage(
  
  dashboardHeader(title = "Eduvos IT Graduates Dashboard"), #Page Header
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Employment Rate", tabName = "employment", icon = icon("dashboard")), #Menu Tab
      menuItem("Top Tools", tabName = "tools", icon = icon("tools")), #Menu Tab
      menuItem("Industries", tabName = "industries", icon = icon("briefcase")), #Menu Tab
      menuItem("Job Roles", tabName = "roles", icon = icon("users")), #Menu Tab
      menuItem("Extra Visualization", tabName = "visuals", icon = icon("chart-bar")), #Menu Tab
      menuItem("User Guide", tabName = "guide", icon = icon ("info-circle")),
      selectInput("studyField", "Select Study Field:", choices = c("All", "IT", "Data Science", "Computer Science"), selected = "All"), #Drop down menu to provide filtering
      selectInput("toolType", "Select Tool Type:", choices = c("Programming Languages", "Databases", "Web Frameworks", "Platforms","AISearch", "AITool" )) #Drop down menu to provide filtering
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "employment",
              fluidRow(
                box(title = "Employment Rate by Study Field", width = 12, status = "primary", solidHeader = TRUE, plotOutput("employmentPlot"))#Tab Display box, populate the plot later
              )
      ),
      
      tabItem(tabName = "tools",
              fluidRow(
                box(title = "Top Tech Tools Used by Graduates", width = 12, status = "info", solidHeader = TRUE, plotOutput("toolsPlot"))#Tab Display box, populate the plot later
              )
      ),
      
      tabItem(tabName = "industries",
              fluidRow(
                box(title = "Industries Graduates Work In", width = 12, status = "warning", solidHeader = TRUE, plotOutput("industryPlot"))#Tab Display box, populate the plot later
              )
      ),
      
      tabItem(tabName = "roles",
              fluidRow(
                box(title = "Top Job Roles of Graduates", width = 12, status = "success", solidHeader = TRUE, plotOutput("jobRolePlot"))#Tab Display box, populate the plot later
              )
      ),
      tabItem(tabName = "visuals", #This was an additional tab to provide extra visualisation
              fluidRow(
                box(title = "Distribution of Programming Languages", width = 12, plotOutput("proLangPlot")), #Tab Display box, populate the plot later
                box(title = "Education Level vs Employment Type", width = 12, plotOutput("scatterPlotEduVsEmp")),#Tab Display box, populate the plot later
                box(title = "Graduates per Campus per Degree", width = 12, plotOutput("campusVdegreePlot"))
              )
      ),
      tabItem(tabName = "guide", 
              fluidRow(
                box(title = "User Guide", width = 12, status = "info", solidHeader = TRUE,
                    h4("How to Use the Eduvos IT Graduates Dashboard"),
                    p("1. Navigation: Use the sidebar menu on the left to switch between different data visualizations."),
                    p("2. Filtering Data: Use the dropdown menus provided in the sidebar to filter by study field and tool type."),
                    p("3. Understanding the Data: Each tab item represents a specific aspect of the survey data (employment rate, tools, industries and job roles)."),
                    p("4. Extra Visualisations: Additional visualisations are provided in the tab."))
              )
      )
    )
  )
)

# Creating the server function
server <- function(input, output) {
  
  #First step is to retrieve the already cleaned and preproccesed .csv from q1 and q2
  
  df <- read.csv("preprocessed_graduate_survey.csv")
  
  # Filter data based on selected Study Field, to help aid with the drop down menu filteration
  filtered_data <- reactive({
    if (input$studyField == "All") {
      return(df)  # Return entire dataset if "All" is selected
    } else {
      return(df %>% filter(StudyField == input$studyField)) # Return data that is specifically IT, Data Science or Computer Science
    }
  })
  
  # Employment Rate Plot (Employed vs Not Employed, Percentage on Y-axis)
  output$employmentPlot <- renderPlot({ #menu and tab box created in ui
    employment_data <- filtered_data() %>% #retrieve filtered data, which is filtered depending on what study the user chose
      mutate(EmploymentStatus = ifelse(grepl("Employed", Employment), "Employed", "Unemployed")) %>%
      count(EmploymentStatus) %>% 
      mutate(Percentage = n / sum(n) * 100) # Calculating Percentage
    
    ggplot(employment_data, aes(x = EmploymentStatus, y = Percentage, fill = EmploymentStatus)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Employment Rate for", input$studyField, "Graduates"),
           x = "Employment Status", y = "Percentage")
    
  })
  
  
  
  # Top 10 Tools Plot (Programming Languages, Databases, etc.)
  output$toolsPlot <- renderPlot({
    tool_column <- switch(input$toolType, #Converting user selection to column headers of dataframe
                          "Programming Languages" = "ProgLang", 
                          "Databases" = "Databases",
                          "Web Frameworks" = "WebFramework",
                          "Platforms" = "Platform",
                          "AISearch" = "AISearch",
                          "AITool" = "AITool")
    
    # Split tools by ';' and count frequency of each tool
    tool_data <- filtered_data() %>% # Still filtering by Study Field selection first
      separate_rows(!!sym(tool_column), sep = ";") %>%  # Split values
      count(!!sym(tool_column), name = "n") %>% # Count 
      arrange(desc(n)) %>%
      head(10)  # Get the top 10 tools
    
    ggplot(tool_data, aes(x = reorder(!!sym(tool_column), n), y = n, fill = !!sym(tool_column))) +
      geom_bar(stat = "identity") +
      labs(title = paste("Top 10", input$toolType, "Used by", input$studyField, "Graduates"),
           x = input$toolType, y = "Count") +
      coord_flip() # Flip for better readability
  })
  
  # Top 10 Job Roles Plot (Filtered by StudyField)
  output$jobRolePlot <- renderPlot({
    job_roles_data <- filtered_data() %>%
      count(Role) %>%
      arrange(desc(n)) %>%
      head(10)  # Get the top 10 job roles
    
    ggplot(job_roles_data, aes(x = reorder(Role, n), y = n, fill = Role)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Top Job Roles for", input$studyField, "Graduates"),
           x = "Job Role", y = "Count") +
      coord_flip() # Flip for better readability
  })
  
  # Top 10 Industry Plot (Filtered by StudyField)
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
      coord_flip() # Flip for better readability
  })
  
  #Addtional Plot of Pie Chart to show distribution of Programming Languages
  output$proLangPlot <- renderPlot({
    proLang_data <- filtered_data()%>%
      mutate(ProgLang = str_split(ProgLang, ";")) %>%
      unnest(ProgLang) %>%
      count(ProgLang) %>%
      arrange(desc(n)) %>%
      head(10)  # Get the top 10 languages
    
    ggplot(proLang_data, aes(x = "", y = n, fill = ProgLang)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") + #Pie chart
      theme(axis.text.x = element_blank()) +
      labs(title = "Top 10 Programming Languages Distribution", fill = "Language")
  })
  
  #Additional Plot to show number of graduates per campus/study field
  output$campusVdegreePlot <- renderPlot({
    ggplot(df, aes(x = Campus, fill = StudyField)) +
      geom_bar(position = "dodge") +
      labs(title = "Number of Graduates per Campus/Study Field", 
           x = "Campus", y = "Graduate Count", fill = "Study Field")
  })
  
  #Addtional Plot to show the relationship between Education Level and Employment Type
  
  df <- df %>%
    mutate(Employment = case_when(
      str_detect(Employment, "Employed") ~ "Employed",
      TRUE ~ "Unemployed"
    ))
  
  output$scatterPlotEduVsEmp <- renderPlot({
    ggplot(df, aes(x = EduLevel, y = Employment, color = EduLevel)) +
      geom_jitter(width = 0.2, height = 0.2, alpha = 0.7) +
      theme_minimal() +
      labs(title = "Education Level vs Employment Type",
           x = "Education Level",
           y = "Employment Type")+
      theme(axis.text.x = element_blank(),axis.title.x = element_blank())  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
