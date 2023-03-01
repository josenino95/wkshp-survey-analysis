library(shiny)
library(tidyverse)

# pre-load data
allResults <- read_csv("../data-joined/all_workshops.csv")
workshops <- workshops <- levels(as.factor(allResults$workshop))

# Define server logic
server <- function(input, output) {
  
  results <-eventReactive(input$update, {
    selected <- input$workshopsSelect
    if(!length(selected)){
      selected <- workshops
    }
    filter(allResults, workshop %in% selected)
  }, ignoreNULL = FALSE)
  
  output$departments <- renderTable({
    df <- results()
    select(df , dept_select.pre) %>% 
      separate_rows(dept_select.pre, sep=",") %>% 
      group_by(dept_select.pre) %>%
      summarise(count=n()) %>%
      arrange(desc(count))
  })

  output$otherDepartments <- renderTable({
    df <- results()
    group_by(df, dept_other.pre) %>% 
      summarise(count=n()) %>%
      arrange(desc(count))
  })

  output$occupation <- renderTable({
    df <- results()
    select(df, occupation.pre) %>% 
      separate_rows(occupation.pre, sep=",") %>% 
      group_by(occupation.pre) %>%
      summarise(count=n()) %>%
      arrange(desc(count))
  })

  output$motivations <- renderTable({
    df <- results()
    select(df, motivation_select.pre) %>% 
      separate_rows(motivation_select.pre, sep=",") %>% 
      group_by(motivation_select.pre) %>%
      summarise(count=n()) %>%
      arrange(desc(count))
  })
  
  output$hopes <- renderTable({
    df <- results()
    group_by(df, workshop) %>% 
      select(hopes.pre, workshop) %>% 
      drop_na()
  })

  
  output$applying <- renderTable({
    df <- results()
    group_by(df, agree_apply.post) %>% 
      select(agree_apply.post) %>% 
      drop_na() %>%
      summarize(count=n())
  })
  
  output$comfort <- renderTable({
    df <- results()
    group_by(df, agree_comfortable.post) %>% 
      select(agree_comfortable.post) %>%
      drop_na() %>%
      summarize(count=n())
  })
  
  output$clearanswers <- renderTable({
    df <- results()
    group_by(df, agree_clearanswers.post) %>% 
      select(agree_clearanswers.post) %>%
      drop_na() %>%
      summarize(count=n())
  })
  
  output$instrEnthusiasm <- renderTable({
    df <- results()
    group_by(df, agree_instr_enthusiasm.post) %>% 
      select(agree_instr_enthusiasm.post) %>%
      drop_na() %>%
      summarize(count=n())
  })
  
  output$instrComfort <- renderTable({
    df <- results()
    group_by(df, agree_instr_interaction.post) %>% 
      select(agree_instr_interaction.post) %>%
      drop_na() %>%
      summarize(count=n())
  })
  
  output$instrKnowledge <- renderTable({
    df <- results()
    group_by(df,agree_instr_knowledge.post) %>% 
      select(agree_instr_knowledge.post) %>%
      drop_na() %>%
      summarize(count=n())
  })
  
  output$interactionEx <- renderTable({
    df <- results()
    select(df, instructor_example.post, workshop) %>%
      drop_na()
  })
  
  output$strength <- renderTable({
    df <- results()
    select(df, workshop_strengths.post, workshop) %>% 
      drop_na()
  })
    
  output$improvement <- renderTable({
    df <- results()
    select(df, workshop_improved.post, workshop) %>% 
      drop_na()
  })
  
  output$topicSuggestion <- renderTable({
    df <- results()
    select(df, workshop, suggest_topics.post) %>% 
      drop_na()
  })
  

}

# Define ui elements
workshopsSelect <- selectInput(
  "workshopsSelect",
  "Filter by Workshop(s)",
  workshops,
  selected = NULL,
  multiple = TRUE,
  selectize = TRUE,
  width = NULL,
  size = NULL
)

ui <- fluidPage(
    titlePanel("Carpentries Workshop Survey Results"),
    sidebarLayout(
        sidebarPanel(
          workshopsSelect,
          actionButton("update", "Update View")
        ),
        mainPanel(
          h4("Departments"),
          tableOutput("departments"),
          
          h4("Other Departments"),
          tableOutput("otherDepartments"),
          
          h4("Occupations"),
          tableOutput("occupation"),
          
          h4("Motivations"),
          tableOutput("motivations"),
          
          h4("Hopes"),
          tableOutput("hopes"),
          
          h4("Applying the Material"),
          tableOutput("applying"),
          
          h4("Comfortable in the workshop environment"),
          tableOutput("comfort"),
          
          h4("Got Clear Answers"),
          tableOutput("clearanswers"),
          
          h4("Instructors were Enthusiastic"),
          tableOutput("instrEnthusiasm"),
          
          h4("Comfortable interacting with instructors"),
          tableOutput("instrComfort"),
          
          h4("Instructors were Knowledgeable"),
          tableOutput("instrKnowledge"),
          
          h4("Instructor interaction example"),
          tableOutput("interactionEx"),
          
          h4("Workshop Strengths"),
          tableOutput("strength"),
          
          h4("Ways to improve the workshop"),
          tableOutput("improvement"),
          
          h4("Topic Suggestions"),
          tableOutput("topicSuggestion")
        )
    )
)


# Run the application 
shinyApp(ui = ui, server = server)
