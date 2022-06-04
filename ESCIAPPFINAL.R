library(shiny)
library(tidyverse)
library(reactable)
library(shinyWidgets) #use for picketinput
library(plotly)





####Get Data
inventory<-read_csv("Juvenile Inventory  - Sheet41.csv")


#############################################################

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Outline of Pinto Abalone Summary App"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      p("This round I have added a select input that has multiple year choices. However, I'm still working on how to add a 
      filter by family and facilities option. The family option would need to be a write-in option, or possibly a dropdown list.
      I will then need to figure out how to add these all to a reactive expression to apply to each visual. 
        So far only figured out how to do one at a time. "),
      
      # Input: Select Cohort ----
      checkboxGroupInput("Cohort", "Cohort:",
                   choices = unique(inventory$Cohort), selected = c("2019","2020","2021")),
      # Input: Select Facility
      checkboxGroupInput("Facility", "Facility:",
                         choices = unique(inventory$Facility), selected = c("MCR","SA","PTMSC")),
      
      #Input: Select Family
      #pickerInput("Family", "Families:", 
                  #choices = unique(inventory$Family), options = list(`actions-box` = TRUE),multiple = TRUE),
      
      ### WANT TO: ADD inputs for family (write-in option). Not sure how to add this to the reactive in the server section but default keeping all families.
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      ##INTRO
      p("The goal of this app is to display projections of the total count and mean size of post-settled pinto abalone at X time in a conservation aquaculture setting. 
    Larval abalone families are settled into individual tanks and then reared in those tanks for the first year of their life prior to being outplanted into the wild. 
    Bimonthly counts and mean standard lengths are taken of a subset of animals in each tank. Bimonthly counts help inform how many animals will be greater than 5 mm at the 
    time of outplant, a requirement for outplanting."),
    
    p("My hope is to create an app that uses the bimonthly data to 1) track the current growth and survival of animals in the hatchery via table and visual summary and 2) project the estimated size and total count of animals at the time of outplant. This app will be extremely helpful for 
    the management and logistical planning of outplanting pinto abalone each year. The field season planning occurs months prior to the final count of animals and having a 
    more fine-tuned method of projecting the final count will be extremely beneficial to this process. It will also be a place where partners can check on the status of the animals at various satellite facilities that rear pinto abalone."),
    
    p("Tabs below showcase bimonthly counts and mean standard lengths taken to date in plot and table format and projected total counts and mean standard lengths. 
      Both are a work in progress..."),
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Total Count and Mean SL", plotOutput("plot1"), plotOutput("plot2"), reactableOutput("table")),
                  tabPanel("Projections", plotOutput("plot3"))
        ),
      hr(),
    titlePanel("Current Issue to sort:"),
    p("1. wrangle data for model selection"),
    p("2. select model and decide on visual to showcase (include graph and summary table to give final counts at X time"),
    p("3. rework reactive so data allows more than one sorting feature (i.e. cohort, family, and/or facility). Will likely add a second reactive option for projection data"),
    p("4. Add file upload option for satellite facilites to view their newly collected data"),
    p("5. Work on formatting and overall appearance")
      )
    )
  )


# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
    df_filtered <- reactive({
      
      df <- inventory %>% filter(Cohort == input$Cohort & Facility == input$Facility)
      
      return(df)
      
    })
    
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
    
    output$plot1<- renderPlot({
      
      df.no.set<-df_filtered() %>% 
      filter(Age.Month > 0) %>% 
        group_by(Cohort,Facility,Family,Age.Month) %>% 
        summarise(Total.Animals)
 
    ggplot(df.no.set,aes(x = Age.Month, y = Total.Animals, color = as.factor(Cohort))) + 
      geom_jitter(alpha=.5) +
      geom_smooth(method="lm")+
      labs( color= "Cohort")+
      scale_color_manual(breaks = c("2019", "2020", "2021"),
                         values=c("cadetblue", "darkgrey", "black")) +
      theme_classic()
  }, res = 96)
  
  #meanSL
  #Plot2
  output$plot2 <- renderPlot({
    
    df.grouped<-df_filtered() %>% 
      group_by(Cohort, Facility,Family,Age.Month) %>% 
      summarise(Mean.Size = mean(Mean.Size))
  
    ggplot(df.grouped,aes(x = Age.Month, y = Mean.Size, color = as.factor(`Cohort`))) + 
      geom_jitter(alpha=.5)+
      scale_color_manual(breaks = c("2019", "2020", "2021"),
                         values=c("cadetblue", "darkgrey", "black")) +
      geom_smooth(method="lm", alpha=.25)+
      labs(color= "Cohort")+
      theme_classic()
  }, res = 96)
  
  
  # Generate a summary of the data ----
  #table
  #To do: Figure out how to summarize this table  only after inventory has been filtered by input (df_filtered). Sum by family 
  output$table <- renderReactable({reactable(df_filtered(),
                                             sortable = TRUE,
                                             searchable = TRUE, 
                                             bordered = TRUE, 
                                             striped = TRUE, 
                                             highlight = TRUE)})
  #Projections
  #WANT TO: Figure out which data to use for this and which model to use. 
  #Plot3
  output$plot3 <- renderPlot({
    ggplot(df_filtered(), aes(x = Age.Month, y = Mean.Size, color = as.factor(`Cohort`))) + 
      geom_jitter(alpha=.5)+
      geom_smooth(method = "lm")+
      scale_color_manual(breaks = c("2019", "2020", "2021"),
                         values=c("cadetblue", "darkgrey", "black")) +
      geom_smooth(method="lm", alpha=.25)+
      labs(color= "Cohort")+
      theme_classic()
  }, res = 96)
  
}

# Create Shiny app ----
shinyApp(ui, server)
