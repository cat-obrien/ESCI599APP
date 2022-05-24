library(shiny)
library(tidyverse)
library(reactable)

####Get Data
inventory<-read_csv("Juvenile Inventory  - Sheet41.csv")


#############################################################

ui <- fluidPage(
  
  ##INTRO
  titlePanel("Outline of Pinto Abalone Summary App"),
  p("The goal of this app is to display projections of the total count and mean size of post-settled pinto abalone at X time in a conservation aquaculture setting. 
    Larval abalone families are settled into individual tanks and then reared in those tanks for the first year of their life prior to being outplanted into the wild. 
    Bimonthly counts and mean standard lengths are taken of a subset of animals in each tank. Bimonthly counts help inform how many animals will be greater than 5 mm at the 
    time of outplant, a requirement for outplanting. 
    
    My hope is to create an app/model that uses the bimonthly data to 1) track the current growth and survival of animals in the hatchery via table and visual summary and 2) project the estimated size and total count of animals at the time of outplant. This app will be extremely helpful for 
    the management and logistical planning of outplanting pinto abalone each year. The field season planning occurs months prior to the final count of animals and having a 
    more fine-tuned method of projecting the final count will be extremely beneficial to this process. It will also be a place where partners can check on the status of the animals at various satellite facilities that rear pinto abalone."),
  
  
  ##FILTERS FOR WHOLE PAGE
  titlePanel("SelectInputs//filtering", ),
  p("Working towards adding selectInput drop down options (cohort//family//facility) for filtering graphs/tables/and projections below. The goal is to have this selection affect both graphs, the summary table, and the survival model"),
  
  p("This round I have added a select input that has multiple year choices. However, I'm still working on getting it default show all years. 
    Right now you must select the years to see the graph. I am also working on figuring out how to add a filter by family option. 
    There are so many families that a drop down list would be extensive. Working towards a write-in option that will filter the graphs and table. After that I'll add a facilities filtering option too. 
    That can be checkbox or dropdown. I will then need to figure out how to add these all to a reactive expression to apply to each visual. So far only figured out how to do one at a time. "),
  #inputs
  #To do: Look into switching to checkbox option instead-> currently a drop down select option. Need to update to default to all years
  selectInput("mydropdown", "Select Cohort", choices = unique(inventory$Cohort), multiple = TRUE),
  
  #To do: Attempt Filter by text input box for family. Not sure how to default show all families, and then filter if not empty.
  #textInput("text", label = h3("Family"), value = "example (F X M): GR07 X OR34)"),
  
  
  ## TOTAL COUNT
  titlePanel("Total animals by cohort//family//facility"),
  p("This round I have added a basic graph for total counts. Ideally I would like to add a way to filter out the first settlement data points (day 0) since it swamps the graph. 
    Still working on how to do that after the data is already filtered via a reactive expression since I still want this graph to be filtered by the select inputs above.  "),
  
  #plot
  plotOutput("plot1", click = "plot_click", ),
  
  hr(),
  
  
  ## MEANSL
  titlePanel("Mean Standard Length by cohort//family//facility"),
  p("This round inserted a graph of mean standard length to date. It uses the reactive data which is filtered by the select inputs"),
  
  
  #plot
  plotOutput("plot2", click = "plot_click", ),
  hr(),
  
  ##SUMMARY TABLE
  titlePanel("Summary table of counts and size"),
  p("This round inserted a table with all information from above graphs. Still working on how to include it using the reactive data filtered by the select inputs "),
  
  # Table
  reactableOutput("table"),
  hr(),
  
  ##PROJECTIONS
  titlePanel("Projection Data"),
  p("This section will include the model that works best with projecting numbers based on previous years survival. I would also like this model to update based on the reactive data selected by the user in the select inputs section. 
    Might be nice to also include a graph of predicted vs observed for the user to see the confidence of the model.(Pending to both)"),
  hr(),
  
)







#######################################################
server <- function(input, output) {
  
  #Reactive
  df_filtered <- reactive({
    
    df <- filter(inventory, Cohort == input$mydropdown)
    
    return(df)
    
  })
  
  #Total Count
  #Plot
  #would like to filter out the total set on settlement day since it's too drastic a drop from Day 0 to Day 180. 
  #Can I filter data that is already apart of reactive? ideally df_filtered would be selected and then 
  output$plot1 <- renderPlot({
    ggplot(df_filtered(), aes(x = `Months since Set date`, y = `Total Animals`, color = as.factor(`Cohort`))) + 
      geom_jitter(alpha=.5) +
      geom_smooth(method="lm")+
      labs( color= "Cohort")+
      scale_color_manual(breaks = c("2019", "2020", "2021"),
                         values=c("cadetblue", "darkgrey", "black")) +
      theme_classic()
  }, res = 96)
  
  #meanSL
  #Plot
  output$plot2 <- renderPlot({
    ggplot(df_filtered(), aes(x = `Months since Set date`, y = `Mean SL (mm)`, color = as.factor(`Cohort`))) + 
      geom_jitter(alpha=.5)+
      scale_color_manual(breaks = c("2019", "2020", "2021"),
                         values=c("cadetblue", "darkgrey", "black")) +
      geom_smooth(method="lm", alpha=.25)+
      labs(color= "Cohort")+
      theme_classic()
  }, res = 96)
  
  #Summary
  #table
  #To do: Figure out how to summarize this table  only after inventory has been filtered by input (df_filtered). Sum by family 
  output$table <- renderReactable({reactable(df_filtered(),
                                             sortable = TRUE,
                                             filterable = TRUE,
                                             searchable = TRUE, 
                                             bordered = TRUE, 
                                             striped = TRUE, 
                                             highlight = TRUE)})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

