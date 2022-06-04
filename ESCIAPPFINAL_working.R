library(shiny)
library(tidyverse)
library(reactable)
library(shinyWidgets) #use for picketinput
library(caret)





####Get Data
inventory<-read_csv("Juvenile Inventory  - Sheet41 (1).csv")
mock.df<-read_csv("train.csv")
mock.df$cohort<-as.factor(mock.df$cohort)

linebreaks <- function(n){HTML(strrep(br(), n))}
#############################################################

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Pinto Abalone Summary App"),
  
  p("The goal of this app is to display projections of the total count and mean size of post-settled pinto abalone at X time in a conservation aquaculture setting. 
  Larval abalone families are settled into individual tanks and then reared in those tanks for the first year of their life prior to being outplanted into the wild. 
    Bimonthly counts and mean standard lengths are taken of a subset of animals in each tank. Bimonthly counts help inform how many animals will be greater than 5 mm at the 
    time of outplant, a requirement for outplanting.")
  , 
    p("To date, the data supports a linear regression of mean size, but does not allow for modeling of total count accurately. 
    As of now this app can be used to graph current inventory data and visually compare to past years, 
    while also giving an estimated of when animals will reach a mean size > 5mm based on past cohorts."),
  
    p("My hope is to continue working on this app to include a place for satellite facilities to upload their most recent 
    inventories to compare across facilities and to continue working on how to accurately predict total count at the time of 
    outplant (~ 10 months of age) to better inform management and logistical planning of outplanting abalone. 
    The field season planning occurs months prior to the final count of animals and having a fine-tuned method of projecting the final 
    count will be extremely beneficial to this process. 
    "),
 
   # br() element to introduce extra vertical spacing ----
 linebreaks(2),
  # Sidebar layout with input and output definitions ----

        sidebarLayout(position = "right",
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      p("At this time I have only allowed the data to be filtered by cohort. As more data accrues at various satellie facilities I plan to include a filter for facility."),
      
      # Input: Select Cohort ----
      checkboxGroupInput("Cohort", "Cohort:",
                   choices = unique(inventory$Cohort), selected = c("2019","2020","2021")),
    ),
      # Input: Select Facility
      #checkboxGroupInput("Facility", "Facility:",
                         #choices = unique(inventory$Facility), selected = c("MCR","SA","PTMSC")),
      
      #Input: Select Family
      #pickerInput("Family", "Families:", 
                  #choices = unique(inventory$Family), options = list(`actions-box` = TRUE),multiple = TRUE)

    
    # Main panel for displaying outputs ----
    mainPanel(
      
      ##INTRO
    
    
    p("Tabs below showcase bimonthly counts and mean standard lengths taken to date in plot and table format. The second tab includes projected mean size and count data."),
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Mean Size and Count", 
                           linebreaks(2),
                           plotOutput("plot2"), 
                           linebreaks(2),
                           plotOutput("plot1"), 
                           linebreaks(2),
                           reactableOutput("table"),
                           p("As you can see from the table above, the number of families and tanks is variable across months 
                             within one cohort, when in reality there should be a more consistent number at each month. 
                             This is due to the nature of we collect bimonthly data. Not all tanks are 
                             counted at once based on workload required to take standard lengths and counts. 
                             The variation causes an unreliable estimate of total animals at X time. 
                             Future work needs to be done to bin data to get total counts across all families 
                             and tanks in a cohort at set increments to then compare across time.")),
                  tabPanel("Projections", 
                           linebreaks(2),
                           p(" The linear regression below uses provided data to predict mean standard length of abalone across months."),
                           plotOutput("plot3", hover ="plot_hover"), verbatimTextOutput("info"),
                           linebreaks(2),
                           p("Below is linear regression of average abalone per tank using mock data. 
                             It highlights how the inventory data could be used to predict an average total count per tank 
                             based on age of abalone and mean size. From there a group could theoretically input how many tanks 
                             they have to get a general estimate of total count. Alternatively, a group could upload their data 
                             that could then append it to the larger dataset, including past cohorts, to run a linear regression 
                             to predict a final count...still trying to figure out how that could work. 
                              The biggest issue is trying to compare total count to a consistent metric to then predict long term counts. 
                              Number of tanks seemed like my best option, but abalone don't always stay in the same tank and sometimes get consolidated into other tanks throughout the year.  
                             "),
                           plotOutput("plot4", hover ="plot_hover4"), verbatimTextOutput("info4"))
                  
        ),

 
    linebreaks(3),  
    hr(),
    titlePanel("Outstanding issues:"),
    p("1. Rework data to support model to predict total animals at X time"),
    p("2. Add file upload option for satellite facilites to view their newly collected data"),
    p("3. Work on formatting and overall appearance"),
      )
    )
  )


# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
    df_filtered <- reactive({
      
      df1 <- inventory %>% filter(Cohort == input$Cohort)
      
      return(df1)
      
    })
    
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
    
    
    output$plot1<- renderPlot({
      
      df.no.set<-df_filtered() %>% 
      filter(Age.Month > 0) %>% 
        group_by(Cohort, Age.Month) %>% 
        summarise('avg.tank' = (sum(Total.Animals)/n_distinct(Tank)))
 
    ggplot(df.no.set,aes(x = Age.Month, y = avg.tank, color = as.factor(Cohort))) + 
      geom_jitter(alpha=.5) +
      geom_smooth(method="lm")+
      labs(title="Average Count of Abalone per Tank", caption="Data Source: Puget Sound Restoration Fund Inventory Data 2019-2022", color= "Cohort", x="Months Since Settlement Date", y =" Average number of abalone per tank")+
      scale_color_manual(breaks = c("2019", "2020", "2021"),
                         values=c("cadetblue", "darkgrey", "black")) +
      theme_classic()+
      theme(plot.title = element_text(face = "bold", hjust=.5),
            plot.caption = element_text(color= "gray",hjust=0))
  }, res = 96)
  
  #meanSL
    
  
  #Plot2
  output$plot2 <- renderPlot({
    
    df.grouped<-df_filtered() %>% 
      group_by(Cohort, Family,Age.Month) %>% 
      summarise(Mean.Size = mean(Mean.Size))
  
    ggplot(df.grouped,aes(x = Age.Month, y = Mean.Size, color = as.factor(`Cohort`))) + 
      geom_jitter(alpha=.5)+
      scale_color_manual(breaks = c("2019", "2020", "2021"),
                         values=c("cadetblue", "darkgrey", "black")) +
      labs(title="Mean Size of Abalone per Tank", color= "Cohort", x="Months Since Settlement Date", y= "Mean size per tank (mm)")+
      geom_smooth(method="lm", alpha=.25)+
      theme_classic()+
      theme(plot.title = element_text(face = "bold", hjust=.5))
     
      
  }, res = 96)
  
  
  # Generate a summary of the data ----
  #table
  #To do: Figure out how to summarize this table  only after inventory has been filtered by input (df_filtered). Sum by family 
  output$table <- renderReactable({
    
    df.table<-df_filtered() %>% 
      group_by(Cohort, Age.Month) %>% 
      summarise('No. Families' = n_distinct(Family),
                'No. Tanks' = n_distinct(Tank),
                'Total Animals' = sum(Total.Animals),
                'Mean Size (mm)' = round(mean(Mean.Size),2),
                'Average per Tank' = round(sum(Total.Animals)/n_distinct(Tank),0))
    
    reactable(df.table,
                                             sortable = TRUE,
                                            filterable = TRUE,
                                             searchable = TRUE, 
                                             bordered = TRUE, 
                                             striped = TRUE, 
                                             highlight = TRUE)})

  
  #Projections
  #WANT TO: Figure out which data to use for this and which model to use. 
  
  #remove SA
  df<-inventory %>% 
    filter(Facility != "SA") %>% 
    filter(Age.Month > 0) %>% 
    select(Cohort, Family,Tank, Age.Month, Total.Animals,Mean.Size)
  #change to filters
  df$Cohort<-as.factor(df$Cohort)
  df$Family<-as.factor(df$Family)
  df$Tank<-as.factor(df$Tank)
  
  # Define training control
  set.seed(123)
  train.control <- trainControl(method = "repeatedcv", 
                                number = 10, repeats = 3)
  
  # mean.Sl
    #Train the model
  model2 <- train(Mean.Size ~Age.Month + Total.Animals + Cohort, data = df, method = "lm",
                  trControl = train.control)
  
   #add new predictions to dataframe
    df$yhat.size<-predict(object = model2, newdata = df)
 
    #sum data
    sum.inv<-df %>% 
      select(Cohort, Family,Tank, Age.Month, Total.Animals,Mean.Size,yhat.size) %>% 
      filter(!is.na(Mean.Size)) %>% 
      group_by(Age.Month) %>% 
      
      summarise('No. Families' = n_distinct(Family),
                'No. Tanks' = n_distinct(Tank),
                'Total Animals' = sum(Total.Animals),
                'Mean Size (mm)' = mean(Mean.Size),
                'Pred. Mean Size (mm)' = mean(yhat.size),
                'Average per Tank' = sum(Total.Animals)/n_distinct(Tank))

  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  #defined below then use the value computed from this expression
    #Plot3
  #mean.sl
  output$plot3 <- renderPlot({
    ggplot(sum.inv, aes(x = Age.Month, y = sum.inv$`Pred. Mean Size (mm)`)) + 
      geom_jitter(alpha=.5)+
      geom_smooth(method="lm", alpha=.25, color="goldenrod2")+
      labs(y ="Predicted Mean Standard Length (mm)", x="Months Since Settlement Date", title="Predicted Mean Size of Pinto Abalone", subtitle="lm(Mean Size ~ Total Animals + Age)", caption = "Data Source: Puget Sound Restoration Fund Inventory Data 2019 - 2022")+
      theme_classic() +
      theme(plot.title = element_text(face = "bold", hjust=0.5),
            plot.subtitle = element_text(hjust =0.5),
            plot.caption = element_text(color= "gray",hjust=0))
  }, res = 96)
  
    #output info  
    output$info <- renderText({
      xy_str <- function(e) {
        if(is.null(e)) return("\nNULL")
        paste0("\nMonths Since Settlement Date= ", round(e$x, 0), "\nPredicted Mean Standard Length (mm) = ", round(e$y, 2), "\n")
      }
    
      paste0(
        "Predicted Mean Standard Length | lm(Mean Size ~ Total Animals + Age) ", xy_str(input$plot_hover)
      )
    })
    
    
    ###repeat for mock train data
    #Total
    # Train the model
    model <- train(count~month + mean.sl + cohort + no.tank, data = mock.df, method = "lm",
                   trControl = train.control)
    #add new predictions to dataframe
    mock.df$yhat.count<-predict(object = model, newdata = mock.df)
    
    #Plot4
    #count
    output$plot4 <- renderPlot({
      
      sum.m<-mock.df %>% 
      group_by(month) %>% 
        summarise(no.fam = sum(no.fam),
                  no.tank = sum(no.tank),
                  count = sum(count),
                  mean.sl = mean(mean.sl),
                 yhat.count= sum(yhat.count),
                 avg.tank = sum(yhat.count)/sum(no.tank))
      
      ggplot(sum.m, aes(x = month, y =sum.m$avg.tank)) + 
        geom_jitter(alpha=.5)+
        geom_smooth(method="lm", alpha=.25, color="goldenrod2")+
        labs(y ="Predicted Average Abalone Per Tank", x="Months Since Settlement Date", title="Predicted Average Total Count of Pinto Abalone per Tank", subtitle="lm(Total Animals ~ Mean Size + Age + No. Tanks)", caption = "Data Source: Puget Sound Restoration Fund Inventory Data 2019 - 2022")+
        theme_classic() +
        theme(plot.title = element_text(face = "bold", hjust=0.5),
              plot.subtitle = element_text(hjust =0.5),
              plot.caption = element_text(color= "gray",hjust=0))
    }, res = 96)
    
    #output info  
    output$info4 <- renderText({
      xy_str4 <- function(e) {
        if(is.null(e)) return("\nNULL")
        paste0("\nMonths Since Settlement Date= ", round(e$x, 0), "\nPredicted Total Count = ", round(e$y, 2), "\n")
      }
      
      paste0(
        "Predicted Total Count | lm(Total Animals ~ Age + Mean Standard Length (mm)) ", xy_str4(input$plot_hover4)
      )
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)
