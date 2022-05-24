library(shiny)

#############################################################
ui <- fluidPage(
  ##INTRO
  titlePanel("Outline of Pinto Abalone Summary App"),
  p("The goal of this app is to display projections of the total count and mean size of post-settled pinto abalone at X time in a conservation aquaculture setting. 
    Larval abalone families are settled into individual tanks and then reared in those tanks for the first year of their life prior to being outplanted into the wild. 
    Bimonthly counts and mean standard lengths are taken of a subset of animals in each tank. Bimonthly counts help inform how many animals will be greater than 5 mm at the 
    time of outplant, a requirement for outplanting. 
    
    My hope is to create an app/model that uses the bimonthly data to 1) track the current growth and survival of animals 
    in the hatchery via table and visual summary and 2) project the estimated size and total count of animals at the time of outplant. This app will be extremely helpful for 
    the management and logistical planning of outplanting pinto abalone each year. The field season planning occurs months prior to the final count of animals and having a 
    more fine-tuned method of projecting the final count will be extremely beneficial to this process. It will also be a place where partners can check on the status of the 
    animals at various satellite facilities that rear pinto abalone."),
  ##FILTERS FOR WHOLE PAGE
  titlePanel("SelectInputs//filtering", ),
  p("Add selectInput drop down options (cohort//family//facility) here for filtering graphs/tables/and projections below"),

  
  ## TOTAL COUNT
  titlePanel("Total animals by cohort//family//facility"),
  p("insert graph of total counts to date"),
  hr(),
  ## MEANSL
  titlePanel("Mean Standard Length by cohort//family//facility"),
  p("Similar to above but insert graph of mean standard length to date "),
  hr(),
  
  ##SUMMARY TABLE
  titlePanel("Summary table of counts and size"),
  p("Insert table with all information from above graphs "),
  hr(),
  hr(),
  
  ##PROJECTIONS
  titlePanel("Projection Data"),
  p("This section will include the model that works best with projecting numbers based on previous years survival. If possible, I think it would be ideal if there was one 
    overarching filtering mechanism that changed the outcome for all graphs/tables/projections i.e. (only want to look at one facility and one cohort for all counts, meanSL, 
    and projections versus having that options repeat for each section). Might be nice to also include a graph of predicted vs observed for the user to see the confidence of the model. ")
)



#######################################################
server <- function(input, output) {
  
 
}

# Run the application 
shinyApp(ui = ui, server = server)