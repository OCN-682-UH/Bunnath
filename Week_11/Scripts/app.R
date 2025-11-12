#Author: Zoe Sidana Bunnath
#Date: November 9 2025

#This Shiny app presents a sample survey using wind farm data to illustrate how people make choices between different renewable energy options.

# Load Libraries
library(shiny) # for building Shiny app
library(shinydashboard) # for creating dashboard
library(tidyverse) # for data wrangling
library(shinyjs) # for Javascript for interactivity
library(shinyWidgets) # for widgets
library(shinythemes) # for themes
library(digest) # for creating a unique filename 

outputDir <- "survey_responses" 

# Create the output directory if it doesn't exist
if (!dir.exists(outputDir)) {
  dir.create(outputDir)
}

saveData <- function(data) {
  # Get reactive data, add timestamp, and transpose
  data_df <- data() %>% mutate(timestamp = Sys.time())
  data_t <- t(as.data.frame(data_df))
  
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data_t))
  
  # Write the file to the local system
  write.csv(
    x = data_t,
    file = file.path(outputDir, fileName), 
    row.names = TRUE,  # Set to TRUE to keep the field names as row names
    quote = TRUE
  )
}

loadData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  # The load function here will produce a list of single-column data frames.
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  
  # Combine all data together into one data.frame 
  # NOTE: Combining transposed data requires careful handling to reconstruct rows.
  data <- do.call(rbind, data)
  data
}


# Read in the design ----
design <- readRDS(gzcon(url("https://raw.githubusercontent.com/edsandorf/evdce/refs/heads/main/Data/design-windmills.rds")))
design <- design$design

# Attributes
attribute_descriptions <- c(
  "Size of wind farms",
  "Maximum height of turbines",
  "Reduction of red kite population",
  "Minimum distance to residential areas",
  "Monthly surcharge to power bill"
)

# Function to create a single choice task table for display
create_choice_task <- function(design_row_index, task_number) {
  # Grab the design, create the choice task, and modify for correct display.
  task_data <- design[design_row_index, ] |>
    select(-block) |>
    pivot_longer(
      cols = everything(),
      names_to = c("alt", ".value"),
      values_to = "level",
      names_sep = "_"
    ) |>
    mutate(
      farm = farm2 * 2 + farm3 * 3,
      farm = ifelse(farm == 0 | is.na(farm), 1, farm),
      farm = case_when(
        farm == 1 ~ "Small",
        farm == 2 ~ "Medium",
        farm == 3 ~ "Large"
      ),
      height = height2 * 2 + height3 * 3,
      height = ifelse(height == 0 | is.na(height), 1, height),
      height = case_when(
        height == 1 ~ "Low",
        height == 2 ~ "Medium",
        height == 3 ~ "High"
      ),
      redkite = ifelse(is.na(redkite), 0, redkite + 10),
      redkite = paste0(redkite, "%"),
      distance = ifelse(is.na(distance), 750, distance * 1000 + 750),
      distance = paste0(distance, "m"),
      cost = ifelse(is.na(cost), 0, cost),
      cost = paste0("€", cost)
    ) |>
    select(farm, height,
           redkite, distance, cost, -farm2, -farm3, -height2, -height3, -sq, alt) |>
    pivot_longer(
      cols = -alt,
      names_to = "attribute",
      values_to = "level"
    ) |>
    pivot_wider(
      names_from = alt,
      values_from = level
    ) |>
    mutate(
      attribute = attribute_descriptions
    )
  
  # Add the choice row
  choice_row <- c(
    "",
    paste('<input type="radio" name="ct', task_number, '" value="Prog 1"/>', sep = ""),
    paste('<input type="radio" name="ct', task_number, '" value="Prog 2"/>', sep = ""),
    paste('<input type="radio" name="ct', task_number, '" value="Prog 3"/>', sep = "")
  )
  
  final_table <- rbind(task_data, choice_row)
  rownames(final_table)[nrow(final_table)] <- "?WHICH PROGRAMME DO YOU PREFER?"
  
  return(final_table)
}


## Setting up the Shiny Application

ui <- dashboardPage(
  dashboardHeader(title = "My Shiny DCE App"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome"),
      menuItem("Sample Survey", tabName = "sample_survey")
    )
  ),
  ## Body content
  dashboardBody(
    # Must use shinyjs
    useShinyjs(),
    tabItems(
      # First tab content
      tabItem(tabName = "welcome",
              h2("Welcome to My Shiny DCE App!"),
              p("This app explores how people make trade-offs when faced with different options. 
                Discrete Choice Experiments (DCEs) help researchers understand what people value most 
                — such as price, quality, or environmental benefits."),
              br(), 
              # User input for name
              textInput("username", "What’s your name?", placeholder = "Enter your name here"),
              
              # make a calendar to select a date 
              dateInput("date", 
                        label = h3("Select Today's Date"), 
                        value = "2025-11-01"), 
              hr(),
              fluidRow(column(3, verbatimTextOutput("value"))),
              
              br(), 
              
              # Button to trigger a welcome message
              actionButton("goButton", "Get Started!"),
              br(), br(),
              
              # Display personalized text
              h4(textOutput("welcome_text")),
              hr(),
              
              # Footer note
              p("You can now click on 'Sample Survey' in the sidebar to explore the DCE!")
      ),
      
      # Second tab content
      tabItem(tabName = "sample_survey",
              
              fluidPage(
                
                # Set the theme
                theme = shinytheme("cosmo"),
                
                # Define some CSS
                tags$style(
                  type = "text/css",
                  "tr:last-child {border-bottom:1px solid #D5D5D5;}
                   td {border-left:1px solid #D5D5D5; border-right:1px solid #D5D5D5;}
                   .choicebuttons .radio-inline {margin: 0 10% 0 3%;}",
                  HTML("td:first-child { font-weight: bold }")
                ),
                
                # Application title
                title = "Discrete Choice Experiment",
                
                # Define the title panel
                titlePanel(
                  div(
                    p("Sample Discrete Choice Experiment"),
                    align = "center"
                  )
                ),
                
                # Survey introduction - This is the only one shown at first
                div(
                  id = "intro",
                  h4("Introduction"),
                  img(src = "windfarm.jpg", 
                      height = "250px", 
                      width = "100%"
                  ), #add windfarm image
                  br(),
                  p("This sample survey uses wind farm data to explore how people make choices about renewable energy development. Each scenario presents different wind farm options that vary in features such as distance from homes, number of turbines, landscape impact, and cost to households. By comparing these options, the survey helps demonstrate how people weigh trade-offs between environmental benefits and local impacts. The goal is to better understand which factors influence public support for renewable energy projects.", style = "text-align: left;"),
                  h4("Thank you for your participation"),
                  actionButton(
                    inputId = "click2socdem",
                    label = "Next",
                    style = "text-align: center;"
                  )
                ),
                
                # Socio-demographic questions wrapped in hidden()
                hidden(
                  div(
                    id = "socdem",
                    h3("Sociodemographic characteristics"),
                    
                    # Gender
                    radioButtons(
                      inputId = "gender",
                      label  = "Please select your gender",
                      choices = c(
                        "Female" = "female",
                        "Male" = "male",
                        "Other" = "other",
                        "Prefer not to say" = "prefer_not-to-say"
                      ),
                      inline  = TRUE
                    ),
                    
                    # Age
                    numericInput(
                      inputId = "age",
                      label = "What is your age?",
                      value = NULL,
                      min = 16,
                      max = 100,
                      step = 1
                    ),
                    
                    #Educational level
                    radioButtons(
                      inputId = "edu",
                      label  = "What is your highest completed education degree?",
                      choices = c(
                        "None completed or primary" = "primary",
                        "Secondary education" = "secondary",
                        "College or higher" = "tertiary"
                      ),
                      inline  = TRUE
                    ),
                    
                    # Include the action button send people to the next page of the survey
                    actionButton(
                      inputId = "click2explanation",
                      label = "Next"
                    ),
                    style = "text-align: center;"
                  )
                ),
                
                
                # Explanation of the DCE wrapped in hidden()
                hidden(
                  div(
                    id = "explanation",
                    p(
                      "Next, we will show you two different hypothetical scenarios.
                      We will ask you to please answer, for each one of them, which of the three
                      possible programmes would you choose regarding onshore wind power.",
                      style = "text-align: left;"
                    ),
                    p(
                      "All answers are anonymous and confidential. There are no right or wrong
                      answers. We simply want to know your opinion.",
                      style = "text-align: left;"
                    ),
                    
                    # Include the action button to send people on to the next page
                    actionButton(
                      inputId = "click2choice1",
                      label = "Next"
                    ),
                    style = "text-align: center;"
                  )
                ),
                
                # Choice task 1 wrapped in hidden()
                hidden(
                  div(
                    id = "choicetask1",
                    h4(
                      strong("Choice task number 1"),
                      style = "text-align: center;"
                    ),
                    
                    div(
                      id = "ct1",
                      class = "shiny-input-radiogroup",
                      tableOutput("tct1")
                    ),
                    
                    # Include the action button that sends people to the next choice task.
                    actionButton(
                      inputId = "click2choice2",
                      label = "Next"
                    ),
                    style = "text-align: center;"
                  )
                ),
                
                # Choice task 2 wrapped in hidden()
                hidden(
                  div(
                    id = "choicetask2",
                    h4(
                      strong("Choice task number 2"),
                      style = "text-align: center;"
                    ),
                    
                    div(
                      id = "ct2",
                      class = "shiny-input-radiogroup",
                      tableOutput("tct2")
                    ),
                    
                    # Include the action button that sends people to the next choice task.
                    actionButton(
                      inputId = "click2protest",
                      label = "Next"
                    ),
                    style = "text-align: center;"
                  )
                ),
                
                # Gathering data on potential protesters wrapped in hidden()
                hidden(
                  div(
                    id = "protest",
                    textInput(
                      "protest",
                      "Why did you choose programme 1 in all of the situations presented?",
                      width = 500
                    ),
                    actionButton(
                      inputId = "click2thanksprotest",
                      label = "Next"
                    ),
                    style = "text-align: center;"
                  )
                ),
                
                # Show a message while saving the results wrapped in hidden()
                hidden(
                  h3(
                    id = "savingmessage",
                    "We are saving your answers, this will take a moment ..."
                  )
                ),
                
                hidden(
                  div(
                    id = "submit",
                    actionButton(
                      inputId = "click2thanks",
                      label = "Submit Final Answers"
                    ),
                    style = "text-align: center;"
                  )
                ),
                
                # Thank you slide wrapped in hidden()
                hidden(
                  div(
                    id = "thankyou",
                    h3("Thank you for participating in this survey."),
                    p(
                      "The results will be available in",
                      a(href = "http://www.et.bs.ehu.es/~etpmaxxp/", "this website"),
                      "shortly."
                    ),
                    div(
                      tags$button(
                        id = "close",
                        type = "button",
                        class = "btn action-button",
                        onclick = "setTimeout(function(){window.close();},500);",
                        "End survey"
                      ),
                      style = "text-align: center;")
               )
            )
         )
      )
    )
  )
)


server <- function(input, output, session) {
  
  # --- Setup for Choice Tasks (Inside Server for Multi-User Safety) ---
  nct_shown <- 2
  
  # Randomly select the rows from the design matrix
  rct <- sample(seq_len(nrow(design)), nct_shown)
  
  # Create the two choice task tables using the defined function
  ct_shown_1 <- create_choice_task(rct[1], 1)
  ct_shown_2 <- create_choice_task(rct[2], 2)
  
  # --- Welcome Tab Logic ---
  observeEvent(
    input$goButton, {
      req(input$username)
      shinyjs::alert(paste("Welcome,",
                           input$username,
                           "! Let’s explore discrete choice experiment together!")
      )
    }
  )
  
  output$welcome_text <- renderText({
    if (input$username != "") {
      paste("Hi",
            input$username,
            "— today is",
            format(input$date, "%A, %B %d, %Y")
      )
    } else {
      paste("Please enter your name to begin.")
    }
  }
  )
  
  output$value <- renderPrint(
    {input$date}
  ) 
  
  # --- Survey Tab Logic ---
  
  # Define a set of mandatory fields
  mandatory_fields <- c("gender", "age", "edu")
  
  # Observer: Intro to Socdem
  observeEvent(
    input$click2socdem, {
      hide("intro")
      show("socdem")
      
      #Only show next button when answers are filled
      observe({
        mandatory_filled <- vapply(
          mandatory_fields,
          function(x) {
            !is.null(input[[x]]) && input[[x]] != ""
          },
          logical(1)
        )
        
        # Check if all mandatory fields are filled
        mandatory_filled <- all(mandatory_filled)
        
        # Toggle the next button
        toggleState(
          id = "click2explanation",
          condition = mandatory_filled && !is.na(input$age) && input$age >= 16
        )
      })
    }
  )
  
  # Observer: Socdem to Explanation
  observeEvent(
    input$click2explanation, {
      hide("socdem")
      show("explanation")
    }
  )
  
  # Observer: Explanation to Choice Task 1
  observeEvent(
    input$click2choice1, {
      hide("explanation")
      show("choicetask1")
      
      #Only show next button when a choice is selected:
      observe({
        toggleState(
          id = "click2choice2",
          condition = !is.null(input$ct1)
        )
      })
    }
  )
  
  # Observer: Choice Task 1 to Choice Task 2
  observeEvent(
    input$click2choice2, {
      hide("choicetask1")
      show("choicetask2")
      show("submit")
      
      #Only show next button when a choice is selected:
      observe({
        toggleState(
          id = "click2thanks",
          condition = !is.null(input$ct2)
        )
      })
    }
  )
  
  # Reactive object to temporarily save the user's data
  form_data <- reactive({
    tibble(
      cm1 = input$ct1,
      cm2 = input$ct2,
      cs1 = rct[1],
      cs2 = rct[2],
      gender = input$gender,
      age = input$age,
      edu = input$edu,
      protest = input$protest # will be NULL if skipped
    )
  })
  
  # Observer: Submit button logic (Choice Task 2, Protest, Saving, Thank you)
  observeEvent(
    input$click2thanks, {
      
      # Show protest question if all choices are for the Status Quo (assuming Prog 1 is SQ)
      if (input$ct1 == "Prog 1" && input$ct2 == "Prog 1") {
        hide("choicetask2")
        hide("submit")
        show("protest")
        
        # Enable the next button only when text is entered
        observe({
          toggleState(
            id = "click2thanksprotest",
            condition = nchar(input$protest) > 0
          )
        })
        
        # Save data after protest question is answered
        observeEvent(
          input$click2thanksprotest, {
            hide("protest")
            show("savingmessage")
            # Save data using the multiple-file method
            saveData(form_data)
            hide("savingmessage")
            show("thankyou")
          }
        )
        
      } else { # If not protesting, proceed to save directly
        hide("choicetask2")
        hide("submit")
        show("savingmessage")
        # Save data using the multiple-file method
        saveData(form_data)
        hide("savingmessage")
        show("thankyou")
      }
    }
  )
  
  # Render the choice task tables
  output$tct1 <- renderTable(
    ct_shown_1,
    sanitize.text.function = function(x) {
      x
    },
    align = c("lccc"),
    width = "90%",
    spacing = "s",
    striped = TRUE
  )
  
  output$tct2 <- renderTable(
    ct_shown_2,
    sanitize.text.function = function(x) {
      x
    },
    align = c("lccc"),
    width = "90%",
    spacing = "s",
    striped = TRUE
  )
  
  # Final observer to ensure that the app closes when the user is done
  observeEvent(input$close, {
    stopApp()
  })
}

# Run the application
shinyApp(ui = ui, server = server)