#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyjs)
library(audio)
library(dplyr)
library(tidyr)
# audio_dir <- "../Dissertation/Part1/Exp1/Recordings/06_ResynthesizedRecordings/"
# tg_dir <- "../Dissertation/Part1/Exp1/Recordings/06_ResynthesizedRecordings/TextGrids"
# pt_dir <- "../Dissertation/Part1/Exp1/Recordings/06_ResynthesizedRecordings/PitchTiers"

# All the files in each subdirectory are the same, so we need to read in
# just one to get the menu options
base_dir <- "06_ResynthesizedRecordings/ConstantScaling_100"
wav_files <- list.files(base_dir, pattern = ".wav$")
file_info <- 
  data.frame(file = wav_files) |> 
  separate(col = file,
           sep = "_",
           into = c('utterance', 'session','tune','take','pa','bt'))

# Set menu options
utterances <- unique(file_info$utterance)
tunes <- unique(file_info$tune)

experiment_directories <- c("06_ResynthesizedRecordings",
                            "06c_ResynthesizedRecordingsExp3",
                            "06c_ResynthesizedRecordingsExp3CURVED")

# Get all the audio directories with files
file_dirs <- list.dirs(experiment_directories,recursive = FALSE)

# Change slightly to get virtual directory names for howler to use
set_subdirectories <- 
  gsub("06.?_ResynthesizedRecordings[Exp3/]+","",file_dirs) |> 
  unique()

virtual_dir_names <- paste0(rep(c('exp1','exp3'), each = length(set_subdirectories)),
                            set_subdirectories)

# Set the names on the virtual dirs to match the user input
names(virtual_dir_names) <- file_dirs

# Add each individual virtual directory with the preformed names
for (dir_i in seq_along(file_dirs)) {
  addResourcePath(virtual_dir_names[dir_i], file_dirs[dir_i])
}


ui <- dashboardPage(
  dashboardHeader(title = "Dynamic selectInput"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItemOutput("menuitem")
    )
  ),
  dashboardBody(
    numericInput("go_btns_quant","Continuum 1: Number of Steps",value = 5,min = 1,max = 10),
    numericInput("c2_btns_quant","Continuum 2: Number of Steps",value = 5,min = 1,max = 10),
    shiny::selectInput("utterances", "Utterances to play", choices = utterances, multiple = TRUE),
    shiny::selectInput("tunes", "Original Tunes to play", choices = tunes, multiple = TRUE),
    shiny::selectInput("exp_dir", "Which experiment stimuli to play", 
                       choices = experiment_directories),
    shiny::selectInput("file_dir", "Which set of files to play", 
                       choices = set_subdirectories, multiple=FALSE,width = '500px'),
    useShinyjs(),
    includeScript('www/howler.js'),
    uiOutput("go_buttons"),
    plotOutput("plot")
  ),
  
)

server <- function(input, output, session) {
  output$menuitem <- renderMenu({
    menuItem("Menu item", icon = icon("calendar"))
  })
  
  # to store observers and make sure only once is created per button
  obsList <- list()
  
  # output$audio_dir <- input$file_dir
  
  output$go_buttons <- renderUI({
    # Create the grid of buttons by creating individual columns, where each
    # row is thus one actionbutton that plays audio. The outer lapply then
    # creates the multiple columns
    buttons <- as.list(rev(1:input$go_btns_quant))
    buttons <- 
      # Creates columns
      lapply(1:input$c2_btns_quant,
             \(x) {
               buts <- 
                 # Creates rows
                 lapply(buttons, 
                        function(i)
                        {
                          btName <- paste0("go_btn",i,x)
                          # creates an observer only if it doesn't already exists
                          if (is.null(obsList[[btName]])) {
                            # make sure to use <<- to update global variable obsList
                            obsList[[btName]] <<- observeEvent(input[[btName]],ignoreInit = FALSE, {
                              # Determine all the files to play given the menu options
                              utts_tunes <- expand.grid(input$utterances, input$tunes)
                              pat <- paste0("(",paste0(utts_tunes$Var1,"_.._", utts_tunes$Var2), 
                                     paste0("_..._",i,"_",x,".wav"),")", collapse = "|")
                              real_path <- paste0(input$exp_dir, "/", input$file_dir)
                              audio_files <- list.files(real_path, 
                                                        pattern = pat)
                              print(audio_files)
                              
                              # Create the string to represent the javascript
                              # array of filepaths to load and play
                              audio_call <- paste0("'", 
                                                   virtual_dir_names[real_path],
                                                   "/",
                                                   audio_files, 
                                                   "'", collapse = ", ")
                              
                              # Create the Howler call and play each file
                              # in sequence. The onend behavior is so that
                              # each file plays only after the previous
                              # one ends, rather than playing all at once.
                              # This is done by forcing each sound object
                              # to play the next one, and the last sound object
                              # will just print 'done' to the console.
                              play_call <- paste0("
                                                      let myFiles = [",
                                                  audio_call,
                                                  "];
                                                      let sounds = new Array(myFiles.length);
                                                      
                                                      myFiles.forEach((url, i) => {
                                                      sounds [i] = new Howl({src: [url]});
                                                      sounds[i].on('end', function() {
                                                        if (i < myFiles.length-1) {
                                                          sounds[i+1].play();
                                                        } else{
                                                        console.log('done');
                                                      }})
                                                  }); sounds[0].play()")
                              
                              # Run the javascript code
                              shinyjs::runjs(play_call)
                              
                              cat("Button ", i, x, "\n")
                            })
                          }
fluidRow(
  actionButton(btName,paste("Go",i,x))
)
                        })
               column(2, buts, 1)
             }
      )
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
