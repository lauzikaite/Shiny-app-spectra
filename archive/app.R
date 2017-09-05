library(shiny)
library(ggplot2)
library(shinythemes)


# Load data ---------------------------------------------------------------
# setwd("/Users/el1514/Documents/Scripts/Github-all/Github-R/game/dietary")

static <- read.table("spectra_static.txt", header = T, stringsAsFactors = F)[,c("ppm","intensity5")]
colnames(static) <- c("ppm", "intensity")

reactive_1 <- read.table('Hippuric_acid_new.txt', header = T, stringsAsFactors = F)
# default - almost none, intensity1 - huge levels, intensity2 - medium levels

reactive_2 <- read.table('Tartaric_acid_new.txt', header = T, stringsAsFactors = F)
# default - almost none, intensity1 - huge levels, intensity2 - medium levels

reactive_3 <- read.table('L-Carnitine_new.txt', header = T, stringsAsFactors = F)
# default - almost none, intensity1 - huge levels, intensity2 - medium levels

reactive_4 <- read.table('TMAO_new.txt', header = T, stringsAsFactors = F)
# default - almost none, intensity1 - huge levels, intensity2 - medium levels

# Shiny App ---------------------------------------------------------------

# Define UI for application that draws spectra
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                # Application title
                titlePanel("What's in your urine?"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    p("Describe your meals in the last 24 hours"),
                    selectInput(inputId = "hippurate",
                                label = "How many fruits or vegetables did you have?",
                                c("None, I am a meat-lover" = 0, "Plenty, I live on fruits and vegetables!" = 1, "A single fruit or a small salad" = 2),
                                selected = 2),
                    selectInput(inputId = "tartrate",
                                label = "Did you have any fresh grapes?",
                                c("None" = 0, "More than a handful, grapes are my favourite snack" = 1, "Just a few, I went to a conference buffet lunch" = 2),
                                selected = 2),
                    selectInput(inputId = "carnitine",
                                label = "Did you eat any meat?",
                                c("None, I am a vegetarian" = 0, "A juicy burger for lunch and fried chichen for dinner" = 1, "A healthy portion of lean meat" = 2),
                                selected = 2),
                    selectInput(inputId = "tmao",
                                label = "Did you have any fish or seafood?",
                                c("None" = 0, "A posh platter of seafood" = 1, "Some fish sauce definitely got into my plate" = 2),
                                selected = 2),
                    
                    actionButton("go", "See how it looks!")
                  ),
                  
                  # Show a plot
                  mainPanel(
                    p("Your dietary habits, even those forbidden snacks, can be identified by running your urine sample through an NMR machine.
                      Here is how your urine would likely to look, based on your selected inputs."),
                    plotOutput("spectra")
                    )
                )
                )
