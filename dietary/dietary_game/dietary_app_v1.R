library(shiny)
library(ggplot2)
library(shinythemes)


# Load data ---------------------------------------------------------------
# setwd("/Users/el1514/Documents/Scripts/Github-all/Github-R/game/dietary")

static <- read.table("./dietary_game/spectra_static.txt", header = T, stringsAsFactors = F)[,c("ppm","intensity5")]
colnames(static) <- c("ppm", "intensity")

reactive_1 <- read.table('./dietary_biomolecules/Hippuric_acid_new.txt', header = T, stringsAsFactors = F)
# default - almost none, intensity1 - huge levels, intensity2 - medium levels

reactive_2 <- read.table('./dietary_biomolecules/Tartaric_acid_new.txt', header = T, stringsAsFactors = F)
# default - almost none, intensity1 - huge levels, intensity2 - medium levels

reactive_3 <- read.table('./dietary_biomolecules/L-Carnitine_new.txt', header = T, stringsAsFactors = F)
# default - almost none, intensity1 - huge levels, intensity2 - medium levels

reactive_4 <- read.table('./dietary_biomolecules/TMAO_new.txt', header = T, stringsAsFactors = F)
# default - almost none, intensity1 - huge levels, intensity2 - medium levels

# Check UI --------------------------------------------------------
# # ui will be used to select cond from available combinations
# cond <- 0 # default
# cond <- 1 # cond1
# cond<- 2 # cond2
# 
# # build reactive spectra 
# spectra_r <- reactive_4[,c(1,(2+cond))]
# colnames(spectra_r ) <- c("ppm", "intensity")
# 
# ## build static spectra
# omit <- which(static$ppm %in% spectra_r$ppm)
# spectra_s <- as.data.frame(static[-omit,])
# colnames(spectra_s) <- c("ppm", "intensity")
# data <- rbind(spectra_s, spectra_r) 
# data <- subset(data, ppm >0)
# 
# ggplot() +
#   geom_line(data = data, aes(x=-ppm, y=intensity)) +
#   theme_bw() +
#   theme(axis.text = element_text(size=12),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) +
#   xlab("ppm") +
#   scale_x_continuous(breaks = c(0, -2, -4, -6, -8, -10), labels = c(0, 2, 4, 6, 8, 10)) +
#   scale_y_continuous(limits = c(0, 150))

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

# Define server logic required to draw spectra
server <- function(input, output) {
  
  
  make_data <- eventReactive(input$go, {
    runif(input$hippurate)
    if(input$hippurate == 2) {
      reactive_1_s <- data.frame("ppm" = reactive_1[,1], "intensity" = reactive_1[,(4)])
      reactive_2_s <- data.frame("ppm" = reactive_2[,1], "intensity" = reactive_2[,(4)])
      reactive_3_s <- data.frame("ppm" = reactive_3[,1], "intensity" = reactive_3[,(4)])
      reactive_4_s <- data.frame("ppm" = reactive_4[,1], "intensity" = reactive_4[,(4)])
      mkdb <- rbind(static, reactive_1_s, reactive_2_s, reactive_3_s, reactive_4_s)
      mkdb <- subset(mkdb, ppm >0)
    } else {
      reactive_1_s <- data.frame("ppm" = reactive_1[,1], "intensity" = reactive_1[,(2 + as.numeric(input$hippurate))])
      reactive_2_s <- data.frame("ppm" = reactive_2[,1], "intensity" = reactive_2[,(2 + as.numeric(input$tartrate))])
      reactive_3_s <- data.frame("ppm" = reactive_3[,1], "intensity" = reactive_3[,(2 + as.numeric(input$carnitine))])
      reactive_4_s <- data.frame("ppm" = reactive_4[,1], "intensity" = reactive_4[,(2 + as.numeric(input$tmao))])
      mkdb <- rbind(static, reactive_1_s, reactive_2_s, reactive_3_s, reactive_4_s)
      mkdb <- subset(mkdb, ppm >0)
      }
  })
  

  output$spectra <- renderPlot({
    ggplot() +
      geom_line(data = make_data(), aes(x=-ppm, y=intensity)) +
      theme_bw() +
      theme(axis.text = element_text(size=12),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      xlab("ppm") +
      scale_x_continuous(breaks = c(0, -2, -4, -6, -8, -10), labels = c(0, 2, 4, 6, 8, 10)) +
      scale_y_continuous(limits = c(0, 150))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
