library(knitr)
library(shiny)
library(ggplot2)
library(shinythemes)
library(plotly)
library(shinydashboard)


# Load data ---------------------------------------------------------------
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

# Server ------------------------------------------------------------------
# Define server logic required to draw spectra
server <- function(input, output, session) {
  
  # reactive dataset generation
  make_data <- eventReactive(input$go, {
    runif(input$hippurate)
    if(input$hippurate == 2) {
      reactive_1_s <- data.frame("ppm" = reactive_1[,1], "intensity" = reactive_1[,(4)])
      reactive_2_s <- data.frame("ppm" = reactive_2[,1], "intensity" = reactive_2[,(4)])
      reactive_3_s <- data.frame("ppm" = reactive_3[,1], "intensity" = reactive_3[,(4)])
      reactive_4_s <- data.frame("ppm" = reactive_4[,1], "intensity" = reactive_4[,(4)])
      mkdb <- rbind(static, reactive_1_s, reactive_2_s, reactive_3_s, reactive_4_s)
    } else {
      reactive_1_s <- data.frame("ppm" = reactive_1[,1], "intensity" = reactive_1[,(2 + as.numeric(input$hippurate))])
      reactive_2_s <- data.frame("ppm" = reactive_2[,1], "intensity" = reactive_2[,(2 + as.numeric(input$tartrate))])
      reactive_3_s <- data.frame("ppm" = reactive_3[,1], "intensity" = reactive_3[,(2 + as.numeric(input$carnitine))])
      reactive_4_s <- data.frame("ppm" = reactive_4[,1], "intensity" = reactive_4[,(2 + as.numeric(input$tmao))])
      mkdb <- rbind(static, reactive_1_s, reactive_2_s, reactive_3_s, reactive_4_s)
    }
  })
  
  # reactivily generate a new dataset for plotting calling make_data(), which takes ui input
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
  
  # output$spectra_text <- renderUI({
  #   HTML(paste("As you have eaten"))
  #   
  #   
  #     fruits[input$index] <- paste("<b>",fruits[input$index],"</b>")
  #     HTML(paste(fruits))
  #   })
  

  # once ui selections are made, go to the tab with spectra and update spectra
  observeEvent(input$go, {
    updateTabItems(session = session, inputId = "tabs", selected = "spectra")
  })
  
  observeEvent(input$go2learn, {
    updateTabItems(session = session, inputId = "tabs", selected = "learn")
  })
  
  
  output$image1<-renderText({c('<img src="',"http://www.imperial.ac.uk/ImageCropToolT4/imageTool/uploaded-images/CSM_Web_002--tojpeg_1490287529495_x2.jpg",'">')})
}


# UI ---------------------------------------------------------------
# Define UI for application that draws spectra

body <-  dashboardBody(
  
  tabItems(
    
    tabItem(tabName = "about",
            fluidRow(
            box(
                includeMarkdown("./www/about.md"),
                width = 8),
            
            # import image through url using output$image1
            box(htmlOutput("image1", width = "190.8173", height = "315"),
                width = 4)
            
            # box(img(src = "NMR.png", width = "190.8173", height = "315", align = "center"),
            #     width = 4)
            )
    ),
    
    tabItem(tabName = "describe",
            
            box(title = "Describe your meals in the last 24 hours",
                 width = 8,
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
                 actionButton(inputId = "go",
                              label = "See spectra!",
                              style="color: #fff;
                               #background-color: #337ab7;
                              background-color: #31212d;
                              #border-color: #2e6da4;
                              border-color: #222d32;"
                              )
                )
            ),
    
    tabItem(tabName = "spectra",
              box(title = "Your spectra",
                  width = 8,
                  "Your dietary habits, even those forbidden snacks, can be identified by running your urine sample through an NMR machine.
                  Here is how your urine would likely to look, based on your selected inputs.",
                  
                  plotOutput("spectra"),
                  
                  "Each peak in your urine NMR spectra represents a specific food item.", 
             
                  actionButton(inputId = "go2learn",
                               label = "Explore!",
                               style="color: #fff;
                               #background-color: #337ab7;
                               background-color: #31212d;
                               #border-color: #2e6da4;
                               border-color: #222d32;"
                               )
              )
    ),
            
    tabItem(tabName = "learn",
            box(
                includeMarkdown("./www/learn.md"),
                width = 8
                )
            ),
  
    tabItem(tabName = "more",
            box(
               includeMarkdown("./www/more.md"),
              width = 8
              )
          )
  ),
  tags$footer(
              tags$a(href="https://github.com/lauzikaite", "Code available @GitHub"),
              style = "color: white;
              position:absolute;
              bottom:0;
              padding: 3px;
              background-color: #222d32;
              color: white;
              width:100%;
              z-index: 1000;
              "
              ),
  
  tags$head(tags$style("#myplot{height:100vh !important;}"))
)


ui <- dashboardPage(

  skin = "black",
  
  dashboardHeader(title = "Spectra game"),
  
  dashboardSidebar(
    
    sidebarMenu(id = "tabs",
      
      menuItem("About", tabName = "about", icon = icon("exclamation-circle")),
      
      menuItem("Describe your diet", tabName = "describe", icon = icon("check")),
      
      menuItem("Look at it!", tabName = "spectra", icon = icon("binoculars")),
      
      menuItem("Learn about it!", tabName = "learn", icon = icon("book")),
      
      menuItem("More?", tabName = "more", icon = icon("question-circle"))
    )
  ),
  
  body
  
)

# Create Shiny app ----
shinyApp(ui, server)