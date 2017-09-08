library(knitr)
library(shiny)
library(ggplot2)
library(shinythemes)
library(plotly)
library(shinydashboard)

# version2.0: 

# Load data ---------------------------------------------------------------
static <- read.table("Static.txt", header = T, stringsAsFactors = F)

reactive_1 <- read.table('Hippurate.txt', header = T, stringsAsFactors = F)
# default - almost none, intensity1 - huge levels, intensity2 - medium levels

reactive_2 <- read.table('Tartrate.txt', header = T, stringsAsFactors = F)
# default - almost none, intensity1 - huge levels, intensity2 - medium levels

reactive_3 <- read.table('Carnitine.txt', header = T, stringsAsFactors = F)
# default - almost none, intensity1 - huge levels, intensity2 - medium levels

reactive_4 <- read.table('TMAO.txt', header = T, stringsAsFactors = F)
# default - almost none, intensity1 - huge levels, intensity2 - medium levels

# Server ------------------------------------------------------------------
# Define server logic required to draw spectra
server <- function(input, output) {
  options(warn =-1) 
  
  # reactive dataset generation
  make_data <- eventReactive(input$go, {

    reactive_1_s <- data.frame("ppm" = reactive_1[,1], "intensity" = reactive_1[,(2 + as.numeric(input$hippurate))], "peak" = reactive_1$peak)
    reactive_2_s <- data.frame("ppm" = reactive_2[,1], "intensity" = reactive_2[,(2 + as.numeric(input$tartrate))], "peak" = reactive_2$peak)
    reactive_3_s <- data.frame("ppm" = reactive_3[,1], "intensity" = reactive_3[,(2 + as.numeric(input$carnitine))], "peak" = reactive_3$peak)
    reactive_4_s <- data.frame("ppm" = reactive_4[,1], "intensity" = reactive_4[,(2 + as.numeric(input$tmao))], "peak" = reactive_4$peak)
    
    mkdb <- data.frame("ppm" = c(static$ppm, reactive_1_s$ppm, reactive_2_s$ppm, reactive_3_s$ppm, reactive_4_s$ppm),
                       "intensity" = c(static$intensity, reactive_1_s$intensity, reactive_2_s$intensity, reactive_3_s$intensity, reactive_4_s$intensity),
                       "type" = c(rep("Other", nrow(static)), rep("Hippurate", nrow(reactive_1_s)), rep("Tartrate", nrow(reactive_2_s)), rep("Carnitine", nrow(reactive_3_s)), rep("TMAO", nrow(reactive_4_s))),
                       "peak" = as.factor(c(static$peak, reactive_1_s$peak, reactive_2_s$peak, reactive_3_s$peak, reactive_4_s$peak)))
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
  
  # reactivily generate a second ggplot which will show the peaks
  output$spectra2 <- renderPlotly({
    
    plot_ly(data = subset(make_data(), type == "Other"), x = ~ ppm , y = ~intensity, type = 'scatter', mode = 'lines',
            line = list(width = 0.8, color = "black"),
            name = "Other",
            showlegend = TRUE) %>%
      add_lines(data = subset(make_data(), type == "Carnitine" & peak == 1), y = ~intensity, type = 'scatter', mode = 'lines', name = "Carnitine", line = list(width = 0.8, color = '#7b3294')) %>%
      add_lines(data = subset(make_data(), type == "Carnitine" & peak == 2), y = ~intensity, type = 'scatter', mode = 'lines', name = "Carnitine", line = list(width = 0.8, color = '#7b3294'), showlegend = FALSE) %>%
      add_lines(data = subset(make_data(), type == "Carnitine" & peak == 3), y = ~intensity, type = 'scatter', mode = 'lines', name = "Carnitine", line = list(width = 0.8, color = '#7b3294'), showlegend = FALSE) %>%
      add_lines(data = subset(make_data(), type == "Carnitine" & peak == 4), y = ~intensity, type = 'scatter', mode = 'lines', name = "Carnitine", line = list(width = 0.8, color = '#7b3294'), showlegend = FALSE) %>%
      add_lines(data = subset(make_data(), type == "Hippurate" & peak == 1), y = ~intensity, type = 'scatter', mode = 'lines', name = "Hippurate", line = list(width = 0.8, color = '#a6dba0')) %>%
      add_lines(data = subset(make_data(), type == "Hippurate" & peak == 2), y = ~intensity, type = 'scatter', mode = 'lines', name = "Hippurate", line = list(width = 0.8, color = '#a6dba0'), showlegend = FALSE) %>%
      add_lines(data = subset(make_data(), type == "Hippurate" & peak == 3), y = ~intensity, type = 'scatter', mode = 'lines', name = "Hippurate", line = list(width = 0.8, color = '#a6dba0'), showlegend = FALSE) %>%
      add_lines(data = subset(make_data(), type == "Hippurate" & peak == 4), y = ~intensity, type = 'scatter', mode = 'lines', name = "Hippurate", line = list(width = 0.8, color = '#a6dba0'), showlegend = FALSE) %>%
      add_lines(data = subset(make_data(), type == "Tartrate" & peak == 1), y = ~intensity, type = 'scatter', mode = 'lines', name = "Tartrate", line = list(width = 0.8, color = '#c2a5cf')) %>%
      add_lines(data = subset(make_data(), type == "TMAO" & peak == 1), y = ~intensity, type = 'scatter', mode = 'lines', name = "TMAO", line = list(width = 0.8, color = '#008837')) %>%
      layout(legend = list(x = 0.1, y = 1, orientation="h")) %>%
      layout(xaxis = list(title = "ppm",
                          range = c(9, 0), autorange = F, autorange="reversed",
                          showgrid = TRUE,
                          zeroline = FALSE,
                          showline = FALSE),
             yaxis = list(range = c(0, 250),
                          showgrid = TRUE,
                          zeroline = FALSE,
                          showline = TRUE,
                          showticklabels = FALSE))
    
  })
  
  
  # output$spectra_text <- renderUI({
  #   HTML(paste("As you have eaten"))
  #   
  #   
  #     fruits[input$index] <- paste("<b>",fruits[input$index],"</b>")
  #     HTML(paste(fruits))
  #   })
  
  
  # once ui selections are made, go to the tab with spectra and update spectra
  # observeEvent(input$go, {
  #   updateTabItems(session = session, inputId = "tabs", selected = "spectra")
  # })
  
  
  # jump to another menu tab, based on second action button
  # observeEvent(input$go2learn, {
  #   updateTabItems(session = session, inputId = "tabs", selected = "learn")
  # })
  
  output$image1<-renderText({c('<img src="',"http://www.imperial.ac.uk/ImageCropToolT4/imageTool/uploaded-images/CSM_Web_002--tojpeg_1490287529495_x2.jpg",'">')})
}


# UI ---------------------------------------------------------------
# Define UI for application that draws spectra

body <-  dashboardBody(
  
  tabItems(
    
    tabItem(tabName = "about",
            
            box(
              includeMarkdown("./www/about.md"),
              width = "50%", align = "justify"
            ),
            
            # import image through url using output$image1
            box(htmlOutput("image1", width = "50%", height = "50%"),
                width = "50%", align = "center"
            )
            # import saved png image (jpg not supported)
            # box(img(src = "NMR.png", width = "190.8173", height = "315", align = "center"),
            #     width = 4)
            
    ),
    
    tabItem(tabName = "describe",
            
            fluidRow(
              
              column(width = 8 , 
                     box(
                       # title = "Describe your meals in the last 24 hours",
                       title = tagList(shiny::icon("check"), "Describe your meals in the last 24 hours"),
                       solidHeader = TRUE,
                       status = "primary",
                       width = NULL,
                       selectInput(inputId = "hippurate",
                                   label = "How many fruits or vegetables did you have?",
                                   c("None, I am a meat-lover" = 0, "Plenty, I live on fruits and vegetables!" = 1, "A single fruit or a small salad" = 2),
                                   selected = 2
                       ),
                       selectInput(inputId = "tartrate",
                                   label = "Did you have any fresh grapes?",
                                   c("None" = 0, "More than a handful, grapes are my favourite snack" = 1, "Just a few, I went to a conference buffet lunch" = 2),
                                   selected = 2
                       ),
                       selectInput(inputId = "carnitine",
                                   label = "Did you eat any meat?",
                                   c("None, I am a vegetarian" = 0, "A juicy burger for lunch and fried chichen for dinner" = 1, "A healthy portion of lean meat" = 2),
                                   selected = 2
                       ),
                       selectInput(inputId = "tmao",
                                   label = "Did you have any fish or seafood?",
                                   c("None" = 0, "A posh platter of seafood" = 1, "Some fish sauce definitely got into my plate" = 2),
                                   selected = 2
                       ),
                       # selectInput(inputId = "input5",
                       #             label = "Another question?",
                       #             c("None" = 0, "A lot" = 1, "A bit" = 2),
                       #             selected = 2
                       #             ),
                       # selectInput(inputId = "input6",
                       #             label = "Another question?",
                       #             c("None" = 0, "A lot" = 1, "A bit" = 2),
                       #             selected = 2
                       #             ),
                       actionButton(inputId = "go",
                                    label = "See spectra!",
                                    style="color: #fff;
                                    #background-color: #337ab7;
                                    background-color: #31212d;
                                    #border-color: #2e6da4;
                                    border-color: #222d32;"
                       )
                     )
              )
            ),
            
            fluidRow(
              tabBox(width = 8,
                     title = tagList(shiny::icon("binoculars"), "Check your spectrum"),
                     id = "tabset1",
                     side = "right",
                     tabPanel(title = "Spectrum",
                              "Your dietary habits, even those forbidden snacks, can be identified by running your urine sample through an NMR machine.
                              Here is how your urine would likely to look, based on your selected inputs.",
                              plotOutput("spectra")
                     ),
                     tabPanel(title = "Peaks",
                              "Each peak in your urine NMR spectra represents a specific food item.",
                              # includeMarkdown("./www/learn.md")
                              plotlyOutput("spectra2")
                              
                     )
              )
              )
            
            
  ),
  
  
  
  
  tabItem(tabName = "more",
          box(
            includeMarkdown("./www/more.md"),
            width = "100%", align = "justify"
          )
  )
  ),
  
  # define how footer looks like
  tags$footer(
    tags$a(href="https://github.com/lauzikaite", "Code available @GitHub"),
    style = "color: white;
    position:absolute;
    align: right;
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
                
                menuItem("About", tabName = "about", icon = icon("book")),
                
                menuItem("Check your diet", tabName = "describe", icon = icon("check")),
                
                menuItem("More?", tabName = "more", icon = icon("question-circle"))
    )
  ),
  
  body
  
)

# Create Shiny app ----
shinyApp(ui, server)