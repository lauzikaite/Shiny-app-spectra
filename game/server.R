library(ggplot2)
library(shinythemes)

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
