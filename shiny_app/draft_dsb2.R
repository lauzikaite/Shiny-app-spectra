library(ggplot2)
library(shinythemes)

# Load data ---------------------------------------------------------------
static <- read.table("/Users/el1514/Documents/Scripts/Github-public/ShinyApp-DietarySpectra/game/spectra_static.txt", header = T, stringsAsFactors = F)[,c("ppm","intensity5")]
colnames(static) <- c("ppm", "intensity")

reactive_1 <- read.table('/Users/el1514/Documents/Scripts/Github-public/ShinyApp-DietarySpectra/game/Hippuric_acid_new.txt', header = T, stringsAsFactors = F)
# default - almost none, intensity1 - huge levels, intensity2 - medium levels

reactive_2 <- read.table('/Users/el1514/Documents/Scripts/Github-public/ShinyApp-DietarySpectra/game/Tartaric_acid_new.txt', header = T, stringsAsFactors = F)
# default - almost none, intensity1 - huge levels, intensity2 - medium levels

reactive_3 <- read.table('/Users/el1514/Documents/Scripts/Github-public/ShinyApp-DietarySpectra/game/L-Carnitine_new.txt', header = T, stringsAsFactors = F)
# default - almost none, intensity1 - huge levels, intensity2 - medium levels

reactive_4 <- read.table('/Users/el1514/Documents/Scripts/Github-public/ShinyApp-DietarySpectra/game/TMAO_new.txt', header = T, stringsAsFactors = F)
# default - almost none, intensity1 - huge levels, intensity2 - medium levels

# reduce static spectrum size by removing ppm values in the reactive spectra
reactive_ppm <- data.frame("ppm" = c(reactive_1[,1],reactive_2[,1], reactive_3[,1], reactive_4[,1]))
static_s <- which(static$ppm %in% reactive_ppm$ppm)
static_s <- as.data.frame(static[-static_s,])
static_s <- subset(static_s, ppm>0)

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
      mkdb <- rbind(static_s, reactive_1_s, reactive_2_s, reactive_3_s, reactive_4_s)
      mkdb$type <- c(rep(0, nrow(static_s)), rep(1, nrow(reactive_1_s)), rep(2, nrow(reactive_2_s)), rep(3, nrow(reactive_3_s)), rep(4, nrow(reactive_4_s)))
      mkdb$type <- c(rep("Other", nrow(static_s)), rep("Hippurate", nrow(reactive_1_s)), rep("Tartrate", nrow(reactive_2_s)), rep("Carnitine", nrow(reactive_3_s)), rep("TMAO", nrow(reactive_4_s)))
      
      mkdb <- mkdb[order(mkdb$ppm, decreasing = F),]
    } else {
      reactive_1_s <- data.frame("ppm" = reactive_1[,1], "intensity" = reactive_1[,(2 + as.numeric(input$hippurate))])
      reactive_2_s <- data.frame("ppm" = reactive_2[,1], "intensity" = reactive_2[,(2 + as.numeric(input$tartrate))])
      reactive_3_s <- data.frame("ppm" = reactive_3[,1], "intensity" = reactive_3[,(2 + as.numeric(input$carnitine))])
      reactive_4_s <- data.frame("ppm" = reactive_4[,1], "intensity" = reactive_4[,(2 + as.numeric(input$tmao))])
      mkdb <- rbind(static, reactive_1_s, reactive_2_s, reactive_3_s, reactive_4_s)
    }
  })
  
  
  output$spectra <- renderPlot({
    ggplot() +
      geom_line(data = make_data(), aes(x=-ppm, y=intensity)) +
      geom_line(data = reactive_ppm, )
      theme_bw() +
      theme(axis.text = element_text(size=12),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      xlab("ppm") +
      scale_x_continuous(breaks = c(0, -2, -4, -6, -8, -10), labels = c(0, 2, 4, 6, 8, 10)) +
      scale_y_continuous(limits = c(0, 150))
  })
  
  
  
  
}


# second plot 
ggplot() +
  geom_line(data = mkdb,aes(x=-(ppm), y=intensity, color = factor(type), group =1)) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.position=c(0.5,0.95),
        # legend.justification=c(0.5,1), 
        legend.direction = "horizontal") +
  xlab("ppm") +
  # scale_color_manual(values = c("black","#a6cee3", "#1f78b4","#b2df8a","#33a02c")) +
  scale_color_manual(values = c('black','#7b3294','#c2a5cf','#a6dba0','#008837'),
                     labels = c("Other", "Hippuric acid", "Tartaric acid", "Carnitine", "TMAO")) +
  scale_x_continuous(breaks = c(0, -2, -4, -6, -8, -10), labels = c(0, 2, 4, 6, 8, 10)) +
  scale_y_continuous(limits = c(0, 200))


mkdb$type <- as.factor(mkdb$type)

# this one works but still joins line by groups, rather than as a single line
plot_ly(data = mkdb, x = ~ ppm , y= ~intensity, type = 'scatter', mode = 'lines', color = ~type,
        line = list(width = 0.5),
        colors = c('black','#7b3294','#c2a5cf','#a6dba0','#008837'),
        connectgaps = FALSE,
        stream = list(maxpoints = 10000)) %>%
  layout(legend = list(x = 0.1, y = 0.9, orientation="h")) %>%
  layout(xaxis = list(title = "ppm",
                      range = c(9, 0), autorange = F, autorange="reversed",
                      showgrid = TRUE,
                      zeroline = FALSE,
                      showline = FALSE),
         yaxis = list(range = c(0, 250),
                      showgrid = TRUE,
                      zeroline = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE))




p  <- ggplot() +
  geom_path(data = mkdb,aes(x=-(ppm), y=intensity, color = type, group =1)) 

ggplotly(p)
