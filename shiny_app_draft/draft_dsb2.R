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
reactive_3 <- reactive_3[order(reactive_3$ppm),]
# default - almost none, intensity1 - huge levels, intensity2 - medium levels

reactive_4 <- read.table('/Users/el1514/Documents/Scripts/Github-public/ShinyApp-DietarySpectra/game/TMAO_new.txt', header = T, stringsAsFactors = F)
# default - almost none, intensity1 - huge levels, intensity2 - medium levels

# reduce static spectrum size by removing ppm values in the reactive spectra
reactive_ppm <- data.frame("ppm" = c(reactive_1[,1],reactive_2[,1], reactive_3[,1], reactive_4[,1]))
static_s <- which(static$ppm %in% reactive_ppm$ppm)
static_s <- as.data.frame(static[-static_s,])
static_s <- subset(static_s, ppm>0)
static_s <- static_s[order(static_s$ppm),]




# Modify data -------------------------------------------------------------
# Find all individual sub-peaks and mark them into sub-peaks groups

### reactive_1
ppm <- reactive_1$ppm
subp <- which(diff(ppm) > 0.00055)
# subpeaks <- c(rep(1, length(1:(subp[1]+1))),
#                rep(2, length((subp[1]+1):(subp[2]+1))),
#                rep(3, length(subp[2]:(subp[3]-1))),
#                rep(4, length(subp[3]:(length(ppm))))
#                 )

subpeaks <- c(rep(1, subp[1]),
               rep(2, length((subp[1]+1):(subp[2]))),
               rep(3, length((subp[2]+1):(subp[3]))),
               rep(4, length((subp[3]+1):(length(ppm))))
                )
reactive_1$peak <- subpeaks   
  
### reactive_2
ppm <- reactive_2$ppm
subp <- which(diff(ppm) > 0.00055)
reactive_2$peak <- rep(1, nrow(reactive_2))

### reactive_3
ppm <- reactive_3$ppm
subp <- which(diff(ppm) > 0.00055)
subpeaks <- c(rep(1, subp[1]),
              rep(2, length((subp[1]+1):(subp[2]))),
              rep(3, length((subp[2]+1):(subp[3]))),
              rep(4, length((subp[3]+1):(length(ppm))))
)
reactive_3$peak <- subpeaks   

### reactive_4
ppm <- reactive_4$ppm
subp <- which(diff(ppm) > 0.00055)
reactive_4$peak <- rep(1, nrow(reactive_4))

### static_s
static_s$peak <- rep(1, nrow(static_s))


### write new txt files for all spectra

write.table(reactive_1, "/Users/el1514/Documents/Scripts/Github-public/ShinyApp-DietarySpectra/shiny_app/Hippurate.txt", quote = F, row.names = F, col.names = T)
write.table(reactive_2, "/Users/el1514/Documents/Scripts/Github-public/ShinyApp-DietarySpectra/shiny_app/Tartrate.txt", quote = F, row.names = F, col.names = T)
write.table(reactive_3, "/Users/el1514/Documents/Scripts/Github-public/ShinyApp-DietarySpectra/shiny_app/Carnitine.txt", quote = F, row.names = F, col.names = T)
write.table(reactive_4, "/Users/el1514/Documents/Scripts/Github-public/ShinyApp-DietarySpectra/shiny_app/TMAO.txt", quote = F, row.names = F, col.names = T)
write.table(static_s, "/Users/el1514/Documents/Scripts/Github-public/ShinyApp-DietarySpectra/shiny_app/Static.txt", quote = F, row.names = F, col.names = T)
test <- read.table("/Users/el1514/Documents/Scripts/Github-public/ShinyApp-DietarySpectra/shiny_app/Static.txt", header = T, stringsAsFactors = F)

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
      # mkdb <- rbind(static_s, reactive_1_s, reactive_2_s, reactive_3_s, reactive_4_s)
      mkdb <- data.frame("ppm" = c(static_s$ppm, reactive_1_s$ppm, reactive_2_s$ppm, reactive_3_s$ppm, reactive_4_s$ppm),
                         "intensity" = c(static_s$intensity, reactive_1_s$intensity, reactive_2_s$intensity, reactive_3_s$intensity, reactive_4_s$intensity),
                         "type" = c(rep("Other", nrow(static_s)), rep("Hippurate", nrow(reactive_1_s)), rep("Tartrate", nrow(reactive_2_s)), rep("Carnitine", nrow(reactive_3_s)), rep("TMAO", nrow(reactive_4_s)))
                         )
      
      # mkdb$type <- c(rep(0, nrow(static_s)), rep(1, nrow(reactive_1_s)), rep(2, nrow(reactive_2_s)), rep(3, nrow(reactive_3_s)), rep(4, nrow(reactive_4_s)))
      # mkdb$type <- c(rep("Other", nrow(static_s)), rep("Hippurate", nrow(reactive_1_s)), rep("Tartrate", nrow(reactive_2_s)), rep("Carnitine", nrow(reactive_3_s)), rep("TMAO", nrow(reactive_4_s)))
      
      # mkdb <- mkdb[order(mkdb$ppm, decreasing = F),]
    } else {
      reactive_1_s <- data.frame("ppm" = reactive_1[,1], "intensity" = reactive_1[,(2 + as.numeric(input$hippurate))])
      reactive_2_s <- data.frame("ppm" = reactive_2[,1], "intensity" = reactive_2[,(2 + as.numeric(input$tartrate))])
      reactive_3_s <- data.frame("ppm" = reactive_3[,1], "intensity" = reactive_3[,(2 + as.numeric(input$carnitine))])
      reactive_4_s <- data.frame("ppm" = reactive_4[,1], "intensity" = reactive_4[,(2 + as.numeric(input$tmao))])
      mkdb <- rbind(static, reactive_1_s, reactive_2_s, reactive_3_s, reactive_4_s)
    }
  })
}

# Drafts ------------------------------------------------------------------

# Drafts - reactive -------------------------------------------------------

reactive_1_s <- data.frame("ppm" = reactive_1[,1], "intensity" = reactive_1[,(4)], "peak" = reactive_1$peak)
reactive_2_s <- data.frame("ppm" = reactive_2[,1], "intensity" = reactive_2[,(4)], "peak" = reactive_2$peak)
reactive_3_s <- data.frame("ppm" = reactive_3[,1], "intensity" = reactive_3[,(4)], "peak" = reactive_3$peak)
reactive_4_s <- data.frame("ppm" = reactive_4[,1], "intensity" = reactive_4[,(4)], "peak" = reactive_4$peak)
mkdb <- data.frame("ppm" = c(static_s$ppm, reactive_1_s$ppm, reactive_2_s$ppm, reactive_3_s$ppm, reactive_4_s$ppm),
                   "intensity" = c(static_s$intensity, reactive_1_s$intensity, reactive_2_s$intensity, reactive_3_s$intensity, reactive_4_s$intensity),
                   "type" = c(rep("Other", nrow(static_s)), rep("Hippurate", nrow(reactive_1_s)), rep("Tartrate", nrow(reactive_2_s)), rep("Carnitine", nrow(reactive_3_s)), rep("TMAO", nrow(reactive_4_s))),
                   "peak" = c(static_s$peak, reactive_1_s$peak, reactive_2_s$peak, reactive_3_s$peak, reactive_4_s$peak)
)
mkdb$peak <- as.factor(mkdb$peak
                       )
# Drafts - ggplot ---------------------------------------------------------
# Carnitine Hippurate Other Tartrate TMAO

# Plots all peaks differently, perfect
ggplot() +
  geom_line(data = mkdb,aes(x=-(ppm), y=intensity, color = factor(type), group =1)) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.position=c(0.5,0.95),
        legend.direction = "horizontal") +
  xlab("ppm") +
  scale_color_manual(values = c('#a6dba0', '#7b3294', 'black','#c2a5cf','#008837'),
                     labels = c("Carnitine", "Hippuric acid", "Other", "Tartaric acid", "TMAO")) +
  scale_x_continuous(breaks = c(0, -2, -4, -6, -8, -10), labels = c(0, 2, 4, 6, 8, 10)) +
  scale_y_continuous(limits = c(0, 200))


# Drafts - plot_ly --------------------------------------------------------
# this one works but still joins line by groups, rather than as a single line
plot_ly(data = mkdb, x = ~ ppm , y= ~intensity, type = 'scatter', color = ~type,
        # mode = 'markers', 
        # marker = list(size = 0.5),
        mode = 'lines',
        line = list(width = 0.3),
        colors = c('#a6dba0', '#7b3294', 'black','#c2a5cf','#008837'),
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

# Plot each peak as a seperate trace, and each sub-peak separetaly, but then peaks are not joined together at the ends
plot_ly(data = subset(mkdb, type == "Other"), x = ~ ppm , y = ~intensity, type = 'scatter', mode = 'lines',
        line = list(width = 0.8, color = "black"),
        name = "Other",
        showlegend = TRUE) %>%
  add_lines(data = subset(mkdb, type == "Carnitine" & peak == 1), y = ~intensity, type = 'scatter', mode = 'lines', name = "Carnitine", line = list(width = 0.8, color = '#7b3294')) %>%
  add_lines(data = subset(mkdb, type == "Carnitine" & peak == 2), y = ~intensity, type = 'scatter', mode = 'lines', name = "Carnitine", line = list(width = 0.8, color = '#7b3294'), showlegend = FALSE) %>%
  add_lines(data = subset(mkdb, type == "Carnitine" & peak == 3), y = ~intensity, type = 'scatter', mode = 'lines', name = "Carnitine", line = list(width = 0.8, color = '#7b3294'), showlegend = FALSE) %>%
  add_lines(data = subset(mkdb, type == "Carnitine" & peak == 4), y = ~intensity, type = 'scatter', mode = 'lines', name = "Carnitine", line = list(width = 0.8, color = '#7b3294'), showlegend = FALSE) %>%
  add_lines(data = subset(mkdb, type == "Hippurate" & peak == 1), y = ~intensity, type = 'scatter', mode = 'lines', name = "Hippurate", line = list(width = 0.8, color = '#a6dba0')) %>%
  add_lines(data = subset(mkdb, type == "Hippurate" & peak == 2), y = ~intensity, type = 'scatter', mode = 'lines', name = "Hippurate", line = list(width = 0.8, color = '#a6dba0'), showlegend = FALSE) %>%
  add_lines(data = subset(mkdb, type == "Hippurate" & peak == 3), y = ~intensity, type = 'scatter', mode = 'lines', name = "Hippurate", line = list(width = 0.8, color = '#a6dba0'), showlegend = FALSE) %>%
  add_lines(data = subset(mkdb, type == "Hippurate" & peak == 4), y = ~intensity, type = 'scatter', mode = 'lines', name = "Hippurate", line = list(width = 0.8, color = '#a6dba0'), showlegend = FALSE) %>%
  add_lines(data = subset(mkdb, type == "Tartrate" & peak == 1), y = ~intensity, type = 'scatter', mode = 'lines', name = "Tartrate", line = list(width = 0.8, color = '#c2a5cf')) %>%
  add_lines(data = subset(mkdb, type == "TMAO" & peak == 1), y = ~intensity, type = 'scatter', mode = 'lines', name = "TMAO", line = list(width = 0.8, color = '#008837')) %>%
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


