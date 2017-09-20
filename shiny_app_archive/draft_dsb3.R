library(ggplot2)
library(plotly)

# Load data ---------------------------------------------------------------
static <- read.table("/Users/el1514/Documents/Scripts/Github-public/ShinyApp-DietarySpectra/shiny_app/Static.txt", header = T, stringsAsFactors = F)

reactive_1 <- read.table('/Users/el1514/Documents/Scripts/Github-public/ShinyApp-DietarySpectra/shiny_app/Hippurate.txt', header = T, stringsAsFactors = F)
bcg_1 <- rbind(data.frame("ppm" = 3.964816,
                                   "default" = 0,
                                   "intensity1" = 0,
                                   "intensity2" = 0,
                                   "peak" = 1
                          ),
               reactive_1,
               data.frame("ppm" = 7.850467,
                          "default" = 0,
                          "intensity1" = 0,
                          "intensity2" = 0,
                          "peak" = 4)
                )
               

reactive_2 <- read.table('/Users/el1514/Documents/Scripts/Github-public/ShinyApp-DietarySpectra/shiny_app/Tartrate.txt', header = T, stringsAsFactors = F)
bcg_2 <- rbind(data.frame("ppm" = 4.332051,
                          "default" = 0,
                          "intensity1" = 0,
                          "intensity2" = 0,
                          "peak" = 1
                  ),
                  reactive_2,
                  data.frame("ppm" = 4.337548,
                             "default" = 0,
                             "intensity1" = 0,
                             "intensity2" = 0,
                             "peak" = 1)
                  )

reactive_3 <- read.table('/Users/el1514/Documents/Scripts/Github-public/ShinyApp-DietarySpectra/shiny_app/Carnitine.txt', header = T, stringsAsFactors = F)
bcg_3 <- rbind(data.frame("ppm" = 3.224849,
                          "default" = 0,
                          "intensity1" = 0,
                          "intensity2" = 0,
                          "peak" = 1),
                reactive_3,
                data.frame("ppm" = 4.562397,
                           "default" = 0,
                           "intensity1" = 0,
                           "intensity2" = 0,
                           "peak" = 4)
                )

reactive_4 <- read.table('/Users/el1514/Documents/Scripts/Github-public/ShinyApp-DietarySpectra/shiny_app/TMAO.txt', header = T, stringsAsFactors = F)
bcg_4 <- rbind(data.frame("ppm" = 3.224849,
                          "default" = 0,
                          "intensity1" = 0,
                          "intensity2" = 0,
                          "peak" = 1),
                reactive_4,
                data.frame("ppm" = 3.235844,
                           "default" = 0,
                           "intensity1" = 0,
                           "intensity2" = 0,
                           "peak" = 1)
                )

# bcg_static <- static[-(which(static$ppm %in% bcg_1$ppm ||static$ppm  %in% bcg_2$ppm || static$ppm  %in% bcg_3$ppm || ))]

# Server ------------------------------------------------------------------
# Define server logic required to draw spectra

reactive_1_s <- data.frame("ppm" = reactive_1[,1], "intensity" = reactive_1[,(4)], "peak" = reactive_1$peak)
reactive_2_s <- data.frame("ppm" = reactive_2[,1], "intensity" = reactive_2[,(4)], "peak" = reactive_2$peak)
reactive_3_s <- data.frame("ppm" = reactive_3[,1], "intensity" = reactive_3[,(4)], "peak" = reactive_3$peak)
reactive_4_s <- data.frame("ppm" = reactive_4[,1], "intensity" = reactive_4[,(4)], "peak" = reactive_4$peak)
mkdb <- data.frame("ppm" = c(static$ppm, reactive_1_s$ppm, reactive_2_s$ppm, reactive_3_s$ppm, reactive_4_s$ppm),
                   "intensity" = c(static$intensity, reactive_1_s$intensity, reactive_2_s$intensity, reactive_3_s$intensity, reactive_4_s$intensity),
                   "type" = c(rep("Other", nrow(static)), rep("Hippurate", nrow(reactive_1_s)), rep("Tartrate", nrow(reactive_2_s)), rep("Carnitine", nrow(reactive_3_s)), rep("TMAO", nrow(reactive_4_s))),
                   "peak" = c(static$peak, reactive_1_s$peak, reactive_2_s$peak, reactive_3_s$peak, reactive_4_s$peak)
)

reactive_1_s <- data.frame("ppm" = bcg_1[,1], "intensity" = bcg_1[,(4)], "peak" = bcg_1$peak)
reactive_2_s <- data.frame("ppm" = bcg_2[,1], "intensity" = bcg_2[,(4)], "peak" = bcg_2$peak)
reactive_3_s <- data.frame("ppm" = bcg_3[,1], "intensity" = bcg_3[,(4)], "peak" = bcg_3$peak)
reactive_4_s <- data.frame("ppm" = bcg_4[,1], "intensity" = bcg_4[,(4)], "peak" = bcg_4$peak)

mkpl <- data.frame("ppm" = c(static$ppm, reactive_1_s$ppm, reactive_2_s$ppm, reactive_3_s$ppm, reactive_4_s$ppm),
                   "intensity" = c(static$intensity, reactive_1_s$intensity, reactive_2_s$intensity, reactive_3_s$intensity, reactive_4_s$intensity),
                   "type" = c(rep("Other", nrow(static)), rep("Hippurate", nrow(reactive_1_s)), rep("Tartrate", nrow(reactive_2_s)), rep("Carnitine", nrow(reactive_3_s)), rep("TMAO", nrow(reactive_4_s))),
                   "peak" = c(static$peak, reactive_1_s$peak, reactive_2_s$peak, reactive_3_s$peak, reactive_4_s$peak)
)

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
                      showline = FALSE,
                      fixedrange = FALSE),
         yaxis = list(range = c(0, 250),
                      showgrid = TRUE,
                      zeroline = FALSE,
                      showline = TRUE,
                      showticklabels = FALSE,
                      fixedrange = FALSE)) 


