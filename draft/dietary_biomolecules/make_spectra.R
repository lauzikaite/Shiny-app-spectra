library(ggplot2)
library(reshape2)

# Load spectra ------------------------------------------------------------
spectra <- read.table("/Users/el1514/Documents/Scripts/Github-all/Github-R/game/apap_spectra.txt", header = T, stringsAsFactors = F)

###---overlay all 12 samples to find the right one
# spectra_m <- melt(spectra, id.vars = "ppm", variable.name = "sample", measure.vars = colnames(spectra)[2:13])
# 
# ggplot(data = spectra_m, aes(x = -ppm, y=value, colour = factor(sample), group =1)) +
#   geom_point(size=0.1) +
#   geom_line(size=0.1) +
#   theme_bw() +
#   theme(axis.text = element_text(size=12),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) 
## intensity5 seem to have most intense peaks

###---extract target peaks from the spectrum
peak <- c(4.336) # Tartaric acid
peak <- c(2.425, 3.215,  3.419, 4.555) # Carnitine
peak <- c(3.25) # TMAO

# TMAO --------------------------------------------------------------------
p1 <- subset(spectra, ppm > 3.225 & ppm < 3.2355, select = c("ppm", "intensity5"))

ggplot(data = p1, aes(x=-ppm, y=intensity5)) +
  geom_point(size=0.1) +
  geom_line(size=0.1) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

peak_list <- list(p1)
## now go to section 'generate'

# Carnitine ---------------------------------------------------------------
p1 <- subset(spectra, ppm > 2.4185 & ppm < 2.4235, select = c("ppm", "intensity5"))
p2 <- subset(spectra, ppm > 3.225 & ppm < 3.235, select = c("ppm", "intensity5"))
p3 <- subset(spectra, ppm > 3.421 & ppm < 3.435, select = c("ppm", "intensity5"))
p4 <- subset(spectra, ppm > 4.558 & ppm < 4.562, select = c("ppm", "intensity5"))

ggplot(data = p4, aes(x=-ppm, y=intensity5)) +
  geom_point(size=0.1) +
  geom_line(size=0.1) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

peak_list <- list(p1, p2, p3, p4)
## now go to section 'generate'

# Tartaric acid -----------------------------------------------------------

p1 <- subset(spectra, ppm > 4.3325 & ppm < 4.3375, select = c("ppm", "intensity5"))

ggplot(data = p1, aes(x=-ppm, y=intensity5)) +
  geom_point(size=0.1) +
  geom_line(size=0.1) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

###---fix and make a proper peak out of a bottom
shape1 <- which(round(p1$ppm, digits = 6)  ==  4.335349)
p1[shape1, "intensity5"] <- p1[(shape1 - 1), "intensity5"] + (abs((p1[( shape1 - 2), "intensity5"] - p1[( shape1 - 1), "intensity5"]))*2)

ggplot(data = p1, aes(x=-ppm, y=intensity5)) +
  geom_point(size=0.1) +
  geom_line(size=0.1) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

peak_list <- list(p1)
## now go to section 'generate'




# Hippurate ---------------------------------------------------------------
peak <- read.table("/Users/el1514/Documents/Scripts/Github-all/Github-R/game/dietary/Hippuric_acid.txt", header = T)$ppm 

p1 <- subset(spectra, ppm > 3.965 & ppm < 3.981, select = c("ppm", "intensity5"))
p2 <- subset(spectra, ppm > 7.54 & ppm < 7.575, select = c("ppm", "intensity5"))
p3 <- subset(spectra, ppm > 7.625 & ppm < 7.65, select = c("ppm", "intensity5"))
p4 <- subset(spectra, ppm > 7.825 & ppm < 7.85, select = c("ppm", "intensity5"))

###---fix obscure shapes - p2
# fix unneccessary peak
shape1 <- which(round(p2$ppm, digits = 6)  ==  7.546454)
shape2 <- which(round(p2$ppm, digits = 6)  ==  7.547004)
p2[shape1, "intensity5"] <- p2[(shape1 - 1), "intensity5"] + ((p2[( shape1 + 2), "intensity5"] - p2[(shape1 - 1), "intensity5"])/2)
p2[shape2, "intensity5"] <- p2[(shape2 - 2), "intensity5"] + ((p2[( shape2 + 2), "intensity5"] - p2[(shape2 - 1), "intensity5"])/2)

# fix unneccessary bottom
shape3 <- which(round(p2$ppm, digits = 6)  ==  7.567345)
p2[shape3, "intensity5"] <- p2[(shape3 - 1), "intensity5"] + ((p2[( shape3 + 1), "intensity5"] - p2[(shape3 - 1), "intensity5"])/2)

ggplot(data = p2, aes(x=-ppm, y=intensity5)) +
  geom_point(size=0.1) +
  geom_line(size=0.1) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

###---fix obscure shapes - p3
shape1 <- which(round(p3$ppm, digits = 6)  ==  7.631666)
shape2 <- which(round(p3$ppm, digits = 6)  ==  7.632216 )

# fix unneccessary bottom
p3[shape1, "intensity5"] <- p3[(shape1 - 1), "intensity5"] + ((p3[( shape1 + 1), "intensity5"] - p3[(shape1 - 1), "intensity5"])/2)
p3[shape2, "intensity5"] <- p3[(shape2 - 1), "intensity5"] + ((p3[( shape2 + 1), "intensity5"] - p3[(shape2 - 1), "intensity5"])/2)

ggplot(data = p3, aes(x=-ppm, y=intensity5)) +
  geom_point(size=0.1) +
  geom_line(size=0.1) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

###---fix obscure shapes - p4
shape1 <- which(round(p4$ppm, digits = 6)  ==  7.831226)
shape2 <- which(round(p4$ppm, digits = 6)  ==  7.831776)
shape3 <- which(round(p4$ppm, digits = 6) == 7.835074)
p4[shape1, "intensity5"] <- p4[(shape1 - 1), "intensity5"] + ((p4[( shape1 + 2), "intensity5"] - p4[(shape1 - 1), "intensity5"])/2)
p4[shape2, "intensity5"] <- p4[(shape2 - 1), "intensity5"] + ((p4[( shape2 + 2), "intensity5"] - p4[(shape2 - 1), "intensity5"])/2)
# fix unneccessary bottom
p4[shape3, "intensity5"] <- p4[(shape3 - 1), "intensity5"] + ((p4[( shape3 + 1), "intensity5"] - p4[(shape3 - 1), "intensity5"])/2)

ggplot(data = p4, aes(x=-ppm, y=intensity5)) +
  geom_point(size=0.1) +
  geom_line(size=0.1) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

peak_list <- list(p1, p2, p3, p4)
## now go to section 'generate'

# Generate ----------------------------------------------------------------
## Generate intensities for each individual sub-peak separately
## Use find_peaks.R script
generate_intensities <- function(fs, pl, increase) {
  
    peaks_new <- lapply(1:length(pl), function(l, increase) {
      spectra_reactive <- data.frame(ppm = pl[[l]]$ppm,
                                     default = pl[[l]]$intensity5,
                                     intensity1 = pl[[l]]$intensity5,
                                     intensity2 = pl[[l]]$intensity5)
      target <- spectra_reactive[, "default"]
      
      if(missing(increase)) {
        inc <- max(target)
      } else {
        inc <- increase
      }
      print(paste("Increase by ",inc))
      inc1 <- inc
      inc2 <- inc/10
      peaks <- find_peaks(target, m = 4)
      bottom <- find_lowest(target, peaks, m = 2)
      peaks_h <- generate_higher(target, peaks, bottom, inc1)
      peaks_h2 <- generate_higher(target, peaks, bottom, inc2)
      return(list(higher = peaks_h, lower = peaks_h2))
      })
    return(peaks_new)
    } 

t <- generate_intensities(spectra, peak_list)  


# Build new spectra -------------------------------------------------------
l=1
spectra_new <- data.frame(ppm = peak_list[[l]]$ppm,
                          default = peak_list[[l]]$intensity5,
                          intensity1 = t[[l]]$higher,
                          intensity2 = t[[l]]$lower)

ggplot(data = spectra_new) +
  geom_line(aes(x=-ppm, y=default), size=0.1, colour = "black") +
  geom_line(aes(x=-ppm, y=intensity1), size=0.1, colour = "red") +
  geom_line(aes(x=-ppm, y=intensity2), size=0.1, colour = "blue") +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(name = "ppm", 
                     breaks = c(2, 0, -4, -6, -8),
                     labels = c(-2,0, 4, 6, 8))

# Build new spectra - multiple sub-peaks ----------------------------------
spectra_new <- 
  lapply(1:length(t), function(l) {
    spectra_new <- data.frame(ppm = peak_list[[l]]$ppm,
                              default = peak_list[[l]]$intensity5,
                              intensity1 = t[[l]]$higher,
                              intensity2 = t[[l]]$lower)
    
    ggplot(data = spectra_new) +
      geom_line(aes(x=-ppm, y=default), size=0.1, colour = "black") +
      geom_line(aes(x=-ppm, y=intensity1), size=0.1, colour = "red") +
      geom_line(aes(x=-ppm, y=intensity2), size=0.1, colour = "blue") +
      theme_bw() +
      theme(axis.text = element_text(size=12),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      scale_x_continuous(name = "ppm", 
                         breaks = c(2, 0, -4, -6, -8),
                         labels = c(-2,0, 4, 6, 8))
    
    return(spectra_new) 
  })

spectra_new <- do.call(rbind, spectra_new)


## write generated new spectra
write.table(spectra_new, "/Users/el1514/Documents/Scripts/Github-all/Github-R/game/dietary/TMAO_new.txt", quote = F, row.names = F, col.names = T)
t <- read.table("/Users/el1514/Documents/Scripts/Github-all/Github-R/game/dietary/TMAO_new.txt", stringsAsFactors = F, header = T)
spectra_new <- data.frame(ppm = t$ppm,
                          default = t$default,
                          intensity1 = t$intensity1,
                          intensity2 = t$intensity2)



# Generate manually -------------------------------------------------------
l = 1
spectra_reactive <- data.frame(ppm = peak_list[[l]]$ppm,
                               default = peak_list[[l]]$intensity5,
                               intensity1 = peak_list[[l]]$intensity5,
                               intensity2 = peak_list[[l]]$intensity5)
target <- spectra_reactive[, "default"]

ggplot(data = spectra_reactive,aes(x=-ppm, y=default)) +
  # geom_point(size=0.1) +
  geom_line(size=0.1) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(name = "ppm", 
                     breaks = c(2, 0, -4, -6, -8),
                     labels = c(-2,0, 4, 6, 8))


# Check UI apraoch --------------------------------------------------------
# ui will be used to select cond from available combinations
cond <- 0 # default
cond <- 1 # cond1
cond<- 2 # cond2


# build reactive spectra 
spectra_r <- spectra_new[,c(1,(2+cond))]
colnames(spectra_r ) <- c("ppm", "intensity")

## build static spectra
omit <- which(spectra$ppm %in% spectra_new$ppm)
spectra_s <- as.data.frame(spectra[-omit,c(1,6)])
colnames(spectra_s) <- c("ppm", "intensity")
data <- rbind(spectra_s, spectra_r) 

ggplot() +
  geom_line(data = data, aes(x=-ppm, y=intensity)) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("ppm") +
  scale_x_continuous(breaks = c(0, -2, -4, -6, -8, -10), labels = c(0, 2, 4, 6, 8, 10)) +
  scale_y_continuous(limits = c(0, 150))



# Check final plot --------------------------------------------------------
## plot default and one new intensity together
## build static spectra
omit <- which(spectra$ppm %in% spectra_new$ppm)
spectra_s <- as.data.frame(spectra[-omit,c(1, 6, 6, 6)])
colnames(spectra_s) <- c("ppm", "default", "intensity1", "intensity2")

data <- rbind(spectra_new, spectra_s)
data$peak <- c(rep(1, nrow(spectra_new)),
               rep(0, nrow(spectra_s)))

ggplot() +
  geom_line(data = data, aes(x=-ppm, y=intensity1, colour = peak, group =1)) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("ppm") +
  scale_x_continuous(breaks = c(0, -2, -4, -6, -8, -10), labels = c(0, 2, 4, 6, 8, 10)) +
  scale_y_continuous(limits = c(0, 150))

