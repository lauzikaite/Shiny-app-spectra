target <- spectra_reactive[, "default"]

# find peak top, arrange all dp orderly by intensity - works only if peak is there!!
pt <- which(target == max(target))
p1 <- target[1:pt]
p2 <- target[(pt+1):length(target)]

p1 <- p1[order(p1)]
p2 <- p2[order(p2, decreasing = T)]
p <- c(p1, p2)

## use ratios for increasing peak height
np1 <- p1

for(x in 2:length(p1)) {
  np1[1] <- p1[1]+1
  np1[x] <- np1[x-1] * (p1[x]/p1[x-1])
  
}

np2 <- p2

for(x in 2:length(p2)) {
  np2[1] <- p2[1]+1.5
  np2[x] <- np2[x-1] * (p2[x]/p2[x-1])
  
}

np <- c(np1, np2)
spectra_reactive$"intensity1" <- np

ggplot() +
  geom_line(data = spectra_reactive, aes(x=-ppm, y=intensity1)) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("ppm") +
  scale_x_continuous(breaks = c(0, -2, -4, -6, -8, -10), labels = c(0, 2, 4, 6, 8, 10)) +
  scale_y_continuous(limits = c(0, 150))



###---use ratios to decrease peak size
pt <- which(target == max(target))
p1 <- target[1:pt]
p2 <- target[(pt+1):length(target)]

p1 <- p1[order(p1)]
p2 <- p2[order(p2, decreasing = T)]
p <- c(p1, p2)

np1 <- p1

for(x in 2:length(p1)) {
  np1[1] <- p1[1]-1
  np1[x] <- np1[x-1] * (p1[x]/p1[x-1])
  
}

np2 <- p2

for(x in 2:length(p2)) {
  np2[1] <- p2[1]-0.5
  np2[x] <- np2[x-1] * (p2[x]/p2[x-1])
  
}

np <- c(np1, np2)

spectra_reactive$"intensity2" <- np





