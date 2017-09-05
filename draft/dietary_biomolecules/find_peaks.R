find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}

# peaks <- find_peaks(target, m=4)
# plot(target, type="p")
# points(peaks, target[peaks], col="red")

#########################################
find_lowest <- function(x, p, m=2) {
  if(length(p) == 1) {
    print("One peak present in data, bottom are start and end")
    return(0)
    
    } else { 
      
      if(length(p) == 2) {
      print("Two peaks present in data, looking for peak's bottom")
        
      between <- x[p[[1]] : p[[2]]]
      shape <- diff(sign(diff(between, na.pad = FALSE)))
      pks <- sapply(which(shape > 0), FUN = function(i){
        z <- i - 1 - m
        z <- ifelse(z > 0, z, 1)
        w <- i + m + 1
        w <- ifelse(w < length(x), w, length(x))
        if(all(between[c(z : i, (i + 2) : w)] >= between[i + 1])) return(which(x == between[[i+1]])) else return(numeric(0))
      })
      pks <- unlist(pks)
      pks
      } else {
        
        if(length(p) == 3) {
          print("Three peaks present in data, looking for two bottoms")
          
          between <- x[p[[1]] : p[[2]]]
          shape <- diff(sign(diff(between, na.pad = FALSE)))
          pks <- sapply(which(shape > 0), FUN = function(i){
            z <- i - 1 - m
            z <- ifelse(z > 0, z, 1)
            w <- i + m + 1
            w <- ifelse(w < length(x), w, length(x))
            if(all(between[c(z : i, (i + 2) : w)] >= between[i + 1])) return(which(x == between[[i+1]])) else return(numeric(0))
          })
          pks1 <- unlist(pks)
          pks1
          
          between <- x[p[[2]] : p[[3]]]
          shape <- diff(sign(diff(between, na.pad = FALSE)))
          pks <- sapply(which(shape > 0), FUN = function(i){
            z <- i - 1 - m
            z <- ifelse(z > 0, z, 1)
            w <- i + m + 1
            w <- ifelse(w < length(x), w, length(x))
            if(all(between[c(z : i, (i + 2) : w)] >= between[i + 1])) return(which(x == between[[i+1]])) else return(numeric(0))
          })
          pks2 <- unlist(pks)
          pks2
      
      return(c(pks1, pks2))
        }
      }
  }
  }

# bottom <- find_lowest(target, peaks, m=2)
# plot(target, type="l")
# points(bottom, target[bottom], col="red")

#########################################
generate_higher <- function(x, p, b, c) {

    if(length(p) == 1 & b == 0) {
    
    print("One peak present in data, generate new values for it")
    #from start to top
    p1 <- x[1:p[[1]]]
    #from top to end
    p2 <- x[(p[[1]]+1):length(x)]

    p1 <- p1[order(p1)]
    p2 <- p2[order(p2, decreasing = T)]

    np1 <- p1
    for(y in 2:length(p1)) {
      np1[1] <- p1[1]+c
      np1[y] <- np1[y-1] * (p1[y]/p1[y-1]) * 2
    }
    
    np2 <- p2
    for(y in 2:length(p2)) {
      np2[1] <- p1[(length(p1)-1)] 
      np2[y] <- np2[y-1] * (p2[y]/p2[y-1])
    }
  
    plot(x=c(np1, np2), col="red", type="l")
    points(c(p1, p2), type="l")
    return(c(np1, np2))
    
  } else {

  if(length(p) ==2 & length(b) ==1) {
    
      print("Two peaks present in data, generating new values for them")
      #from first to top1
      p1 <- x[1:p[[1]]]
      #from top1 to bottom
      p2 <- x[(p[[1]]+1):b]
      #from bottom to top2
      p3 <- x[(b+1):p[[2]]]
      #from top2 to end
      p4 <- x[(p[[2]]+1):length(x)]
     
      p1 <- p1[order(p1)]
      p2 <- p2[order(p2, decreasing = T)]
      p3 <- p3[order(p3)]
      p4 <- p4[order(p4, decreasing = T)]
      
      # inc <- max(x)/5
      
      np1 <- p1
      for(y in 2:length(p1)) {
        np1[1] <- p1[1]+c
        np1[y] <- np1[y-1] * (p1[y]/p1[y-1])
      }
      
      np2 <- p2
      for(y in 2:length(p2)) {
        np2[1] <- p2[1]+c
        np2[y] <- np2[y-1] * (p2[y]/p2[y-1])
      }
      
      np3 <- p3
      for(y in 2:length(p3)) {
        np3[1] <- p3[1]+c
        np3[y] <- np3[y-1] * (p3[y]/p3[y-1])
      }
      
      np4 <- p4
      for(y in 2:length(p4)) {
        np4[1] <- p4[1]+c
        np4[y] <- np4[y-1] * (p4[y]/p4[y-1])
      }
      
      plot(x=c(np1, np2, np3, np4), col="red", type="l")
      points(c(p1, p2, p3, p4), type="l")
      return(c(np1, np2, np3, np4))
      
  } else {
    
    if(length(p) == 3 & length(b) == 2) {
      
      print("Three peaks present in data, generating new values for them")
      #from first to top1
      p1 <- x[1:p[[1]]]
      #from top1 to bottom
      p2 <- x[(p[[1]]+1):b[[1]]]
      #from bottom to top2
      p3 <- x[(b[[1]]+1):p[[2]]]
      #from top2 to bottom2
      p4 <- x[(p[[2]]+1):b[[2]]]
      #from bottom2 to top3
      p5 <- x[(b[[2]]+1):p[[3]]]
      #from top3 to end
      p6 <- x[(p[[3]]+1):length(x)]
      
      p1 <- p1[order(p1)]
      p2 <- p2[order(p2, decreasing = T)]
      p3 <- p3[order(p3)]
      p4 <- p4[order(p4, decreasing = T)]
      p5 <- p5[order(p5)]
      p6 <- p6[order(p6, decreasing = T)]
      
      # inc <- max(x)/5
      
      np1 <- p1
      for(y in 2:length(p1)) {
        np1[1] <- p1[1]+c
        np1[y] <- np1[y-1] * (p1[y]/p1[y-1])
      }
      
      np2 <- p2
      for(y in 2:length(p2)) {
        np2[1] <- p2[1]+c
        np2[y] <- np2[y-1] * (p2[y]/p2[y-1])
      }
      
      np3 <- p3
      for(y in 2:length(p3)) {
        np3[1] <- p3[1]+c
        np3[y] <- np3[y-1] * (p3[y]/p3[y-1])
      }
      
      np4 <- p4
      for(y in 2:length(p4)) {
        np4[1] <- p4[1]+c
        np4[y] <- np4[y-1] * (p4[y]/p4[y-1])
      }
      
      np5 <- p5
      for(y in 2:length(p5)) {
        np5[1] <- p5[1]+c
        np5[y] <- np5[y-1] * (p5[y]/p5[y-1])
      }

      np6 <- p6
      for(y in 2:length(p6)) {
        np6[1] <- p6[1]+c
        np6[y] <- np6[y-1] * (p6[y]/p6[y-1])
      }
      
      plot(x=c(np1, np2, np3, np4, np5, np6), col="red", type="l")
      points(c(p1, p2, p3, p4, p5, p6), type="l")
      return(c(np1, np2, np3, np4, np5, np6))
    }
  }
  }
  }

# inc <- max(target)
# inc <- max(target)/3
# peaks_h <- generate_higher(target, peaks, bottom, inc)

#########################################
generate_lower <- function(x, p, b) {
  
  if(length(p) == 1 & b == 0) {
  
  print("One peak present in data, generate new low values for it")
  #from start to top
  p1 <- x[1:p[[1]]]
  #from top to end
  p2 <- x[(p[[1]]+1):length(x)]
  
  p1 <- p1[order(p1)]
  p2 <- p2[order(p2, decreasing = T)]
  
  dec <- min(x)
  
  np1 <- p1
  for(y in 2:length(p1)) {
    np1[1] <- p1[1] - dec
    np1[y] <- np1[y-1] * (p1[y]/p1[y-1])
  }
  
  np2 <- p2
  for(y in 2:length(p2)) {
    np2[1] <- p1[(length(p1)-1)] 
    np2[y] <- np2[y-1] * (p2[y]/p2[y-1])
  }
  
  plot(x=c(np1, np2), col="red", type="l")
  points(c(p1, p2), type="l")
  return(c(np1, np2))
  
} else {
  
  if(length(p) ==2 & length(b) ==1) {
    
    print("Two peaks present in data, generating new values for them")
    #from first to top1
    p1 <- x[1:p[[1]]]
    #from top1 to bottom
    p2 <- x[(p[[1]]+1):b]
    #from bottom to top2
    p3 <- x[(b+1):p[[2]]]
    #from top2 to end
    p4 <- x[(p[[2]]+1):length(x)]
    
    p1 <- p1[order(p1)]
    p2 <- p2[order(p2, decreasing = T)]
    p3 <- p3[order(p3)]
    p4 <- p4[order(p4, decreasing = T)]
    
    inc <- max(x)/5
    
    np1 <- p1
    for(y in 2:length(p1)) {
      np1[1] <- p1[1]+inc
      np1[y] <- np1[y-1] * (p1[y]/p1[y-1])
    }
    
    np2 <- p2
    for(y in 2:length(p2)) {
      np2[1] <- p2[1]+inc
      np2[y] <- np2[y-1] * (p2[y]/p2[y-1])
    }
    
    np3 <- p3
    for(y in 2:length(p3)) {
      np3[1] <- p3[1]+inc
      np3[y] <- np3[y-1] * (p3[y]/p3[y-1])
    }
    
    np4 <- p4
    for(y in 2:length(p4)) {
      np4[1] <- p4[1]+inc
      np4[y] <- np4[y-1] * (p4[y]/p4[y-1])
    }
    
    plot(x=c(np1, np2, np3, np4), col="red", type="l")
    points(c(p1, p2, p3, p4), type="l")
    return(c(np1, np2, np3, np4))
    
  } else {
    
    if(length(p) == 3 & length(b) == 2) {
      
      print("Three peaks present in data, generating new values for them")
      #from first to top1
      p1 <- x[1:p[[1]]]
      #from top1 to bottom
      p2 <- x[(p[[1]]+1):b[[1]]]
      #from bottom to top2
      p3 <- x[(b[[1]]+1):p[[2]]]
      #from top2 to bottom2
      p4 <- x[(p[[2]]+1):b[[2]]]
      #from bottom2 to top3
      p5 <- x[(b[[2]]+1):p[[3]]]
      #from top3 to end
      p6 <- x[(p[[3]]+1):length(x)]
      
      p1 <- p1[order(p1)]
      p2 <- p2[order(p2, decreasing = T)]
      p3 <- p3[order(p3)]
      p4 <- p4[order(p4, decreasing = T)]
      p5 <- p5[order(p5)]
      p6 <- p6[order(p6, decreasing = T)]
      
      inc <- max(x)/5
      
      np1 <- p1
      for(y in 2:length(p1)) {
        np1[1] <- p1[1]+inc
        np1[y] <- np1[y-1] * (p1[y]/p1[y-1])
      }
      
      np2 <- p2
      for(y in 2:length(p2)) {
        np2[1] <- p2[1]+inc
        np2[y] <- np2[y-1] * (p2[y]/p2[y-1])
      }
      
      np3 <- p3
      for(y in 2:length(p3)) {
        np3[1] <- p3[1]+inc
        np3[y] <- np3[y-1] * (p3[y]/p3[y-1])
      }
      
      np4 <- p4
      for(y in 2:length(p4)) {
        np4[1] <- p4[1]+inc
        np4[y] <- np4[y-1] * (p4[y]/p4[y-1])
      }
      
      np5 <- p5
      for(y in 2:length(p5)) {
        np5[1] <- p5[1]+inc
        np5[y] <- np5[y-1] * (p5[y]/p5[y-1])
      }
      
      np6 <- p6
      for(y in 2:length(p6)) {
        np6[1] <- p6[1]+inc
        np6[y] <- np6[y-1] * (p6[y]/p6[y-1])
      }
      
      plot(x=c(np1, np2, np3, np4, np5, np6), col="red", type="l")
      points(c(p1, p2, p3, p4, p5, p6), type="l")
      return(c(np1, np2, np3, np4, np5, np6))
    }
  }
}
}

# peak_l <- generate_lower(target, peaks, bottom)

