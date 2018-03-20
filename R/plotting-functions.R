plot.hys <- function(x,
                     ...) {
  a <- x
    ti <- (1:101) * pi/50
    newX <- a$values["b.x"] * cos(ti) + a$values["cx"]
    newY <- a$values["b.y"] * cos(ti) + a$values["retention"] * 
      sin(ti) + a$values["cy"]
    lines(x = newX, y = newY, ...)
    arrow.ind <- round(seq(from = 2, to = length(newX), length.out = 10))
    arrX <- newX[arrow.ind]
    arrY <- newY[arrow.ind]
    arrX0 <- newX[arrow.ind-1]
    arrY0 <- newY[arrow.ind-1]
    arrows(x0 = arrX0, y0 = arrY0, x1 = arrX, y1 = arrY, lwd = 2, length = 0.05, ...)
}

plot.TxT <- function(Tcan, 
                     Tair, 
                     hour, 
                     group = NULL,
                     DOY = NULL,
                     doy.range = NULL,
                     color.palette = NULL, 
                     cex = 1.2,
                     col = NULL,
                     alpha = 1,
                     col.ref.line = "black",
                     col.fit.line = "blue",
                     col.hysteresis = "red",
                     ...){
  
  # Data management
    if(!is.null(doy.range)){
      DOY.mask <- DOY %in% seq(doy.range[1], doy.range[2], 1)
        Tcan <- Tcan[DOY.mask]
        Tair <- Tair[DOY.mask]
        group <- group[DOY.mask]
        DOY <- DOY[DOY.mask]
    }     
  
  # Handle color palette
    if(is.null(color.palette)){
      clrpal <- colorRampPalette(colors = c("dodgerblue", "green","yellow","darkorange"))(24)
    } else {
      clrpal <- color.palette(24)
    }
    clrpal <- adjustcolor(col = clrpal, alpha.f = alpha)
      
  # Main plot
    plot(Tcan ~ Tair, 
         pch = 21, 
         bg = clrpal[hour+1],
         col = col,
         cex = cex,
         ...)
  
  # 1-1 line
    abline(a = 0, b = 1, col = col.ref.line)
    
  # Fit   
    #summary(lm(PF.summer$TIR.PV ~ PF.summer$X.T_HMP.))
    abline(lm(Tcan ~ Tair), col = col.fit.line)
    
  # Legend
    labs.len <- length(unique(hour))
    leg.labs <- c(min(hour),
                      rep(NA, 5),
                      6, rep(NA, 5),
                      12, rep(NA, 5),
                      18, rep(NA, 4),
                  max(hour))
    usr.range <- par("usr")
    x.range <- usr.range[2] - usr.range[1]
    y.range <- usr.range[4] - usr.range[3] 
      
    color.legend(xl = usr.range[1] + x.range * 0.05, 
                 yb = usr.range[3] + y.range * 0.4, 
                 xr = usr.range[1] + x.range * 0.1, 
                 yt = usr.range[3] + y.range * 0.85, 
                 rect.col = clrpal,
                 legend = leg.labs,
                 gradient = "y",
                 align = "rb")
    text(x = usr.range[1] + x.range * 0.05, y = usr.range[3] + y.range * 0.93, labels = "Hour", adj = 0)
 
  # Stats
    text(x = usr.range[1] + x.range * 0.6, y = usr.range[3] + y.range * 0.05, labels = paste("Slope: ", round(coef(lm(Tcan ~ Tair))[2], 2)), adj = 0)

    hys.fit <- floop(x = Tair, y = Tcan, times = hour, period = 24)
    plot.hys(hys.fit, col = col.hysteresis)
    
}
 









