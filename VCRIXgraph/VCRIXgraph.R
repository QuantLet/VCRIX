
rm(list = ls(all = TRUE))
options(scipen = 999)


#Load data

load("vcrixindex.RData")


#Install libraries

libraries = ("ggplot2")
lapply(libraries, function(x)
  if (!(x %in% installed.packages())) {
    install.packages(x)
  })
lapply(libraries,
       library,
       quietly = TRUE,
       character.only = TRUE)


#Plot VCRIX

plot(
  vcrix$date,   #x axis
  vcrix$vcrix,  #y axis
  type   = "l",   #type of graph
  col    = "blue", 
  lwd    = 2,      #thickness of the line
  xlab   = "Date",
  ylab   = "VCRIX"
)


#Plot CRIX
plot(
  vcrix$date,
  vcrix$crix,
  type  = "l",
  col   = "black",
  lwd   = 2,
  xlab  = "Date",
  ylab  = "CRIX"
)


# Plot VCRIX and CRIX

par(mar     = c(5, 5, 2, 5))
with(vcrix,
     plot(
       date,
       crix,
       type = "line",
       col  = "black",
       ylab = "CRIX",
       ylim = c(0, 60000),
       xlab = NA
     ))
par(new = T)
with(
  vcrix,
  plot(
    date,
    vcrix,
    type = "line",
    col  = "blue",
    lwd  = 3,
    axes = FALSE,
    xlab = "",
    ylab = "",
    ylim = c(0, 2000)
  )
)
axis(side  = 4)
mtext(side = 4, line = 3, 'VCRIX')
#legend("topleft", legend=c(expression("CRIX"), "VCRIX"), lty=c(1,1), col=c("black", "blue"))


# Plot VCRIX with BTC returns

par(mar  = c(5, 5, 2, 5))
with(
  vcrix,
  plot(
    date,
    btcreturn,
    type = "line",
    col  = "red",
    ylab = "btc returns",
    ylim = c(-0.3, 0.3),#ensuring the proper scale
    xlab = NA
  )
)
par(new = T)
with(
  vcrix,
  plot(
    date,
    vcrix,
    type = "line",
    col  = "blue",
    lwd  = 3,
    axes = FALSE,
    xlab = "",
    ylab = "",
    ylim = c(0, 2000)   #ensuring the proper scale
  )
)

axis(side = 4)
mtext(side = 4, line = 3, 'VCRIX')


# Plot VCRIX with trend line

trend = ggplot(vcrix, aes(x = date, y = vcrix)) +
  geom_line(color    = "blue") + theme_bw() +
  theme(                             #setting up the plot theme
    panel.border     = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line        = element_line(colour = "black")
  )
trend + geom_smooth(
  method = "loess",
  se     = FALSE,
  span   = 0.6,
  col    = "red"
) +
  theme_classic()
