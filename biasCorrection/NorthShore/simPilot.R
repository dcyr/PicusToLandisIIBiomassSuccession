#rm(list =ls())
#wwd <- setwd("C:/Users/Dominic Cyr/Dropbox/NorthShore")
wwd <- getwd()

simInfo <- read.csv("simInfo.csv", colClasses = c(simDir = "character"))
simDir <- simInfo$simDir

require(parallel)
require(doSNOW)
n <- floor(detectCores() * 0.90)

# #######
cl = makeCluster(n, outfile = "") ## 
registerDoSNOW(cl)

foreach(i = 1:20) %dopar% { # length(simDir)
    if (i <= n) {  ### to reduce the probability of several processes
        ### trying to access the same file at the same time
        Sys.sleep(runif(1)*2)
    }
    
    setwd(paste(wwd, simDir[i], sep ="/"))
    shell("landis scenario.txt", wait = T)
}

stopCluster(cl)