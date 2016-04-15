library(RCurl)

download.file("https://raw.githubusercontent.com/downloads/ThomasK81/PeRseus/raw/master/phi0448/phi001.rds", 
              destfile = "./temp.rds", 
              method = "curl", 
              extra='-L')
corpus2 <- load("./temp.rds")