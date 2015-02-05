library(R.utils)

dir.create("~/Dropbox/git_root/climate-bayes/data/crutm")
dir.create("~/Dropbox/git_root/climate-bayes/data/crutm/anomalies")
dir.create("~/Dropbox/git_root/climate-bayes/data/crutm/nobs")


download.file(url='http://www.metoffice.gov.uk/hadobs/crutem4/data/gridded_fields/CRUTEM.4.3.0.0.anomalies.txt.gz',
              destfile="~/Dropbox/git_root/climate-bayes/data/CRUTM.anomalies.txt.gz")
gunzip("~/Dropbox/git_root/climate-bayes/data/CRUTM.anomalies.txt.gz",
      destname="~/Dropbox/git_root/climate-bayes/data/crutm/anomalies/CRUTM.anomalies.txt")


download.file(url='http://www.metoffice.gov.uk/hadobs/crutem4/data/gridded_fields/CRUTEM.4.3.0.0.nobs.txt.gz',
              destfile="~/Dropbox/git_root/climate-bayes/data/CRUTEM.4.3.0.0.nobs.txt.gz")
gunzip("~/Dropbox/git_root/climate-bayes/data/CRUTEM.4.3.0.0.nobs.txt.gz",
      destname="~/Dropbox/git_root/climate-bayes/data/crutm/nobs/CRUTM.nobs.txt")

options(warn=2)
anom.txt <- readLines("~/Dropbox/git_root/climate-bayes/data/crutm/anomalies/CRUTM.anomalies.txt")
# Save into a 3D array of lat,lon, time
lat <- seq(87.5, -87.5, by=-5)
lon <- seq(-177.5, 177.5, by=5)
# Find how many time periods:
num.times <- length(anom.txt) / (length(lat)+1)

anomalies <- array(0, dim = c(num.times, length(lat), length(lon)))
for( r in 1:length(anom.txt)){
  if( (r-1) %% 37 != 0 ){
    d2 <- (r-1) %% 37 # lat row
    d1 <- (r-1) %/% 37 + 1 # time period
    anomalies[d1, d2, ] <- as.numeric(substring(anom.txt[[r]], seq(1, 791, 11), seq(11, 792, 11)))
  }else {
    
  }
}
anomalies[anomalies< -1000] <- NA
nyears <- ceiling(num.times / 12)
dimnames(anomalies) <- list(paste(rep(seq(1850, length=nyears), each=12), rep(seq(1,12), time=nyears), '01', sep='-'),
                         lat, lon)
rm(anom.txt)

nobs.txt <- readLines("~/Dropbox/git_root/climate-bayes/data/crutm/nobs/CRUTM.nobs.txt")
nobs <- array(0, dim = c(num.times, length(lat), length(lon)))
for( r in 1:length(nobs.txt)){
  if( (r-1) %% 37 != 0 ){
    d2 <- (r-1) %% 37 # lat row
    d1 <- (r-1) %/% 37 + 1 # time period
    nobs[d1, d2, ] <- as.numeric(substring(nobs.txt[[r]], seq(1, 791, 11), seq(11, 792, 11)))
  }else {
    
  }
}
nobs[nobs< -1000] <- NA
nyears <- ceiling(num.times / 12)
dimnames(nobs) <- list(paste(rep(seq(1850, length=nyears), each=12), rep(seq(1,12), time=nyears), '01', sep='-'),
                            lat, lon)
rm(nobs.txt)

save(nobs, anomalies, file="/Users/nnagle/Dropbox/git_root/climate-bayes/data/CRU.Rdata")
rm(list=ls())
