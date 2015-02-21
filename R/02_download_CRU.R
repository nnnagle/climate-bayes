library(R.utils)
library(tidyr)
library(dplyr)

if(!file.exists('~/Dropbox/git_root/climate-bayes/data/crutm/anomalies/CRUTM.anomalies.txt')){
  dir.create("~/Dropbox/git_root/climate-bayes/data/crutm")
  dir.create("~/Dropbox/git_root/climate-bayes/data/crutm/anomalies")
  dir.create("~/Dropbox/git_root/climate-bayes/data/crutm/nobs")
  
  
  download.file(url='http://www.metoffice.gov.uk/hadobs/crutem4/data/gridded_fields/CRUTEM.4.3.0.0.anomalies.txt.gz',
                destfile="~/Dropbox/git_root/climate-bayes/data/CRUTM.anomalies.txt.gz")
  gunzip("~/Dropbox/git_root/climate-bayes/data/CRUTM.anomalies.txt.gz",
         destname="~/Dropbox/git_root/climate-bayes/data/crutm/anomalies/CRUTM.anomalies.txt")
}

# Download the station data here as described here: http://www.metoffice.gov.uk/hadobs/crutem4/data/station_file_format.txt
if(!file.exists('~/Dropbox/git_root/climate-bayes/data/crutm/anomalies/station_files/CRUTEM.4.3.0.0.station_files.zip')){
  dir.create("~/Dropbox/git_root/climate-bayes/data/crutm/anomalies/station_files")  
  download.file(url="http://www.metoffice.gov.uk/hadobs/crutem4/data/station_files/CRUTEM.4.3.0.0.station_files.zip",
                 destfile="~/Dropbox/git_root/climate-bayes/data/crutm/anomalies/station_files/CRUTEM.4.3.0.0.station_files.zip")
  unzip("~/Dropbox/git_root/climate-bayes/data/crutm/anomalies/station_files/CRUTEM.4.3.0.0.station_files.zip",
        exdir="~/Dropbox/git_root/climate-bayes/data/crutm/anomalies/station_files")
  
}
   

if(!file.exists("~/Dropbox/git_root/climate-bayes/data/crutm/nobs/CRUTM.nobs.txt")){
  download.file(url='http://www.metoffice.gov.uk/hadobs/crutem4/data/gridded_fields/CRUTEM.4.3.0.0.nobs.txt.gz',
                destfile="~/Dropbox/git_root/climate-bayes/data/CRUTEM.4.3.0.0.nobs.txt.gz")
  gunzip("~/Dropbox/git_root/climate-bayes/data/CRUTEM.4.3.0.0.nobs.txt.gz",
         destname="~/Dropbox/git_root/climate-bayes/data/crutm/nobs/CRUTM.nobs.txt")
}
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
dimnames(anomalies) <- list(time=paste(rep(seq(1850, length=nyears), each=12), 
                                       rep(sprintf('%02i', seq(1,12)), time=nyears), '01', sep='-'),
                            lat=lat, lon=lon)
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
dimnames(nobs) <- list(time=paste(rep(seq(1850, length=nyears), each=12), 
                                  rep(sprintf('%02i', seq(1,12)), time=nyears), '01', sep='-'),
                       lat=lat, lon=lon)
rm(nobs.txt)



####################################################################################
# Process individual station data
months <- c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec')
colnames <- c('year', months, paste('source',months,sep='_'))
files <- list.files('~/Dropbox/git_root/climate-bayes/data/crutm/anomalies/station_files/CRUTEM.4.3.0.0.station_files/',
                    recursive=TRUE, full.names=TRUE)
# Remove the Index file:
files <- files[!grepl('Index',files)]
# Create the file reader function
#   the row before the data begins is Obs:
file.reader <- function(x,colnames){
  f <- file(x, encoding='Latin1')
  on.exit(close(f))
  lines <- readLines(f)
  skip <- which(grepl('Obs:', lines))
  df <- data.frame(station=as.character(basename(x)),read.table(x, skip=skip), stringsAsFactors = FALSE)
  names(df) <- c('station', colnames)
  #close(f)
  return(df)
}
# load the data (columns are station, year, jan, feb... dec)
# NOTE:: This throws a warning.  It is be because of an encoding with station names.
#   It doesn't seem to affect the result
# Note2: I think I fixed it by adding a file connection with encoding
stations <- lapply(files, function(x) file.reader(x, colnames)[,c('station', 'year', months)])

# tidy the data
stations <- lapply(stations, function(x) gather(x, key=month, value=sat, -year, -station))

stations.df <- bind_rows(stations) %>% arrange(station, year, month) %>% filter(sat!=-99)
summary(stations.df)

# Read in the Index file
widths=c(6, 23, 15, 7, 7, 6,5,5)
Index.file <- list.files('~/Dropbox/git_root/climate-bayes/data/crutm/anomalies/station_files/CRUTEM.4.3.0.0.station_files/',
           recursive=TRUE, full.names=TRUE, pattern='Index')
inst.meta <- read.fwf(file(Index.file, encoding='Latin1'), widths, comment.char='@',
                      stringsAsFactors=FALSE)
names(inst.meta) <- c('station','name','country','lat','lon','elev','start','last')

save(nobs, anomalies, inst.meta, stations.df, file="/Users/nnagle/Dropbox/git_root/climate-bayes/data/CRU.Rdata")
rm(list=ls())

