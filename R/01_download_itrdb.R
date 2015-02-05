# download the tree ring data and process
library("dplR")
library('dplyr')
dir.create("~/Dropbox/git_root/climate-bayes/data/itrdb")
download.file(url="http://www1.ncdc.noaa.gov/pub/data/paleo/treering/chronologies/itrdb-v705-usa-crn.zip",
              destfile="~/Dropbox/git_root/climate-bayes/data/itrdb-usa.zip")
unzip("~/Dropbox/git_root/climate-bayes/data/itrdb-usa.zip", 
      exdir="~/Dropbox/git_root/climate-bayes/data/itrdb")

download.file(url="http://www1.ncdc.noaa.gov/pub/data/paleo/treering/chronologies/itrdb-v705-canada-crn.zip",
              destfile="~/Dropbox/git_root/climate-bayes/data/itrdb-canada.zip")
unzip("~/Dropbox/git_root/climate-bayes/data/itrdb-canada.zip", 
      exdir="~/Dropbox/git_root/climate-bayes/data/itrdb")

read.crn.head <- function(fname){
  header <- readLines(fname, n=4)
  crn <- try(dplR::read.crn(fname))
  if(class(crn)!='try-error')
    return(data_frame(site_id=substr(header[[1]],1,6),
                      site_name=substr(header[[1]], 10, 61),
                      species_code=substr(header[[1]], 61, 65),
                      lat_lon=substr(header[[2]], 48,57),
                      years=substr(header[[2]], 68, 76),
                      first=row.names(crn)[1],
                      last=tail(row.names(crn),1),
                      fname=fname))
  else return(NULL)
}

# Get list of files in itrdb folder
files <- list.files('~/Dropbox/git_root/climate-bayes/data/itrdb', full.names=TRUE)
# Filter file names to be "standard" chronologies.  No exotics like "latewood", "arstan" etc.
files <- files[grepl('[[:digit:]]\\.crn', files)]
# Feed files through my read.crn.head
temp <- lapply(files,read.crn.head)
# rbind that into a data.frame
temp <- do.call(rbind, temp)
# filter that data.frame to have valid lat-lon strings and valid year ranges
df <- temp %>% filter(grepl(lat_lon, pattern='[[:digit:]]{4,4}-[[:digit:]]{5,5}')) %>% filter(last<2015)
# Create properly formatted lat/lon coordinates (convert from dddmm to decimal)
df <- df %>% mutate(lat=as.numeric(substr(lat_lon, 1, 2))+as.numeric(substr(lat_lon, 3, 4))/60,
                    lon=as.numeric(substr(lat_lon, 6, 8))+as.numeric(substr(lat_lon, 9, 10))/60)

# Plot the lat/lon
df %>% ggplot() + geom_point(aes(x=-lon, y=lat))
which(table(df$species_code)>20)
# Filter database on species with more than 20 series.
df %>% filter( species_code %in% names(table(df$species_code))[table(df$species_code)>20] ) -> df.clean

# Plot the lat/lon again.
temp2 %>% ggplot() + geom_point(aes(x=-lon, y=lat, color=species_code))



