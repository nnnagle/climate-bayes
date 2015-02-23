rm(list=ls())


library(dplyr)
library(ggplot2)
library(INLA)
library(mapproj)



load('~/Dropbox/git_root/climate-bayes/data/indiv.station.Rdata')
load('~/Dropbox/git_root/climate-bayes/data/itrdb_meta.Rdata')

spatial.coords <- ann.temp %>% distinct(station)

# There are too many stations for the mesher.  
# Let's keep all stations north of 40, and a subset of those south

# Don't try this with more than about sample_n(600)
spatial.subset <- bind_rows(spatial.coords %>% ungroup %>% filter(lat>50),
                        spatial.coords %>% ungroup %>% filter(lat<=50) %>% sample_n(600))
spatial.subset %>% ggplot() + geom_point(aes(x=lon, y=lat))
# convert lat lon to 3d coordinates:
loc.cartesian <- inla.mesh.map(spatial.subset[, c('lon', 'lat')], projection='longlat')
mesh2 = inla.mesh.2d(loc=loc.cartesian, max.edge=c(.06, .3), offset=c(0, -.2), cutoff=.06)
plot(mesh2)
summary(mesh2)

# Reproject the grid points back to lat-lon
mesh.latlon <- inla.mesh.map(mesh2$loc, projection='longlat', inverse=FALSE)
mesh.latlon <- as.data.frame(mesh.latlon)
names(mesh.latlon) <- c('lon', 'lat')


library("PBSmapping")
xlim=-100+c(-180, 180)
xlim=-100+c(-179, 179)
xlim=-100+c(-150, 150)
ylim=c(0,89)
worldmap = map_data("world")
names(worldmap) <- c("X","Y","PID","POS","region","subregion")
worldmap1 = clipLines(worldmap, xlim=xlim,ylim=ylim, keepExtra=TRUE)
worldmap2 = clipLines(worldmap, xlim=xlim+360.1,ylim=ylim, keepExtra=TRUE)
worldmap12 <- bind_rows(worldmap1, worldmap2)

ggplot(mesh.latlon) + geom_point(aes(x=lon, y=lat)) + 
  coord_map(projection = 'mollweide', orientation=c(110,-100,0)) +
  geom_path(data=worldmap1,aes(X,Y,group=PID), fill = NA,color="grey50")+  
  geom_path(data=worldmap2,aes(X,Y,group=PID), fill = NA,color="grey50")

###################################################################
# Create the mesh-to-data basis (interpolation) matrices
A.inst <- spatial.coords %>% ungroup %>% select(lon, lat) %>% as.matrix %>% 
  inla.mesh.map(loc=., projection='longlat') %>% 
  inla.spde.make.A(mesh=mesh2, loc=.)
A.tree <- tree.meta %>% ungroup %>% select(lon, lat) %>% mutate(lon=-lon) %>%
  select(lon, lat) %>% as.matrix %>%
  inla.mesh.map(loc=., projection="longlat") %>%
  inla.spde.make.A(mesh=mesh2, loc=.)
  
save(A.tree, A.inst, mesh2, file='~/Dropbox/git_root/climate-bayes/data/spatial_fields.Rdata')

