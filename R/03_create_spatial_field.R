#install.packages("INLA", repos="http://www.math.ntnu.no/inla/R/testing")

library(dplyr)
library(ggplot2)
library(INLA)
library(mapproj)

load('~/Dropbox/git_root/climate-bayes/data/CRU.Rdata')
load('~/Dropbox/git_root/climate-bayes/data/itrdb_meta.Rdata')

# Create the spatial grid:
anom.df <- reshape2::melt(anomalies, stringsAsFactors=FALSE, value.name='sas') # surface air temperature
anom.df$time <- as.character(anom.df$time)
nobs.df <- reshape2::melt(nobs, stringsAsFactors=FALSE, value.name='nobs')
nobs.df$time <- as.character(nobs.df$time)

# Extract distinct coords in north america
anom.df %>% select(lat, lon) %>% distinct %>% filter(lat < 70 & lat>30 & lon < -50 & lon>-165) -> spatial.coords

tree.meta$lon2 <- -tree.meta$lon

# Calculate distance from spatial coords to trees
dist.matrix <- fields::rdist.earth(as.matrix(spatial.coords[,c('lon', 'lat')]), as.matrix(tree.meta[,c('lon2', 'lat')]))
min.dist <- apply(dist.matrix, 1, min)

spatial.coords %>% filter(min.dist<300) %>% ggplot() + geom_point(aes(x=lon, y=lat)) + 
  geom_point(data=tree.meta, aes(x=lon2, y=lat, color=species_code)) + coord_map()

spatial.coords <- spatial.coords %>% filter(min.dist<300) %>% arrange(lat, lon)

# Add a spatial_ID
spatial.coords$SID <- seq(1:nrow(spatial.coords))

# Filter the climate data down to these locations
anom.df <- inner_join(anom.df, spatial.coords)
nobs.df <- inner_join(nobs.df, spatial.coords)

# Save the filtered anomaly data:
save(anom.df, nobs.df, file='~/Dropbox/git_root/climate-bayes/data/anomaly.Rdata')


# convert lat lon to 3d coordinates:
loc.cartesian <- inla.mesh.map(spatial.coords[, c('lon', 'lat')], projection='longlat')
mesh2 = inla.mesh.2d(loc=loc.cartesian, max.edge=c(.1, .2), offset=c(-1,.2))
mesh2 = inla.mesh.2d(loc=loc.cartesian, max.edge=c(.05, .3), offset=c(0, -.2), cutoff=.05)
plot(mesh2)
proj2b = inla.mesh.projector(mesh2, projection="mollweide")


# More about the matern here: http://www.math.ntnu.no/inla/r-inla.org/doc/latent/matern2d.pdf
# The following code is from: http://people.bath.ac.uk/fl353/tutorials/oulu/tut_interface.R
spde2 = inla.spde2.matern(mesh2, alpha=2)
Q2 = inla.spde2.precision(spde2, theta=c(0,1))
x2 = inla.qsample(n=1, Q2, seed=1234L)[,1]
proj2a = inla.mesh.projector(mesh2, projection="longlat", dims=c(361,181))
proj2b = inla.mesh.projector(mesh2, projection="longsinlat", dims=c(361,181))
proj2c = inla.mesh.projector(mesh2, projection="mollweide", dims=c(361,181))

# Create the projection matrices
A.inst <- inla.spde.make.A(mesh=mesh2, loc=loc.cartesian)
A.tree <- inla.spde.make.A(mesh=mesh2, 
                           loc=inla.mesh.map(as.matrix(tree.meta[, c('lon2', 'lat')]), 
                                             projection='longlat'))

# save spatial matrices:
save(A.tree, A.inst, spde2, mesh2, spatial.coords, file='~/Dropbox/git_root/climate-bayes/data/spatial_fields.Rdata')
# 
# ###################################################
# ### code chunk number 30: proj2a
# ###################################################
# my.levelplot <-
#   function(proj, values,
#            col.regions=grey.colors(64, 1, 0),
#            aspect="fill",
#            contour=TRUE, labels=FALSE,
#            xlim=range(proj$x), ylim=range(proj$y),
#            ...) {
#     z = inla.mesh.project(proj, values)
#     print(levelplot(
#       row.values=proj$x, column.values=proj$y, x=z,
#       xlim=xlim, ylim=ylim,
#       col.regions=col.regions, aspect=aspect,
#       contour=contour, labels=labels, ...))
#   }
# lattice.to.plot <- function(latt, projection, skip=10) {
#   loc = inla.mesh.map(latt$loc, projection=projection, inverse=FALSE)
#   mat1 = matrix(loc[,1], nrow=latt$dims[1], ncol=latt$dims[2])
#   mat2 = matrix(loc[,2], nrow=latt$dims[1], ncol=latt$dims[2])
#   hskip = seq(1,latt$dims[1],skip)
#   vskip = seq(1,latt$dims[2],skip)
#   return(rbind(
#     cbind(as.vector(rbind(cbind(mat1[,vskip], NA), NA)),
#           as.vector(rbind(cbind(mat2[,vskip], NA), NA))),
#     cbind(as.vector(rbind(cbind(t(mat1[hskip,]), NA), NA)),
#           as.vector(rbind(cbind(t(mat2[hskip,]), NA), NA)))))
# }
# latt=inla.mesh.lattice(x=seq(-180,180,1),
#                        y=seq(-90,90,1)*(1-1e-8), units="longlat")
# 
# my.levelplot(proj2a, x2, at=pretty(x2, 16), aspect=1/2,
#              xlab="Longitude", ylab="Latitude", main="Lon-Lat projection")
# trellis.focus("panel", 1, 1, highlight=FALSE)
# print(llines(lattice.to.plot(latt, "longlat", 30), col=1))
# trellis.unfocus()
# 
# ###################################################
# ### code chunk number 31: proj2b
# ###################################################
# my.levelplot(proj2b, x2, at=pretty(x2, 16), aspect=1/2,
#              xlab="Longitude", ylab="", main="Lon-sin(Lat) projection")
# trellis.focus("panel", 1, 1, highlight=FALSE)
# print(llines(lattice.to.plot(latt, "longsinlat", 30), col=1))
# trellis.unfocus()
# 
# ###################################################
# ### code chunk number 32: proj2c
# ###################################################
# my.levelplot(proj2c, x2, at=pretty(x2, 16), aspect=1/2,
#              xlab="", ylab="", main="Mollweide projection")
# trellis.focus("panel", 1, 1, highlight=FALSE)
# print(llines(lattice.to.plot(latt, "mollweide", 30), col=1))
# trellis.unfocus()
# 
# 
# # 
# # A.precip <- inla.spde.make.A(mesh=mesh2, loc=loc.cartesian)
# # A.tree <- inla.spde.make.A(mesh=mesh2, 
# #                            loc=inla.mesh.map(as.matrix(tree.meta[, c('lon2', 'lat')]), 
# #                                              projection='longlat'))
# # 
# # B <- inla.mesh.basis(mesh2, type='b.spline', n=3)
# # 
# # plot(mesh2, rgl=TRUE, col=B[,3])
# 
# #http://people.bath.ac.uk/fl353/isba/isbaspde.R
# # ##############
# # sigma0 = 1   ## Standard deviation
# # range0 = 0.2 ## Spatial range
# # ## Convert into tau and kappa:
# # kappa0 = sqrt(8)/range0
# # tau0 = 1/(sqrt(4*pi)*kappa0*sigma0)
# # ## Construct spde object and set priors
# # ## that will be used by inla().
# # spde=inla.spde2.matern(mesh2,
# #                        B.tau=cbind(log(tau0),1,0),
# #                        B.kappa=cbind(log(kappa0),0,1),
# #                        theta.prior.mean=c(0,0),
# #                        theta.prior.prec=c(0.1,1))
