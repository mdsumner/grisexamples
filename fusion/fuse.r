## wtf
#' Combine a list of PolygonDataFrame objects into a single object. 
#' 
#' Objects are combined by modifying sequential IDs to increase. 
c_poly <- function(x) {
  poly <- x[[1]]
  if (length(x) == 1L) {
    warning("input of length one, returning first element")
    return(poly)
  }
  for (i in seq_along(x)[-1]) {
    x0 <- spChFIDs(x[[i]], as.character(seq(nrow(x[[i]])) + max(as.integer(row.names(poly)))))
    poly <- maptools::spRbind(poly, x0)
  }
  poly
}


library(maptools)
data(wrld_simpl)
library(raster)
library(gris)
library(dplyr)
map1 <- subset(wrld_simpl, NAME == "Australia", select = "NAME")
#ras <- raster(map1, nrow = 15, ncol = 25)
ras <- raster(map1, nrow = 65, ncol = 85)
pgrid <- disaggregate(as(ras, "SpatialPolygonsDataFrame"))
pgrid$NAME <- sprintf(sprintf("g%%0%ii", ceiling(log10(ncell(ras) + 1))),  seq(ncell(ras)))
pgrid$layer <- NULL



## something like this
pbind <- c_poly(list(pgrid, map1))
pmesh0 <- RTriangle::triangulate(mkpslg(gris(pbind)))
pmesh <- gris:::as.gris.triangulation(pmesh0, type = "poly")
pmesh$oXt <- pmesh$b %>% transmute(.tr0 = .br0, .ob0 = .ob0)
pmesh$tXv <- with(pmesh, data_frame(.vx1 = bXv$.vx0[seq(1, nrow(bXv), by = 3)], 
                     .vx2 = bXv$.vx0[seq(2, nrow(bXv), by = 3)], 
                     .vx3 = bXv$.vx0[seq(3, nrow(bXv), by = 3)], 
                     .tr0 = oXt$.tr0))


centroids <- gris:::tricentroids(pmesh)

#mLink <- over(SpatialPoints(centroids %>% dplyr::select(x, y) %>% as.matrix, proj4string = CRS(projection(m))), m)
mLink <-extract(as(map1, "SpatialPolygons"), centroids %>% dplyr::select(x, y) %>% as.matrix)$poly.ID
pLink <- extract(as(pbind, "SpatialPolygons"), centroids %>% dplyr::select(x, y) %>% as.matrix)$poly.ID
str(mLink)
str(pLink)

pmesh$o$name <- c("gr", "oz")[is.na(mLink) + 1]
topo <- raster("E:\\DATA\\Etopo\\Etopo1Ice\\Etopo1.tif")
pmesh$v$z <- extract(topo, pmesh$v %>% select(x, y) %>% as.matrix) * 25
gris:::plot3d.gris(pmesh, verts = c("x", "y", "z"), objname = "name")


