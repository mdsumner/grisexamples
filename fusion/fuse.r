library(maptools)
data(wrld_simpl)
library(raster)
library(gris)
m <- subset(wrld_simpl, NAME == "Australia", select = "NAME")
r <- raster(m, nrow = 15, ncol = 25)
p <- as(r, "SpatialPolygonsDataFrame")
p$NAME <- "grid"
p$layer <- NULL
row.names(m)
plot(p)
plot(m, add = TRUE)

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

pp <- c_poly(list(p, m))

## something like this


gg <- gris(pp)
gt <- gris:::triangulate(gg)

gtt <- RTriangle::triangulate(mkpslg(gg))
g2 <- gris:::as.gris.triangulation(gtt, type = "mesh")
g2$oXt <- g2$b %>% transmute(.tr0 = .br0, .ob0 = .ob0)
g2$tXv <- data_frame(.vx1 = g2$bXv$.vx0[seq(1, nrow(g2$bXv), by = 3)], 
                     .vx2 = g2$bXv$.vx0[seq(2, nrow(g2$bXv), by = 3)], 
                     .vx3 = g2$bXv$.vx0[seq(3, nrow(g2$bXv), by = 3)], 
                     .tr0 = g2$oXt$.tr0)

centroids <- gris:::tricentroids(g2)

#mLink <- over(SpatialPoints(centroids %>% dplyr::select(x, y) %>% as.matrix, proj4string = CRS(projection(m))), m)
mLink <- which(!is.na(extract(as(m, "SpatialPolygons"), centroids %>% dplyr::select(x, y) %>% as.matrix)$poly.ID))
pLink <- which(!is.na(extract(as(p, "SpatialPolygons"), centroids %>% dplyr::select(x, y) %>% as.matrix)$poly.ID))
#centroids$tri <- pointInTriangle(g2, centroids %>% dplyr::select(x, y) %>% as.matrix)


# normverts3 <- function(x) {
#   v <-  x$v %>% mutate(newID = as.integer(factor(paste(x, y, sep = ";"))))
# }
# 
# mktripslg <- function(x) {
#   v <- x$v %>% distinct(.vx0) %>% mutate(structuralIndex = row_number())
#   P <- v %>% dplyr::select(x, y) %>% as.matrix
#   tXv <- x$tXv
#   ind <- rbind(tXv %>% dplyr::select(.vx1, .vx2) %>% as.matrix, 
#                tXv %>% dplyr::select(.vx2, .vx3) %>% as.matrix, 
#                tXv %>% dplyr::select(.vx3, .vx1) %>% as.matrix)
#   ind[] <- v$structuralIndex[match(ind, v$.vx0)]
#   RTriangle::pslg(P = P, S = ind)
# }




tps <- mktripslg(gt)

tritip <- RTriangle::triangulate(tps)

gris:::tricentroids()


 pts <-  structure(c(114.005980431693, 114.53341808864, -12.6058410644529, 
                     -11.7544494628906, 0, 0), .Dim = c(2L, 3L), .Dimnames = list(NULL, 
                                                                            c("x", "y", "z")))
 
 triangle <- structure(c(112.951105117798, 112.951105117798, 114.797136917114, 
                         -10.051666259766, -13.031536865234, -13.031536865234, 0, 0, 0), .Dim = c(3L, 
                                                                                         3L), .Dimnames = list(NULL, c("x", "y", "z")))
 
 plot(rbind(pts, triangle)); polypath(triangle); text(pts, lab = 1:2, pos = 3); text(triangle, lab = letters[1:3], pos = 3)
 
mkm <- function(x, n) matrix(rep(x, n), ncol = ncol(x), byrow = TRUE)

SameSide <- function(p1,p2, a,b) {
 cp2 = vcrossp(b-a, p2-a)
 a <- mkm(a, nrow(p1))
 b <- mkm(b, nrow(p1))
 p2 <- mkm(p2, nrow(p1))
 cp1 = vcrossp(b-a, p1- a)
 c(cp1 %*% t(cp2)) >=0 
}

PointInTriangle <- function(p, triangle) {
  a <- triangle[1L,,drop=FALSE]
  b <- triangle[2L,,drop=FALSE]
  c <- triangle[3L,,drop=FALSE]
  SameSide(p,a, b,c) & SameSide(p,b, a,c) & SameSide(p,c, a,b)
}

#PointInTriangle(pts[,,drop=FALSE], triangle)

 
pit <- function(x, xy) {
 tbbox <- tribbox(x)
 df <- NULL
 ##ibox <- vector("list", nrow(tbbox))
 for (i in seq(nrow(tbbox))) {
   testme <- which(xy[,1] >= tbbox$xmin[i] & xy[,1] <= tbbox$xmax[i] &
                xy[,2] >= tbbox$ymin[i] & xy[,2] <= tbbox$ymax[i])
   if (length(testme) > 0) {
   triangle <- x$tXv[i, ] %>% v3ToLong %>% inner_join(x$v, ".vx0") %>% distinct(.vx0) %>% 
         dplyr::transmute(x, y, z = 0) %>% as.matrix
      test <-  PointInTriangle(xy[testme,,drop = FALSE ], triangle)
    #  print(i)
      if (any(test)) df <- bind_rows(df, data_frame(.xy0 = testme[test], .tr0 = rep(x$oXt$.tr0[i], sum(test))))
   }
 }
 df
}

gt$centroids <- tricentroids(gt)
xy <- gt$centroids  %>% transmute(x, y, z = 0)  %>% as.matrix

system.time(res <- pit(gt, xy))
rg <- gris:::grisTri2rgl(gt)

pc <- t(rg$vb[1:2, ])
tc <- t(rg$it)

## abandon this, use geometry::tsearch
library(geometry)
system.time(res2 <- tsearch(pc[,1], pc[,2], tc, xy[,1], xy[,2], bary = FALSE))
