
library(sf)
library(spdep)

setwd("/home/champ/Dropbox/1_H5N8_FR2021/3_document/PlosOne/Simulated data")

# Defines the number of levels needed
nblvl <- 4 

# Loads a sample of municipalities (all data presented here were simulated)
com <- st_read("SampleOfMunicipalitiesF.shp", quiet = T)

# Sets the names of variables which will be processed in this script
varnp <- c("var1", "var2")

# Saves the number of municipalities
Nmat <- nrow(com)

# Copies the names of variables (var1 & var2) according to the number of levels selected  
fulllvl <- paste0(rep(varnp, each = nblvl), "_"  , rep(1:nblvl, length(varnp))   )
com[, fulllvl] <- NA

# Creates a neighbors list from a polygon list
col_nb <- poly2nb(com)
W <- nb2mat(col_nb, style = "B") # binary coding, i.e., presence/absence of neighbors

# Copies the neighbors list according to the number of levels selected (one per level)
Wx <- list(W)
for(lvl in 2:nblvl){
  Wx[[lvl]] <- matrix(0, Nmat, Nmat)
}

# Updates the neighbors list 
for(ii in 1:Nmat){  
  
  idlvlX <- list()  
  idlvlX[[1]] <- ii
  idlvlX[[2]] <- which(Wx[[1]][ii, ] > 0) 
  
  for(lvl in 3:(nblvl+1)){
    
    idlvlX[[lvl]] <- lapply(idlvlX[[lvl-1]], function(ww) which(Wx[[1]][ww,] > 0))
    idlvlX[[lvl]] <- unique(unlist(idlvlX[[lvl]]))
    
    match1 <- match(unlist(idlvlX[1:(lvl-1)]), idlvlX[[lvl]])
    
    idlvlX[[lvl]] <- idlvlX[[lvl]][-na.omit(match1)]  
    
    Wx[[lvl-1]][unlist(idlvlX[2:lvl]), ii] <- 1
    Wx[[lvl-1]][ii, unlist(idlvlX[2:lvl])] <- 1
    
    if(length(idlvlX[[lvl]]) == 0) {
      idlvlX[[lvl]] <- 0
    }
  }
}


# Sums the values of var1 & var2 over the targeted municipalities 
# and their surrounding municipalities
# for the 4 sets of neighbor connections.
for(h in 1:nblvl){
  for(ii in 1:Nmat){
    idv <- c(ii, which( Wx[[h]][ii, ] > 0 ))
    datii <- st_drop_geometry(com)[idv, ]
    
    for(v in 1:length(varnp)){
      nvh <- paste0(varnp[v], "_", h)
      com[ii, nvh] <- sum(datii[, varnp[v]])
    }
  }
}

# Checks the results saved in the shapefile "com"
idvar1 <- regexpr("var1", colnames(com))
idvar1 <- which(idvar1 != -1)
pmax <- max(st_drop_geometry(com[idvar1]))

plot(com[idvar1], border = rgb(0, 0, 0, 0), breaks = seq(0, pmax^(1/3), length.out = 20)^3)


idvar2 <- regexpr("var2", colnames(com))
idvar2 <- which(idvar2 != -1)
plot(com[idvar2], border = rgb(0, 0, 0, 0), breaks = seq(0, pmax^(1/3), length.out = 20)^3)
