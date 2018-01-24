library("reshape2")
library("gdxrrw")

# Setup working environment
setwd("/home/saintlyvi/ckan/FilesFromBryce")
gams_lib_dir <- ("/home/saintlyvi/opt/gams/gams24.7_linux_x64_64_sfx")
igdx(gams_lib_dir)
gdxName <- "tra_results.gdx"

# Obtain gdx file info
info <- gdxInfo(gdxName, dump = FALSE, returnDF = TRUE)
sets <- info$set$name
params <- info$parameters$name

# Combine all sets dataframes and all params dataframes into a list of lists called data
sets_data <- lapply(sets, function(s) rgdx.set(gdxName, s, compress=FALSE, ts=TRUE, useDomInfo = TRUE, check.names = TRUE, te = FALSE))
names(sets_data) <- sets
params_data <- lapply(params, function(p) tryCatch(rgdx.param(gdxName, p, names=NULL, compress=FALSE, ts=FALSE, squeeze=FALSE, useDomInfo = TRUE, check.names = TRUE), error=function(e) rgdx.scalar(gdxName, p, ts=FALSE)))
names(params_data) <- params
data <- list(sets = sets_data, params = params_data) 

#save data as .rds file
saveRDS(data, "tra_results.rds")

