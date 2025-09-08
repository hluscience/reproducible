
# Load implementation of other methods
source("../../other_methods/ramp.fit.R")
source("../../other_methods/cv.hierNet.R")
source("../../other_methods/cv.glinternet.R")
source("../../other_methods/hierFabs.fit.R")
library(sprintr)

# Function to run methods
method_run <- function(x, y, method, family){

  out <- list()

  # glinternet
  if("glinternet" %in% method) {
    glinternet <- cv.glinternet(x=x, y=y, family = family)
    out <- append(out, list(glinternet = glinternet))
    cat("glinternet Done",fill = TRUE)}

  # sprinter
  if("sprinter" %in% method) {
    sprintr <- sprintr::cv.sprinter(x=x, y=y, family = family, cv_step1 = TRUE, square = FALSE, nlam1 = 100)
    out <- append(out, list(sprintr = sprintr))
    cat("sprinter Done", fill = TRUE)}

  # sprinter(square=TRUE)
  if("sprinterT" %in% method) {
    sprintrT <- sprintr::cv.sprinter(x=x, y=y, family = family, cv_step1 = TRUE, square = TRUE, nlam1 = 100)
    out <- append(out, list(sprintrT = sprintrT))
    cat("sprinter(square=TRUE) Done", fill = TRUE)}

  # sprinter(2cv)
  if("sprinter2" %in% method) {
    sprintr2 <- sprintr::cv.sprinter(x=x, y=y, family = family, cv_step1 = FALSE, square = FALSE)
    out <- append(out, list(sprintr2 = sprintr2))
    cat("sprinter(2cv) Done", fill = TRUE)}

  # MEL
  if("MEL" %in% method) {
    MEL <- sprintr::mel(x=x, y=y, family = family)
    out <- append(out, list(MEL = MEL))
    cat("MEL Done", fill = TRUE)}

  # APL
  if("APL" %in% method) {
    APL <- sprintr::apl(x=x, y=y, family = family)
    out <- append(out, list(APL = APL))
    cat("APL Done", fill = TRUE)}

  # SIS
  if("SIS" %in% method) {
    SIS <- sprintr::sis_lasso(x=x, y=y, family = family)
    out <- append(out, list(SIS = SIS))
    cat("SIS Done", fill = TRUE)}

  # RAMP
  if("RAMP" %in% method) {
    RAMP <- ramp.fit(x=x, y=y, hier = "Weak", family = family)
    out <- append(out, list(RAMP = RAMP))
    cat("RAMP Done", fill = TRUE)}

  # hierFabs
  if("hierFabs" %in% method) {
    hierFabs <- hierFabs.fit(x=x, y=y, family = family)
    out <- append(out, list(hierFabs = hierFabs))
    cat("hierFabs Done", fill = TRUE)}

  # hierNet
  if("hierNet" %in% method) {
    hierNet <- cv.hierNet(x, y, family = family, nfolds = 5, strong = FALSE)
    out <- append(out, list(hierNet = hierNet))
    cat("hierNet Done",fill = TRUE)}

  return(out)
}
