method_run <- function(x, y, ix, method, family){
  
  out <- list()
  
  # sprinter
  if("sprinter" %in% method) {
    sprintr <- sprintr.glm::cv.sprinter(x=x, y=y, family = family, cv_step1 = FALSE, square = FALSE)
    out <- append(out, list(sprintr = sprintr))
    cat("sprinter done", fill = TRUE)}
  
  # MEL
  if("MEL" %in% method) {
    MEL <- sprintr.glm::mel(x=x, y=y, family = family)
    out <- append(out, list(MEL = MEL))
    cat("MEL done", fill = TRUE)}
  
  # APL
  if("APL" %in% method) {
    APL <- sprintr.glm::apl(x=x, y=y, family = family)
    out <- append(out, list(APL = APL))
    cat("APL done", fill = TRUE)}
  
  
  # RAMP
  if("RAMP" %in% method) {
    RAMP <- sprintr.glm::ramp.fit(x=x, y=y, hier = "Weak", family = family) 
    out <- append(out, list(RAMP = RAMP))
    cat("RAMP done", fill = TRUE)}
  
  
  # glinternet
  if("glinternet" %in% method) {
    glinternet <- sprintr.glm::cv.glinternet(x=x, y=y, family = family)
    out <- append(out, list(glinternet = glinternet))
    cat("glinternet done",fill = TRUE)}
  
  
  # hierNet
  if("hierNet" %in% method) {
    hierNet <- sprintr.glm::cv.hierNet(x, y, family = family, nfolds = 5, strong = FALSE) # nfolds = 10, strong = TRUE
    out <- append(out, list(hierNet = hierNet))
    cat("hierNet done",fill = TRUE)}
  
  # SIS
  if("SIS" %in% method) {
    SIS <- sprintr.glm::sis_lasso(x=x, y=y, family = family)
    out <- append(out, list(SIS = SIS))
    cat("SIS done", fill = TRUE)}
  
  return(out)
}