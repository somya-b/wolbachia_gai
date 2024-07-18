times.dep.func2 <- function(t){
  start = 1828
  times.pred <- cbind(
    "MaxTemp" = c(start,t),	"MinTemp" = c(start,t),	"MeanTemp" = c(start,t),	"Rain" = c(start,t),	
    "WindSMax" = c(start,t),	"WindSMean" = c(start,t),	"RH" = c(start,t),	"NDVI_A" = c(start,t),	"P300m" = c(start,t),	"P500m" = c(start,t),	"V_Density" = c(start,t),
    "A_HDB_A" = c(start,t),	"a_HDB_P" = c(start,t),	"D_To_Drain" = c(start,t),	"length_D" = c(start,t),	"Grass_P" = c(start,t),	"MVege_P" = c(start,t),
    "d2m_max" = c(start,t),	"d2m_mean" = c(start,t),	"d2m_min" = c(start,t),	"e_max" = c(start,t),	"e_mean" = c(start,t),
    "lsp_max" = c(start,t),	"lsp_mean" = c(start,t),	"mn2t_max" = c(start,t),	"mn2t_mean" = c(start,t),	"mn2t_min" = c(start,t),	"mx2t_max" = c(start,t),
    "mx2t_min" = c(start,t),	"sp_max" = c(start,t),	"sp_mean" = c(start,t),	"sp_min" = c(start,t),	"stl1_max" = c(start,t),	"stl1_mean" = c(start,t),	"stl1_min" = c(start,t),
    "t2m_mean" = c(start,t),	"t2m_min" = c(start,t),	"tp_max" = c(start,t),	"tp_mean" = c(start,t),	"HDB_RU" = c(start,t)
  )
  
  return(times.pred)
}

read_excel_allsheets <- function(filename, tibble = FALSE, col_types=NULL) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_types = col_types))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

getCI <- function(org, boot){

  boot <- lapply(boot, function(x){or = match(colnames(org),colnames(x));x<-x[,or]; x})
  
  row <- nrow(boot[[1]])
  col <- ncol(boot[[1]])
  s <- matrix(nrow=row,ncol=col) 
  for (c in 1:col) {
    for (r in 1:row) {
      # extract 100 bootsratp samples for each sector and eweek and get standard deviation
      s[r,c] <- sd(unlist(rapply(boot, \(x) x[r,c,drop=F], how='list')),na.rm=T)
    }
  }

  root.n <- sqrt(length(boot))
  ub <- org +1.96*(s/root.n)
  lb <- org -1.96*(s/root.n)

  return(list(lb=lb, ub=ub))
}




