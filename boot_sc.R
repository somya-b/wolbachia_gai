##loading data
library(readxl)
library(data.table)
source('newnewdata/sec-names.R') #sector names for different intervention and control group
source('funcs.R') #functions file

dates <- read_xlsx('startDates.xlsx') #intervention start dates for sectors
listData <- loadRData('list-gai.rdata') #data

subData <- listData[c(5:57)] #extracting covariates data

map.t <- read.csv('newnewdata/map-t.csv') #for making week-year time serially for MSCMT function input


controls.identifier <- donor

treat <- dates$Sector_ID[dates$FirstSustainedRelease>2020]

bt.data <- loadRData('newnewdata/bootstrap.rdata') #bootstapped data
bt.data <- lapply(bt.data, function(x){rownames(x)<-map.t$date;x})


library(doParallel)
n <- detectCores()
cl <- makeCluster(n-1)
registerDoParallel(cl, cores = n)

# change agg.fn = 'mean' for model3 and remove for model4
run.model <- function(data,i){
  treatment.identifier <- treat[i]
  t <- dates$Date[dates$Sector_ID==treat[i]]
  times.dep  <- cbind("gai.wildF"  = c(1828,t))
  times.pred <- cbind(times.dep,times.dep.func2(t))
  agg.fns <- rep("last", ncol(times.pred))
  m2 <- mscmt(ldat, treatment.identifier, controls.identifier, times.dep, times.pred, agg.fns, verbose=F)
  
  return(m2)
}


#change M2 to M3/M4 for model3/4
intermediate_directory <- 'BOOT/M2'

out <- foreach(id=rep(1:100, each=97),i=rep(1:97,1:100), .packages = c('MSCMT','dplyr','data.table')) %dopar%{
  
  # Create a unique filename for each iteration of the parallel loop
  each_filename <- paste0('RESULT_',as.character(i), '.rdata')
  each_folder <- paste0('B_',as.character(id))
  each_filepath <- file.path(intermediate_directory,each_folder,each_filename)
  
  # If the file exists, skip to the next iteration
  if (file.exists(each_filepath)) {next}
  
  # Otherwise, run your code
  ldat <- c(list(as.matrix(bt.data[[id]])),subData) 
  names(ldat)[1] <- 'gai.wildF'
  each_result <- run.model(data=ldat, i=i)
  
  # Save the result individually
  save(each_result, file = each_filepath)
}


stopCluster(cl)






