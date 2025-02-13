##loading data
library(readxl)
source('newnewdata/sec-names.R') #sector names for different intervention and control group
source('funcs.R') #functions file

dates <- read_xlsx('startDates.xlsx') #intervention start dates for sectors
listData <- loadRData('list-gai.rdata') #data

controls.identifier <- donor
treat <- dates$Sector_ID[dates$FirstSustainedRelease>2020 ]

ldat <- c(list(listData$gai.wildF))
names(ldat)[1] <- 'gai.wildF' 


library(doParallel)
n <- detectCores()
cl <- makeCluster(n-1)
registerDoParallel(cl, cores = n)


run.model <- function(i){
  treatment.identifier <- treat[i]
  t <- dates$Date[dates$Sector_ID==treat[i]] 
  times.dep  <- cbind("gai.wildF"  = c(1828,t))
  times.pred <- cbind("gai.wildF"  = c(1828,t))
  
  m4 <- mscmt(ldat, treatment.identifier, controls.identifier, times.dep, times.pred,verbose = F)
  
  return(m4)
}

# Set up a scratch directory for your intermediate files
intermediate_directory <- 'RESULTS/M4'

out <- foreach(i=1:97, .packages = c('MSCMT','dplyr')) %dopar% {
  
  # Create a unique filename for each iteration of the parallel loop
  each_filename <- paste0('RESULT_', as.character(i), '.rdata') 
  each_filepath <- file.path(intermediate_directory, each_filename)
  
  # If the file exists, skip to the next iteration
  if (file.exists(each_filepath)) {next}
  
  # Otherwise, run your code
  each_result <- run.model(i=i)
  
  # Save the result individually
  save(each_result, file = each_filepath)
}


stopCluster(cl)



