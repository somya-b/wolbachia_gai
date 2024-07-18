# find non-release adjacent (NA.) sectors
cat <- read.csv('newnewdata/core-buff.csv') # contains category of all sectors
ij <- apply(cat, 2,function(x) {c('NA.') %in% x}) #index of sectors which are ever NA. sectors
na.cat<-cat[,ij] # selecting NA. category sectors

# finding dates date as when the sectors became NA. category
map.t <- read.csv('newnewdata/map-t.csv')
dates <- data.frame(Sector_ID = colnames(na.cat), ey.ew=NA, time=NA, length=NA)

for (i in 1:nrow(dates)) {
  s <- dates$Sector_ID[i]
  j <- min( which(na.cat[,s] == 'NA.') ) # when sector first gets NA. category
  dates$length[i] <- length(which(na.cat[,s] == 'NA.')) #number of weeks for which sector was NA. category
  dates$ey.ew[i] <- map.t$ey.ew[j] 
  dates$time[i] <- map.t$date[j]
}

# removing sectors which are NA. category for less than a month
i <- dates$length < 4
dates <- dates[!i,]

# remove sectors which have dates date before 2020 (same exclusion criteria as actual release sectors)
i <- dates$ey.ew < 2020
dates <- dates[!i,]

treat <- dates$Sector_ID

### the NA cateogry sectors include some release sites
library(MSCMT)
library(readxl)
source('newnewdata/sec-names.R') #sector names for different intervention and control group
source('funcs.R') #functions file

dates <- read_xlsx('startDates.xlsx') #intervention start dates for sectors
listData <- loadRData('list-gai.rdata') #data

subData <- listData[c(5:57)] #extracting covariates data

ldat <- c(list(listData$gai.wildF), subData) #combining gai and covariates 
names(ldat)[1] <- 'gai.wildF'


library(doParallel)
n <- detectCores()
cl <- makeCluster(n-1)
registerDoParallel(cl, cores = n)

run.model <- function(i){
  treatment.identifier <- dates$Sector_ID[i]
  t <- dates$time[i]
  controls.identifier <- donor[!donor %in% treatment.identifier]
  times.dep  <- cbind("gai.wildF"  = c(1828,t))
  times.pred <- cbind(times.dep,times.dep.func2(t))
  agg.fns <- rep("last", ncol(times.pred))
  m2 <- mscmt(ldat, treatment.identifier, controls.identifier, times.dep, times.pred, agg.fns, verbose=F)
  
  return(m2)
}

# Set up a scratch directory for your intermediate files
intermediate_directory <- 'RESULTS/SPILL'

out <- foreach(i=1:length(treat), .packages = c('MSCMT','dplyr','data.table')) %dopar% {
  
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


