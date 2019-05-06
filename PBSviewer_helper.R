library("plotly")
library("shiny")
library("formattable")
library("tidyverse")
library("shinydashboard")

jobs_format_memory <- function(x) {
  
}

getJobDetails <- function() {
  message(paste0(Sys.time(), " - Step 1: Get list of jobs"))
  job_list <- read.fwf(
    pipe("ssh -Y ieo4169@hpcfe01.ieo.it /opt/pbs/bin/qstat"),
    skip = 2,
    widths = c(18),
    col.names = c("id"),
    stringsAsFactors = FALSE
  )
  message(paste0(Sys.time(), " - Step 1: Done."))
  
  job_list <- trimws(job_list[,1])
  
  job_details <- data.frame(
    "id" = character(),
    "name" = character(),
    "user" = character(),
    "status" = character(),
    "queue" = character(),
    "server" = character(),
    "date_submitted" = character(),
    "walltime" = character(),
    "cpu_time_used" = character(),
    "cpu_percent" = numeric(),
    "n_cpu_requested" = numeric(),
    "n_cpu_used" = numeric(),
    "mem_requested" = character(),
    "mem_requested_not_formatted" = integer(),
    "mem_used" = character(),
    "vmem_used" = character(),
    "executing_host" = character(),
    stringsAsFactors = FALSE
  )
  
  message(paste0(Sys.time(), " - Step 2: Get details for each job..."))
  temp_data <- parallel::mclapply(
    job_list,
    FUN = function(x) {
      c <- read.fwf(pipe(paste0("ssh -Y ieo4169@hpcfe01.ieo.it /opt/pbs/bin/qstat -f ", x)), skip = 1, widths = c(80), stringsAsFactors = FALSE)
    },
    mc.cores = parallel::detectCores()-1,
    mc.preschedule = TRUE
  )
  message(paste0(Sys.time(), " - Step 2: Done."))
  
  message(paste0(Sys.time(), " - Step 3: Parse details for each job..."))
  for ( i in 1:length(temp_data) ) {
    temp_job_id <- job_list[i]
    message(temp_job_id)
    
    temp_id <- gsub(temp_job_id, pattern = "\\.hpcfe01", replacement = "")
    temp_id <- as.numeric(gsub(temp_id, pattern = "[^0-9.-]", replacement = ""))
    
    data_this_job <- temp_data[[i]]
    temp <- data_this_job[1:33,]
    temp <- trimws(temp)
    temp <- data.frame(do.call("rbind", strsplit(as.character(temp), " = ", fixed = TRUE)), stringsAsFactors = FALSE)
    temp[,1] <- trimws(temp[,1])
    temp[,2] <- trimws(temp[,2])
    
    #
    temp_job_owner <- strsplit(temp[which(temp[,1] == "Job_Owner"), 2], "@", fixed = TRUE)[[1]][1]
    
    #
    temp_server <- gsub(temp[which(temp[,1] == "server"), 2], pattern = ".cluster.loc", replacement = "")
    
    #
    temp_date_submitted <- as.character(strptime(temp[which(temp[,1] == "ctime"), 2], format="%a %B %d %H:%M:%S %Y"))
    
    #
    if ( "resources_used.walltime" %in% temp[,1] ) {
      temp_resources_walltime <- temp[which(temp[,1] == "resources_used.walltime"),2]
    } else {
      temp_resources_walltime <- NA
    }
    
    #
    if ( "resources_used.cput" %in% temp[,1] ) {
      temp_resources_cpu_time_used <- temp[which(temp[,1] == "resources_used.cput"),2]
    } else {
      temp_resources_cpu_time_used <- NA
    }
    
    #
    if ( "resources_used.cpupercent" %in% temp[,1] ) {
      temp_resources_cpu_percent <- as.numeric(temp[which(temp[,1] == "resources_used.cpupercent"),2])
    } else {
      temp_resources_cpu_percent <- NA
    }
    
    #
    if ( "Resource_List.ncpus" %in% temp[,1] ) {
      temp_resources_n_cpus_requested <- as.numeric(temp[which(temp[,1] == "Resource_List.ncpus"),2])
    } else {
      temp_resources_n_cpus_requested <- 0
    }
    
    #
    if ( "resources_used.ncpus" %in% temp[,1] ) {
      temp_resources_n_cpus_used <- as.numeric(temp[which(temp[,1] == "resources_used.ncpus"),2])
    } else {
      temp_resources_n_cpus_used <- 0
    }
    
    #
    if ( "Resource_List.mem" %in% temp[,1] ) {
      x <- temp[which(temp[,1] == "Resource_List.mem"),2]
      y <- x
      if ( grepl(x, pattern = "kb") ) {
        y <- as.numeric(gsub(x, pattern = "kb", replacement = "000"))
        x <- utils:::format.object_size(as.numeric(gsub(x, pattern = "kb", replacement = "000")), "auto")
      } else if ( grepl(x, pattern = "mb") ) {
        y <- as.numeric(gsub(x, pattern = "mb", replacement = "000000"))
        x <- utils:::format.object_size(as.numeric(gsub(x, pattern = "mb", replacement = "000000")), "auto")
      } else if ( grepl(x, pattern = "gb") ) {
        y <- as.numeric(gsub(x, pattern = "gb", replacement = "000000000"))
        x <- utils:::format.object_size(as.numeric(gsub(x, pattern = "gb", replacement = "000000000")), "auto")
      }
      temp_resources_mem_requested_not_formatted <- y
      temp_resources_mem_requested <- x
    } else {
      temp_resources_mem_requested_not_formatted <- 0
      temp_resources_mem_requested <- "0 Mb"
    }
    
    #
    if ( "resources_used.mem" %in% temp[,1] ) {
      x <- temp[which(temp[,1] == "resources_used.mem"),2]
      if ( grepl(x, pattern = "kb") ) {
        y <- as.numeric(gsub(x, pattern = "kb", replacement = "000"))
        x <- utils:::format.object_size(as.numeric(gsub(x, pattern = "kb", replacement = "000")), "auto")
      } else if ( grepl(x, pattern = "mb") ) {
        y <- as.numeric(gsub(x, pattern = "mb", replacement = "000000"))
        x <- utils:::format.object_size(as.numeric(gsub(x, pattern = "mb", replacement = "000000")), "auto")
      } else if ( grepl(x, pattern = "gb") ) {
        y <- as.numeric(gsub(x, pattern = "gb", replacement = "000000000"))
        x <- utils:::format.object_size(as.numeric(gsub(x, pattern = "gb", replacement = "000000000")), "auto")
      }
      temp_resources_mem_used_not_formatted <- y
      temp_resources_mem_used <- x
    } else {
      temp_resources_mem_used_not_formatted <- 0
      temp_resources_mem_used <- "0 Mb"
    }
    
    #
    if ( "resources_used.vmem" %in% temp[,1] ) {
      x <- temp[which(temp[,1] == "resources_used.vmem"),2]
      if ( grepl(x, pattern = "kb") ) {
        y <- as.numeric(gsub(x, pattern = "kb", replacement = "000"))
        x <- utils:::format.object_size(as.numeric(gsub(x, pattern = "kb", replacement = "000")), "auto")
      } else if ( grepl(x, pattern = "mb") ) {
        y <- as.numeric(gsub(x, pattern = "mb", replacement = "000000"))
        x <- utils:::format.object_size(as.numeric(gsub(x, pattern = "mb", replacement = "000000")), "auto")
      } else if ( grepl(x, pattern = "gb") ) {
        y <- as.numeric(gsub(x, pattern = "gb", replacement = "000000000"))
        x <- utils:::format.object_size(as.numeric(gsub(x, pattern = "gb", replacement = "000000000")), "auto")
      }
      temp_resources_vmem_used_not_formatted <- y
      temp_resources_vmem_used <- x
    } else {
      temp_resources_vmem_used_not_formatted <- 0
      temp_resources_vmem_used <- "0 Mb"
    }
    
    #
    if ( "exec_host" %in% temp[,1] ) {
      temp_executing_host <- strsplit(temp[which(temp[,1] == "exec_host"),2], split = "/")[[1]][1]
    } else {
      temp_executing_host <- NA
    }
    
    #
    temp_details <- data.frame(
      "id" = temp_id,
      "name" = temp[which(temp[,1] == "Job_Name"), 2],
      "user" = temp_job_owner,
      "status" = temp[which(temp[,1] == "job_state"), 2],
      "queue" = temp[which(temp[,1] == "queue"), 2],
      "server" = temp_server,
      "date_submitted" = temp_date_submitted,
      "walltime" = temp_resources_walltime,
      "cpu_time_used" = temp_resources_cpu_time_used,
      "cpu_percent" = temp_resources_cpu_percent,
      "n_cpu_requested" = temp_resources_n_cpus_requested,
      "n_cpu_used" = temp_resources_n_cpus_used,
      "mem_requested" = temp_resources_mem_requested,
      "mem_requested_not_formatted" = temp_resources_mem_requested_not_formatted,
      "mem_used" = temp_resources_mem_used,
      "mem_used_not_formatted" = temp_resources_mem_used_not_formatted,
      "vmem_used" = temp_resources_vmem_used,
      "vmem_used_not_formatted" = temp_resources_vmem_used_not_formatted,
      "executing_host" = temp_executing_host,
      stringsAsFactors = FALSE
    )    
    
    #
    job_details <- rbind(job_details, temp_details)
  }
  message(paste0(Sys.time(), " - Step 3: Done."))
  
  for ( i in 1:length(job_details) ) {
    job_details[,i] <- trimws(job_details[,i])
  }
  
  return(job_details)
}

getNodeDetails <- function() {
  node_list <- read.fwf(
    pipe("ssh -Y ieo4169@hpcfe01.ieo.it /opt/pbs/bin/pbsnodes -aSj"),
    skip = 3,
    widths = c(16,16,7,6,7,13,8,8,8,400),
    col.names = c(
      "node","state","n_jobs","jobs_running","jobs_suspended","memory_free",
      "n_cpus_free","n_mics","n_gpus","jobs"
    ),
    stringsAsFactors = FALSE
  )
  
  for ( i in 1:length(node_list) ) {
    node_list[,i] <- trimws(node_list[,i])
  }
  
  node_list <- node_list %>%
    dplyr::mutate(
      n_jobs = as.numeric(n_jobs),
      jobs_running = as.numeric(jobs_running),
      jobs_suspended = as.numeric(jobs_suspended),
      jobs = gsub(jobs, pattern = "\\.hpcfe01", replacement = "")
    )
  
  x <- node_list$n_cpus_free
  n_cpus_scale <- x
  for ( i in 1:length(n_cpus_scale) ) {
    values <- strsplit(x[i], split = "/")[[1]]
    n_cpus_scale[i] <- as.double(values[1]) / as.double(values[2])
  }
  
  node_list$n_cpus_scale <- n_cpus_scale
  
  x <- gsub(node_list$memory_free, pattern = "gb", replacement = "")
  memory_free_scale <- x
  for ( i in 1:length(memory_free_scale) ) {
    values <- strsplit(x[i], split = "/")[[1]]
    memory_free_scale[i] <- as.double(values[1]) / as.double(values[2])
  }
  
  node_list$memory_free_scale <- memory_free_scale
  
  return(node_list)
}

getJobDetails_data <- getJobDetails()
getNodeDetails_data <- getNodeDetails()
