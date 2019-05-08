library("plotly")
library("shiny")
library("formattable")
library("tidyverse")
library("shinydashboard")

jobsFormatMemory <- function(x) {
  if ( grepl(x, pattern = "kb") ) {
    y <- as.numeric(gsub(x, pattern = "kb", replacement = "000"))
    x <- utils:::format.object_size(as.numeric(gsub(x, pattern = "kb", replacement = "000")), "auto")
  } else if ( grepl(x, pattern = "mb") ) {
    y <- as.numeric(gsub(x, pattern = "mb", replacement = "000000"))
    x <- utils:::format.object_size(as.numeric(gsub(x, pattern = "mb", replacement = "000000")), "auto")
  } else if ( grepl(x, pattern = "gb") ) {
    y <- as.numeric(gsub(x, pattern = "gb", replacement = "000000000"))
    x <- utils:::format.object_size(as.numeric(gsub(x, pattern = "gb", replacement = "000000000")), "auto")
  } else {
    y <- as.numeric(gsub(x, pattern = "[^0-9.-]", replacement = ""))
    x <- x
  }
  return(list("formatted" = x, "numeric" = y))
}

testConnection <- function(user, host) {
  tryCatch(
    system(paste0("ssh -Y ", user, "@", host, " echo")),
    error = function(err) NULL
  )
}

getNewTimePoint <- function(x) {
  current_time <- as.POSIXct(Sys.time())
  new_table <- data.frame(
    "time" = as.POSIXct(character()),
    "node" = character(),
    "n_cpu" = numeric(),
    "memory" = numeric(),
    stringsAsFactors = FALSE
  )
  for ( i in 1:length(x$node) ) {
    current_node <- x$node[i]
    current_n_cpu <- strsplit(x$n_cpus_free[which(x$node == current_node)], split = "/")[[1]][1]
    current_memory <- gsub(x$memory_free[which(x$node == current_node)], pattern = "gb", replacement = "")
    current_memory <- strsplit(current_memory, split = "/")[[1]][1]
    new_entry <- data.frame(
      "time" = current_time,
      "node" = current_node,
      "n_cpu" = as.numeric(current_n_cpu),
      "memory" = as.numeric(current_memory),
      stringsAsFactors = FALSE
    )
    new_table <- rbind(new_table, new_entry)
  }
  return(new_table)
}

getJobDetails <- function(user, host) {
  message(paste0(Sys.time(), " - Run getJobDetails()"))
  user <- trimws(user)
  host <- trimws(host)
  message(paste0(Sys.time(), " - user: ", user, "; host: ", host))
  #message(paste0(Sys.time(), " - Step 1: Get list of jobs"))
  job_list <- read.fwf(
    pipe(paste0("ssh -Y ", user, "@", host, " /opt/pbs/bin/qstat")),
    widths = c(80),
    stringsAsFactors = FALSE
  )
  row <- grep(job_list[,1], pattern = "--------")
  temp <- gsub(job_list[row,1], pattern = "  ", replacement = "- ")
  temp <- as.vector(gregexpr(temp, pattern = " ")[[1]])
  job_list <- job_list[(row+1):nrow(job_list),]
  job_list <- substr(job_list, 1, temp[1])
  job_list <- trimws(job_list)
  #message(paste0(Sys.time(), " - Step 1: Done."))
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
  #message(paste0(Sys.time(), " - Step 2: Get details for each job..."))
  temp_data <- parallel::mclapply(
    job_list,
    FUN = function(x) {
      c <- tryCatch(
        read.fwf(
          pipe(paste0("ssh -Y ", user, "@", host, " /opt/pbs/bin/qstat -f ", x)),
          skip = 0,
          widths = c(500),
          stringsAsFactors = FALSE
        ),
        error = function(err) NA
      )
    },
    mc.cores = parallel::detectCores()-1,
    mc.preschedule = TRUE
  )
  #message(paste0(Sys.time(), " - Step 2: Done."))
  temp_data <- temp_data[which(!is.na(temp_data))]
  job_names <- rep(NA, length(temp_data))
  for ( i in 1:length(temp_data) ) {
    position_of_variable_List <- grep(temp_data[[i]][,1], pattern = "Variable_List")
    temp_data[[i]] <- temp_data[[i]][1:position_of_variable_List,]
    temp_data[[i]] <- trimws(temp_data[[i]])
    job_names[i] <- gsub(temp_data[[i]][1], pattern = "Job Id: ", replacement = "")
    temp_data[[i]] <- temp_data[[i]][-1]
    temp_data[[i]] <- data.frame(
      do.call(
        "rbind",
        strsplit(
          temp_data[[i]],
          " = ",
          fixed = TRUE
        )
      ),
      stringsAsFactors = FALSE
    )
  }
  names(temp_data) <- job_names
  #message(paste0(Sys.time(), " - Step 3: Parse details for each job..."))
  for ( i in 1:length(temp_data) ) {
    temp_job_id <- names(temp_data)[i]
    #message(temp_job_id)
    temp_id <- gsub(temp_job_id, pattern = "\\.hpcfe01", replacement = "")
    temp_id <- as.numeric(gsub(temp_id, pattern = "[^0-9.-]", replacement = ""))
    data_this_job <- temp_data[[i]]
    #
    temp_job_owner <- strsplit(
      data_this_job[which(data_this_job[,1] == "Job_Owner"),2],
      "@",
      fixed = TRUE
    )[[1]][1]
    #
    temp_server <- gsub(
      data_this_job[which(data_this_job[,1] == "server"),2],
      pattern = ".cluster.loc",
      replacement = ""
    )
    #
    temp_date_submitted <- as.character(
      strptime(
        data_this_job[which(data_this_job[,1] == "ctime"),2],
        format="%a %B %d %H:%M:%S %Y"
      )
    )
    #
    if ( "resources_used.walltime" %in% data_this_job[,1] ) {
      temp_resources_walltime <- data_this_job[which(data_this_job[,1] == "resources_used.walltime"),2]
    } else {
      temp_resources_walltime <- NA
    }
    #
    if ( "resources_used.cput" %in% data_this_job[,1] ) {
      temp_resources_cpu_time_used <- data_this_job[which(data_this_job[,1] == "resources_used.cput"),2]
    } else {
      temp_resources_cpu_time_used <- NA
    }
    #
    if ( "resources_used.cpupercent" %in% data_this_job[,1] ) {
      temp_resources_cpu_percent <- as.numeric(data_this_job[which(data_this_job[,1] == "resources_used.cpupercent"),2])
    } else {
      temp_resources_cpu_percent <- NA
    }
    #
    if ( "Resource_List.ncpus" %in% data_this_job[,1] ) {
      temp_resources_n_cpus_requested <- as.numeric(data_this_job[which(data_this_job[,1] == "Resource_List.ncpus"),2])
    } else {
      temp_resources_n_cpus_requested <- 0
    }
    #
    if ( "resources_used.ncpus" %in% data_this_job[,1] ) {
      temp_resources_n_cpus_used <- as.numeric(data_this_job[which(data_this_job[,1] == "resources_used.ncpus"),2])
    } else {
      temp_resources_n_cpus_used <- 0
    }
    #
    if ( "Resource_List.mem" %in% data_this_job[,1] ) {
      x <- data_this_job[which(data_this_job[,1] == "Resource_List.mem"),2]
      x <- jobsFormatMemory(x)
      temp_resources_mem_requested_not_formatted <- x[["numeric"]]
      temp_resources_mem_requested <- x[["formatted"]]
    } else {
      temp_resources_mem_requested_not_formatted <- 0
      temp_resources_mem_requested <- "0 Mb"
    }
    #
    if ( "resources_used.mem" %in% data_this_job[,1] ) {
      x <- data_this_job[which(data_this_job[,1] == "resources_used.mem"),2]
      x <- jobsFormatMemory(x)
      temp_resources_mem_used_not_formatted <- x[["numeric"]]
      temp_resources_mem_used <- x[["formatted"]]
    } else {
      temp_resources_mem_used_not_formatted <- 0
      temp_resources_mem_used <- "0 Mb"
    }
    #
    if ( "resources_used.vmem" %in% data_this_job[,1] ) {
      x <- data_this_job[which(data_this_job[,1] == "resources_used.vmem"),2]
      x <- jobsFormatMemory(x)
      temp_resources_vmem_used_not_formatted <- x[["numeric"]]
      temp_resources_vmem_used <- x[["formatted"]]
    } else {
      temp_resources_vmem_used_not_formatted <- 0
      temp_resources_vmem_used <- "0 Mb"
    }
    #
    if ( "exec_host" %in% data_this_job[,1] ) {
      temp_executing_host <- strsplit(data_this_job[which(data_this_job[,1] == "exec_host"),2], split = "/")[[1]][1]
    } else {
      temp_executing_host <- NA
    }
    #
    temp_details <- data.frame(
      "id" = temp_id,
      "name" = data_this_job[which(data_this_job[,1] == "Job_Name"), 2],
      "user" = temp_job_owner,
      "status" = data_this_job[which(data_this_job[,1] == "job_state"), 2],
      "queue" = data_this_job[which(data_this_job[,1] == "queue"), 2],
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
  #message(paste0(Sys.time(), " - Step 3: Done."))
  for ( i in 1:length(job_details) ) {
    job_details[,i] <- trimws(job_details[,i])
  }
  message(paste0(Sys.time(), " - getJobDetails() finished."))
  return(job_details)
}

getNodeDetails <- function(user, host) {
  
  message(paste0(Sys.time(), " - Run getNodeDetails()"))
  
  user <- trimws(user)
  host <- trimws(host)
  
  message(paste0(Sys.time(), " - user: ", user, "; host: ", host))

  node_list <- read.fwf(
    pipe(paste0("ssh -Y ", user, "@", host, " /opt/pbs/bin/pbsnodes -aSj")),
    widths = c(500),
    stringsAsFactors = FALSE
  )
  
  row <- grep(node_list[,1], pattern = "--------")
  temp <- gsub(node_list[row,1], pattern = "  ", replacement = "- ")
  temp <- as.vector(gregexpr(temp, pattern = " ")[[1]])
  
  widths <- rep(NA, length(temp)+1)
  for ( i in 1:length(widths) ) {
    if ( i == 1 ) {
      widths[i] <- temp[i]
    } else if ( i < length(widths) ) {
      widths[i] <- temp[i] - temp[i-1]
    } else {
      widths[i] <- 400
    }
  }

  node_list <- read.fwf(
    pipe(paste0("ssh -Y ", user, "@", host, " /opt/pbs/bin/pbsnodes -aSj")),
    skip = row,
    widths = widths,
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

  node_list <- node_list %>%
    select(node, state, n_jobs, jobs_running, jobs_suspended, n_cpus_free,
      memory_free, n_mics, n_gpus, jobs, n_cpus_scale, memory_free_scale)

  message(paste0(Sys.time(), " - getNodeDetails() finished."))

  return(node_list)
}
