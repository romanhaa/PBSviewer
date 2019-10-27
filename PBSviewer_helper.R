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
    system(paste0("ssh ", user, "@", host, " echo")),
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
  x <- filter(x, !node %in% c("cn13","cn14","hpcfe02"))
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

formatMemory <- function(x) {
  for ( i in 1:length(x) ) {
    if ( grepl(x[i], pattern = "kb") ) {
      x[i] <- utils:::format.object_size(as.numeric(gsub(x[i], pattern = "kb", replacement = "000")), "auto")
    } else if ( grepl(x[i], pattern = "mb") ) {
      x[i] <- utils:::format.object_size(as.numeric(gsub(x[i], pattern = "mb", replacement = "000000")), "auto")
    } else if ( grepl(x[i], pattern = "gb") ) {
      x[i] <- utils:::format.object_size(as.numeric(gsub(x[i], pattern = "gb", replacement = "000000000")), "auto")
    } else {
      x[i] <- x[i]
    }
  }
  return(x)
}

numericMemory <- function(x) {
  for ( i in 1:length(x) ) {
    if ( grepl(x[i], pattern = "kb") ) {
      x[i] <- as.numeric(gsub(x[i], pattern = "kb", replacement = "000"))
    } else if ( grepl(x[i], pattern = "mb") ) {
      x[i] <- as.numeric(gsub(x[i], pattern = "mb", replacement = "000000"))
    } else if ( grepl(x[i], pattern = "gb") ) {
      x[i] <- as.numeric(gsub(x[i], pattern = "gb", replacement = "000000000"))
    } else {
      x[i] <- as.numeric(gsub(x[i], pattern = "[^0-9.-]", replacement = ""))
    }
  }
  return(x)
}

formatTime <- function(x) {
  temp_hours <- vapply(strsplit(x, ":"), `[`, 1, FUN.VALUE = character(1)) %>%
    as.integer()
  temp_hours <- temp_hours * 60
  temp_minutes <- vapply(strsplit(x, ":"), `[`, 2, FUN.VALUE = character(1)) %>%
    as.integer()
  return(temp_hours + temp_minutes)
}

getJobDetails <- function(user, host) {
  message(paste0(Sys.time(), " - Run getJobDetails()"))

  t <- read.fwf(
      pipe(paste0("ssh ", user, "@", host, " /opt/pbs/bin/qstat -fw")),
      skip = 0,
      widths = c(800),
      stringsAsFactors = FALSE
    ) %>%
    dplyr::mutate(
      V1 = trimws(V1),
      V1 = gsub(V1, pattern = 'Job Id: ', replacement = 'Job Id = '),
      line_number = rownames(.) %>% as.integer()
    )

  list_of_jobs <- t %>%
    dplyr::filter(grepl(V1, pattern = 'Job Id')) %>%
    dplyr::pull(V1)

  lines_NA <- t %>%
    dplyr::filter(is.na(V1)) %>%
    dplyr::pull(line_number)

  job_details_not_formatted <- list()

  for ( i in 1:length(list_of_jobs) ) {
    current_job <- list_of_jobs[i]
    row_start_current_job <- t %>%
      dplyr::filter(V1 == current_job) %>%
      dplyr::pull(line_number)
    row_end_current_job <- lines_NA[i] - 1
    temp_data <- t %>%
      dplyr::select(1) %>%
      dplyr::slice(row_start_current_job:row_end_current_job) %>%
      tidyr::separate(V1, c('parameter','value'), sep = ' = ')
    job_details_not_formatted[[current_job]] <- temp_data
  }

  job_details_not_formatted <- job_details_not_formatted %>%
    purrr::reduce(full_join, by = 'parameter') %>%
    t()
  parameter_names <- job_details_not_formatted[1,]
  job_details_formatted <- job_details_not_formatted[2:nrow(job_details_not_formatted),] %>%
    tibble::as_tibble()
  colnames(job_details_formatted) <- parameter_names

  job_details_formatted <- job_details_formatted %>%
    dplyr::rename(
      id = `Job Id`,
      name = Job_Name,
      user = Job_Owner,
      status = job_state,
      date_submitted = qtime,
      walltime = resources_used.walltime,
      cpu_time_used = resources_used.cput,
      cpu_percent = resources_used.cpupercent,
      n_cpu_requested = Resource_List.ncpus,
      n_cpu_used = resources_used.ncpus,
      mem_requested_raw = Resource_List.mem,
      mem_used_raw = resources_used.mem,
      vmem_used_raw = resources_used.vmem,
      date_created = ctime,
      date_eligible = etime,
      date_last_modified = mtime,
      date_start = stime
    ) %>%
    dplyr::mutate(
      user = vapply(strsplit(user, "@"), `[`, 1, FUN.VALUE = character(1)),
      server = gsub(server, pattern = '.cluster.loc', replacement = ''),
      date_submitted = strptime(date_submitted, format = "%a %B %d %H:%M:%S %Y") %>% as.character(),
      walltime = formatTime(walltime),
      cpu_time_used = formatTime(cpu_time_used),
      cpu_percent = cpu_percent %>% as.integer(),
      n_cpu_requested = n_cpu_requested %>% as.integer(),
      n_cpu_used = n_cpu_used %>% as.integer(),
      mem_requested = formatMemory(mem_requested_raw),
      mem_requested_numeric = numericMemory(mem_requested_raw),
      mem_used = formatMemory(mem_used_raw),
      mem_used_numeric = numericMemory(mem_used_raw),
      vmem_used = formatMemory(vmem_used_raw),
      vmem_used_numeric = numericMemory(vmem_used_raw),
      exec_host = vapply(strsplit(exec_host, "/"), `[`, 1, FUN.VALUE = character(1)),
      date_created = strptime(date_created, format = "%a %B %d %H:%M:%S %Y") %>% as.character(),
      date_eligible = strptime(date_eligible, format = "%a %B %d %H:%M:%S %Y") %>% as.character(),
      date_last_modified = strptime(date_last_modified, format = "%a %B %d %H:%M:%S %Y") %>% as.character(),
      date_start = strptime(date_start, format = "%a %B %d %H:%M:%S %Y") %>% as.character()
    ) %>%
    dplyr::select(
      id, name, user, queue, date_submitted, status, exec_host, walltime, cpu_time_used,
      n_cpu_requested, n_cpu_used, mem_requested, mem_used, vmem_used,
      everything()
    )

  # user <- trimws(user)
  # host <- trimws(host)
  # message(paste0(Sys.time(), " - user: ", user, "; host: ", host))
  # #message(paste0(Sys.time(), " - Step 1: Get list of jobs"))
  # job_list <- read.fwf(
  #   pipe(paste0("ssh -Y ", user, "@", host, " /opt/pbs/bin/qstat")),
  #   widths = c(80),
  #   stringsAsFactors = FALSE
  # )
  # row <- grep(job_list[,1], pattern = "--------")
  # temp <- gsub(job_list[row,1], pattern = "  ", replacement = "- ")
  # temp <- as.vector(gregexpr(temp, pattern = " ")[[1]])
  # job_list <- job_list[(row+1):nrow(job_list),]
  # job_list <- substr(job_list, 1, temp[1])
  # job_list <- trimws(job_list)
  # #message(paste0(Sys.time(), " - Step 1: Done."))
  # job_details <- data.frame(
  #   "id" = character(),
  #   "name" = character(),
  #   "user" = character(),
  #   "status" = character(),
  #   "queue" = character(),
  #   "server" = character(),
  #   "date_submitted" = character(),
  #   "walltime" = character(),
  #   "cpu_time_used" = character(),
  #   "cpu_percent" = numeric(),
  #   "n_cpu_requested" = numeric(),
  #   "n_cpu_used" = numeric(),
  #   "mem_requested" = character(),
  #   "mem_requested_not_formatted" = integer(),
  #   "mem_used" = character(),
  #   "vmem_used" = character(),
  #   "executing_host" = character(),
  #   stringsAsFactors = FALSE
  # )
  # #message(paste0(Sys.time(), " - Step 2: Get details for each job..."))
  # temp_data <- parallel::mclapply(
  #   job_list,
  #   FUN = function(x) {
  #     c <- tryCatch(
  #       read.fwf(
  #         pipe(paste0("ssh -Y ", user, "@", host, " /opt/pbs/bin/qstat -f ", x)),
  #         skip = 0,
  #         widths = c(500),
  #         stringsAsFactors = FALSE
  #       ),
  #       error = function(err) NA
  #     )
  #   },
  #   mc.cores = parallel::detectCores()-1,
  #   mc.preschedule = TRUE
  # )
  # #message(paste0(Sys.time(), " - Step 2: Done."))
  # temp_data <- temp_data[which(!is.na(temp_data))]
  # job_names <- rep(NA, length(temp_data))
  # for ( i in 1:length(temp_data) ) {
  #   position_of_variable_List <- grep(temp_data[[i]][,1], pattern = "Variable_List")
  #   temp_data[[i]] <- temp_data[[i]][1:position_of_variable_List,]
  #   temp_data[[i]] <- trimws(temp_data[[i]])
  #   job_names[i] <- gsub(temp_data[[i]][1], pattern = "Job Id: ", replacement = "")
  #   temp_data[[i]] <- temp_data[[i]][-1]
  #   temp_data[[i]] <- data.frame(
  #     do.call(
  #       "rbind",
  #       strsplit(
  #         temp_data[[i]],
  #         " = ",
  #         fixed = TRUE
  #       )
  #     ),
  #     stringsAsFactors = FALSE
  #   )
  # }
  # names(temp_data) <- job_names
  # #message(paste0(Sys.time(), " - Step 3: Parse details for each job..."))
  # for ( i in 1:length(temp_data) ) {
  #   temp_job_id <- names(temp_data)[i]
  #   #message(temp_job_id)
  #   temp_id <- gsub(temp_job_id, pattern = "\\.hpcfe01", replacement = "")
  #   temp_id <- as.numeric(gsub(temp_id, pattern = "[^0-9.-]", replacement = ""))
  #   data_this_job <- temp_data[[i]]
  #   #
  #   temp_job_owner <- strsplit(
  #     data_this_job[which(data_this_job[,1] == "Job_Owner"),2],
  #     "@",
  #     fixed = TRUE
  #   )[[1]][1]
  #   #
  #   temp_server <- gsub(
  #     data_this_job[which(data_this_job[,1] == "server"),2],
  #     pattern = ".cluster.loc",
  #     replacement = ""
  #   )
  #   #
  #   temp_date_submitted <- as.character(
  #     strptime(
  #       data_this_job[which(data_this_job[,1] == "ctime"),2],
  #       format="%a %B %d %H:%M:%S %Y"
  #     )
  #   )
  #   #
  #   if ( "resources_used.walltime" %in% data_this_job[,1] ) {
  #     temp_resources_walltime <- data_this_job[which(data_this_job[,1] == "resources_used.walltime"),2]
  #   } else {
  #     temp_resources_walltime <- NA
  #   }
  #   #
  #   if ( "resources_used.cput" %in% data_this_job[,1] ) {
  #     temp_resources_cpu_time_used <- data_this_job[which(data_this_job[,1] == "resources_used.cput"),2]
  #   } else {
  #     temp_resources_cpu_time_used <- NA
  #   }
  #   #
  #   if ( "resources_used.cpupercent" %in% data_this_job[,1] ) {
  #     temp_resources_cpu_percent <- as.numeric(data_this_job[which(data_this_job[,1] == "resources_used.cpupercent"),2])
  #   } else {
  #     temp_resources_cpu_percent <- NA
  #   }
  #   #
  #   if ( "Resource_List.ncpus" %in% data_this_job[,1] ) {
  #     temp_resources_n_cpus_requested <- as.numeric(data_this_job[which(data_this_job[,1] == "Resource_List.ncpus"),2])
  #   } else {
  #     temp_resources_n_cpus_requested <- 0
  #   }
  #   #
  #   if ( "resources_used.ncpus" %in% data_this_job[,1] ) {
  #     temp_resources_n_cpus_used <- as.numeric(data_this_job[which(data_this_job[,1] == "resources_used.ncpus"),2])
  #   } else {
  #     temp_resources_n_cpus_used <- 0
  #   }
  #   #
  #   if ( "Resource_List.mem" %in% data_this_job[,1] ) {
  #     x <- data_this_job[which(data_this_job[,1] == "Resource_List.mem"),2]
  #     x <- jobsFormatMemory(x)
  #     temp_resources_mem_requested_not_formatted <- x[["numeric"]]
  #     temp_resources_mem_requested <- x[["formatted"]]
  #   } else {
  #     temp_resources_mem_requested_not_formatted <- 0
  #     temp_resources_mem_requested <- "0 Mb"
  #   }
  #   #
  #   if ( "resources_used.mem" %in% data_this_job[,1] ) {
  #     x <- data_this_job[which(data_this_job[,1] == "resources_used.mem"),2]
  #     x <- jobsFormatMemory(x)
  #     temp_resources_mem_used_not_formatted <- x[["numeric"]]
  #     temp_resources_mem_used <- x[["formatted"]]
  #   } else {
  #     temp_resources_mem_used_not_formatted <- 0
  #     temp_resources_mem_used <- "0 Mb"
  #   }
  #   #
  #   if ( "resources_used.vmem" %in% data_this_job[,1] ) {
  #     x <- data_this_job[which(data_this_job[,1] == "resources_used.vmem"),2]
  #     x <- jobsFormatMemory(x)
  #     temp_resources_vmem_used_not_formatted <- x[["numeric"]]
  #     temp_resources_vmem_used <- x[["formatted"]]
  #   } else {
  #     temp_resources_vmem_used_not_formatted <- 0
  #     temp_resources_vmem_used <- "0 Mb"
  #   }
  #   #
  #   if ( "exec_host" %in% data_this_job[,1] ) {
  #     temp_executing_host <- strsplit(data_this_job[which(data_this_job[,1] == "exec_host"),2], split = "/")[[1]][1]
  #   } else {
  #     temp_executing_host <- NA
  #   }
  #   #
  #   temp_details <- data.frame(
  #     "id" = temp_id,
  #     "name" = data_this_job[which(data_this_job[,1] == "Job_Name"), 2],
  #     "user" = temp_job_owner,
  #     "status" = data_this_job[which(data_this_job[,1] == "job_state"), 2],
  #     "queue" = data_this_job[which(data_this_job[,1] == "queue"), 2],
  #     "server" = temp_server,
  #     "date_submitted" = temp_date_submitted,
  #     "walltime" = temp_resources_walltime,
  #     "cpu_time_used" = temp_resources_cpu_time_used,
  #     "cpu_percent" = temp_resources_cpu_percent,
  #     "n_cpu_requested" = temp_resources_n_cpus_requested,
  #     "n_cpu_used" = temp_resources_n_cpus_used,
  #     "mem_requested" = temp_resources_mem_requested,
  #     "mem_requested_not_formatted" = temp_resources_mem_requested_not_formatted,
  #     "mem_used" = temp_resources_mem_used,
  #     "mem_used_not_formatted" = temp_resources_mem_used_not_formatted,
  #     "vmem_used" = temp_resources_vmem_used,
  #     "vmem_used_not_formatted" = temp_resources_vmem_used_not_formatted,
  #     "executing_host" = temp_executing_host,
  #     stringsAsFactors = FALSE
  #   )
  #   #
  #   job_details <- rbind(job_details, temp_details)
  # }
  # #message(paste0(Sys.time(), " - Step 3: Done."))
  # for ( i in 1:length(job_details) ) {
  #   job_details[,i] <- trimws(job_details[,i])
  # }
  message(paste0(Sys.time(), " - getJobDetails() finished."))
  return(job_details_formatted)
}

getNodeDetails <- function(user, host) {

  message(paste0(Sys.time(), " - Run getNodeDetails()"))

  user <- trimws(user)
  host <- trimws(host)

  message(paste0(Sys.time(), " - user: ", user, "; host: ", host))

  node_list <- read.fwf(
    pipe(paste0("ssh ", user, "@", host, " /opt/pbs/bin/pbsnodes -aSj")),
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
    pipe(paste0("ssh ", user, "@", host, " /opt/pbs/bin/pbsnodes -aSj")),
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
      jobs_suspended = as.numeric(jobs_suspended)
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
