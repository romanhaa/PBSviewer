source("PBSviewer_helper.R")

shinyApp(
  ui = dashboardPage(
    skin = "yellow",
    dashboardHeader(title = "PBSviewer"),
    dashboardSidebar(
      sidebarMenu(
        tags$head(tags$style(HTML(".content-wrapper {overflow-x: scroll;}"))),
        menuItem("Load data", tabName = "load_data", icon = icon("spinner")),
        menuItem("Jobs", tabName = "jobs", icon = icon("angle-right"), selected = TRUE),
        menuItem("Nodes", tabName = "nodes", icon = icon("server")),
        menuItem("About", tabName = "about", icon = icon("info"))
      )
    ),
    dashboardBody(
      tags$script(HTML('$("body").addClass("fixed");')),
      tags$style(HTML(".box.box-solid.box-primary> .box-header { background: #f39c12; } .box.box-solid.box-primary { border-bottom-color: #848484; border-left-color: #848484; border-right-color: #848484; border-top-color: #848484; }")),
      tabItems(
        tabItem(tabName = "load_data",
          fluidPage(
            fluidRow(
              column(12,
                titlePanel("Load data"),
                textInput(
                  "user",
                  label = "User",
                  value = "",
                  placeholder = "Insert user name here."
                ),
                textInput(
                  "host",
                  label = "Host",
                  value = "",
                  placeholder = "Insert host address here."
                ),
                actionButton(
                  "button_load_data",
                  "Load data"
                )
              )
            )
          )
        ),
        tabItem(tabName = "jobs",
          fluidPage(
            fluidRow(
              column(12,
                titlePanel("Details by job"),
                DT::dataTableOutput("jobs")
              )
            ),
            br(),
            box(
              title = p("Number of CPUs and memory by job", style = "padding-right: 5px; display: inline"),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              plotlyOutput("jobs_n_cpu_over_memory")
            )
          )
        ),
        tabItem(tabName = "nodes",
          fluidPage(
            fluidRow(
              column(12,
                titlePanel("Details by node"),
                DT::dataTableOutput("nodes")
              )
            ),
            br(),
            box(
              title = p("CPU by job", style = "padding-right: 5px; display: inline"),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              plotlyOutput("jobs_n_cpus")
            ),
            box(
              title = p("Memory by job", style = "padding-right: 5px; display: inline"),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              plotlyOutput("jobs_memory")
            ),
            box(
              title = p("Free CPUs in time", style = "padding-right: 5px; display: inline"),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              plotlyOutput("jobs_n_cpus_free_in_time")
            ),
            box(
              title = p("Free memory in time", style = "padding-right: 5px; display: inline"),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              plotlyOutput("jobs_memory_free_in_time")
            )
          )
        ),
        tabItem(tabName = "about",
          fluidPage(
            fluidRow(
              column(12,
                titlePanel("About this application"),
                htmlOutput("about")
              )
            )
          )
        )
      )
    )
  ),
  server = function(input, output, session) {

    getPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))

    data_jobs <- reactiveValues(data = readRDS("getJobDetails_data.rds"))
    data_nodes <- reactiveValues(data = readRDS("getNodeDetails_data.rds"))
    data_nodes_time_series_temp <- reactiveValues(data = data.frame(
      "time" = as.POSIXct(character()),
      "node" = character(),
      "n_cpu" = numeric(),
      "memory" = numeric(),
      stringsAsFactors = FALSE
    ))
    data_nodes_time_series <- reactiveValues(data = data.frame(
      "time" = as.POSIXct(character()),
      "node" = character(),
      "n_cpu" = numeric(),
      "memory" = numeric(),
      stringsAsFactors = FALSE
    ))

    observe({
      req(
        input[["user"]],
        input[["host"]]
      )
      invalidateLater(60*1000, session)
      if ( testConnection(input[["user"]],input[["host"]]) != 255 ) {
        message(paste0(Sys.time(), " - Refresh data for jobs..."))
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Collecting new data... please have some patience.", value = 0)
        data_jobs$data <- getJobDetails(input[["user"]], input[["host"]])
        progress$inc(1, detail = "Done!")
      }
    })

    observe({
      req(
        input[["user"]],
        input[["host"]]
      )
      invalidateLater(60*1000, session)
      if ( testConnection(input[["user"]],input[["host"]]) != 255 ) {
        message(paste0(Sys.time(), " - Refresh data for nodes..."))
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Collecting new data... please have some patience.", value = 0)
        data_nodes$data <- getNodeDetails(input[["user"]], input[["host"]])
        data_nodes_time_series_temp$data <- getNewTimePoint(data_nodes$data)
      }
    })

    observeEvent(input[["button_load_data"]], {
      req(
        input[["user"]],
        input[["host"]]
      )
      if ( testConnection(input[["user"]],input[["host"]]) != 255 ) {
        message(paste0(Sys.time(), " - Load data..."))
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Collecting data... please have some patience.", value = 0)
        data_jobs$data <- getJobDetails(input[["user"]], input[["host"]])
        data_nodes$data <- getNodeDetails(input[["user"]], input[["host"]])
        progress$inc(1, detail = "Done!")
      }
    })

    observe({
      message(paste0(Sys.time(), " - Refresh data for time series..."))
      data_nodes_time_series$data <- rbind(isolate(data_nodes_time_series$data), data_nodes_time_series_temp$data)
    })

    jobs_color_status <- function(x) {
      colors <- rep(NA, length(x))
      for ( i in 1:length(x) ) {
        if ( x[i] == "R" ) {
          colors[i] <- "green"
        } else if ( x[i] == "E" ) {
          colors[i] <- "red"
        } else if ( x[i] == "Q" ) {
          colors[i] <- "blue"
        }
      }
      return(colors)
    }
    
    jobs_color_queue <- function(x) {
      colors <- rep(NA, length(x))
      for ( i in 1:length(x) ) {
        if ( x[i] == "turbovnc" ) {
          colors[i] <- "blue"
        } else if ( x[i] == "workq" ) {
          colors[i] <- "green"
        } else if ( x[i] == "interactive" ) {
          colors[i] <- "red"
        }
      }
      return(colors)
    }
    
    output[["jobs"]] <- DT::renderDataTable({
      table <- formattable(
        data_jobs$data,
        list(
          "status" = formatter("span", style = x ~ formattable::style(color = jobs_color_status(x))),
          "queue" = formatter("span", style = x ~ formattable::style(color = jobs_color_queue(x))),
          "mem_requested" = formatter(
            "span",
            style = function(x) {
              formattable::style(
                display            = "block",
                padding            = "0 4px",
                `border-radius`    = "4px",
                `background-color` = csscolor(gradient(as.numeric(data_jobs$data$mem_requested_not_formatted), "white", "pink"))
              )
            }
          ),
          "mem_used" = formatter(
            "span",
            style = function(x) {
              formattable::style(
                display            = "block",
                padding            = "0 4px",
                `border-radius`    = "4px",
                `background-color` = csscolor(gradient(as.numeric(data_jobs$data$mem_used_not_formatted), "white", "pink"))
              )
            }
          ),
          "vmem_used" = formatter(
            "span",
            style = function(x) {
              formattable::style(
                display            = "block",
                padding            = "0 4px",
                `border-radius`    = "4px",
                `background-color` = csscolor(gradient(as.numeric(data_jobs$data$vmem_used_not_formatted), "white", "pink"))
              )
            }
          )
        )
      ) %>%
      formattable::as.datatable(
        filter = "top",
        selection = "none",
        escape = FALSE,
        autoHideNavigation = TRUE,
        rownames = FALSE,
        class = "cell-border stripe",
        extensions = c("Buttons"),
        options = list(
          columnDefs = list(
            list(
              visible = FALSE,
              targets = c(5,7,9,13,15,17)
            )
          ),
          scrollX = TRUE,
          dom = "Bfrtip",
          paging = FALSE,
          buttons = list(
            "colvis"
          )
        )
      ) %>%
      DT::formatStyle(
        columns = c("name"),
        textAlign = "left"
      ) %>%
      DT::formatStyle(
        columns = c("cpu_time_used", "cpu_percent", "n_cpu_requested", "n_cpu_used", "mem_requested", "mem_used", "vmem_used", "mem_requested_not_formatted", "mem_used_not_formatted", "vmem_used_not_formatted"),
        textAlign = "right"
      ) %>%
      DT::formatStyle(
        columns = colnames(data_jobs$data), fontSize = "85%"
      )

      table$x$data$id <- as.numeric(table$x$data$id)
      table$x$data$cpu_percent <- as.numeric(table$x$data$cpu_percent)
      table$x$data$n_cpu_requested <- as.numeric(table$x$data$n_cpu_requested)
      table$x$data$n_cpu_used <- as.numeric(table$x$data$n_cpu_used)

      return(table)
    })

    output[["jobs_n_cpu_over_memory"]] <- plotly::renderPlotly({
      to_plot <- data_jobs$data %>%
        dplyr::select(id, name, user, n_cpu_requested, n_cpu_used, mem_requested, mem_used, mem_used_not_formatted) %>%
        dplyr::mutate(
          n_cpu_used = as.numeric(n_cpu_used),
          mem_used_not_formatted = as.numeric(mem_used_not_formatted) / 1000000000
        ) %>%
        replace(is.na(.), 0)
      to_plot$id <- factor(as.numeric(to_plot$id))
      
      plotly::plot_ly(
        data = to_plot,
        type = "scatter",
        mode = "markers",
        x = ~mem_used_not_formatted,
        y = ~n_cpu_used,
        color = ~user,
        colors = getPalette(length(unique(to_plot$user))),
        marker = list(
          size = 15,
          opacity = 0.5,
          line = list(
            color = "black",
            width = 2
          )
        ),
        hoverinfo = "text",
        text = ~paste(
          "<b>Job ID</b>: ", to_plot[ , "id" ], "<br>",
          "<b>Job Name</b>: ", to_plot[ , "name" ], "<br>",
          "<b>User</b>: ", to_plot[ , "user" ], "<br>",
          "<b># of CPUs requested</b>: ", to_plot[ , "n_cpu_requested" ], "<br>",
          "<b># of CPUs used</b>: ", to_plot[ , "n_cpu_used" ], "<br>",
          "<b>Memory requested</b>: ", to_plot[ , "mem_requested" ], "<br>",
          "<b>Memory used</b>: ", to_plot[ , "mem_used" ], "<br>"
        )
      ) %>%
      plotly::layout(
        title = "",
        xaxis = list(
          title = "Used memory [GB]",
          mirror = TRUE,
          showline = TRUE,
          zeroline = TRUE
        ),
        yaxis = list(
          title = "Used number of CPUs",
          mirror = TRUE,
          showline = TRUE,
          zeroline = TRUE
        ),
        hoverlabel = list(font = list(size = 12)),
        showlegend = FALSE
      )
    })
    
    nodes_n_cpus_scale = function(x) {
      x <- data_nodes$data$n_cpus_free
      out <- x
      for ( i in 1:length(out) ) {
        values <- strsplit(x[i], split = "/")[[1]]
        out[i] <- 1.0 - (as.double(values[1]) / as.double(values[2]))
      }
      return(out)
    }
    
    nodes_memory_scale = function(x) {
      x <- gsub(data_nodes$data$memory_free, pattern = "gb", replacement = "")
      out <- x
      for ( i in 1:length(out) ) {
        values <- strsplit(x[i], split = "/")[[1]]
        out[i] <- 1.0 - (as.double(values[1]) / as.double(values[2]))
      }
      return(out)
    }
    
    jobs_memory_color = function(x) {
      x <- gsub(data_nodes$data$memory_free, pattern = "gb", replacement = "")
      out <- x
      for ( i in 1:length(out) ) {
        values <- strsplit(x[i], split = "/")[[1]]
        out[i] <- 1.0 - (as.double(values[1]) / as.double(values[2]))
      }
      return(out)
    }
    
    output[["nodes"]] <- DT::renderDataTable({
      table <- data_nodes$data %>%
      formattable(
        list(
          "memory_free" = formatter(
            "span",
            style = function(x) {
              formattable::style(
                display            = "block",
                padding            = "0 4px",
                `border-radius`    = "4px",
                `background-color` = csscolor(gradient(as.numeric(data_nodes$data$memory_free_scale), "white", "green"))
              )
            }
          ),
          "n_cpus_free" = formatter(
            "span",
            style = function(x) {
              formattable::style(
                display            = "block",
                padding            = "0 4px",
                `border-radius`    = "4px",
                `background-color` = csscolor(gradient(as.numeric(data_nodes$data$n_cpus_scale), "white", "green"))
              )
            }
          )
        )
      ) %>%
      formattable::as.datatable(
        filter = "top",
        selection = "single",
        escape = FALSE,
        autoHideNavigation = TRUE,
        rownames = FALSE,
        class = "cell-border stripe",
        extensions = c("Buttons"),
        options = list(
          columnDefs = list(
            list(
              visible = FALSE,
              targets = c(1,3,4,7,8,10,11)
            )
          ),
          scrollX = TRUE,
          dom = "Bfrtip",
          paging = FALSE,
          buttons = list(
            "colvis"
          )
        )
      ) %>% 
      DT::formatStyle(
        columns = c("n_jobs","memory_free","n_cpus_free"),
        textAlign = "right"
      ) %>% 
      DT::formatStyle(
        columns = colnames(data_nodes$data), fontSize = "85%"
      )

      table$x$data$n_jobs <- as.numeric(table$x$data$n_jobs)
      
      return(table)
    })

    output[["jobs_n_cpus"]] <- renderPlotly({
      if ( !is.null(input[["nodes_rows_selected"]]) ) {
        selected_row <- input[["nodes_rows_selected"]]
        jobs <- strsplit(data_nodes$data[selected_row,"jobs"], split = ",")[[1]]
        to_plot <- data_jobs$data %>%
          dplyr::filter(id %in% jobs)
      } else {
        to_plot <- data_jobs$data
      }
      to_plot <- to_plot %>%
      dplyr::mutate(
        n_cpu_requested = as.numeric(n_cpu_requested),
        n_cpu_used = as.numeric(n_cpu_used)
      ) %>%
      replace(is.na(.), 0)
      to_plot$id <- factor(as.numeric(to_plot$id))
      to_plot %>%
      plot_ly() %>%
      add_trace(
        x = ~id,
        y = ~n_cpu_requested,
        name = "requested",
        type = "bar",
        hoverinfo = "text",
        text = ~paste(
          "<b>Job ID</b>: ", to_plot[ , "id" ], "<br>",
          "<b>Job Name</b>: ", to_plot[ , "name" ], "<br>",
          "<b>User</b>: ", to_plot[ , "user" ], "<br>",
          "<b># of CPUs requested</b>: ", to_plot[ , "n_cpu_requested" ], "<br>",
          "<b># of CPUs used</b>: ", to_plot[ , "n_cpu_used" ], "<br>",
          "<b>Memory requested</b>: ", to_plot[ , "mem_requested" ], "<br>",
          "<b>Memory used</b>: ", to_plot[ , "mem_used" ], "<br>"
        ),
        marker = list(
          color = "#e67e22",
          line = list(
            color = "#d35400",
            width = 1.5
          )
        )
      ) %>%
      add_trace(
        x = ~id,
        y = ~n_cpu_used,
        name = "used",
        type = "bar",
        hoverinfo = "text",
        text = ~paste(
          "<b>Job ID</b>: ", to_plot[ , "id" ], "<br>",
          "<b>Job Name</b>: ", to_plot[ , "name" ], "<br>",
          "<b>User</b>: ", to_plot[ , "user" ], "<br>",
          "<b># of CPUs requested</b>: ", to_plot[ , "n_cpu_requested" ], "<br>",
          "<b># of CPUs used</b>: ", to_plot[ , "n_cpu_used" ], "<br>",
          "<b>Memory requested</b>: ", to_plot[ , "mem_requested" ], "<br>",
          "<b>Memory used</b>: ", to_plot[ , "mem_used" ], "<br>"
        ),
        marker = list(
          color = "#f1c40f",
          line = list(
            color = "#f39c12",
            width = 1.5
          )
        )
      ) %>%
      layout(
        title = "",
        barmode = "group",
        xaxis = list(
          title = "",
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE
        ),
        yaxis = list(
          title = "# of CPUs",
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE
        )
      )
    })

    output[["jobs_memory"]] <- renderPlotly({
      if ( !is.null(input[["nodes_rows_selected"]]) ) {
        selected_row <- input[["nodes_rows_selected"]]
        jobs <- strsplit(data_nodes$data[selected_row,"jobs"], split = ",")[[1]]
        to_plot <- data_jobs$data %>%
          dplyr::filter(id %in% jobs)
      } else {
        to_plot <- data_jobs$data
      }
      to_plot <- to_plot %>%
      dplyr::mutate(
        mem_requested_not_formatted = as.numeric(mem_requested_not_formatted) / 1000000000,
        mem_used_not_formatted = as.numeric(mem_used_not_formatted) / 1000000000
      ) %>%
      replace(is.na(.), 0)
      to_plot$id <- factor(as.numeric(to_plot$id))
      if ( max(c(to_plot$mem_requested_not_formatted,to_plot$mem_used_not_formatted)) < 80 ) {
        ymax <- 80
      } else {
        ymax <- max(c(to_plot$mem_requested_not_formatted,to_plot$mem_used_not_formatted)) * 1.1
      }
      to_plot %>%
      plot_ly() %>%
      add_trace(
        x = ~id,
        y = ~mem_requested_not_formatted,
        name = "requested",
        type = "bar",
        hoverinfo = "text",
        text = ~paste(
          "<b>Job ID</b>: ", to_plot[ , "id" ], "<br>",
          "<b>Job Name</b>: ", to_plot[ , "name" ], "<br>",
          "<b>User</b>: ", to_plot[ , "user" ], "<br>",
          "<b>Status</b>: ", to_plot[ , "status" ], "<br>",
          "<b># of CPUs requested</b>: ", to_plot[ , "n_cpu_requested" ], "<br>",
          "<b># of CPUs used</b>: ", to_plot[ , "n_cpu_used" ], "<br>",
          "<b>Memory requested</b>: ", to_plot[ , "mem_requested" ], "<br>",
          "<b>Memory used</b>: ", to_plot[ , "mem_used" ], "<br>"
        ),
        marker = list(
          color = "#e67e22",
          line = list(
            color = "#d35400",
            width = 1.5
          )
        )
      ) %>%
      add_trace(
        x = ~id,
        y = ~mem_used_not_formatted,
        name = "used",
        type = "bar",
        hoverinfo = "text",
        text = ~paste(
          "<b>Job ID</b>: ", to_plot[ , "id" ], "<br>",
          "<b>Job Name</b>: ", to_plot[ , "name" ], "<br>",
          "<b>User</b>: ", to_plot[ , "user" ], "<br>",
          "<b>Status</b>: ", to_plot[ , "status" ], "<br>",
          "<b># of CPUs requested</b>: ", to_plot[ , "n_cpu_requested" ], "<br>",
          "<b># of CPUs used</b>: ", to_plot[ , "n_cpu_used" ], "<br>",
          "<b>Memory requested</b>: ", to_plot[ , "mem_requested" ], "<br>",
          "<b>Memory used</b>: ", to_plot[ , "mem_used" ], "<br>"
        ),
        marker = list(
          color = "#f1c40f",
          line = list(
            color = "#f39c12",
            width = 1.5
          )
        )
      ) %>%
      layout(
        title = "",
        barmode = "group",
        xaxis = list(
          title = "",
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE
        ),
        yaxis = list(
          title = "Memory [GB]",
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE,
          range = c(0, ymax)
        )
      )
    })

    output[["jobs_n_cpus_free_in_time"]] <- renderPlotly({
      if ( nrow(data_nodes_time_series$data) >= 1 ) {
        if ( !is.null(input[["nodes_rows_selected"]]) ) {
          selected_row <- input[["nodes_rows_selected"]]
          node_to_show <- data_nodes_time_series$data[selected_row,"node"]
          to_plot <- data_nodes_time_series$data %>%
            dplyr::filter(node == node_to_show)
        } else {
          to_plot <- data_nodes_time_series$data
        }
        if ( length(unique(to_plot$time)) <= 20 ) {
          style <- "lines+markers"
          markers <- list(size = 10, opacity = 0.5)
        } else {
          style <- "lines+markers"
          markers <- list(size = 5, opacity = 0.5)
        }
        plotly::plot_ly(
          to_plot,
          x = ~time,
          y = ~n_cpu,
          color = ~node,
          colors = getPalette(length(unique(to_plot$node))),
          mode = style,
          type = "scatter",
          hoverinfo = "text",
          text = ~paste(
            "<b>Time</b>: ", to_plot[ , "time" ], "<br>",
            "<b>Node</b>: ", to_plot[ , "node" ], "<br>",
            "<b>Number of free CPUs</b>: ", to_plot[ , "n_cpu" ], "<br>",
            "<b>Free memory [GB]</b>: ", to_plot[ , "memory" ], "<br>"
          ),
          marker = markers,
          line = list(
            opacity = 0.5
          )
        ) %>%
        layout(
          title = "",
          xaxis = list(
            title = "",
            mirror = TRUE,
            showline = TRUE,
            zeroline = FALSE
          ),
          yaxis = list(
            title = "Number of free CPUs",
            mirror = TRUE,
            showline = TRUE,
            zeroline = FALSE
          )
        )
      }
    })

    output[["jobs_memory_free_in_time"]] <- plotly::renderPlotly({
      if ( nrow(data_nodes_time_series$data) >= 1 ) {
        if ( !is.null(input[["nodes_rows_selected"]]) ) {
          selected_row <- input[["nodes_rows_selected"]]
          node_to_show <- data_nodes_time_series$data[selected_row,"node"]
          to_plot <- data_nodes_time_series$data %>%
            dplyr::filter(node == node_to_show)
        } else {
          to_plot <- data_nodes_time_series$data
        }
        if ( length(unique(to_plot$time)) <= 20 ) {
          style <- "lines+markers"
          markers <- list(size = 10, opacity = 0.5)
        } else {
          style <-  "lines+markers"
          markers <- list(size = 5, opacity = 0.5)
        }
        plotly::plot_ly(
          to_plot,
          x = ~time,
          y = ~memory,
          color = ~node,
          colors = getPalette(length(unique(to_plot$node))),
          mode = style,
          type = "scatter",
          hoverinfo = "text",
          text = ~paste(
            "<b>Time</b>: ", to_plot[ , "time" ], "<br>",
            "<b>Node</b>: ", to_plot[ , "node" ], "<br>",
            "<b>Number of free CPUs</b>: ", to_plot[ , "n_cpu" ], "<br>",
            "<b>Free memory [GB]</b>: ", to_plot[ , "memory" ], "<br>"
          ),
          marker = markers,
          line = list(
            opacity = 0.5
          )
        ) %>%
        layout(
          title = "",
          xaxis = list(
            title = "",
            mirror = TRUE,
            showline = TRUE,
            zeroline = FALSE
          ),
          yaxis = list(
            title = "Free memory [GB]",
            mirror = TRUE,
            showline = TRUE,
            zeroline = FALSE
          )
        )
      }
    })

    output[["about"]] <- renderText({
      '<b>Author:</b><br>
      Roman Hillje<br>
      Department of Experimental Oncology<br>
      IEO, European Institute of Oncology IRCCS, Milan<br>
      <br>
      <b>Links:</b><br>
      <ul>
        <li><a href=https://github.com/romanhaa/PBSviewer title="PBSviewer repository on GitHub" target="_blank"><b>PBSviewer repository on GitHub</b></a></li>
      </ul>'
    })

  }
)










