#' Sensor Data Excel
#'
#' This function allows you to input your temperature data from the sensor and analyze it for monthly, dayly, hourly and minute by minute average temperatures and heat index in addition to creating graphs. The output is an excel file with multiple sheets. When you run the function, you will be prompted to choose which csv you want to analyze
#' @param folder_name what you want to call the folder where the excel workbook and graphs will save
#' @return Excel sheets with monthly, dayly, hourly and minute by minute average temperatures and heat index
#' @example process_sensors_excel_version(your_file_name_here) will prompt file selection; choose a .csv file
#' @export

process_sensors_excel_version <- function(folder_name) {

   #This will prompt you to choose the csv file you want to analyze
   person_location <- read_csv(file.choose())

   # create temporary object with clean column names
   temp <<- janitor::clean_names(person_location)
   # parse date objects and remove unneeded columns
   temp <<- temp %>%
      mutate(date = mdy(date)) %>%
      mutate(year = year(date)) %>%
      mutate(month = month(date)) %>%
      mutate(day = day(date)) %>%
      mutate(hour = hour(time)) %>%
      mutate(minute = minute(time)) %>%
      select(date, year, month, day, hour, minute, temperature, humidity) %>%
      mutate(heat_index = heat.index(t=temperature, rh=humidity, temperature.metric = "fahrenheit"))

   # calculate average temperature, humidity, heat index by month for the sensor, and store it as temporary object
   temp_humidity_heat_index_month <<- temp %>%
      group_by(month) %>%
      summarise(mean_temp = round(mean(temperature),1),
                mean_humidity = round(mean(humidity),1),
                mean_heat_index = round(mean(heat_index),1)
      )

   # calculate average temperature by day for the sensor, and store it as temporary object
   temp_humidity_heat_index_day <<- temp %>%
      mutate(day = make_datetime(year = year, month = month, day = day)) %>%
      group_by(day) %>%
      summarise(mean_temp = round(mean(temperature),1),
                mean_humidity = round(mean(humidity),1),
                mean_heat_index = round(mean(heat_index),1)
      ) %>%
      select(day, mean_temp, mean_humidity, mean_heat_index)

   # calculate average temperature by hour for the sensor, and store it as temporary object
   temp_humidity_heat_index_hour <<- temp %>%
      mutate(hour = make_datetime(year = year, month = month, day = day, hour = hour)) %>%
      group_by(hour) %>%
      summarise(mean_temp = round(mean(temperature),1),
                mean_humidity = round(mean(humidity),1),
                mean_heat_index = round(mean(heat_index),1)
      ) %>%
      select(hour, mean_temp, mean_humidity, mean_heat_index)

   # calculate average temperature by minute for the sensor, and store it as temporary object
   temp_humidity_heat_index_minute <<- temp %>%
      mutate(minute = make_datetime(year = year, month = month, day = day, hour = hour, min = minute)) %>%
      group_by(minute) %>%
      summarise(mean_temp = round(mean(temperature),1),
                mean_humidity = round(mean(humidity),1),
                mean_heat_index = round(mean(heat_index),1)
      )

   # Create an empty Excel workbook
   wb <- openxlsx::createWorkbook("workbook")

   # Create four empty Excel worksheets in Workbook
   openxlsx::addWorksheet(wb, "temp_humidity_heat_index_month")
   openxlsx::addWorksheet(wb, "temp_humidity_heat_index_day")
   openxlsx::addWorksheet(wb, "temp_humidity_heat_index_hour")
   openxlsx::addWorksheet(wb, "temp_humidity_heat_index_minute")

   # Write aggregated dataframes to corresponding worksheets
   openxlsx::writeData(wb, sheet = 1, temp_humidity_heat_index_month)
   openxlsx::writeData(wb, sheet = 2, temp_humidity_heat_index_day)
   openxlsx::writeData(wb, sheet = 3, temp_humidity_heat_index_hour)
   openxlsx::writeData(wb, sheet = 4, temp_humidity_heat_index_minute)

   # Create folder in working directory
   folder_path <- paste0(getwd(), "/", deparse(substitute(folder_name)))
   dir.create(path = folder_path)
   excel_filename <- paste0(getwd(), "/", deparse(substitute(folder_name)),"/",  deparse(substitute(folder_name)), "_", Sys.Date(), "_temp_humidity_heat_index_means.xlsx")

   # Save worksheet to correct folder
   openxlsx::saveWorkbook(wb, excel_filename, overwrite = TRUE)


   ######################
   #### Build graphs ####
   ######################

   # Build monthly graph temperature
   chart_title <- paste0(deparse(substitute(folder_name)), " mean temperature by month")
   temp_month_graph <- ggplot(data = temp_humidity_heat_index_month, aes(x = month, y = mean_temp)) +
      geom_bar(stat="identity") +
      ggtitle(chart_title) +
      xlab("Month Number") +
      ylab("Mean Temperature")
   plot(temp_month_graph)
   filename <- paste0(deparse(substitute(folder_name)),"_", Sys.Date(), "_temp_means_month.pdf")
   filepath <- paste0(getwd(), "/", deparse(substitute(folder_name)),"/")
   ggsave(filename, plot = temp_month_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)

   # Build daily graph temperature
   chart_title <- paste0(deparse(substitute(folder_name)), " mean temperature by day")
   temp_day_graph <- ggplot() +
      geom_line(data = temp_humidity_heat_index_day, aes(x = day, y = mean_temp, colour="real temp")) +
      geom_line(data = temp_humidity_heat_index_day, aes(x = day, y = mean_heat_index, colour="heat index")) +
      scale_color_manual(
         values = c('real temp' = 'red',
                    'heat index' = 'purple')
      ) +
      ggtitle(chart_title) +
      xlab("Days") +
      ylab("Means") +
      scale_x_datetime(date_labels = "%b %d %H", date_breaks = "1 day")
   plot(temp_day_graph)
   filename <- paste0(deparse(substitute(folder_name)),"_", Sys.Date(), "_temp_means_day.pdf")
   filepath <- paste0(getwd(), "/", deparse(substitute(folder_name)),"/")
   ggsave(filename, plot = temp_day_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)

   # Build hourly graph temperature
   chart_title <- paste0(deparse(substitute(folder_name)), " mean temperature by hour")
   temp_hour_graph <- ggplot(data = temp_humidity_heat_index_hour, aes(x = hour, y = mean_temp)) +
      geom_line() +
      ggtitle(chart_title) +
      xlab("Hours") +
      ylab("Mean Temperature") +
      scale_x_datetime(date_labels = "%b %d %H", date_breaks = "1 day") +
      theme(axis.text.x = element_text(angle=50,hjust=1))
   plot(temp_hour_graph)
   filename <- paste0(deparse(substitute(folder_name)),"_", Sys.Date(), "_temp_means_hour.pdf")
   filepath <- paste0(getwd(), "/", deparse(substitute(folder_name)),"/")
   ggsave(filename, plot = temp_hour_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)

   # Build minute graph temperature
   chart_title <- paste0(deparse(substitute(folder_name)), " mean temperature by minute")
   temp_minute_graph <- ggplot(data =  temp_humidity_heat_index_minute, aes(x = minute, y = mean_temp)) +
      geom_line() +
      ggtitle(chart_title) +
      xlab("Hours") +
      ylab("Mean Temperature") +
      scale_x_datetime(date_labels = "%b %d %H", date_breaks = "1 day") +
      theme(axis.text.x = element_text(angle=50,hjust=1))
   plot(temp_minute_graph)
   filename <- paste0(deparse(substitute(folder_name)),"_", Sys.Date(), "_temp_means_minute.pdf")
   filepath <- paste0(getwd(), "/", deparse(substitute(folder_name)),"/")
   ggsave(filename, plot = temp_minute_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)

}

#' Sensor Data R
#'
#' This function allows you to input your temperature data from the sensor and analyze it for monthly, dayly, hourly and minute by minute average temperatures and heat index in addition to creating graphs. The output is an R script with tables and graphs. When you run the function, you will be prompted to choose which csv you want to analyze.
#' @param folder_name what you want to call the folder where the excel workbook and graphs will save
#' @return Graphs and tables with monthly, dayly, hourly and minute by minute average temperatures and heat index
#' @export


process_sensors_r_version <- function(folder_name) {

   person_location <- read_csv(file.choose())

   # clean column names
   temp <<- janitor::clean_names(person_location)

   # parse date objects and remove unneeded columns
   temp <<- temp %>%
      mutate(date = mdy(date)) %>%
      mutate(year = year(date)) %>%
      mutate(month = month(date)) %>%
      mutate(day = day(date)) %>%
      mutate(hour = hour(time)) %>%
      mutate(minute = minute(time)) %>%
      select(date, year, month, day, hour, minute, temperature, humidity) %>%
      mutate(heat_index = heat.index(t=temperature, rh=humidity, temperature.metric = "fahrenheit"))

   # calculate average temperature, humidity, heat index by month for the sensor, and store it as temporary object
   temp_humidity_heat_index_month <<- temp %>%
      group_by(month) %>%
      summarise(mean_temp = round(mean(temperature),1),
                mean_heat_index = round(mean(heat_index),1),
                mean_humidity = round(mean(humidity),1))
   table_name <- paste0(deparse(substitute(folder_name)), "_month_averages")
   assign(table_name, temp_humidity_heat_index_month, envir = parent.frame())

   # calculate average temperature by day for the sensor, and store it as temporary object
   temp_humidity_heat_index_day <- temp %>%
      mutate(day = make_datetime(year = year, month = month, day = day)) %>%
      group_by(day) %>%
      summarise(mean_temp = round(mean(temperature),1),
                mean_heat_index = round(mean(heat_index),1),
                mean_humidity = round(mean(humidity),1)) %>%
      select(day, mean_temp, mean_humidity, mean_heat_index)
   table_name <- paste0(deparse(substitute(folder_name)), "_day_averages")
   assign(table_name, temp_humidity_heat_index_day, envir = parent.frame())

   # calculate average temperature by hour for the sensor, and store it as temporary object
   temp_humidity_heat_index_hour <- temp %>%
      mutate(hour = make_datetime(year = year, month = month, day = day, hour = hour)) %>%
      group_by(hour) %>%
      summarise(mean_temp = round(mean(temperature),1),
                mean_heat_index = round(mean(heat_index),1),
                mean_humidity = round(mean(humidity),1)) %>%
      select(hour, mean_temp, mean_humidity, mean_heat_index)
   table_name <- paste0(deparse(substitute(folder_name)), "_hourly_averages")
   assign(table_name, temp_humidity_heat_index_hour, envir = parent.frame())

   # Calculate average temperature by minute for the sensor, and store it as temporary object
   # Note: because temperature collection is done at intervals that are sometimes greater than a minute, won't have          exactly one value per minute.
   temp_humidity_heat_index_minute <- temp %>%
      mutate(minute = make_datetime(year = year, month = month, day = day, hour = hour, min = minute)) %>%
      group_by(minute) %>%
      summarise(mean_temp = round(mean(temperature),1),
                mean_heat_index = round(mean(heat_index),1),
                mean_humidity = round(mean(humidity),1))
   table_name <- paste0(deparse(substitute(folder_name)), "_minute_averages")
   assign(table_name, temp_humidity_heat_index_minute, envir = parent.frame())

   ######################
   #### Build graphs ####
   ######################

   # Build monthly graph temperature
   chart_title <- paste0(deparse(substitute(folder_name)), " mean temperature by month")
   temp_month_graph <- ggplot(data = temp_humidity_heat_index_month, aes(x = month, y = mean_temp)) +
      geom_bar(stat="identity") +
      ggtitle(chart_title) +
      xlab("Month Number") +
      ylab("Mean Temperature")
   plot(temp_month_graph)
   filename <- paste0(deparse(substitute(folder_name)),"_", Sys.Date(), "_temp_means_month.pdf")
   filepath <- paste0(getwd(), "/", deparse(substitute(folder_name)))
   ggsave(filename, plot = temp_month_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)

   # Build daily graph temperature
   chart_title <- paste0(deparse(substitute(folder_name)), " mean temperature by day")
   temp_day_graph <- ggplot() +
      geom_line(data = temp_humidity_heat_index_day, aes(x = day, y = mean_temp, colour="real temp")) +
      geom_line(data = temp_humidity_heat_index_day, aes(x = day, y = mean_heat_index, colour="heat index")) +
      scale_color_manual(
         values = c('real temp' = 'red',
                    'heat index' = 'purple')
      ) +
      ggtitle(chart_title) +
      xlab("Days") +
      ylab("Means") +
      scale_x_datetime(date_labels = "%b %d %H", date_breaks = "1 day")
   plot(temp_day_graph)
   filename <- paste0(deparse(substitute(folder_name)),"_", Sys.Date(), "_temp_means_day.pdf")
   filepath <- paste0(getwd(), "/", deparse(substitute(folder_name)))
   ggsave(filename, plot = temp_day_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)

   # Build hourly graph temperature
   chart_title <- paste0(deparse(substitute(folder_name)), " mean temperature by hour")
   temp_hour_graph <- ggplot(data = temp_humidity_heat_index_hour, aes(x = hour, y = mean_temp)) +
      geom_line() +
      ggtitle(chart_title) +
      xlab("Hours") +
      ylab("Mean Temperature") +
      scale_x_datetime(date_labels = "%b %d %H", date_breaks = "1 day") +
      theme(axis.text.x = element_text(angle=50,hjust=1))
   plot(temp_hour_graph)
   filename <- paste0(deparse(substitute(folder_name)),"_", Sys.Date(), "_temp_means_hour.pdf")
   filepath <- paste0(getwd(), "/", deparse(substitute(folder_name)))
   ggsave(filename, plot = temp_hour_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)

   # Build minute graph temperature
   chart_title <- paste0(deparse(substitute(folder_name)), " mean temperature by minute")
   temp_minute_graph <- ggplot(data =  temp_humidity_heat_index_minute, aes(x = minute, y = mean_temp)) +
      geom_line() +
      ggtitle(chart_title) +
      xlab("Hours") +
      ylab("Mean Temperature") +
      scale_x_datetime(date_labels = "%b %d %H", date_breaks = "1 day") +
      theme(axis.text.x = element_text(angle=50,hjust=1))
   plot(temp_minute_graph)
   filename <- paste0(deparse(substitute(folder_name)),"_", Sys.Date(), "_temp_means_minute.pdf")
   filepath <- paste0(getwd(), "/", deparse(substitute(folder_name)))
   ggsave(filename, plot = temp_minute_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)

}

