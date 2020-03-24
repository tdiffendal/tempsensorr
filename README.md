# tempsensorr
tempsensorr package

The Howard Center for Investigative Journalism Code Red Temperature and Humidity Sensors

This R Markdown was created by [The Howard Center for Investigative Journalism](https://merrill.umd.edu/about-merrill/signature-programs/the-howard-center-for-investigative-journalism/) at the University of Maryland's Philip Merrill College of Journalism for the purpose of providing a guide which other organizations can use to build their own temperature sensors. These sensors were used in the Howard Center's recent project, [Code Red](https://cnsmaryland.org/interactives/summer-2019/code-red/index.html), to examine how heat unequally affects residents of Baltimore. 

We found that neighborhoods with greater people of color or low-income populations are significantly hotter than surrounding richer, whiter and more rural neighborhoods. Heat exacerbates a range of health issues such as COPD and asthma, and the heated difference is also not an accident. The hotter communities were historically excluded from receiving loans due to red-lining, a process which determined the risk of investing in an area based significantly on race.

We built temperature and humidity sensors, using this ["Harlem Heat" project guide](https://github.com/datanews/harlem-heat) created by WNYC public radio for their [Harlem Heat Project](https://www.wnyc.org/series/harlem-heat-project). We placed them in several homes in Baltimore in summer 2019 and joined them with outdoor temperature to examine how heat index varied indoors and out.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

These are the R libraries used for the code and which must be installed and loaded prior to running the function.

```{r}

#### Load Packages ####
library(tidyverse)
library(janitor)
library(lubridate)
library(openxlsx)
#library(readxl)
library(weathermetrics)

# For debugging rm(list=ls())

```

## Sensor Building

## Excel Version

The purpose of the following section of code is to load the data from the temperature sensors and output an Excel file with accompanying graphs. 

The code as written as the function process_sensors()Running the code altogether will produce four Excel worksheets with the average temperature, humidity and heat index values by month, day, hour and minute. Four graphs will also be created, plotting the same data as in the Excel sheets (temperature, humidity, and heat index values by month, day, hour and minute).

#### Build the function to ingest, clean, analyze, build graphs, and write out Excel files and graphs. 

```{r}
process_sensors_excel_version <- function(folder_name) {
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
   temp_month_graph <- ggplot2::ggplot(data = temp_humidity_heat_index_month, aes(x = month, y = mean_temp)) +
      geom_bar(stat="identity") +
      ggtitle(chart_title) +
      xlab("Month Number") +
      ylab("Mean Temperature")
   plot(temp_month_graph)
   filename <- paste0(deparse(substitute(folder_name)),"_", Sys.Date(), "_temp_means_month.pdf")
   filepath <- paste0(getwd(), "/", deparse(substitute(folder_name)),"/")
   ggplot2::ggsave(filename, plot = temp_month_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)

   # Build daily graph temperature
   chart_title <- paste0(deparse(substitute(folder_name)), " mean temperature by day")
   temp_day_graph <- ggplot2::ggplot() +
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
   ggplot2::ggsave(filename, plot = temp_day_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)

   # Build hourly graph temperature
   chart_title <- paste0(deparse(substitute(folder_name)), " mean temperature by hour")
   temp_hour_graph <- ggplot2::ggplot(data = temp_humidity_heat_index_hour, aes(x = hour, y = mean_temp)) +
      geom_line() +
      ggtitle(chart_title) +
      xlab("Hours") +
      ylab("Mean Temperature") +
      scale_x_datetime(date_labels = "%b %d %H", date_breaks = "1 day") +
      theme(axis.text.x = element_text(angle=50,hjust=1))
   plot(temp_hour_graph)
   filename <- paste0(deparse(substitute(folder_name)),"_", Sys.Date(), "_temp_means_hour.pdf")
   filepath <- paste0(getwd(), "/", deparse(substitute(folder_name)),"/")
   ggplot2::ggsave(filename, plot = temp_hour_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)

   # Build minute graph temperature
   chart_title <- paste0(deparse(substitute(folder_name)), " mean temperature by minute")
   temp_minute_graph <- ggplot2::ggplot(data =  temp_humidity_heat_index_minute, aes(x = minute, y = mean_temp)) +
      geom_line() +
      ggtitle(chart_title) +
      xlab("Hours") +
      ylab("Mean Temperature") +
      scale_x_datetime(date_labels = "%b %d %H", date_breaks = "1 day") +
      theme(axis.text.x = element_text(angle=50,hjust=1))
   plot(temp_minute_graph)
   filename <- paste0(deparse(substitute(folder_name)),"_", Sys.Date(), "_temp_means_minute.pdf")
   filepath <- paste0(getwd(), "/", deparse(substitute(folder_name)),"/")
   ggplot2::ggsave(filename, plot = temp_minute_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)

}

#### Excecute the function process_sensors, with the object name we just built
process_sensors_excel_version(Stephanie)

```

## R Version

For those who want to manipulate the data, the following section of code creates R dataframes and graphs which users can then manipulate for their own analysis

### Processing Function

```{r}

#######################################
##### Sensor Processing Function ######
#######################################
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
   
    # Create folder in working directory
   folder_path <- paste0(getwd(), "/", deparse(substitute(folder_name)))
   dir.create(path = folder_path)

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
   filepath <- paste0(deparse(substitute(folder_name)), "/")
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
   filepath <- paste0(deparse(substitute(folder_name)),"/")
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
   filepath <- paste0(deparse(substitute(folder_name)),"/")
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
   filepath <- paste0(deparse(substitute(folder_name)),"/")
   ggsave(filename, plot = temp_minute_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)

}

process_sensors_r_version(Stephanie)
```


## Compare to External Temperature Data

We compared the temperature data collected by our sensors to the outdoor temperature to show differences between the two and further illustrate how temperature indoors in Baltimore City can often be greater than outdoors. We obtained temperature data from [Iowa State University's Environmental Mesonet](https://mesonet.agron.iastate.edu/request/download.phtml?network=MD_ASOS). 

To calculate differences between outside temperatures in your region and the data from your sensors, go to the link above, select your state network and chose a station location near to where your sensors collected data. Many different data are offered, but we selected Air Temperature [F], Dew Point [F], Relative Humidity [%], and Heat Index/Wind Chill [F]. Choose your start and end dates, timezone, data format, and change "View result data in web browser" to "Save result data to file on computer". Finally, click "Get Data".

The code below loads and cleans the outside temperature data before comparing it to the sensor data.

```{r}

# Load external temperature data 
outside_data <- read_csv(file.choose())

# Clean data 
outside_data <- outside_data  %>%
  select(-relh) %>%
  filter(tmpf != 'M', dwpf != 'M') %>%
  mutate(tmpf = as.numeric(tmpf),
         dwpf = as.numeric(dwpf)) %>%
  mutate(date = as.Date(valid, format="%Y-%m-%d")) %>%
  mutate(year=year(valid)) %>%
  mutate(month=month(valid)) %>%
  mutate(day=day(valid)) %>%
  mutate(hour=hour(valid)) %>%
  distinct(valid, .keep_all = TRUE) %>%
  mutate(heat_index = heat.index(t=tmpf, dp=dwpf, temperature.metric = "fahrenheit", round=0)) %>%
  mutate(relative_humidity = dewpoint.to.humidity(dp = dwpf, t = tmpf, temperature.metric = "fahrenheit")) %>%
  select(date, year, month, day, hour, tmpf, dwpf, relative_humidity, heat_index) %>%
  group_by(date, year, month, day, hour) %>%
  summarise(avg_hourly_temperature_outside = mean(tmpf),
            avg_hourly_dewpoint_outside = mean(dwpf),
            avg_hourly_relative_humidity_outside = mean(relative_humidity),
            avg_hourly_heat_index_outside = mean(heat_index)
  ) %>%
  distinct()

##Process the sensor data and produce an Excel workbook with 4 sheets and graphs
process_sensor_and_outside_data <- function(person_location) {
  # parse date objects and remove unneeded columns
  temp <- person_location %>%  
    mutate(date = date(datetime)) %>%
    mutate(year = year(datetime)) %>%
    mutate(month = month(datetime)) %>%
    mutate(day = day(datetime)) %>%
    mutate(hour = hour(datetime)) %>%
    mutate(minute = minute(datetime)) %>%
    select(datetime, date, year, month, day, hour, minute, temperature, relative_humidity) %>%
    mutate(heat_index = heat.index(t=temperature, rh=relative_humidity, temperature.metric = "fahrenheit")) %>%
    left_join(outside_data, by = c("year", "month", "day", "hour")) %>%
    select(-contains(".y"), -contains("dew")) %>%
    rename(date = `date.x`) %>%
    mutate(indoor_temperature_difference = temperature - avg_hourly_temperature_outside,
           indoor_heat_index_difference = heat_index - avg_hourly_heat_index_outside)
  
  # calculate average temperature by day for the sensor, and store it as temporary object 
  temp_humidity_heat_index_day <- temp %>%
    group_by(date) %>%
    summarise(mean_indoor_temperature = round(mean(temperature),1),
              mean_indoor_heat_index = round(mean(heat_index),1),
              mean_indoor_relative_humidity = round(mean(relative_humidity),1),
              mean_outdoor_temperature = round(mean(avg_hourly_temperature_outside),1),
              mean_outdoor_heat_index = round(mean(avg_hourly_heat_index_outside),1),
              indoor_temperature_difference = mean_indoor_temperature - mean_outdoor_temperature,
              indoor_heat_index_difference = mean_indoor_heat_index - mean_outdoor_heat_index)
    table_name <- paste0(deparse(substitute(person_location)), "_day_averages")
    assign(table_name, temp_humidity_heat_index_day, envir = parent.frame())
    
    # calculate average temperature by day and hour for the sensor, and store it as temporary object 
    temp_humidity_heat_index_day_hour <- temp %>%
      mutate(date_hour = make_datetime(year = year, month = month, day = day, hour = hour)) %>%
      group_by(date_hour) %>%
      summarise(mean_indoor_temperature = round(mean(temperature),1),
                mean_indoor_heat_index = round(mean(heat_index),1),
                mean_indoor_relative_humidity = round(mean(relative_humidity),1),
                mean_outdoor_temperature = round(mean(avg_hourly_temperature_outside),1),
                mean_outdoor_heat_index = round(mean(avg_hourly_heat_index_outside),1),
                indoor_temperature_difference = mean_indoor_temperature - mean_outdoor_temperature,
                indoor_heat_index_difference = mean_indoor_heat_index - mean_outdoor_heat_index)
      table_name <- paste0(deparse(substitute(person_location)), "_day_hourly_averages")
      assign(table_name, temp_humidity_heat_index_day_hour, envir = parent.frame())
    
    # calculate average temperature by minute for the sensor, and store it as temporary object
      # Note: because temperature collection is done at intervals that are sometimes greater than a minute, won't have          exactly one value per minute.
    temp_humidity_heat_index_day_minute <- temp %>%
    mutate(date_hour_minute = make_datetime(year = year, month = month, day = day, hour = hour, min = minute)) %>%
    group_by(date_hour_minute) %>%
    summarise(mean_indoor_temperature = round(mean(temperature),1),
                mean_indoor_heat_index = round(mean(heat_index),1),
                mean_indoor_relative_humidity = round(mean(relative_humidity),1),
                mean_outdoor_temperature = round(mean(avg_hourly_temperature_outside),1),
                mean_outdoor_heat_index = round(mean(avg_hourly_heat_index_outside),1),
                indoor_temperature_difference = mean_indoor_temperature - mean_outdoor_temperature,
                indoor_heat_index_difference = mean_indoor_heat_index - mean_outdoor_heat_index)
      table_name <- paste0(deparse(substitute(person_location)), "_day_minute_averages")
      assign(table_name, temp_humidity_heat_index_day_minute, envir = parent.frame())
    
    ######################
    #### Build graphs ####
    ######################
    
    # Build monthly graph temperature
    chart_title <- paste0(deparse(substitute(person_location)), " mean temperature by month")
    temp_month_graph <- ggplot2::ggplot(data = temp_humidity_heat_index_month, aes(x = month, y = mean_temp)) +
      geom_bar(stat="identity") +
      ggtitle(chart_title) +
      xlab("Month Number") +
      ylab("Mean Temperature")
    plot(temp_month_graph)
    filename <- paste0(deparse(substitute(person_location)),"_", date(now()), "_temp_means_month.pdf")
    filepath <- paste0("output_files/", deparse(substitute(person_location)),"/")
    ggplot2::ggsave(filename, plot = temp_month_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)
    
    # Build daily graph temperature
    chart_title <- paste0(deparse(substitute(person_location)), " mean temperature by day")
    temp_day_graph <- ggplot2::ggplot() +
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
    filename <- paste0(deparse(substitute(person_location)),"_", date(now()), "_temp_means_day.pdf")
    filepath <- paste0("output_files/", deparse(substitute(person_location)),"/")
    ggplot2::ggsave(filename, plot = temp_day_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)
    
    # Build hourly graph temperature
    chart_title <- paste0(deparse(substitute(person_location)), " mean temperature by hour")
    temp_hour_graph <- ggplot2::ggplot(data = temp_humidity_heat_index_hour, aes(x = hour, y = mean_temp)) +
      geom_line() +
      ggtitle(chart_title) +
      xlab("Hours") +
      ylab("Mean Temperature") + 
      scale_x_datetime(date_labels = "%b %d %H", date_breaks = "1 day") + 
      theme(axis.text.x = element_text(angle=50,hjust=1))
    plot(temp_hour_graph)
    filename <- paste0(deparse(substitute(person_location)),"_", date(now()), "_temp_means_hour.pdf")
    filepath <- paste0("output_files/", deparse(substitute(person_location)),"/")
    ggplot2::ggsave(filename, plot = temp_hour_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)
   
    # Build minute graph temperature
    chart_title <- paste0(deparse(substitute(person_location)), " mean temperature by minute")
    temp_minute_graph <- ggplot2::ggplot(data =  temp_humidity_heat_index_minute, aes(x = minute, y = mean_temp)) +
      geom_line() +
      ggtitle(chart_title) +
      xlab("Hours") +
      ylab("Mean Temperature") + 
      scale_x_datetime(date_labels = "%b %d %H", date_breaks = "1 day") + 
      theme(axis.text.x = element_text(angle=50,hjust=1))
    plot(temp_minute_graph)
    filename <- paste0(deparse(substitute(person_location)),"_", date(now()), "_temp_means_minute.pdf")
    filepath <- paste0("output_files/", deparse(substitute(person_location)),"/")
    ggplot2::ggsave(filename, plot = temp_minute_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)
    
    
}  

process_sensor_and_outside_data(person_location)
```
