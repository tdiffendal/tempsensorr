% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function.R
\name{process_sensors_excel_version}
\alias{process_sensors_excel_version}
\title{Sensor Data Excel}
\usage{
process_sensors_excel_version(folder_name)
}
\arguments{
\item{folder_name}{what you want to call the folder where the excel workbook and graphs will save}
}
\value{
Excel sheets with monthly, dayly, hourly and minute by minute average temperatures and heat index as well as graphs plotting the same information
}
\description{
This function allows you to input your temperature data from the sensor and analyze it for monthly, dayly, hourly and minute by minute average temperatures and heat index in addition to creating graphs. The output is an excel file with multiple sheets. When you run the function, you will be prompted to choose which csv you want to analyze.
}
