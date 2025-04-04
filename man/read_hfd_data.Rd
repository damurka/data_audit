% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_load_data.R
\name{read_hfd_data}
\alias{read_hfd_data}
\title{Read and Clean HFD Data from Excel}
\usage{
read_hfd_data(path)
}
\arguments{
\item{path}{A string specifying the file path to the Excel file.}
}
\value{
A named list (classed as `hfd_data`) of tibble objects, each
  representing a cleaned version of one sheet in the Excel file.
}
\description{
Reads multiple sheets from a Health Facility Data (HFD) Excel file, cleans
column names, removes unnecessary rows, converts relevant columns to numeric,
and ensures required columns are present.
}
\details{
This function expects the following sheets in the Excel file:

- **Admin_data**
- **Population_data**
- **Reporting_completeness**
- **Service_data_1**
- **Service_data_2**
- **Service_data_3**

Column names are converted to lower case, spaces removed, and default names
(like `...1`, `...2`) are cleaned up. The first two rows of each sheet are
typically header rows, so they are removed. Columns that are irrelevant for
downstream analysis are dropped, and remaining columns are coerced to numeric
where appropriate.
}
