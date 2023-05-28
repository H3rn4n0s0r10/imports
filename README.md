# Imports Dashboard

This repository contains a Shiny dashboard application for analyzing import data. The dashboard allows users to search for import information based on a specific tariff code or import identification number (NIT). It provides various visualizations and data tables to explore import data and market trends.

## Features

- **Code Arancelario Search**: Enter a tariff code to retrieve import data associated with that code.
- **NIT Search**: Enter an import identification number (NIT) to retrieve import data associated with that number.
- **Data Visualization**: Visualize market data with interactive charts and maps.
- **Data Tables**: Explore detailed import data through sortable and searchable tables.
- **Export Data**: Export filtered data to a CSV file.

## Prerequisites

To run the Imports Dashboard locally, you need to have the following software installed:

- R (version 3.5.0 or higher)
- RStudio (optional but recommended)

## Installation

1. Clone the repository to your local machine using the following command:

2. Open RStudio and set the working directory to the root folder of the cloned repository.

3. Install the required R packages by running the following command in the RStudio console:
```R
install.packages(c("shiny", "tidyverse", "dplyr", "shinydashboard", "kableExtra", "DT", "leaflet", "leaflet.extras", "rworldxtra", "raster", "sf"))
Usage
Run the app.R file in RStudio.

The Imports Dashboard will open in your default web browser.

Use the sidebar menu to navigate between different tabs and search for import data.

Enter a tariff code or NIT in the search bar and click the search button to retrieve relevant import data.

Explore the various visualizations and data tables available in each tab to gain insights into import trends and market information.

Use the export button to download the filtered data as a CSV file.

Contributing
Contributions are welcome! If you find any issues or have suggestions for improvements, please open an issue or submit a pull request.

License
This project is licensed under the MIT License.

Acknowledgements
The shiny package for building interactive web applications in R.
The tidyverse suite of R packages for data manipulation and visualization.
The leaflet package for interactive maps.
The rworldxtra package for additional geographical data.
The sf package for working with spatial data.
