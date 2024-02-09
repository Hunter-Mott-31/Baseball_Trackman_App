# [CWL Analytics Shiny App](https://analytical-lion.shinyapps.io/cwl_analytics/)

## Overview
This Shiny app provides statistical analysis and visualizations for baseball players in the California Winter League (CWL) based on Trackman data from the 2024 season.
It is tailored for players participating in the CWL who wish to track and compare their performance metrics, aiding in player development and potential scouting opportunities.

## Features
- Pitcher Tab: Explore average pitcher velocity, spin rates, pitch movement, pitch release, and a data table of overall stats based on pitch type.
- Hitter Tab: Compare various statistics for hitters, including exit velocity, launch angle, and distance hit.
- About: Information about the purpose of the app, Trackman resources, and my contact details.

## Data Source
The data used in this app was collected from CWL games using Trackman's system. Ensure you choose the necessary player type filters for accurate data.

Data Files: Use your Raw Trackman .csv file(s)

## How to Use
- Choose the "Pitcher" tab to explore pitcher visualizations/statistics by looking up the Pitcher's name.
- Choose to the "Hitter" tab to explore hitter visualization/statistics by looking up the Hitter's name. 

## Installation
To run the app locally, follow these steps:

1. Clone this repository to your local machine and upload your own Trackman Data into it.
2. Install the required R packages using `install.packages(c("shiny", "shinythemes", "tidyverse", "DT"))`.
3. Load in `CWL_Analytics_App.R`.
4. Run the app.

## Contact
Feel free to reach out if you have any comments, recommendations, or concerns about the app.

LinkedIn: [Hunter Mott](https://www.linkedin.com/in/hunter-mott/)
Email: [h-mott@outlook.com](mailto:h-mott@outlook.com)
Twitter: [Analytical Lion](https://twitter.com/Analytical_Lion)

## License
This project is licensed under the MIT License.
