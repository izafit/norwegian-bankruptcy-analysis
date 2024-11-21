# norwegian-bankruptcy-analysis
Analysis of bankruptcy, employment, and revenue in Norwegian companies.

This repository contains an analysis of bankruptcy, employment, and revenue data for Norwegian companies over a set period. The analysis focuses on relationships between bankruptcy events, revenue, and employment within different organizational sectors.

## Overview
The goal of this analysis is to investigate how revenue (Omsetning) and employment (Sysselsette) are related to bankruptcy events in Norway. Key insights are drawn from correlations, histograms, and basic descriptive statistics. The datasets used for the analysis are as follows:
1. K_konkurser.xlsx: Contains bankruptcy data for various Norwegian companies.
2. K_sysselsettnings.xlsx: Contains employment data by quarter for Norwegian companies.
3. K_organisasjon.xlsx: Contains organizational data for various Norwegian companies.

The data used in this analysis was sourced from the official Statistics Norway (SSB) website (www.ssb.no).

The analysis involves merging datasets based on the Kvartal (quarter) column, cleaning the data (e.g., converting columns to numeric values), and exploring relationships between different variables through correlation analysis and visualizations.

## Setup
To run the code, you need to install and load the following R packages:

dplyr: For data manipulation
readxl: For reading Excel files
corrplot: For correlation matrix visualization
viridis: For color palettes in visualizations

## Analysis Steps
- Data Loading: The data is loaded from Excel files using readxl::read_excel().
- Data Cleaning: The Kvartal column (quarter) is transformed into a Date format for easier analysis.
- Data Merging: The data from the employment and organizational datasets are merged into one dataset (syssel_organisasjon) based on the Kvartal column.
- Data Conversion: Certain columns are converted to numeric for proper analysis.
- Correlation Analysis: A correlation matrix is calculated for Omsetning (Revenue) and Sysselsette (Employment) to explore their relationship.
- Visualization: Histograms are plotted to visualize the distribution of revenue and employment across the dataset.
- Data Filtering: The dataset is filtered to exclude rows with non-positive values for Omsetning (revenue) and Sysselsette (employment).

## Results
- Correlation Analysis: The correlation matrix shows the relationship between revenue and employment, helping to understand how these factors interact with each other.
- Histograms: The histograms visualize the distribution of revenue and employment across the dataset. The revenue histogram shows how many companies report various levels of revenue, while the employment histogram reveals the number of employees in these companies.
- Descriptive Statistics: Basic statistics of filtered data provide insights into the central tendency and spread of revenue and employment.

## Conclusion
The analysis reveals significant relationships between revenue and employment within Norwegian companies, as well as some insights into the distribution of these variables over time. Further analysis could involve more granular statistical models or predictions based on the data.

## Future Work
1. Implement more advanced statistical modeling (e.g., regression analysis) to predict bankruptcy risk based on employment and revenue.
2. Expand the dataset to include additional factors such as sector type, company age, and geographical location.
3. Enhance data visualization to include time-series trends and sector-specific analysis.
