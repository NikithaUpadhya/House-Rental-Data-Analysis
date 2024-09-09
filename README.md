# House-Rental-Data-Analysis
# House Rental Dataset Analysis
This repository contains an R script for analyzing a house rental dataset. The analysis includes data cleaning, exploration, and visualization to derive insights into rental patterns across different cities in India. The script utilizes various data analysis libraries and covers essential exploratory data analysis (EDA) tasks, data cleaning, and visualization.

# Dataset
The dataset used for this analysis includes the following columns:

- Date_Posted: Date the rental listing was posted.
- rooms: Number of rooms in the rental property.
- rent_fee: Monthly rent for the property.
- sqfeet: Size of the property in square feet.
- levels: Number of levels/floors in the building.
- area: The locality or region within the city.
- locale: Detailed address of the property.
- city_india: The city where the rental property is located.
- furnishing: Furnishing status (furnished/unfurnished).
- tenant: Preferred tenant type (e.g., family, bachelor).
- washrooms: Number of washrooms in the rental property.
- contact_person: Name of the contact person for the rental listing.


# Key Steps in the Analysis 
1. Data Import & Preparation
The dataset is imported using the read.csv() function.
The column names are renamed for consistency and ease of reference.
The dataset is briefly explored by viewing its structure, dimensions, and summary statistics.

2. Data Cleaning
Missing values are checked using sum(is.na()) to ensure data integrity.
Duplicate entries are identified and removed using the !duplicated() function to avoid redundant data points.
Data types for columns such as rooms and rent_fee are verified to ensure consistency.

3. Data Exploration & Visualization
The script uses popular R packages such as ggplot2, plotrix, tidyverse, and lessR for visualization and data manipulation.
A range of plots is created to visualize trends in rental prices, such as:
Distribution of rent fees across different room sizes.
Analysis of rent in relation to the furnishing status of the properties.
City-wise comparison of rental rates.

4. Pre-processing
The dataset is pre-processed to remove irrelevant or inconsistent data, ensuring that the final analysis is accurate and representative.

5. Summary Statistics
The dataset is summarized using basic statistical functions to get a quick overview of the trends in rental fees, property sizes, and city-wise distribution.
Required Packages

# The following R libraries are used in the script:

- ggplot2: For data visualization and creating aesthetic graphs.
- plotrix: For additional plotting functions.
- lessR: Simplifies some commonly used data manipulation and visualization tasks.
- tidyverse: A collection of packages for data manipulation and visualization, including dplyr, ggplot2, and more.


# How to Run
- Clone the repository.
- Ensure that the dataset file House_Rent_Dataset.csv is available in your working directory.
- Run the script House_Rental_Dataset_Analysis_Nikitha.R in your preferred R environment.
