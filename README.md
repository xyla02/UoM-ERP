# UoM-ERP

This repository accompanies my extended research project for the University of Manchester titled **"Title"**.

## Context

The effectiveness of the 2019 Consent Decree for the Chicago Police Department (CPD) has been widely debated, with some suggesting that earlier reforms may explain the limited observed changes. From the data, it can be seen that passive resisters always receive low force, and assailants always receive high force. However, active resisters can receive either. When observing the conditional probability of an active resister receiving high force over time, there is a disruption in the trend exhibited from 2005 to 2011. In 2012, it begins to decline. This was during the time when the New York City Department (NYPD)'s controversial Stop and Frisk Policy was under heavy scrunity, which then later resulted in a lawsuit.

Interrupted Time Series Analysis with ARIMA and Difference in Differences models were implemented to evaluate whether this decline was significant or not. From the previous literature, the period when the ACLU released the report regarding CPD excessive stop and searches, there is evidence to suggest that an additional "downwards" effect might occur.

This code allows one to reproduce the results found.

## Data

The data for this project was provided by **[insert data source]**. It includes the records of police force incidents in Chicago from 2004 till 2020. 

## Environment

All work is done in **RStudio version 4.1**. The following R packages are essential to this project:

- forecast                                                                   (Time series forecasting with ARIMA models)
- HonestDiD, url: https://github.com/asheshrambachan/HonestDiD/tree/master   (Difference in Difference analysis)

## Repository Structure

There are two main RScripts in this repository:

1. **Data Pre-Processing.R**  
    This RScript prepares the data for further analyses and must be run before all the other scripts.
     -Filtering actions such that only member actions are in the dataset
     - Classification of force incidents to low-intensity and high-intensity by the action taken during the incident.
     - Removal of null values
2. **EDA**
   This contains all the exploratory data analysis prior to the two models.
    - Time series decomposition
    - High/low force counts by resistance type
    - Plot of inital high/low force counts solely for active resisters
4. **Modelling.R**  
   This RScript contains all the analysis performed on the high force incidents on active resisters:
    - Naive mean model on the monthly conditional probability of high force incidents on active resisters.
    - ARIMA counterfactual forecasting and comparing the monthly conditional probability of high force incidents on active resisters.
    - Difference in Difference analysis on the yearly total number of high force incidents on active resisters.

The time window aggregatation by month and year simplifies the analysis process, and reduce the complexity in temporal dependency that is required for appropriate modelling, especially in difference in differences.
