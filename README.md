# UoM-ERP

This repository accompanies my extended research project for the University of Manchester titled **"Title"**.

The aim of this project is to assess the impact of the **American Civil Liberties Union (ACLU)** of Illinois' settlement with the Chicago Police Department regarding **stop-and-frisk practices** on the total number of force incidents in Chicago, specifically those classified as **high-intensity force incidents**.

## Context

The effect of the ACLU period is interwined with the effect of other interventions such as the NYPD lawsuit took place in 2012-06-17, and the police investigation 2017-01-01. In order to assess the impact of the ACLU period, multiple effect is assessed to provide an overview of the impact:
- The NYPD lawsuit period effect.
- The combined effect of the NYPD lawsuit period and the ACLU period.
- The additional effect of the ACLU period after the NYPD lawsuit period.

## Data

The data for this project was provided by **[insert data source]**. It includes the records of police force incidents in Chicago from 2004 till 2020. 

## Environment

All work is done in **RStudio version 4.1**. The following R packages are essential to this project:

- forecast                                                                   (Time series forecasting with ARIMA models)
- HonestDiD, url: https://github.com/asheshrambachan/HonestDiD/tree/master   (Difference in Difference analysis)

## Repository Structure

There are two main RScripts in this repository:

1. **Data Aggregation.R**  
    This RScript prepares the data for further analyses:
     - Classification of force incidents to low-intensity and high-intensity by the action taken during the incident.
     -
   From the different "resistance type", **Active Resister** is the only resistance type with both low and high force incidents after classification, and this shift the focus only onto active resisters where variance presents.
3. **Modelling.R**  
   This RScript contains all the analysis performed on the high force incidents on active resisters:
    - Naive mean model on the monthly conditional probability of high force incidents on active resisters.
    - ARIMA counterfactual forecasting and comparing the monthly conditional probability of high force incidents on active resisters.
    - Difference in Difference analysis on the yearly total number of high force incidents on active resisters.

The time window aggregatation by month and year simplifies the analysis process, and reduce the complexity in temporal dependency that is required for appropriate modelling, especially in difference in differences.
