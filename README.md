# Exploring the Relationship Between Weather and Cycling in Munich

This repository contains an R-based data visualization project exploring how weather conditions—specifically sunshine and rainfall—influence cycling behavior in Munich.

## Project Overview

Munich has a growing cycling culture, but how local usage patterns shift under varying weather conditions is a key factor for urban mobility planning. This project analyzes longitudinal data from 2011 to 2025 to quantify the robustness of cycling activity using a custom "Resilience Index".

### Research Question

> "How do variations in weather conditions influence daily cycling trips in Munich, and how can we quantify the robustness of cycling activity under changing weather?" 
> 
> 

## Data Source & Preparation

The data is sourced from **Open Data Munich (Münchener Raddauerzählstellen)** and consists of daily records from six counting stations:

* Rudolf-Harbig-Weg (Olympiapark) 


* Birketweg (Hirschgarten) 


* Arnulfstraße 


* Erhardtstraße (Deutsches Museum) 


* Margaretenstraße 


* Bad-Kreuther-Straße 



### Data Cleaning Process

The raw data presented challenges such as inconsistent date formats, syntax irregularities in station names, and schema drifts over several years. The cleaning strategy included:

* **Standardization**: Unified column naming and date parsing.


* **Integration**: Merging isolated annual CSV files into a single longitudinal dataset.


* **Aggregation**: Converting daily records into yearly metrics for trend analysis.



## Repository Structure

* `Dataset.R`: Loads, cleans, and merges the yearly CSV datasets from the `Yearly Datasets/` directory.
* `Defect Stations.R`: Generates a heatmap visualization to identify "Out of Order" vs. "Working" periods for each counting station to ensure data quality.
* `Sunshine Hours.R`: Visualizes the correlation between daily sunshine (binned in 2-hour increments) and average daily trips.
* `Resilience Index.R`: Calculates the core metric comparing average trips on rainy days vs. sunny days to measure "weather-proof" cycling trends.

## Key Methodology: The Resilience Index

The **Resilience Index** is defined as the ratio of average trips on rainy days to average trips on sunny days.

* **Rainy Day**: Days with >2mm of precipitation.


* **Sunny Day**: Days with >6 hours of sunshine and zero rain.

**How to interpret the value**:
* **= 1.0** (Same average trips on rainy and sunny days)
 
* **< 1.0** (Fewer average trips on rainy days)
 
* **> 1.0** (More average trips on rainy days)

## Key Findings

* **Sunshine Dependency**: Daily cycling volume nearly triples (from ~6k to ~19k trips) as sunshine hours increase.


* **Rising Resilience**: Since 2020, Munich's cyclists have become more "weather-proof." The Resilience Index reached "Above Average" levels in 2021 and 2022 (approx. 65–70%), indicating that a higher percentage of cyclists now continue to ride even on rainy days.



## Team

Developed by:

* Daniel Bader 


* Johannes Büschel 


* Dominik Moirano 



Technical University of Munich (TUM) | Hochschule für Politik München | Chair of Computational Social Science 

---

**AI Clarification:** AI was used to leverage ideas and speed up the initial coding framework and plot creation. All data collection decisions, preprocessing, analytical steps, and final insights were developed entirely by the project team.
