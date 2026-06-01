# flu-metrocast-shiny

An interactive dashboard for exploring forecast performance metrics across geographic regions and forecasting horizons.

## Live Dashboard

Dashboard: https://dongahkim.shinyapps.io/flu-metrocast-shiny/

## Overview

This dashboard was developed to investigate spatial variation in influenza forecasting performance, particularly weighted interval score (WIS), across locations and forecast horizons.

The application allows users to:

* Explore WIS across geographic regions
* Compare forecasting performance across locations
* Visualize forecast accuracy and uncertainty
* Examine temporal changes in forecast performance
* Access historical influenza surveillance data

## Purpose

Forecast performance can vary substantially across regions due to differences in population size, surveillance quality, reporting variability, and epidemic dynamics.

This dashboard was designed to support exploratory analysis of:

* Regional heterogeneity in forecasting performance
* Local versus aggregated forecasting accuracy
* Spatial patterns in forecast uncertainty
* Forecast evaluation across multiple horizons

## Data Updates

Forecast data are automatically updated through GitHub Actions workflows and synchronized with the dashboard.

## Technologies

* R Shiny
* ggplot2
* dplyr
* GitHub Actions
* Automated data pipelines

## Source Code

GitHub Repository:
https://github.com/donga0223/flu-metrocast-shiny

