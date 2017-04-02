Reproductible Research Course Project 1
=======================================

#R Markdown

##Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit,
 Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – 
 a group of enthusiasts who take measurements about themselves regularly to improve their health, 
 to find patterns in their behavior, or because they are tech geeks.
  But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.
The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data 
The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as 𝙽𝙰)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken



##Objective

The objective of this project is to deliver an R markdown with:

1) Code for reading in the dataset and/or processing the data
2) Histogram of the total number of steps taken each day
3) Mean and median number of steps taken each day
4) Time series plot of the average number of steps taken
5) The 5-minute interval that, on average, contains the maximum number of steps
6) Code to describe and show a strategy for imputing missing data
7) Histogram of the total number of steps taken each day after missing values are imputed
8) Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
9) All of the R code needed to reproduce the results (numbers, plots, etc.) in the report


##Delivery
The delivery include
1) c5_w2_Course_Project.Rmd: R markdown script.Set the source folder properly in the setwd() sentence
in order to locate the date.
2) activity.csv: raw data to analyze
3) c5_w2_Course_Project.html: knit html output.
