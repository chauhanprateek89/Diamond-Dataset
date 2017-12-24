# EDA and Hypothesis testing on Diamonds dataset

Using R as the preferred choice of language extensive EDA was performed on the Diamonds data set, which can be found in 'ggplot2' package.  
The concepts of data wrangling and data visualization were implemented using packages such as: 
	- plotly : creates interactive visualizations
	- GGally : extends 'ggplot2' by adding several functions to reduce the complexity of combining geometric objects with transformed data.
	- GridExtra : arranges multiple grid-based plots on a page, and draws tables.
	- scales : map data to aesthetics, and provide methods for automatically determining breaks and labels for axes and legends.
	- memisc : provides functionality to produce tables and data frames of arbitrary descriptive statistics and (almost) publication-ready tables.
	- lattice : data visualization tool for multivariate data set.
	- dplyr : fast, consistent tool for working with data frame like objects.
	- tidyR : designed specifically for data tidying (not general reshaping or aggregating) and works well with 'dplyr' data pipelines.

After extracting insights from the data set and creating a report of descriptive analytics using R markdown files, hypothesis testing was performed on the data set.  
Five different hypothesis were formed for different variables to implement the concepts of statistical inference.  
The tests performed to determine the correct hypothesis were:
	- z-test
	- 2-sample T-test
	- Paired T-test
	- Chi-squared test
	- ANOVA
	- Tukey's Honest Significance Difference Test
	- Cremer's V test

Once hypothesis testing was completed, categorical variable distribution along with correlation and regression between nominal variables was implemented. Also, k-means clustering was performed to showcase the concepts of unsupervised learning.