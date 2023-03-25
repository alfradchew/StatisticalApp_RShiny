## STATISTICAL APP USER MANUAL

CONTENTS OF THIS FILE
---------------------
* [Introduction](#introduction)
* [1. Installation](#1-installation)
* [2. Overview](#2-overview)
* [3. Control Panel](#3-control-panel)
* [4. Statistics](#4-statistics)
    * [4a. Characterization Table](#4a-characterization-table)
    * [4b. Correlation Table](#4b-correlation-table)
    * [4c. Differential Analysis](#4c-differential-analysis)
* [5. Vizualization](#5-vizualization)
    * [5a. Histogram](#5a-histogram)
    * [5b. Scatterplot](#5b-scatterplot)
    * [5c. Boxplot](#5c-boxplot)
    * [5d. Stacked Barplot](#5d-stacked-barplot)
    * [5e. Correlation Plot](#5e-correlation-plot)
* [Credits](#credits)
  
  
INTRODUCTION 
---------------------

The Statistical App provides some basic statistical functionalities and vizualization tools for the users to analyse their data.

This guide is intended for the end users of the app. 

The Statistical App is built using the Shiny, a web application framework for R developed by RStudio. Along with the obvious requirements of R and Shiny server, the following packages are required to run the application: 


### 1. INSTALLATION
---------------------

The Statistical App is built using the Shiny, a web application framework for R developed by RStudio. Along with the obvious requirements of R and Shiny server, the following packages are required to run the application: 

* __shiny__: For shiny server
* __shinyBS__: Provides the "bsButton" feature
* __shinyjs__: Provides the "hidden" function
* __XLConnect__: Reads xlsx files
* __psych__: Gives the "describe" function which displays basic statistics of a variable such as mean and median
* __ggplot2__: Provides vizualizations for the data * __reshape2__: Provides the melt function for the correlation plot
* __plyr__: Provides the function "ddply" for the barplot when vizualizing contingency tables
* __xlsx__: Export excel files

Run the file __install.packages.R__ to install the above packages. If you encountered the following errors when loading __XLConnect__:

Loading required package: rJava  
Error : .onLoad failed in loadNamespace() for 'rJava', details:  
  call: fun(libname, pkgname)  
 error: No CurrentVersion entry in Software/JavaSoft registry! Try re-installing Java and make sure R and Java have matching architectures.  

It is likely due to the fact that you are using a 64-bit OS and R version but do not have Java installed with the same architecture. Download Java 64-bit
from this page: https://www.java.com/en/download/manual.jsp and reload the package again.

![alt text](https://github.com/alfrad92/statistical-app_RShiny/blob/master/Statistical_App/images/load_location.png)

Alternatively, the app can be directly accessed from [here](https://alfradc.shinyapps.io/statistical_app/). However, it should be noted that it is much faster running the app on your local machine.

To begin, load the file "simulated data.xlsx" in the "Statistical_App" folder onto the app.


### 2. OVERVIEW 
---------------------

Below shows how the R files are connected with one another.

![alt text](https://github.com/alfrad92/statistical-app_RShiny/blob/master/Statistical_App/images/layout.png)


### 3. CONTROL PANEL
---------------------

The options in the left panel allow the user to load the excel file, choose which variables to be used for the statistical analysis, determine which variables are categorical and continuous, and select which functionalities to use.

* __Upload__: Load the dataset and click upload.
 * __Select all variables__: Selects all the variables in the __Select explanatory variables__ list.
 * __Select explanatory variables__: Displays all the relevant variables for selection.
 * __Select categorical explanatory variables__: Determines which variables are considered categorical. __THIS AFFECTS THE TYPE OF STATISTICAL TESTS USED IN THE LATER STAGE__. Variables that do not contain a single numerical data are pre-selected.
 * __Select main function__: Displays all the available functions.

Available functions:
* [Statistics](#4-statistics)
* [Vizualization](#5-vizualization)


### 4. STATISTICS
---------------------

Conducts basic statistical analysis and significant tests on the variables and provides the relevant vizualization plots.

Available sub-functions:
* [Characterization Table](#4a-characterization-table)
* [Correlation Table](#4b-correlation-table)
* [Differential Analysis](#4c-differential-analysis)


### 4a. CHARACTERIZATION TABLE
---------------------

Provides a summary of statistics for the variables. For continuous variables, the table displays the sample size, number of missing values, mean, standard deviation, minimum and maximum value, and the range of the variable. For categorical variable, the table displays the sample size of each subgroup for every categorical variable.


### 4b. CORRELATION TABLE
---------------------

Provides pairwise correlation and its p-value table.


### 4c DIFFERENTIAL ANALYSIS
-------------------------
Conducts pairwise significance test based on the outcome of interest. 

Non-parametric tests will be used to conduct the pairwise significance test. The significance table displays the name of variable, the type of variable, number of __non-missing paired__ samples, p-value, adjusted p-value, rho and rsquared (for continuous outcome and predictor), and the name of the statistical test. The table is sorted by p-values. The adjusted p-value is based on benjamini hochberg.

Methods used: 

* __Continuous outcome (Y)__ vs __Continuous predictor (X)__: Spearman's rank correlation test.
* __Continuous outcome (Y)__ vs __Categorical predictor (X)__: Mann-Whitney U test (for 2 groups) and Kruskal Wallis test (for more than 2 groups).
* __Categorical outcome (Y)__ vs __Continuous predictor (X)__: Mann-Whitney U test (for 2 groups) and Kruskal Wallis test (for more than 2 groups).
* __Categorical outcome (Y)__ vs __Categorical predictor (X)__: Fisher's Exact test. __Currently, the test does not consider missing values in the variables__. 

__VIZUALIZATION__

Displays the relevant plots based on the type of variables for the outcome and predictor. 

* __Continuous outcome (Y)__ vs __Continuous predictor (X)__: Scatterplot with line of best fit.
* __Continuous outcome (Y)__ vs __Categorical predictor (X)__: Vertical boxplot.
* __Categorical outcome (Y)__ vs __Continuous predictor (X)__: Horizontal boxplot,
* __Categorical outcome (Y)__ vs __Categorical predictor (X)__: Barplot.

### 5. VIZUALIZATION
---------------------

![alt text](https://github.com/alfrad92/statistical-app_RShiny/blob/master/Statistical_App/images/vizualization.png)

Available vizualizations:

* [Histogram](#5a-histogram)
* [Scatterplot](#5b-scatterplot)
* [Boxplot](#5c-boxplot)
* [Stacked Barplot](#5d-stacked-barplot)
* [Correlation Plot](#5e-correlation-plot)


### 5a. HISTOGRAM
----------------------

Analyses continuous variables only. Categorical variables selected previously at the [main control panel](#3-control-panel) are not allowed to be vizualized.


### 5b. SCATTERPLOT
----------------------

Analyses continuous variables only. Categorical variables selected previously at the [main control panel](#3-control-panel) are not allowed to be vizualized.

* __Select variable X:__ Select the continuous variable to be displayed on the X axis.  
* __Select variable Y:__ Select the continuous variable to be displayed on the Y axis. The variable selected earlier for the B axis will not be included for selection for the Y axis.  
* __Display by Group Variable:__ Provides a different colour for the points in the scatterplot for each group.
* __Insert Standard Deviational Ellipse:__ Provides an elliptical boundary which the majority of the points are located in. Only avaiable when the __"Display by Group Variables"__ is selected.  
* __Select Group Variable:__ Select the cateogorical variable for the group.  
* __Insert Line of Best Fit:__ Provides a linear line of best fit. Different colours are used for each line when __"Display by Group Variables"__ is selected.  


### 5c. BOXPLOT
-------------------------------

Only applicable when at least one continuous and one categorical variable are selected. The __"Display by Group Variable"__ option is only available when at least 2 categorical variables are selected.

* __Select continuous variable:__ Select the continuous variable to be displayed on the Y axis.  
* __Select categorical variable:__ Select the group variable to be displayed on the X axis.  
* __Invert Graph:__ Rotates the graph 90 degrees clockwise.  
* __Display by Group Variable:__ Select another cateogorical variable for the subgroup.  
* __Insert Standard Deviational Ellipse:__ Provides an elliptical boundary which the majority of the points are located in. Only avaiable when the __"Display by Group Variables"__ is selected.  


### 5d. STACKED BARPLOT
------------------------------

Only applicable when are least 2 cateogorical variables are selected.

* __Select variable of interest:__ Select the categorical variable to be displayed on the X axis.   
* __Select group variable:__ Select the group variable to be furthur classify the samples.  
* __Invert Graph:__ Rotates the graph 90 degrees clockwise.  
* __Insert count label:__ Insert the frequency values in the stacked barplot.


### 5e. CORRELATION PLOT
------------------------------

Provides a triangular correlation plot filled with with a colour gradient. The colour gradient is based on the correlation value. 

__The maximum number of variables are allowed in the plot is 10__. If more than 10 variables are selected, only the first 19 will be plotted. 

Select __"Display only significant variables "__ to remove text labels for insignificant variables in the plot. The significance threshold level is based on the value entered in __"Imput Significance Level"__. For example, if 95% significance level is selected, correlation values with p-values less than 0.05 will not be displayed.


### CREDITS
---------------------
This app is created by [Chew Si Wei Alfrad](https://sg.linkedin.com/in/alfrad-chew-39809a151), who graduated from the National University of Singapore with a Bachelor of Science (B.Sc) with Honours (Distinction), majoring in Statistics and Economics. Alfrad can also be contacted at alfrad.chew@u.nus.edu.
