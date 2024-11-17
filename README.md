# Focusing-a-Viral-Risk-Ranking-Tool-on-Prediction
## The following workflow is provided to take users through a three part process of maintaining, organizing, and analyzing the "Spillover Rankings" as provided by the SpillOver: Viral Risk Ranking tool. 

# STEP 1: Download CSV File from Spillover Global Website
## STEP 1 is required to obtain the original data as provided by the Spillover: Viral Risk Ranking tool. 
### Visit: https://spillover.global/ranking-comparison/.
### Click on "Download Results" to download the CSV file.
### The download should result in a file named SpilloverRankings.csv.

# STEP 2: Run the Jupyter Notebook (Spillover_Organization.ipynb)
## STEP 2 is required to organize the column entitled 'Risk Levels' into a usable format for data analysis. In the original csv this column contains the "Risk Name", "Risk Score", "Impact Score", "Weighted Score", and "Corresponding level/levels" for all factors evaluated for each virus (row). This Jupyter Notebook was originally edited and run in Google Colab.
### Upload the SpilloverRankings.csv as directed in the first code cell.
### Run the second code cell to organize the data.

# STEP 3: Run the R Script (Spillover_Evaluation_Analysis.R)
## STEP 3 is required to visualize and analyze the data. This R Script was edited and run in R Studio.
### Set working directory.
### Load data.
### Run code to reproduce Figures 1, 2, and 3.
