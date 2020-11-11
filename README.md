# Stereotyping_ROCDS
Code and files related to ROC Data Science Meetup 12 Nov 2020.

Explores fairness metrics and Shapley explanation techniques for detecting stereotyping and feature bias in models. Shows that fairness metrics do not distinguish stereotyping from decisions made based on possibly reasonable factors.  Demonstrates that features driving differences can be isolated using Shapley techinques, and suggests additional tests for analyzing causes of differences.  

To run the code, do the following:
1.  Install h2o and dplyr
2.  Edit the file 00_setup.R, setting kOutputDir to  a writeable directory on your machine
3.  Run the file 00_run_all.R

Because exact Shapley values are calculated, runtimes are long.  
