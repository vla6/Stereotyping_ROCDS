# Stereotyping_ROCDS
Code and files related to ROC Data Science Meetup 12 Nov 2020.\
https://www.meetup.com/ROC-Data-Science/events/274237489/

Explores fairness metrics and Shapley explanation techniques for detecting stereotyping and feature bias in models. Shows that fairness metrics do not distinguish stereotyping from decisions made based on reasonable factors.  Demonstrates that features driving differences can be isolated using Shapley techinques, and suggests additional tests for analyzing causes of differences.  

To run the code, do the following:
1.  Install h2o (see http://h2o-release.s3.amazonaws.com/h2o/rel-zermelo/1/index.html)
2.  Open the package 202010_fairness.Rproj in R Studio (if not using RStudio, set your working directory to the folder containing the project)
3.  Edit the file 00_setup.R, setting kOutputDir to a writeable directory on your machine
4.  Run the file 00_run_all.R

Because exact Shapley values are calculated, runtimes are long.  The number of samples analyzed can be reduced to speed up the scripts.  
