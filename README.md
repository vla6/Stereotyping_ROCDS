# Stereotyping_ROCDS
Code and files related to ROC Data Science Meetup 12 Nov 2020.\
https://www.meetup.com/ROC-Data-Science/events/274237489/

Explores fairness metrics and Shapley explanation techniques for detecting stereotyping and feature bias in models. Shows that fairness metrics do not distinguish stereotyping from decisions made based on reasonable factors.  Demonstrates that features driving differences can be isolated using Shapley techinques, and suggests additional tests for analyzing causes of differences.  

Uses an xgboost model via h2o; currently this is supprted only on Linux.  To use a random forest model instead (which will work on Windows), modify 02_models.R by changing the value of kModelType near the top of the file.  

To run the code, do the following:
1.  Install h2o (see http://h2o-release.s3.amazonaws.com/h2o/rel-zermelo/1/index.html)
2.  Open the package 202010_fairness.Rproj in R Studio (if not using RStudio, set your working directory to the folder containing the project)
3.  Edit the file 00_setup.R, setting kOutputDir to a writeable directory on your machine
4.  Run the file 00_run_all.R

Because exact Shapley values are calculated, runtimes are long.  The number of samples analyzed can be reduced to speed up the scripts.  

**Towards Data Science Article Code**

For [Fairness Metrics Won’t Save You from Stereotyping](https://towardsdatascience.com/fairness-metrics-wont-save-you-from-stereotyping-27127e220cac), you only need to run scripts 00, 01, 02, 03, and 05.  

For [No Free Lunch with Feature Bias](https://medium.com/towards-data-science/no-free-lunch-with-feature-bias-561c9cd3dd18) and [How to Fix Feature Bias](https://medium.com/towards-data-science/how-to-fix-feature-bias-9e47abccb942), run scripts 00, 01, 02,04, 06, and 07.
