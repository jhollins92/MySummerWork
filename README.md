# This repository contains all the R code relevant to my project.
# Code_for_wenesday.R contains the code used to extract basic information from the models(eg. number of species, model ID). It also contains commands for plotting the information extracted.
# Model_DataFrame.R contains the code used to construct a data frame summarising all the models (well, at least the model that I could get to work).
# MORESBOTERMS.R contains the code used to extract SBO terms from the models.
# PlotsCode.R contains the code used to extract start dates from the models and then using this information, plots graphs of number of biomodels against time and total number of species against time.
# Species_Connections2.R finds out the number of connections for each species, ie the number of times a species occurs as a reactant or product in all the reactions in the database.
# Unknown_Parameters.R is the code used to find out the proportion of parameters and species whose initial value amount concentration is unknown. This code appear in Model_DataFrame.R
# WhichOnesWork.R contains the code I used to set up an automated way of checking which models will work when read into R. This code also appears in the other files, so it is not necessary to run the code in this file in order for the other R scripts to work properly.
# Apologies for the random (and slightly mental) filenames, this is a result of me naming the files whatever first came into my head to call them.
============