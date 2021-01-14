# mavenR: A Workflow Pipeline for Processing MAVEn Datasets

**version 1.0.0**  *2020-05-18*

This repository holds code being developed by James S. Waters at Providence College (PC) to analyze high-throughput metabolic phenotyping data. We are grateful for the assistance of Megan L. Larsen & Juniper L. Simonis of DAPPER Stats in the initial code development and release. 

For a brief overview of the analysis pipeline, [a vignette is available here][https://waterslab.github.io/mavenR/vignettes/mavenworkflow.html].

The 'mavenR' package provides a current working version of the code pipeline.

The code relies on data that are confidential and owned by Providence College. Thus, the focal data files are not saved under version control (i.e., it is "git-ignored"). This is done by putting the file in a folder called `data`, and then listing the folder in the `.gitignore` file. In order to execute the code, the user must have the `data` directory with the file locally. 

Additional, intermidate data files that need to be "git-ignored" might be generated in the future. Any such files will be kept in the `data` folder, and  thus "git-ignored".
