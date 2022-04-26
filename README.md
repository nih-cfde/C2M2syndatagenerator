# Crosscut Meta Data Model Synthetic Data Generator 

A script that can be run in the cloud to create synthetic datasets for testing and demonstration

To Run:

1. Click the [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/nih-cfde/C2M2syndatagenerator/main?urlpath=rstudio) button to load Rstudio in your browser
2. In the lower right corner click `builddataset.R`
3. In the top right text editor, change any desired values in the `User Settable Options` section
4. Click the `source` button at the top of the text editor to run the tool

To download results:

1. Open the `Terminal` and commpress your output folder by running `tar -zcvf outdir.tar.gz outdir/`
1. In the side panel, check the box next to your copmressed file
1. Click the blue gear that says `More`
1. Choose `Export` and select a location for saving the file
1. Locally, uncompress the the file with `tar -xf outdir.tar.gz`

Follow [these steps](https://docs.nih-cfde.org/en/latest/cfde-submit/docs/#use-the-tool) to submit your data for testing. 

Optional, save a copy of your modified `builddataset.R` as a branch for future reference.

For more information, see the
* [C2M2 Technical Doc](https://docs.nih-cfde.org/en/latest/c2m2/draft-C2M2_specification/) 
* [C2M2 Wiki](https://github.com/nih-cfde/published-documentation/wiki)
