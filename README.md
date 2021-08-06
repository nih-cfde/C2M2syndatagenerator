# C2M2syndatagenerator
To create synthetic datasets for testing and demonstration

To Run:

1. Click the [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/nih-cfde/C2M2syndatagenerator/HEAD?urlpath=rstudio) button to load Rstudio in your browser
2. In the lower right corner click `builddataset.R`
3. In the top right text editor, change any desired values in the `User Settable Options` section
4. Click the `source` button at the top of the text editor to run the tool


To download results:

1. After running, check the box next to your output folder in the lower right corner
2. Click the blue gear that says `More`
3. Choose `Export`


The above binder runs great for small datesets, but can't generate more than about a million lines per file. To generate larger synthetic datasets, use pangeo binder. To launch with pangeo instead use [![Binder](https://binder.pangeo.io/badge_logo.svg)](https://binder.pangeo.io/v2/gh/nih-cfde/C2M2syndatagenerator/main)
