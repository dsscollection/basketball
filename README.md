Repository for dsscollection submission "Modeling Offensive Player Movement in Professional Basketball" by Steven Wu and Luke Bornn.

Below are descriptions of the subdirectories of this repo:
- analysis: contains all code and materials required to create the PDF of the paper submission. To create the PDF, you will need `knitr`, `dplyr`, `ggplot2`, `raster`, `grid`, `gridExtra` installed. Then, either (1) open 'article.Rnw' in RStudio and click 'File -> Preview' or (2) open up a Terminal and type 

> Rscript -e "library(knitr); knit('./EPV_demo.Rnw')".

- data: contains a .csv of the dataset the article operates on, as well as .Rdata files of intermediate data that is helpful for testing changes to the implementation of functions
- plots: contains static plots that the article uses, namely plots that were generated over a season's worth of data

We would like to acknowledge STATS, LLC for consenting the inclusion of a full-game data sample.
