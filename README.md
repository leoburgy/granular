# granular

[![Linux build status](https://travis-ci.org/csiro-crop-informatics/granular.svg?branch=master)

Granular is an R package containing a shiny app the implements the mix function from the [mixdist](http://ms.mcmaster.ca/peter/mix/mix.html) package, principally dealing with starch granule size data from MasterSizer instruments.

The shiny app is designed to make it as easy as possible to use the power of the mixdist package to estimate the underlying distributions of starch granule populations from a mix of granule sizes.

###Installation and usage

granular can be installed locally with `devtools::install_github("csiro-crop-informatics/granular")`.

After installation, the shiny app is located under `shiny/granular/`. 

The shiny app can be started locally with `granular::run_granular()`. If this command is run from RStudio, a browser windown will be opened with the app. Otherwise, navigate to 127.0.0.1:port (the R session will tell you which port the server is listening on, otherwise you can add port as an argument to `run_granular`).