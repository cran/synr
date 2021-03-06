# synr <img src="man/figures/synr_logo.png" align="right" width="120" />
[![Build Status](https://app.travis-ci.com/datalowe/synr.svg?branch=master)](https://app.travis-ci.com/datalowe/synr)

This is an [R](https://www.r-project.org/) package for working with data resulting from [grapheme-color synesthesia](https://en.wikipedia.org/wiki/Grapheme%E2%80%93color_synesthesia)-related [consistency tests](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4118597/#S4title). synr provides tools for exploring test data, including visualizing a single participant's data, and applying summarizing functions such as calculating [color variation/consistency scores](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4118597/#S4title) or classifying participant data as valid or invalid.

## Installation
TODO: This section will be updated once it's been decided where to make synr binaries available. 

## Usage
Once data are in an appropriately formatted data frame/tibble ('long format' - see vignettes for more information), everything starts with rolling up participant data into a 'ParticipantGroup' object with `create_participantgroup_widedata`:

```r
library(synr)

pgroup <- create_participantgroup(
    formatted_df, # data frame/tibble to use, with data in 'long format'
    n_trials_per_grapheme=3, # number of trials that grapheme was used for
    participant_col_name="participant_id", # name of column which holds participant ID's
    symbol_col_name='symbol', # name of column which holds grapheme symbol strings
    color_col_name='color', # rname of column which holds response color HEX codes
    color_space_spec = "Luv" # color space to use for all calculations with participant group
)
```
Using the resulting object (`pgroup`), you can call various methods. A few examples follow. 

### Example group-level method: get_mean_consistency_scores
`pgroup$get_mean_consistency_scores(symbol_filter=LETTERS)` would return a vector of [CIELUV](https://en.wikipedia.org/wiki/CIELUV)-based consistency scores, using only data from trials involving capital letters.

### Example group-level method: check_valid_get_twcv_scores
`pgroup$check_valid_get_twcv_scores(symbol_filter=0:9)` would return a data frame which describes classifications of all participant data, where each data set is classified as 'invalid' or 'valid', based largely on [DBSCAN clustering](https://en.wikipedia.org/wiki/DBSCAN). This may be used to identify participants who varied their responses too little, e. g. by responding with an orange color on every trial.

### Example participant-level method: get_plot
`pgroup$participants[[1]]$get_plot(symbol_filter=LETTERS)` would produce a bar plot of per-grapheme consistency scores for a single participant, using only data from trials involving capital letters. You can see an example below.

<img src="man/figures/example_consistency_plot.png" width="400" alt="Example bar plot of grapheme-level consistency scores">

### Detailed usage information
More details on required data format and how to use the above functions and more can be found in the [package's vignettes](https://datalowe.github.io/synr/articles/), which are also included in the package itself (run `help(synr)` to find them).

## Feedback
If you have any suggestions on improvements you are very welcome to directly raise issues or commit code improvements to the github repository at https://github.com/datalowe/synr.
