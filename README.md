# ReMoDe

ReMoDe is an R package designed for the robust detection of modes in data distributions. It uses statistical tests, including Fisher's exact test and binomial tests, to determine if a given maximum in a data distribution is a true local maximum. It was specifically designed for mode detection in ordinal distributions but can also be applied to continuous data. 

## Features

* Mode Detection based on recursive statical testing to identify local maxima in the distribution
* Data formatting: Converts raw data into count data for analysis. Input can thus be in counts or raw
* Stability Analysis: Includes functionality to assess the stability of the number and location of detected modes using jackknife resampling
* Visualization: Provides methods to plot the histogram of data along with identified modes, as well as outcomes of the stabilty analysis

## Usage 

```R
# count data as input
xt <- c(8,20,5,2,5,2,30)

# apply ReMoDe algorithm 
results <- remode(xt)

# plot result
barplot(results)

# Perform stability analysis 
stability_info <- remode_stability(results)
```

## Citation

Please cite the following paper 
```
To be added
```