
# blindreview

The goal of blindreview is to transform a database by masking the
    identity of the treatments and of the observation numbers so that the data
    may be reviewed in the absence of treatment knowledge. This database
    is suitable for a forward search using the forsearch package. The outcome of
    that package is a set of graphs that identify the best fitting observations 
    and the worst fitting observations to the planned analysis of the data 
    without also identifying the treatment that resulted in these observations.
    With regard to clinical trials, the US Food & Drug Administration guidance
    for industry "E9 Statistical Principles for Clinical Trals" is an important
    reference.
    
## Installation

Install the current version of blindreview with:
``` r
library(blindreview)
```
