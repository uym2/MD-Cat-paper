# Title of Dataset: Data used in the manuscript "Expectation-Maximization enables Phylogenetic Dating under a Categorical Rate Model"
---

This repository includes 4 datasets:
* simulated data of HIV viruses: 12 different model conditions (reuse and augment the data by To et. al.)
* simulated data of Angiosperms: 5 different scenarios (reuse and augment the data by Beaulieu et. al.)
* real biological data of HIV-1 M group (subtypes A-L): downloaded from LANL (http://www.hiv.lanl.gov/), filtered and subsampled to 1104 sequences
* real biological data of Angiosperms: include DNA sequences of 4 genes atpB, rbcL, psbA, and 18S published by Beaulieu et. al. 

## Description of the Data and file structure
* simHIV: simulated data of HIV viruses. The 12 model conditions are grouped into 3 main categories: unimodals, bimodals, and multimodals as described in the manuscript.
Note on the mismatch between the file name and the model name in the manuscript:
    ** For bimodals:
        clock1 --> bimodal 3
        clock2 --> bimodal 4
        clock3 --> bimodal 1
        clock4 --> bimodal 2
    ** For multimodals:
        trilnorm --> trimodal 3
        trilnormcave --> trimodal 1
        trilnormvex --> trimodal 2
