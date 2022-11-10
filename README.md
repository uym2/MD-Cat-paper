# MD-Cat data suit
---
This repository includes 4 datasets used in the manuscript "Expectation-Maximization enables Phylogenetic Dating under a Categorical Rate Model".
* Simulated data of HIV viruses: 12 different model conditions (reuse and augment the data by [To et. al.](https://datadryad.org/stash/dataset/doi:10.5061/dryad.968t3)
* Simulated data of Angiosperms: 5 different scenarios (reuse and augment the data by Beaulieu et. al.)
* Real biological data of HIV-1 M group (subtypes A-L): downloaded from LANL [http://www.hiv.lanl.gov/], filtered and subsampled to 1104 sequences
* Real biological data of Angiosperms: include DNA sequences of 4 genes atpB, rbcL, psbA, and 18S published by Beaulieu et. al. 

## Description of the Data and file structure
Files belonging to each of the 4 datasets can be identified by a unique prefix:
* simHIV: files belonging to the simulated data of HIV viruses. The 12 model conditions are grouped into 3 main categories: unimodals, bimodals, and multimodals as described in the manuscript.
Note on the mismatch between the file name and the model name in the manuscript:
    * For bimodals:
        * clock1 --> bimodal 3
        * clock2 --> bimodal 4
        * clock3 --> bimodal 1
        * clock4 --> bimodal 2
    * For multimodals:
        * trilnorm --> trimodal 3
        * trilnormcave --> trimodal 1
        * trilnormvex --> trimodal 2
* simAngio: files belonging to the simulated data of Angiosperms.
* realHIV: files belonging to the real HIV data.
* realAngio: files belonging to the real angiosperm data.
