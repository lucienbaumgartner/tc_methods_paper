README
================
Lucien Baumgartner
8/12/2020

  - [The Project](#the-project)
  - [Study 1: Valence - Proof of
    Concept](#study-1-valence---proof-of-concept)
      - [Data](#data)
      - [Hypotheses](#hypotheses)
      - [Results](#results)

# The Project

So far we lack the means to distinguish thick concepts from descriptive
concepts without relying on intuitions, and to measure evaluative
intensity. Using a corpus-based approach, we show that thick adjectives
and descriptive adjectives behave differently when being combined with
the terms “and” and “but”. After having shown the validity of our
approach, we show how this method can help to answer questions in
different domains of discourse: We provide empirical data revealing that
thick concepts are used differently in legal contexts compared to
everyday language.

# Study 1: Valence - Proof of Concept

## Data

The data for the first corpus study are 256’838 target structures from
reddit comments (100 days; 09.02.20-19.05.20). The data was gathered
using the pushhift API via R.

  - SOURCE: [pushshift API](https://pushshift.io/api-parameters/) (last
    retrieved 19.05.2020, open data)

## Hypotheses

## Results

![pos](output/plots/SUBSAMPLE_POS_only_02_09_20.png)
![neg](output/plots/SUBSAMPLE_NEG_only_02_09_20.png)
![neutral](output/plots/SUBSAMPLE_NEUTRAL_only_02_09_20.png)

![estimated
means](output/plots/SUBSAMPLE_emmeans_catxCCONJ_02_09_20.png)

Results of the ANOVA:

    CCONJ = and:
     contrast                                                 estimate      SE     df t.ratio  p.value
     descriptive_concepts - thick_moral_concepts              -0.14493 0.00138 449990 -104.899 <.0001 
     descriptive_concepts - (thick_non-moral_concepts)        -0.13761 0.00138 449990  -99.603 <.0001 
     descriptive_concepts - thin_concepts                     -0.15856 0.00158 449990 -100.053 <.0001 
     descriptive_concepts - (value-associated_concepts)       -0.09289 0.00138 449990  -67.235 <.0001 
     thick_moral_concepts - (thick_non-moral_concepts)         0.00732 0.00134 449990    5.442 <.0001 
     thick_moral_concepts - thin_concepts                     -0.01364 0.00155 449990   -8.782 <.0001 
     thick_moral_concepts - (value-associated_concepts)        0.05204 0.00134 449990   38.696 <.0001 
     (thick_non-moral_concepts) - thin_concepts               -0.02095 0.00155 449990  -13.494 <.0001 
     (thick_non-moral_concepts) - (value-associated_concepts)  0.04472 0.00134 449990   33.254 <.0001 
     thin_concepts - (value-associated_concepts)               0.06567 0.00155 449990   42.293 <.0001 
    
    CCONJ = but:
     contrast                                                 estimate      SE     df t.ratio  p.value
     descriptive_concepts - thick_moral_concepts              -0.05375 0.00138 449990  -38.908 <.0001 
     descriptive_concepts - (thick_non-moral_concepts)        -0.05782 0.00138 449990  -41.850 <.0001 
     descriptive_concepts - thin_concepts                     -0.02836 0.00158 449990  -17.893 <.0001 
     descriptive_concepts - (value-associated_concepts)       -0.01830 0.00138 449990  -13.243 <.0001 
     thick_moral_concepts - (thick_non-moral_concepts)        -0.00407 0.00134 449990   -3.023 0.0211 
     thick_moral_concepts - thin_concepts                      0.02540 0.00155 449990   16.357 <.0001 
     thick_moral_concepts - (value-associated_concepts)        0.03546 0.00134 449990   26.367 <.0001 
     (thick_non-moral_concepts) - thin_concepts                0.02946 0.00155 449990   18.975 <.0001 
     (thick_non-moral_concepts) - (value-associated_concepts)  0.03952 0.00134 449990   29.391 <.0001 
     thin_concepts - (value-associated_concepts)               0.01006 0.00155 449990    6.478 <.0001 
    
    Note: contrasts are still on the abs scale 
    P value adjustment: tukey method for comparing a family of 5 estimates 

![estimated
means](output/plots/SUBSAMPLE_emmeans_TARGET_polxCCONJ_02_09_20.png)

Results of the ANOVA:

    CCONJ = and:
     contrast                                                          estimate      SE     df t.ratio  p.value
     thick_moral_concepts_NEG - thick_moral_concepts_POS                -0.7712 0.00349 359984 -221.078 <.0001 
     thick_moral_concepts_NEG - (thick_non-moral_concepts_NEG)          -0.0497 0.00349 359984  -14.236 <.0001 
     thick_moral_concepts_NEG - (thick_non-moral_concepts_POS)          -0.7116 0.00349 359984 -203.997 <.0001 
     thick_moral_concepts_NEG - thin_concepts_NEG                       -0.0939 0.00403 359984  -23.305 <.0001 
     thick_moral_concepts_NEG - thin_concepts_POS                       -0.5838 0.00403 359984 -144.924 <.0001 
     thick_moral_concepts_NEG - (value-associated_concepts_NEG)         -0.1281 0.00349 359984  -36.706 <.0001 
     thick_moral_concepts_NEG - (value-associated_concepts_POS)         -0.6382 0.00349 359984 -182.937 <.0001 
     thick_moral_concepts_POS - (thick_non-moral_concepts_NEG)           0.7216 0.00349 359984  206.841 <.0001 
     thick_moral_concepts_POS - (thick_non-moral_concepts_POS)           0.0596 0.00349 359984   17.081 <.0001 
     thick_moral_concepts_POS - thin_concepts_NEG                        0.6774 0.00403 359984  168.154 <.0001 
     thick_moral_concepts_POS - thin_concepts_POS                        0.1875 0.00403 359984   46.535 <.0001 
     thick_moral_concepts_POS - (value-associated_concepts_NEG)          0.6432 0.00349 359984  184.371 <.0001 
     thick_moral_concepts_POS - (value-associated_concepts_POS)          0.1331 0.00349 359984   38.140 <.0001 
     (thick_non-moral_concepts_NEG) - (thick_non-moral_concepts_POS)    -0.6620 0.00349 359984 -189.761 <.0001 
     (thick_non-moral_concepts_NEG) - thin_concepts_NEG                 -0.0442 0.00403 359984  -10.976 <.0001 
     (thick_non-moral_concepts_NEG) - thin_concepts_POS                 -0.5341 0.00403 359984 -132.595 <.0001 
     (thick_non-moral_concepts_NEG) - (value-associated_concepts_NEG)   -0.0784 0.00349 359984  -22.470 <.0001 
     (thick_non-moral_concepts_NEG) - (value-associated_concepts_POS)   -0.5885 0.00349 359984 -168.701 <.0001 
     (thick_non-moral_concepts_POS) - thin_concepts_NEG                  0.6178 0.00403 359984  153.362 <.0001 
     (thick_non-moral_concepts_POS) - thin_concepts_POS                  0.1279 0.00403 359984   31.742 <.0001 
     (thick_non-moral_concepts_POS) - (value-associated_concepts_NEG)    0.5836 0.00349 359984  167.290 <.0001 
     (thick_non-moral_concepts_POS) - (value-associated_concepts_POS)    0.0735 0.00349 359984   21.060 <.0001 
     thin_concepts_NEG - thin_concepts_POS                              -0.4899 0.00450 359984 -108.779 <.0001 
     thin_concepts_NEG - (value-associated_concepts_NEG)                -0.0342 0.00403 359984   -8.484 <.0001 
     thin_concepts_NEG - (value-associated_concepts_POS)                -0.5443 0.00403 359984 -135.123 <.0001 
     thin_concepts_POS - (value-associated_concepts_NEG)                 0.4557 0.00403 359984  113.135 <.0001 
     thin_concepts_POS - (value-associated_concepts_POS)                -0.0544 0.00403 359984  -13.504 <.0001 
     (value-associated_concepts_NEG) - (value-associated_concepts_POS)  -0.5101 0.00349 359984 -146.231 <.0001 
    
    CCONJ = but:
     contrast                                                          estimate      SE     df t.ratio  p.value
     thick_moral_concepts_NEG - thick_moral_concepts_POS                -0.0469 0.00349 359984  -13.449 <.0001 
     thick_moral_concepts_NEG - (thick_non-moral_concepts_NEG)          -0.1713 0.00349 359984  -49.117 <.0001 
     thick_moral_concepts_NEG - (thick_non-moral_concepts_POS)           0.1009 0.00349 359984   28.919 <.0001 
     thick_moral_concepts_NEG - thin_concepts_NEG                       -0.1411 0.00403 359984  -35.026 <.0001 
     thick_moral_concepts_NEG - thin_concepts_POS                        0.0631 0.00403 359984   15.676 <.0001 
     thick_moral_concepts_NEG - (value-associated_concepts_NEG)         -0.0711 0.00349 359984  -20.383 <.0001 
     thick_moral_concepts_NEG - (value-associated_concepts_POS)         -0.0577 0.00349 359984  -16.544 <.0001 
     thick_moral_concepts_POS - (thick_non-moral_concepts_NEG)          -0.1244 0.00349 359984  -35.668 <.0001 
     thick_moral_concepts_POS - (thick_non-moral_concepts_POS)           0.1478 0.00349 359984   42.368 <.0001 
     thick_moral_concepts_POS - thin_concepts_NEG                       -0.0942 0.00403 359984  -23.379 <.0001 
     thick_moral_concepts_POS - thin_concepts_POS                        0.1101 0.00403 359984   27.323 <.0001 
     thick_moral_concepts_POS - (value-associated_concepts_NEG)         -0.0242 0.00349 359984   -6.934 <.0001 
     thick_moral_concepts_POS - (value-associated_concepts_POS)         -0.0108 0.00349 359984   -3.095 0.0551 
     (thick_non-moral_concepts_NEG) - (thick_non-moral_concepts_POS)     0.2722 0.00349 359984   78.035 <.0001 
     (thick_non-moral_concepts_NEG) - thin_concepts_NEG                  0.0303 0.00403 359984    7.510 <.0001 
     (thick_non-moral_concepts_NEG) - thin_concepts_POS                  0.2345 0.00403 359984   58.212 <.0001 
     (thick_non-moral_concepts_NEG) - (value-associated_concepts_NEG)    0.1002 0.00349 359984   28.734 <.0001 
     (thick_non-moral_concepts_NEG) - (value-associated_concepts_POS)    0.1136 0.00349 359984   32.572 <.0001 
     (thick_non-moral_concepts_POS) - thin_concepts_NEG                 -0.2420 0.00403 359984  -60.070 <.0001 
     (thick_non-moral_concepts_POS) - thin_concepts_POS                 -0.0377 0.00403 359984   -9.368 <.0001 
     (thick_non-moral_concepts_POS) - (value-associated_concepts_NEG)   -0.1720 0.00349 359984  -49.302 <.0001 
     (thick_non-moral_concepts_POS) - (value-associated_concepts_POS)   -0.1586 0.00349 359984  -45.463 <.0001 
     thin_concepts_NEG - thin_concepts_POS                               0.2042 0.00450 359984   45.349 <.0001 
     thin_concepts_NEG - (value-associated_concepts_NEG)                 0.0700 0.00403 359984   17.374 <.0001 
     thin_concepts_NEG - (value-associated_concepts_POS)                 0.0834 0.00403 359984   20.698 <.0001 
     thin_concepts_POS - (value-associated_concepts_NEG)                -0.1343 0.00403 359984  -33.328 <.0001 
     thin_concepts_POS - (value-associated_concepts_POS)                -0.1209 0.00403 359984  -30.004 <.0001 
     (value-associated_concepts_NEG) - (value-associated_concepts_POS)   0.0134 0.00349 359984    3.839 0.0035 
    
    P value adjustment: bonferroni method for 28 tests
