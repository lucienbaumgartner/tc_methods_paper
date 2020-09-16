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
          - [Testing H1a-d](#testing-h1a-d)
          - [Testing H2a-b](#testing-h2a-b)
          - [Additional Material](#additional-material)
              - [Overview of the Pooled
                Subsample](#overview-of-the-pooled-subsample)
              - [Do the Target Categories have Significantly Different
                Average *Absolute*
                Valences?](#do-the-target-categories-have-significantly-different-average-absolute-valences)
              - [Do the Target Categories have Significantly Different
                Average *Polar*
                Valences?](#do-the-target-categories-have-significantly-different-average-polar-valences)

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

The data for the first study are 600’000+ reddit comments, of which we
took a subsample of 5000 occurrences for each target adjectives (45) in
combination with the respective conjunction (2). Accordingly, the pooled
subsamples consist of (45x2x5000=) 450’000 comments.

## Hypotheses

**H1: Valence for pos/neg target adjectives**

  - H1a: neg x but = pos
  - H1b: neg x and = neg
  - H1c: pos x but = neg
  - H1d: pos x and = pos

**H2: Comparative neutral valence**

  - H2a: neut x and \> neg x and
  - H2b: neut x and \< pos x and

## Results

![h1\_2](output/plots/SUBSAMPLE_H1_02_09_20.png)

### Testing H1a-d

  - H1a: neg x but = pos :white\_check\_mark:
  - H1b: neg x and = neg :white\_check\_mark:
  - ~~H1c: pos x but = neg~~ :negative\_squared\_cross\_mark:
  - H1d: pos x and = pos :white\_check\_mark:

**H\_A: fitted mean greater than 0**

<pre>
CCONJ = and:
 TARGET_pol  emmean      SE     df t.ratio  p.value
 negative   -0.3004 0.00130 449994 -231.505 1.0000 
 neutral     0.0178 0.00184 449994    9.724 <.0001 
 <b>positive    0.3210 0.00130 449994  247.361 <.0001</b> 

CCONJ = but:
 TARGET_pol  emmean      SE     df t.ratio  p.value
 <b>negative    0.1703 0.00130 449994  131.230 <.0001</b> 
 neutral     0.1686 0.00184 449994   91.866 <.0001 
 positive    0.0700 0.00130 449994   53.910 <.0001 

P values are right-tailed 
</pre>

**H\_A: fitted mean smaller than 0**

<pre>
CCONJ = and:
 TARGET_pol  emmean      SE     df t.ratio  p.value
 <b>negative   -0.3004 0.00130 449994 -231.505 <.0001</b>  
 neutral     0.0178 0.00184 449994    9.724 1.0000 
 positive    0.3210 0.00130 449994  247.361 1.0000 

CCONJ = but:
 TARGET_pol  emmean      SE     df t.ratio  p.value
 negative    0.1703 0.00130 449994  131.230 1.0000 
 neutral     0.1686 0.00184 449994   91.866 1.0000 
 <b>positive    0.0700 0.00130 449994   53.910 1.0000</b>  

P values are left-tailed 
</pre>

### Testing H2a-b

  - H2a: neut x and \> neg x and :white\_check\_mark:
  - H2b: neut x and \< pos x and :white\_check\_mark:

**effect sizes two sided**

<pre>
CCONJ = and:
 TARGET_pol  emmean      SE     df lower.CL upper.CL
 negative   <b>-0.3004</b> 0.00130 449994  -0.3030  -0.2979
 neutral     <b>0.0178</b> 0.00184 449994   0.0143   0.0214
 positive    <b>0.3210</b> 0.00130 449994   0.3185   0.3236
</pre>

**pairwise comparisons**

<pre>
CCONJ = and:
 contrast            estimate      SE     df t.ratio  p.value
 <b>negative - neutral   -0.3183 0.00225 449994 -141.599 <.0001</b> 
 negative - positive  -0.6215 0.00184 449994 -338.609 <.0001 
 <b>neutral - positive   -0.3032 0.00225 449994 -134.874 <.0001</b> 
</pre>

### Additional Material

#### Overview of the Pooled Subsample

![pos](output/plots/SUBSAMPLE_POS_only_02_09_20.png)
![neg](output/plots/SUBSAMPLE_NEG_only_02_09_20.png)
![neutral](output/plots/SUBSAMPLE_NEUTRAL_only_02_09_20.png) \#\#\#\#
Observed Average per Word

<pre>
   GEN                       TARGET        CCONJ     avg     n
   <chr>                     <chr>         <chr>   <dbl> <int>
 1 descriptive_concepts      dry           and   -0.0292  5000
 2 descriptive_concepts      dry           but    0.250   5000
 3 descriptive_concepts      large         and    0.123   5000
 4 descriptive_concepts      large         but    0.0831  5000
 5 descriptive_concepts      loud          and   -0.0392  5000
 6 descriptive_concepts      loud          but    0.209   5000
 7 descriptive_concepts      narrow        and   -0.0616  5000
 8 descriptive_concepts      narrow        but    0.183   5000
 9 descriptive_concepts      permanent     and    0.0145  5000
10 descriptive_concepts      permanent     but    0.0827  5000
11 descriptive_concepts      rainy         and   -0.104   5000
12 descriptive_concepts      rainy         but    0.178   5000
13 descriptive_concepts      short         and    0.202   5000
14 descriptive_concepts      short         but    0.337   5000
15 descriptive_concepts      wooden        and   -0.175   5000
16 descriptive_concepts      wooden        but   -0.0478  5000
17 descriptive_concepts      yellow        and    0.229   5000
18 descriptive_concepts      yellow        but    0.243   5000
19 thick_moral_concepts      compassionate and    0.402   5000
20 thick_moral_concepts      compassionate but    0.128   5000
21 thick_moral_concepts      courageous    and    0.416   5000
22 thick_moral_concepts      courageous    but    0.0715  5000
23 thick_moral_concepts      cruel         and   -0.252   5000
24 thick_moral_concepts      cruel         but    0.282   5000
25 thick_moral_concepts      friendly      and    0.404   5000
26 thick_moral_concepts      friendly      but    0.126   5000
27 thick_moral_concepts      generous      and    0.483   5000
28 thick_moral_concepts      generous      but    0.152   5000
29 thick_moral_concepts      honest        and    0.325   5000
30 thick_moral_concepts      honest        but    0.155   5000
31 thick_moral_concepts      reckless      and   -0.403   5000
32 thick_moral_concepts      reckless      but    0.0447  5000
33 thick_moral_concepts      rude          and   -0.408   5000
34 thick_moral_concepts      rude          but    0.0128  5000
35 thick_moral_concepts      selfish       and   -0.423   5000
36 thick_moral_concepts      selfish       but    0.0158  5000
37 thick_moral_concepts      vicious       and   -0.342   5000
38 thick_moral_concepts      vicious       but    0.0421  5000
39 thick_non-moral_concepts  beautiful     and    0.404   5000
40 thick_non-moral_concepts  beautiful     but   -0.198   5000
41 thick_non-moral_concepts  boring        and   -0.261   5000
42 thick_non-moral_concepts  boring        but    0.293   5000
43 thick_non-moral_concepts  delicious     and    0.401   5000
44 thick_non-moral_concepts  delicious     but    0.0284  5000
45 thick_non-moral_concepts  disgusting    and   -0.434   5000
46 thick_non-moral_concepts  disgusting    but    0.282   5000
47 thick_non-moral_concepts  funny         and    0.283   5000
48 thick_non-moral_concepts  funny         but   -0.0422  5000
49 thick_non-moral_concepts  insane        and   -0.229   5000
50 thick_non-moral_concepts  insane        but    0.216   5000
51 thick_non-moral_concepts  justified     and    0.286   5000
52 thick_non-moral_concepts  justified     but   -0.0207  5000
53 thick_non-moral_concepts  stupid        and   -0.338   5000
54 thick_non-moral_concepts  stupid        but    0.186   5000
55 thick_non-moral_concepts  ugly          and   -0.317   5000
56 thick_non-moral_concepts  ugly          but    0.277   5000
57 thick_non-moral_concepts  wise          and    0.356   5000
58 thick_non-moral_concepts  wise          but    0.126   5000
59 thin_concepts             awful         and   -0.354   5000
60 thin_concepts             awful         but    0.243   5000
61 thin_concepts             bad           and   -0.153   5000
62 thin_concepts             bad           but    0.186   5000
63 thin_concepts             good          and    0.0558  5000
64 thin_concepts             good          but   -0.0398  5000
65 thin_concepts             great         and    0.349   5000
66 thin_concepts             great         but    0.0222  5000
67 thin_concepts             terrible      and   -0.307   5000
68 thin_concepts             terrible      but    0.232   5000
69 thin_concepts             terrific      and    0.250   5000
70 thin_concepts             terrific      but    0.0665  5000
71 value-associated_concepts bloody        and   -0.349   5000
72 value-associated_concepts bloody        but    0.0235  5000
73 value-associated_concepts broken        and   -0.349   5000
74 value-associated_concepts broken        but    0.233   5000
75 value-associated_concepts closed        and    0.141   5000
76 value-associated_concepts closed        but    0.173   5000
77 value-associated_concepts empty         and   -0.269   5000
78 value-associated_concepts empty         but    0.172   5000
79 value-associated_concepts homeless      and   -0.361   5000
80 value-associated_concepts homeless      but    0.152   5000
81 value-associated_concepts quiet         and    0.214   5000
82 value-associated_concepts quiet         but    0.278   5000
83 value-associated_concepts rich          and    0.173   5000
84 value-associated_concepts rich          but    0.0113  5000
85 value-associated_concepts shiny         and    0.402   5000
86 value-associated_concepts shiny         but    0.103   5000
87 value-associated_concepts sunny         and    0.374   5000
88 value-associated_concepts sunny         but    0.173   5000
89 value-associated_concepts tall          and    0.200   5000
90 value-associated_concepts tall          but    0.121   5000
</pre>

#### Do the Target Categories have Significantly Different Average *Absolute* Valences?

I quickly checked whether the interaction between the categories of our
target adjectives and the conjunction produces significant valence
effects. As we can see, the categories do indeed occupy different
valence strata. Thin concepts, however, have a surprisingly high
estimated average valence in AND-conjunctions, even higher than thick
concepts (moral and non-moral). This is rather unexpected. In
BUT-conjunction, on the other hand, thin concepts behave as expected and
have lower average valence than thick conepts. Also note that moral and
non-moral thick concepts occupy a similar strata and could maybe be
combined in the future.

**Note that the plot below shows estimated marginal means**:
<https://cran.r-project.org/web/packages/emmeans/vignettes/basics.html>

![estimated
means](output/plots/SUBSAMPLE_emmeans_catxCCONJ_02_09_20.png)

Results of the ANOVA:

<b>m1 \<- aov(abs(sentiWords) \~ cat\*CCONJ, data = dfx\_sample)</b>

<b> estimated means </b>

<pre>
                         GEN CCONJ    emmean           SE     df  lower.CL  upper.CL
1       descriptive_concepts   and 0.2986580 0.0010022992 449990 0.2966935 0.3006225
2       thick_moral_concepts   and 0.4435842 0.0009508645 449990 0.4417205 0.4454478
3   thick_non-moral_concepts   and 0.4362664 0.0009508645 449990 0.4344027 0.4381300
4              thin_concepts   and 0.4572198 0.0012275608 449990 0.4548139 0.4596258
5  value-associated_concepts   and 0.3915487 0.0009508645 449990 0.3896851 0.3934124
6       descriptive_concepts   but 0.3456327 0.0010022992 449990 0.3436682 0.3475971
7       thick_moral_concepts   but 0.3993863 0.0009508645 449990 0.3975226 0.4012499
8   thick_non-moral_concepts   but 0.4034516 0.0009508645 449990 0.4015879 0.4053152
9              thin_concepts   but 0.3739884 0.0012275608 449990 0.3715825 0.3763944
10 value-associated_concepts   but 0.3639294 0.0009508645 449990 0.3620657 0.3657930
</pre>

<b> pairwise comparisons </b>

<pre>
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
</pre>

#### Do the Target Categories have Significantly Different Average *Polar* Valences?

I also briefly had a look whether the results make sense, once we
substitute *absolute* sentiment values with *polar* values. Overall,
there is no overlap between conjunctions within a polarity group, which
is a good sign. The two types of thick concepts (moral/non-m.) do not
behave similarly anymore, in contrast to the analysis above using
absolute valence measures. This speaks against collapsing the types into
a single category.

**Note that the plot below shows estimated marginal means**:
<https://cran.r-project.org/web/packages/emmeans/vignettes/basics.html>

![estimated
means](output/plots/SUBSAMPLE_emmeans_TARGET_polxCCONJ_02_09_20.png)

Results of the ANOVA:

<b> m1 \<- aov(sentiWords \~ (cat x TARGET\_pol)\*CCONJ, data =
dfx\_sample\[\!dfx\_sample$cat == ‘descriptive\_concepts’,\])</b>

<pre>
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
</pre>
