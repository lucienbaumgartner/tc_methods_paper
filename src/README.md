# Reproducibility
All scripts in this have to be executed in the order of the numerical prefix. Note that the scripts were written with a Mac OS, and have to be slightly adapted for Windows.

## Data Collection
The scripts xx--yy concern data collection for Study 1 and produce the corpus for Study 1. To reproduce the corpus, simply run the scripts. Make sure you have a stable internet connection and be aware of the considerable runtime. There may be small deviations in data collection, due to changes in the pushshift.io API over time. Also note that you will have to adapt **all absolute paths** in the scripts, but not the relative paths.

Please note that you can also request the data via email at lucien.baumgartner@philos.uzh.ch. In that case, you do not need to run the scripts xx--yy.

The data for Study 2 has been collected using [The Corpus of Contemporary American English (COCA)](https://www.english-corpora.org/coca/), and can be found at `input/study2.csv` or `input/study.xlsx` (identical content).

## Analyses
The analyses require corpus data, which can either be requested via email at lucien.baumgartner@philos.uzh.ch, or collected locally by running the scripts xx--yy. The analyses for Study 1 & 2 can be found at `src/xx-study-1.R` and `src/xx-study-2.R`, respectively. All plot results are available at `output/paper/`, and are reproducible with the aforementioned scripts.
