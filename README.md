# Research Evaluation of Faculty

This R Shiny app:

1. summarizes publication data for Faculty of Mathematics and Computer Science, Adam Mickiewicz University in Poznań gathered from:
   1. [Reporting Module of Polish Scientific Bibliography](https://pbn.nauka.gov.pl/pbn-report-web/) (preprocessed by [export cleaner](https://github.com/andre-wojtowicz/pbn-export-cleaner)),
   2. [Scopus (SciVal)](https://www.scival.com/).
2. presents the journal and conferences lists from Polish Ministry of Science and Higher Education.

The app has modular design and supports internationalization. It also uses non-standard way of loading data, i.e. is utilizes Rserve (normally all data is loaded in `global.R` but moving it to separate permanent R server-like process can speed-up first time page loading).
