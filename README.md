
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dataInfrastructures

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/petrpajdla/dataInfrastructures/master?urlpath=rstudio)

This repository contains the data, code and text of my PhD thesis. This
is still a **work-in-progress** thing, if you want to have a look at it,
feel free to do so, but consider yourself warned.

> Pjdla, P., (2023). *Archaeology Data Infrastructures*. Department of
> Archaeology and Museology, Faculty of Arts, Masaryk University.
> Accessed 17 Feb 2023. Online at
> <https://petrpajdla.github.io/dataInfrastructures/>

### How to cite

Please cite this compendium as:

> Pajdla, P., (2023). *Compendium of R code and data for Archaeology
> Data Infrastructures*. Accessed 17 Feb 2023. Online at
> <https://petrpajdla.github.io/dataInfrastructures/>

## Contents

The **analysis** directory contains:

- [:file_folder: index](/index.qmd): Quarto source document containing
- the index (preface) of the thesis, the landing page.
- [:file_folder: chapters](/chapters/): Individual chapters of the
  thesis.
- [:file_folder: appendices](/appendices/): Appendices to the main text.
- [:file_folder: data](/analysis/data): Data used in the analysis.
- [:file_folder: figures](/analysis/figures): Plots and other
  illustrations
- [:file_folder:
  supplementary-materials](/analysis/supplementary-materials):
  Supplementary materials including notes and other documents prepared
  and collected during the analysis.

## How to run in your browser or download and run locally

The research compendium was initially set up with the
[rrtools](https://github.com/benmarwick/rrtools/) package. This research
compendium has been developed using the statistical programming language
R. To work with the compendium, you will need installed on your computer
the [R software](https://cloud.r-project.org/) itself and optionally
[RStudio Desktop](https://rstudio.com/products/rstudio/download/).

You can download the compendium as a zip from from this URL:
[master.zip](/archive/master.zip). After unzipping: - open the `.Rproj`
file in RStudio - run `devtools::install()` to ensure you have the
packages this analysis depends on (also listed in the
[DESCRIPTION](/DESCRIPTION) file). - finally, open
`analysis/paper/paper.Rmd` and knit to produce the `paper.docx`, or run
`rmarkdown::render("analysis/paper/paper.Rmd")` in the R console

### Licenses

**Text and figures :** [CC
BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC 0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.
