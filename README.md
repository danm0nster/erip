[![CC BY 4.0][cc-by-shield]][cc-by]
[![DOI](https://zenodo.org/badge/1000169037.svg)](https://doi.org/10.5281/zenodo.17865640)


# Replication data and code for  _The Role of Emotional Regulation in Politics: Evidence from Cross-Country Surveys_

This repository contains data and R code to replicate the results in the article

> Julie Hassing Nielsen & Dan Mønster
>
> The Role of Emotional Regulation in Politics: Evidence from Cross-Country Surveys
>
> Political Psychology, Volume **47**, Issue 1, e70094, February 2026.
>
> [DOI: 10.1111/pops.70094](https://doi.org/10.1111/pops.70094)


## Data
The following two data files are included:
* `survey_dk.csv`: Data from survey in Denmark.
* `survey_us.csv`: Data from survey in United States.

If you use any of the datasets above, please cite our article, and this repository.

For detailed information about the variables in the data sets, see Nielsen & Mønster (YYYY).

## Replication script
To replicate the results, the replication script should be run under [R version 4.1.2](https://cran.r-project.org). A later version may work, but this cannot be guaranteed.

To ensure replicability of the script, we use the [groundhog package](https://groundhogr.com/). Using groundhog guarantees that you will use the same versions of all packages that we used when producing the results in the paper. Therefore, you only need to install the `groundhog` package, which will the take care of installing the correct versions of all other packages without interfering with your current R installation. You can install `groundhog` with the following command:
```
install.packages('groundhog')
```

Running the replication script will generate the tables (in HTML format) in the article and the online appendix. Some tables are split into two parts &mdash; one for each of the two countries, Denmark and the US. In some cases the generated tables are formatted slightly differently from the final tables in the article or online appendix, but the information is the same.

Run the script in a new R session, or restart the R session in RStudio, to avoid conflicts with other versions of required packages that may already be loaded. The most robust way is to use `Rscript`(Mac OS and Linux) or `Rscript.exe` (Windows) after installing `groundhog` as shown above. Run the replication script on the command line like this:
```
Rscript replication.R
```

On Windows:
```
Rscript.exe replication.R
```

**Note:** Running the script requires that your current working directory is set to where the script is located, and that the `Rscript` executable is in your search path. If this is not the case, consult the R documentation.

## Results
For reference, we have included the output of running the script in this repository in the directory `results`. 
With R version 4.1.2 the replication script should result in identical output.

The script uses R (R Core Team, 2021) and the following packages
* `groundhog`: Simonsohn & Gruson, 2025.
* `dplyr`: Wickham et al., 2021.
* `table1`: Rich, 2021.
* `markdown`: Allaire et al., 2019.
* `psych`: Revelle 2021.
* `kableExtra`: Zhu, 2021.
* `lme4`: Bates et al., 2015.
* `MuMIn`: Bartón, 2020.
* `lmerTest`: Kuznetsova et al., 2017.
* `texreg`: Leifeld, 2013.
* `effectsize`: Ben-Shachar et al., 2020.

## References
Allaire, JJ, Jeffrey Horner, Yihui Xie, Vicent Marti and Natacha Porte (2019). markdown: Render Markdown with the C Library 'Sundown'. https://CRAN.R-project.org/package=markdown

Bartoń, Kamil (2020). MuMIn: Multi-Model Inference.  https://CRAN.R-project.org/package=MuMIn

Ben-Shachar M, Lüdecke D, Makowski D (2020). effectsize: Estimation of Effect Size Indices and Standardized Parameters. Journal of Open Source Software, 5(56), 2815. doi: 10.21105/joss.02815

Douglas Bates, Martin Maechler, Ben Bolker, Steve Walker (2015). Fitting Linear Mixed-Effects Models Using lme4. Journal of Statistical Software, 67(1), 1-48. DOI: 10.18637/jss.v067.i01.

Kuznetsova A, Brockhoff PB, Christensen RHB (2017). “lmerTest Package: Tests in Linear Mixed Effects Models.” _Journal of Statistical Software_, 82(13), 1-26. doi: 10.18637/jss.v082.i13 (URL: https://doi.org/10.18637/jss.v082.i13).

Lang, P. J., Bradley, M. M., & Cuthbert, B. N. (1997). International affective picture system (IAPS): Technical manual and affective ratings. NIMH Center for the Study of Emotion and Attention, 1(39-58), 3.

Leifeld, Philip (2013). texreg: Conversion of Statistical Model Output in R to LaTeX and HTML Tables. Journal of Statistical Software, 55(8), 1-24. URL http://dx.doi.org/10.18637/jss.v055.i08.

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/.

Revelle, W. (2021) psych: Procedures for Personality and Psychological Research, Northwestern University, Evanston, Illinois, USA, https://CRAN.R-project.org/package=psych

Rich, Benjamin (2021). table1: Tables of Descriptive Statistics in HTML.  https://CRAN.R-project.org/package=table1

Simonsohn, Uri and Hugo Gruson (2025). groundhog: Version-Control for CRAN, GitHub, and GitLab Packages. R package version 3.2.3. https://CRAN.R-project.org/package=groundhog

Wickham, Hadley, Romain François, Lionel Henry and Kirill Müller (2021). dplyr: A Grammar of Data Manipulation.  https://CRAN.R-project.org/package=dplyr

Zhu, Hao (2021). kableExtra: Construct Complex Table with 'kable' and Pipe Syntax.  https://CRAN.R-project.org/package=kableExtra

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg
