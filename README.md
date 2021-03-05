# daily_COVID_deaths_Germany

This repository was originally created to attempt to infer the number of deaths due to COVID each day in Germany, by Bundesland, using data updated daily by the Robert Koch Institute (RKI). The daily csv files used can be found [here][1].

For privacy reasons, the RKI does not directly report the number of deaths that occur each day. And although the updated data posted each day by the RKI contains a column for reporting date ("Meldedatum"), this refers to the date on which the case was originally reported, not the date on which the eventual death was reported.

To estimate the number of deaths each day, I assumed that deaths occurred on the day on which they were first reported. Due to reporting delays, this assumption won't hold for all cases, but this seemed like the simplest way to at least estimate the true number of deaths.

We have since requested weekly data directly from the RKI, so this repository is no longer maintained.

It has since come to my attention that [RiskLayer GmbH][2] seems to be taking a similar approach to estimating daily death counts, both on the Bundesland- and the Landkreis-level. The resulting data, as well as case data compiled over the course of the pandemic, can be found [here][3].

[1]: https://github.com/CharlesStr/CSV-Dateien-mit-Covid-19-Infektionen-
[2]: http://www.risklayer.com/en/
[3]: https://github.com/jgehrcke/covid-19-germany-gae
