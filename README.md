# Swedish Corona Statistics

## Feedback

Please share your feedback in the [issue-tracker](https://github.com/adamaltmejd/covid/issues).

## Replication

To build the graphs locally, clone the repository, set the top directory as the working directory in R and run the code below. You need to have the font [EB Garamond](https://fonts.google.com/specimen/EB+Garamond) installed and R built with Cairo support (check `capabilities()` if you are unsure).

```
renv::restore()
drake::r_make()
```

If you prefer working in Python, @morberg has put together a [Python notebook](https://github.com/morberg/covid-notebook) using the same data.

Other useful sites to track Swedish Covid-19 statistics:

* <https://platz.se/coronavirus/>
* <https://c19.se>
