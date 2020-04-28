# Swedish Corona Statistics

## Feedback

Please share your feedback in the [issue-tracker](https://github.com/adamaltmejd/covid/issues).

## Replication

To build the graphs locally, clone the repository, set the top directory as the working directory in R and run the code below. You need to have the font [EB Garamond](https://fonts.google.com/specimen/EB+Garamond) installed and R built with Cairo support (check `capabilities()` if you are unsure).

```
renv::restore()
drake::r_make()
```
