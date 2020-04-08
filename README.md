# Swedish Corona Statistics

# Cron

Use crontab to ensure new statistics are downloaded each day (published at 14.00, we download a couple of minutes later to be sure). But replace path with your working directory.

```
10 14 * * * <path_to_wd>/src/cron.sh
```
