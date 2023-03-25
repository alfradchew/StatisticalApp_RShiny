## Q1

INTRODUCTION 
---------------------

Question 1 contains the following file:

 * __completed__: To store the csv files after finished processing so that the next cronjob won't reprocess the files again in the input folder. Name of file is in "YYYYMMDD HH application_dataset_i.csv" format
 * __input__: Store the raw csv files
 * __successful_cases__: Store the successful applicants csv files
 * __unsuccessful_cases__: Store the unsuccessful applicants csv files
 * __cronjob.txt__: Command line for crontab
 median
 * __helper.py__: Helper file to store all the functions
 * __logfiles.txt__: Contain logs of the the cronjob
 * __main.py__: Provides the melt function for the correlation plot
 * __pipeline.sh__: Shell file to execute the cronjob


### REQUIREMENTS
---------------------

A python environment which can run the following packages - __pandas__, __logging__, __shutil__, __os__, __hashlib__, __re__, __dateutil__ and __datetime__

Root access to your computer / vm

For macOS, ensure that __cron__ and terminal.app has __Full Disk Access__. For reference, refer to these links:
[Link 1](https://medium.com/vunamhung/granting-full-disk-access-to-cron-29d2267fbe62), [Link 2](https://osxdaily.com/2020/04/27/fix-cron-permissions-macos-full-disk-access/)


### INSTRUCTIONS
---------------------

Step 1: Clone the repo https://github.com/alfradchew/de.git

Step 2: Give the __pipeline.sh__ executable access

```
chmod 744 pipeline.sh
```

Step 3: Edit line 19 of __main.py__ to reflect the current working directory

```
# Change to your directory
os.chdir('/Users/alfradchew/Downloads/de/q1/')
```

Step 4: Edit __pipeline.sh__ to reflect your python path and working directory

```
# Change to your python path and working directory
/Users/alfradchew/opt/anaconda3/envs/alfrad/bin/python /Users/alfradchew/Downloads/de/q1/main.py
```

Step 5: Edit __cronjob.txt__ to reflect your working directory

```
# Change to your working directory
00 * * * * /Users/alfradchew/Downloads/de/q1/pipeline.sh >> /Users/alfradchew/Downloads/de/q1/logfiles.txt 2>&1
```

Step 6: Initiate cronjob

```
crontab cronjob.txt
```

Step 7: View your cronjob

```
crontab -l

00 * * * * /Users/alfradchew/Downloads/de/q1/pipeline.sh >> /Users/alfradchew/Downloads/de/q1/logfiles.txt 2>&1
```
