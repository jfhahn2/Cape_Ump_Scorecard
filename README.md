# Cape_Ump_Scorecard
Generates Cape Cod League Umpire Scorecards from Trackman CSV File in R

To generate an Ump Scorecard:

1. Set Umpire Name on Line 11
2. Read in Trackman File on Line 18
3. Run line 1 through line 70
4. Find the misses.csv file written on line 70
5. In the Bases column of misses.csv, add the correct number of baserunners for each pitch
   5a. Bases empty = 0, Runner on 3rd = 3, Bases Loaded = 123, etc.
6. Read in the updated misses.csv file on Line 76
7. Run rest of code

