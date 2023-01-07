# simulation_sample_2
This repository hosts the script, dataset, and visualization outputs related to the comparison of employment outcomes between the Indonesian Labor Force Survey (Sakernas) and 
the World Bank-administered Indonesian High-Frequency (HiFy) Monitoring datasets. 

The simulation exercise is based on an ongoing work to monitor COVID-19 effects on employment across gender groups in Indonesia, 
with [Daniel Halim](https://www.danielhalim.com/) and [Ririn Salwa Purnamasari](https://www.worldbank.org/en/about/people/r/ririn-salwa-purnamasari). 
The work consists of two layers of analysis from different data sources: the nationally-representative labor force survey dataset (Sakernas), and 
the HiFy data. Due to the different nature of the datasets, the findings from these two analyses are expected to supplement each other. 

Through the simulation exercise conducted in this repository, the author aims to demonstrate the value of adding a second, non-household head, adult sample 
in high-frequency monitoring surveys to maintain representativeness at the national level. 

The visual outputs suggest that the work participation among the HiFy second samples approach that of the national population if a similar sample-drawing scheme 
is implemented on the Sakernas data.

## Data sources
The two datasets used in the simulation exercise are as follows:
1. Indonesian National Labor Force Survey (Sakernas); Sakernas is a nationally-representative labor force survey that is administered twice a year, in February and in August. 
The February and August rounds differ importantly in terms of the level of representativeness--while August rounds are representative at the district level, February 
rounds are only representative at the province level. Unfortunately, due to data distribution policy, the author is unable to share the Sakernas datasets on this repository.

2. Indonesia High-Frequency (HiFy) COVID-19 Monitoring dataset; the dataset is a cleaned version of Round 5 Indonesia HiFy datasets, which are publicly accessible 
at this [link](https://microdata.worldbank.org/index.php/catalog/3938).


## Structure
The repository contains two main folders:

1. **script**: contains the main R scripts that clean the raw datasets, simulate the Sakernas data, and visualize the simulation results
2. **output**: the folder contains the visualization output from the simulation exercises.
