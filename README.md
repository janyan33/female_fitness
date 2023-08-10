# File directory

I've added an RMarkdown file which generates a Word doc detailing the updated analyses for Experiments 1 (excluding longevity because this remained unchanged) and 3.

Experiment 1: Female fitness
Everything is located in the female_fitness folder
R project file: female_fitness.Rproj
Code for analyses and visualization: In scripts folder 
	1. Longevity: longevity_analyses.R
	2. Eggs: egg_analyses.R
	3. Hatchlings: Hatchlings.R

Experiment 2: Female insemination status
Everything is located in the female_insem_status folder
Code for analyses and visualization: In scripts folder 
	File name: observations_analysis.R
Code for creating network visualizations: In scripts folder 
	File name: sexual_networks.R

Experiment 3: Successive traumatic inseminations (we called this female_coercion at the time of the experiment)
Everything is also located in the female fitness folder
R project file: female_fitness.Rproj
Code for analyses and visualization: In female_coercion folder 
	File name: female_coercion.R

I've also added the code for when I analyzed Experiment 3 data without the day effects correction to show that results are the same either way. This can be found on Lines 75 - 89 of the female_coercion.R script. 