# a-ABCD

########
CITATION
########

Rapuano, KM, Conley, MI, Juliano, AC, Conan, GM, Maza, MT, Woodman, K, Martinez, SA, Earl, E, Perrone, A, Feczko, E, Fair, DA, Watts, R, Casey, BJ, Rosenberg, MD. (2022). An open-access accelerated adult equivalent of the ABCD Study neuroimaging dataset (a-ABCD). NeuroImage.

########
DATA
########

a-ABCD_behav.csv: a-ABCD participant task performance data from the in-scanner stop-signal, monetary incentive delay, and emotional n-back tasks and the post-scan recognition memory test for EN-back stimuli. 

a-ABCD_mood_feelings.csv: Participants rated how relaxed, happy, scared, awake, upset, angry, excited, tired, sleepy, and sad they felt on a scale from 1 (“very slightly or not at all”) to 5 (“extremely”) immediately before and after each scan session. This file includes the a-ABCD participants' self-reported mood and feelings data. 

########
SCRIPTS
########

a-ABCD_Figure1.R: Loads curated ABCD data files (which can be downloaded from https://nda.nih.gov/abcd in accordance with an approved Data Use Certification [https://nda.nih.gov/abcd/request-access]) and a-ABCD behavioral data. Generates two plots visualizing the distributions of task performance data in ABCD and a-ABCD Study datasets. Performs percentile analyses reported in the "Overall task performance" section of the text.

a-ABCD_Figure2.R: Loads a-ABCD behavioral data and generates plot showing change in task performance across sessions. 

a-ABCD_behav_stats.R: Loads a-ABCD behavioral data and performs analyses of task performance data reported in the "Changes in performance across sessions" section of the text.

a-ABCD_mood_feelings.R: Loads a-ABCD self-reported mood and feeling questionnaire data and performs analyses reported in the "Self-reported mood and feeling questionnaires" section of the text.
