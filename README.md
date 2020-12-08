# Missing diagnoses during the COVID-19 pandemic: a year in review

This is the data repository for the project "Missing diagnoses during the COVID-19 pandemic: a year in review".

- Héctor Pifarré i Arolas, Centre for Research in Health Economics, Universitat Pompeu Fabra. For questions regarding the repo, please email hector.pifarre@upf.edu.
- Josep Vidal-Alaball, Health Promotion in Rural Areas Research Group, Gerència Territorial de la Catalunya Central,Institut Català de la Salut
- Joan Gil, Universitat de Barcelona.
- Francesc López Seguí, Centre for Research in Health Economics, Universitat Pompeu Fabra.
- Catia Nicodemo: Centre of Organisation, Department of Primary Economics, University of Oxford. 
- Marc Sáez, Universitat de Girona.

All code is included in the `Code` folder and data in `Data` folder, which reproduce analysis in the project.

# References
The data was extracted from the computerised medical record of the Information System for Primary Care Services (SISAP in Catalan) of the Catalan Institute of Health, in Barcelona, Spain. These data are not publicly available, and restrictions apply to the availability of the data used for the current study. These data are available upon reasonable request addressed to the Catalan Institute of Health. The usage of the data in the current study is regulated by data confidentiality agreements between the researchers of the team and the Catalan Institute of Health and reviewed by the Institutional Committee for Ethical Review of Projects at Universitat Pompeu Fabra. 

To support scientific transparency, we have asked and received permission from the Catalan Institute of Health to post a de-identified and appropriately redacted random sample of the data for replication purposes.



# Code file descriptions
- `A - Datavisits cleaning final data sample version.R`: raw data reading and preparation; this is the version with redacted sample data.
- `A - ABS data reading.R`: population data at primary care team level.
- `A - Diagnostic codebook.R`: ICD-10 codebook for reference.
- `A - Panel ABS.R`: panel structure template for the data.
- `B - Detections management.R`: identifies new diagnoses in the data.
- `C - Detections analysis.R`: creates a panel of new diagnosis at the ICD-10 chapter level.
- `C - Robustness subchapters.R`: creates a panel of new diagnosis at the ICD-10 sub-chapter level.
- `C - Robustness subchapters.R`: creates a panel of new diagnosis at the ICD-10 sub-chapter level.
- `D - Final data for plots.R`: compiles the final data for visualization.
- `E - Visualization.R`: creates the main figure and a variety of descriptive statistics used in the article (and some extra).


# Main data file descriptions
- `stmf_5.rds`: Short term mortality fluctuations in 5-years age groups
- `OffsetsHMD.rds`: Exposures for mortality baseline estimation
- `total_deaths_country_b.rds`: Total number of deaths, both genders combined
- `deaths_age_country_b.rds`: Age distribution of deaths per country -- both
- `YLL_covid19_age_cummulative.rds`: Age distribution of deaths per country and YLLs per age
- `YLL_covid19_age_cummulative_countries.rds`:Age distribution of deaths per country and YLLs per age ++ cut offs
- `YLL_covid19_complete.rds`: YLL losts per death, rates, absolutes both gendered at not
- `YLL_lost_by_age_global.rds`: YLL lost per age all countries combined, then at cut offs
- `YLL_lost_by_age_global_cut_offs.rds`
- `Age_death_descriptives_all.rds`: Average age at which people die from covid gendered and not
- `YLL_other_causes_comparison.rds`: Comparisons to other causes
- `YLL_projections.rds`: Projections vs current
- `YLL.comparison_excess.rds`: Excess mortality
- `ages_at_death_global.rds` and `avgsd_age_death_both_global.rds`: Average age at death, age distribution at death
- `results.gender.total.rds`: gender totals
