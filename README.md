# Missing diagnoses during the COVID-19 pandemic: a year in review

This is the data repository for the project "Missing diagnoses during the COVID-19 pandemic: a year in review".

- Héctor Pifarré i Arolas, Centre for Research in Health Economics, Universitat Pompeu Fabra. For questions regarding the repo, please email hector.pifarre@upf.edu.
- Josep Vidal-Alaball, Health Promotion in Rural Areas Research Group, Ger\`encia Territorial de la Catalunya Central,Institut Catal`a de la Salut
- Joan Gil, Centre for Research in Health Economics, Universitat Pompeu Fabra. 
- Francesc López Seguí, University of Wisconsin-Madison.
- Catia Nicodemo: Centre of Organisation, Department of Primary Economics, University of Oxford. E
- Marc Sáez, Max Planck Institute for Demographic Research. 

All code is included in the `Code` folder and data in `Data` folder, which reproduce analysis in the project.

# References
Data 



# Code file descriptions
- `A - 1_Mortality_baseline.R`: mortality baseline etimation from weekly mortality.
- `A - 2_Excess_mortality_2020`: calculations of 2020 excess mortality.
- `B - Excess YLL.R`: calculations of YLL for excess mortality.
- `B - Other Causes YLL.R`: calculations of YLL for other causes of mortality.
- `B - Projected YLL.R`: calculations of YLL for projected scenarios.
- `C - Compilation main article measures.R`: creates measures cited throughout manuscript.
- `C - Compilation final measures.R`: prepares data to create the measures cited throughout manuscript.
- `C - Final tables.R`: creates tables in manuscript, and a few extra.
- `C - Figures.R`: creates plots, some used in manuscript.

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
