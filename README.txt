|-- Economics_Paper_Project/
    |-- README.md              # Project description and instructions
    |-- data/
        |-- raw/               # Raw, unmodified data
            |-- gas_stations.csv
            |-- gas_prices.csv
            |-- oil_prices.csv
            |-- population.csv
        |-- processed/         # Cleaned and merged data
            |-- cleaned_gas_stations.csv
            |-- cleaned_gas_prices.csv
            |-- merged_data.csv
    |-- R/
        |-- scripts/
            |-- 01_data_cleaning.R      # Data cleaning scripts
            |-- 02_data_merging.R      # Data merging scripts
            |-- 03_analysis.R          # Analysis scripts
        |-- functions/                 # Any additional custom R functions
            |-- custom_function1.R
            |-- custom_function2.R
    |-- outputs/
        |-- figures/            # Graphs from analysis
            |-- figure1.png
            |-- figure2.png
        |-- tables/             # Tables from analysis
            |-- table1.csv
            |-- table2.csv
    |-- paper/
        |-- paper_draft.Rmd    # R Markdown document for the paper
        |-- paper_draft.pdf    # Compiled version of the paper
        |-- references.bib     # BibTeX file for references
    |-- .gitignore             # (Optional) If using git, specify files/folders to ignore
    |-- .Rproj                # (Optional) R project file if using RStudio projects
