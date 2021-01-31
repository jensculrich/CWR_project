# CWR_project

## Metadata
### GRIN_data_US_raw_data
  table downloaded from https://npgsweb.ars-grin.gov/gringlobal/taxon/taxonomysearchcwr, 
  with filter setting "Country=United States" and "Primary, Secondary, Tertiary, Graftstock" all selected

### GRIN_data_Canada_raw_data
  table downloaded from https://npgsweb.ars-grin.gov/gringlobal/taxon/taxonomysearchcwr, 
  with filter setting "Country=Canada" and "Primary, Secondary, Tertiary, Graftstock" all selected
  
### GRIN_data_US_cleaned_data
  to match the format for crossreferencing with the PNAS USA CWR inventory, I removed species author names and duplicate rows 
  for each CWR (e.g. Prunus pumila is a CWR of apricot and cherry, I removed rows so that there is only one row containing unique species name Prunus pumila)

### GRIN_data_Canada_cleaned_data
  to match the format for crossreferencing with the PNAS USA CWR inventory, I removed species author names and duplicate rows 
  for each CWR (e.g. Prunus pumila is a CWR of apricot and cherry, I removed rows so that there is only one row containing unique species name Prunus pumila)

### pnas_tableS1_USinventory_1A_1B_1C
  USA CWR inventory from https://www.pnas.org/content/117/52/33351/tab-figures-data
  limited to CWR relationship categories 1A, 1B, and 1C
  
### pnas_tableS1_USinventory_full_species_list
  full USA CWR inventory from https://www.pnas.org/content/117/52/33351/tab-figures-data
