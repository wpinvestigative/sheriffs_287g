[This data is published under an [Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) license](https://creativecommons.org/licenses/by-nc-sa/4.0/)]

# About this story

[Under Trump, ICE aggressively recruited sheriffs as partners to question and detain undocumented immigrants](https://www.washingtonpost.com/)

# Methodology

The analysis of low-level arrests was based on FBI Uniform Crime Reporting data, which is self-reported by agencies. After consulting criminologists, The Post categorized the following crimes as low-level: vandalism, liquor law violations, drunkenness, gambling, suspicion, disorderly conduct, vagrancy, curfew violations and marijuana possession. Also included were arrests made on suspicion of criminal activity, but not leading to charges and arrests labeled as “other offenses.” The data did not include driving offenses or drunken driving. Sheriffs agencies were identified with the help of the [Inter-University Consortium for Political and Social Research](https://www.icpsr.umich.edu/web/pages/NACJD/index.html)'s agency identifiers crosswalk, and the Post’s reporting narrowed down those that participated in the 287(g) program. The number of officers per agency, as described by UCR, does not include civilian employees, such as clerks, jailers or correctional officers without arresting powers. More details on the methodology can be found on [Github](https://github.com/wpinvestigative/sheriffs_287g).


# About the data

The data is pulled from the FBI UCR API using a modified version of the FBI R package. Agencies are identified from the crosswalk data. 287(g) counties are flagged by the Post.

Special thanks to Aaron Littman and Jacob Kaplan for advice in working with UCR data. Additional thanks to Kaplan for creating the [fbi package](https://github.com/jacobkap/fbi) to help explore the FBI's [Crime Data Explorer](https://crime-data-explorer.fr.cloud.gov/) api. Also thanks to the [Inter-university Consortium for Political and Social Research](https://www.icpsr.umich.edu/web/pages/NACJD/index.html) for their [Law Enforcement Agency Identifiers Crosswalk](https://www.icpsr.umich.edu/web/NACJD/studies/35158/summary).

# Folders of note in this repo

* **[data/raw_data](data/raw_data)** - downloaded data from the FBI UCR API and 287(g) designations

* **[scripts/import](scripts/import)** - Scripts to import FBI UCR data via API

* **[outputs/findings](https://github.com/wpinvestigative/sheriffs_287g/tree/main/outputs/findings)** - Exploratory notebooks

# Notebooks

* [Analysis](http://wpinvestigative.github.io/sheriffs_287g/outputs/findings/01_analysis.html) - Exploratory analysis