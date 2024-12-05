# CHD Complexity Score

An algorithm for scoring the complexity of a congenital heart disease patient's diagnosis based on the ESC 2020 guidelines (<https://doi.org/10.1093/eurheartj/ehaa554>)

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

## Contents

-   **DiagnosisReferenceTable.csv** - contained the expert provided complexity score for each EPCC diagnosis code used in the CHAANZ CHD Registry. Complexity scores were assigned by CHD clinicians based on ESC guidelines or expert knowledge where necessary.

-   **diagnosis_example.csv** - an example of the input patient diagnostic data

-   **procedure_example.csv** - an example of the input patient procedural data

-   **complexity_procedure.R -** Contains the function that will calculate patient complexity

-   **Run_Complexity.Rmd** - An example of the function in use.

## Using the `esc.complexity.procedure()` function

### Inputs

**`patient.diagnosis`** - a diagnosis-level data frame with two variables:

-   `patient_id` - Unique patient identifier that can be used to group codes by patients and to match with procedures

-   `diagnosis_code` - Character, EPCC Code denoting the patient's diagnoses

**`patient.procedures`** - a procedure-level data frame with two variables:

-   `patient_id` - Unique patient identifier that can be used to group codes by patients and to match with diagnoses

-   `procedure_code` - Character, EPCC Code denoting the patient's procedures

**`complexity.reference`** - List of EPCC diagnosis codes with complexity score

-   This is provided as "DiagnosisReferenceTable.csv"

### Output

**`patient.complexity`** - a patient-level data frame with 6 variables:

-   `patient_id` - Unique patient identifier that can be used to group EPCC_codes by patients

-   `diagnosis` - Nested dataframe with diagnosis level information for each patient

-   `no_dx` - Number of diagnoses

-   `patientcomplexity` - The numeric complexity score for a patient, 0.5 - missing, 1 - mild, 1.5/2.5 - unknown, 2 - moderate, 3 - severe

-   `uncertaincomplexity` - Flag for patients whose complexity is determined by an uncertain diagnosis (i.e. score of 0.5, 1.5 or 2.5)

-   `patientclassification` - The final classification score
