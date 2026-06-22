

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1ea1668b-6ae1-4cca-af3a-430eed3928a6"),
    all_cohort1=Input(rid="ri.foundry.main.dataset.ca3e686b-a2cd-4f80-8494-fc7458661c3e"),
    condition_occurrence=Input(rid="ri.foundry.main.dataset.526c0452-7c18-46b6-8a5d-59be0b79a10b"),
    icd_to_snomed=Input(rid="ri.foundry.main.dataset.b35ab8eb-05a3-4fbd-a60a-d647642e7fc5")
)
WITH cohort_conditions_snomed AS (
    SELECT co.person_id,
    co.condition_concept_id,  
    co.condition_source_concept_id,
    co.condition_concept_name,
    co.condition_start_date,
    co.condition_end_date,
    co.data_partner_id,
    ch.has_lsd,
    ch.COVID_pos_indicator,
    ch.number_of_visits,
    ch.days_observed,
    ch.lsd_condition_name,
    ch.subclass
FROM all_cohort1 ch
JOIN condition_occurrence co ON ch.person_id = co.person_id),
icd_map AS (
    SELECT icd_concept_name,
    icd_concept_id,
    vocabulary_id,
    concept_code
    FROM icd_to_snomed
)
SELECT DISTINCT
    person_id,
    condition_concept_id,  
    condition_concept_name,
    condition_start_date,
    condition_end_date,
    data_partner_id,
    has_lsd,
    lsd_condition_name,
    subclass,
    COVID_pos_indicator,
    number_of_visits,
    days_observed,
    icd_concept_name,
    icd_concept_id,
    vocabulary_id,
    concept_code
FROM cohort_conditions_snomed
LEFT JOIN icd_map ON cohort_conditions_snomed.condition_source_concept_id = icd_map.icd_concept_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e29718de-5a2e-4838-93aa-4da82d3d656b"),
    all_cohort_conditions=Input(rid="ri.foundry.main.dataset.1ea1668b-6ae1-4cca-af3a-430eed3928a6")
)
SELECT DISTINCT *
FROM all_cohort_conditions
WHERE concept_code IS NOT NULL

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.fc6a2e9a-5d95-43ea-b5dc-4a8d4972c867"),
    all_cohort1=Input(rid="ri.foundry.main.dataset.ca3e686b-a2cd-4f80-8494-fc7458661c3e"),
    death=Input(rid="ri.foundry.main.dataset.9c6c12b0-8e09-4691-91e4-e5ff3f837e69")
)
WITH new AS (SELECT * FROM death WHERE death_date IS NOT NULL),
list AS (SELECT DISTINCT person_id,
    concat_ws(';', collect_list(cause_concept_id)) AS cause_concept_id,
    concat_ws(';', collect_list(cause_concept_name)) AS cause_concept_name,
    concat_ws(';', collect_list(death_type_concept_id)) AS death_type_concept_id,
    max(death_date) AS death_date,
    any_value(data_partner_id) AS data_partner_id
FROM new
GROUP BY person_id),
all_cohort_death AS (SELECT list.*,
    datediff(death_date, '2025-07-11') AS days_diff,
    cohort.has_lsd,
    cohort.lsd_condition_name,
    cohort.COVID_pos_indicator
    FROM list
JOIN all_cohort1 cohort ON cohort.person_id = list.person_id)

SELECT *
FROM all_cohort_death
WHERE days_diff BETWEEN -2555 AND 0 -- seven years, greater than this is an errant death_date

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.5432bf45-5660-4a0b-a8e7-f194abcbd593"),
    all_cohort1=Input(rid="ri.foundry.main.dataset.ca3e686b-a2cd-4f80-8494-fc7458661c3e"),
    drug_exposure=Input(rid="ri.foundry.main.dataset.fd499c1d-4b37-4cda-b94f-b7bf70a014da")
)
WITH exposure AS (SELECT drug_exposure.*,
    co.has_lsd,
    co.COVID_pos_indicator,
    co.lsd_condition_name
FROM drug_exposure
JOIN all_cohort1 co ON co.person_id = drug_exposure.person_id)

SELECT e.*
FROM exposure e
WHERE drug_exposure_start_date IS NOT NULL 
    AND drug_exposure_start_date <= DATE '2025-07-11'

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ebf51d85-06d3-4de0-ad87-a0463f8764fe"),
    all_cohort_drug_exposure=Input(rid="ri.foundry.main.dataset.5432bf45-5660-4a0b-a8e7-f194abcbd593"),
    lsd_drugs=Input(rid="ri.foundry.main.dataset.682e5590-21f8-49d6-82b9-7b1066ea95ce")
)
SELECT cd.person_id,
    cd.drug_concept_id,
    cd.drug_concept_name,
    cd.data_partner_id,
    cd.lsd_condition_name,
    cd.has_lsd,
    cd.COVID_pos_indicator,
    f.drug_name
FROM all_cohort_drug_exposure cd 
JOIN lsd_drugs f ON f.concept_id = cd.drug_concept_id
WHERE has_lsd = 1

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2cabfb3b-fa18-4578-b402-1bc737fc35e5"),
    all_cohort_conditions_icd=Input(rid="ri.foundry.main.dataset.e29718de-5a2e-4838-93aa-4da82d3d656b"),
    phecode_map=Input(rid="ri.foundry.main.dataset.c2bec0d5-a47e-420c-9f1f-1526771f00b7")
)
SELECT DISTINCT 
    cc.person_id,
    pm.phecode,
    SUBSTRING_INDEX(pm.phecode, '.', 1) AS prefix,
    pm.phecode_string,
    pm.category,
    pm.ICD,
    cc.condition_start_date,
    cc.condition_end_date,
    cc.days_observed,
    cc.number_of_visits,
    cc.COVID_pos_indicator,
    cc.has_lsd,
    cc.lsd_condition_name,
    cc.subclass
FROM all_cohort_conditions_icd cc
JOIN phecode_map pm 
    ON cc.concept_code = pm.ICD
WHERE cc.concept_code IS NOT NULL 
    AND cc.concept_code NOT IN ('U07.1', 'U00', 'J12.81', 'J12.82', 'B34.2', 'B97.21', 'B97.29', 'B97.2') -- exclude specific and generic covid diagnoses

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.602a59e0-4ad7-4937-abb3-21cbad37838d"),
    all_cohort1=Input(rid="ri.foundry.main.dataset.ca3e686b-a2cd-4f80-8494-fc7458661c3e"),
    vaccine_fact_de_identified=Input(rid="ri.foundry.main.dataset.8e6b9f03-c33d-4301-9114-7c8881a505ab")
)
SELECT vax.*
FROM vaccine_fact_de_identified vax
JOIN all_cohort1 co ON vax.person_id = co.person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.bf9477ec-a135-4e1c-8838-a92f12c21eb6"),
    Logic_Liaison_Covid_19_Patient_Summary_Facts_Table_De_identified_=Input(rid="ri.foundry.main.dataset.ae01d2c8-5c70-428f-a0aa-de30d587b2bb"),
    specific_lsds=Input(rid="ri.foundry.main.dataset.3500942a-19c9-4245-84fe-67d90a2cb0e4")
)
SELECT 
    L.person_id,
    L.COVID_first_PCR_or_AG_lab_positive,
    L.COVID_first_diagnosis_date,
    L.COVID_first_poslab_or_diagnosis_date,
    CAST(L.number_of_visits_before_covid AS INT) AS number_of_visits_before_covid,
    L.observation_period_before_covid,
    CAST(L.number_of_visits_post_covid AS INT) AS number_of_visits_post_covid,
    L.observation_period_post_covid,
    L.city,
    L.state,
    L.postal_code,
    L.county,
    CAST(L.age_at_covid AS INT) AS Age,
    L.race_ethnicity,
    L.data_partner_id,
    L.data_extraction_date,
    L.cdm_name,
    L.cdm_version,
    L.shift_date_yn,
    L.max_num_shift_days,
    CAST(L.BMI_max_observed_or_calculated_before_or_day_of_covid AS FLOAT) AS BMI_max_observed_or_calculated_before_or_day_of_covid,
    L.TUBERCULOSIS_before_or_day_of_covid_indicator,
    L.MILDLIVERDISEASE_before_or_day_of_covid_indicator,
    L.MODERATESEVERELIVERDISEASE_before_or_day_of_covid_indicator,
    L.THALASSEMIA_before_or_day_of_covid_indicator,
    L.RHEUMATOLOGICDISEASE_before_or_day_of_covid_indicator,
    L.DEMENTIA_before_or_day_of_covid_indicator,
    L.CONGESTIVEHEARTFAILURE_before_or_day_of_covid_indicator,
    L.SUBSTANCEUSEDISORDER_before_or_day_of_covid_indicator,
    L.DOWNSYNDROME_before_or_day_of_covid_indicator,
    L.KIDNEYDISEASE_before_or_day_of_covid_indicator,
    L.MALIGNANTCANCER_before_or_day_of_covid_indicator,
    L.DIABETESCOMPLICATED_before_or_day_of_covid_indicator,
    L.CEREBROVASCULARDISEASE_before_or_day_of_covid_indicator,
    L.PERIPHERALVASCULARDISEASE_before_or_day_of_covid_indicator,
    L.PREGNANCY_before_or_day_of_covid_indicator,
    L.HEARTFAILURE_before_or_day_of_covid_indicator,
    L.HEMIPLEGIAORPARAPLEGIA_before_or_day_of_covid_indicator,
    L.PSYCHOSIS_before_or_day_of_covid_indicator,
    L.OBESITY_before_or_day_of_covid_indicator,
    L.CORONARYARTERYDISEASE_before_or_day_of_covid_indicator,
    L.SYSTEMICCORTICOSTEROIDS_before_or_day_of_covid_indicator,
    L.DEPRESSION_before_or_day_of_covid_indicator,
    L.METASTATICSOLIDTUMORCANCERS_before_or_day_of_covid_indicator,
    L.HIVINFECTION_before_or_day_of_covid_indicator,
    L.CHRONICLUNGDISEASE_before_or_day_of_covid_indicator,
    L.PEPTICULCER_before_or_day_of_covid_indicator,
    L.SICKLECELLDISEASE_before_or_day_of_covid_indicator,
    L.MYOCARDIALINFARCTION_before_or_day_of_covid_indicator,
    L.DIABETESUNCOMPLICATED_before_or_day_of_covid_indicator,
    L.CARDIOMYOPATHIES_before_or_day_of_covid_indicator,
    L.HYPERTENSION_before_or_day_of_covid_indicator,
    L.OTHERIMMUNOCOMPROMISED_before_or_day_of_covid_indicator,
    L.Antibody_Neg_before_or_day_of_covid_indicator,
    L.PULMONARYEMBOLISM_before_or_day_of_covid_indicator,
    L.TOBACCOSMOKER_before_or_day_of_covid_indicator,
    L.SOLIDORGANORBLOODSTEMCELLTRANSPLANT_before_or_day_of_covid_indicator,
    L.Antibody_Pos_before_or_day_of_covid_indicator,
    CAST(L.number_of_COVID_vaccine_doses_before_or_day_of_covid AS INT) AS number_of_COVID_vaccine_doses_before_or_day_of_covid,
    L.LL_IMV_during_strong_covid_hospitalization_indicator,
    L.COVID_patient_death_during_strong_covid_hospitalization_indicator,
    L.COVIDREGIMENCORTICOSTEROIDS_during_strong_covid_hospitalization_indicator,
    L.COVID_diagnosis_during_strong_covid_hospitalization_indicator,
    L.REMDISIVIR_during_strong_covid_hospitalization_indicator,
    L.LL_ECMO_during_strong_covid_hospitalization_indicator,
    L.LL_IMV_during_weak_covid_hospitalization_indicator,
    L.COVID_patient_death_during_weak_covid_hospitalization_indicator,
    L.COVIDREGIMENCORTICOSTEROIDS_during_weak_covid_hospitalization_indicator,
    L.COVID_diagnosis_during_weak_covid_hospitalization_indicator,
    L.REMDISIVIR_during_weak_covid_hospitalization_indicator,
    L.LL_ECMO_during_weak_covid_hospitalization_indicator,
    CAST(L.BMI_max_observed_or_calculated_post_covid AS FLOAT) AS BMI_max_observed_or_calculated_post_covid,
    L.TUBERCULOSIS_post_covid_indicator,
    L.PCR_AG_Pos_post_covid_indicator,
    L.MILDLIVERDISEASE_post_covid_indicator,
    L.MODERATESEVERELIVERDISEASE_post_covid_indicator,
    L.PNEUMONIADUETOCOVID_post_covid_indicator,
    L.THALASSEMIA_post_covid_indicator,
    L.RHEUMATOLOGICDISEASE_post_covid_indicator,
    L.DEMENTIA_post_covid_indicator,
    L.CONGESTIVEHEARTFAILURE_post_covid_indicator,
    L.SUBSTANCEUSEDISORDER_post_covid_indicator,
    L.Long_COVID_clinic_visit_post_covid_indicator,
    L.DOWNSYNDROME_post_covid_indicator,
    L.KIDNEYDISEASE_post_covid_indicator,
    L.MALIGNANTCANCER_post_covid_indicator,
    L.MISC_post_covid_indicator,
    L.DIABETESCOMPLICATED_post_covid_indicator,
    L.CEREBROVASCULARDISEASE_post_covid_indicator,
    L.PERIPHERALVASCULARDISEASE_post_covid_indicator,
    L.PREGNANCY_post_covid_indicator,
    L.HEARTFAILURE_post_covid_indicator,
    L.HEMIPLEGIAORPARAPLEGIA_post_covid_indicator,
    L.PSYCHOSIS_post_covid_indicator,
    L.OBESITY_post_covid_indicator,
    L.CORONARYARTERYDISEASE_post_covid_indicator,
    L.PCR_AG_Neg_post_covid_indicator,
    L.SYSTEMICCORTICOSTEROIDS_post_covid_indicator,
    L.DEPRESSION_post_covid_indicator,
    L.METASTATICSOLIDTUMORCANCERS_post_covid_indicator,
    L.HIVINFECTION_post_covid_indicator,
    L.CHRONICLUNGDISEASE_post_covid_indicator,
    L.B94_8_post_covid_indicator,
    L.PEPTICULCER_post_covid_indicator,
    L.SICKLECELLDISEASE_post_covid_indicator,
    L.MYOCARDIALINFARCTION_post_covid_indicator,
    L.Long_COVID_diagnosis_post_covid_indicator,
    L.DIABETESUNCOMPLICATED_post_covid_indicator,
    L.CARDIOMYOPATHIES_post_covid_indicator,
    L.HYPERTENSION_post_covid_indicator,
    L.OTHERIMMUNOCOMPROMISED_post_covid_indicator,
    L.Antibody_Neg_post_covid_indicator,
    L.PULMONARYEMBOLISM_post_covid_indicator,
    L.TOBACCOSMOKER_post_covid_indicator,
    L.SOLIDORGANORBLOODSTEMCELLTRANSPLANT_post_covid_indicator,
    L.Antibody_Pos_post_covid_indicator,
    CAST(L.number_of_COVID_vaccine_doses_post_covid AS INT) AS number_of_COVID_vaccine_doses_post_covid,
    L.had_at_least_one_reinfection_post_covid_indicator,
    L.first_strong_COVID_ED_only_start_date,
    L.first_strong_COVID_hospitalization_start_date,
    L.first_strong_COVID_hospitalization_end_date,
    L.first_weak_COVID_ED_only_start_date,
    L.first_weak_COVID_hospitalization_start_date,
    L.first_weak_COVID_hospitalization_end_date,
    L.strong_COVID_hospitalization_length_of_stay,
    L.COVID_patient_death_indicator,
    L.death_within_specified_window_post_covid,
    L.Severity_Type,
    S.condition_concept_id,
    S.condition_source_concept_id,
    S.condition_source_value,
    S.specific_condition_name,
    S.condition_start_date,
    S.condition_end_date,
    S.data_partner_id AS lsd_data_partner_id,
    CASE 
           WHEN S.condition_concept_id IS NOT NULL THEN 1
           ELSE 0 
       END AS has_lsd,
    CASE 
            WHEN L.sex = 'MALE' THEN 'Male'
            WHEN L.sex = 'FEMALE' THEN 'Female'
            ELSE 'Unknown'
        END AS Sex,
    CASE 
            WHEN L.race = 'Other' THEN 'Other/Unknown'
            WHEN L.race IS NULL THEN 'Other/Unknown'
            ELSE L.race
        END AS Race
FROM Logic_Liaison_Covid_19_Patient_Summary_Facts_Table_De_identified_ L
LEFT JOIN specific_lsds S ON L.person_id = S.person_id
-- LONG to INT
-- number_of_visits_before_covid
-- number_of_visits_post_covid
--age_at_covid
-- number_of_COVID_vaccine_doses_before_or_day_of_covid
-- number_of_COVID_vaccine_doses_post_covid

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e29af5b3-d495-4d88-8eca-f39d24a8ac51"),
    all_lsd_imputation=Input(rid="ri.foundry.main.dataset.d383f14f-a3d6-4c9e-9a1b-de058e51dc3f"),
    all_non_lsd_imputation=Input(rid="ri.foundry.main.dataset.d8f95670-8a0e-4d11-bb50-8a28ab56cd9f")
)
SELECT *
FROM all_non_lsd_imputation

UNION

SELECT *
FROM all_lsd_imputation

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.8feaa134-5dd4-4418-a291-5f8fe583b9b8"),
    all_vaccines_deduplicated=Input(rid="ri.foundry.main.dataset.2ee63311-8ecc-4bac-a5f3-7f8c7b1a3c3d"),
    bmi_measurements=Input(rid="ri.foundry.main.dataset.5ee96688-bafb-4a95-aa4c-c9eeb7894531"),
    location=Input(rid="ri.foundry.main.dataset.77647aff-bb7e-4bce-9f04-19a971bc83ce"),
    lsd_persons=Input(rid="ri.foundry.main.dataset.431ea30f-2865-47fc-9728-2a6b88fea36f"),
    visits_df=Input(rid="ri.foundry.main.dataset.68b92e18-8033-40d6-95cf-61594bfe4c00")
)
WITH zip AS (SELECT 
    person_id,
    n.data_partner_id,
    n.lsd_condition_name,
    Sex,
    Race,
    CAST(Age AS INT) AS Age,
    has_lsd,
    COVID_pos_indicator,
    l.zip AS postal_code
FROM lsd_persons n
LEFT JOIN location l ON n.location_id = l.location_id),
w_visits AS (
    SELECT z.*,
        v.days_observed,
        CAST(v.number_of_visits AS INT) AS number_of_visits
    FROM zip z
    LEFT JOIN visits_df v ON z.person_id = v.person_id
),
w_bmi AS (
    SELECT w_visits.*,
        bmi.BMI_overall
    FROM w_visits
    LEFT JOIN bmi_measurements bmi ON w_visits.person_id = bmi.person_id
),
w_vaccine AS (
    SELECT w_bmi.*,
        CAST(COALESCE(combined_number_of_COVID_vaccine_doses, 0) AS INT) AS combined_number_of_COVID_vaccine_doses -- replace NULL with 0
    FROM w_bmi
    LEFT JOIN all_vaccines_deduplicated vax ON w_bmi.person_id = vax.person_id
)
SELECT *
FROM w_vaccine
WHERE number_of_visits >= 2

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.9e3b0f7e-de97-43e9-a9d9-178708a1ba89"),
    condition_occurrence=Input(rid="ri.foundry.main.dataset.526c0452-7c18-46b6-8a5d-59be0b79a10b"),
    lsd_concept_set=Input(rid="ri.foundry.main.dataset.e734aa32-858e-432e-9f14-5f96ee4195d5")
)
SELECT condition_occurrence.*
FROM condition_occurrence JOIN lsd_concept_set ON condition_occurrence.condition_concept_id = lsd_concept_set.concept_id;

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b7aa593b-5428-4b25-ad90-f0ba001d9840"),
    all_vaccines_deduplicated=Input(rid="ri.foundry.main.dataset.2ee63311-8ecc-4bac-a5f3-7f8c7b1a3c3d"),
    bmi_measurements=Input(rid="ri.foundry.main.dataset.5ee96688-bafb-4a95-aa4c-c9eeb7894531"),
    location=Input(rid="ri.foundry.main.dataset.77647aff-bb7e-4bce-9f04-19a971bc83ce"),
    non_lsd_persons=Input(rid="ri.foundry.main.dataset.317815dc-34f5-4ccd-8cb9-117e2dda7b20"),
    visits_df=Input(rid="ri.foundry.main.dataset.68b92e18-8033-40d6-95cf-61594bfe4c00")
)
WITH zip AS (SELECT 
    person_id,
    n.data_partner_id,
    'None' AS lsd_condition_name,
    Sex,
    Race,
    CAST(Age AS INT) AS Age,
    has_lsd,
    COVID_pos_indicator,
    l.zip AS postal_code
FROM non_lsd_persons n
LEFT JOIN location l ON n.location_id = l.location_id),
w_visits AS (
    SELECT z.*,
        v.days_observed,
        CAST(v.number_of_visits AS INT) AS number_of_visits
    FROM zip z
    LEFT JOIN visits_df v ON z.person_id = v.person_id
),
w_bmi AS (
    SELECT w_visits.*,
        bmi.BMI_overall
    FROM w_visits
    LEFT JOIN bmi_measurements bmi ON w_visits.person_id = bmi.person_id
),
w_vaccine AS (
    SELECT w_bmi.*,
        CAST(COALESCE(combined_number_of_COVID_vaccine_doses, 0) AS INT) AS combined_number_of_COVID_vaccine_doses -- replace NULL with 0
    FROM w_bmi
    LEFT JOIN all_vaccines_deduplicated vax ON w_bmi.person_id = vax.person_id
),
filtered AS (
    SELECT *
    FROM w_vaccine 
    WHERE number_of_visits >= 2
)

SELECT *
FROM (
    SELECT *
    FROM filtered
    ORDER BY RAND(128)
    LIMIT 10000000
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2ee63311-8ecc-4bac-a5f3-7f8c7b1a3c3d"),
    Deduplicated=Input(rid="ri.foundry.main.dataset.e08ebf15-9acd-459c-8d02-b01004fe6302"),
    all_cov_07_11_24=Input(rid="ri.foundry.main.dataset.bf9477ec-a135-4e1c-8838-a92f12c21eb6")
)
WITH raw_counts AS (SELECT
    dp.person_id,
    COUNT(CASE WHEN dp.vax_date <= all_cov.COVID_first_poslab_or_diagnosis_date THEN 1 END) AS number_of_COVID_vaccine_doses_before_or_day_of_covid,
    COUNT(CASE WHEN dp.vax_date > all_cov.COVID_first_poslab_or_diagnosis_date THEN 1 END) AS  number_of_COVID_vaccine_doses_post_covid,
    COUNT(CASE WHEN all_cov.COVID_first_poslab_or_diagnosis_date IS NULL THEN 1 END) AS number_of_COVID_vaccine_doses
FROM Deduplicated dp
LEFT JOIN all_cov_07_11_24 all_cov ON dp.person_id = all_cov.person_id
GROUP BY dp.person_id),
capped_counts AS (SELECT 
    person_id,
    CASE WHEN number_of_COVID_vaccine_doses_before_or_day_of_covid > 4 THEN 4 ELSE number_of_COVID_vaccine_doses_before_or_day_of_covid END AS number_of_COVID_vaccine_doses_before_or_day_of_covid,
    CASE WHEN number_of_COVID_vaccine_doses_post_covid > 4 THEN 4 ELSE number_of_COVID_vaccine_doses_post_covid END AS number_of_COVID_vaccine_doses_post_covid,
    CASE WHEN number_of_COVID_vaccine_doses > 8 THEN 8 ELSE number_of_COVID_vaccine_doses END AS number_of_COVID_vaccine_doses -- number of covid vaccinations for someone not infected with COVID
FROM raw_counts)
SELECT person_id,
    CAST(number_of_COVID_vaccine_doses_before_or_day_of_covid AS INT) AS number_of_COVID_vaccine_doses_before_or_day_of_covid,
    CAST(number_of_COVID_vaccine_doses_post_covid AS INT) AS number_of_COVID_vaccine_doses_post_covid,
    CAST(number_of_COVID_vaccine_doses AS INT) AS number_of_COVID_vaccine_doses,
    CAST(GREATEST(number_of_COVID_vaccine_doses_before_or_day_of_covid, number_of_COVID_vaccine_doses) AS INT) AS combined_number_of_COVID_vaccine_doses 
FROM capped_counts

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.5ee96688-bafb-4a95-aa4c-c9eeb7894531"),
    canonical_units_of_measure=Input(rid="ri.foundry.main.dataset.09b4a60a-3da4-4754-8a7e-0b874e2a6f2b"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6"),
    measurement=Input(rid="ri.foundry.main.dataset.29834e2c-f924-45e8-90af-246d29456293")
)
WITH bmi_concepts AS (
    SELECT concept_id,
        CASE 
            WHEN codeset_id = 1000053700 THEN 'HEIGHT'
            WHEN codeset_id = 1000017356 THEN 'WEIGHT'
            WHEN codeset_id = 1000036697 THEN 'BMI'
        END AS indicator_prefix
    FROM concept_set_members 
    WHERE codeset_id IN (1000053700, 1000017356, 1000036697) -- height, weight, BMI
        AND is_most_recent_version = true 
),
canonical_units AS (
    SELECT omop_unit_concept_id,
        omop_unit_concept_name,
        max_acceptable_value,
        min_acceptable_value,
        CASE WHEN
            codeset_id = 754731201 THEN 'HEIGHT'
            WHEN codeset_id = 776390058 THEN 'WEIGHT'
            WHEN codeset_id = 65622096 THEN 'BMI'
        END AS matched_prefix
    FROM canonical_units_of_measure
),
fusion_df AS (
    SELECT  
        indicator_prefix,
        concept_id,
        omop_unit_concept_id,
        omop_unit_concept_name AS harmonized_unit_name,
        max_acceptable_value,
        min_acceptable_value
    FROM bmi_concepts b
    JOIN canonical_units cu ON b.indicator_prefix = cu.matched_prefix
),
all_bmi AS (
    SELECT person_id,
    measurement_date,
    measurement_concept_id,
    indicator_prefix,
    harmonized_value_as_number,
    harmonized_unit_concept_id,
    harmonized_unit_name
FROM measurement m
JOIN fusion_df fd
    ON m.harmonized_unit_concept_id = fd.omop_unit_concept_id
   AND m.measurement_concept_id = fd.concept_id
WHERE m.measurement_date IS NOT NULL
      AND m.harmonized_value_as_number IS NOT NULL
      AND m.harmonized_value_as_number 
            BETWEEN fd.min_acceptable_value AND fd.max_acceptable_value),
recent_bmi AS (
    SELECT *
    FROM (
    SELECT *,
       ROW_NUMBER() OVER (
                   PARTITION BY person_id, indicator_prefix
                   ORDER BY measurement_date DESC, harmonized_value_as_number DESC
               ) AS rn
    FROM all_bmi)
    WHERE rn = 1),
pivoted_bmi AS (
    SELECT
    person_id,
    MAX(CASE WHEN indicator_prefix = 'HEIGHT' THEN harmonized_value_as_number END) AS HEIGHT,
    MAX(CASE WHEN indicator_prefix = 'WEIGHT' THEN harmonized_value_as_number END) AS WEIGHT,
    MAX(CASE WHEN indicator_prefix = 'BMI' THEN harmonized_value_as_number END) AS BMI
FROM recent_bmi
GROUP BY person_id),
bmi_calculated AS (
    SELECT person_id,
    HEIGHT, 
    WEIGHT,
    BMI,
    WEIGHT / (HEIGHT * HEIGHT) AS calculated_BMI,
    CASE 
        WHEN BMI > 0 THEN BMI
        ELSE WEIGHT / (HEIGHT * HEIGHT)
        END AS BMI_overall
    FROM pivoted_bmi
)

SELECT 
    person_id,
    HEIGHT, 
    WEIGHT,
    ROUND(calculated_BMI) AS calculated_BMI,
    ROUND(BMI_overall) AS BMI_overall
FROM bmi_calculated
WHERE BMI_overall BETWEEN 1 AND 100 -- accepted range

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a0c8a3b9-6a48-4b81-8baf-ebee57a05d38"),
    cohort2=Input(rid="ri.foundry.main.dataset.2be8aaea-6435-48ce-890d-d4441c292af7"),
    condition_occurrence=Input(rid="ri.foundry.main.dataset.526c0452-7c18-46b6-8a5d-59be0b79a10b"),
    icd_to_snomed=Input(rid="ri.foundry.main.dataset.b35ab8eb-05a3-4fbd-a60a-d647642e7fc5")
)
WITH omop_conditions AS (
    SELECT co.*,
    ch.has_lsd,
    ch.Long_COVID_any_indicator,
    ch.Severity_Type,
    ch.Binary_Severity,
    ch.weights,
    ch.COVID_first_poslab_or_diagnosis_date,
    ch.number_of_visits_before_covid,
    ch.number_of_visits_post_covid,
    ch.observation_period_before_covid,
    ch.observation_period_post_covid,
    CASE WHEN 
        condition_start_date <= ch.COVID_first_poslab_or_diagnosis_date THEN 'before_or_on_covid'
        ELSE 'post_covid'
    END AS covid_period
FROM cohort2 ch
JOIN condition_occurrence co ON ch.person_id = co.person_id),
cohort_conditions_snomed AS (
    SELECT *,
    datediff(condition_start_date, COVID_first_poslab_or_diagnosis_date) AS days_diff
    FROM omop_conditions 
),
icd_map AS (
    SELECT icd_concept_name,
    icd_concept_id,
    vocabulary_id,
    concept_code
    FROM icd_to_snomed
)
SELECT DISTINCT
    person_id,
    condition_concept_id,  
    condition_concept_name,
    condition_start_date,
    condition_end_date,
    data_partner_id,
    has_lsd,
    Long_COVID_any_indicator,
    Severity_Type,
    Binary_Severity,
    weights,
    COVID_first_poslab_or_diagnosis_date,
    number_of_visits_before_covid,
    number_of_visits_post_covid,
    observation_period_before_covid,
    observation_period_post_covid,
    covid_period,
    days_diff,
    icd_concept_name,
    icd_concept_id,
    vocabulary_id,
    concept_code
FROM cohort_conditions_snomed
LEFT JOIN icd_map ON cohort_conditions_snomed.condition_source_concept_id = icd_map.icd_concept_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.841ff505-990e-49a8-b5f6-27dbcfaac01f"),
    cohort_conditions_2_1=Input(rid="ri.foundry.main.dataset.a0c8a3b9-6a48-4b81-8baf-ebee57a05d38")
)
SELECT DISTINCT 
    person_id,
    condition_concept_id,
    condition_start_date,
    condition_end_date,
    data_partner_id,
    has_lsd,
    Long_COVID_any_indicator,
    Severity_Type,
    Binary_Severity,
    weights,
    COVID_first_poslab_or_diagnosis_date,
    number_of_visits_before_covid,
    number_of_visits_post_covid,
    observation_period_before_covid,
    observation_period_post_covid,
    covid_period,
    days_diff,
    icd_concept_name,
    icd_concept_id,
    vocabulary_id,
    concept_code
FROM cohort_conditions_2_1
WHERE concept_code IS NOT NULL

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6662c2bb-0b44-4f01-9285-e8be46bf9a91"),
    cohort2=Input(rid="ri.foundry.main.dataset.2be8aaea-6435-48ce-890d-d4441c292af7"),
    death=Input(rid="ri.foundry.main.dataset.9c6c12b0-8e09-4691-91e4-e5ff3f837e69")
)
WITH new AS (SELECT * FROM death WHERE death_date IS NOT NULL),
list AS (SELECT DISTINCT person_id,
    concat_ws(';', collect_list(cause_concept_id)) AS cause_concept_id,
    concat_ws(';', collect_list(cause_concept_name)) AS cause_concept_name,
    concat_ws(';', collect_list(death_type_concept_id)) AS death_type_concept_id,
    max(death_date) AS death_date,
    any_value(data_partner_id) AS data_partner_id
FROM new
GROUP BY person_id),
cov AS (SELECT list.*,
    datediff(death_date, COVID_first_poslab_or_diagnosis_date) AS days_diff,
    cohort.COVID_first_poslab_or_diagnosis_date,
    cohort.Severity_Type,
    cohort.Binary_Severity,
    cohort.has_lsd,
    cohort.lsd_condition_name,
    cohort.Long_COVID_any_indicator
    FROM list
JOIN cohort2 cohort ON cohort.person_id = list.person_id)

SELECT *
FROM cov
WHERE days_diff BETWEEN 0 AND 1825 -- five years, greater than this is an errant death_date

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a079bee5-0a15-4160-8032-55854022e928"),
    cohort2=Input(rid="ri.foundry.main.dataset.2be8aaea-6435-48ce-890d-d4441c292af7"),
    drug_exposure=Input(rid="ri.foundry.main.dataset.fd499c1d-4b37-4cda-b94f-b7bf70a014da"),
    drugs_of_interest=Input(rid="ri.foundry.main.dataset.a18947cf-261d-4233-82db-5fc578d65258"),
    lsd_drugs=Input(rid="ri.foundry.main.dataset.682e5590-21f8-49d6-82b9-7b1066ea95ce")
)
SELECT drug.person_id,
    drug.drug_concept_id,
    drug.drug_concept_name,
    drug.drug_exposure_start_date,
    drug.drug_exposure_end_date,
    co.Severity_Type,
    co.Binary_Severity,
    co.COVID_first_poslab_or_diagnosis_date,
    co.has_lsd,
    co.lsd_condition_name,
    co.Long_COVID_any_indicator,
    co.subclass,
    datediff(drug_exposure_start_date, COVID_first_poslab_or_diagnosis_date) AS days_diff
FROM drug_exposure drug
JOIN cohort2 co ON co.person_id = drug.person_id
WHERE drug_exposure_start_date IS NOT NULL 
    AND drug_exposure_start_date <= DATE '2025-07-11'

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2d9e53a6-52ce-4129-bbc7-7c55520833e2"),
    cohort2=Input(rid="ri.foundry.main.dataset.2be8aaea-6435-48ce-890d-d4441c292af7"),
    concept=Input(rid="ri.foundry.main.dataset.5cb3c4a3-327a-47bf-a8bf-daf0cafe6772"),
    measurement=Input(rid="ri.foundry.main.dataset.29834e2c-f924-45e8-90af-246d29456293")
)
WITH joined AS (
    SELECT m.*,
    co.Severity_Type,
    co.COVID_first_poslab_or_diagnosis_date,
    co.has_lsd,
    co.Long_COVID_any_indicator
FROM measurement m
JOIN cohort2 co ON co.person_id = m.person_id),
complete AS (
    SELECT *
    FROM joined
    WHERE harmonized_value_as_number IS NOT NULL
    AND harmonized_unit_concept_id IS NOT NULL
),
days AS (
    SELECT ce.person_id,
    ce.measurement_concept_id,
    ce.measurement_concept_name,
    ce.measurement_date,
    ce.harmonized_value_as_number,
    ce.Severity_Type,
    ce.has_lsd,
    ce.Long_COVID_any_indicator,
    ce.COVID_first_poslab_or_diagnosis_date,
    concept.concept_code AS units,
    datediff(ce.measurement_date, ce.COVID_first_poslab_or_diagnosis_date) AS days_diff -- days since COVID
FROM complete ce
JOIN concept ON concept.concept_id = ce.harmonized_unit_concept_id)

SELECT *
FROM days 
WHERE days_diff BETWEEN -30 AND 30

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.977b8ef2-7d67-414f-85e0-2f5524a772aa"),
    cohort_conditions_icd_2_1=Input(rid="ri.foundry.main.dataset.841ff505-990e-49a8-b5f6-27dbcfaac01f"),
    phecode_map=Input(rid="ri.foundry.main.dataset.c2bec0d5-a47e-420c-9f1f-1526771f00b7")
)
SELECT DISTINCT 
    cc.person_id,
    pm.phecode,
    SUBSTRING_INDEX(pm.phecode, '.', 1) AS prefix,
    pm.phecode_string,
    pm.category,
    pm.ICD,
    cc.days_diff,
    cc.condition_start_date,
    cc.condition_end_date,
    cc.observation_period_before_covid,
    cc.observation_period_post_covid,
    cc.covid_period,
    cc.has_lsd,
    cc.Severity_Type,
    cc.Binary_Severity
FROM cohort_conditions_icd_2_1 cc
JOIN phecode_map pm 
    ON cc.concept_code = pm.ICD
WHERE cc.concept_code IS NOT NULL 
    AND cc.concept_code NOT IN ('U07.1', 'U00', 'J12.81', 'J12.82', 'B34.2', 'B97.21', 'B97.29', 'B97.2') -- exclude specific and generic covid diagnoses

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.667826d8-583c-4744-ba97-30b3a43c86d1"),
    cohort2=Input(rid="ri.foundry.main.dataset.2be8aaea-6435-48ce-890d-d4441c292af7"),
    procedure_occurrence=Input(rid="ri.foundry.main.dataset.f8826e21-741d-49bb-a7eb-47ea98bb2b5f"),
    procedures_of_interest=Input(rid="ri.foundry.main.dataset.4d03d58e-122b-4bad-92b0-f64625f77978")
)
WITH procedure_cohort AS (
SELECT pro.person_id,
    pro.procedure_concept_name,
    pro.procedure_concept_id,
    pro.procedure_date,
    co.COVID_first_poslab_or_diagnosis_date,
    co.Severity_Type,
    co.Binary_Severity,
    co.has_lsd,
    co.Long_COVID_any_indicator,
    co.subclass,
    datediff(procedure_date, COVID_first_poslab_or_diagnosis_date) AS days_diff
FROM procedure_occurrence pro
JOIN cohort2 co ON co.person_id = pro.person_id
WHERE procedure_date IS NOT NULL 
    AND procedure_date <= DATE '2025-07-11')

SELECT pc.*,
    poi.procedure_name
FROM procedure_cohort pc
JOIN procedures_of_interest poi ON pc.procedure_concept_id = poi.concept_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a18947cf-261d-4233-82db-5fc578d65258"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6")
)
SELECT *,
    CASE 
        WHEN codeset_id = 1000011148 THEN 'dexamethasone'
        WHEN codeset_id = 270102919 THEN 'heparin'
        WHEN codeset_id = 932266800 THEN 'systemic hydrocortisone'
        WHEN codeset_id = 425433230 THEN 'insulin'
        WHEN codeset_id = 1000033171 THEN 'lisinopril'
        WHEN codeset_id = 1000082442 THEN 'nirmatrelvir'
        WHEN codeset_id = 940495880 THEN 'methylprednisolone'
        WHEN codeset_id = 65058498 THEN 'prednisone'
        WHEN codeset_id = 1000040403 THEN 'remdesivir'
        WHEN codeset_id = 745170047 THEN 'carvedilol'
        WHEN codeset_id = 769518525 THEN 'budesonide'
        WHEN codeset_id = 883259145 THEN 'albuterol'
        WHEN codeset_id = 472540083 THEN 'apixaban'
        WHEN codeset_id = 56320741 THEN 'aspirin'
        WHEN codeset_id = 231875380 THEN 'famotidine'
    END AS drug_name
FROM concept_set_members
WHERE codeset_id IN (
    1000011148, -- dexamethasone
    270102919, -- heparin
    932266800, -- systemic hydrocortisone
    425433230, -- insulin
    1000033171, -- lisinopril
    1000082442, -- nirmatrelvir + paxlovid
    940495880, -- methylprednisolone
    65058498, -- prednisone 
    1000040403, -- remdesivir
    745170047, -- carvedilol
    769518525, -- budesonide
    883259145, -- albuterol
    472540083, -- apixaban
    56320741, -- aspirin
    231875380 -- famotidine
    )
    AND is_most_recent_version = true; 

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3326f9d3-68c4-4993-a336-b6764c27d290"),
    all_cohort_phecodes=Input(rid="ri.foundry.main.dataset.2cabfb3b-fa18-4578-b402-1bc737fc35e5")
)
WITH ranked AS (
  SELECT
    person_id,
    phecode,
    has_lsd,
    lsd_condition_name,
    subclass,
    condition_start_date,
    ROW_NUMBER() OVER (
      PARTITION BY person_id, phecode
      ORDER BY condition_start_date ASC
    ) AS rn
  FROM all_cohort_phecodes
  WHERE days_observed >= 365
    AND prefix NOT RLIKE '^GE_963' -- exclude lysosomal storage diseases
)
SELECT
  person_id,
  phecode,
  CAST(has_lsd AS INT) AS has_lsd,
  lsd_condition_name,
  subclass,
  condition_start_date
FROM ranked
WHERE rn = 1

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7d140f59-7701-43fe-a4e3-13e8027ac651"),
    cohort_drug_exposure=Input(rid="ri.foundry.main.dataset.a079bee5-0a15-4160-8032-55854022e928"),
    lsd_drugs=Input(rid="ri.foundry.main.dataset.682e5590-21f8-49d6-82b9-7b1066ea95ce")
)
SELECT cd.person_id,
    cd.drug_concept_id,
    cd.drug_concept_name,
    cd.lsd_condition_name,
    cd.has_lsd,
    f.drug_name
FROM cohort_drug_exposure cd 
JOIN lsd_drugs f ON f.concept_id = cd.drug_concept_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b35ab8eb-05a3-4fbd-a60a-d647642e7fc5"),
    concept=Input(rid="ri.foundry.main.dataset.5cb3c4a3-327a-47bf-a8bf-daf0cafe6772"),
    concept_relationship=Input(rid="ri.foundry.main.dataset.0469a283-692e-4654-bb2e-26922aff9d71")
)
WITH icd AS (SELECT * -- Get ICD10CM
FROM concept
WHERE vocabulary_id = 'ICD10CM'),
joined AS (
SELECT icd.*,
    concept_id_1,
    concept_id_2, 
    relationship_id
FROM icd
LEFT JOIN concept_relationship ON icd.concept_id = concept_relationship.concept_id_1
    AND (relationship_id = 'Maps to' OR relationship_id IS NULL)), -- keep all ICD with a LEFT JOIN
snomed_names AS (
SELECT j.*, -- Bring in the name
    c.concept_name AS mapped_concept_name,
    c.domain_id AS mapped_domain_id,
    c.vocabulary_id AS mapped_vocabulary_id
FROM joined j
LEFT JOIN concept c ON j.concept_id_2 = c.concept_id
    AND (c.vocabulary_id = 'SNOMED' OR c.vocabulary_id IS NULL))
SELECT concept_id_1 AS icd_concept_id,
    concept_name AS icd_concept_name,
    domain_id,
    vocabulary_id, 
    concept_code,
    relationship_id,
    concept_id_2 AS snomed_concept_id,
    mapped_concept_name,
    mapped_domain_id,
    mapped_vocabulary_id
FROM snomed_names

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3f98f6de-4736-464a-9bff-d7d38fa822aa"),
    lsd_imputation=Input(rid="ri.foundry.main.dataset.dbd2b4e5-9fc5-4e75-a5ed-33d8ac7d7b0c"),
    non_lsd_imputation=Input(rid="ri.foundry.main.dataset.58ff31f6-450d-4c6a-a943-0f734eda8a18")
)
SELECT *
FROM non_lsd_imputation

UNION

SELECT *
FROM lsd_imputation

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d31ab264-559b-46fb-b036-ec1c279e405a"),
    concept_ancestor=Input(rid="ri.foundry.main.dataset.c5e0521a-147e-4608-b71e-8f53bcdbe03c"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6"),
    lsd_concept_set=Input(rid="ri.foundry.main.dataset.e734aa32-858e-432e-9f14-5f96ee4195d5")
)
WITH descendants AS (
    SELECT 
        concept_ancestor.*,
        concept_set_members.concept_name AS descendant_name
FROM concept_ancestor
JOIN concept_set_members ON concept_ancestor.descendant_concept_id = concept_set_members.concept_id),
ancestors AS (
    SELECT descendants.*,
        concept_set_members.concept_name AS ancestor_name
    FROM descendants
    JOIN concept_set_members ON descendants.ancestor_concept_id = concept_set_members.concept_id)

SELECT DISTINCT ancestors.ancestor_concept_id,
     ancestors.ancestor_name,
     ancestors.descendant_concept_id,
    ancestors.descendant_name,        
    ancestors.min_levels_of_separation,
    ancestors.max_levels_of_separation
FROM ancestors
JOIN lsd_concept_set ON ancestors.ancestor_concept_id = lsd_concept_set.concept_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e734aa32-858e-432e-9f14-5f96ee4195d5"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6")
)
SELECT *
FROM concept_set_members 
WHERE codeset_id = 79972333 AND is_most_recent_version = true;

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ad0cb927-3946-4c17-80f4-12621d45fb74"),
    all_cov_07_11_24=Input(rid="ri.foundry.main.dataset.bf9477ec-a135-4e1c-8838-a92f12c21eb6")
)
SELECT *
FROM all_cov_07_11_24
WHERE has_lsd = 1;

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.682e5590-21f8-49d6-82b9-7b1066ea95ce"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6")
)
SELECT concept_id,
    concept_name,
    CASE
    WHEN LOWER(concept_name) LIKE '%migalastat%' THEN 'migalastat'
    WHEN LOWER(concept_name) LIKE '%vestronidase alfa%' THEN 'vestronidase alfa'
    WHEN LOWER(concept_name) LIKE '%velaglucerase alfa%' THEN 'velaglucerase alfa'
    WHEN LOWER(concept_name) LIKE '%taliglucerase alfa%' THEN 'taliglucerase alfa'
    WHEN LOWER (concept_name) LIKE '%sebelipase alfa%' THEN 'sebelipase alfa'
    WHEN LOWER(concept_name) LIKE '%miglustat%' THEN 'miglustat'
    WHEN LOWER(concept_name) LIKE '%laronidase%' THEN 'laronidase'
    WHEN LOWER(concept_name) LIKE '%imiglucerase%' THEN 'imiglucerase'
    WHEN LOWER(concept_name) LIKE '%idursulfase%' THEN 'idursulfase'
    WHEN LOWER(concept_name) LIKE '%galsulfase%' THEN 'galsulfase'
    WHEN LOWER(concept_name) LIKE '%elosulfase alfa%' THEN 'elosulfase alfa'
    WHEN LOWER(concept_name) LIKE '%eliglustat%' THEN 'eliglustat'
    WHEN LOWER(concept_name) LIKE '%cysteamine%' THEN 'cysteamine'
    WHEN LOWER(concept_name) LIKE '%cerliponase alfa%' THEN 'cerliponase alfa'
    WHEN LOWER(concept_name) LIKE '%alglucosidase alfa%' THEN 'alglucosidase alfa'
    WHEN LOWER(concept_name) LIKE '%alglucerase%' THEN 'alglucerase'
    WHEN LOWER(concept_name) LIKE '%agalsidase beta%' THEN 'agalsidase beta'
    ELSE 'Other'
  END AS drug_name
FROM concept_set_members
WHERE codeset_id = 215091197
    AND is_most_recent_version = true

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.431ea30f-2865-47fc-9728-2a6b88fea36f"),
    all_cov_07_11_24=Input(rid="ri.foundry.main.dataset.bf9477ec-a135-4e1c-8838-a92f12c21eb6"),
    person=Input(rid="ri.foundry.main.dataset.af5e5e91-6eeb-4b14-86df-18d84a5aa010"),
    specific_lsds=Input(rid="ri.foundry.main.dataset.3500942a-19c9-4245-84fe-67d90a2cb0e4")
)
WITH demographics AS (SELECT DISTINCT l.*,
    p.location_id,
     CASE 
        WHEN p.gender_concept_name = 'MALE' THEN 'Male'
        WHEN p.gender_concept_name = 'FEMALE' THEN 'Female'
        ELSE 'Unknown'
        END AS Sex,
    CASE
        WHEN p.race_concept_name IN ('Hispanic') THEN 'Hispanic or Latino'
        WHEN p.race_concept_name IN (
            'Asian', 'Asian Indian', 'Bangladeshi', 'Bhutanese', 'Burmese', 'Cambodian', 'Chinese', 'Filipino',
            'Hmong', 'Indonesian', 'Japanese', 'Korean', 'Laotian', 'Malaysian', 'Maldivian', 'Nepalese',
            'Okinawan', 'Pakistani', 'Singaporean', 'Sri Lankan', 'Taiwanese', 'Thai', 'Vietnamese'
        ) THEN 'Asian'
        WHEN p.race_concept_name IN (
            'African', 'African American', 'Barbadian', 'Black', 'Black or African American', 'Dominica Islander',
            'Haitian', 'Jamaican', 'Madagascar', 'Trinidadian', 'West Indian'
        ) THEN 'Black or African American'
        WHEN p.race_concept_name IN ('White') THEN 'White'
        WHEN p.race_concept_name IN (
            'Melanesian', 'Micronesian', 'Native Hawaiian or Other Pacific Islander', 'Other Pacific Islander', 'Polynesian'
        ) THEN 'Native Hawaiian or Other Pacific Islander'
        WHEN p.race_concept_name IN ('American Indian or Alaska Native') THEN 'American Indian or Alaska Native'
        WHEN p.race_concept_name IN (
            'More than one race', 'Multiple race', 'Multiple races', 'Other', 'Other Race', 'Asian or Pacific islander', 'No Information', 'No matching concept', 'Refuse to Answer',
            'Unknown', 'Unknown racial group'
        ) THEN 'Other/Unknown'
        ELSE 'Other/Unknown'
    END AS Race,
    FLOOR(
    MONTHS_BETWEEN(
        DATE('2025-01-07'),
        TO_DATE(CONCAT(p.year_of_birth, '-', LPAD(p.month_of_birth, 2, '0'), '-01'))
    ) / 12) AS Age,
      CASE WHEN all_cov.COVID_first_poslab_or_diagnosis_date IS NOT NULL THEN 1 
      ELSE 0
    END AS COVID_pos_indicator,
    all_cov.COVID_first_poslab_or_diagnosis_date,
    1 AS has_lsd
FROM specific_lsds l
LEFT JOIN person p ON p.person_id = l.person_id
LEFT JOIN all_cov_07_11_24 all_cov ON p.person_id = all_cov.person_id)

SELECT person_id,
    condition_concept_id,
    condition_source_concept_id,
    condition_source_value,
    specific_condition_name AS lsd_condition_name,
    condition_start_date,
    condition_end_date,
    data_partner_id,
    location_id,
    COALESCE(Sex, 'Unknown') AS Sex,
    COALESCE(Race, 'Other/Unknown') AS Race,
    CASE 
        WHEN Age < 0 THEN 0 
        ELSE Age
    END AS Age,
    has_lsd,
    COVID_pos_indicator,
    COVID_first_poslab_or_diagnosis_date
FROM demographics d

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6047ebb9-fec2-4a84-89cb-b480ec11e423"),
    lsd_cov=Input(rid="ri.foundry.main.dataset.ad0cb927-3946-4c17-80f4-12621d45fb74")
)
SELECT 
    person_id,
    has_lsd,
    Sex,
    CASE 
        WHEN Race = 'Unknown' 
            OR Race = 'Other/Unknown' THEN 'Other/Unknown'
            ELSE Race
        END AS Race,
    Severity_Type,
    lsd_data_partner_id AS data_partner_id,
    COVID_first_poslab_or_diagnosis_date,
    observation_period_before_covid,
    observation_period_post_covid,
    CASE 
        WHEN number_of_visits_before_covid IS NULL THEN 0
        ELSE number_of_visits_before_covid END AS number_of_visits_before_covid,
    CASE 
        WHEN number_of_visits_post_covid IS NULL THEN 0
        ELSE number_of_visits_post_covid END AS 
        number_of_visits_post_covid,
    BMI_max_observed_or_calculated_before_or_day_of_covid AS BMI_before_or_day_of_covid,
    had_at_least_one_reinfection_post_covid_indicator AS had_reinfection_post_covid,
    CASE WHEN 
    number_of_COVID_vaccine_doses_before_or_day_of_covid IS NULL THEN 0
    ELSE number_of_COVID_vaccine_doses_before_or_day_of_covid
    END AS COVID_vaccine_doses_before_or_day_of_covid,
    postal_code,
    CASE
        WHEN Long_COVID_clinic_visit_post_covid_indicator = 1
        OR Long_COVID_diagnosis_post_covid_indicator = 1 THEN 1
        ELSE 0
    END AS Long_COVID_any_indicator,
    CASE 
        WHEN specific_condition_name IS NULL THEN 'None' 
        ELSE specific_condition_name 
    END AS lsd_condition_name,
    CASE 
        WHEN Severity_Type = 'Mild_No_ED_or_Hosp_around_COVID_index' 
            OR Severity_Type = 'Mild_ED_around_strong_signal_COVID_index'
            OR Severity_Type = 'Mild_ED_around_weak_signal_COVID_index'
            THEN 0
        ELSE 1
    END AS Binary_Severity,
    CASE 
        WHEN Age < 0 THEN 0 
        ELSE Age
    END AS Age
FROM lsd_cov
WHERE number_of_visits_before_covid >= 1

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f0503f83-467d-43b5-801d-4c8b3a99d771"),
    all_cov_07_11_24=Input(rid="ri.foundry.main.dataset.bf9477ec-a135-4e1c-8838-a92f12c21eb6")
)
SELECT *
FROM all_cov_07_11_24
WHERE has_lsd = 0;

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.317815dc-34f5-4ccd-8cb9-117e2dda7b20"),
    all_cov_07_11_24=Input(rid="ri.foundry.main.dataset.bf9477ec-a135-4e1c-8838-a92f12c21eb6"),
    lsd_persons=Input(rid="ri.foundry.main.dataset.431ea30f-2865-47fc-9728-2a6b88fea36f"),
    person=Input(rid="ri.foundry.main.dataset.af5e5e91-6eeb-4b14-86df-18d84a5aa010")
)
WITH demographics AS (SELECT DISTINCT p.*,
    CASE 
        WHEN p.gender_concept_name = 'MALE' THEN 'Male'
        WHEN p.gender_concept_name = 'FEMALE' THEN 'Female'
        ELSE 'Unknown'
        END AS Sex,
    CASE
        WHEN p.race_concept_name IN ('Hispanic') THEN 'Hispanic or Latino'
        WHEN p.race_concept_name IN (
            'Asian', 'Asian Indian', 'Bangladeshi', 'Bhutanese', 'Burmese', 'Cambodian', 'Chinese', 'Filipino',
            'Hmong', 'Indonesian', 'Japanese', 'Korean', 'Laotian', 'Malaysian', 'Maldivian', 'Nepalese',
            'Okinawan', 'Pakistani', 'Singaporean', 'Sri Lankan', 'Taiwanese', 'Thai', 'Vietnamese'
        ) THEN 'Asian'
        WHEN p.race_concept_name IN (
            'African', 'African American', 'Barbadian', 'Black', 'Black or African American', 'Dominica Islander',
            'Haitian', 'Jamaican', 'Madagascar', 'Trinidadian', 'West Indian'
        ) THEN 'Black or African American'
        WHEN p.race_concept_name IN ('White') THEN 'White'
        WHEN p.race_concept_name IN (
            'Melanesian', 'Micronesian', 'Native Hawaiian or Other Pacific Islander', 'Other Pacific Islander', 'Polynesian'
        ) THEN 'Native Hawaiian or Other Pacific Islander'
        WHEN p.race_concept_name IN ('American Indian or Alaska Native') THEN 'American Indian or Alaska Native'
        WHEN p.race_concept_name IN (
            'More than one race', 'Multiple race', 'Multiple races', 'Other', 'Other Race', 'Asian or Pacific islander', 'No Information', 'No matching concept', 'Refuse to Answer',
            'Unknown', 'Unknown racial group'
        ) THEN 'Other/Unknown'
        ELSE 'Other/Unknown'
    END AS Race,
    FLOOR(
    MONTHS_BETWEEN(
        DATE('2025-01-07'),
        TO_DATE(CONCAT(p.year_of_birth, '-', LPAD(p.month_of_birth, 2, '0'), '-01'))
    ) / 12
    ) AS Age,
     CASE WHEN all_cov.COVID_first_poslab_or_diagnosis_date IS NOT NULL THEN 1 
      ELSE 0
    END AS COVID_pos_indicator,
    all_cov.COVID_first_poslab_or_diagnosis_date,
    0 AS has_lsd
FROM person p
LEFT JOIN all_cov_07_11_24 all_cov ON p.person_id = all_cov.person_id
WHERE p.person_id NOT IN (SELECT l.person_id FROM lsd_persons l))

SELECT person_id,
    data_partner_id,
    location_id,
    Sex,
    Race,
    CASE 
        WHEN Age < 0 THEN 0 
        ELSE Age
    END AS Age,
    has_lsd,
    COVID_pos_indicator,
    COVID_first_poslab_or_diagnosis_date
FROM demographics d

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.75cac421-ffed-4d84-b191-e3e6dfc7521e"),
    non_lsd_cov=Input(rid="ri.foundry.main.dataset.f0503f83-467d-43b5-801d-4c8b3a99d771")
)
SELECT
    person_id,
    has_lsd,
    Sex,
    CASE 
        WHEN Race = 'Unknown' 
            OR Race = 'Other/Unknown' THEN 'Other/Unknown'
            ELSE Race
        END AS Race,
    Severity_Type,
    data_partner_id,
    COVID_first_poslab_or_diagnosis_date,
    observation_period_before_covid,
    observation_period_post_covid,
    CASE 
        WHEN number_of_visits_before_covid IS NULL THEN 0
        ELSE number_of_visits_before_covid END AS number_of_visits_before_covid,
    CASE 
        WHEN number_of_visits_post_covid IS NULL THEN 0
        ELSE number_of_visits_post_covid END AS 
        number_of_visits_post_covid,
    BMI_max_observed_or_calculated_before_or_day_of_covid AS BMI_before_or_day_of_covid,
    had_at_least_one_reinfection_post_covid_indicator AS had_reinfection_post_covid,
    CASE WHEN 
    number_of_COVID_vaccine_doses_before_or_day_of_covid IS NULL THEN 0
    ELSE number_of_COVID_vaccine_doses_before_or_day_of_covid
    END AS COVID_vaccine_doses_before_or_day_of_covid,
    postal_code,
    CASE
        WHEN Long_COVID_clinic_visit_post_covid_indicator = 1
        OR Long_COVID_diagnosis_post_covid_indicator = 1 THEN 1
        ELSE 0
    END AS Long_COVID_any_indicator,
    CASE 
        WHEN specific_condition_name IS NULL THEN 'None' 
        ELSE specific_condition_name 
    END AS lsd_condition_name,
     CASE 
        WHEN Severity_Type = 'Mild_No_ED_or_Hosp_around_COVID_index' 
            OR Severity_Type = 'Mild_ED_around_strong_signal_COVID_index'
            OR Severity_Type = 'Mild_ED_around_weak_signal_COVID_index'
            THEN 0
        ELSE 1
    END AS Binary_Severity,
    CASE 
        WHEN Age < 0 THEN 0 
        ELSE Age
    END AS Age
FROM non_lsd_cov
WHERE number_of_visits_before_covid >= 1;

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c2bec0d5-a47e-420c-9f1f-1526771f00b7"),
    phecodeX_wlabels=Input(rid="ri.foundry.main.dataset.157790c9-b157-44db-9c5e-e34998407d81")
)
SELECT *
FROM phecodeX_wlabels
WHERE vocabulary_id == 'ICD10CM';

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.4d03d58e-122b-4bad-92b0-f64625f77978"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6")
)
SELECT *,
    CASE 
        WHEN codeset_id = 996569760 THEN 'continuous positive airway pressure'
        WHEN codeset_id = 993408479 THEN 'specimen collection for SARS-CoV-2'
        WHEN codeset_id = 30670147 THEN 'electrocardiography'
        WHEN codeset_id = 577359343 THEN 'echocardiography'
        WHEN codeset_id = 764129526 THEN 'inhalation treatment with aerosol or nebulizer'
        WHEN codeset_id = 209464212 THEN 'intravenous infusion, for therapy or prophylaxis'
        WHEN codeset_id = 632643495 THEN 'intravenous infusion, hydration'
        WHEN codeset_id = 5668392 THEN 'other respiratory ventilation'
        WHEN codeset_id = 920228667 THEN  'radiologic examination, chest'
        WHEN codeset_id = 147612319 THEN 'therapeutic, prophylactic, or diagnostic injection'
        WHEN codeset_id = 970208606 THEN 'critical care'
    END AS procedure_name
FROM concept_set_members
WHERE codeset_id IN (
    996569760, -- continuous positive airway pressure
    993408479, -- specimen collection for SARS-CoV-2
    30670147, -- electrocardiography
    577359343, -- echocardiography
    764129526, -- inhalation treatment with aerosol or nebulizer
    209464212, -- intravenous infusion, for therapy or prophylaxis
    632643495, -- intravenous infusion, hydration
    5668392, -- other respiratory ventilation
    920228667, -- radiologic examination, chest
    147612319, -- therapeutic, prophylactic, or diagnostic injection
    970208606 -- critical care
    )
    AND is_most_recent_version = true; 

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.cb16e05f-0750-4afe-afe6-f62bba4a0a90"),
    cohort2=Input(rid="ri.foundry.main.dataset.2be8aaea-6435-48ce-890d-d4441c292af7"),
    cohort_death=Input(rid="ri.foundry.main.dataset.6662c2bb-0b44-4f01-9285-e8be46bf9a91")
)
WITH combined AS (
    SELECT 
        ld.person_id,
        ld.has_lsd,
        ld.lsd_condition_name,
        ld.Severity_Type,
        d.death_date,
        CASE
            WHEN ld.Binary_Severity = 0 THEN 'Not Hospitalized'
            WHEN ld.Binary_Severity = 1 THEN 'Hospitalized'
        END AS Severity,
        CASE 
            WHEN d.death_date IS NOT NULL THEN DATEDIFF(d.death_date, ld.COVID_first_poslab_or_diagnosis_date) -- event
            WHEN d.death_date IS NULL THEN DATEDIFF('2025-07-11', ld.COVID_first_poslab_or_diagnosis_date) -- right censored
            ELSE NULL
        END AS days,
        CASE 
            WHEN d.death_date IS NOT NULL THEN 1
            ELSE 0
        END AS status,
        ld.COVID_first_poslab_or_diagnosis_date
    FROM cohort2 ld
    LEFT JOIN cohort_death d ON d.person_id = ld.person_id
)

SELECT *
FROM combined cb
WHERE cb.days >= 0

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.68b92e18-8033-40d6-95cf-61594bfe4c00"),
    manifest_safe_harbor=Input(rid="ri.foundry.main.dataset.0d33109d-733f-48a6-8d4b-53bdf679a518"),
    microvisits_to_macrovisits=Input(rid="ri.foundry.main.dataset.89927e78-e712-4dcd-a470-18c1620bd03e")
)
WITH visits_df AS (
SELECT 
  m.person_id,
  m.macrovisit_start_date,
  m.macrovisit_end_date,
  m.visit_start_date,
  m.visit_end_date,
  f.run_date AS data_extraction_date
FROM microvisits_to_macrovisits m
LEFT JOIN manifest_safe_harbor f
  ON m.data_partner_id = f.data_partner_id
WHERE m.visit_start_date >= DATE('2017-05-01')
  AND m.visit_start_date < DATE_ADD(f.run_date, 365 * 2)),
hosp_visits AS (
    SELECT DISTINCT person_id, macrovisit_start_date, visit_start_date
    FROM visits_df
    WHERE macrovisit_start_date IS NOT NULL),
non_hosp_visits AS (
    SELECT DISTINCT person_id, visit_start_date
    FROM visits_df
    WHERE macrovisit_start_date IS NULL
),
union_visits AS (
SELECT person_id, visit_start_date FROM hosp_visits
UNION
SELECT person_id, visit_start_date FROM non_hosp_visits),
visits_summary AS (
SELECT
    person_id,
    COUNT(*) AS number_of_visits,
    DATEDIFF(MAX(visit_start_date), MIN(visit_start_date)) AS days_observed
FROM union_visits
GROUP BY person_id)

SELECT *
FROM visits_summary

