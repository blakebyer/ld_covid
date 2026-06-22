library(tidyverse)
cbind_na <- function(...) {
           dfs <- list(...)
           max_length <- max(sapply(dfs, nrow))
            
            pad_rows <- function(df, max_length) {
                 if (nrow(df) < max_length) {
                 n_missing <- max_length - nrow(df)
                 df[(nrow(df) + 1):max_length, ] <- NA
                }
                 df
         }
             padded_dfs <- lapply(dfs, pad_rows, max_length = max_length)
            tibble::as_tibble(do.call(cbind, padded_dfs))
         }

split_df <- function(df, lsd_conditions) { # get lsd condition and their matched counterparts

        var_name <- deparse(substitute(lsd_conditions))
        
        subclasses <- df %>%
            filter(lsd_condition_name %in% lsd_conditions) %>%
            pull(subclass) %>%
            unique()
        
        sub_df <- df %>%
            filter(lsd_condition_name %in% c(lsd_conditions, "None") &
            subclass %in% subclasses)

        return(sub_df)
    }

count_events <- function(df, name, event) {
  count_df <- tibble(
    dataset       = name,
    n_events      = sum(df[[event]], na.rm = TRUE),
    # n_events_ld   = sum(df[[event]][df$has_lsd == 1], na.rm = TRUE),
    # n_events_ctrl = sum(df[[event]][df$has_lsd == 0], na.rm = TRUE),
    n_ld          = sum(df$has_lsd == 1, na.rm = TRUE),
    n_control     = sum(df$has_lsd == 0, na.rm = TRUE),
    n_patients    = nrow(df)
  )
  return(count_df)
}

sphingo <-
    c("Metachromatic leukodystrophy, congenital type",
    "Gaucher's disease",
    "Niemann-Pick disease, type B",
    "Multiple sulfatase deficiency",
    "Sphingomyelin/cholesterol lipidosis",
    "Globoid cell leukodystrophy, late-onset",
    "Niemann-Pick disease, type C, subacute form",
    "Arylsulfatase A deficiency",
    "Metachromatic leukodystrophy, late infantile type",
    "Metachromatic leukodystrophy",
    "Niemann-Pick disease, type C, acute form",
    "Dystonia due to metachromatic leucodystrophy",
    "Metachromatic leukodystrophy due to sphingolipid activator protein I deficiency",
    "Gaucher disease with ophthalmoplegia and cardiovascular calcification",
    "Encephalopathy due to prosaposin deficiency",
    "Galactocerebroside beta-galactosidase deficiency - early onset",
    "Galactosylceramide beta-galactosidase deficiency",
    "Niemann-Pick disease, type A",
    "Chronic non-neuropathic Gaucher's disease",
    "Metachromatic leukodystrophy without arylsulfatase deficiency",
    "Atypical Gaucher disease due to saposin C deficiency",
    "Sphingolipidosis",
    "Niemann-Pick disease, type C",
    "Acute neuronopathic Gaucher's disease",
    "Autosomal recessive cerebellar ataxia with late-onset spasticity",
    "Metachromatic leukodystrophy, adult type",
    "Metachromatic leukodystrophy, juvenile type",
    "Sphingolipid activator protein 1 deficiency",
    "Perinatal lethal Gaucher disease",
    "Fabry's disease",
    "Niemann-Pick disease, type C, chronic form",
    "Niemann-Pick disease, type D",
    "Metachromatic leukodystrophy due to deficiency of cerebroside sulfatase activator",
    "Subacute neuronopathic Gaucher's disease",
    "B variant hexosaminidase A deficiency - adult",
    "GM2 gangliosidosis",
    "Gangliosidosis",
    "Tay-Sachs disease, variant AB",
    "Infantile GM1 gangliosidosis",
    "Total hexosaminidase deficiency - juvenile",
    "Adult GM1 gangliosidosis",
    "B variant hexosaminidase A deficiency - infantile",
    "B variant hexosaminidase A deficiency - juvenile",
    "Juvenile GM2 gangliosidosis",
    "Sandhoff disease",
    "Juvenile GM1 gangliosidosis",
    "Infantile GM2 gangliosidosis",
    "Tay-Sachs disease",
    "Total hexosaminidase deficiency - adult",
    "B variant hexosaminidase A deficiency",
    "Total hexosaminidase deficiency - infantile",
    "Adult chronic GM2 gangliosidosis",
    "B1 variant hexosaminidase A deficiency",
    "GM1 gangliosidosis",
    "Farber's lipogranulomatosis")

lipid <- c("Metachromatic leukodystrophy, congenital type",
    "Gaucher's disease",
    "Niemann-Pick disease, type B",
    "Multiple sulfatase deficiency",
    "Sphingomyelin/cholesterol lipidosis",
    "Globoid cell leukodystrophy, late-onset",
    "Niemann-Pick disease, type C, subacute form",
    "Arylsulfatase A deficiency",
    "Metachromatic leukodystrophy, late infantile type",
    "Metachromatic leukodystrophy",
    "Niemann-Pick disease, type C, acute form",
    "Dystonia due to metachromatic leucodystrophy",
    "Metachromatic leukodystrophy due to sphingolipid activator protein I deficiency",
    "Gaucher disease with ophthalmoplegia and cardiovascular calcification",
    "Encephalopathy due to prosaposin deficiency",
    "Galactocerebroside beta-galactosidase deficiency - early onset",
    "Galactosylceramide beta-galactosidase deficiency",
    "Niemann-Pick disease, type A",
    "Chronic non-neuropathic Gaucher's disease",
    "Metachromatic leukodystrophy without arylsulfatase deficiency",
    "Atypical Gaucher disease due to saposin C deficiency",
    "Sphingolipidosis",
    "Niemann-Pick disease, type C",
    "Acute neuronopathic Gaucher's disease",
    "Autosomal recessive cerebellar ataxia with late-onset spasticity",
    "Metachromatic leukodystrophy, adult type",
    "Metachromatic leukodystrophy, juvenile type",
    "Sphingolipid activator protein 1 deficiency",
    "Perinatal lethal Gaucher disease",
    "Fabry's disease",
    "Niemann-Pick disease, type C, chronic form",
    "Niemann-Pick disease, type D",
    "Metachromatic leukodystrophy due to deficiency of cerebroside sulfatase activator",
    "Subacute neuronopathic Gaucher's disease",
    "B variant hexosaminidase A deficiency - adult",
    "GM2 gangliosidosis",
    "Gangliosidosis",
    "Tay-Sachs disease, variant AB",
    "Infantile GM1 gangliosidosis",
    "Total hexosaminidase deficiency - juvenile",
    "Adult GM1 gangliosidosis",
    "B variant hexosaminidase A deficiency - infantile",
    "B variant hexosaminidase A deficiency - juvenile",
    "Juvenile GM2 gangliosidosis",
    "Sandhoff disease",
    "Juvenile GM1 gangliosidosis",
    "Infantile GM2 gangliosidosis",
    "Tay-Sachs disease",
    "Total hexosaminidase deficiency - adult",
    "B variant hexosaminidase A deficiency",
    "Total hexosaminidase deficiency - infantile",
    "Adult chronic GM2 gangliosidosis",
    "B1 variant hexosaminidase A deficiency",
    "GM1 gangliosidosis",
    "Genetic disorder of lipid storage",
    "I-cell disease",
    "Lipid storage myopathy",
    "Autosomal dominant myoglobinuria",
    "Primary triglyceride deposit cardiomyovasculopathy",
    "Neutral lipid storage disease with myopathy",
    "Cerebral lipidosis",
    "Triglyceride storage disease with ichthyosis",
    "Lipid storage disease",
    "Juvenile neuronal ceroid lipofuscinosis",
    "Pulmonary lipid storage disease",
    "Salla disease",
    "Chemically-induced lipidosis",
    "Genetic recurrent myoglobinuria",
    "Xanthomatosis, familial",
    "Retinal dystrophy in cerebroretinal lipidosis",
    "Retinal dystrophy in systemic lipidosis",
    "Cholesterol ester storage disease",
    "Neuronal ceroid lipofuscinosis",
    "Progressive myoclonic epilepsy type 3",
    "Adult neuronal ceroid lipofuscinosis",
    "Late-infantile neuronal ceroid lipofuscinosis",
    "Neuronal ceroid lipofuscinosis 8",
    "Infantile neuronal ceroid lipofuscinosis",
    "Congenital neuronal ceroid lipofuscinosis",
    "ATPase cation transporting 13A2 related juvenile neuronal ceroid lipofuscinosis",
    "Lysosomal acid lipase deficiency",
    "Wolman's disease")

fabry <- c("Fabry's disease")

gaucher <-
    c("Gaucher's disease",
    "Subacute neuronopathic Gaucher's disease",
    "Gaucher disease with ophthalmoplegia and cardiovascular calcification",
    "Acute neuronopathic Gaucher's disease",
    "Perinatal lethal Gaucher disease",
    "Atypical Gaucher disease due to saposin C deficiency",
    "Chronic non-neuropathic Gaucher's disease")

metak <- 
    c("Deficiency of cerebroside-sulfatase",
    "Metachromatic leukodystrophy without arylsulfatase deficiency",
    "Metachromatic leukodystrophy, adult type",
    "Metachromatic leukodystrophy, juvenile type",
    "Sphingolipid activator protein 1 deficiency",
    "Metachromatic leukodystrophy due to deficiency of cerebroside sulfatase activator",
    "Arylsulfatase A deficiency",
    "Metachromatic leukodystrophy, late infantile type",
    "Metachromatic leukodystrophy",
    "Dystonia due to metachromatic leucodystrophy",
    "Metachromatic leukodystrophy due to sphingolipid activator protein I deficiency",
    "Metachromatic leukodystrophy, congenital type")

mucopoly <-
    c("Maroteaux-Lamy syndrome, intermediate form",
    "Mucopolysaccharidosis, MPS-VII",
    "Mucopolysaccharidosis, MPS-I-S",
    "Hunter's syndrome, severe form",
    "Morquio syndrome",
    "Mucopolysaccharidosis, MPS-I-H/S",
    "Mucopolysaccharidosis, MPS-III-B",
    "Mucopolysaccharidosis, MPS-IV-A",
    "Hunter's syndrome, mild form",
    "Maroteaux-Lamy syndrome, severe form",
    "Mucopolysaccharidosis",
    "Mucopolysaccharidosis, MPS-III-D",
    "Mucopolysaccharidosis, MPS-I",
    "Mucopolysaccharidosis, MPS-III-A",
    "Mucopolysaccharidosis, MPS-I-H",
    "Mucopolysaccharidosis, MPS-III-C",
    "Sanfilippo syndrome",
    "Maroteaux-Lamy syndrome",
    "Mucopolysaccharidosis, MPS-IV-B",
    "Maroteaux-Lamy syndrome, mild form",
    "Mucopolysaccharidosis, MPS-II",
    "Deficiency of N-acetylgalactosamine-4-sulfatase",
    "Mucopolysaccharidosis-like plus disease")

ncl <- 
    c("Neuronal ceroid lipofuscinosis",
    "Progressive myoclonic epilepsy type 3",
    "Adult neuronal ceroid lipofuscinosis",
    "Late-infantile neuronal ceroid lipofuscinosis",
    "Juvenile neuronal ceroid lipofuscinosis",
    "Neuronal ceroid lipofuscinosis 8",
    "Infantile neuronal ceroid lipofuscinosis",
    "Congenital neuronal ceroid lipofuscinosis",
    "ATPase cation transporting 13A2 related juvenile neuronal ceroid lipofuscinosis")

cys <-
    c("Adult cystinosis",
    "Juvenile nephropathic cystinosis",
    "Benign adult cystinosis",
    "Infantile nephropathic cystinosis",
    "Congenital Fanconi syndrome",
    "Cystinosis")

gang <-
    c("B variant hexosaminidase A deficiency - adult",
    "GM2 gangliosidosis",
    "Gangliosidosis",
    "Tay-Sachs disease, variant AB",
    "Infantile GM1 gangliosidosis",
    "Total hexosaminidase deficiency - juvenile",
    "Adult GM1 gangliosidosis",
    "B variant hexosaminidase A deficiency - infantile",
    "B variant hexosaminidase A deficiency - juvenile",
    "Juvenile GM2 gangliosidosis",
    "Sandhoff disease",
    "Juvenile GM1 gangliosidosis",
    "Infantile GM2 gangliosidosis",
    "Tay-Sachs disease",
    "Total hexosaminidase deficiency - adult",
    "B variant hexosaminidase A deficiency",
    "Total hexosaminidase deficiency - infantile",
    "Adult chronic GM2 gangliosidosis",
    "B1 variant hexosaminidase A deficiency",
    "GM1 gangliosidosis")

amino <- 
    c("Sialic acid storage disease, severe infantile type",
    "Adult cystinosis",
    "Juvenile nephropathic cystinosis",
    "Benign adult cystinosis",
    "Infantile nephropathic cystinosis",
    "Congenital Fanconi syndrome",
    "Cystinosis",
    "Sialuria",
    "Sialic storage disease",
    "Salla disease",
    "Disorder of sialic acid metabolism")

glycopro <-
    c("Glycoprotein storage disorder",
    "Oligosaccharidosis",
    "Dysmorphic sialidosis, infantile form",
    "Mannosidosis",
    "Sialidosis",
    "Infantile fucosidosis",
    "Beta-D-mannosidosis",
    "Juvenile fucosidosis",
    "Alpha-N-acetylgalactosaminidase deficiency type 2",
    "Mannosidosis, type II",
    "Alpha-N-acetylgalactosaminidase deficiency",
    "Aspartylglucosaminuria",
    "Alpha-N-acetylgalactosaminidase deficiency type 1",
    "Dysmorphic sialidosis, juvenile form",
    "Combined deficiency of sialidase AND beta galactosidase",
    "Mannosidosis, type I",
    "Fucosidosis",
    "Sialidosis type 1",
    "Alpha-N-acetylgalactosaminidase deficiency type 3",
    "Adult fucosidosis",
    "Dysmorphic sialidosis, congenital form",
    "Dysmorphic sialidosis",
    "Dysmorphic sialidosis with renal involvement",
    "Mucolipidosis type IV",
    "Pseudo-Hurler polydystrophy",
    "I-cell disease",
    "Mucolipidosis")

other <-
    c("Lysosomal storage disease",
    "Danon disease",
    "Acid phosphatase deficiency",
    "Intestinal lipofuscinosis",
    "Disorder of lysosomal enzyme",
    "Pancreatic triacylglycerol lipase deficiency",
    "Dysostosis multiplex group",
    "Arylsulfatase deficiency without MLD (metachromatic leukodystrophy)",
    "Lipofuscinosis",
    "Dysostosis multiplex")

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.8f5423a8-7919-4673-919a-7b2513090902"),
    all_cohort_ert_exposure=Input(rid="ri.foundry.main.dataset.ebf51d85-06d3-4de0-ad87-a0463f8764fe")
)
library(gtsummary)
library(dplyr)
All_cohort_ert_summary <- function(all_cohort_ert_exposure) {
     df <- all_cohort_ert_exposure %>% 
        dplyr::select(person_id, drug_name) %>%
        dplyr::distinct()  # avoid double-counting people per drug

    ert_long <- df %>%
    dplyr::count(drug_name, name = "on_ert") %>%
        mutate(
            on_ert = if_else(on_ert < 20, "<20", as.character(on_ert))
        )

    return(ert_long)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.98967c34-e20f-4baf-96c9-baf1712f2e89"),
    cohort2=Input(rid="ri.foundry.main.dataset.2be8aaea-6435-48ce-890d-d4441c292af7"),
    cohort_conditions_icd_2_1=Input(rid="ri.foundry.main.dataset.841ff505-990e-49a8-b5f6-27dbcfaac01f")
)
library(survival) # clogit
library(lavaan) # multiple mediation analysis
library(purrr) # map functions
library(dplyr) # data manipulation
library(broom) # tidy glm or clogit object
library(comorbidity) # elix comorbidities
library(tidygraph) # sem ggraph
library(ggraph) # sem graph
library(gtools) # stars.pval
Cov_outcomes_sem <- function( cohort_conditions_icd_2_1, cohort2) {

    co <- cohort2

    pre_covid <- cohort_conditions_icd_2_1 %>% # conditions before COVID
        filter(days_diff <= 0) 
    
    elix <- comorbidity(x = pre_covid, id = "person_id", code = "concept_code", map = "elixhauser_icd10_quan", assign0 = TRUE) # don't double count assign0 = TRUE

    input_df <- co %>%
        left_join(elix, by = "person_id") %>%
        mutate(across(
            -c(person_id, postal_code), 
            ~ replace_na(., 0) # replace empty comorbidities with 0
        ),
        has_lsd = as.numeric(has_lsd),
        Binary_Severity = as.numeric(Binary_Severity)
        )

    concept_list <- list(
        sphingo   = sphingo,
        lipid     = lipid,
        fabry     = fabry,
        gaucher   = gaucher,
        metak     = metak,
        amino     = amino,
        gang      = gang,
        cys       = cys,
        ncl       = ncl
    )

    mediators_map <- list(
        sphingo = c("carit", "hypunc", "para", "ond", "diabunc", "diabc", "ld", "rheumd", "wloss", "fed", "alcohol"),
        lipid   = c("carit", "para", "ond", "diabc", "ld", "rheumd", "wloss", "dane", "alcohol"),
        fabry   = c("valv", "rf"),
        gaucher = c("coag"),
        metak   = c("para", "ond"),
        gang    = c("ond"),
        ncl     = c("ond")
        # amino, cys intentionally omitted → mediators_map[[ "amino" ]] and [[ "cys" ]] will be NULL
        )

    df_list <- purrr::imap(concept_list, function(cond_vec, nm) {
        split_df(input_df, cond_vec) %>%
            dplyr::mutate(!!rlang::sym(nm) := ifelse(lsd_condition_name %in% cond_vec, 1, 0))
        })

    outcome <- "Binary_Severity"

    make_sem_model <- function(exposure, outcome, mediators) {
  
        # a-paths: mediator ~ a*exposure
        a_paths <- paste0(mediators, " ~ a", seq_along(mediators), "*", exposure)
        
        # b-paths: outcome ~ b*mediator
        b_paths <- paste0(outcome, " ~ ", paste0("b", seq_along(mediators), "*", mediators, collapse = " + "))
        
        # indirect effects: ind_med := a*b
        ind_defs <- paste0("ind_", mediators, " := a", seq_along(mediators), "*b", seq_along(mediators))
        
        # total indirect
        total_ind <- paste0("total_ind := ", paste0("ind_", mediators, collapse = " + "))
        
        # direct path
        direct <- paste0(outcome, " ~ c_prime*", exposure)
        
        # total effect
        total <- "total := c_prime + total_ind"
        
        # combine all
        model_string <- paste(
            "# direct effect",
            direct, "",
            "# a-paths",
            paste(a_paths, collapse = "\n"), "",
            "# b-paths",
            paste(b_paths, collapse = "\n"), "",
            "# indirects",
            paste(ind_defs, collapse = "\n"), "",
            "# totals",
            total_ind, total,
            sep = "\n"
        )
        
        return(model_string)
        }

    model_str <- make_sem_model(exposure = "sphingo", outcome = "Binary_Severity", mediators = mediators_map$sphingo)
    print(model_str)

    sem_results <- purrr::imap_dfr(df_list, function(dat, nm) {
        layout    <- if (nm == "sphingo") "sugiyama" else "stress"
        mediators <- mediators_map[[nm]]

        if (is.null(mediators)) return(NULL)

        model_str <- make_sem_model(
            exposure  = nm,
            outcome   = outcome, 
            mediators = mediators
        )
        
        fit.sem <- lavaan::sem(
            model_str,
            data     = dat,
            ordered  = c(outcome, mediators),
            estimator = "WLSMV"
        )

        # include unstandardized and standardized estimates for better comparison to log-link
        pe <- parameterEstimates(fit.sem) %>% 
            as_tibble() %>% 
            mutate(
            estimate_type = "unstandardized",
            ld_name       = nm
            )

        ss <- standardizedSolution(fit.sem) %>% 
            as_tibble() %>% 
            mutate(
            estimate_type = "standardized",
            ld_name       = nm
            )

        param_long <- dplyr::bind_rows(pe, ss)

        edges <- ss %>% 
            filter(op == "~") %>% 
            transmute(
            from = rhs,
            to   = lhs,
            est  = round(est.std, 2),
            pval = pvalue
            )

        comorbidity_map <- c(
            chf      = "Congestive heart failure",
            carit    = "Cardiac arrhythmias",
            valv     = "Valvular disease",
            pcd      = "Pulmonary circulation disorders",
            pvd      = "Peripheral vascular disorders",
            hypunc   = "Hypertension, uncomplicated",
            hypc     = "Hypertension, complicated",
            para     = "Paralysis",
            ond      = "Other neurological disorders",
            cpd      = "Chronic pulmonary disease",
            diabunc  = "Diabetes, uncomplicated",
            diabc    = "Diabetes, complicated",
            hypothy  = "Hypothyroidism",
            rf       = "Renal failure",
            ld       = "Liver disease",
            pud      = "Peptic ulcer disease, excluding bleeding",
            aids     = "AIDS/HIV",
            lymph    = "Lymphoma",
            metacanc = "Metastatic cancer",
            solidtum = "Solid tumor, without metastasis",
            rheumd   = "Rheumatoid arthritis/collagen vascular disease",
            coag     = "Coagulopathy",
            obes     = "Obesity",
            wloss    = "Weight loss",
            fed      = "Fluid and electrolyte disorders",
            blane    = "Blood loss anemia",
            dane     = "Deficiency anemia",
            alcohol  = "Alcohol abuse",
            drug     = "Drug abuse",
            psycho   = "Psychoses",
            depre    = "Depression",
            sphingo  = "Sphingolipidosis",
            lipid    = "Lipid storage disease",
            fabry    = "Fabry's disease",
            metak    = "Metachromatic leukodystrophy",
            gaucher  = "Gaucher's disease",
            amino    = "Disorder of lysosomal\namino acid transport",
            gang     = "Gangliosidosis",
            cys      = "Cystinosis",
            ncl      = "Neuronal ceroid lipofuscinosis",
            Binary_Severity = "Hospitalized"
        )

        edges <- edges %>% 
            mutate(
            p_stars = ifelse(pval < 0.05, stars.pval(pval), ""),
            label   = paste0(est, p_stars),
            from    = recode(from, !!!comorbidity_map),
            to      = recode(to,   !!!comorbidity_map)
            )

        nodes <- tibble(name = unique(c(edges$from, edges$to)))

        g <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
        set.seed(30)

        dag <- ggraph(g, layout = layout) +
            geom_edge_link(
            aes(label = label),
            arrow      = arrow(type = "closed", length = unit(3, "mm")),
            color      = "black",
            angle_calc = "along",
            label_dodge = unit(2.5, "mm"),
            start_cap  = ggraph::square(2, "mm"),
            end_cap    = ggraph::square(2, "mm")
            ) +
            geom_node_label(aes(label = name),
                            size = 4, label.r = unit(0, "lines"),
                            fill = "white", repel = TRUE) +
            coord_flip() +
            theme_void(base_size = 16)

        plot(dag)

        param_long
    })

    return(sem_results)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.30533199-8d47-47ed-82ce-ab8a293586f3"),
    cohort_phecodes_2_1=Input(rid="ri.foundry.main.dataset.977b8ef2-7d67-414f-85e0-2f5524a772aa"),
    phecode_map=Input(rid="ri.foundry.main.dataset.c2bec0d5-a47e-420c-9f1f-1526771f00b7")
)
library(dplyr)
library(tidyr)
acute_covid_phecodes <- function(cohort_phecodes_2_1, phecode_map) {
    map <- phecode_map %>% select(phecode, phecode_string, `category`) %>% distinct()
     
     phe_df <- cohort_phecodes_2_1 %>%
        filter(
           days_diff <= 15 & days_diff > 0, 
            observation_period_before_covid >= 365,
            observation_period_post_covid >= 365, 
            !grepl("^(GE_|CM_|NB_|PP_)", prefix)) %>%# only lsd patients, outside acute window, excluding COVID and genetic terms and patient must have sufficient history
        mutate(has_lsd = case_when(
            has_lsd == 1 ~ "has_lsd",
            has_lsd == 0 ~ "no_lsd"
        )) %>%
        group_by(person_id, phecode, covid_period) %>%
        slice_min(order_by = condition_start_date, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        distinct()
       
   totals <- phe_df %>%
        distinct(person_id, has_lsd) %>%
        count(has_lsd, name = "lsd_count") %>%
        pivot_wider(
            names_from = has_lsd,
            values_from = lsd_count,
            names_prefix = "total_"
        )

    contingency <- phe_df %>%
        distinct(person_id, phecode, has_lsd) %>%
        count(phecode, has_lsd, name = "phecode_count") %>%
        pivot_wider(
            names_from = has_lsd,
            values_from = phecode_count,
            values_fill = 0
        ) %>%
        crossing(totals) %>% 
        mutate(
            phecode,
            a = has_lsd,                 # with phecode & has_lsd
            b = total_has_lsd - a,       # has_lsd without phecode
            c = no_lsd,              # with phecode & no_lsd
            d = total_no_lsd - c,     # no_lsd without phecode
            .keep = "none"
        ) %>% dplyr::filter( # sanity check filter
                a >= 20,
                c >= 20
        )
            
    results <- contingency %>%
        mutate(
            test = mapply(function(a, b, c, d) {
            ft <- fisher.test(matrix(c(a, b, c, d), nrow = 2))
            list(
                odds_ratio = unname(ft$estimate),
                lower_ci   = ft$conf.int[1],
                upper_ci   = ft$conf.int[2],
                p_value    = ft$p.value
            )
            }, a, b, c, d, SIMPLIFY = FALSE)
        ) %>%
        tidyr::unnest_wider(test) %>%
        mutate(
            log2_or = log2(odds_ratio),
            p_adj = p.adjust(p_value, method = "fdr"),
            log_p = -log10(p_adj),
            enriched_lsd = odds_ratio > 2 & is.finite(odds_ratio) & p_adj < 0.05,
            enriched_control = odds_ratio < 0.5 & is.finite(odds_ratio) & p_adj < 0.05,
            significant = p_adj < 0.05
        ) %>%
        inner_join(map, by = "phecode") %>%
        arrange(desc(odds_ratio), p_adj)
        
    return(results)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ca3e686b-a2cd-4f80-8494-fc7458661c3e"),
    all_imputed=Input(rid="ri.foundry.main.dataset.e29af5b3-d495-4d88-8eca-f39d24a8ac51")
)
library(MatchIt)
library(dplyr)
library(cobalt)
library(ggplot2)
all_cohort1 <- function(all_imputed) {
    set.seed(128) # very important
    df1 <- all_imputed

    df2 <- df1 %>%
        dplyr::mutate( # Convert columns to data types suitable for cohort matching
            Race = factor(Race, levels = c("White", "Black or African American", "Asian", 
                                          "American Indian or Alaska Native", 
                                          "Native Hawaiian or Other Pacific Islander", 
                                          "Other/Unknown", "Hispanic or Latino")),
            Age = as.numeric(Age),
            data_partner_id = as.factor(data_partner_id),
            has_lsd = factor(has_lsd, levels = c(0, 1)),
            Sex = as.factor(Sex),
            log_obv = log1p(days_observed)
        ) %>% 
        dplyr::select(person_id, Race, Age, Sex, data_partner_id, has_lsd, log_obv) 

    matched <- matchit(
        has_lsd ~ Age + Race + Sex + log_obv,
        method = "nearest",
        distance = "mahalanobis",
        ratio = 2,
        data = df2, 
        exact = c("data_partner_id")
    )

    print(summary(matched))
    
    matched_data <- match.data(matched, data = df2) 

    # Love plot for all variables
    l <- love.plot(matched, stats = "m",
        abs = FALSE, shapes = 15, size = 5, sample.names = c("Unmatched", "Matched"), 
        thresholds = c(m = .05),
        colors = c("#A463F2FF", "#3CA951FF")) +
        labs(title = "") + 
        scale_y_discrete(labels = c("Race_Native Hawaiian or Other Pacific Islander" = "Race_Native Hawaiian\nor Other Pacific Islander", 
            "Race_American Indian or Alaska Native" = "Race_American Indian\nor Alaska Native", "Race_Black or African American" = "Race_Black\nor African American", "log_obv" = "ln(# days observed)"
            ),
                    limits = c("Age", "Race_White", "Race_Black or African American", "Race_Asian", 
                              "Race_American Indian or Alaska Native", 
                              "Race_Native Hawaiian or Other Pacific Islander", 
                               "Race_Other/Unknown", "Sex_Male", "Sex_Female", "Sex_Unknown", "log_obv")) +
        guides(color = guide_legend(override.aes = list(size = 5))) +
        theme_classic(base_size = 16) +
        theme(aspect.ratio = 3/2) 
    plot(l)

    # Balance plots for select variables
    features <- c("Age", "Sex")
    for (f in features) {
        k <- bal.plot(matched, var.name = f, which = "both", sample.names = c("Unmatched", "Matched"), disp.means = TRUE) +
            labs(title = "") +
            scale_fill_manual(
                name = "Group",
                values = c(`0` = "#F8766D", `1` = "#00BFC4"),
                labels = c(`0` = "Unaffected", `1` = "Affected")
            ) +
            theme_classic(base_size = 16)
        
        plot(k)
    }

    # Race balance plot (fine because Race has limited levels)
    r <- bal.plot(matched, var.name = "Race", which = "both", sample.names = c("Unmatched", "Matched"), disp.means = TRUE) +
        labs(title = "") +
        scale_fill_manual(
                name = "Group",
                values = c(`0` = "#F8766D", `1` = "#00BFC4"),
                labels = c(`0` = "Unaffected", `1` = "Affected") # Unaffected Affected
            ) +
        scale_x_discrete(
                    limits = c("White", "Black or African American", "Asian", 
                              "American Indian or Alaska Native", "Native Hawaiian or Other Pacific Islander",
                               "Other/Unknown")) + 
        theme_classic(base_size = 16) +
        theme(axis.text.x = element_text(hjust = 1.0, vjust = 1.0, angle = 45))

    plot(r)

    # # Join back extra columns
    extra_cols <- df1 %>% dplyr::select(person_id, lsd_condition_name, COVID_pos_indicator, days_observed, number_of_visits, postal_code, BMI_overall, combined_number_of_COVID_vaccine_doses)
    cohort <- dplyr::inner_join(extra_cols, matched_data, by = "person_id")

    return(cohort)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.46a184a8-8b03-4432-bbe9-dd084956feef"),
    all_cohort_death=Input(rid="ri.foundry.main.dataset.fc6a2e9a-5d95-43ea-b5dc-4a8d4972c867")
)
library(dplyr)
library(gtsummary)
all_cohort_death_summary <- function(all_cohort_death) {
    death <- all_cohort_death %>%
        dplyr::select(person_id, lsd_condition_name, has_lsd, COVID_pos_indicator) %>%
        dplyr::distinct()

    message("Total number of deaths in non-COVID cohort= ", nrow(death))

    lsd_stats <- death %>%
        group_by(lsd_condition_name) %>%
        summarise(
            n_deaths = n()
        ) %>%
        ungroup() %>%
        arrange(desc(n_deaths)) %>%
        print(n=Inf)
    
    has_lsd_cov_stats <- death %>%
        group_by(COVID_pos_indicator, has_lsd) %>%
        summarise(
            n_deaths = n()
        ) %>%
        print(n=Inf)
    
    has_lsd_stats <- death %>%
        group_by(has_lsd) %>%
        summarise(
            n_deaths = n()
        ) %>%
        print(n=Inf)

     has_cov_stats <- death %>%
        group_by(COVID_pos_indicator) %>%
        summarise(
            n_deaths = n()
        ) %>%
        print(n=Inf)

    lsd_stats_clean <- lsd_stats %>%
        mutate(
            n_deaths = if_else(n_deaths < 20,"<20", as.character(n_deaths))
        )

    return(lsd_stats_clean)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2add8536-9541-4ec5-8982-9f76d4aa0a29"),
    all_cohort_conditions_icd=Input(rid="ri.foundry.main.dataset.e29718de-5a2e-4838-93aa-4da82d3d656b")
)
library(dplyr)
library(comorbidity)
all_cohort_elix <- function(all_cohort_conditions_icd) {
    anytime <- all_cohort_conditions_icd
    lsd_stat <- anytime %>% select(person_id, has_lsd, COVID_pos_indicator, number_of_visits, days_observed) %>% 
        distinct() %>%
        mutate(
            visits_per_year = (number_of_visits / days_observed) * 365,
            p25 = quantile(visits_per_year, 0.25, na.rm = TRUE),
            p75 = quantile(visits_per_year, 0.75, na.rm = TRUE),
            utilization = cut(
                visits_per_year,
                breaks = c(-Inf, p25[1], p75[1], Inf),
                labels = c("util_low", "util_moderate", "util_high"),
                include.lowest = TRUE
            )
        ) %>%
        select(-p25, -p75)

    print(quantile(lsd_stat$visits_per_year, probs = c(0.25, 0.75)))

    # map ICD codes to comorbidities 
    elix <- comorbidity(x = anytime, id = "person_id", code = "concept_code", map = "elixhauser_icd10_quan", assign0 = TRUE)

    elix_score <- score(x = elix, weights = "swiss", assign0 = TRUE) # swiss or wv. wv was in canada 2009 345k patients, swiss is switzerland 2012-2017 6 million patients

    # add score and has_lsd in
    elix$elix_score <- elix_score
    elix <- elix %>% inner_join(lsd_stat, by = "person_id") %>%
        select(-number_of_visits, -visits_per_year, -days_observed)

    stats <- elix %>%
        group_by(has_lsd) %>%
        summarise(
            median = median(elix_score, na.rm = TRUE),
            q1 = quantile(elix_score, 0.25, na.rm = TRUE),
            q3 = quantile(elix_score, 0.75, na.rm = TRUE)
        )

    w <- wilcox.test(elix_score ~ has_lsd, data = elix)
    print(w)    
    print(stats)

    return(elix)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.dbbc4bb1-5589-422c-b4e7-4cd362eda086"),
    enrichment_prep=Input(rid="ri.foundry.main.dataset.3326f9d3-68c4-4993-a336-b6764c27d290"),
    phecode_map=Input(rid="ri.foundry.main.dataset.c2bec0d5-a47e-420c-9f1f-1526771f00b7")
)
library(dplyr)
library(tidyr)
library(purrr)
all_cohort_enriched_phecodes <- function( phecode_map, enrichment_prep) {
     map <- phecode_map %>% select(phecode, phecode_string, `category`) %>% distinct()
     
    phe_df <- enrichment_prep %>%
        mutate(has_lsd = ifelse(has_lsd == 1, "has_lsd", "no_lsd"))
       
   totals <- phe_df %>%
        distinct(person_id, has_lsd) %>%
        count(has_lsd, name = "lsd_count") %>%
        pivot_wider(
            names_from = has_lsd,
            values_from = lsd_count,
            names_prefix = "total_"
        )

    contingency <- phe_df %>%
        distinct(person_id, phecode, has_lsd) %>%
        count(phecode, has_lsd, name = "phecode_count") %>%
        pivot_wider(
            names_from = has_lsd,
            values_from = phecode_count,
            values_fill = 0
        ) %>%
        mutate(
            phecode = phecode,
            total_has_lsd = totals$total_has_lsd,
            total_no_lsd = totals$total_no_lsd,
            a = has_lsd,                 # with phecode & has_lsd
            b = total_has_lsd - a,       # has_lsd without phecode
            c = no_lsd,              # with phecode & no_lsd
            d = total_no_lsd - c,     # no_lsd without phecode
            .keep = "none"
        ) %>%
        dplyr::filter( # sanity check filter
                a >= 20,
                c >= 20
        )

    fisher_results <- contingency %>%
        select(phecode, a, b, c, d) %>%
        pmap_dfr(function(phecode, a, b, c, d) {
        ft <- fisher.test(matrix(c(a, b, c, d), nrow = 2))
        tibble(
            phecode,
            a, b, c, d,
            odds_ratio  = unname(ft$estimate),
            lower_ci    = ft$conf.int[1],
            upper_ci    = ft$conf.int[2],
            p_value     = ft$p.value
        )
        })

    results <- fisher_results %>%
        mutate(
            log2_or = log2(odds_ratio),
            p_adj   = p.adjust(p_value, method = "fdr"),
            log_p   = -log10(p_adj),
            enriched_lsd     = odds_ratio > 2 & is.finite(odds_ratio) & p_adj < 0.05,
            enriched_control = odds_ratio < 0.5 & is.finite(odds_ratio) & p_adj < 0.05,
            significant      = p_adj < 0.05
        ) %>%
        left_join(map, by = "phecode") %>%
        arrange(desc(odds_ratio), p_adj)

    return(results)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d383f14f-a3d6-4c9e-9a1b-de058e51dc3f"),
    all_lsd_prep=Input(rid="ri.foundry.main.dataset.8feaa134-5dd4-4418-a291-5f8fe583b9b8")
)
library(mice)
library(dplyr)
all_lsd_imputation <- function(all_lsd_prep) {
    # works with environment profile-high-driver-cores-and-memory
    set.seed(2048)

    df1 <- all_lsd_prep

    sub_df <- df1 %>% # df for imputation
        select(Age, Race, Sex, BMI_overall, days_observed, number_of_visits, COVID_pos_indicator, lsd_condition_name, data_partner_id) %>%
        mutate(
            Age = as.integer(Age),
            Race = as.factor(Race),
            Sex = as.factor(Sex),
            BMI_overall = as.numeric(BMI_overall),
            days_observed = as.integer(days_observed),
            number_of_visits = as.integer(number_of_visits),
            COVID_pos_indicator = as.integer(COVID_pos_indicator),
            lsd_condition_name = as.factor(lsd_condition_name),
            data_partner_id = as.factor(data_partner_id)
        )

    # print proportion missing
    print(mean(is.na(sub_df$Age)))
    print(mean(is.na(sub_df$BMI_overall)))

    impute <- mice(sub_df, m=2, method = "pmm", maxit = 5, seed = 2048) # Impute age # and BMI using predictive mean matching. m = number of imputed datasets, make this lower if you have OOM errors

    # return the imputed "mids" object to dataframe
    imp_df <- mice::complete(impute,2) # there is also a complete function in dplyr, so be careful here. select 2nd dataset

    df1$Age <- as.integer(imp_df$Age)
    df1$BMI_overall <- as.numeric(imp_df$BMI_overall)

    return(df1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d8f95670-8a0e-4d11-bb50-8a28ab56cd9f"),
    all_non_lsd_prep=Input(rid="ri.foundry.main.dataset.b7aa593b-5428-4b25-ad90-f0ba001d9840")
)
library(mice)
#library(furrr) # for futuremice
library(dplyr)
all_non_lsd_imputation <- function(all_non_lsd_prep) {
    # works with environment profile-high-driver-cores-and-memory
    set.seed(2048)

    #options(future.globals.maxSize = 20 * 1024^3) # 20 GB

    df1 <- all_non_lsd_prep

   sub_df <- df1 %>% # df for imputation
        select(Age, Race, Sex, BMI_overall, days_observed, number_of_visits, COVID_pos_indicator, data_partner_id) %>%
        mutate(
            Age = as.integer(Age),
            Race = as.factor(Race),
            Sex = as.factor(Sex),
            BMI_overall = as.numeric(BMI_overall),
            days_observed = as.integer(days_observed),
            number_of_visits = as.integer(number_of_visits),
            COVID_pos_indicator = as.integer(COVID_pos_indicator),
            data_partner_id = as.factor(data_partner_id)
        )

    # print proportion missing
    print(mean(is.na(sub_df$Age)))
    print(mean(is.na(sub_df$BMI_overall)))

    # # make method
    # met <- make.method(sub_df)
    # met[] <- ""                # disable imputation for all
    # met["Age"] <- "pmm" # pmm only for Age and BMI
    # met["BMI_overall"] <- "pmm"

    # # make predictor matrix
    # pred <- make.predictorMatrix(sub_df) 
    # pred[,] <- 0 # impute only Age and BMI

    # # Age predicted by Race, Sex, BMI, days_observed, COVID status, data partner
    # pred["Age", c("Race", "Sex", "BMI_overall", "days_observed","number_of_visits", "COVID_pos_indicator", "data_partner_id")] <- 1

    # # BMI predicted by Age, Race, Sex, days_observed, COVID status, data partner
    # pred["BMI_overall", c("Age", "Race", "Sex", "days_observed", "number_of_visits", "COVID_pos_indicator", "data_partner_id")] <- 1

     impute <- mice(sub_df, m=2, method = "pmm", maxit = 5, seed = 2048) # Impute age # and BMI using predictive mean matching. m = number of imputed datasets, make this lower if you have OOM errors

    # return the imputed "mids" object to dataframe
    imp_df <- mice::complete(impute,2) # there is also a complete function in dplyr, so be careful here. select 2nd dataset

    df1$Age <- as.integer(imp_df$Age)
    df1$BMI_overall <- as.numeric(imp_df$BMI_overall)

    # impute <- mice(sub_df, m=1, method = met, predictorMatrix = pred, donors = 3, maxit = 5, seed = 2048) # impute Age and BMI with pmm, lower donors to 3 instead of 5

    # # return the imputed "mids" object to dataframe
    # imp_df <- mice::complete(impute,1) # there is also a complete function in dplyr, so be careful here. select 2nd dataset
    
    # # # replace columns in original dataframe with imputed column
    # df1$Age <- as.integer(imp_df$Age)
    # df1$BMI_overall <- as.numeric(imp_df$BMI_overall)

    return(df1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.39b79347-c502-4b17-9b7e-9501c9260947"),
    all_cohort1=Input(rid="ri.foundry.main.dataset.ca3e686b-a2cd-4f80-8494-fc7458661c3e"),
    all_cohort_elix=Input(rid="ri.foundry.main.dataset.2add8536-9541-4ec5-8982-9f76d4aa0a29")
)
library(dplyr)
library(gtsummary)
all_table_1 <- function(all_cohort1, all_cohort_elix) {
    
    co <- all_cohort1

    elix <- all_cohort_elix %>%
        dplyr::select(
            -COVID_pos_indicator,
            -has_lsd,
            -utilization
        )
    
    cohort_df <- co %>%
        dplyr::select(person_id,
        number_of_visits,
        days_observed,
        BMI_overall,
        COVID_pos_indicator,
        lsd_condition_name,
        has_lsd,
        Race,
        Age,
        Sex)

     comorbid <- cohort_df %>%
         left_join(elix, by = "person_id") %>%
         mutate(across(
            -c(person_id, elix_score),  # all columns except elix_score and person_id replaced with zero
            ~ replace_na(., 0)
        ))

    atlantic <- tribble(
        ~district, ~zip3_codes, ~region,
        "CONNECTICUT", "060, 061, 062, 063, 064, 065, 066, 067, 068, 069", "ATLANTIC",
        "DE-PA2", "180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199", "ATLANTIC",
        "MA-RI", "010, 011, 012, 013, 014, 015, 016, 017, 018, 019, 020, 021, 022, 023, 024, 025, 026, 027, 028, 029, 055", "ATLANTIC",
        "MARYLAND", "200, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 214, 215, 216, 217, 218, 219", "ATLANTIC",
        "ME-NH-VT", "030, 031, 032, 033, 034, 035, 036, 037, 038, 039, 040, 041, 042, 043, 044, 045, 046, 047, 048, 049, 050, 051, 052, 053, 054, 056, 057, 058, 059", "ATLANTIC",
        "NEW JERSEY", "070, 071, 072, 073, 074, 075, 076, 077, 078, 079, 080, 081, 082, 083, 084, 085, 086, 087, 088, 089", "ATLANTIC",
        "NEW YORK 1", "100, 101, 102, 103, 104, 112", "ATLANTIC",
        "NEW YORK 2", "005, 110, 111, 113, 114, 115, 116, 117, 118, 119", "ATLANTIC",
        "NEW YORK 3", "105, 106, 107, 108, 109, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149", "ATLANTIC",
        "NORTH CAROLINA", "270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, 283, 284, 285, 286, 287, 288, 289", "ATLANTIC",
        "PENNSYLVANIA 1", "150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179", "ATLANTIC",
        "VIRGINIA", "201, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246", "ATLANTIC"
    )

    central <- tribble(
        ~district, ~zip3_codes, ~region,
        "IA-NE-SD", "500, 501, 502, 503, 504, 505, 506, 507, 508, 509, 510, 511, 512, 513, 514, 515, 516, 520, 521, 522, 523, 524, 525, 526, 527, 528, 570, 571, 572, 573, 574, 575, 576, 577, 680, 681, 683, 684, 685, 686, 687, 688, 689, 690, 691, 692, 693", "CENTRAL",
  "ILLINOIS 1", "600, 601, 602, 603, 606, 607, 608, 610, 611", "CENTRAL",
  "ILLINOIS 2", "604, 605, 609, 612, 613, 614, 615, 616, 617, 618, 619, 620, 622, 623, 624, 625, 626, 627, 628, 629", "CENTRAL",
  "INDIANA", "460, 461, 462, 463, 464, 465, 466, 467, 468, 469, 470, 471, 472, 473, 474, 475, 476, 477, 478, 479", "CENTRAL",
  "KS-MO", "630, 631, 633, 634, 635, 636, 637, 638, 639, 640, 641, 644, 645, 646, 647, 648, 649, 650, 651, 652, 653, 654, 655, 656, 657, 658, 660, 661, 662, 664, 665, 666, 667, 668, 669, 670, 671, 672, 673, 674, 675, 676, 677, 678, 679", "CENTRAL",
  "KY-WV", "247, 248, 249, 250, 251, 252, 253, 254, 255, 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 267, 268, 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416, 417, 418, 420, 421, 422, 423, 424, 425, 426, 427", "CENTRAL",
  "MICHIGAN 1", "480, 481, 482, 483, 484, 485, 492", "CENTRAL",
  "MICHIGAN 2", "486, 487, 488, 489, 490, 491, 493, 494, 495, 496, 497, 498, 499", "CENTRAL",
  "MN-ND", "550, 551, 553, 554, 555, 556, 557, 558, 559, 560, 561, 562, 563, 564, 565, 566, 567, 580, 581, 582, 583, 584, 585, 586, 587, 588", "CENTRAL",
  "OHIO 1", "434, 435, 436, 439, 440, 441, 442, 443, 444, 445, 446, 447, 448, 449, 458", "CENTRAL",
  "OHIO 2", "430, 431, 432, 433, 437, 438, 450, 451, 452, 453, 454, 455, 456, 457, 459", "CENTRAL",
  "WISCONSIN", "530, 531, 532, 534, 535, 537, 538, 539, 540, 541, 542, 543, 544, 545, 546, 547, 548, 549", "CENTRAL"
    )

    southern <- tribble(
    ~district, ~zip3_codes, ~region,
    "AL-MS", "350, 351, 352, 354, 355, 356, 357, 358, 359, 360, 361, 362, 363, 364, 365, 366, 367, 368, 369, 386, 387, 388, 389, 390, 391, 392, 393, 394, 395, 396, 397", "SOUTHERN",
    "AR-OK", "716, 717, 718, 719, 720, 721, 722, 723, 724, 725, 726, 727, 728, 729, 730, 731, 734, 735, 736, 737, 738, 739, 740, 741, 743, 744, 745, 746, 747, 748, 749", "SOUTHERN",
    "FLORIDA 1", "320, 321, 322, 323, 324, 325, 326, 327, 344", "SOUTHERN",
    "FLORIDA 2", "328, 329, 335, 336, 337, 338, 339, 341, 342, 346, 347", "SOUTHERN",
    "FLORIDA 3", "330, 331, 332, 333, 334, 349", "SOUTHERN",
    "GEORGIA", "300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317, 318, 319, 398, 399", "SOUTHERN",
    "LOUISIANA", "700, 701, 703, 704, 705, 706, 707, 708, 710, 711, 712, 713, 714", "SOUTHERN",
    "PUERTO RICO", "006, 007, 008, 009", "SOUTHERN",
    "SOUTH CAROLINA", "290, 291, 292, 293, 294, 295, 296, 297, 298, 299", "SOUTHERN",
    "TENNESSEE", "370, 371, 372, 373, 374, 375, 376, 377, 378, 379, 380, 381, 382, 383, 384, 385", "SOUTHERN",
    "TEXAS 1", "750, 751, 752, 753, 754, 755, 756, 757, 758, 759, 760, 761, 762, 763, 764, 766, 767", "SOUTHERN",
    "TEXAS 2", "770, 772, 773, 774, 775, 776, 777, 778, 779, 783, 784, 785", "SOUTHERN",
    "TEXAS 3", "733, 765, 768, 769, 780, 781, 782, 786, 787, 788, 789, 790, 791, 792, 793, 794, 795, 796, 797, 798, 799, 885", "SOUTHERN"
    )

    western_pacific <- tribble(
    ~district, ~zip3_codes, ~region,
    "ALASKA", "995, 996, 997, 998, 999", "WESTERN-PACIFIC",
    "AZ-NM", "850, 851, 852, 853, 855, 856, 857, 859, 860, 863, 864, 865, 870, 871, 873, 874, 875, 876, 877, 878, 879, 880, 881, 882, 883, 884", "WESTERN-PACIFIC",
    "CALIFORNIA 1", "940, 941, 943, 944, 949, 950, 951, 954, 955, 959, 960", "WESTERN-PACIFIC",
    "CALIFORNIA 2", "942, 945, 946, 947, 948, 952, 956, 957, 958, 961", "WESTERN-PACIFIC",
    "CALIFORNIA 3", "913, 914, 915, 916, 930, 931, 932, 933, 934, 935, 936, 937, 938, 939, 953", "WESTERN-PACIFIC",
    "CALIFORNIA 4", "910, 911, 912, 917, 918, 926, 927, 928", "WESTERN-PACIFIC",
    "CALIFORNIA 5", "900, 901, 902, 903, 904, 905, 906, 907, 908", "WESTERN-PACIFIC",
    "CALIFORNIA 6", "919, 920, 921, 922, 923, 924, 925", "WESTERN-PACIFIC",
    "CO-WY", "800, 801, 802, 803, 804, 805, 806, 807, 808, 809, 810, 811, 812, 813, 814, 815, 816, 820, 821, 822, 823, 824, 825, 826, 827, 828, 829, 830, 831","WESTERN-PACIFIC",
    "HAWAII", "967, 968, 969", "WESTERN-PACIFIC",
    "ID-MT-OR", "590, 591, 592, 593, 594, 595, 596, 597, 598, 599, 832, 833, 834, 835, 836, 837, 838, 970, 971, 972, 973, 974, 975, 976, 977, 978, 979", "WESTERN-PACIFIC",
    "NV-UT", "840, 841, 842, 843, 844, 845, 846, 847, 889, 890, 891, 893, 894, 895, 897, 898", "WESTERN-PACIFIC",
    "WASHINGTON", "980, 981, 982, 983, 984, 985, 986, 988, 989, 990, 991, 992, 993, 994", "WESTERN-PACIFIC"
    )

    all_zip3 <- bind_rows(atlantic, central, southern, western_pacific) %>%
        separate_rows(zip3_codes, sep = ",\\s*") %>%
        rename(zip3 = zip3_codes) %>%
        select(zip3, region)

    co_zips <- co %>%
        left_join(all_zip3, join_by(postal_code == zip3)) %>%
        mutate(region = replace_na(region, "UNKNOWN")) %>%
        select(person_id, region)

    df <- comorbid %>%
        inner_join(co_zips, by = "person_id") %>%
        mutate(
                Sex = factor(Sex, levels = c('Female', 'Male', 'Unknown')),
                Age = as.integer(Age), 
                Race = case_when(
                    Race == "Unknown" | Race == "Other/Unknown" ~ "Other/Unknown",
                    TRUE ~ Race
                ),
                Race = factor(Race, levels = c("White",
                    "Black or African American",
                    "Asian",
                    "American Indian or Alaska Native",
                    "Native Hawaiian or Other Pacific Islander",
                    "Other/Unknown")),
                has_lsd = case_when(
                    has_lsd == 0 ~ "Control",
                    has_lsd == 1 ~ "LD"
                ),
                lsd_class = case_when(
                    lsd_condition_name %in% c(
                    "None",
                    "Lipid storage disease",
                    "Sphingolipidosis",
                    "Fabry's disease",
                    "Metachromatic leukodystrophy",
                    "Gaucher's disease",
                    "Mucopolysaccharidosis",
                    "Mucopolysaccharidosis, MPS-I-H",
                    "Neuronal ceroid lipofuscinosis",
                    "Cystinosis",
                    "Mucopolysaccharidosis, MPS-II",
                    "Tay-Sachs disease",
                    "Mucopolysaccharidosis, MPS-IV-A",
                    "Galactosylceramide beta-galactosidase deficiency",
                    "GM2 gangliosidosis",
                    "Niemann-Pick disease, type C",
                    "Gangliosidosis",
                    "Sphingomyelin/cholesterol lipidosis",
                    "Niemann-Pick disease, type B",
                    "Mucopolysaccharidosis, MPS-I-H/S",
                    "Danon disease",
                    "Cerebral lipidosis",
                    "Sandhoff disease",
                    "Mucopolysaccharidosis, MPS-I-S"
                    ) ~ lsd_condition_name,
                TRUE ~ "Other"
                ),
                lsd_class = factor(lsd_class, levels = c(
            "None",
            "Lipid storage disease",
            "Sphingolipidosis",
            "Fabry's disease",
            "Metachromatic leukodystrophy",
            "Gaucher's disease",
            "Mucopolysaccharidosis",
            "Mucopolysaccharidosis, MPS-I-H",
            "Neuronal ceroid lipofuscinosis",
            "Cystinosis",
            "Mucopolysaccharidosis, MPS-II",
            "Tay-Sachs disease",
            "Mucopolysaccharidosis, MPS-IV-A",
            "Galactosylceramide beta-galactosidase deficiency",
            "GM2 gangliosidosis",
            "Niemann-Pick disease, type C",
            "Gangliosidosis",
            "Sphingomyelin/cholesterol lipidosis",
            "Niemann-Pick disease, type B",
            "Mucopolysaccharidosis, MPS-I-H/S",
            "Danon disease",
            "Cerebral lipidosis",
            "Sandhoff disease",
            "Mucopolysaccharidosis, MPS-I-S",
            "Other"
                )
            )
            )

    rownames(df) <- df$person_id
    df$person_id <- NULL
    df$lsd_condition_name <- NULL

    tbl1 <- df %>%
        tbl_summary(by=has_lsd)

    tbl1_df <- as_tibble(tbl1, col_labels = FALSE) %>%
       rename(no_lsd = stat_1, has_lsd = stat_2)

    col_labels1 <- names(as_tibble(tbl1, col_labels = TRUE))
    
    tbl1_df <- tibble::as_tibble(
        rbind(
        col_labels1,
        as.matrix(tbl1_df)
        )
    )

    tbl2 <- df %>%
        dplyr::select(Age, COVID_pos_indicator, lsd_class,
            days_observed,
            number_of_visits, BMI_overall,
            #Sex, # try to include? You can infer unknown from total % because it is mutually exclusive not like the elix comorbidities
            #   chf,
            # carit,
            # valv,
            # pcd,
            # pvd,
            # hypunc,
            # hypc,
            # para,
            # ond,
            # cpd,
            # diabunc,
            # diabc,
            # hypothy,
            # rf,
            # ld,
            # pud,
            # aids,
            # lymph,
            # metacanc,
            # solidtum,
            # rheumd,
            # coag,
            # obes,
            # wloss,
            # fed,
            # blane,
            # dane,
            # alcohol,
            # drug,
            # psycho,
            # depre,
            elix_score) %>%
        tbl_summary(by=lsd_class)
    
    col_labels2 <- names(as_tibble(tbl2, col_labels = TRUE))

     n_totals <- stringr::str_extract(gsub(",", "", gsub("\\s*=\\s*", "=", col_labels2)), "(?<=N=)\\d+") %>% na.omit() %>% as.numeric()

    tbl2 <- tbl2 %>%
    modify_table_body(
        ~ {
        .x %>%
            mutate(across(
            starts_with("stat_"),
            ~ {
                col_idx <- as.numeric(sub("stat_", "", cur_column()))
                n_total <- n_totals[col_idx]
                
                is_match <- grepl("^([0-9,]+) \\([0-9.]+%\\)$", .)
                cell_val <- as.numeric(gsub(",", "", sub(" .*", "", .)))

                fmt_pct <- function(x) {
                    paste0(signif(100 * x / n_total, 2), "%")
                }
                
                ifelse(
                !is_match, .,
                ifelse(
                    cell_val < 20,  paste0("<20 (<", fmt_pct(20), ")"),
                    ifelse(
                    (n_total - cell_val) < 20, paste0(">", n_total - 20, " (>", fmt_pct(n_total - 20), ")"), .
                    )
                )
                )
            }
            )) %>%
            filter(!(variable == "elix_score" & label == "Unknown"))
        }
    )

    tbl2_df <- as_tibble(tbl2, col_labels = FALSE) %>%
        rename(label1 = label)
    
    tbl2_df <- tibble::as_tibble(
        rbind(
        col_labels2,
        as.matrix(tbl2_df)
        )
    )

    final_df <- cbind_na(tbl1_df, tbl2_df)

    return(final_df)
    
    # stats <- df %>%
    #     group_by(lsd_condition_name) %>%
    #     summarise(
    #         n = n(),
    #         age = median(Age, na.rm = TRUE),
    #         q1 = quantile(Age, 0.25, na.rm = TRUE),
    #         q3 = quantile(Age, 0.75, na.rm = TRUE)
    #     )

   # return(stats)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2be8aaea-6435-48ce-890d-d4441c292af7"),
    imputed=Input(rid="ri.foundry.main.dataset.3f98f6de-4736-464a-9bff-d7d38fa822aa")
)
library(MatchIt)
library(dplyr)
library(cobalt)
library(ggplot2)
cohort2 <- function(imputed) {
    set.seed(128) # very important, prev 128
    
    df1 <- imputed

    df2 <- df1 %>%
        dplyr::mutate( # Convert columns to data types suitable for cohort matching
            Race = factor(Race, levels = c("White", "Black or African American", "Asian", 
                                          "American Indian or Alaska Native", 
                                          "Native Hawaiian or Other Pacific Islander", 
                                          "Other/Unknown", "Hispanic or Latino")),
            Age = as.numeric(Age),
            data_partner_id = as.factor(data_partner_id),
            has_lsd = factor(has_lsd, levels = c(0, 1)),
            Sex = as.factor(Sex),
            log_obv = log1p(observation_period_before_covid)
        ) %>% 
        dplyr::select(person_id, Race, Age, Sex, data_partner_id, has_lsd, log_obv)

    # ## Exact match on data partner, closest on other covariates
   matched <- matchit(has_lsd ~ Age + Race + Sex + log_obv, method = "nearest", distance = "mahalanobis", ratio = 2, data = df2, exact = c("data_partner_id")) # try ratio = 2
   print(summary(matched)) 

    # Love plot for all variables
    l <- love.plot(matched, stats = "m",
        abs = FALSE, shapes = 15, size = 5, sample.names = c("Unmatched", "Matched"), 
        thresholds = c(m = .05),
        colors = c("#A463F2FF", "#3CA951FF")) +
        labs(title = "") + 
        scale_y_discrete(labels = c("Race_Native Hawaiian or Other Pacific Islander" = "Race_Native Hawaiian\nor Other Pacific Islander", 
            "Race_American Indian or Alaska Native" = "Race_American Indian\nor Alaska Native", "Race_Black or African American" = "Race_Black\nor African American", "log_obv" = "ln(# days observed)"
            ),
                    limits = c("Age", "Race_White", "Race_Black or African American", "Race_Asian", 
                              "Race_American Indian or Alaska Native", 
                              "Race_Native Hawaiian or Other Pacific Islander", 
                               "Race_Other/Unknown", "Sex_Male", "Sex_Female", "Sex_Unknown", "log_obv")) +
        guides(color = guide_legend(override.aes = list(size = 5))) +
        theme_classic(base_size = 16) +
        theme(aspect.ratio = 3/2)
    plot(l)

    # Balance plots for select variables
    features <- c("Age", "Sex")
    for (f in features) {
        k <- bal.plot(matched, var.name = f, which = "both", sample.names = c("Unmatched", "Matched"), disp.means = TRUE) +
            labs(title = "") +
            scale_fill_manual(
                name = "Group",
                values = c(`0` = "#F8766D", `1` = "#00BFC4"),
                labels = c(`0` = "Unaffected", `1` = "Affected")
            ) +
            theme_classic(base_size = 16)
        
        plot(k)
    }

    # Race balance plot (fine because Race has limited levels)
    r <- bal.plot(matched, var.name = "Race", which = "both", sample.names = c("Unmatched", "Matched"), disp.means = TRUE) +
        labs(title = "") +
        scale_fill_manual(
                name = "Group",
                values = c(`0` = "#F8766D", `1` = "#00BFC4"),
                labels = c(`0` = "Unaffected", `1` = "Affected") # Unaffected Affected
            ) +
        scale_x_discrete(
                    limits = c("White", "Black or African American", "Asian", 
                              "American Indian or Alaska Native", 
                               "Other/Unknown")) + 
        theme_classic(base_size = 16) +
        theme(axis.text.x = element_text(hjust = 1.0, vjust = 1.0, angle = 45))

    plot(r)

    matched_data <- match.data(matched, data = df2) # Extract all data from matched dataset 

    # assign weights
    n_case <- sum(matched_data$has_lsd == 1)
    n_control <- sum(matched_data$has_lsd == 0)

    matched_data$weights <- ifelse(matched_data$has_lsd == 1, 1, n_case / n_control)

    extra_cols <- df1 %>% dplyr::select(
        person_id,
        Severity_Type,
        COVID_first_poslab_or_diagnosis_date,
        observation_period_before_covid,
        observation_period_post_covid,
        number_of_visits_before_covid,
        number_of_visits_post_covid,
        BMI_before_or_day_of_covid,
        had_reinfection_post_covid,
        COVID_vaccine_doses_before_or_day_of_covid,
        postal_code,
        Long_COVID_any_indicator,
        lsd_condition_name,
        Binary_Severity
    )
    
    cohort <- dplyr::inner_join(extra_cols, matched_data, by = "person_id")

    return(cohort)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.08f2bdf8-c72c-4267-af87-b2658a778c60"),
    cohort_conditions_icd_2_1=Input(rid="ri.foundry.main.dataset.841ff505-990e-49a8-b5f6-27dbcfaac01f")
)
library(comorbidity)
library(dplyr)
cohort_elix_2_1 <- function(cohort_conditions_icd_2_1) {

    # conditions anytime
    anytime <- cohort_conditions_icd_2_1
    lsd_stat <- anytime %>% select(person_id, has_lsd, number_of_visits_before_covid, observation_period_before_covid, Binary_Severity, weights) %>% 
        distinct() %>%
        mutate(
            visits_per_year = (number_of_visits_before_covid / observation_period_before_covid) * 365,
            p25 = quantile(visits_per_year, 0.25, na.rm = TRUE),
            p75 = quantile(visits_per_year, 0.75, na.rm = TRUE),
            utilization = cut(
                visits_per_year,
                breaks = c(-Inf, p25[1], p75[1], Inf),
                labels = c("util_low", "util_moderate", "util_high"),
                include.lowest = TRUE
            )
        ) %>%
        select(-p25, -p75)

    print(quantile(lsd_stat$visits_per_year, probs = c(0.25, 0.75)))

    # map ICD codes to comorbidities 
    elix <- comorbidity(x = anytime, id = "person_id", code = "concept_code", map = "elixhauser_icd10_quan", assign0 = TRUE)

    elix_score <- score(x = elix, weights = "swiss", assign0 = TRUE) # swiss or wv. wv was in canada 2009 345k patients, swiss is switzerland 2012-2017 6 million patients

    # add score and has_lsd in
    elix$elix_score <- elix_score
    elix <- elix %>% inner_join(lsd_stat, by = "person_id") %>%
        select(-number_of_visits_before_covid, -visits_per_year, -observation_period_before_covid)

    stats <- elix %>%
        group_by(has_lsd) %>%
        summarise(
            median = median(elix_score, na.rm = TRUE),
            q1 = quantile(elix_score, 0.25, na.rm = TRUE),
            q3 = quantile(elix_score, 0.75, na.rm = TRUE)
        )

    w <- wilcox.test(elix_score ~ has_lsd, data = elix)
    print(w)    
    print(stats)

    return(elix)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.23b657b2-6578-4fb5-983f-bc5668ce7a38"),
    survival_prep=Input(rid="ri.foundry.main.dataset.cb16e05f-0750-4afe-afe6-f62bba4a0a90")
)
library(broom)
library(ggsurvfit)
library(survival)
library(dplyr)
library(ggplot2) # requires 3.5.0, base R > 4.3
library(patchwork)
cohort_survival <- function(survival_prep) {
    
    surv <- survival_prep %>%
        dplyr::select(person_id, days, status, Severity, has_lsd) %>%
        dplyr::mutate(
            Severity = as.factor(Severity),
            days     = as.numeric(days),
            status   = as.numeric(status),
            has_lsd  = dplyr::case_when(
                has_lsd == 0 ~ "Control",
                has_lsd == 1 ~ "LD"
            )
        ) %>%
        dplyr::distinct()

    message("Total unique patients: ", dplyr::n_distinct(surv$person_id))
    message("Control: ", sum(surv$has_lsd == "Control", na.rm = TRUE),
            " | LD: ", sum(surv$has_lsd == "LD", na.rm = TRUE))

    # time points for summaries / tidy_survfit
    times <- c(0, 15, 30, 60, 90, 180, 200, 365, 400, 600, 730, 800, 1000, 1095, 1200, 1400, 1460, 1600, 1800, 1825)

    run_block <- function(fit, res, label_plot, model_label, times,
                        color_values = NULL) {
    
        # KM summary at chosen times (printed for inspection)
        s <- summary(fit, times = times)
        cat(label_plot, "\n")
        print(s)
        cat("\nLog-rank:\n")
        print(res)
        cat("\n")
        
        # use res$pvalue ONLY
        pval <- res$pvalue
        pval_cap <- if (is.null(pval) || is.na(pval)) {
        "Log-rank p=NA"
        } else if (pval < 0.001) {
        "Log-rank p<0.001"
        } else {
        sprintf("Log-rank p=%.3f", pval)
        }
        
        p <- fit %>%
        ggsurvfit(linewidth = 0.5) +  # thinner lines for small base size
        add_confidence_interval() +
        add_censor_mark(size = 0.05) +
        coord_cartesian(xlim = c(0, 1825)) +
        scale_ggsurvfit() +
        labs(
            title   = label_plot,
            x       = "Time (days)",
            y       = "Survival Probability",
            caption = pval_cap
        ) +
        theme_classic(base_size = 6) +
        theme(
            aspect.ratio         = 0.67,
            legend.position      = "bottom",
            legend.key.size      = unit(6, "pt"),
            plot.title           = element_text(hjust = 0.5)
        )
        
        # custom colors if provided
        if (!is.null(color_values)) {
        p <- p + scale_color_manual(values = color_values)
        }
        
        # tidy table at these times
        df_tidy <- tidy_survfit(fit, times = times) %>%
        dplyr::mutate(model = model_label)
        
        list(df = df_tidy, plot = p)
  }
  
    # container for internal results
    res <- list()
    
    # -----------------------------
    # 1) Overall LD vs Control
    # -----------------------------
    fit_overall <- survfit2(Surv(days, status) ~ has_lsd, data = surv)
    res_overall <- survdiff(Surv(days, status) ~ has_lsd, data = surv)
    
    overall_res <- run_block(
        fit         = fit_overall,
        res         = res_overall,
        label_plot  = "Cohort Survival: LD vs Control",
        model_label = "Overall: LD vs Control",
        times       = times
    )
    
    final_df <- overall_res$df
    res$overall <- overall_res
    
    # -----------------------------
    # 2) By Severity (LD vs Control within each Severity)
    # -----------------------------
    sev_results <- list()
    
    for (type in unique(surv$Severity)) {
        fit_sev <- survfit2(
        Surv(days, status) ~ has_lsd,
        data   = surv,
        subset = Severity == type
        )
        res_sev <- survdiff(
        Surv(days, status) ~ has_lsd,
        data   = surv,
        subset = Severity == type
        )
        
        sev_res <- run_block(
        fit         = fit_sev,
        res         = res_sev,
        label_plot  = paste0("Severity = ", type, " (LD vs Control)"),
        model_label = paste0("LD vs Control, Severity = ", type),
        times       = times
        )
        
        sev_results[[as.character(type)]] <- sev_res
        final_df <- dplyr::bind_rows(final_df, sev_res$df)
    }
    
    res$by_severity <- sev_results
    
    # -----------------------------
    # 3) LD: Hospitalized vs Not Hospitalized
    # -----------------------------
    ld_sub <- surv %>%
        dplyr::filter(
        has_lsd == "LD",
        Severity %in% c("Hospitalized", "Not Hospitalized")
        )
    
    if (nrow(ld_sub) > 0 && dplyr::n_distinct(ld_sub$Severity) > 1) {
        fit_ld <- survfit2(Surv(days, status) ~ Severity, data = ld_sub)
        res_ld <- survdiff(Surv(days, status) ~ Severity, data = ld_sub)
        
        ld_res <- run_block(
        fit          = fit_ld,
        res          = res_ld,
        label_plot   = "LD: Hospitalized vs Not Hospitalized",
        model_label  = "LD: Hospitalized vs Not Hospitalized",
        times        = times,
        color_values = c(
            "Not Hospitalized" = "#2d8cff",
            "Hospitalized"     = "#f26d21"
        )
        )
        
        res$ld_hosp_vs_not <- ld_res
        final_df <- dplyr::bind_rows(final_df, ld_res$df)
    } else {
        message("Skipping LD hospitalized vs Not Hospitalized (insufficient LD data).")
        res$ld_hosp_vs_not <- NULL
    }
    
    # -----------------------------
    # 4) Control: Hospitalized vs Not Hospitalized
    # -----------------------------
    ctrl_sub <- surv %>%
        dplyr::filter(
        has_lsd == "Control",
        Severity %in% c("Hospitalized", "Not Hospitalized")
        )
    
    if (nrow(ctrl_sub) > 0 && dplyr::n_distinct(ctrl_sub$Severity) > 1) {
        fit_ctrl <- survfit2(Surv(days, status) ~ Severity, data = ctrl_sub)
        res_ctrl <- survdiff(Surv(days, status) ~ Severity, data = ctrl_sub)
        
        ctrl_res <- run_block(
        fit          = fit_ctrl,
        res          = res_ctrl,
        label_plot   = "Control: Hospitalized vs Not Hospitalized",
        model_label  = "Control: Hospitalized vs Not Hospitalized",
        times        = times,
        color_values = c(
            "Not Hospitalized" = "#2d8cff",
            "Hospitalized"     = "#f26d21"
        )
        )
        
        res$ctrl_hosp_vs_not <- ctrl_res
        final_df <- dplyr::bind_rows(final_df, ctrl_res$df)
    } else {
        message("Skipping Control hospitalized vs Not Hospitalized (insufficient Control data).")
        res$ctrl_hosp_vs_not <- NULL
    }
    
    # -----------------------------
    # Mask small counts in final_df
    # -----------------------------
    count_cols   <- c("n.risk", "n.event", "n.censor", "cum.event", "cum.censor")
    cols_to_mask <- intersect(count_cols, names(final_df))
    
    if (length(cols_to_mask) > 0) {
        final_df <- final_df %>%
        dplyr::mutate(
            dplyr::across(
            dplyr::all_of(cols_to_mask),
            ~ dplyr::if_else(!is.na(.x) & .x < 20,
                            "<20",
                            as.character(.x))
            )
        )
    }
  
    # store final_df inside res
    res$final_df <- final_df

    p_sev_not_hosp <- res$by_severity[["Not Hospitalized"]]$plot
    p_sev_hosp <- res$by_severity[["Hospitalized"]]$plot   
    p_ld <- res$ld_hosp_vs_not$plot                    
    p_ctrl <- res$ctrl_hosp_vs_not$plot      

    patch <- (p_sev_hosp | p_sev_not_hosp) /
            (p_ld | p_ctrl)

    patch <- patch +
      plot_annotation(tag_levels = "a") &
      theme(
        plot.tag    = element_text(face = "bold", size = 12),
        plot.margin = margin(0, 0, 0, 0)
      )

    # png(graphicsFile, width = 170, height = 127, units = "mm", res = 600)
    # print(patch)

    # image: svg
    svg(graphicsFile, width=6.69, height=5, bg="transparent")
    print(patch)

    return(res$final_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7f229f7a-265d-4b6c-9283-3e9147ff0eba"),
    cohort2=Input(rid="ri.foundry.main.dataset.2be8aaea-6435-48ce-890d-d4441c292af7"),
    cohort_conditions_icd_2_1=Input(rid="ri.foundry.main.dataset.841ff505-990e-49a8-b5f6-27dbcfaac01f")
)
library(survival)
library(dplyr)
library(purrr)
library(tibble)
library(comorbidity)
library(broom)
cov_outcomes_clogit <- function(cohort2, cohort_conditions_icd_2_1) {
  
    co <- cohort2
    
    pre_covid <- cohort_conditions_icd_2_1 %>%
        filter(days_diff <= 0)
    
    elix <- comorbidity(
        x       = pre_covid,
        id      = "person_id",
        code    = "concept_code",
        map     = "elixhauser_icd10_quan",
        assign0 = TRUE
    ) # don't double count assign0 = TRUE
    
    input_df <- co %>%
        left_join(elix, by = "person_id") %>%
        mutate(
        across(
            -c(person_id, postal_code),
            ~ replace_na(., 0) # replace empty comorbidities with 0
        ),
        has_lsd         = as.numeric(has_lsd),
        Binary_Severity = as.numeric(Binary_Severity)
        )
    
    concept_list <- list(
        sphingo = sphingo,
        lipid   = lipid,
        fabry   = fabry,
        gaucher = gaucher,
        metak   = metak,
        amino   = amino,
        gang    = gang,
        cys     = cys,
        ncl     = ncl
    )
    # sphingo: carit, hypunc, para, ond, diabunc, diabc, ld, rheumd, wloss, fed, alcohol
    # lipid:  carit, para, ond, diabc, ld, rheumd, wloss, dane, alcohol
    # fabry:  valv, rf
    # gaucher: coag
    # metak:  para, ond
    # gang:   ond
    # ncl:    ond
    # amino, cys: no mediators

    mediators_map <- list(
    sphingo = c("carit", "hypunc", "para", "ond", "diabunc", "diabc", "ld", "rheumd", "wloss", "fed", "alcohol"),
    lipid   = c("carit", "para", "ond", "diabc", "ld", "rheumd", "wloss", "dane", "alcohol"),
    fabry   = c("valv", "rf"),
    gaucher = c("coag"),
    metak   = c("para", "ond"),
    gang    = c("ond"),
    ncl     = c("ond")
    # amino, cys intentionally omitted → mediators_map[[ "amino" ]] and [[ "cys" ]] will be NULL
    )
    
    df_list <- purrr::imap(concept_list, function(cond_vec, nm) {
        split_df(input_df, cond_vec) %>%
        dplyr::mutate(
            !!rlang::sym(nm) := ifelse(lsd_condition_name %in% cond_vec, 1, 0)
        )
    })
    
    clogit_results <- purrr::imap_dfr(df_list, function(dat, nm) {
  
        mediators <- mediators_map[[nm]]  # will be NULL for amino, cys, etc.
        exposure  <- nm
        
        ## --- Total effect (no mediators) ---
        f_total <- as.formula(
            paste0("Binary_Severity ~ ", exposure, " + strata(subclass)")
        )
        fit_total <- clogit(f_total, data = dat)
        tidy_total <- tidy(fit_total, exponentiate = TRUE, conf.int = TRUE) %>%
            dplyr::mutate(ld_name = nm, model_type = "total")
        
        # if no mediators for this subtype, just return the total-effect row
        if (is.null(mediators) || length(mediators) == 0) {
            return(tidy_total)
        }
        
        ## --- Direct effect (adjust for mediators) ---
        f_direct <- as.formula(
            paste0(
            "Binary_Severity ~ ", exposure, " + ",
            paste(mediators, collapse = " + "),
            " + strata(subclass)"
            )
        )
        fit_direct <- clogit(f_direct, data = dat)
        tidy_direct <- tidy(fit_direct, exponentiate = TRUE, conf.int = TRUE) %>%
            dplyr::mutate(ld_name = nm, model_type = "direct")
        
        ## Combine total + direct for subtypes with mediators
        dplyr::bind_rows(tidy_total, tidy_direct)
        })
    
    fit_lsd <- clogit(Binary_Severity ~ has_lsd + strata(subclass), data = co)
    tidy_lsd <- tidy(fit_lsd, exponentiate = TRUE, conf.int = TRUE) %>%
        mutate(ld_name = "has_lsd", model_type = "total")
    
    final_results <- bind_rows(clogit_results, tidy_lsd)
    
    return(final_results)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.9d1b4abf-e600-41db-939e-a5f0b19d8673"),
    all_cohort1=Input(rid="ri.foundry.main.dataset.ca3e686b-a2cd-4f80-8494-fc7458661c3e")
)
library(dplyr)
library(purrr)
covid_events_summary <- function(all_cohort1) {
    co <- all_cohort1
    
    concept_list <- list(
        sphingo   = sphingo,
        lipid     = lipid,
        fabry     = fabry,
        gaucher   = gaucher,
        metak     = metak,
        amino     = amino,
        gang      = gang,
        cys       = cys,
        ncl       = ncl,
        glycopro  = glycopro #,
        #other     = other
    )

    # apply split_df(co, ...) to each vector
    df_list <- imap(concept_list, ~ split_df(co, .x))

    # covid counts
    all_counts <- purrr::map2_dfr(df_list, names(df_list),
                              ~ count_events(.x, .y, "COVID_pos_indicator"))

    return(all_counts)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.69318eb8-0656-41b8-a1dc-837e077f33c4"),
    cohort_death=Input(rid="ri.foundry.main.dataset.6662c2bb-0b44-4f01-9285-e8be46bf9a91")
)
library(ggplot2)
library(dplyr)
library(rstatix)
library(ggpubr)
death_cov_severity <- function(cohort_death) {
    
    df <- cohort_death %>%
        mutate(
        has_lsd = case_when(
            has_lsd == 0 ~ "Control",
            has_lsd == 1 ~ "LD"
        ),
        has_lsd = factor(has_lsd, levels=c("LD", "Control")),
        Binary_Severity = factor(Binary_Severity, levels = c(1,0))
        )

    n_deaths <- df %>%
        group_by(has_lsd) %>%
        summarise(
            n_deaths = n_distinct(person_id)
        ) %>%
        print()

    stats <- df %>% group_by(has_lsd) %>%
        wilcox_test(days_diff ~ Binary_Severity) %>%
        adjust_pvalue(method = "holm") %>% 
        add_significance() %>%
        print()

    stats2 <- df %>% group_by(Binary_Severity) %>%
        wilcox_test(days_diff ~ has_lsd) %>%
        adjust_pvalue(method = "holm") %>% 
        add_significance() %>%
        print()

    stats3 <- df %>% group_by(has_lsd, Binary_Severity) %>%
        summarise(
            med = median(days_diff, na.rm = TRUE)
        ) %>%
        print()

    dens_df <- df %>%
        group_by(has_lsd, Binary_Severity) %>%
        do({
            d <- density(.$days_diff)
            data.frame(x = d$x, y = d$y)
        })

    # Join median with its corresponding density height
    med_fac <- df %>%
        group_by(has_lsd, Binary_Severity) %>%
        summarise(med = median(days_diff, na.rm = TRUE), .groups = "drop") %>%
        left_join(
            dens_df %>%
            group_by(has_lsd, Binary_Severity) %>%
            summarise(across(c(x, y), list), .groups = "drop"),
            by = c("has_lsd", "Binary_Severity")
        ) %>%
        rowwise() %>%
        mutate(
            yend = {
            y[which.min(abs(x - med))]
            }
        )

    p <- ggplot(data = df) + 
        geom_density(alpha = 0.5, linewidth = 0.25, aes(x = days_diff, color = Binary_Severity, fill = Binary_Severity)) + 
        geom_segment(
            data = med_fac,
            aes(x = med, xend = med, y = 0, yend = yend, color = Binary_Severity),
            linewidth = 0.25
        ) +
        labs(x = "Time (days)", y = "Density", fill="", color="") + 
        scale_fill_manual(values = c("0" = "#2d8cff", "1" = "#f26d21"), 
            labels = c("0" = "Not Hospitalized", "1" = "Hospitalized")
        ) +  
        scale_color_manual(values = c("0" = "#2d8cff", "1" = "#f26d21"), 
            labels = c("0" = "Not Hospitalized", "1" = "Hospitalized")
        ) +  
        xlim(0, 1460) + 
        facet_wrap(~has_lsd) + 
        theme_grey(base_size = 6) +
        theme(legend.position = "bottom", legend.key.size = unit(6, "pt"), plot.margin = margin(0,5,0,0)) +
        guides(
    color = guide_legend(override.aes = list(linewidth = 0.25)),
    fill  = guide_legend(override.aes = list(alpha = 0.5))
  )
    
    # png(graphicsFile, width = 85, height = 64, units = "mm", res = 600)   
    # print(p)

    # image: svg
    svg(graphicsFile, width=3.34, height=2.52, bg="transparent")
    print(p)
    return(stats)

}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.bdfc992b-6bdf-4966-9d7b-e0f138f9a4ad"),
    drug_exposure_survival_prep=Input(rid="ri.foundry.main.dataset.4a6a24f9-9d89-4293-bede-4e0ea1511a80")
)
library(ggsurvfit) # Survival
library(survival) # survdiff & coxph
library(dplyr) # Data wrangling
library(ggplot2) # Plotting.  # requires ggplot 3.5.0 and R base 4.3.3
library(broom) # dataframe from Surv object
drug_exposure_survival <- function(drug_exposure_survival_prep) {
    
    df1 <- drug_exposure_survival_prep
    
    counts <- df1 %>%
        distinct(person_id, has_lsd) %>%
        summarize(
            LSD = sum(has_lsd == 1),
            Control = sum(has_lsd == 0)
        )

    message(
    "Total survival cohort: ", counts$LSD + counts$Control,
    " (LSD: ", counts$LSD, 
    ", Controls: ", counts$Control, ")"
    )

    surv <- df1 %>%
        dplyr::select(
        person_id, days, status,
        drug_name, has_lsd, subclass, elix_score, Binary_Severity
        ) %>%
        dplyr::mutate(
        drug_name  = factor(drug_name),
        has_lsd    = factor(ifelse(has_lsd == 1, "LD", "Control"),
                            levels = c("Control", "LD")),
        subclass   = factor(subclass),
        Binary_Severity = factor(Binary_Severity),
        elix_score = ifelse(is.na(elix_score), 0, elix_score),
        days       = as.numeric(days),
        status     = as.numeric(status)
        ) %>%
        dplyr::distinct()

    all_drugs <- setdiff(levels(surv$drug_name), "none")

    results_list <- list()

    for (drug in all_drugs) {

        message("\n=== Fitting survival model for drug: ", drug, " ===")

        # Use this drug vs. no-drug ("none")
        dat <- surv %>%
            mutate(drug_exposed = ifelse(drug_name == drug, 1, 0)) %>%
            filter(drug_name %in% c("none", drug))

        # Cox PH model, adjusted for Elix, stratified by subclass
        fit <- coxph(
            Surv(days, status) ~
                drug_exposed * has_lsd +
                elix_score +
                strata(subclass),
                data = dat
        )

        cat("Testing PH assumption:\n")
        print(cox.zph(fit))

        # Tidy HR table for this drug
        hr_tbl <- tidy(fit, exponentiate = TRUE, conf.int = TRUE)
        hr_tbl$drug <- drug
        results_list[[drug]] <- hr_tbl

    }

    final_results <- bind_rows(results_list)

    return(final_results)

}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.4a6a24f9-9d89-4293-bede-4e0ea1511a80"),
    cohort2=Input(rid="ri.foundry.main.dataset.2be8aaea-6435-48ce-890d-d4441c292af7"),
    cohort_conditions_icd_2_1=Input(rid="ri.foundry.main.dataset.841ff505-990e-49a8-b5f6-27dbcfaac01f"),
    cohort_death=Input(rid="ri.foundry.main.dataset.6662c2bb-0b44-4f01-9285-e8be46bf9a91"),
    cohort_drug_exposure=Input(rid="ri.foundry.main.dataset.a079bee5-0a15-4160-8032-55854022e928"),
    drugs_of_interest=Input(rid="ri.foundry.main.dataset.a18947cf-261d-4233-82db-5fc578d65258")
)
library(dplyr)
library(comorbidity)
drug_exposure_survival_prep <- function(cohort_conditions_icd_2_1, cohort_drug_exposure, cohort2, cohort_death, drugs_of_interest) {

    pre_covid <- cohort_conditions_icd_2_1 %>% # conditions before COVID
        filter(days_diff <= 0) 
    
    elix <- comorbidity(x = pre_covid, id = "person_id", code = "concept_code", map = "elixhauser_icd10_quan", assign0 = TRUE) # don't double count assign0 = TRUE

    elix_score <- score(x = elix, weights = "swiss", assign0 = TRUE)
    elix$elix_score <- elix_score

# SELECT dc.*,
#     doi.drug_name
# FROM drug_cohort dc
# JOIN drugs_of_interest doi ON dc.drug_concept_id = doi.concept_id

    latest_drug <- cohort_drug_exposure %>%
        inner_join(drugs_of_interest, by=join_by(drug_concept_id == concept_id)) %>%
        select(person_id, drug_concept_id, drug_concept_name, drug_name, days_diff, drug_exposure_start_date, drug_exposure_end_date) %>%
        filter(between(days_diff, 1, 15)) %>%
        group_by(person_id, drug_name) %>%
        slice_min(order_by=drug_exposure_start_date, n=1, with_ties = FALSE) %>%
        ungroup()

    w_cohort <- cohort2 %>%
        select(person_id, subclass, has_lsd, Severity_Type, Binary_Severity, COVID_first_poslab_or_diagnosis_date) %>%
        left_join(latest_drug, by=join_by(person_id)) %>% 
        left_join(elix %>% select(person_id, elix_score), by=join_by(person_id)) %>%
        mutate(
            drug_name = coalesce(drug_name, "none"),
            drug_before_landmark = if_else(is.na(drug_concept_id), 0, 1),
            landmark_date = COVID_first_poslab_or_diagnosis_date + 15
        )

    censor_date <- as.Date("2025-07-11")

    survival_df <- w_cohort %>%
        left_join(cohort_death %>% select(person_id, death_date), by=join_by(person_id)) %>%
        mutate(
            days = if_else(
                !is.na(death_date),
                as.integer(death_date - landmark_date),
                as.integer(censor_date - landmark_date)
            )
        ) %>%
        filter(days >= 0) %>% # death must be on/after landmark to avoid immortal time bias
        mutate(status = if_else(!is.na(death_date), 1, 0))

    
    return(survival_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0f7d3703-96a2-4de7-9b77-5226bfd7810c"),
    cohort_elix_2_1=Input(rid="ri.foundry.main.dataset.08f2bdf8-c72c-4267-af87-b2658a778c60")
)
library(dplyr)
library(FactoMineR)
library(ggplot2)
library(factoextra) # facto viz
library(ggpubr) # ggviolin and ggarrange
library(RColorBrewer)
library(gtools)
library(patchwork)
library(ggrepel)
elix_mca <- function(cohort_elix_2_1) {
   elix_base <- cohort_elix_2_1 %>%
        mutate(
            utilization = case_when(
                utilization == "util_low" ~ "utilization_low",
                utilization == "util_moderate" ~ "utilization_moderate",
                utilization == "util_high" ~ "utilization_high"
            ),
            lsd = has_lsd,
            Severity = Binary_Severity,
            control_not_hosp = as.integer(lsd == 0 & Severity == 0),
            control_hosp     = as.integer(lsd == 0 & Severity == 1),
            ld_not_hosp      = as.integer(lsd == 1 & Severity == 0),
            ld_hosp          = as.integer(lsd == 1 & Severity == 1)
        )
    
    elix_num <- elix_base
        
    comorb_cols <- c(
    "chf","carit","valv","pcd","pvd","hypunc","hypc","para","ond","cpd",
    "diabunc","diabc","hypothy","rf","ld","pud","aids","lymph","metacanc",
    "solidtum","rheumd","coag","obes","wloss","fed","blane","dane",
    "alcohol","drug","psycho","depre"
    )

    comorbidity_map <- c(
    chf      = "Congestive heart failure",
    carit    = "Cardiac arrhythmias",
    valv     = "Valvular disease",
    pcd      = "Pulmonary circulation disorders",
    pvd      = "Peripheral vascular disorders",
    hypunc   = "Hypertension, uncomplicated",
    hypc     = "Hypertension, complicated",
    para     = "Paralysis",
    ond      = "Other neurological disorders",
    cpd      = "Chronic pulmonary disease",
    diabunc  = "Diabetes, uncomplicated",
    diabc    = "Diabetes, complicated",
    hypothy  = "Hypothyroidism",
    rf       = "Renal failure",
    ld       = "Liver disease",
    pud      = "Peptic ulcer disease, excluding bleeding",
    aids     = "AIDS/HIV",
    lymph    = "Lymphoma",
    metacanc = "Metastatic cancer",
    solidtum = "Solid tumor, without metastasis",
    rheumd   = "Rheumatoid arthritis/\ncollagen vascular disease",
    coag     = "Coagulopathy",
    obes     = "Obesity",
    wloss    = "Weight loss",
    fed      = "Fluid and electrolyte disorders",
    blane    = "Blood loss anemia",
    dane     = "Deficiency anemia",
    alcohol  = "Alcohol abuse",
    drug     = "Drug abuse",
    psycho   = "Psychoses",
    depre    = "Depression"
    )

    outcomes <- c("lsd", "Severity", "control_not_hosp", "control_hosp", "ld_not_hosp", "ld_hosp")

    corr_results <- map_dfr(outcomes, function(outcome) {
        map_dfr(comorb_cols, function(col) {
            res <- cor.test(
            as.numeric(elix_num[[outcome]]),
            as.numeric(elix_num[[col]]),
            method = "pearson",
            use = "complete.obs"
            )
            tibble(
            outcome     = outcome,
            variable    = col,
            correlation = unname(res$estimate),
            statistic   = unname(res$statistic),
            df          = unname(res$parameter),
            p_value     = res$p.value,
            ci_lower    = res$conf.int[1],
            ci_upper    = res$conf.int[2],
            method      = res$method
            )
        })
    })

    nice_map <- comorbidity_map[comorb_cols] # named by codes 
    rename_map <- stats::setNames(comorb_cols, unname(nice_map))

    corr_df <- corr_results %>%
        group_by(outcome) %>%
        mutate(p_adj = p.adjust(p_value, method = "holm")) %>%
        ungroup() %>%
        mutate(variable = dplyr::recode(variable, !!!comorbidity_map)) %>%
        arrange(outcome, desc(abs(correlation)))

    # remove unnecessary vars
    elix_mca <- elix_base %>%
        mutate(
        lsd = ifelse(lsd == 1, "LD", "Control"),    
        Severity = ifelse(Severity == 1, "Hospitalized", "Not Hospitalized"),
        ld_by_Severity = interaction(lsd, Severity, sep = "_", drop = TRUE)) %>% 
        select(-elix_score, -Binary_Severity, -weights, -has_lsd,
         -control_not_hosp, -control_hosp, -ld_not_hosp, -ld_hosp) %>%
        rename(!!!rename_map)

    # Row weights MUST be a separate numeric vector
    weights <- elix_base$weights

    # Person id
    rownames(elix_mca) <- elix_base$person_id
    elix_mca$person_id <- NULL

    # Recode ONLY the renamed comorbidity columns to factor(No, Yes)
    supp_vars <- c("lsd", "Severity", "ld_by_Severity")
    elix_mca <- elix_mca %>%
        mutate(
            across(
            .cols = all_of(unname(nice_map)),
            .fns  = ~ factor(recode(as.character(.), "0" = "No", "1" = "Yes"),
                            levels = c("No","Yes"))
            )
        )

    # Indices for supplementary variables
    sup_idx <- which(names(elix_mca) %in% supp_vars)

    cat("Input features:", setdiff(colnames(elix_mca), supp_vars), "\n")

    # run mca
    mca_res <- MCA(
        elix_mca,
        ncp       = ncol(elix_mca),
        quali.sup = sup_idx,
        row.w     = weights,
        graph     = FALSE
    )

    print(summary(mca_res))

    centroid_diff <- function(mca_res, group, k = 10) {
        #max(which(cumsum(mca_res$eig[, 2]) <= 70)) # select dims up to cum variance X% 
        coords_k <- mca_res$ind$coord[, 1:k]
        colnames(coords_k) <- make.names(colnames(coords_k))
        df <- data.frame(coords_k, group = factor(group))
        group_name <- deparse(substitute(group))
  
        # Print header with group name
        cat(paste("\n--- Centroid Difference Results for:", group_name, "---\n"))

        f <- as.formula(
            paste0("cbind(", paste(colnames(coords_k), collapse = ", "), ") ~ group")
        )

        m <- manova(f, data = df)

        # overall group separation 
        cat("\n--- Overall Group Separation (MANOVA, Pillai's Trace) ---\n")
        print(summary(m, test = "Pillai"))
        
        # --- 3. Per-Dimension ANOVAs ---
        cat("\n--- Per-Dimension ANOVA (which MCA dims separate groups) ---\n")
        print(summary.aov(m))
        
        # --- 4. Tukey Post-hoc Tests ---
        cat("\n--- Tukey Post-hoc Tests (Direction of Differences) ---\n")
        for (dim in colnames(coords_k)) {
            cat("\n###", dim, "###\n")
            tuk <- TukeyHSD(aov(df[[dim]] ~ df$group))
            print(tuk)
        }
    }

    lsd <- centroid_diff(mca_res, group = elix_base$lsd, k = 20)
    sev <- centroid_diff(mca_res, group = elix_base$Binary_Severity, k = 20)

    eig_df <- tibble(
        dimension = rownames(mca_res$eig),
        eigenvalue = mca_res$eig[, 1],
        percent_variance = mca_res$eig[, 2],
        cumulative_variance = mca_res$eig[, 3]
    )

    ind_df <- tibble(
        #person_id = rownames(mca_res$ind$coord),
        ind_dim1 = mca_res$ind$coord[, 1],
        ind_dim2 = mca_res$ind$coord[, 2],
        ind_contrib_dim1 = mca_res$ind$contrib[, 1],
        ind_contrib_dim2 = mca_res$ind$contrib[, 2],
        ind_cos2_dim1 = mca_res$ind$cos2[, 1],
        ind_cos2_dim2 = mca_res$ind$cos2[, 2],
        elix_score = elix_base$elix_score,
        lsd = elix_mca$lsd,
        Binary_Severity = elix_mca$Severity
    )

    var_df <- tibble(
        var = rownames(mca_res$var$coord),
        var_dim1 = mca_res$var$coord[, 1],
        var_dim2 = mca_res$var$coord[, 2],
        var_contrib_dim1 = mca_res$var$contrib[, 1],
        var_contrib_dim2 = mca_res$var$contrib[, 2],
        var_cos2_dim1 = mca_res$var$cos2[, 1],
        var_cos2_dim2 = mca_res$var$cos2[, 2]
    )

    eta2_df <- tibble(
        var_eta2 = rownames(mca_res$var$eta2),
        var_eta2_dim1 = mca_res$var$eta2[, 1],
        var_eta2_dim2 = mca_res$var$eta2[, 2]
    )
    
    grp <- elix_mca[, "lsd"] %>% as.factor()
    grp2 <- elix_mca[, "Severity"] %>% as.factor()
   
    j <- fviz_mca_ind(mca_res, label="none", pointsize = 0.5, habillage=grp, axes.linetype = "blank") +
        geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey30") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey30") +
        ylim(-1,1.35) +
        labs(color = "", title = "") + 
        theme_grey(base_size = 6) + 
          scale_fill_manual(values = c("Control" = "#F8766D", "LD" = "#00BFC4")
        ) +
        theme(plot.title = element_blank(),
         legend.position = c(0.12, 0.9),
          legend.title = element_blank(),
        legend.background = element_rect(fill="white",
                                  linewidth=0.25, linetype="solid", 
                                  color ="grey30"),
        legend.key.size   = unit(3, "mm"), 
        #axis.text = element_blank(), 
        #axis.ticks = element_blank(), 
        aspect.ratio = 1, 
        plot.margin = margin(0,0,0,0))
    

     i <- fviz_mca_ind(mca_res, label="none", pointsize = 0.5, habillage=grp2, axes.linetype = "blank") + 
           geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey30") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey30") +
         labs(color = "", title = "") + 
        theme_grey(base_size = 6) + 
       scale_color_manual(values = c("Not Hosp" = "#2d8cff", "Hosp" = "#f26d21")
        ) + 
     theme(plot.title = element_blank(),
         legend.position = c(0.12, 0.9),
         legend.title = element_blank(),
         legend.background = element_rect(fill="white",
                                  linewidth=0.25, linetype="solid", 
                                  color ="grey30"),
        legend.key.size   = unit(3, "mm"), 
        #axis.text = element_blank(), 
        #axis.ticks = element_blank(), 
        aspect.ratio = 1, 
        plot.margin = margin(0,5,0,0))
    

    p <- ggplot(ind_df, aes(x = ind_dim1, y = ind_dim2)) +
        stat_summary_2d(aes(z = elix_score), fun = mean, bins = 25) +
        scale_fill_viridis_c(option = "turbo") +
        geom_vline(xintercept = 0, linewidth = 0.5, linetype = "dashed", color = "grey30") +
        geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dashed", color = "grey30") +
        ylim(-1,1.35) +
        labs(x = "Dim1 (19.9%)", y = "Dim2 (6.3%)", fill = "Score") +  #, title = "Elixhauser Score - MCA") +
        scale_x_continuous(breaks = seq(-0.5, 2.0, by = 0.5)) +
         theme_grey(base_size = 6) + 
         theme(plot.title = element_blank(),
         legend.position = c(0.1, 0.85),
        legend.title = element_text(size = 6.4),
        legend.background = element_rect(fill="white",
                                  linewidth=0.25, linetype="solid", 
                                  color ="grey30"),
        legend.key.size   = unit(3, "mm"), 
        #axis.text.y = element_blank(), 
        #axis.ticks.y = element_blank(), 
        #axis.title.y = element_blank(),
        aspect.ratio = 1, 
        plot.margin = margin(0,0,0,0))

    ph <- ggplot(corr_df %>%
        filter(outcome %in% c("lsd", "Severity", "ld_hosp")), 
        aes(x = reorder(variable, -correlation), 
            y = factor(outcome, levels = c("ld_hosp", "Severity", "lsd"), labels = c("LD: Hospitalized", "Hospitalized", "LD")), 
            fill = correlation)) +
        geom_tile() + 
        geom_text(aes(label = stars.pval(p_adj)),
            size = 6/.pt, color = "black") +
        scale_fill_viridis_c(option = "rocket", direction = 1, begin = 0.3, end = 1) +
        labs(x = "", y = "", fill = expression(phi)) +
        coord_equal() +
        theme_classic(base_size = 6) +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.line = element_blank(),
            plot.margin = margin(0,0,0,20)
        )

    wrs_dim1 <- wilcox.test(ind_dim1 ~ lsd, data = ind_df)
    print(wrs_dim1)

    wrs_dim2 <- wilcox.test(ind_dim2 ~ lsd, data = ind_df)
    print(wrs_dim2)

    hosp_dim1 <- wilcox.test(ind_dim1 ~ Binary_Severity, data = ind_df)
    print(hosp_dim1)
    hosp_dim2 <- wilcox.test(ind_dim2 ~ Binary_Severity, data = ind_df)
    print(hosp_dim2)

    stats_dims <- ind_df %>%
        group_by(lsd) %>%
        summarise(
            median_dim1 = median(ind_dim1),
            median_dim2 = median(ind_dim2)
        )
    print(stats_dims)
    
    pval_string <- function(pval) {
        ifelse(
            pval < 2.2e-16,
            "p < 2.2e-16",
            paste("p =", formatC(pval, format = "e", digits = 1))
        )
    }

    q <- ggplot(ind_df, aes(x = ind_dim1, y = lsd, fill = lsd)) +
    geom_violin(trim = FALSE, color = "grey30") +
    geom_boxplot(width = 0.025, fill = "grey30", color = "grey30", outlier.shape = NA) +
    stat_summary(fun = median, geom = "point", shape = 16, size = 0.5, color = "white") +
    scale_x_continuous(breaks = seq(-1, 2.5, by = 0.5)) +
    labs(y="", x = "Dim1", fill = "Group", title = "") +
    annotate("text", x = Inf, y = Inf, label = pval_string(wrs_dim1$p.value),  hjust = 1.1, vjust = 2.0, size = 2) + 
      scale_fill_manual(values = c("Control" = "#F8766D", "LD" = "#00BFC4")
        ) +
    theme_classic(base_size = 6) +
    theme(
        legend.position = "none",
        axis.text.x     = element_blank(),
        axis.line       = element_blank(),
        axis.ticks.x    = element_blank(),
        panel.border    = element_rect(fill = NA, color = "black", linewidth = 0.5),
        plot.title      = element_blank(),
        axis.title.x.top = element_blank(),
        aspect.ratio    = 0.5,
        plot.margin     = margin(0,0,0,0)
    )

    k <- ggplot(ind_df, aes(x = ind_dim2, y = lsd, fill = lsd)) +
        geom_violin(trim = FALSE, color = "grey30") +
        geom_boxplot(width = 0.025, fill = "grey30", color = "grey30", outlier.shape = NA) +
        stat_summary(fun = median, geom = "point", shape = 16, size = 0.5, color = "white") +
        scale_x_continuous(breaks = seq(-1, 2.5, by = 0.5)) +
        labs(y="", x = "Dim2", fill = "Group", title = "") +
        annotate("text", x = Inf, y = Inf, label = pval_string(wrs_dim2$p.value),  hjust = 1.1, vjust = 2.0, size = 2) + 
        scale_fill_manual(values = c("Control" = "#F8766D", "LD" = "#00BFC4")
        ) +
        theme_classic(base_size = 6) +
        theme(
            legend.position = "none",
            axis.line       = element_blank(),
            #axis.text.y       = element_blank(),
            #axis.ticks.y      = element_blank(),
            panel.border    = element_rect(fill = NA, color = "black", linewidth = 0.5),
             plot.title      = element_blank(),
            aspect.ratio    = 0.5,
            plot.margin     = margin(0,0,0,0)
        )

    r <- ggplot(ind_df, aes(x = ind_dim1, y = Binary_Severity, fill = Binary_Severity)) +
    geom_violin(trim = FALSE, color = "grey30") +
    geom_boxplot(width = 0.025, fill = "grey30", color = "grey30", outlier.shape = NA) +
    stat_summary(fun = median, geom = "point", shape = 16, size = 0.5, color = "white") +
    scale_x_continuous(breaks = seq(-1, 2.5, by = 0.5)) +
    labs(x = "Dim1", y = "", fill = "Group", title = "") +
    annotate("text", x = Inf, y = Inf, label = pval_string(hosp_dim1$p.value),  hjust = 1.1, vjust = 2.0, size = 2) + 
       scale_fill_manual(values = c("Not Hosp" = "#2d8cff", "Hosp" = "#f26d21")
        ) + 
    theme_classic(base_size = 6) +
    theme(
        legend.position = "none",
        axis.line       = element_blank(),
        panel.border    = element_rect(fill = NA, color = "black", linewidth = 0.5),
         plot.title      = element_blank(),
        aspect.ratio    = 0.5,
        plot.margin     = margin(0,0,0,0)
    )

    a <- ggplot(ind_df, aes(x = ind_dim2, y = Binary_Severity, fill = Binary_Severity)) +
    geom_violin(trim = FALSE, color = "grey30") +
    geom_boxplot(width = 0.025, fill = "grey30", color = "grey30", outlier.shape = NA) +
    stat_summary(fun = median, geom = "point", shape = 16, size = 0.5, color = "white") +
    scale_x_continuous(breaks = seq(-1, 2.5, by = 0.5)) +
    labs(x = "Dim2", y = "", fill = "Group", title = "", caption = "") +
     annotate("text", x = Inf, y = Inf, label = pval_string(hosp_dim2$p.value),  hjust = 1.1, vjust = 2.0,  size = 2) + 
       scale_fill_manual(values = c("Not Hosp" = "#2d8cff", "Hosp" = "#f26d21")
        ) + 
    theme_classic(base_size = 6) +
    theme(
        legend.position = "none",
        axis.line       = element_blank(),
        #axis.ticks.y    = element_blank(),
        #axis.text.y     = element_blank(),
        panel.border    = element_rect(fill = NA, color = "black", linewidth = 0.5),
        plot.title      = element_blank(),
        aspect.ratio    = 0.5,
        plot.margin     = margin(0, 0, 0, 0)
    )
    
    sup_coords <- as.data.frame(mca_res$quali.sup$coord)
    sup_coords$name <- rownames(sup_coords)
    special <- c("LD_Hospitalized") # "Control"

    l <- fviz_mca_var(mca_res,col.var="contrib",repel=TRUE,axes.linetype="blank",select.var=list(contrib=30))
    l$layers <- l$layers[1]

    l <- l +
    geom_point(data=sup_coords,aes(x=`Dim 1`,y=`Dim 2`),shape=18,color="red",size=2) +
    geom_hline(yintercept=0,linetype="dashed",linewidth=0.5,color="grey30") +
    geom_vline(xintercept=0,linetype="dashed",linewidth=0.5,color="grey30") +
    geom_text_repel(
    data = subset(sup_coords, !name %in% special),
    aes(x = `Dim 1`, y = `Dim 2`, label = gsub("_", ": ", name)),
    color = "red", size = 2,
    max.overlaps = Inf, seed = 6, direction = "both",
    bg.colour = "white", bg.r = 0.15, nudge_y=0.05
    ) +
    geom_text_repel(
    data = subset(sup_coords, name %in% special),
    aes(x = `Dim 1`, y = `Dim 2`, label = gsub("_", ": ", name)),
    color = "red", size = 2,
    max.overlaps = Inf, seed = 6, direction = "x", #nudge_y=0.05,
    bg.colour = "white", bg.r = 0.15
    ) +
    geom_text_repel(
    aes(label = gsub("_", ": ", name), color = contrib),
    size = 2, max.overlaps = 5, seed = 20,
    bg.colour = "white", bg.r = 0.15
    ) +
    theme_grey(base_size=6) +
    scale_color_viridis_c(option="plasma",direction=1,begin=0.2,end=0.9,na.value="red") +
    labs(color="Contribution",title="") +
    theme(aspect.ratio=1,plot.title=element_blank(),
            legend.position=c(0.12,0.95),legend.direction="horizontal",
            legend.title=element_text(size=6.4),
            legend.key.height=unit(3,"mm"),legend.key.width=unit(2,"mm"),
            legend.background=element_rect(fill="white",linewidth=0.25,linetype="solid",color="grey30"),
            plot.margin=margin(0,0,0,0))

    b <- fviz_contrib(mca_res, choice="var", axes = 1, top = 15,
        fill = "steelblue") +
        labs(y = "Contribution (%)", x = "", title = "Dim1 Contributions") +
        theme_classic(base_size = 6) + # hjust = 1.0, vjust = 1.0, angle = 45 # angle = 90, hjust = 1.0, vjust = 0.5
        scale_x_discrete(labels = function(x) gsub("_", ": ", x)) +
        theme(axis.line = element_blank(), axis.text.x = element_text(hjust = 1.0, vjust = 1.0, angle = 45), panel.border = element_rect(linewidth = 0.5, color = "black", fill = NA), plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0,0,0,15))

    o <- fviz_contrib(mca_res, choice="var", axes = 2, top = 15,
         fill = "steelblue") +
        labs(y = "", x = "", title = "Dim2 Contributions") +
         theme_classic(base_size = 6) +
         scale_x_discrete(labels = function(x) gsub("_", ": ", x)) +
         scale_y_continuous(breaks = c(0, 3,6,9)) + 
        theme(axis.line = element_blank(), axis.text.x = element_text(hjust = 1.0, vjust = 1.0, angle = 45), panel.border = element_rect(linewidth = 0.5, color = "black", fill = NA), plot.title = element_text(hjust = 0.5), 
        plot.margin = margin(0,0,0,0))
    
    # eig_vals <- mca_res$eig[,2]
    # n <- fviz_eig(mca_res, choice = "variance", addlabels = FALSE, barcolor = "steelblue", ylab = "Variance (%)") +
    #     geom_text(aes(x = 1:length(eig_vals), y = eig_vals,
    #     label = round(eig_vals, 1)),
    # vjust = 0, hjust = 0, size = 2) + 
    #     theme_classic(base_size = 6) + 
    #     theme(axis.line = element_blank(), panel.border = element_rect(linewidth = 0.5, color = "black", fill = NA), plot.title = element_text(hjust = 0.5), aspect.ratio = 0.5, 
    #     plot.margin = margin(0,0,0,0))
    eig_top10 <- eig_df[1:10,] %>%
        mutate(dimension_num = as.integer(gsub("dim ", "", dimension)))

    n <- ggplot(eig_top10, aes(x = dimension_num, y = percent_variance)) +
        geom_col(fill = "steelblue", color = "white") +
        geom_line(color = "black") +
        geom_point(color = "black", size = 1) +
        geom_text(
            aes(label = paste0(round(percent_variance, 1), "%")),
            vjust = -0.2, hjust = -0.1, size = 2
        ) +
        scale_x_continuous(
            breaks = eig_top10$dimension_num,
            labels = eig_top10$dimension_num
        ) +
        labs(x = "Dimensions", y = "Variance explained (%)", title = "Scree plot") +
        theme_classic(base_size = 6) +
        theme(
            panel.border = element_rect(linewidth = 0.5, color = "black", fill = NA),
            plot.title = element_text(hjust = 0.5),
            aspect.ratio = 0.5,
            plot.margin = margin(0,0,0,0)
        )

    # top <- (j | i)
    # top <- wrap_elements(top)
    # left <- (b )
    # left <- wrap_elements(left)
    # right <- (k / a)
    # right <- wrap_elements(right)
    top <- (j | p) + plot_layout(widths = c(1,1))
    #top <- wrap_elements(top)
    bottom <- (q | k)
    bottom <- wrap_elements(bottom)
    # bottom <- (b | o)
    # bottom <- wrap_elements(bottom)
    # top <- n
    # top <- wrap_elements(top)

    patch <- (l / ph) +
        plot_layout(heights = c(4,1)) + # heights = c(2,1,1)
        plot_annotation(tag_levels = "a") &
        theme(
            plot.tag = element_text(face = "bold", size = 12),
            plot.margin = margin(0,0,0,10)
        )

    png(graphicsFile, width=170, height=200, units="mm", res=600)
    print(patch)

    # svg(graphicsFile, width = 6.69, height = 3.34, bg = "transparent")
    # print(ph)

    final_df <- cbind_na(corr_df, var_df, eig_df, eta2_df)

    return(final_df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.737bf81d-8b8b-4cd3-bea8-0b91ba1dfb6b"),
    enrichment_prep=Input(rid="ri.foundry.main.dataset.3326f9d3-68c4-4993-a336-b6764c27d290"),
    phecode_map=Input(rid="ri.foundry.main.dataset.c2bec0d5-a47e-420c-9f1f-1526771f00b7")
)
library(dplyr)
library(tidyr)
library(purrr)
enriched_phecodes_by_ld <- function( phecode_map, enrichment_prep) {
    map <- phecode_map %>% select(phecode, phecode_string, `category`) %>% distinct()
    
    phe_df <- enrichment_prep

     concept_list <- list(
        sphingo   = sphingo,
        lipid     = lipid,
        fabry     = fabry,
        gaucher   = gaucher,
        metak     = metak,
        amino     = amino,
        glycopro = glycopro,
        gang      = gang,
        cys       = cys,
        ncl       = ncl
    )
    
    df_list <- purrr::imap(concept_list, function(cond_vec, nm) {
        split_df(phe_df, cond_vec) %>%
            dplyr::mutate(
            !!rlang::sym(nm) := as.integer(lsd_condition_name %in% cond_vec),
            group = ifelse(.data[[nm]] == 1, "yes", "no")  # <-- yes/no by lsd subtype
            )
        })

    run_phecode_enrichment <- function(phe_df, map) {
        if (nrow(phe_df) == 0) {
            return(tibble::tibble())
        }
        totals <- phe_df %>%
            distinct(person_id, group) %>%
            count(group, name = "count") %>%
            pivot_wider(
            names_from = group,
            values_from = count,
            names_prefix = "total_"
            )

        if (!all(c("total_yes", "total_no") %in% names(totals))) {
            return(tibble::tibble())
        }
        
        contingency <- phe_df %>%
            distinct(person_id, phecode, group) %>%
            count(phecode, group, name = "phecode_count") %>%
            pivot_wider(
            names_from = group,
            values_from = phecode_count,
            values_fill = 0
            ) %>%
            mutate(
            phecode = phecode,
            total_yes = totals$total_yes,
            total_no  = totals$total_no,
            a = yes,                  # with phecode & yes
            b = total_yes - a,        # yes without phecode
            c = no,                   # with phecode & no
            d = total_no - c,         # no without phecode
            .keep = "none"
            ) %>%
            dplyr::filter( # sanity check filter
                a >= 20,
                c >= 20
            )

         if (nrow(contingency) == 0) {
            return(tibble::tibble())
        }
        
        fisher_results <- contingency %>%
            select(phecode, a, b, c, d) %>%
            pmap_dfr(function(phecode, a, b, c, d) {
            ft <- fisher.test(matrix(c(a, b, c, d), nrow = 2))
            tibble(
                phecode,
                a, b, c, d,
                odds_ratio = unname(ft$estimate),
                lower_ci   = ft$conf.int[1],
                upper_ci   = ft$conf.int[2],
                p_value    = ft$p.value
            )
            })
        
        results <- fisher_results %>%
            mutate(
            log2_or = log2(odds_ratio),
            p_adj   = p.adjust(p_value, method = "fdr"),
            log_p   = -log10(p_adj),
            enriched_yes     = odds_ratio > 2 & is.finite(odds_ratio) & p_adj < 0.05,
            enriched_no      = odds_ratio < 0.5 & is.finite(odds_ratio) & p_adj < 0.05,
            significant      = p_adj < 0.05
            ) %>%
            left_join(map, by = "phecode") %>%
            arrange(desc(odds_ratio), p_adj)
        
        return(results)
        }

    phecode_enrichment_results <- purrr::imap_dfr(df_list, function(dat, nm) {
            res <- run_phecode_enrichment(dat, map)
            res %>% mutate(ld_name = nm)
        })

    return(phecode_enrichment_results)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c6244ffd-6f06-4c55-aba8-7f39157a393d"),
    ert_drug_exposure=Input(rid="ri.foundry.main.dataset.7d140f59-7701-43fe-a4e3-13e8027ac651")
)
library(dplyr)
library(gtsummary)
ert_drug_table <- function(ert_drug_exposure) {
     df <- ert_drug_exposure %>% 
        dplyr::filter(has_lsd == 1) %>%
        dplyr::select(person_id, drug_name) %>%
        dplyr::distinct()  # avoid double-counting people per drug

    ert_long <- df %>%
    dplyr::count(drug_name, name = "on_ert") %>%
        mutate(
            on_ert = if_else(on_ert < 20, "<20", as.character(on_ert))
        )

    return(ert_long)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2e36883b-cb2d-4a09-8012-4ffaaa2297a4"),
    cohort_phecodes_2_1=Input(rid="ri.foundry.main.dataset.977b8ef2-7d67-414f-85e0-2f5524a772aa"),
    phecode_map=Input(rid="ri.foundry.main.dataset.c2bec0d5-a47e-420c-9f1f-1526771f00b7")
)
library(dplyr)
library(tidyr)
hosp_phecodes <- function( phecode_map, cohort_phecodes_2_1) {
     
     map <- phecode_map %>% select(phecode, phecode_string, `category`) %>% distinct()
     
     phe_df <- cohort_phecodes_2_1 %>%
        filter(Binary_Severity == 1, # hosp
           days_diff <= 15 & days_diff > 0, 
            observation_period_before_covid >= 365,
            observation_period_post_covid >= 365, 
            !grepl("^(GE_|CM_|NB_|PP_)", prefix)) %>%# only lsd patients, outside acute window, excluding COVID and genetic terms and patient must have sufficient history
        mutate(has_lsd = case_when(
            has_lsd == 1 ~ "has_lsd",
            has_lsd == 0 ~ "no_lsd"
        )) %>%
        group_by(person_id, phecode, covid_period) %>%
        slice_min(order_by = condition_start_date, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        distinct()
       
   totals <- phe_df %>%
        distinct(person_id, has_lsd) %>%
        count(has_lsd, name = "lsd_count") %>%
        pivot_wider(
            names_from = has_lsd,
            values_from = lsd_count,
            names_prefix = "total_"
        )

    contingency <- phe_df %>%
        distinct(person_id, phecode, has_lsd) %>%
        count(phecode, has_lsd, name = "phecode_count") %>%
        pivot_wider(
            names_from = has_lsd,
            values_from = phecode_count,
            values_fill = 0
        ) %>%
        crossing(totals) %>% 
        mutate(
            phecode,
            a = has_lsd,                 # with phecode & has_lsd
            b = total_has_lsd - a,       # has_lsd without phecode
            c = no_lsd,              # with phecode & no_lsd
            d = total_no_lsd - c,     # no_lsd without phecode
            .keep = "none"
        ) %>% dplyr::filter( # sanity check filter
                a >= 20,
                c >= 20
        )
            
    results <- contingency %>%
        mutate(
            test = mapply(function(a, b, c, d) {
            ft <- fisher.test(matrix(c(a, b, c, d), nrow = 2))
            list(
                odds_ratio = unname(ft$estimate),
                lower_ci   = ft$conf.int[1],
                upper_ci   = ft$conf.int[2],
                p_value    = ft$p.value
            )
            }, a, b, c, d, SIMPLIFY = FALSE)
        ) %>%
        tidyr::unnest_wider(test) %>%
        mutate(
            log2_or = log2(odds_ratio),
            p_adj = p.adjust(p_value, method = "fdr"),
            log_p = -log10(p_adj),
            enriched_lsd = odds_ratio > 2 & is.finite(odds_ratio) & p_adj < 0.05,
            enriched_control = odds_ratio < 0.5 & is.finite(odds_ratio) & p_adj < 0.05,
            significant = p_adj < 0.05
        ) %>%
        left_join(map, by = "phecode") %>%
        arrange(desc(odds_ratio), p_adj)

    return(results)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c97039f0-5ad7-4f1e-9dbb-c21987d18ce2"),
    cohort_phecodes_2_1=Input(rid="ri.foundry.main.dataset.977b8ef2-7d67-414f-85e0-2f5524a772aa"),
    phecode_map=Input(rid="ri.foundry.main.dataset.c2bec0d5-a47e-420c-9f1f-1526771f00b7")
)
library(dplyr)
library(tidyr)
hosp_phecodes_controls <- function( phecode_map, cohort_phecodes_2_1) {
     map <- phecode_map %>% select(phecode, phecode_string, `category`) %>% distinct()

   phe_df <- cohort_phecodes_2_1 %>%
        filter(has_lsd == 0, 
            days_diff <= 15 & days_diff > 0, 
            observation_period_before_covid >= 365,
            observation_period_post_covid >= 365, 
             !grepl("^(GE_|CM_|NB_|PP_)", prefix)) %>% # only lsd patients, outside acute window, excluding COVID and genetic terms and patient must have sufficient history
        mutate(Binary_Severity = case_when(
            Binary_Severity == 1 ~ "hosp",
            Binary_Severity == 0 ~ "no_hosp"
        )) %>%
        group_by(person_id, phecode, covid_period) %>%
        slice_min(order_by = condition_start_date, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        distinct()
        
   totals <- phe_df %>%
        distinct(person_id, Binary_Severity) %>%
        count(Binary_Severity, name = "severity_count") %>%
        pivot_wider(
            names_from = Binary_Severity,
            values_from = severity_count,
            names_prefix = "total_"
        )

    contingency <- phe_df %>%
        distinct(person_id, phecode, Binary_Severity) %>%
        count(phecode, Binary_Severity, name = "phecode_count") %>%
        pivot_wider(
            names_from = Binary_Severity,
            values_from = phecode_count,
            values_fill = 0
        ) %>%
        crossing(totals) %>% 
        mutate(
            phecode,
            a = hosp,                 # with phecode & hosp
            b = total_hosp - a,       # hosp without phecode
            c = no_hosp,              # with phecode & no_hosp
            d = total_no_hosp - c,     # no_hosp without phecode
            .keep = "none"
        ) %>% dplyr::filter( # sanity check filter
                a >= 20,
                c >= 20
        )
            
    results <- contingency %>%
        mutate(
            test = mapply(function(a, b, c, d) {
            ft <- fisher.test(matrix(c(a, b, c, d), nrow = 2))
            list(
                odds_ratio = unname(ft$estimate),
                lower_ci   = ft$conf.int[1],
                upper_ci   = ft$conf.int[2],
                p_value    = ft$p.value
            )
            }, a, b, c, d, SIMPLIFY = FALSE)
        ) %>%
        tidyr::unnest_wider(test) %>%
        mutate(
            log2_or = log2(odds_ratio),
            p_adj = p.adjust(p_value, method = "fdr"),
            log_p = -log10(p_adj),
            enriched_hosp = odds_ratio > 2 & is.finite(odds_ratio) & p_adj < 0.05,
            enriched_no_hosp = odds_ratio < 0.5 & is.finite(odds_ratio) & p_adj < 0.05,
            significant = p_adj < 0.05
        ) %>%
        left_join(map, by = "phecode") %>%
        arrange(desc(odds_ratio), p_adj)

    return(results)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f295dcd3-2625-4008-a6f2-dc282daa3ae1"),
    cohort_phecodes_2_1=Input(rid="ri.foundry.main.dataset.977b8ef2-7d67-414f-85e0-2f5524a772aa"),
    phecode_map=Input(rid="ri.foundry.main.dataset.c2bec0d5-a47e-420c-9f1f-1526771f00b7")
)
library(dplyr)
library(tidyr)
hosp_phecodes_lsd <- function( phecode_map, cohort_phecodes_2_1) {
    
     map <- phecode_map %>% select(phecode, phecode_string, `category`) %>% distinct()

    phe_df <- cohort_phecodes_2_1 %>%
        filter(has_lsd == 1, 
            days_diff <= 15 & days_diff > 0, 
            observation_period_before_covid >= 365,
            observation_period_post_covid >= 365, 
             !grepl("^(GE_|CM_|NB_|PP_)", prefix)) %>% # only lsd patients, outside acute window, excluding COVID and genetic terms and patient must have sufficient history
        mutate(Binary_Severity = case_when(
            Binary_Severity == 1 ~ "hosp",
            Binary_Severity == 0 ~ "no_hosp"
        )) %>%
        group_by(person_id, phecode, covid_period) %>%
        slice_min(order_by = condition_start_date, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        distinct()
       

   totals <- phe_df %>%
        distinct(person_id, Binary_Severity) %>%
        count(Binary_Severity, name = "severity_count") %>%
        pivot_wider(
            names_from = Binary_Severity,
            values_from = severity_count,
            names_prefix = "total_"
        )

    contingency <- phe_df %>%
        distinct(person_id, phecode, Binary_Severity) %>%
        count(phecode, Binary_Severity, name = "phecode_count") %>%
        pivot_wider(
            names_from = Binary_Severity,
            values_from = phecode_count,
            values_fill = 0
        ) %>%
        crossing(totals) %>% 
        mutate(
            phecode,
            a = hosp,                 # with phecode & hosp
            b = total_hosp - a,       # hosp without phecode
            c = no_hosp,              # with phecode & no_hosp
            d = total_no_hosp - c,     # no_hosp without phecode
            .keep = "none"
        ) %>% dplyr::filter( # sanity check filter
                a >= 20,
                c >= 20
        )
            
    results <- contingency %>%
        mutate(
            test = mapply(function(a, b, c, d) {
            ft <- fisher.test(matrix(c(a, b, c, d), nrow = 2))
            list(
                odds_ratio = unname(ft$estimate),
                lower_ci   = ft$conf.int[1],
                upper_ci   = ft$conf.int[2],
                p_value    = ft$p.value
            )
            }, a, b, c, d, SIMPLIFY = FALSE)
        ) %>%
        tidyr::unnest_wider(test) %>%
        mutate(
            log2_or = log2(odds_ratio),
            p_adj = p.adjust(p_value, method = "fdr"),
            log_p = -log10(p_adj),
            enriched_hosp = odds_ratio > 2 & is.finite(odds_ratio) & p_adj < 0.05,
            enriched_no_hosp = odds_ratio < 0.5 & is.finite(odds_ratio) & p_adj < 0.05,
            significant = p_adj < 0.05
        ) %>%
        left_join(map, by = "phecode") %>%
        arrange(desc(odds_ratio), p_adj)

    return(results)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.75249b97-a1c4-4af1-9da1-5d345797fcc4"),
    cohort2=Input(rid="ri.foundry.main.dataset.2be8aaea-6435-48ce-890d-d4441c292af7")
)
library(dplyr)
library(purrr)
hospitalization_events_summary <- function(cohort2) {

    co <- cohort2

    concept_list <- list(
        sphingo   = sphingo,
        lipid     = lipid,
        fabry     = fabry,
        gaucher   = gaucher,
        metak     = metak,
        amino     = amino,
        gang      = gang,
        cys       = cys,
        ncl       = ncl
    )

    # apply split_df(co, ...) to each vector
    df_list <- imap(concept_list, ~ split_df(co, .x))

    all_counts <- purrr::map2_dfr(df_list, names(df_list),
                              ~ count_events(.x, .y, "Binary_Severity")) %>% rename()

    return(all_counts)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.214af28a-06c0-43e2-80b7-192bfd9b533a"),
    all_cohort1=Input(rid="ri.foundry.main.dataset.ca3e686b-a2cd-4f80-8494-fc7458661c3e")
)
library(survival)
library(dplyr)
library(broom)
library(stringr)
infection_clogit <- function(all_cohort1) {
    
    input_df <- all_cohort1

    concept_list <- list(
        sphingo   = sphingo,
        lipid     = lipid,
        fabry     = fabry,
        gaucher   = gaucher,
        metak     = metak,
        amino     = amino,
        gang      = gang,
        cys       = cys,
        ncl       = ncl,
        glycopro = glycopro
    )

    df_list <- purrr::imap(concept_list, function(cond_vec, nm) {
        split_df(input_df, cond_vec) %>%
            dplyr::mutate(!!rlang::sym(nm) := ifelse(lsd_condition_name %in% cond_vec, 1, 0))
        })

    run_clogit <- function(df, nm) {
        # e.g., "COVID_pos_indicator ~ sphingo + strata(subclass)"
        fml <- as.formula(paste("COVID_pos_indicator ~", nm, "+ strata(subclass)"))
        fit <- clogit(fml, data = df)
        tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>%
            mutate(term_group = nm)
    }

    # Run per-concept models and row-bind results
    final_df <- map_dfr(names(concept_list), function(nm) {
        run_clogit(df = df_list[[nm]], nm = nm)
    })

    fit.clr <- clogit(COVID_pos_indicator ~ has_lsd + strata(subclass),
                  data = input_df)

    print(summary(fit.clr))
    
    res <- tidy(fit.clr, exponentiate = TRUE, conf.int = TRUE) %>% mutate(term_group = "overall")

    return(bind_rows(final_df, res))

}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.dbd2b4e5-9fc5-4e75-a5ed-33d8ac7d7b0c"),
    lsd_prep=Input(rid="ri.foundry.main.dataset.6047ebb9-fec2-4a84-89cb-b480ec11e423")
)
library(mice)
library(dplyr)
lsd_imputation <- function(lsd_prep) {
    # works with environment profile-high-driver-cores-and-memory-minimal
    set.seed(2048)

    df1 <- lsd_prep

    sub_df <- df1 %>% # df for imputation
        select(Age, Race, Sex, observation_period_before_covid, data_partner_id, BMI_before_or_day_of_covid, number_of_visits_before_covid, Binary_Severity, lsd_condition_name) %>%
        mutate(
            Age = as.integer(Age),
            Race = as.factor(Race),
            Sex = as.factor(Sex),
            BMI_before_or_day_of_covid = as.numeric(BMI_before_or_day_of_covid),
            observation_period_before_covid = as.integer(observation_period_before_covid),
            number_of_visits_before_covid = as.integer(number_of_visits_before_covid),
            Binary_Severity = as.integer(Binary_Severity),
            lsd_condition_name = as.factor(lsd_condition_name),
            data_partner_id = as.factor(data_partner_id)
        )

    # print proportion missing
    print(mean(is.na(sub_df$Age)))
    print(mean(is.na(sub_df$BMI_before_or_day_of_covid)))

    impute <- mice(sub_df, m=2, method = "pmm", maxit = 5, seed = 2048) # Impute age # and BMI using predictive mean matching. m = number of imputed datasets, make this lower if you have OOM errors

    # return the imputed "mids" object to dataframe
    imp_df <- mice::complete(impute,2) # there is also a complete function in dplyr, so be careful here. select 2nd dataset

    df1$Age <- as.integer(imp_df$Age)
    df1$BMI_before_or_day_of_covid <- as.numeric(imp_df$BMI_before_or_day_of_covid)

    return(df1)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2256e216-2e28-4160-9fe2-5951c8745268"),
    cohort_measurement_numeric=Input(rid="ri.foundry.main.dataset.2d9e53a6-52ce-4129-bbc7-7c55520833e2")
)
library(dplyr) # data manipulation
library(broom) # tidy objects
library(stringr) # severity grouping
library(ggplot2) # plotting
library(gtools) # stars.pval
library(emmeans) # emmeans
library(patchwork) # multi panel plots
library(ggh4x)
library(tidyr)
measurement_contrasts <- function(cohort_measurement_numeric) {
    
    measurement_df <- cohort_measurement_numeric %>%
    # Severity types
    # Moderate_Hosp_around_strong_signal_COVID_index
    # Moderate_Hosp_around_weak_signal_COVID_index
    # Death_within_n_days_after_COVID_index
    # Mild_No_ED_or_Hosp_around_COVID_index
    # Severe_ECMO_IMV_in_Hosp_around_weak_signal_COVID_index
    # Severe_ECMO_IMV_in_Hosp_around_strong_signal_COVID_index
    # Mild_ED_around_strong_signal_COVID_index
    # Mild_ED_around_weak_signal_COVID_index
        dplyr::mutate(Severity = case_when( # define severity groups
                str_detect(Severity_Type, "Mild") ~ "Mild",
                str_detect(Severity_Type, "Moderate") ~ "Moderate",
                str_detect(Severity_Type, "Severe") ~ "Severe",
                str_detect(Severity_Type,"Death") ~ "Death"
                ))   

    # measurements with enough values and/or are clinically meaningful
    allowed_measurements <- c(
        "Systolic blood pressure",
        "Diastolic blood pressure",
        "Heart rate",
        "Oxygen saturation in Arterial blood by Pulse oximetry",
        "Respiratory rate",
        "Body temperature",
        "Sodium [Moles/volume] in Serum or Plasma",
        "Urea nitrogen [Mass/volume] in Serum or Plasma",
        "Potassium [Moles/volume] in Serum or Plasma",
        "Hemoglobin [Mass/volume] in Blood",
        "Chloride [Moles/volume] in Serum or Plasma",
        "Glucose [Mass/volume] in Serum or Plasma",
        "Platelets [#/volume] in Blood by Automated count",
        "Creatinine [Mass/volume] in Serum or Plasma",
        "Leukocytes [#/volume] in Blood by Automated count",
        "Aspartate aminotransferase [Enzymatic activity/volume] in Serum or Plasma"
    )

    slim_measurement_df <- measurement_df %>%
        filter(measurement_concept_name %in% allowed_measurements) %>%
        mutate(
          measurement_label = paste(
            stringr::str_squish(
                stringr::str_replace_all(
                measurement_concept_name,
                c(
                    "Aspartate aminotransferase" = "AST",
                    "blood pressure" = "BP",
                    "\\[.*?\\]" = "",
                    "\\s+in.*" = ""
                )
                )
            ),
            paste0("(", units, ")"),
            sep = "\n"
            )
        ) %>%
        distinct()
        
    days_of_interest <- 0:21

    stats_tbl <- slim_measurement_df %>%
        dplyr::filter(
            days_diff %in% days_of_interest,
            !is.na(harmonized_value_as_number),
            is.finite(harmonized_value_as_number),
            harmonized_value_as_number > 0
        ) %>%
        dplyr::group_by(days_diff, measurement_label, has_lsd, Severity) %>%
        dplyr::summarise(
            n        = dplyr::n(),
            median   = stats::median(harmonized_value_as_number),
            q1       = stats::quantile(harmonized_value_as_number, 0.25),
            q3       = stats::quantile(harmonized_value_as_number, 0.75),
            .groups  = "drop"
        ) %>% print(n=Inf, width=Inf)
    
    em_aov <- function(df, day) { 
        df_day <- dplyr::filter(df, days_diff == day) %>%
            dplyr::filter(
                !is.na(harmonized_value_as_number),
                is.finite(harmonized_value_as_number),
                harmonized_value_as_number > 0
                )
        
        # Estimated marginal means on response scale (back-transformed)
        em_df <- df_day %>%
            dplyr::group_by(measurement_label) %>%
            dplyr::do({
            fit <- aov(log(harmonized_value_as_number) ~ has_lsd * Severity, data = .)
            
            # Inform emmeans this is a log-transformed response
            em  <- emmeans::emmeans(fit, ~ has_lsd * Severity, tran = "log")
            
            # Back-transform to original scale
            em_resp <- summary(em, type = "response")
            tibble::as_tibble(em_resp)
            }) %>%
            dplyr::ungroup() %>%
            dplyr::rename(
            measurement_label1 = measurement_label,
            SE1          = SE,
            df1          = df,
            lower.CL1    = lower.CL,
            upper.CL1    = upper.CL
            ) %>%
            dplyr::mutate(model1 = paste0("Model day = ", day))
        
        
        # Pairwise contrasts (response scale = interpretable ratios)
        pairs_df <- df_day %>%
            dplyr::group_by(measurement_label) %>%
            dplyr::do({
            d  <- .                      # original data for this measurement_label × day
            nd <- nrow(d)                # total n for this measurement_label × day
            fit <- aov(log(harmonized_value_as_number) ~ has_lsd * Severity, data = .)
            
            em  <- emmeans::emmeans(fit, ~ has_lsd * Severity, tran = "log")
            
            # Pairwise comparisons with interpreted contrast ratios
            out <- summary(
                pairs(em),
                type  = "response",   # ratios on original scale
                infer = TRUE,         # CIs + p-values
                adjust = "none"
            )
            
            tibble::as_tibble(out) %>%
                dplyr::mutate(
                p.holm  = p.adjust(p.value, method = "holm"),
                p.fdr   = p.adjust(p.value, method = "fdr"),
                signif  = stars.pval(p.holm),
                
                # same severity indicator
                same_severity =
                    sub(".* ", "", sub(" [/-] .*", "", contrast)) == sub(".* ", "", sub(".* [/-] ", "", contrast)),
                ci_increase = (lower.CL > 1 & upper.CL > 1),
                ci_decrease = (lower.CL < 1 & upper.CL < 1),
                n_total_measurements = nd
                )
            }) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(model = paste0("Model day = ", day))
        
        # Return combined frame
        return(cbind_na(pairs_df, em_df))
    }

    day0  <- em_aov(slim_measurement_df, 0)
    day1  <- em_aov(slim_measurement_df, 1)
    day3  <- em_aov(slim_measurement_df, 3)
    day5  <- em_aov(slim_measurement_df, 5)
    day7  <- em_aov(slim_measurement_df, 7)
    day10 <- em_aov(slim_measurement_df, 10)
    day14 <- em_aov(slim_measurement_df, 14)
    day21 <- em_aov(slim_measurement_df, 21)

    clean_stats <- stats_tbl %>%
        mutate(
            days_diff = paste0("days_diff=", days_diff),
            has_lsd   = paste0("has_lsd=", has_lsd)
        ) %>%
        unite("measurement_key", days_diff, measurement_label, has_lsd, Severity, sep = "__")

    final_df <- dplyr::bind_rows(
        day0,
        day1,
        day3,
        day5,
        day7,
        day10,
        day14,
        day21
    ) %>%
    cbind_na(clean_stats)

    return(final_df)

}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0932b15d-5ca5-49d6-a4ef-e2e3e3a0f3ac"),
    cohort_phecodes_2_1=Input(rid="ri.foundry.main.dataset.977b8ef2-7d67-414f-85e0-2f5524a772aa"),
    phecode_map=Input(rid="ri.foundry.main.dataset.c2bec0d5-a47e-420c-9f1f-1526771f00b7")
)
library(dplyr)
library(tidyr)
new_onset_phecodes <- function(cohort_phecodes_2_1, phecode_map) {
     map <- phecode_map %>% select(phecode, phecode_string, `category`) %>% distinct()
    
    phe_df <- cohort_phecodes_2_1 %>%
        filter((days_diff > 15 & days_diff <= 365) |
            (days_diff >= -365 & days_diff <= 0),
                observation_period_before_covid >= 365,
                observation_period_post_covid >= 365,
                !grepl("^(GE_|CM_|NB_|PP_)", prefix)) %>%
        group_by(person_id, phecode, covid_period, has_lsd) %>%
        slice_min(order_by = condition_start_date, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        distinct() %>%
        mutate(phecode_value = 1)

    phe_wide <- phe_df %>%
        select(person_id, has_lsd, phecode, covid_period, phecode_value) %>%
        pivot_wider(
            names_from = covid_period,
            values_from = phecode_value,
            values_fill = 0
        ) %>%
        mutate(new_onset = ifelse(before_or_on_covid == 0 & post_covid == 1, 1, 0))

    contingency <- phe_wide %>%
        group_by(phecode) %>%
        summarise(
            a = sum(new_onset == 1 & has_lsd == 1), # LSD new onset
            b = sum(new_onset == 0 & has_lsd == 1), # LSD no onset
            c = sum(new_onset == 1 & has_lsd == 0), # control new onset
            d = sum(new_onset == 0 & has_lsd == 0)  # control no onset
        ) %>% dplyr::filter( # sanity check filter
                a >= 20,
                c >= 20
        )

     results <- contingency %>%
        mutate(
            test = mapply(function(a, b, c, d) {
            ft <- fisher.test(matrix(c(a, b, c, d), nrow = 2))
            list(
                odds_ratio = unname(ft$estimate),
                lower_ci   = ft$conf.int[1],
                upper_ci   = ft$conf.int[2],
                p_value    = ft$p.value
            )
            }, a, b, c, d, SIMPLIFY = FALSE)
        ) %>%
        tidyr::unnest_wider(test) %>%
        mutate(
            log2_or = log2(odds_ratio),
            p_adj = p.adjust(p_value, method = "fdr"),
            log_p = -log10(p_adj),
            enriched_lsd = odds_ratio > 2 & is.finite(odds_ratio) & p_adj < 0.05,
            enriched_control = odds_ratio < 0.5 & is.finite(odds_ratio) & p_adj < 0.05,
            significant = p_adj < 0.05
        ) %>%
        left_join(map, by = "phecode") %>%
        arrange(desc(odds_ratio), p_adj)

    return(results)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6ce936c7-e8ce-47c1-b83e-0e2231276604"),
    cohort_phecodes_2_1=Input(rid="ri.foundry.main.dataset.977b8ef2-7d67-414f-85e0-2f5524a772aa"),
    phecode_map=Input(rid="ri.foundry.main.dataset.c2bec0d5-a47e-420c-9f1f-1526771f00b7")
)
library(dplyr)
library(tidyr)
new_onset_phecodes_controls <- function( phecode_map, cohort_phecodes_2_1) {
    map <- phecode_map %>% select(phecode, phecode_string, `category`) %>% distinct()
    
    phe_df <- cohort_phecodes_2_1 %>%
        filter(has_lsd == 0, 
           (days_diff > 15 & days_diff <= 365) |
            (days_diff >= -365 & days_diff <= 0),
          observation_period_before_covid >= 365,
           observation_period_post_covid >= 365,
             !grepl("^(GE_|CM_|NB_|PP_)", prefix)) %>% # only lsd patients, outside acute window, excluding COVID and genetic terms and patient must have sufficient history
        group_by(person_id, phecode, covid_period) %>%
        slice_min(order_by = condition_start_date, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        distinct()

     totals <- phe_df %>%
        distinct(person_id, covid_period) %>%
        count(covid_period, name = "period_count") %>%
        pivot_wider(
            names_from = covid_period,
            values_from = period_count,
            names_prefix = "total_"
        )

     contingency <- phe_df %>%
        distinct(person_id, phecode, covid_period) %>%
        count(phecode, covid_period, name = "phecode_count") %>%
        pivot_wider(
            names_from = covid_period,
            values_from = phecode_count,
            values_fill = 0
        ) %>%
        crossing(totals) %>% 
        mutate(
            phecode,
            a = post_covid,                 # with phecode & post
            b = total_post_covid - a,       # post without phecode
            c = before_or_on_covid,              # with phecode & before
            d = total_before_or_on_covid - c,     # before without phecode
            .keep = "none"
        ) %>% dplyr::filter( # sanity check filter
                a >= 20,
                c >= 20
        )

    results <- contingency %>%
        mutate(
            test = mapply(function(a, b, c, d) {
            ft <- fisher.test(matrix(c(a, b, c, d), nrow = 2))
            list(
                odds_ratio = unname(ft$estimate),
                lower_ci   = ft$conf.int[1],
                upper_ci   = ft$conf.int[2],
                p_value    = ft$p.value
            )
            }, a, b, c, d, SIMPLIFY = FALSE)
        ) %>%
        tidyr::unnest_wider(test) %>%
        mutate(
            log2_or = log2(odds_ratio),
            p_adj = p.adjust(p_value, method = "fdr"),
            log_p = -log10(p_adj),
            enriched_post_covid = odds_ratio > 2 & is.finite(odds_ratio) & p_adj < 0.05,
            enriched_pre_covid = odds_ratio < 0.5 & is.finite(odds_ratio) & p_adj < 0.05,
            significant = p_adj < 0.05
        ) %>%
        left_join(map, by = "phecode") %>%
        arrange(desc(odds_ratio), p_adj)

    return(results)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ce654687-c67b-455f-9462-d2e9330a7e42"),
    cohort_phecodes_2_1=Input(rid="ri.foundry.main.dataset.977b8ef2-7d67-414f-85e0-2f5524a772aa"),
    phecode_map=Input(rid="ri.foundry.main.dataset.c2bec0d5-a47e-420c-9f1f-1526771f00b7")
)
library(dplyr)
library(tidyr)
new_onset_phecodes_lsd <- function( phecode_map, cohort_phecodes_2_1) {
    
    map <- phecode_map %>% select(phecode, phecode_string, `category`) %>% distinct()
    
    phe_df <- cohort_phecodes_2_1 %>%
        filter(has_lsd == 1, 
           (days_diff > 15 & days_diff <= 365) |
            (days_diff >= -365 & days_diff <= 0),
          observation_period_before_covid >= 365,
           observation_period_post_covid >= 365,
             !grepl("^(GE_|CM_|NB_|PP_)", prefix)) %>% # only lsd patients, outside acute window, excluding COVID, congenital, newborn, and pregnancy terms
        group_by(person_id, phecode, covid_period) %>%
        slice_min(order_by = condition_start_date, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        distinct()

     totals <- phe_df %>%
        distinct(person_id, covid_period) %>%
        count(covid_period, name = "period_count") %>%
        pivot_wider(
            names_from = covid_period,
            values_from = period_count,
            names_prefix = "total_"
        )

     contingency <- phe_df %>%
        distinct(person_id, phecode, covid_period) %>%
        count(phecode, covid_period, name = "phecode_count") %>%
        pivot_wider(
            names_from = covid_period,
            values_from = phecode_count,
            values_fill = 0
        ) %>%
        crossing(totals) %>% 
        mutate(
            phecode,
            a = post_covid,                 # with phecode & post
            b = total_post_covid - a,       # post without phecode
            c = before_or_on_covid,              # with phecode & before
            d = total_before_or_on_covid - c,     # before without phecode
            .keep = "none"
        ) %>% dplyr::filter( # sanity check filter
                a >= 20,
                c >= 20
        )

    results <- contingency %>%
        mutate(
            test = mapply(function(a, b, c, d) {
            ft <- fisher.test(matrix(c(a, b, c, d), nrow = 2))
            list(
                odds_ratio = unname(ft$estimate),
                lower_ci   = ft$conf.int[1],
                upper_ci   = ft$conf.int[2],
                p_value    = ft$p.value
            )
            }, a, b, c, d, SIMPLIFY = FALSE)
        ) %>%
        tidyr::unnest_wider(test) %>%
        mutate(
            log2_or = log2(odds_ratio),
            p_adj = p.adjust(p_value, method = "fdr"),
            log_p = -log10(p_adj),
            enriched_post_covid = odds_ratio > 2 & is.finite(odds_ratio) & p_adj < 0.05,
            enriched_pre_covid = odds_ratio < 0.5 & is.finite(odds_ratio) & p_adj < 0.05,
            significant = p_adj < 0.05
        ) %>%
        left_join(map, by = "phecode") %>%
        arrange(desc(odds_ratio), p_adj)

    return(results)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.58ff31f6-450d-4c6a-a943-0f734eda8a18"),
    non_lsd_prep=Input(rid="ri.foundry.main.dataset.75cac421-ffed-4d84-b191-e3e6dfc7521e")
)
library(mice)
library(dplyr)
non_lsd_imputation <- function(non_lsd_prep) {

    # works with environment profile-high-driver-cores-and-memory-minimal
    set.seed(2048)
    
    df1 <- non_lsd_prep

    sub_df <- df1 %>% # df for imputation
        select(Age, Race, Sex, observation_period_before_covid, data_partner_id, BMI_before_or_day_of_covid, number_of_visits_before_covid, Binary_Severity) %>%
        mutate(
            Age = as.integer(Age),
            Race = as.factor(Race),
            Sex = as.factor(Sex),
            BMI_before_or_day_of_covid = as.numeric(BMI_before_or_day_of_covid),
            observation_period_before_covid = as.integer(observation_period_before_covid),
            number_of_visits_before_covid = as.integer(number_of_visits_before_covid),
            Binary_Severity = as.integer(Binary_Severity),
            data_partner_id = as.factor(data_partner_id)
        )
        
    # print proportion missing
    print(mean(is.na(sub_df$Age)))
    print(mean(is.na(sub_df$BMI_before_or_day_of_covid)))

    impute <- mice(sub_df, m=2, method = "pmm", maxit = 5, seed = 2048) # Impute age and BMI using predictive mean matching. m = number of imputed datasets, make this lower if you have OOM errors

    # return the imputed "mids" object to dataframe
    imp_df <- mice::complete(impute,2) # there is also a complete function in dplyr, so be careful here. select 2nd dataset

    df1$Age <- as.integer(imp_df$Age)
    df1$BMI_before_or_day_of_covid <- as.numeric(imp_df$BMI_before_or_day_of_covid)

    return(df1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.36089ecb-569e-4a93-a876-e024833c5cb5"),
    cohort_phecodes_2_1=Input(rid="ri.foundry.main.dataset.977b8ef2-7d67-414f-85e0-2f5524a772aa"),
    phecode_map=Input(rid="ri.foundry.main.dataset.c2bec0d5-a47e-420c-9f1f-1526771f00b7")
)
library(dplyr)
library(tidyr)
not_hosp_phecodes <- function( phecode_map, cohort_phecodes_2_1) {
    map <- phecode_map %>% select(phecode, phecode_string, `category`) %>% distinct()
     
     phe_df <- cohort_phecodes_2_1 %>%
        filter(Binary_Severity == 0, # not hosp 
           days_diff <= 15 & days_diff > 0, 
            observation_period_before_covid >= 365,
            observation_period_post_covid >= 365, 
            !grepl("^(GE_|CM_|NB_|PP_)", prefix)) %>%# only lsd patients, outside acute window, excluding COVID and genetic terms and patient must have sufficient history
        mutate(has_lsd = case_when(
            has_lsd == 1 ~ "has_lsd",
            has_lsd == 0 ~ "no_lsd"
        )) %>%
        group_by(person_id, phecode, covid_period) %>%
        slice_min(order_by = condition_start_date, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        distinct()
       
   totals <- phe_df %>%
        distinct(person_id, has_lsd) %>%
        count(has_lsd, name = "lsd_count") %>%
        pivot_wider(
            names_from = has_lsd,
            values_from = lsd_count,
            names_prefix = "total_"
        )

    contingency <- phe_df %>%
        distinct(person_id, phecode, has_lsd) %>%
        count(phecode, has_lsd, name = "phecode_count") %>%
        pivot_wider(
            names_from = has_lsd,
            values_from = phecode_count,
            values_fill = 0
        ) %>%
        crossing(totals) %>% 
        mutate(
            phecode,
            a = has_lsd,                 # with phecode & has_lsd
            b = total_has_lsd - a,       # has_lsd without phecode
            c = no_lsd,              # with phecode & no_lsd
            d = total_no_lsd - c,     # no_lsd without phecode
            .keep = "none"
        ) %>% dplyr::filter( # sanity check filter
                a >= 20,
                c >= 20
        )
            
    results <- contingency %>%
        mutate(
            test = mapply(function(a, b, c, d) {
            ft <- fisher.test(matrix(c(a, b, c, d), nrow = 2))
            list(
                odds_ratio = unname(ft$estimate),
                lower_ci   = ft$conf.int[1],
                upper_ci   = ft$conf.int[2],
                p_value    = ft$p.value
            )
            }, a, b, c, d, SIMPLIFY = FALSE)
        ) %>%
        tidyr::unnest_wider(test) %>%
        mutate(
            log2_or = log2(odds_ratio),
            p_adj = p.adjust(p_value, method = "fdr"),
            log_p = -log10(p_adj),
            enriched_lsd = odds_ratio > 2 & is.finite(odds_ratio) & p_adj < 0.05,
            enriched_control = odds_ratio < 0.5 & is.finite(odds_ratio) & p_adj < 0.05,
            significant = p_adj < 0.05
        ) %>%
        left_join(map, by = "phecode") %>%
        arrange(desc(odds_ratio), p_adj)

    return(results)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.38a5da21-811f-484a-84e7-3a27173c3bdc"),
    procedure_survival_prep=Input(rid="ri.foundry.main.dataset.0869ac49-322a-4e7b-82ad-31583d2c8964")
)
library(ggsurvfit)
library(survival) # survdiff
library(dplyr)
library(ggplot2) # requires ggplot 3.5.0 and R base 4.3.3
library(broom) # tidy
procedure_survival <- function(procedure_survival_prep) {
    
    df1 <- procedure_survival_prep
    
    counts <- df1 %>%
        distinct(person_id, has_lsd) %>%
        summarize(
            LSD = sum(has_lsd == 1),
            Control = sum(has_lsd == 0)
        )

    message(
    "Total survival cohort: ", counts$LSD + counts$Control,
    " (LSD: ", counts$LSD, 
    ", Controls: ", counts$Control, ")"
    )

    surv <- df1 %>%
        dplyr::select(
        person_id, days, status,
        procedure_name, has_lsd, subclass, elix_score, Binary_Severity
        ) %>%
        dplyr::mutate(
        procedure_name  = factor(procedure_name),
        has_lsd    = factor(ifelse(has_lsd == 1, "LD", "Control"),
                            levels = c("Control", "LD")),
        subclass   = factor(subclass),
        Binary_Severity = factor(Binary_Severity),
        elix_score = ifelse(is.na(elix_score), 0, elix_score),
        days       = as.numeric(days),
        status     = as.numeric(status)
        ) %>%
        dplyr::distinct()

    all_procedures <- setdiff(levels(surv$procedure_name), "none")

    results_list <- list()

    for (procedure in all_procedures) {

        message("\n=== Fitting survival model for procedure: ", procedure, " ===")

        # Use this procedure vs. no-procedure ("none")
        dat <- surv %>%
            mutate(procedure_exposed = ifelse(procedure_name == procedure, 1, 0)) %>%
            filter(procedure_name %in% c("none", procedure))

        # Cox PH model, adjusted for Elix, stratified by subclass
        fit <- coxph(
            Surv(days, status) ~
                procedure_exposed * has_lsd +
                elix_score +
                strata(subclass),
                data = dat
        )
        
        cat("Testing PH assumption:\n")
        print(cox.zph(fit))

        # Tidy HR table for this procedure
        hr_tbl <- tidy(fit, exponentiate = TRUE, conf.int = TRUE)
        hr_tbl$procedure <- procedure
        results_list[[procedure]] <- hr_tbl

    }

    final_results <- bind_rows(results_list)

    return(final_results)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0869ac49-322a-4e7b-82ad-31583d2c8964"),
    cohort2=Input(rid="ri.foundry.main.dataset.2be8aaea-6435-48ce-890d-d4441c292af7"),
    cohort_conditions_icd_2_1=Input(rid="ri.foundry.main.dataset.841ff505-990e-49a8-b5f6-27dbcfaac01f"),
    cohort_death=Input(rid="ri.foundry.main.dataset.6662c2bb-0b44-4f01-9285-e8be46bf9a91"),
    cohort_procedures=Input(rid="ri.foundry.main.dataset.667826d8-583c-4744-ba97-30b3a43c86d1")
)
library(dplyr)
library(comorbidity)
procedure_survival_prep <- function(cohort_death, cohort_procedures, cohort_conditions_icd_2_1, cohort2) {
    
    pre_covid <- cohort_conditions_icd_2_1 %>% # conditions before COVID
        filter(days_diff <= 0) 
    
    elix <- comorbidity(x = pre_covid, id = "person_id", code = "concept_code", map = "elixhauser_icd10_quan", assign0 = TRUE) # don't double count assign0 = TRUE

    elix_score <- score(x = elix, weights = "swiss", assign0 = TRUE)
    elix$elix_score <- elix_score

    latest_procedure <- cohort_procedures %>%
        select(person_id, procedure_concept_id, procedure_concept_name, procedure_name, days_diff, procedure_date) %>%
        filter(between(days_diff, 1, 15)) %>%
        group_by(person_id, procedure_name) %>%
        slice_min(order_by=procedure_date, n=1, with_ties = FALSE) %>%
        ungroup()

    w_cohort <- cohort2 %>%
        select(person_id, subclass, has_lsd, Severity_Type, Binary_Severity, COVID_first_poslab_or_diagnosis_date) %>%
        left_join(latest_procedure, by=join_by(person_id)) %>% 
        left_join(elix %>% select(person_id, elix_score), by=join_by(person_id)) %>%
        mutate(
            procedure_name = coalesce(procedure_name, "none"),
            procedure_before_landmark = if_else(is.na(procedure_concept_id), 0, 1),
            landmark_date = COVID_first_poslab_or_diagnosis_date + 15
        )

    censor_date <- as.Date("2025-07-11")

    survival_df <- w_cohort %>%
        left_join(cohort_death %>% select(person_id, death_date), by=join_by(person_id)) %>%
        mutate(
            days = if_else(
                !is.na(death_date),
                as.integer(death_date - landmark_date),
                as.integer(censor_date - landmark_date)
            )
        ) %>%
        filter(days >= 0) %>% # death must be on/after landmark to avoid immortal time bias
        mutate(status = if_else(!is.na(death_date), 1, 0))

    
    return(survival_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.06410c6e-3e6b-4dbe-9ef4-76e151fabb22"),
    cohort2=Input(rid="ri.foundry.main.dataset.2be8aaea-6435-48ce-890d-d4441c292af7"),
    cohort_conditions_icd_2_1=Input(rid="ri.foundry.main.dataset.841ff505-990e-49a8-b5f6-27dbcfaac01f")
)
library(purrr)
library(comorbidity)
library(dplyr)
screen_mediators <- function(cohort2,
                             cohort_conditions_icd_2_1) {
  
    # pre-COVID conditions
    pre_covid <- cohort_conditions_icd_2_1 %>%
        dplyr::filter(days_diff <= 0)
    
    # Elixhauser comorbidities (wide)
    elix <- comorbidity(
        x       = pre_covid,
        id      = "person_id",
        code    = "concept_code",
        map     = "elixhauser_icd10_quan",
        assign0 = TRUE
    )
    
    # main analysis df
    input_df <- cohort2 %>%
        dplyr::left_join(elix, by = "person_id") %>%
        dplyr::mutate(
        dplyr::across(
            -c(person_id, postal_code),
            ~ tidyr::replace_na(., 0)
        ),
        has_lsd         = as.numeric(has_lsd),
        Binary_Severity = as.numeric(Binary_Severity)
        )
    
    # LSD subtype sets (must exist in env)
    concept_list <- list(
        sphingo = sphingo,
        lipid   = lipid,
        fabry   = fabry,
        gaucher = gaucher,
        metak   = metak,
        amino   = amino,
        gang    = gang,
        cys     = cys,
        ncl     = ncl
    )
    
    # candidate comorbidity mediators
    all_mediators <- c(
        "chf","carit","valv","pcd","pvd","hypunc","hypc","para","ond","cpd",
        "diabunc","diabc","hypothy","rf","ld","pud","aids","lymph","metacanc",
        "solidtum","rheumd","coag","obes","wloss","fed","blane","dane","alcohol",
        "drug","psycho","depre"
    )
    
    # build per-LSD subgroup datasets
    df_list <- purrr::imap(concept_list, function(cond_vec, nm) {
        split_df(input_df, cond_vec) %>%
        dplyr::mutate(
            !!rlang::sym(nm) := ifelse(lsd_condition_name %in% cond_vec, 1, 0)
        )
    })
    
    # screen comorbidities associated with one LSD subtype
    screen_one <- function(df, exposure, p_thresh = 0.001) {
        
        # keep only mediators that exist as columns and have at least 5 patients
        meds_in_data <- intersect(all_mediators, names(df))
        meds_in_data <- meds_in_data[
            purrr::map_lgl(meds_in_data, ~ sum(df[[.x]] == 1, na.rm = TRUE) >= 5)
        ]
        
        if (length(meds_in_data) == 0 ||
            length(unique(df[[exposure]])) < 2) {
        return(tibble::tibble(
            term = NA_character_,
            estimate = NA_real_,
            std_error = NA_real_,
            statistic = NA_real_,
            p_value = NA_real_,
            conf_low = NA_real_,
            conf_high = NA_real_,
            ld_name = exposure
        ))
        }
        
        form <- as.formula(
        paste(
            exposure, "~",
            paste(meds_in_data, collapse = " + ")
        )
        )
        
        fit <- glm(form, family = binomial, data = df)
        
        sig_tab <- broom::tidy(fit, exponentiate=TRUE, conf.int=TRUE) %>%
            dplyr::filter(term %in% meds_in_data) %>%
            mutate(ld_name = exposure)
        
        sig_tab
    }
    
    # run over all LSD groups and row-bind results
    result <- purrr::imap_dfr(df_list, ~ screen_one(.x, exposure = .y))
    
    return(result)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3500942a-19c9-4245-84fe-67d90a2cb0e4"),
    all_lsds=Input(rid="ri.foundry.main.dataset.9e3b0f7e-de97-43e9-a9d9-178708a1ba89"),
    lsd_concept_relationship=Input(rid="ri.foundry.main.dataset.d31ab264-559b-46fb-b036-ec1c279e405a")
)
library(dplyr)
specific_lsds <- function(all_lsds, lsd_concept_relationship) {
   
    concepts <- lsd_concept_relationship
    patients <- all_lsds

    concepts <- concepts %>%
        dplyr::filter(ancestor_concept_id == 4053270 | ancestor_concept_id == 37155637) # Keep only Disorder of lysosomal enzyme: 4053270 and Lysosomal storage disease: 37155637

    conditions <- concepts %>%
        dplyr::inner_join(patients, by = join_by(descendant_concept_id == condition_concept_id), keep = TRUE) %>%
        dplyr::group_by(person_id) %>% 
        dplyr::slice_max(max_levels_of_separation, with_ties = TRUE) %>%  # Select most specific diagnosis based on graph structure (distance from root parent term)
        dplyr::slice_max(condition_start_date, with_ties = FALSE) %>% # only select max (most recent) date if there are ties in the graph structure
        dplyr::select(person_id,condition_concept_id,condition_source_concept_id,condition_source_value,descendant_name,condition_start_date,condition_end_date,data_partner_id) %>%
        rename(specific_condition_name = descendant_name) %>%
        dplyr::ungroup() #%>% add after group_by(person_id) to find the people with more than one, or more conditions

    return(conditions)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.cb80cafc-4f49-423a-88dd-7e071f91cfce"),
    cohort2=Input(rid="ri.foundry.main.dataset.2be8aaea-6435-48ce-890d-d4441c292af7"),
    cohort_elix_2_1=Input(rid="ri.foundry.main.dataset.08f2bdf8-c72c-4267-af87-b2658a778c60")
)
library(gtsummary)
library(tidyr)
library(dplyr)
library(stringr)
library(tibble)
table_1 <- function( cohort_elix_2_1, cohort2) {

    co <- cohort2
    elix <- cohort_elix_2_1 %>%
        dplyr::select(
            -Binary_Severity,
            -has_lsd,
            -utilization,
            -weights
        )
    cohort_df <- co %>%
        dplyr::select(person_id,
        Severity_Type,
        observation_period_before_covid,
        observation_period_post_covid,
        number_of_visits_before_covid,
        number_of_visits_post_covid,
        BMI_before_or_day_of_covid,
        had_reinfection_post_covid,
        COVID_vaccine_doses_before_or_day_of_covid,
        Long_COVID_any_indicator,
        lsd_condition_name,
        has_lsd,
        Binary_Severity,
        Race,
        Age,
        Sex)

    comorbid <- cohort_df %>%
         left_join(elix, by = "person_id") %>%
         mutate(across(
            -c(person_id, elix_score),  # all columns except elix_score and person_id replaced with zero
            ~ replace_na(., 0)
        ))

    atlantic <- tribble(
        ~district, ~zip3_codes, ~region,
        "CONNECTICUT", "060, 061, 062, 063, 064, 065, 066, 067, 068, 069", "ATLANTIC",
        "DE-PA2", "180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199", "ATLANTIC",
        "MA-RI", "010, 011, 012, 013, 014, 015, 016, 017, 018, 019, 020, 021, 022, 023, 024, 025, 026, 027, 028, 029, 055", "ATLANTIC",
        "MARYLAND", "200, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 214, 215, 216, 217, 218, 219", "ATLANTIC",
        "ME-NH-VT", "030, 031, 032, 033, 034, 035, 036, 037, 038, 039, 040, 041, 042, 043, 044, 045, 046, 047, 048, 049, 050, 051, 052, 053, 054, 056, 057, 058, 059", "ATLANTIC",
        "NEW JERSEY", "070, 071, 072, 073, 074, 075, 076, 077, 078, 079, 080, 081, 082, 083, 084, 085, 086, 087, 088, 089", "ATLANTIC",
        "NEW YORK 1", "100, 101, 102, 103, 104, 112", "ATLANTIC",
        "NEW YORK 2", "005, 110, 111, 113, 114, 115, 116, 117, 118, 119", "ATLANTIC",
        "NEW YORK 3", "105, 106, 107, 108, 109, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149", "ATLANTIC",
        "NORTH CAROLINA", "270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, 283, 284, 285, 286, 287, 288, 289", "ATLANTIC",
        "PENNSYLVANIA 1", "150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179", "ATLANTIC",
        "VIRGINIA", "201, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246", "ATLANTIC"
    )

    central <- tribble(
        ~district, ~zip3_codes, ~region,
        "IA-NE-SD", "500, 501, 502, 503, 504, 505, 506, 507, 508, 509, 510, 511, 512, 513, 514, 515, 516, 520, 521, 522, 523, 524, 525, 526, 527, 528, 570, 571, 572, 573, 574, 575, 576, 577, 680, 681, 683, 684, 685, 686, 687, 688, 689, 690, 691, 692, 693", "CENTRAL",
  "ILLINOIS 1", "600, 601, 602, 603, 606, 607, 608, 610, 611", "CENTRAL",
  "ILLINOIS 2", "604, 605, 609, 612, 613, 614, 615, 616, 617, 618, 619, 620, 622, 623, 624, 625, 626, 627, 628, 629", "CENTRAL",
  "INDIANA", "460, 461, 462, 463, 464, 465, 466, 467, 468, 469, 470, 471, 472, 473, 474, 475, 476, 477, 478, 479", "CENTRAL",
  "KS-MO", "630, 631, 633, 634, 635, 636, 637, 638, 639, 640, 641, 644, 645, 646, 647, 648, 649, 650, 651, 652, 653, 654, 655, 656, 657, 658, 660, 661, 662, 664, 665, 666, 667, 668, 669, 670, 671, 672, 673, 674, 675, 676, 677, 678, 679", "CENTRAL",
  "KY-WV", "247, 248, 249, 250, 251, 252, 253, 254, 255, 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 267, 268, 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416, 417, 418, 420, 421, 422, 423, 424, 425, 426, 427", "CENTRAL",
  "MICHIGAN 1", "480, 481, 482, 483, 484, 485, 492", "CENTRAL",
  "MICHIGAN 2", "486, 487, 488, 489, 490, 491, 493, 494, 495, 496, 497, 498, 499", "CENTRAL",
  "MN-ND", "550, 551, 553, 554, 555, 556, 557, 558, 559, 560, 561, 562, 563, 564, 565, 566, 567, 580, 581, 582, 583, 584, 585, 586, 587, 588", "CENTRAL",
  "OHIO 1", "434, 435, 436, 439, 440, 441, 442, 443, 444, 445, 446, 447, 448, 449, 458", "CENTRAL",
  "OHIO 2", "430, 431, 432, 433, 437, 438, 450, 451, 452, 453, 454, 455, 456, 457, 459", "CENTRAL",
  "WISCONSIN", "530, 531, 532, 534, 535, 537, 538, 539, 540, 541, 542, 543, 544, 545, 546, 547, 548, 549", "CENTRAL"
    )

    southern <- tribble(
    ~district, ~zip3_codes, ~region,
    "AL-MS", "350, 351, 352, 354, 355, 356, 357, 358, 359, 360, 361, 362, 363, 364, 365, 366, 367, 368, 369, 386, 387, 388, 389, 390, 391, 392, 393, 394, 395, 396, 397", "SOUTHERN",
    "AR-OK", "716, 717, 718, 719, 720, 721, 722, 723, 724, 725, 726, 727, 728, 729, 730, 731, 734, 735, 736, 737, 738, 739, 740, 741, 743, 744, 745, 746, 747, 748, 749", "SOUTHERN",
    "FLORIDA 1", "320, 321, 322, 323, 324, 325, 326, 327, 344", "SOUTHERN",
    "FLORIDA 2", "328, 329, 335, 336, 337, 338, 339, 341, 342, 346, 347", "SOUTHERN",
    "FLORIDA 3", "330, 331, 332, 333, 334, 349", "SOUTHERN",
    "GEORGIA", "300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317, 318, 319, 398, 399", "SOUTHERN",
    "LOUISIANA", "700, 701, 703, 704, 705, 706, 707, 708, 710, 711, 712, 713, 714", "SOUTHERN",
    "PUERTO RICO", "006, 007, 008, 009", "SOUTHERN",
    "SOUTH CAROLINA", "290, 291, 292, 293, 294, 295, 296, 297, 298, 299", "SOUTHERN",
    "TENNESSEE", "370, 371, 372, 373, 374, 375, 376, 377, 378, 379, 380, 381, 382, 383, 384, 385", "SOUTHERN",
    "TEXAS 1", "750, 751, 752, 753, 754, 755, 756, 757, 758, 759, 760, 761, 762, 763, 764, 766, 767", "SOUTHERN",
    "TEXAS 2", "770, 772, 773, 774, 775, 776, 777, 778, 779, 783, 784, 785", "SOUTHERN",
    "TEXAS 3", "733, 765, 768, 769, 780, 781, 782, 786, 787, 788, 789, 790, 791, 792, 793, 794, 795, 796, 797, 798, 799, 885", "SOUTHERN"
    )

    western_pacific <- tribble(
    ~district, ~zip3_codes, ~region,
    "ALASKA", "995, 996, 997, 998, 999", "WESTERN-PACIFIC",
    "AZ-NM", "850, 851, 852, 853, 855, 856, 857, 859, 860, 863, 864, 865, 870, 871, 873, 874, 875, 876, 877, 878, 879, 880, 881, 882, 883, 884", "WESTERN-PACIFIC",
    "CALIFORNIA 1", "940, 941, 943, 944, 949, 950, 951, 954, 955, 959, 960", "WESTERN-PACIFIC",
    "CALIFORNIA 2", "942, 945, 946, 947, 948, 952, 956, 957, 958, 961", "WESTERN-PACIFIC",
    "CALIFORNIA 3", "913, 914, 915, 916, 930, 931, 932, 933, 934, 935, 936, 937, 938, 939, 953", "WESTERN-PACIFIC",
    "CALIFORNIA 4", "910, 911, 912, 917, 918, 926, 927, 928", "WESTERN-PACIFIC",
    "CALIFORNIA 5", "900, 901, 902, 903, 904, 905, 906, 907, 908", "WESTERN-PACIFIC",
    "CALIFORNIA 6", "919, 920, 921, 922, 923, 924, 925", "WESTERN-PACIFIC",
    "CO-WY", "800, 801, 802, 803, 804, 805, 806, 807, 808, 809, 810, 811, 812, 813, 814, 815, 816, 820, 821, 822, 823, 824, 825, 826, 827, 828, 829, 830, 831","WESTERN-PACIFIC",
    "HAWAII", "967, 968, 969", "WESTERN-PACIFIC",
    "ID-MT-OR", "590, 591, 592, 593, 594, 595, 596, 597, 598, 599, 832, 833, 834, 835, 836, 837, 838, 970, 971, 972, 973, 974, 975, 976, 977, 978, 979", "WESTERN-PACIFIC",
    "NV-UT", "840, 841, 842, 843, 844, 845, 846, 847, 889, 890, 891, 893, 894, 895, 897, 898", "WESTERN-PACIFIC",
    "WASHINGTON", "980, 981, 982, 983, 984, 985, 986, 988, 989, 990, 991, 992, 993, 994", "WESTERN-PACIFIC"
    )

    all_zip3 <- bind_rows(atlantic, central, southern, western_pacific) %>%
        separate_rows(zip3_codes, sep = ",\\s*") %>%
        rename(zip3 = zip3_codes) %>%
        select(zip3, region)

    co_zips <- co %>%
        left_join(all_zip3, join_by(postal_code == zip3)) %>%
        mutate(region = replace_na(region, "UNKNOWN")) %>%
        select(person_id, region)

   # lsds <- c("None", "Lipid storage disease", "Fabry's disease", "Metachromatic leukodystrophy")
    
    df <- comorbid %>%
        inner_join(co_zips, by = "person_id") %>%
        mutate(
                Sex = factor(Sex, levels = c('Female', 'Male', 'Unknown')),
                Binary_Severity = case_when(
                    Binary_Severity == 0 ~ "Not Hospitalized",
                    Binary_Severity == 1 ~ "Hospitalized"
                ),
                Age = as.integer(Age), 
                Severity_Type = case_when(
                    Severity_Type == "Death_within_n_days_after_COVID_index" ~ "Death",
                    Severity_Type == "Mild_ED_around_strong_signal_COVID_index" ~ "Mild ED",
                    Severity_Type == "Mild_ED_around_weak_signal_COVID_index" ~ "Mild ED",
                    Severity_Type == "Mild_No_ED_or_Hosp_around_COVID_index" ~ "Mild No-ED",
                    Severity_Type == "Moderate_Hosp_around_strong_signal_COVID_index" ~ "Moderate",
                    Severity_Type == "Moderate_Hosp_around_weak_signal_COVID_index" ~ "Moderate",
                    Severity_Type == "Severe_ECMO_IMV_in_Hosp_around_strong_signal_COVID_index" ~ "Severe",
                    Severity_Type == "Severe_ECMO_IMV_in_Hosp_around_weak_signal_COVID_index" ~ "Severe"
                ),
                Race = case_when(
                    Race == "Unknown" | Race == "Other/Unknown" | Race == "Native Hawaiian or Other Pacific Islander" ~ "Other/Unknown",
                    TRUE ~ Race
                ),
                Race = factor(Race, levels = c("White",
                    "Black or African American",
                    "Asian",
                    "American Indian or Alaska Native",
                    "Other/Unknown")),
                has_lsd = case_when(
                    has_lsd == 0 ~ "Control",
                    has_lsd == 1 ~ "LD"
                ),
                group = paste(has_lsd, Severity_Type, sep = "\n"))
    
    # 1) Define your comorbidity columns (same set you used in tbl2)
    comorb_cols <- c(
    "chf","carit","valv","pcd","pvd","hypunc","hypc","para","ond","cpd",
    "diabunc","diabc","hypothy","rf","ld","pud","aids","lymph","metacanc",
    "solidtum","rheumd","coag","obes","wloss","fed","blane","dane","alcohol",
    "drug","psycho","depre"
    )

    df2 <- df %>%
        mutate(
            has_lsd = case_when(
            has_lsd %in% c(1, "has_lsd", "LD", "ld") ~ 1,
            TRUE ~ 0
            )
        )

    # Prevalence by (severity group × LD status) for each comorbidity
    prev_df <- df2 %>%
    select(group, has_lsd, all_of(comorb_cols)) %>%
    pivot_longer(
        cols = all_of(comorb_cols),
        names_to = "comorbidity",
        values_to = "present"
    ) %>%
    group_by(group, has_lsd, comorbidity) %>%
    summarise(
        n = n(),
        cases = sum(present == 1, na.rm = TRUE),
        prev = 100 * cases / n,               # prevalence (%)
        .groups = "drop"
    ) #%>%  print(., n = Inf, width = Inf) 

    # LD vs control gaps within each severity for each comorbidity
    delta_df <- prev_df %>%
    mutate(has_lsd = ifelse(has_lsd == 1, "has_lsd", "no_lsd"),
    severity = case_when(
        str_detect(group, "Death") ~ "Death",
        str_detect(group, "Mild No-ED") ~ "Mild No-ED",
        str_detect(group, "Mild ED") ~ "Mild ED",
        str_detect(group, "Moderate") ~ "Moderate",
        str_detect(group, "Severe") ~ "Severe"
    )) %>%
    select(severity, has_lsd, comorbidity, prev) %>%
    pivot_wider(
        names_from = has_lsd,
        values_from = prev
    ) %>%
    mutate(
        delta_signed = has_lsd - no_lsd,         # LD – Control (signed difference, in % points)
        delta_abs    = abs(delta_signed),            # absolute gap (|LD – Control|)
        vs_string    = sprintf("%.1f%% vs %.1f%%", has_lsd, no_lsd)
    ) %>%
     print(., n = Inf, width = Inf) 

    # Which severity shows the largest overall differences?
    severity_summary <- delta_df %>%
    group_by(severity) %>%
    summarise(
        median_delta = median(delta_abs, na.rm = TRUE),
        mean_delta   = mean(delta_abs,   na.rm = TRUE),
        max_delta    = max(delta_abs,    na.rm = TRUE),
        .groups = "drop"
    ) %>%
    arrange(desc(median_delta)) %>% print(., n = Inf, width = Inf) 

    # Top comorbidity gaps per severity for quick narration
    top_diffs_per_group <- delta_df %>%
    group_by(severity) %>%
    slice_max(order_by = delta_abs, n = 5, with_ties = FALSE) %>%
    ungroup() %>% arrange(desc(delta_abs)) %>% print(., n = Inf, width = Inf) 

    rownames(df) <- df$person_id
    df$person_id <- NULL
    df$lsd_condition_name <- NULL

    tbl1 <- df %>% select(
        -group,
        -chf,
        -carit,
        -valv,
        -pcd,
        -pvd,
        -hypunc,
        -hypc,
        -para,
        -ond,
        -cpd,
        -diabunc,
        -diabc,
        -hypothy,
        -rf,
        -ld,
        -pud,
        -aids,
        -lymph,
        -metacanc,
        -solidtum,
        -rheumd,
        -coag,
        -obes,
        -wloss,
        -fed,
        -blane,
        -dane,
        -alcohol,
        -drug,
        -psycho,
        -depre
    ) %>% tbl_summary(by=has_lsd)

    tbl1_df <- as_tibble(tbl1, col_labels = FALSE) %>%
       rename(no_lsd = stat_1, has_lsd = stat_2)

    col_labels1 <- names(as_tibble(tbl1, col_labels = TRUE))
    
    tbl1_df <- tibble::as_tibble(
        rbind(
        col_labels1,
        as.matrix(tbl1_df)
        )
    )

    tbl2 <- df %>% 
        select(
            chf,
            carit,
            valv,
            pcd,
            pvd,
            hypunc,
            hypc,
            para,
            ond,
            cpd,
            diabunc,
            diabc,
            hypothy,
            rf,
            ld,
            pud,
            aids,
            lymph,
            metacanc,
            solidtum,
            rheumd,
            coag,
            obes,
            wloss,
            fed,
            blane,
            dane,
            alcohol,
            drug,
            psycho,
            depre,
            elix_score,
            group
        ) %>%
        tbl_summary(by = group, statistic = all_categorical() ~
    "{n} ({p}%)")

    tbl2_df <- as_tibble(tbl2, col_labels = FALSE)
    col_labels2 <- names(as_tibble(tbl2, col_labels = TRUE))

    # Extract n_total per column
    n_totals <- stringr::str_extract(gsub(",", "", gsub("\\s*=\\s*", "=", col_labels2)), "(?<=N=)\\d+") %>% na.omit() %>% as.numeric()

    tbl2 <- tbl2 %>%
    modify_table_body(
        ~ {
        .x %>%
            mutate(across(
            starts_with("stat_"),
            ~ {
                col_idx <- as.numeric(sub("stat_", "", cur_column()))
                n_total <- n_totals[col_idx]
                
                is_match <- grepl("^([0-9,]+) \\([0-9.]+%\\)$", .)
                cell_val <- as.numeric(gsub(",", "", sub(" .*", "", .)))

                fmt_pct <- function(x) {
                    paste0(signif(100 * x / n_total, 2), "%")
                }
                
                ifelse(
                !is_match, .,
                ifelse(
                    cell_val < 20,  paste0("<20 (<", fmt_pct(20), ")"),
                    ifelse(
                    (n_total - cell_val) < 20, paste0(">", n_total - 20, " (>", fmt_pct(n_total - 20), ")"), .
                    )
                )
                )
            }
            )) %>%
            filter(!(variable == "elix_score" & label == "Unknown"))
        }
    )
    tbl2_df <- as_tibble(tbl2, col_labels = FALSE)
    
    tbl2_df <- as_tibble(
        rbind(
            col_labels2,
            as.matrix(tbl2_df)
        )
    ) %>%
        rename(
            label1 = label,
            no_lsd_death = stat_1,
            no_lsd_mild_ed = stat_2,
            no_lsd_mild = stat_3,
            no_lsd_moderate = stat_4,
            no_lsd_severe = stat_5,
            has_lsd_death = stat_6,
            has_lsd_mild_ed = stat_7,
            has_lsd_mild = stat_8,
            has_lsd_moderate = stat_9,
            has_lsd_severe = stat_10
        )

    # return column binded tables
    final_df <- cbind_na(tbl1_df, tbl2_df, severity_summary)

    return(final_df)

}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f1d9eb3c-e8d8-4c06-b2f4-ddbf97954d03"),
    drug_exposure_survival_prep=Input(rid="ri.foundry.main.dataset.4a6a24f9-9d89-4293-bede-4e0ea1511a80")
)
library(dplyr)
library(gtsummary)
table_drugs <- function(drug_exposure_survival_prep) {
    
    df <- drug_exposure_survival_prep %>%
        mutate(has_lsd = case_when(
            has_lsd == 0 ~ "Control",
            has_lsd == 1 ~ "LD"
        )) %>%
        select(person_id, drug_name, has_lsd) %>%
        distinct()  # very important otherwise count is inflated

    table1 <- df %>% select(drug_name, has_lsd) %>% 
            tbl_summary(by=has_lsd,
            statistic = all_categorical() ~ c("{n}"))
            
    print(table1)

    table_df <- as_tibble(table1, col_labels = FALSE) %>%
        rename(no_lsd = stat_1, has_lsd = stat_2)

    return(table_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6bbcab81-530a-4852-a107-02111d80ee88"),
    procedure_survival_prep=Input(rid="ri.foundry.main.dataset.0869ac49-322a-4e7b-82ad-31583d2c8964")
)
library(dplyr)
library(gtsummary)
table_procedures <- function(procedure_survival_prep) {
    
    df <- procedure_survival_prep %>% 
        mutate(has_lsd = case_when(
            has_lsd == 0 ~ "Control",
            has_lsd == 1 ~ "LD"
        )) %>%
        select(person_id, procedure_name, has_lsd) %>%
        distinct()  # very important otherwise count is inflated

    table1 <- df %>% select(procedure_name, has_lsd) %>%
        tbl_summary(by=has_lsd,
        statistic = all_categorical() ~ c("{n}"))
            
    print(table1)

    table_df <- as_tibble(table1, col_labels = FALSE) %>%
        rename(no_lsd = stat_1, has_lsd = stat_2)

    return(table_df)
}

