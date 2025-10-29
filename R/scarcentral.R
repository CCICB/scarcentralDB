#' Generate the full dataset underlying the scarcentral database
#'
#' This function constructs and returns a dataset that represents the complete set
#' of tumorigenic processes and their associated scars for the scarcentral database.
#' The dataset is structured as a list of `process` objects, each of which contains a list of
#' `scar` objects linked to the process. The scars include details such as the tumorigenic process
#' that generates them, associated diseases, biological modalities, specificity, and sensitivity metrics.
#'
#' The scarcentral database is designed to provide a comprehensive resource on mutation signatures
#' or scars left by different tumorigenic processes (e.g., radiation therapy). It includes metadata such as
#' research experiments that identified the scars, the biological significance of each scar,
#' and links to related publications and tools.
#'
#' @return A list of `process` objects, each representing a tumorigenic process
#' and its associated multiomic `scars`
#'
#' @export
#'
#' @examples
#' # Generate the full scarcentral dataset
#' dataset <- scarcentral()
#'
#' # Access the first process in the dataset
#' process_1 <- dataset[[1]]
#'
#' # View the scars associated with the first process
#' process_1$scars
scarcentral <- function(){
  list(

    # ======= Radiotherapy ========
    new_process(
      name = "Radiotherapy",
      description = "Damage caused by ionizing radiation therapy, typically seen in post-treatment tumors.",
      class = "exogenous",
      icon = "faRadiation",
      flip_vertical = FALSE,
      flip_horizontal = TRUE,
      scars = list(
        new_scar(
          scar_name = "High Indel Burden",
          process = "Radiotherapy",
          marker_of = "Radiation Induced Damage",
          disease_specificity = "Pan-Cancer",
          diseases = "cancer",
          modality = "DNA",
          measurement = "INDEL / SBS ratio",
          description = "Higher number of INDELS. Driven by an increased burden of deletions, not insertions.",
          experiment = "Analysis of 12 radiation-associated tumours from 3 cancer types compared to radiation-naive tumours",
          paper_url = "https://www.nature.com/articles/ncomms12605",
          specificity = "Low",
          specificity_tooltip = "Also observed in BRCA1/BRCA2 breast cancer",
          sensitivity = "Moderate",
          sensitivity_tooltip = "All radiation tumours had values exceeding the median for their cancer type",
          tool_name = "raDNA",
          tool_url = "https://github.com/selkamand/radna"
        ),
        new_scar(
          scar_name = "High Deletion Burden",
          process = "Radiotherapy",
          marker_of = "Radiation Induced Damage",
          disease_specificity = "Pan-Cancer",
          diseases = "cancer",
          modality = "DNA",
          measurement = "Deletion / Insertion ratio",
          description = "Higher number of small deletions (1-100bp) particularly in regions of microhomology.",
          experiment = "Analysis of 12 radiation-associated tumours from 3 cancer types compared to radiation-naive tumours",
          paper_url = "https://www.nature.com/articles/ncomms12605",
          specificity = "Low",
          specificity_tooltip = "Also observed in BRCA1/BRCA2 breast cancer",
          sensitivity = "Moderate",
          sensitivity_tooltip = "11/12 tumours had values exceeding the median for their cancer type",
          tool_name = "raDNA",
          tool_url = "https://github.com/selkamand/radna"
        ),
        new_scar(
          scar_name = "Topography Agnostic Deletions",
          process = "Radiotherapy",
          marker_of = "Radiation Induced Damage",
          disease_specificity = "Pan-Cancer",
          diseases = "cancer",
          modality = "DNA",
          measurement = "Genome-wide deletion distribution and topographical features",
          description = "Radiation induced deletions appear uniformly across the genome regardless of replication timing and chromatin structure.",
          experiment = "Analysis of 12 radiation-associated tumours from 3 cancer types compared to radiation-naive tumours",
          paper_url = "https://www.nature.com/articles/ncomms12605",
          specificity = "High",
          specificity_tooltip = "No other mutagenic process has this level of indifference to DNA topography",
          sensitivity = "Unknown",
          sensitivity_tooltip = "",
          tool_name = "raDNA",
          tool_url = "https://github.com/selkamand/radna"
        ),
        new_scar(
          scar_name = "Increased Balanced Inversions",
          process = "Radiotherapy",
          marker_of = "Radiation Induced Damage",
          disease_specificity = "Pan-Cancer",
          diseases = "cancer",
          modality = "DNA",
          measurement = "Any balanced inversions present",
          description = "Balanced inversions, a rare type of rearrangement, were present in 92% (11/12) radiation-associated tumours but only 15% of radiation naive tumours.",
          experiment = "Analysis of 12 radiation-associated tumours from 3 cancer types compared to radiation-naive tumours",
          paper_url = "https://www.nature.com/articles/ncomms12605",
          specificity = "Low",
          specificity_tooltip = "Present in 58% of BRCA1/2-deficient breast tumours",
          sensitivity = "High",
          sensitivity_tooltip = "Present in 92% (11/12) radiation-associated tumours but only 15% of radiation-naive tumours",
          tool_name = "raDNA",
          tool_url = "https://github.com/selkamand/radna"
        ),
        new_scar(
          scar_name = "Clonal Deletions",
          process = "Radiotherapy",
          marker_of = "Radiation Induced Cancer",
          disease_specificity = "Pan-Cancer",
          diseases = "cancer",
          modality = "DNA",
          measurement = "Clonal/Subclonal ratio of Deletions to Insertions",
          description = "Radiation Induced Cancers have early and clonal radiation-induced mutations.",
          experiment = "Analysis of 12 radiation-associated tumours from 3 cancer types compared to radiation-naive tumours",
          paper_url = "https://www.nature.com/articles/ncomms12605",
          specificity = "Low",
          specificity_tooltip = "Clonal radiation-induced mutations are not unique to non-radiation induced cancer.",
          sensitivity = "High",
          sensitivity_tooltip = "Deletions were significantly increased compared with insertions amongst clonal mutations.",
          tool_name = "raDNA",
          tool_url = "https://github.com/selkamand/radna"
        ),
        new_scar(
          scar_name = "Glioblastoma Expression Signature",
          process = "Radiotherapy",
          marker_of = "Radiation Induced Glioblastoma",
          disease_specificity = "Disease Specific",
          diseases = "glioblastoma",
          modality = "RNA",
          measurement = "Proportion of RIG-associated genes overexpressed",
          description = "Paediatric glioblastomas were shown to have a pattern of expression distinct from those that spontaneously arise.",
          experiment = "Gene expression microarray profiling of 5 paediatric radiation-induced glioblastomas compared to spontaneous tumours",
          paper_url = "https://doi.org/10.1097/nen.0b013e3181257190",
          specificity = "Unknown",
          specificity_tooltip = "Yet to be systematically evaluated",
          sensitivity = "Unknown",
          sensitivity_tooltip = "Yet to be systematically evaluated",
          tool_name = "RIG",
          tool_url = "https://github.com/CCICB/rig"
        )
      )
    ),
    # ======= Methyltransferase Dysfunction ========
    new_process(
      name = "Methyltransferase Dysfunction",
      description = "Global methylation disrupted by DNMT3A dysfunction. Common in Leukaemia.",
      class = "endogenous",
      icon = "faTimeline",
      flip_vertical = FALSE,
      flip_horizontal = FALSE,
      scars = list(
        new_scar(
          scar_name = "CpG Flanking Sequence Preferences",
          process = "Methyltransferase Dysfunction",
          marker_of = "DNMT3A R88H",
          disease_specificity = "Pan-Cancer",
          diseases = "cancer",
          modality = "Methylation",
          measurement = "Proportion of methylated CpGs in R882/WT preferred contexts",
          description = "DNMT3A R882H and similar mutations change the CpG flanking sequence preferences.",
          experiment = "Libraries containing CpG in 10nt random context were exposed to mutant/WT methyltransferases then bisulfite sequenced.",
          paper_url = "https://doi.org/10.1093/nar/gkz911",
          specificity = "Unknown",
          specificity_tooltip = "Yet to be systematically evaluated",
          sensitivity = "Unknown",
          sensitivity_tooltip = "Yet to be systematically evaluated",
          tool_url = NULL,
          tool_name = NULL
        ),
        new_scar(
          scar_name = "Hemimethylation Burden",
          process = "Methyltransferase Dysfunction",
          marker_of = "Low DNMT1 activity",
          disease_specificity = "Pan-Cancer",
          diseases = "cancer",
          modality = "Methylation",
          measurement = "Proportion of CpGs which are hemimethylated",
          description = "DNMT1 maintenance methyltransferase should decrease hemimethylation.",
          experiment = "",
          paper_url = "",
          specificity = "Unknown",
          specificity_tooltip = "Yet to be systematically evaluated",
          sensitivity = "Unknown",
          sensitivity_tooltip = "Yet to be systematically evaluated",
          tool_url = NULL,
          tool_name = NULL
        )
      )
    ),
    # ======= Mismatch Repair Deficiency ========
    new_process(
      name = "Mismatch Repair Deficiency",
      description = "Failure to repair small mutations leads to their accumulation in the tumour genome",
      class = "endogenous",
      icon = "faShieldHalved",
      flip_vertical = FALSE,
      flip_horizontal = FALSE,
      scars = list(
        # new_scar(
        #   scar_name = "Hemimethylation Burden",
        #   process = "Methyltransferase Dysfunction",
        #   marker_of = "Low DNMT1 activity",
        #   disease_specificity = "Pan-Cancer",
        #   diseases = "cancer",
        #   modality = "Methylation",
        #   measurement = "Proportion of CpGs which are hemimethylated",
        #   description = "DNMT1 maintenance methyltransferase should decrease hemimethylation.",
        #   experiment = "",
        #   paper_url = "",
        #   specificity = "Unknown",
        #   specificity_tooltip = "Yet to be systematically evaluated",
        #   sensitivity = "Unknown",
        #   sensitivity_tooltip = "Yet to be systematically evaluated",
        #   tool_url = NULL,
        #   tool_name = NULL
        # )
        )
    ),
    # ======= Polymerase Proofreading Deficiency ========
    new_process(
      name = "Polymerase Proofreading Deficiency",
      description = "Impaired proofreading of DNA polymerases, particularly Pol ε and Pol δ. This deficiency significantly impacts the fidelity of DNA replication.",
      class = "endogenous",
      icon = "faShieldHalved",
      flip_vertical = FALSE,
      flip_horizontal = FALSE,
      scars = list(
        # new_scar(
          # scar_name = "Hemimethylation Burden",
          # process = "Methyltransferase Dysfunction",
          # marker_of = "Low DNMT1 activity",
          # disease_specificity = "Pan-Cancer",
          # diseases = "cancer",
          # modality = "Methylation",
          # measurement = "Proportion of CpGs which are hemimethylated",
          # description = "DNMT1 maintenance methyltransferase should decrease hemimethylation.",
          # experiment = "",
          # paper_url = "",
          # specificity = "Unknown",
          # specificity_tooltip = "Yet to be systematically evaluated",
          # sensitivity = "Unknown",
          # sensitivity_tooltip = "Yet to be systematically evaluated",
          # tool_url = NULL,
          # tool_name = NULL
        # )
      )
    ),
    # ======= Somatic Hypermutation ========
    new_process(
      name = "Spliceosome Dysfunction",
      description = "Impaired proofreading of DNA polymerases, particularly Pol ε and Pol δ. This deficiency significantly impacts the fidelity of DNA replication.",
      class = "endogenous",
      icon = "faShieldHalved",
      flip_vertical = FALSE,
      flip_horizontal = FALSE,
      scars = list(
        # new_scar(
        # scar_name = "Hemimethylation Burden",
        # process = "Methyltransferase Dysfunction",
        # marker_of = "Low DNMT1 activity",
        # disease_specificity = "Pan-Cancer",
        # diseases = "cancer",
        # modality = "Methylation",
        # measurement = "Proportion of CpGs which are hemimethylated",
        # description = "DNMT1 maintenance methyltransferase should decrease hemimethylation.",
        # experiment = "",
        # paper_url = "",
        # specificity = "Unknown",
        # specificity_tooltip = "Yet to be systematically evaluated",
        # sensitivity = "Unknown",
        # sensitivity_tooltip = "Yet to be systematically evaluated",
        # tool_url = NULL,
        # tool_name = NULL
        # )
      )
    )
  )
}
