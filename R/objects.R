# Constructor

#' Create a new scar created by tumorigenic processes
#'
#' This function constructs a new `scar` object that represents a characteristic pattern observed in DNA,RNA or Methylation
#' that is produced as a by-product of tumorigenic processes (e.g. radiation exposure or molecular dysfunctions).
#'
#' @param process The tumorigenic process responsible for this scar (e.g., radiation or molecular dysfunction).
#' Must be one of the values returned by [valid_processes()] (string).
#' @param scar_name The name of the scar (string).
#' @param marker_of A detailed description of what the scar is a biomarker for,
#' such as a specific biological process or mutation signature (string).
#' @param disease_specificity Specifies if the scar is associated with \strong{"Pan-Cancer"}
#' or \strong{"Disease Specific"} conditions (string).
#' @param diseases A character vector of diseases linked to the scar. If `disease_specificity = "Pan-Cancer"`,
#' this must be \strong{"cancer"}.
#' @param modality The biological modality in which the scar is detected. Must be one of \strong{"DNA"},
#' \strong{"RNA"}, or \strong{"Methylation"} (string).
#' @param measurement The specific metric used to detect or quantify the scar (string).
#' @param description A plain-language description that explains how this scar is related to the
#' tumorigenic process or biological phenomenon it marks (string).
#' @param experiment A plain-language description of the experiment or analysis that discovered this scar
#' and established its association with the tumorigenic process (string).
#' @param paper_url The URL of the scientific publication that provides evidence for this scar's existence and relevance (string).
#' @param specificity Describes how specifically the scar is associated with the process it marks.
#' One of \strong{"High"}, \strong{"Moderate"}, \strong{"Low"}, or \strong{"Unknown"} (string).
#' @param specificity_tooltip A description explaining how the specificity was inferred,
#' including whether other processes might also contribute to the appearance of this scar (string).
#' @param sensitivity Indicates how sensitive the scar is to the process it marks.
#' One of \strong{"High"}, \strong{"Moderate"}, \strong{"Low"}, or \strong{"Unknown"} (string).
#' @param sensitivity_tooltip A description explaining how the sensitivity was inferred (string).
#' @param tool_name The name of the computational or laboratory tool used for scar detection, or \code{NULL}
#' if no tool is available (string or \code{NULL}).
#' @param tool_url The URL for the tool used for scar detection, or \code{NULL} if no tool is available (string or \code{NULL}).
#'
#' @return An object of class \strong{scar}, representing the tumorigenic process and its associated biomarker. Each `scar` object contains the following properties:
#' \describe{
#'   \item{scar_name}{The name of the scar (string).}
#'   \item{process}{The tumorigenic process that generates the scar (string).}
#'   \item{marker_of}{A description of the biological phenomenon the scar serves as a biomarker of (string).}
#'   \item{disease_specificity}{Whether the scar is \strong{"Pan-Cancer"} or \strong{"Disease Specific"} (string).}
#'   \item{diseases}{A character vector of diseases associated with the scar (character).}
#'   \item{modality}{The biological modality in which the scar is detected (e.g., DNA, RNA, Methylation) (string).}
#'   \item{measurement}{The metric used to detect or quantify the scar (string).}
#'   \item{description}{A description explaining how the scar relates to the process (string).}
#'   \item{experiment}{A description of the experiment that identified the scar and its association to the process (string).}
#'   \item{paper_url}{The URL of the publication detailing the scar (string).}
#'   \item{specificity}{How specific the scar is to the process, with possible values \strong{"High"}, \strong{"Moderate"}, \strong{"Low"}, or \strong{"Unknown"} (string).}
#'   \item{specificity_tooltip}{An explanation of how the specificity of the scar was determined (string).}
#'   \item{sensitivity}{How sensitive the scar is as a biomarker of the process, with possible values \strong{"High"}, \strong{"Moderate"}, \strong{"Low"}, or \strong{"Unknown"} (string).}
#'   \item{sensitivity_tooltip}{An explanation of how the sensitivity of the scar was determined (string).}
#'   \item{tool_name}{The name of the tool used to detect the scar, if available (string or \code{NULL}).}
#'   \item{tool_url}{The URL of the tool used for scar detection, if available (string or \code{NULL}).}
#' }
#' @export
#'
#' @examples
#' new_scar(
#'   process = "Radiotherapy",
#'   scar_name = "High Indel Burden",
#'   marker_of = "Radiation Induced Damage",
#'   disease_specificity = "Pan-Cancer",
#'   diseases = "cancer",
#'   modality = "DNA",
#'   measurement = "INDEL / SBS ratio",
#'   description = "Higher number of INDELS (mainly deletions)",
#'   experiment = "Analysis of 12 radiation-associated tumors compared to radiation-naive tumors",
#'   paper_url = "https://www.nature.com/articles/ncomms12605",
#'   specificity = "Low",
#'   specificity_tooltip = "Also observed in BRCA1/BRCA2 breast cancer.",
#'   sensitivity = "Moderate",
#'   sensitivity_tooltip = "Above median in all radiation-induced tumours.",
#'   tool_name = "raDNA",
#'   tool_url = "https://github.com/selkamand/radna"
#' )
#'
new_scar <- function(
    process,
    scar_name,
    marker_of,
    disease_specificity,
    diseases,
    modality,
    measurement,
    description,
    experiment,
    paper_url,
    specificity,
    specificity_tooltip,
    sensitivity,
    sensitivity_tooltip,
    tool_name,
    tool_url
  ){

  # Assertions
  assertions::assert_string(process)
  assertions::assert_subset(process, valid_processes(), msg = "[{process}] is not a valid process. See {.code valid_processes()} for valid options")

  assertions::assert_string(scar_name)
  assertions::assert_string(marker_of)

  assertions::assert_string(disease_specificity)
  assertions::assert_subset(disease_specificity, c("Pan-Cancer", "Disease Specific"), "{.code disease_specificity} must be either {.strong 'Pan-Cancer'} or {.strong 'Disease Specific'},  not [{disease_specificity}]")

  assertions::assert_character(diseases)

  if(disease_specificity == "Pan-Cancer") {
    assertions::assert(length(diseases) == 1, diseases == "cancer", msg = "When {.code disease_specificity = 'Pan-Cancer'} then {.strong diseases} argument must be {.strong 'cancer'}")
  }

  assertions::assert_string(modality)
  assertions::assert_subset(modality, c("DNA", "RNA", "Methylation"))

  assertions::assert_string(description)

  assertions::assert_string(experiment)
  assertions::assert_string(paper_url)

  assertions::assert_string(specificity)
  assertions::assert_subset(specificity, c("High", "Moderate", "Low", "Unknown"))
  assertions::assert_string(specificity_tooltip)

  assertions::assert_string(sensitivity)
  assertions::assert_subset(sensitivity, c("High", "Moderate", "Low", "Unknown"))
  assertions::assert_string(sensitivity_tooltip)

  if(!is.null(tool_url)) assertions::assert_string(tool_url)
  if(!is.null(tool_name)) assertions::assert_string(tool_name) else tool_name ="No tool available"

  # Create List Object
  ls_scar <- list(
    process = process,
    scar_name = scar_name,
    marker_of = marker_of,
    disease_specificity = disease_specificity,
    diseases = diseases,
    modality = modality,
    measurement = measurement,
    description = description,
    experiment = experiment,
    paper_url = paper_url,
    specificity = specificity,
    specificity_tooltip = specificity_tooltip,
    sensitivity = sensitivity,
    sensitivity_tooltip = sensitivity_tooltip,
    tool_name = tool_name,
    tool_url = tool_url
  )

  # Add Scar Class
  structure(
    ls_scar,
    class = "scar"
  )
}



#' Create an object representing a tumorigenic process
#'
#' This function constructs a `process` object representing a tumorigenic process
#' (e.g., endogenous or exogenous processes like radiation or genetic mutations).
#' The object includes details about the process, such as its name, description,
#' icon, scars associated with the process, and whether the process affects DNA, RNA,
#' or methylation markers.
#'
#' @param name The name of the tumorigenic process. Must be one of the values
#' returned by [valid_processes()] (string).
#' @param description A detailed description of the tumorigenic process (string).
#' @param icon A string identifier for the favicon icon representing the process
#' (e.g., `faRadiation` for radiation processes) (string).
#' @param class The classification of the process, either \strong{"endogenous"}
#' (originating from internal factors) or \strong{"exogenous"} (originating from external factors) (string).
#' @param flip_vertical Logical flag indicating if the icon should be flipped vertically (boolean).
#' @param flip_horizontal Logical flag indicating if the icon should be flipped horizontally (boolean).
#' @param scars A list of `scar` objects associated with this tumorigenic process.
#' Each scar must have the same `process` name as the one provided here (list).
#'
#' @return An object of class \strong{process} with the following properties:
#' \describe{
#'   \item{name}{The name of the tumorigenic process (string).}
#'   \item{description}{A detailed description of the tumorigenic process (string).}
#'   \item{icon}{The identifier for the icon used to visually represent the process (string).}
#'   \item{flip_vertical}{A logical flag indicating whether the icon should be flipped vertically (boolean).}
#'   \item{flip_horizontal}{A logical flag indicating whether the icon should be flipped horizontally (boolean).}
#'   \item{dna}{Logical value indicating if this process leaves scars on DNA (boolean).}
#'   \item{rna}{Logical value indicating if this process leaves scars on RNA (boolean).}
#'   \item{meth}{Logical value indicating if this process leaves scars on methylation markers (boolean).}
#'   \item{scars}{A list of `scar` objects associated with the process. Each `scar` must have the same `process` name (list).}
#' }
#' @export
#'
#' @examples
#' # Creating two scars for Radiotherapy
#' high_indel_burden <- new_scar(
#'   process = "Radiotherapy",
#'   scar_name = "High Indel Burden",
#'   marker_of = "Radiation Induced Damage",
#'   disease_specificity = "Pan-Cancer",
#'   diseases = "cancer",
#'   modality = "DNA",
#'   measurement = "INDEL / SBS ratio",
#'   description = "High INDEL counts (mostly deletions)",
#'   experiment = "Analysis of 12 radiation-associated tumors compared to radiation-naive tumors",
#'   paper_url = "https://www.nature.com/articles/ncomms12605",
#'   specificity = "Low",
#'   specificity_tooltip = "Also observed in BRCA1/BRCA2 breast cancer.",
#'   sensitivity = "Moderate",
#'   sensitivity_tooltip = "Above median in all radiation-induced tumors",
#'   tool_name = "raDNA",
#'   tool_url = "https://github.com/selkamand/radna"
#' )
#'
#' high_deletion_burden <- new_scar(
#'   process = "Radiotherapy",
#'   scar_name = "High Deletion Burden",
#'   marker_of = "Radiation Induced Damage",
#'   disease_specificity = "Pan-Cancer",
#'   diseases = "cancer",
#'   modality = "DNA",
#'   measurement = "Deletion / Insertion ratio",
#'   description = "Small deletions in regions of microhomology.",
#'   experiment = "Analysis of 12 radiation-associated tumors",
#'   paper_url = "https://www.nature.com/articles/ncomms12605",
#'   specificity = "Low",
#'   specificity_tooltip = "Also observed in BRCA1/BRCA2 breast cancer.",
#'   sensitivity = "Moderate",
#'   sensitivity_tooltip = "Above median in 11/12 radiation-induced tumors.",
#'   tool_name = "raDNA",
#'   tool_url = "https://github.com/selkamand/radna"
#' )
#'
#' # Create the Radiotherapy process object with two scars
#' radiotherapy_process <- new_process(
#'   name = "Radiotherapy",
#'   description = "Damage caused by ionizing radiation therapy",
#'   class = "exogenous",
#'   icon = "faRadiation",
#'   flip_vertical = FALSE,
#'   flip_horizontal = TRUE,
#'   scars = list(high_indel_burden, high_deletion_burden)
#' )
new_process <- function(
    name,
    description,
    class,
    icon,
    flip_vertical,
    flip_horizontal,
    scars
) {
  # Assertions
  assertions::assert_string(name)
  assertions::assert_subset(name, valid_processes(), msg = "[{process}] is not a valid process. See {.code valid_processes()} for valid options")

  assertions::assert_string(description)
  assertions::assert_string(icon)
  assertions::assert_flag(flip_vertical)
  assertions::assert_flag(flip_horizontal)
  assertions::assert_list(scars)
  assertions::assert_string(class)
  assertions::assert_subset(class, c("endogenous", "exogenous"))

  lapply(scars, \(scar) { assertions::assert_class(scar, class = "scar") })
  lapply(scars, \(scar) { assertions::assert(scar$process == name, msg = "Scar [{scar$name}] cannot be added as a scar of process [{name}] since the scar's 'process' property is [{scar$process}] instead of [{name}]") })
  modalities <- unique(unlist(lapply(scars, \(scar){ scar$modality })))

  # Check which modalities the process leaves scars in
  dna <- "DNA" %in% modalities
  rna <- "RNA" %in% modalities
  meth <- "Methylation" %in% modalities

  ls_process <- list(
    name = name,
    description = description,
    icon = icon,
    flip_vertical = flip_vertical,
    flip_horizontal = flip_horizontal,
    modalities = modalities,
    dna = dna,
    rna = rna,
    meth = meth,
    scars = scars
  )

  structure(
    ls_process,
    class = "process"
  )
}

#' Print process
#'
#' @inheritParams base::print
#' @param ... added for uniformity with generic. Does not do anything when printing process objects
#' @export
#'
print.process <- function(x, ...){
  cat("==============================================\n")
  cat("Process: ", x$name, "\n")
  cat("==============================================\n")
  cat("Description: ", x$description, "\n\n")
  cat("Scars: ", length(x$scars), "\n")
  cat("Modalities: ", paste0(x$modalities, collapse = ", "), "\n")
}

#' Retrieve a list of valid tumorigenic processes
#'
#' This function returns a character vector of valid tumorigenic processes
#' that can be associated with a scar. These processes are high-level descriptions
#' of biological or treatment-related phenomena that leave characteristic
#' mutation signatures or multiomic scars in cancer tissues.
#'
#' @return A character vector of valid tumorigenic processes.
#' @export
#'
#' @examples
#' valid_processes()
valid_processes <- function(){
  c("Radiotherapy", "Recombination", "Methytransferase Dysfunction",
    "Spliceosome Dysfunction", "Ultraviolet Radiation", "Mismatch Repair deficiency",
    "Homologous Repair Deficiency", "APOBEC hyperactivity", "Chemotherapy",
    "Smoking", "Viruses", "Defective Base Excision Repair", "Leaky Checkpoints",
    "ADAR activity", "NHEJ repair", "TOP2A loss", "Sequencing")
}

#' Remove all newline characters from a string
#'
#' This function removes all newline (`\n`) and carriage return (`\r`) characters from the input string.
#'
#' @param x A character string from which to remove newline and carriage return characters.
#'
#' @return A character string with all newline and carriage return characters removed.
#' @export
#'
#' @examples
#' strip_newlines("Hello\nWorld\r!")
strip_newlines <- function(x){
  gsub("[\r\n]", "", x)
}

