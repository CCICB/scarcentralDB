test_that("Scar object creation works", {
  scar_obj <- expect_no_error(new_scar(
    process = "Radiotherapy",
    scar_name = "High Indel Burden",
    marker_of = "Radiation Induced Damage",
    disease_specificity = "Pan-Cancer",
    diseases = "cancer",
    modality = "DNA",
    measurement = "INDEL / SBS ratio",
    description = "Higher number of INDELS. Driven by an increased burden of deletions, not insertions.",
    experiment = "Analysis of 12 radiation-associated tumors compared to radiation-naive tumors",
    paper_url = "https://www.nature.com/articles/ncomms12605",
    specificity = "Low",
    specificity_tooltip = "Also observed in BRCA1/BRCA2 breast cancer.",
    sensitivity = "Moderate",
    sensitivity_tooltip = "All radiation tumors had values exceeding the median for their cancer type.",
    tool_name = "raDNA",
    tool_url = "https://github.com/selkamand/radna"
  ))


  # Check that the object is of class 'scar'
  expect_s3_class(scar_obj, "scar")
  expect_named(scar_obj, c("process", "scar_name", "marker_of", "disease_specificity",
                           "diseases", "modality", "measurement", "description", "experiment",
                           "paper_url", "specificity", "specificity_tooltip", "sensitivity",
                           "sensitivity_tooltip", "tool_name", "tool_url"))
})


test_that("Scar creation with missing tool name and URL works", {

  # Create scar object without tool_name and tool_url
  scar_obj <- new_scar(
    process = "Radiotherapy",
    scar_name = "High Deletion Burden",
    marker_of = "Radiation Induced Damage",
    disease_specificity = "Pan-Cancer",
    diseases = "cancer",
    modality = "DNA",
    measurement = "Deletion / Insertion ratio",
    description = "Higher number of small deletions (1-100bp), particularly in regions of microhomology.",
    experiment = "Analysis of 12 radiation-associated tumors from 3 cancer types compared to radiation-naive tumors",
    paper_url = "https://www.nature.com/articles/ncomms12605",
    specificity = "Low",
    specificity_tooltip = "Also observed in BRCA1/BRCA2 breast cancer.",
    sensitivity = "Moderate",
    sensitivity_tooltip = "11/12 tumors had values exceeding the median for their cancer type.",
    tool_name = NULL,
    tool_url = NULL
  )

  # Check that the object is of class 'scar'
  expect_s3_class(scar_obj, "scar")

  # Check that the tool_name defaults to "No tool available"
  expect_equal(scar_obj$tool_name, "No tool available")

  # Check that tool_url is NULL
  expect_null(scar_obj$tool_url)
})

test_that("Scar creation fails with invalid inputs", {

  # Test that invalid process throws an error
  expect_error(
    new_scar(
      process = "InvalidProcess",
      scar_name = "Test Scar",
      marker_of = "Test Marker",
      disease_specificity = "Pan-Cancer",
      diseases = "cancer",
      modality = "DNA",
      measurement = "Test Measurement",
      description = "Test Description",
      experiment = "Test Experiment",
      paper_url = "https://www.example.com",
      specificity = "Low",
      specificity_tooltip = "Test Tooltip",
      sensitivity = "Moderate",
      sensitivity_tooltip = "Test Tooltip",
      tool_name = NULL,
      tool_url = NULL
    ),
    "is not a valid process"
  )

  # Test that invalid disease specificity throws an error
  expect_error(
    new_scar(
      process = "Radiotherapy",
      scar_name = "Test Scar",
      marker_of = "Test Marker",
      disease_specificity = "InvalidSpecificity",
      diseases = "cancer",
      modality = "DNA",
      measurement = "Test Measurement",
      description = "Test Description",
      experiment = "Test Experiment",
      paper_url = "https://www.example.com",
      specificity = "Low",
      specificity_tooltip = "Test Tooltip",
      sensitivity = "Moderate",
      sensitivity_tooltip = "Test Tooltip",
      tool_name = NULL,
      tool_url = NULL
    ),
    "`disease_specificity` must be either 'Pan-Cancer' or 'Disease Specific', not [InvalidSpecificity]",
    fixed=TRUE
  )

  # Test that Pan-Cancer specificity requires disease to be "cancer"
  expect_error(
    new_scar(
      process = "Radiotherapy",
      scar_name = "Test Scar",
      marker_of = "Test Marker",
      disease_specificity = "Pan-Cancer",
      diseases = "breast cancer",
      modality = "DNA",
      measurement = "Test Measurement",
      description = "Test Description",
      experiment = "Test Experiment",
      paper_url = "https://www.example.com",
      specificity = "Low",
      specificity_tooltip = "Test Tooltip",
      sensitivity = "Moderate",
      sensitivity_tooltip = "Test Tooltip",
      tool_name = NULL,
      tool_url = NULL
    ),
    "must be 'cancer'"
  )
})
