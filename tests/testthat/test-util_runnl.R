testthat::context("util_runnl tests")
testthat::test_that("util_runnl", {

  # Run these tests only on Github actions:
  testthat::skip_if(!identical(Sys.getenv("GITHUB_ACTIONS"), "true"))

  ## Check that JAVA is installed:
  testthat::expect_true(system('java -version') == 0)

  ## Check that netLogo installation worked:
  nlpath <- ifelse(nlrx:::util_get_os() == "win", "C:/Program Files/NetLogo 6.1.1",
                   ifelse(nlrx:::util_get_os() == "unix", "/home/runner/work/netlogo/NetLogo 6.1.1",
                          ifelse(nlrx:::util_get_os() == "mac","/Applications/netlogo/NetLogo 6.1.1",
                                 "FAILED")))

  testthat::expect_true(nlpath != "FAILED")
  testthat::expect_true(dir.exists(nlpath))

  jarpath <- ifelse(nlrx:::util_get_os() == "win", "C:/Program Files/NetLogo 6.1.1/app/netlogo-6.1.1.jar",
                    ifelse(nlrx:::util_get_os() == "unix", "/home/runner/work/netlogo/NetLogo 6.1.1/app/netlogo-6.1.1.jar",
                           ifelse(nlrx:::util_get_os() == "mac","/Applications/netlogo/NetLogo 6.1.1/app/netlogo-6.1.1.jar",
                                  "FAILED")))

  testthat::expect_true(jarpath != "FAILED")
  testthat::expect_true(file.exists(jarpath))

  ## Check that util_create_sim_XML can handle idrunnumber
  modelpath <- file.path(nlpath, "app", "models", "Sample Models",
                         "Biology", "Wolf Sheep Predation.nlogo")
  nl <- nl(nlversion = "6.1.1",
           nlpath = nlpath,
           modelpath = modelpath,
           jvmmem = 1024)

  outpath <- tempdir()

  # Test idrunnum, idfinal and stopcond
  nl@experiment <- experiment(expname = "nlrx_test",
                              outpath = outpath,
                              repetition = 1,
                              tickmetrics = "true",
                              idsetup = "setup",
                              idgo = "go",
                              idfinal = "final",
                              idrunnum = "test",
                              stopcond = "not any? turtles",
                              runtime = 2,
                              evalticks = c(1,2),
                              metrics = c("count sheep","count wolves"),
                              variables = list('initial-number-sheep' =
                                                 list(min=50, max=150,
                                                      step=10, qfun="qunif"),
                                               'initial-number-wolves' =
                                                 list(min=50, max=150,
                                                      step=10, qfun="qunif")),
                              constants = list("model-version" =
                                                 "\"sheep-wolves-grass\"",
                                               "grass-regrowth-time" = 30,
                                               "sheep-gain-from-food" = 4,
                                               "wolf-gain-from-food" = 20,
                                               "sheep-reproduce" = 4,
                                               "wolf-reproduce" = 5,
                                               "show-energy?" = "false"))

  nl@simdesign <- simdesign_lhs(nl=nl,
                                samples=1,
                                nseeds=1,
                                precision=3)

  xmlfile <- file.path(nl@experiment@outpath, "nlrx_test.xml")
  seed <- nl@simdesign@simseeds[1]
  siminputrow <- 1
  util_create_sim_XML(nl, seed, siminputrow, xmlfile)
  testthat::expect_true(file.exists(xmlfile))

  # Test more strict for everything else
  ## Now we check if we can run a simple simulation:
  ## Step1: Create a nl obejct:
  modelpath <- file.path(nlpath, "app", "models", "Sample Models",
                         "Biology", "Wolf Sheep Predation.nlogo")
  nl <- nl(nlversion = "6.1.1",
           nlpath = nlpath,
           modelpath = modelpath,
           jvmmem = 1024)

  outpath <- tempdir()

  ## Step2: Add Experiment
  nl@experiment <- experiment(expname = "nlrx_test",
                              outpath = outpath,
                              repetition = 1,
                              tickmetrics = "true",
                              idsetup = "setup",
                              idgo = "go",
                              idfinal = NA_character_,
                              runtime = 2,
                              evalticks = c(1,2),
                              metrics = c("count sheep","count wolves"),
                              variables = list('initial-number-sheep' =
                                                 list(min=50, max=150,
                                                      step=10, qfun="qunif"),
                                               'initial-number-wolves' =
                                                 list(min=50, max=150,
                                                      step=10, qfun="qunif")),
                              constants = list("model-version" =
                                                 "\"sheep-wolves-grass\"",
                                               "grass-regrowth-time" = 30,
                                               "sheep-gain-from-food" = 4,
                                               "wolf-gain-from-food" = 20,
                                               "sheep-reproduce" = 4,
                                               "wolf-reproduce" = 5,
                                               "show-energy?" = "false"))

  nl@simdesign <- simdesign_lhs(nl=nl,
                                samples=1,
                                nseeds=1,
                                precision=3)


  testthat::context("Create xml files:")
  xmlfile <- file.path(nl@experiment@outpath, "nlrx_test.xml")
  seed <- nl@simdesign@simseeds[1]
  siminputrow <- 1
  util_create_sim_XML(nl, seed, siminputrow, xmlfile)
  testthat::expect_true(file.exists(xmlfile))
  xmlfileread <- XML::xmlParse(file = xmlfile)
  xmlfileread <- XML::xmlToList(xmlfileread)

  testthat::expect_equal(xmlfileread$experiment$setup, "setup")
  testthat::expect_equal(xmlfileread$experiment$go, "go")
  testthat::expect_equal(xmlfileread$experiment$timeLimit[["steps"]], "2")
  testthat::expect_equal(xmlfileread$experiment$metric, "count sheep")
  testthat::expect_true(is.character(
    xmlfileread$experiment$enumeratedValueSet$value[["value"]]))
  testthat::expect_equal(
    xmlfileread$experiment$enumeratedValueSet$.attrs[["variable"]],
    "initial-number-sheep")
  testthat::expect_equal(
    xmlfileread$experiment$.attrs[["name"]], "nlrx_test")
  testthat::expect_equal(xmlfileread$experiment$.attrs[["repetitions"]], "1")
  testthat::expect_equal(
    xmlfileread$experiment$.attrs[["runMetricsEveryStep"]], "true")


  testthat::context("Create batch files:")
  batchfile <- util_read_write_batch(nl)
  testthat::expect_true(file.exists(batchfile))

  testthat::context("Call NetLogo batch file:")
  outfile <- file.path(nl@experiment@outpath, "nlrx_output_1.csv")
  util_call_nl(nl, xmlfile = xmlfile, batchfile = batchfile, outfile = outfile)
  expect_true(file.exists(outfile))

  testthat::context("Gather results:")
  results <- util_gather_results(nl, outfile, seed, siminputrow)
  testthat::expect_match(class(results)[1], "tbl_df")
  testthat::expect_equal(nrow(results), 2)

  testthat::context("Cleanup:")

  if (.Platform$OS.type == "windows") {
    outfile <- gsub("/", "\\\\", outfile)
    xmlfile <- gsub("/", "\\\\", xmlfile)
    batchfile <- gsub("/", "\\\\", batchfile)
  }

  cleanup.files <- list("csv" = outfile,
                        "xml" = xmlfile,
                        "bat" = batchfile)
  util_cleanup(nl,
               cleanup.csv = TRUE,
               cleanup.xml = TRUE,
               cleanup.bat = TRUE,
               cleanup.files)

  testthat::expect_false(file.exists(xmlfile))
  testthat::expect_false(file.exists(batchfile))
  testthat::expect_false(file.exists(outfile))

})
