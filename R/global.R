
# load libraries
library(rclipboard)
library(shinyWidgets)
library(c3)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(ggplot2)
library(highcharter)
library(r2d3)
library(htmltools)
library(shinycssloaders)
library(pals)
library(colorspace)
library(echarts4r)
library(magrittr) #c3 package seems to break the piping so need to reload magrittr "Error in split_chain(match.call(), env = env) : could not find function "split_chain""
library(colourpicker)
library(rcartocolor)
library(khroma)



# ---- create dataset - we don't need to do this if its already saved in /data -
# test <- read.csv("~/network_drives/wxchfpd1/AnalyticalServices/AS/Steve K/R Plotting Packages/R Plotting Packages/data/Book1.csv")
# test$Number <- runif(n=300, min=100, max=1000)
# dataset <- rbind(test,
#                  test %>% mutate(year = year+1, Number = Number + runif(n=300, min=50, max=300)),
#                  test %>% mutate(year = year+2, Number = Number + runif(n=300, min=70, max=100)),
#                  test %>% mutate(year = year+3, Number = Number + runif(n=300, min=100, max=300)),
#                  test %>% mutate(year = year+4, Number = Number + runif(n=300, min=50, max=600)),
#                  test %>% mutate(year = year+5, Number = Number + runif(n=300, min=10, max=80)),
#                  test %>% mutate(year = year+6, Number = Number + runif(n=300, min=10, max=60)),
#                  test %>% mutate(year = year+7, Number = Number + runif(n=300, min=30, max=200)),
#                  test %>% mutate(year = year+8, Number = Number + runif(n=300, min=70, max=200)),
#                  test %>% mutate(year = year+9, Number = Number + runif(n=300, min=200, max=400)),
#                  test %>% mutate(year = year+10, Number = Number + runif(n=300, min=400, max=1600)),
#                  test %>% mutate(year = year+11, Number = Number + runif(n=300, min=800, max=1000))
# )
# dataset$year <- as.character(dataset$year)
# dataset$Rate <- dataset$Number * .005
# dataset$upper_ci <- dataset$Rate * 1.2
# dataset$lower_ci <- dataset$Rate * 0.8
# write.csv(dataset, "~/R Plotting Packages/data/datasetnew.csv")
# ------------------------------------------------------------------------------



# ---- home page text ----------------------------------------------------------
hometext <-
  tagList(
    # "About this application:",
    tags$ul(
      tags$li(tags$p("This application has been created to compare various plotting packages
                        available in R to create interactive plots.")),
      tags$li(tags$p("Plots are creating using a standard dataset so that the appearance, loading time and interactivity can be compared.")),
      tags$li(tags$p("Code complexity can be compared by selecting 'View code' above each plot.")),
      tags$li(tags$p("Bar and line plots have been chosen for this application as these are very commonly used. These can be rendered with
                      multiple variables (grouped plots) and error bars if selected.")),
      tags$li(tags$p("Common colour palettes are available to select. To expand on these with custom made palettes, there is a page called 'colourpicker'
                      which allows users to create a new palette. These custom colour palettes can be made available to the plotting pages (named 'custom_pal_i') by clicking 'Use palette',
                      or saved for future use by clicking 'Copy to clip' then pasting somewhere safe.")),
      tags$li(tags$p("Some comments around the pros and cons of the plotting packages have been made on their respective pages, as well as notable package idiosyncrasies."))
    )
  )
# ------------------------------------------------------------------------------



# ---- Read in dataset and set some variables ----------------------------------
dataset <- read.csv("data/datasetnew.csv")
dataset$year <- as.character(dataset$year)

#only keeping 12 locations - clean this code up
dataset <- dataset %>% 
  filter(Variable_3 != "Location_13" & Variable_3 != "Location_14" & Variable_3 != "Location_15" & Variable_3 != "Location_16" & Variable_3 != "Location_17" & Variable_3 != "Location_18" & Variable_3 != "Location_19" & Variable_3 != "Location_20")

select_years <- unique(dataset$year)

select_numerical <- c("Number", "Rate")

select_primary_var <- c("Variable_1", "Variable_2", "Variable_3")
# ------------------------------------------------------------------------------



# ---- Data sets for view code tabs - created using datapasta ------------------

#single line
single_line <- 
  "dataset_filtered <- data.frame(
                        stringsAsFactors = FALSE,
                        year = c('2010','2011','2012','2013',
                                 '2014','2015','2016','2017','2018','2019','2020',
                                 '2021'),
                        Number = c(462.973908277927,
                                   620.557510790823,539.322659180663,646.496666450519,766.075297717704,
                                   504.209347923764,494.328577206004,565.16316072396,
                                   583.431281687773,736.152318269014,1379.94224626548,
                                   1272.36091807787),
                        upper_ci = c(555.568689933512,
                                     744.669012948987,647.187191016795,775.795999740623,919.290357261244,
                                     605.051217508516,593.194292647205,678.195792868752,
                                     700.117538025328,883.382781922817,1655.93069551857,
                                     1526.83310169345),
                        lower_ci = c(370.379126622342,
                                     496.446008632658,431.45812734453,517.197333160415,612.860238174163,
                                     403.367478339011,395.462861764804,452.130528579168,
                                     466.745025350219,588.921854615211,1103.95379701238,
                                     1017.8887344623)
                  )"

#multi line
multi_line <-
"dataset_filtered <- data.frame(
                      stringsAsFactors = FALSE,
                      Var = c('down','down','down','down',
                               'down','down','down','down','down','down','down',
                               'down','left','left','left','left','left','left',
                               'left','left','left','left','left','left','middle',
                               'middle','middle','middle','middle','middle',
                               'middle','middle','middle','middle','middle','middle',
                               'right','right','right','right','right','right','right',
                               'right','right','right','right','right','up','up',
                               'up','up','up','up','up','up','up','up','up',
                               'up'),
                      year = c('2010','2011','2012','2013',
                               '2014','2015','2016','2017','2018','2019','2020',
                               '2021','2010','2011','2012','2013','2014','2015',
                               '2016','2017','2018','2019','2020','2021','2010',
                               '2011','2012','2013','2014','2015','2016','2017',
                               '2018','2019','2020','2021','2010','2011','2012',
                               '2013','2014','2015','2016','2017','2018','2019',
                               '2020','2021','2010','2011','2012','2013','2014','2015',
                               '2016','2017','2018','2019','2020','2021'),
                      Number = c(96.1186692706542,
                               127.432019640168,111.23023562684,131.995813017827,153.940234601032,
                               103.990923084191,102.755015428481,117.617309080809,
                               120.085550483945,151.693881688057,281.506454974995,
                               256.343771100393,91.3266816304531,124.354161153664,
                               106.552511696715,130.182542931288,155.910269425658,
                               99.1622044386691,97.4492117011221,111.977012811392,115.3092191729,
                               146.748215748928,267.922339665005,254.351733061485,
                               89.5596826892579,116.469639824529,104.927690367471,
                               125.162771058385,154.516566139588,98.7612048610696,
                               95.3070183791104,112.084804385912,113.486901327875,
                               144.908934708103,267.334267178667,249.392558249296,
                               83.6853175997967,115.92987447523,98.7751144266687,121.136025747866,
                               144.668152883009,91.597428842925,89.9894876653678,
                               101.17179049839,109.19420306324,137.137399528525,
                               278.716908976086,245.591521580587,102.283557087765,
                               136.371815697232,117.837107062968,138.019513695152,
                               157.040074668417,110.697586696909,108.827844031923,122.312243947457,
                               125.355407639814,155.663886595401,284.462275470724,
                               266.681334086112),
                      upper_ci = c(115.342403124785,
                               152.918423568201,133.476282752207,158.394975621393,184.728281521238,
                               124.789107701029,123.306018514177,141.140770896971,
                               144.102660580734,182.032658025669,337.807745969994,
                               307.612525320472,109.592017956544,149.224993384397,
                               127.863014036058,156.219051517546,187.09232331079,
                               118.994645326403,116.939054041347,134.37241537367,138.37106300748,
                               176.097858898714,321.506807598006,305.222079673782,
                               107.471619227109,139.763567789434,125.913228440965,
                               150.195325270062,185.419879367505,118.513445833283,
                               114.368422054932,134.501765263095,136.184281593449,
                               173.890721649723,320.8011206144,299.271069899155,
                               100.422381119756,139.115849370277,118.530137312002,145.36323089744,
                               173.60178345961,109.91691461151,107.987385198441,
                               121.406148598068,131.033043675888,164.56487943423,
                               334.460290771304,294.709825896705,122.740268505318,
                               163.646178836678,141.404528475562,165.623416434182,188.448089602101,
                               132.837104036291,130.593412838308,146.774692736948,
                               150.426489167777,186.796663914481,341.354730564868,
                               320.017600903334),
                      lower_ci = c(76.8949354165234,
                               101.945615712134,88.9841885014717,105.596650414262,123.152187680826,
                               83.1927384673525,82.2040123427845,94.0938472646475,
                               96.0684403871559,121.355105350446,225.205163979996,
                               205.075016880315,73.0613453043625,99.4833289229311,
                               85.2420093573723,104.146034345031,124.728215540526,
                               79.3297635509353,77.9593693608977,89.5816102491133,
                               92.2473753383197,117.398572599143,214.337871732004,203.481386449188,
                               71.6477461514063,93.175711859623,83.9421522939764,
                               100.130216846708,123.61325291167,79.0089638888556,
                               76.2456147032883,89.6678435087297,90.7895210622996,
                               115.927147766482,213.867413742933,199.514046599437,
                               66.9482540798373,92.7438995801844,79.020091541335,96.9088205982931,
                               115.734522306407,73.27794307434,71.9915901322942,
                               80.9374323987122,87.3553624505922,109.70991962282,
                               222.973527180869,196.47321726447,81.8268456702121,
                               109.097452557785,94.2696856503748,110.415610956121,
                               125.632059734734,88.5580693575274,87.0622752255388,97.8497951579653,
                               100.284326111851,124.531109276321,227.569820376579,
                               213.345067268889)
              )"
  
  
#single bar
single_bar <-
  "dataset_filtered <- data.frame(
                        stringsAsFactors = FALSE,
                        Variable_1 = c('down', 'left', 'middle', 'right', 'up'),
                        Number = c(1754.70987799739,
                                   1701.24610343728,1671.91203916926,1617.59322528769,1825.55264667987),
                        upper_ci = c(2105.65185359687,
                                   2041.49532412474,2006.29444700311,1941.11187034523,2190.66317601585),
                        lower_ci = c(1403.76790239791,
                                   1360.99688274982,1337.52963133541,1294.07458023015,1460.4421173439)
              )"


#multi bar
multi_bar <- 
  "dataset_filtered <- data.frame(
                        stringsAsFactors = FALSE,
                        Variable_1 = c('down','down','down','left',
                                       'left','left','middle','middle','middle','right',
                                       'right','right','up','up','up'),
                        Variable_2 = c('large','medium','small',
                                       'large','medium','small','large','medium','small',
                                       'large','medium','small','large','medium','small'),
                        Number = c(683.476232046867,
                                       520.418945274013,550.814700676512,569.884575482376,623.179846995848,
                                       508.181680959056,658.028336546023,507.153953145701,
                                       506.729749477538,546.79497297745,583.854107114242,
                                       486.944145196001,596.408162375248,659.926374412247,
                                       569.218109892378),
                        upper_ci = c(820.17147845624,
                                       624.502734328816,660.977640811815,683.861490578852,747.815816395017,
                                       609.818017150867,789.634003855227,608.584743774841,
                                       608.075699373046,656.15396757294,700.62492853709,
                                       584.332974235201,715.689794850298,791.911649294696,
                                       683.061731870854),
                        lower_ci = c(546.780985637493,
                                       416.33515621921,440.65176054121,455.907660385901,498.543877596678,
                                       406.545344767245,526.422669236818,405.72316251656,
                                       405.383799582031,437.43597838196,467.083285691394,
                                       389.555316156801,477.126529900199,527.941099529797,
                                       455.374487913903)
              )"
# ------------------------------------------------------------------------------




# ----- legend formatting for plotly -------------------------------------------
legend_format <- list(
  title = '',
  font = list(
    family = "arial",
    size = 12,
    color = "#000"),
  bgcolor = "#f5f5f5",
  bordercolor = "#f5f5f5",
  borderwidth = 2,
  orientation = 'h', 
  xanchor = 'center', 
  x = .5, 
  y = -.2, 
  entrywidth = 100
)
# ------------------------------------------------------------------------------



# ----- ggplot theme -----------------------------------------------------------
ggplot_theme <- 
ggplot2::theme(panel.background = ggplot2::element_blank(), 
                     legend.title = ggplot2::element_blank(), 
                     axis.ticks = ggplot2::element_blank(), 
                     panel.grid.major.y = ggplot2::element_line(color = "grey"), 
                     axis.text = ggplot2::element_text(size = 10), 
                     axis.title = ggplot2::element_text(size = 12), 
                     legend.text = ggplot2::element_text(size = 10), 
                     strip.text = ggplot2::element_text(size = 10), 
                     legend.key = ggplot2::element_blank(), 
                     plot.margin = ggplot2::margin(t = 10, r = 20, b = 10, l = 20, unit = "pt"))
# ------------------------------------------------------------------------------




# ---- Creating colour palettes to be used in the app --------------------------
# these are some of the commonly used 'accessible' palettes

vibrant <- color("vibrant")
bright <- color("bright")
light <- color("light")
muted <- color("muted")

# COLOUR PALETTE OPTIONS
Palette_Options <- data.frame(
 
  pubr_bar =  c("c('#6b6e90', '#009a93', '#ffae66', '#4e81c1', '#fab900', '#ff59a4')" ,    length(c('#6b6e90', '#009a93', '#ffae66', '#4e81c1', '#fab900', '#ff59a4'))),
  pubr_line =  c("c('#6b6e90', '#f36747', '#009a93', '#fab900', '#ff59a4')" ,    length(c('#6b6e90', '#f36747', '#009a93', '#fab900', '#ff59a4'))),
  RColorBrewer_Set1 = c("RColorBrewer::brewer.pal(9, 'Set1')" ,    length(RColorBrewer::brewer.pal(9, 'Set1'))),
  RColorBrewer_Paired = c("RColorBrewer::brewer.pal(12, 'Paired')" ,    length(RColorBrewer::brewer.pal(12, 'Paired'))),
  RColorBrewer_Dark2 = c("RColorBrewer::brewer.pal(8, 'Dark2')" ,    length(RColorBrewer::brewer.pal(8, 'Dark2'))), 
  RColorBrewer_Accent = c("RColorBrewer::brewer.pal(8, 'Accent')" ,    length(RColorBrewer::brewer.pal(8, 'Accent'))),
  colorspace_Dark = c("qualitative_hcl(8, palette = 'Dark 3')" ,    length(qualitative_hcl(8, palette = 'Dark 3'))), 
  pals_alphabet = c("as.vector(pals::alphabet())" ,    length(as.vector(pals::alphabet()))), 
  pals_alphabet2 = c("as.vector(pals::alphabet2())" ,    length(as.vector(pals::alphabet2()))),
  pals_glasbey = c("pals::glasbey()" ,    length(pals::glasbey())), 
  pals_kelly = c("pals::kelly()" ,    length(pals::kelly())),
  pals_polychrome = c("as.vector(pals::polychrome())" ,    length(as.vector(pals::polychrome()))), 
  pals_trubetskoy = c("as.vector(pals::trubetskoy())" ,    length(as.vector(pals::trubetskoy()))),
  pals_tol = c("pals::tol()" ,    length(pals::tol())), 
  pals_glasbey = c("pals::glasbey()" ,    length(pals::glasbey())), 
  pals_cols25 = c("pals::cols25()",    length(pals::cols25())),
  pals_tableau = c("pals::tableau20()",    length(pals::tableau20())),
  rcartocolor_safe = c("carto_pal(12, 'Safe')",    length(carto_pal(12, 'Safe'))),
  rcartocolor_vivid = c("carto_pal(12, 'Vivid')",    length(carto_pal(12, 'Vivid'))),
  rcartocolor_prism = c("carto_pal(12, 'Prism')",    length(carto_pal(12, 'Prism'))),
  rcartocolor_pastel = c("carto_pal(12, 'Pastel')",    length(carto_pal(12, 'Pastel'))),
  rcartocolor_bold = c("carto_pal(12, 'Bold')",    length(carto_pal(12, 'Bold'))),
  rcartocolor_antique = c("carto_pal(12, 'Antique')",    length(carto_pal(12, 'Antique'))),
  khroma_bright = c("as.vector(bright(7))",    length(as.vector(bright(7)))),
  khroma_vibrant = c("as.vector(vibrant(7))",    length(as.vector(vibrant(7)))),
  khroma_light = c("as.vector(light(9))",    length(as.vector(light(9)))),
  khroma_muted = c("as.vector(muted(9))",    length(as.vector(muted(9))))
  )
saveRDS(Palette_Options, "Palette_Options.RDS")

# create an empty RDS file when app first started and instantiate pal_list_df
# pal_list_df is then populated based on entries in pal_list.RDS from the colourpicker page when palettes are saved (temporarlity) to be used in plotting pages
saveRDS(NULL, "pal_list.RDS")
pal_list_df <- NULL
# ------------------------------------------------------------------------------





