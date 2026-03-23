# ==============================================================================
# FVSPrjBldr/ui.R  --  PATCHED for configurable deployment
# ==============================================================================
# Changes from original:
#   - Sources fvsol_config.R at top level so cfg() is available
#   - Replaces hardcoded "Virginia Tech" with cfg("server","institution")
#   - Replaces hardcoded department with cfg("server","department")
#   - Replaces hardcoded "60 days" with cfg("projects","retention_days")
# ==============================================================================

library(shiny)

# Load configuration before UI definition so cfg() calls work
source("fvsol_config.R")

shinyUI(fluidPage(
  tags$style(HTML(paste0(
    ".nav>li>a {padding:6px;}",
    ".btn {padding:4px 6px;color:darkred; background-color:#eef8ff;}",
    ".form-control {padding:2px 4px; height:auto;}",
    ".form-group {margin-bottom:6px}"))),
  verticalLayout(
    HTML(paste0('<h2 style="margin-top: 1px; margin-bottom: 0px;"><img src="FVSlogo.png"</img>',
                "Forest Vegetation Simulator (FVS-Online)")),
    h3(paste0("Hosted by ", cfg("server", "institution"),
              if (nchar(cfg("server", "department")) > 0)
                paste0(" / ", cfg("server", "department")) else "")),
    h5("Set up an FVSOnline project by filling out this form. ",
      "You will be sent an Email ",
      "with a link to this project.  Note that your project(s) will be ",
      paste0("removed from this server ", cfg("projects", "retention_days"),
             " days after the last access.")),
    p("Check your Email spam files if you don't get the Email promptly."),
    tags$style(type="text/css", "#title { width: 500px; }"),
    textInput("title", "Your new project title"),
    textInput("emailnew", "Your Email address"),
    textInput("emaildup", "Your Email address again"),
#    radioButtons("version",NULL,choices=list(
#       "Use the production version of the software"="production",
#       "Use the development version"="development"),
#       selected="production"),
    p("By pressing submit you are certifying that you agree to the Notice posted below."),
    actionButton("submitnew","Submit"),
    tags$style(type="text/css","#actionMsg{color:darkred;}"),
    textOutput("actionMsg"),p(),
    HTML("<hr/>"),
    h4("Retrieve existing project links"),
    h5("Enter the Email address you used when you created a project(s).",
      " You will be sent an Email with all of the projects under this address."),
    textInput("emailexist","Your Email address"),
    actionButton("submitexist","Submit"),
    textOutput("actionMsg2"),
    HTML("<hr/><h4>Notice</h4><p>",
      "Submissions constitute consent to receive email from this server.",
      " Email addresses are stored only as identifiers and are not shared.",
      "</p><p>",
      paste0("Projects and data may be removed from this system ",
             cfg("projects", "retention_days"),
             " days after the last access. "),
      "The ",cfg("server","institution"),
      " makes no guarantees as to the availability of this service.</p>"),
    p()
  )
))
