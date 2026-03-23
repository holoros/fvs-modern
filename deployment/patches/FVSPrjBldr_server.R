# ==============================================================================
# FVSPrjBldr/server.R  --  PATCHED for configurable deployment
# ==============================================================================
# Changes from original:
#   - Sources fvsol_config.R for centralized configuration
#   - Replaces hardcoded /home/shiny/FVSwork with cfg("paths","work_dir")
#   - Replaces hardcoded /home/shiny/FVS/bin with cfg("paths","fvs_bin")
#   - Replaces hardcoded charcoal2.cnre.vt.edu URL with cfg("server","base_url")
#   - Replaces hardcoded ssmtp command with cfg("email","send_command")
#   - Replaces hardcoded 60-day retention with cfg("projects","retention_days")
#   - Log file name pulled from config
# ==============================================================================

library(shiny)

options(shiny.trace = F)  # change to T for trace


shinyServer(function(input, output, session) {

  trim <- function (x) if (is.null(x)) "" else gsub("^\\s+|\\s+$","",x)
# pop the sink stack
  while(sink.number()) sink()
  try(sink(cfg("logging", "prjbldr_log")))
  cat (date(),"\n")
  cat ("cur dir=",getwd(),"\n")
  source("prjListEmail.R")
  source("uuidgen.R")
  observe({
    if (input$submitnew == 0) return()
    isolate({
      if (nchar(input$title)==0) return()
      emailnew = trim(input$emailnew)
      emaildup = trim(input$emaildup)
      if ((nchar(emailnew)<5 && regexpr("@",emailnew) < 2) || emailnew != emaildup)
      {
        msg = "Email entry error."
        output$actionMsg = renderText(msg)
        return()
      }
      uuid = uuidgen()
      workDir = paste0(cfg("paths", "work_dir"), "/", uuid)
      cat("workDir=",workDir,"\n")
      dir.create(workDir)
#      if (input$version == "production")
        cat(sprintf('library(fvsOL)\nfvsOL(fvsBin="%s")\n',
            cfg("paths", "fvs_bin")),
            file = paste0(workDir, "/app.R"))
#      if (input$version == "development")
#        cat(sprintf('library(fvsOLdev)\nfvsOL(fvsBin="%s")\n',
#            cfg("paths", "fvs_bin_dev")),
#            file = paste0(workDir, "/app.R"))
      # projectId file...
cat("email=",emailnew,"\ntitle=",input$title,"\n")
      cat(file=paste0(workDir,"/projectId.txt"),
          "email=",emailnew,"\ntitle=",input$title,"\n")
      rptFile = tempfile()
      con = file(rptFile,"w")
      link = paste0(cfg("server", "base_url"), "/", uuid)
      cat (file=con,"To:",emailnew,"\n")
      cat (file=con,
           sprintf("Subject: New %s at %s\n",
                   cfg("email", "subject_prefix"),
                   cfg("server", "institution")))
      cat (file=con,"\nHere is a link to the project named: ",input$title,"\n\n")
      cat (file=con,link,"\n\n")
      if (input$version == "development")
        cat (file=con,"This project uses development versions of the FVS software\n")
      cat (file=con,"Note that this project may be removed",
           "from the system", cfg("projects", "retention_days"),
           "days after the last access.")
      close(con)

      mailCmd = gsub("\\{file\\}", rptFile, cfg("email", "send_command"))
      if (nchar(mailCmd) > 0) {
        system(mailCmd)
      } else {
        cat("[FVSPrjBldr] Email sending disabled (send_command is empty)\n")
      }

      if (nchar(input$title))
      {
        msg = paste0('Project: "',input$title,'" created.')
        output$actionMsg = renderText(msg)
      }
      updateTextInput(session=session, inputId="title", value="")
      Sys.sleep(.3)
      unlink(rptFile)
    })
  })

  observe({
    if (input$submitexist==0) return()
    isolate({
      emailexist=trim(input$emailexist)
      if (nchar(emailexist)<5 && regexpr("@",emailexist) < 2) return()
      prjListEmail(emailexist,sendEmail=TRUE)
      updateTextInput(session=session, inputId="emailexist", value="")
    })
  })

})
