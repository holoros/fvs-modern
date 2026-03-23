# ==============================================================================
# FVSPrjBldr/prjListEmail.R  --  PATCHED for configurable deployment
# ==============================================================================
# Changes from original:
#   - Replaces hardcoded /home/shiny/FVSwork with cfg("paths","work_dir")
#   - Replaces hardcoded charcoal2.cnre.vt.edu URL with cfg("server","base_url")
#   - Replaces hardcoded ssmtp command with cfg("email","send_command")
#   - ndays default now pulled from config
#   - Email subject uses configurable institution name
# ==============================================================================

prjListEmail <- function (queryEmail,
                          ndays = cfg("projects", "retention_days"),
                          sendEmail = TRUE)
{
  # Send an Email of the projects associated with an email address
  # to the email address. The Email address must be a match (case insensitive)
  # to the one specified when the project was created.

  trim <- function (x) gsub("^\\s+|\\s+$","",x)

  if (missing(queryEmail)) stop("queryEmail must be specified")
  # insure a single token, no blanks
  queryEmail = scan(text=queryEmail,what="character",quiet=TRUE)
  if (length(queryEmail) > 1) stop ("queryEmail string contains white space")

  workDirs = list.dirs(cfg("paths", "work_dir"), recursive = FALSE)
  ids = lapply(workDirs,function (x)
     {
       fn = paste0(x,"/projectId.txt")
       id = NULL
       if (file.exists(fn))
       {
         id = scan(file=fn,what="character",
               sep="\n",quiet=TRUE)
         if (!is.null(id))
         {
           info = file.info(x)
           attr(id,"ctime") = info[1,"ctime"]
           info = file.info(fn)
           attr(id,"mtime") = info[1,"mtime"]
         }
       }
       id
     })
  names (ids) = sub(cfg("paths", "work_dir"),
                    cfg("server", "base_url"), workDirs)

  rptFile = tempfile()
  con = file(rptFile,"w")

  cat (file=con,"To:",queryEmail,"\n")
  cat (file=con,
       sprintf("Subject: FVSOnline projects at %s\n",
               cfg("server", "institution")))
  cat (file=con,"\n Projects and links for Email:",queryEmail,"\n")
  nprjs = 0
  for (i in 1:length(ids))
  {
    id = unlist(ids[i])
    nam = names(ids[i])
    if (is.null(id)) next
    email = trim(scan(text=id[1],what="character",quiet=TRUE)[2])
    if (tolower(email) == tolower(queryEmail))
    {
      nprjs = nprjs+1
      cat (file=con,"\n",id[1],"\n",id[2],"\n")
      tt = format(attr(ids[i][[1]],"ctime"),usetz=TRUE)
      cat (file=con," created at   = ",tt,"\n")
      tt = format(attr(ids[i][[1]],"mtime"),usetz=TRUE)
      cat (file=con," last modified= ",tt,"\n")
      tt = format(attr(ids[i][[1]],"mtime")+(86400*ndays),usetz=TRUE)#86400=seconds/day
      cat (file=con," auto removal = ",tt,"\n")
      cat (file=con," project link = ",nam,"\n")
    }
  }

  if (nprjs == 0) cat (file=con,"\n There are no projects under this Email address.\n")
  if (nprjs == 1) cat (file=con,"\n There is one project under this Email address.\n")
  if (nprjs  > 1) cat (file=con,"\n There are",nprjs,
                       "projects under this Email address.\n")
  close(con)

  mailCmd = gsub("\\{file\\}", rptFile, cfg("email", "send_command"))
  if (nchar(mailCmd) > 0 && sendEmail) {
    system(mailCmd)
  } else if (!sendEmail) {
    system(paste("cat", rptFile))
  } else {
    cat("[prjListEmail] Email sending disabled (send_command is empty)\n")
  }
  unlink (rptFile)
  nprjs
}
