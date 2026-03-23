!ODE SEGMENT GLBLCNTL
!----------
! COMMON $Id$
!----------
!
character(len=250) :: keywordfile,restartfile,stopptfile
!
integer :: firstWrite,fvsRtnCode,jdstash,jstash,majorstopptcode, &
     majorstopptyear,maxStoppts,minorstopptcode,minorstopptyear, &
     originalRestartCode,oldstopyr,readFilePos,restartcode, &
     seekReadPos,stopstatcd
!
common /GLBLCNTLC/ keywordfile,restartfile,stopptfile
!
common /GLBLCNTL/ firstWrite,fvsRtnCode,jdstash,jstash, &
     majorstopptcode,majorstopptyear,maxStoppts,minorstopptcode, &
     minorstopptyear,oldstopyr,originalRestartCode,readFilePos, &
     restartCode,seekReadPos,stopstatcd
!
!----------
!  VARIABLE DEFINITIONS:
!----------
!           firstWrite = 1 if putstd has not yet happened once, 0 if it has
!           fvsRtnCode = -1 no start up has happened.
!                         0 if all is well, continue processing,
!                         1 if there is a error, return to caller,
!                         2 if STOP was found on the keyword file
!                           or EOF on the restart file.
!                           (not an error, but do not continue).
!              jdstash = restart file unit number, if -1 then no file can be
!                        read.
!               jstash = stoppoint file unit number, if -1, then no file is
!                        output.
!      majorstopptcode = stoppoint request code (stopWithStore).
!      majorstopptyear = stopptyear request code (stopWithStore).
!           maxStoppts = the maximum value of minorstopptcode permitted.
!      minorstopptcode = stoppoint request code (stopWithoutStore).
!                        The "stopptcode" is the location code from where the
!                        program is being asked to return to the caller.
!                        codes are:
!                        0 = Never stop.
!                       -1 = Stop at every stop point.
!                        1 = Stop just before the first call to the
!                            Event Monitor.
!                        2 = Stop just after the first call to the
!                            Event Monitor.
!                        3 = Stop just before the second call to the
!                            Event Monitor.
!                        4 = Stop just after the second call to the
!                            Event Monitor.
!                        5 = Stop after growth has been computed, but prior to
!                            adding the growth.
!                        6 = Stop just before the estab routines are called.
!                        7 = Stop before missing value dubbing.
!      minorstopptyear = stopptyear request code (stopWithoutStore).
!                        codes are:
!                        0 = Never stop, even if minorstopptcode == -1.
!                       -1 = Stop at every cycle.
!                       >0 = Stop during the cycle that contains the year.
!         keywordfile = keyword file name.
!           oldstopyr = if restarting form a restart file, it is the stop year
!                       from the last run. -1 otherwise. This value is used to
!                       to signal if the program is running form a restart file
!                       verses from a keyword file.
! originalRestartCode = restartCode used when a stopWithStore was done.
!         readFilePos = The file position where the current stand starts, can
!                       be used to recall the stand for another alternative.
!         restartcode = restart code. This value is the loction code
!                       where the processing ended. That is, it is the value
!                       of minor|major stopptcode when the program returned
!                       to the caller. If -1, then processing should stop.
!         restartfile = restart file name. If blank, the program assumes that
!                       its memory state is consistent with the last check
!                       point.
!         seekReadPos = A semaphore, readFilePos for first read, -1 for rest of
!                       current stand. This support re-reading a stand.
!          stopptfile = stoppoint file name. If blank, or [none], the program
!                       does not write a stoppoint file, it simply returns.
!          stopstatcd = 0=no stop has been "caused" (this is the initial
!                       condition).
!                       1=stop was caused by a stopWithStore
!                       2=stop was caused by a simulation end signal
!                       3=stop was caused by a stopWithoutStore
!                       4=stop was caused by a the reload of a stand
!
!-----END SEGMENT
