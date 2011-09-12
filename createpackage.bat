rd ezsim /s /q
rd ezsim.Rcheck /s /q

r --no-save -q <createpackage.r 

copy DESCRIPTION ezsim\DESCRIPTION
copy NEWS ezsim\NEWS
copy vignette\vignette.Rnw ezsim\inst\doc\
copy vignette\vignette.pdf ezsim\inst\doc\
del ezsim\ezsim-package.Rd /s
del ezsim\Read-and-delete-me /s

R CMD check ezsim
R CMD build ezsim
R CMD build ezsim --binary

REM RCMD INSTALL ezsim_0.5.0.zip

pause