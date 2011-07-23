RCMD Sweave vignette.Rnw
pdflatex vignette.tex
pause
ps2pdf vignette.pdf out.pdf
del vignette_original.pdf /q
rename vignette.pdf vignette_original.pdf
del vignette.pdf /q
rename out.pdf vignette.pdf
pause