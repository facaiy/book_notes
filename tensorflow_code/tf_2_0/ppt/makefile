PPT = main
TUTOR = tutor
TEX_DIR = tex
PRESET = preset

# Option for latexmk
LATEXMK_OPT = -xelatex -gg -silent -f

.PHONY : all clean preset

all: $(PPT).pdf

$(PPT).pdf : $(PPT).tex $(TEX_DIR)/*.tex banyuan-ppt.cls makefile
	-latexmk $(LATEXMK_OPT) $(PPT)

$(TUTOR).pdf : $(TUTOR).tex $(TEX_DIR)/*.tex banyuan-ppt.cls makefile
	-latexmk $(LATEXMK_OPT) $(TUTOR)
	mv $(TUTOR).pdf ../

preset : clean
	cp -r -f -v tex $(PRESET)/ 
	cp -r -f -v figure $(PRESET)/

clean :
	latexmk -C
	-rm *.xdv *.bbl *.fls $(TEX_DIR)/*.xdv $(TEX_DIR)/*.aux $(TEX_DIR)/*.log $(TEX_DIR)/*.fls _tmp_.pdf *.xml *.nav *.snm
