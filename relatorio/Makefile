TARGET = relatorio
TEX = pdflatex
FLAGS = -interaction=nonstopmode

all: $(TARGET).pdf

$(TARGET).pdf: $(TARGET).tex
	$(TEX) $(FLAGS) $(TARGET).tex

clean:
	rm -f $(TARGET).aux $(TARGET).log $(TARGET).pdf