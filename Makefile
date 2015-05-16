
%.slides.html: %.ipynb
	ipython nbconvert --to=slides $*.ipynb

slides: 4-analyzing-experiments.slides.html
