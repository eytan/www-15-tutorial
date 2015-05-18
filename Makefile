serve:
	python webapp/app.py

data: data/exposure_log.csv data/summary_log.csv data/survey_log.csv

clean_data:
	rm data/*.csv

data/exposure_log.csv: webapp/logs/experiment.log
	python webapp/extract_data.py --event=exposure --infile=$< --outfile=$@

data/summary_log.csv: webapp/logs/experiment.log
	python webapp/extract_data.py --event=summary --infile=$< --outfile=$@

data/survey_log.csv: webapp/logs/survey.log
	python webapp/extract_data.py --event=response --infile=$< --outfile=$@	

%.slides.html: %.ipynb
	ipython nbconvert --to=slides $*.ipynb --config=slides_config.py

slides: 4-analyzing-experiments.slides.html

