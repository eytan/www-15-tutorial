serve:
	python webapp/app.py

data_old: data/exposure_old_log.csv data/summary_old_log.csv data/survey_old_log.csv

data_mturk2: data/exposure_mturk2_log.csv data/summary_mturk2_log.csv data/survey_mturk2_log.csv

clean_data:
	rm data/*.csv

data/exposure_%_log.csv: webapp/logs/experiment_%.log
	python webapp/extract_data.py --event=exposure --infile=$< --outfile=$@

data/summary_%_log.csv: webapp/logs/experiment_%.log
	python webapp/extract_data.py --event=summary --infile=$< --outfile=$@

data/survey_%_log.csv: webapp/logs/survey_%.log
	python webapp/extract_data.py --event=response --infile=$< --outfile=$@	

slides/%.slides.html: %.ipynb
	ipython nbconvert $*.ipynb --to=slides --config=slides/slides_config.py --stdout > $@

slides: slides/4-analyzing-experiments.slides.html

