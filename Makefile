serve:
	python webapp/app.py

data_old: data/exposure_old_log.csv data/summary_old_log.csv data/survey_old_log.csv

data_mturk2: data/exposure_mturk2_log.csv data/summary_mturk2_log.csv data/survey_mturk2_log.csv

clean_data:
	rm data/*.csv

clean:
	rm *.log

data/exposure_%_log.csv: webapp/logs/experiment_%.log
	python webapp/extract_data.py --event=exposure --infile=$< --outfile=$@

data/summary_%_log.csv: webapp/logs/experiment_%.log
	python webapp/extract_data.py --event=summary --infile=$< --outfile=$@

data/survey_%_log.csv: webapp/logs/survey_%.log
	python webapp/extract_data.py --event=response --infile=$< --outfile=$@	

clean_slides:
	rm slides/*.slides.html

slides/%.slides.html: %.ipynb
	ipython nbconvert $*.ipynb --to=slides --config=slides/slides_config.py --stdout --reveal-prefix "http://cdn.jsdelivr.net/reveal.js/2.6.2" > $@

slides: slides/0-estimation-and-power.slides.html slides/1-planout-intro.slides.html slides/2-making-your-own-data.slides.html slides/4-analyzing-experiments.slides.html

