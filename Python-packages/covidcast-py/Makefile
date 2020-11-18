.PHONY = venv, install, build, lint, test, clean


venv:
	python3 -m venv env

install:  venv
	. env/bin/activate; \
	python -m pip install --upgrade pip; \
	pip install -r requirements_dev.txt; \
	pip install -e .

build:
	python setup.py clean
	python setup.py sdist bdist_wheel

lint:
	. env/bin/activate; \
	pylint covidcast/ --rcfile .pylintrc; \
	mypy covidcast --config-file mypy.ini; \
	pydocstyle covidcast/

test:
	. env/bin/activate; \
	pytest tests/ -W ignore::UserWarning

clean:
	rm -rf env
