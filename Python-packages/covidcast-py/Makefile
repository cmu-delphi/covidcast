.PHONY = venv, install-ci, install, build, lint, test, clean


venv:
	python3 -m venv env

install-ci: venv
	. env/bin/activate; \
	python -m pip install --upgrade pip; \
	pip install -r requirements_dev.txt; \
	pip install .

install:  venv
	. env/bin/activate; \
	python -m pip install --upgrade pip; \
	pip install -r requirements_dev.txt; \
	pip install -e .

build:
	python3 setup.py clean
	python3 setup.py sdist bdist_wheel

lint:
	. env/bin/activate; pylint covidcast/ --rcfile .pylintrc
	. env/bin/activate; pydocstyle covidcast/

test:
	. env/bin/activate; \
	pytest tests/ -W ignore::UserWarning

clean:
	rm -rf env
