.PHONY = lint, test, install-requirements, build-and-install

install-requirements:
	pip install -r requirements_dev.txt
	pip install -r requirements_ci.txt

build-and-install: install-requirements
	python3 setup.py clean
	python3 setup.py sdist bdist_wheel
	pip3 install -e .

lint:
	pylint covidcast/ --rcfile .pylintrc
	mypy covidcast --config-file mypy.ini
	pydocstyle covidcast/

test:
	pytest tests/ -W ignore::UserWarning
