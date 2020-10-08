# Developing the covidcast package

## Structure
From `covidcast/Python-packages/covidcast-py`, the Python library files are located in the 
`covidcast/` folder, with corresponding tests in `tests/covidcast/`. 
Currently, primary user facing functions across the modules are being imported in `covidcast/__init__.py` 
for organization and namespace purposes.

Sphinx documentation is in the `docs/` folder. See "Building the Package and Documentation" below 
for information on how to build the documentation.

The CI workflow is stored in the repo's top level directory in `.github/workflows/python_ci.yml`

## Development
These are general recommendations for developing. They do not have to be strictly followed, 
but are encouraged. 

__Environment__
- A virtual environment is recommended, which can be started with the following commands:

    ```sh
    python3 -m venv env
    source env/bin/activate
    ```
    this will create an `env/` folder containing files required in the environment, which
    is gitignored. The environment can be deactived by running `deactivate`, and reactived by
    rerunning `source env/bin/activate`. To create a new environment, you can delete the 
    `env/` folder and rerun the above commands if you do not require the old one anymore, 
    or rerun the above command with a new environment name in place of `env`.

__Style__
- `mypy`, `pylint`, and `pydocstyle` are used for linting, with associated configurations for 
`pylint` in `.pylintrc` and for `mypy` in `mypy.ini`.

__Testing__
- `pytest` is the framework used in this package.
- Each function should have corresponding unit tests. 
- Tests should be deterministic.
- Similarly, tests should not make network calls.

__Documentation__
- New public methods should have comprehensive docstrings and 
an entry in the Sphinx documentation.
- Usage examples in Sphinx are recommended.

## Building the Package and Documentation
The package is fairly straightforward in structure, following the basic
[packaging
documentation](https://packaging.python.org/tutorials/packaging-projects/) and a
few other pieces I found.

When you develop a new package version, there are several steps to consider. 
These are written from the `Python-packages/covidcast-py/` directory:

1. Increment the package version in `setup.py` and in Sphinx's `conf.py`.
2. Install the requirements needed to build the package and documentation:

    ```sh
    pip3 install -r requirements_dev.txt
    ```
   
3. Rebuild the package:

    ```sh
    python3 setup.py clean
    python3 setup.py sdist bdist_wheel
    ```

    Verify the build worked without errors.
4. Locally install the package:
    
    ```sh
    pip3 install .
    ```
   
5. Rebuild the documentation. The documentation lives in `docs/` and is built by
   [Sphinx](https://www.sphinx-doc.org/en/master/), which automatically reads
   the function docstrings and formats them. `docs/index.rst` contains the main
   documentation and the `.. autofunction::` directives insert documentation of
   specified functions.

   To rebuild the documentation, install the `sphinx` package and run

    ```sh
    cd docs/
    make clean
    make html
    ```

    and then open `covidcast/docs/covidcast-py/html/index.html` to preview the
    new version.

    If you make changes to `index.rst`, you can simply run `make html` to
    rebuild without needing to reinstall the package.
6. Upload to PyPI. It should be as easy as

    ```sh
    twine upload dist/covidcast-0.0.9*
    ```

    with whatever glob matches the appropriate release. The PyPI username is
    `__token__`, and should be used with our COVIDcast API token.
