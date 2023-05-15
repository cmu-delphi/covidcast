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
- A virtual environment is recommended to install and develop this package. Run `make install` to generate a 
virtual environment and install the package in editable mode (code changes will automatically propagate). Note that 
editable mode may not reveal errors when packaging files, so if the CI (which does not use editable mode) is failing 
for reasons related to accessing data files, try debugging by installing without editable mode (`make install-ci`)
- If you want to enter the virtual environment in your shell, you can run source env/bin/activate. Run deactivate 
to leave the virtual environment.
- To remove the virtual environment, you can run `make clean` or remove the `env/` folder.

__Style__
- Once your environment is set up, run `make lint` from `Python-packages/covidcast-py/` to run the lint commands.
- `mypy`, `pylint`, and `pydocstyle` are used for linting, with associated configurations for 
`pylint` in `.pylintrc` and for `mypy` in `mypy.ini`.

__Testing__
- Once your environment is set up, run `make test` from `Python-packages/covidcast-py/` to run the test commands.
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
2. Install the package and its dependencies locally with `make install`
3. Rebuild the documentation. The documentation lives in `docs/` and is built by
   [Sphinx](https://www.sphinx-doc.org/en/master/), which automatically reads
   the function docstrings and formats them. `docs/index.rst` contains the main
   documentation and the `.. autofunction::` directives insert documentation of
   specified functions.

   To rebuild the documentation, run

    ```sh
    make sphinx
    ```

    and then open `covidcast/docs/covidcast-py/html/index.html` to preview the
    new version.

    If you make changes to `index.rst`, you can simply run `make sphinx` to
    rebuild without needing to reinstall the package.
4. Build the release artifacts with `make build`.
5. Upload to PyPI. It should be as easy as

    ```sh
    twine upload dist/covidcast-0.0.9*
    ```

    with whatever glob matches the appropriate release. The PyPI username is
    `__token__`, and should be used with our COVIDcast API token.
