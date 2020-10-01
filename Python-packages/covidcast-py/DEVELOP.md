# Developing the covidcast package

The package is fairly straightforward in structure, following the basic
[packaging
documentation](https://packaging.python.org/tutorials/packaging-projects/) and a
few other pieces I found.

When you develop a new package version, there are several steps to consider:

1. Increment the package version in `setup.py` and in Sphinx's `conf.py`.
2. Rebuild the package. You will need to install the `wheel` package:

    ```sh
    python3 setup.py clean
    python3 setup.py sdist bdist_wheel
    ```

    Verify the build worked without errors.
3. Locally install the package with `python3 setup.py install`.
4. Install dependencies  with `pip3 install -r requirements.txt`
5. Rebuild the documentation. The documentation lives in `docs/` and is built by
   [Sphinx](https://www.sphinx-doc.org/en/master/), which automatically reads
   the function docstrings and formats them. `docs/index.rst` contains the main
   documentation and the `.. autofunction::` directives insert documentation of
   specified functions.

   To rebuild the documentation, install the `sphinx` package and run

    ```sh
    cd docs/
    make html
    ```

    and then open `covidcast/docs/covidcast-py/html/index.html` to preview the
    new version.

    If you make changes to `index.rst`, you can simply run `make html` to
    rebuild without needing to reinstall the package.
4. Upload to PyPI. It should be as easy as

    ```sh
    twine upload dist/covidcast-0.0.9*
    ```

    with whatever glob matches the appropriate release. The PyPI username is
    `__token__`, and should be used with our COVIDcast API token.
