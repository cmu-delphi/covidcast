import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="covidcast",
    version="0.0.9", # also update in docs/conf.py
    author="Alex Reinhart",
    author_email="areinhar@stat.cmu.edu",
    description="Access COVID-19 data through the Delphi COVIDcast API",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://cmu-delphi.github.io/covidcast/covidcast-py/html/",
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.6',
    install_requires=[
        'pandas',
        'requests',
        'delphi-epidata>=0.0.7',
        'geopandas',
        'matplotlib',
        'numpy',
        'descartes'
    ],
    package_data={'covidcast': ['shapefiles/county/*', 'shapefiles/state/*']}
)
