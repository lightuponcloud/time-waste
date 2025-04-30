# Propylon Document Manager Assessment

The Propylon Document Management Technical Assessment is a simple (and incomplete) web application consisting of a basic API backend and a React based client.  This API/client can be used as a bootstrap to implement the specific features requested in the assessment description. 

## Getting Started
1. [Install Direnv](https://direnv.net/docs/installation.html)
2. [Install Pipenv](https://pipenv.pypa.io/en/latest/installation/)
3. This project requires Python 3.11 so you will need to ensure that this version of Python is installed on your OS before building the virtual environment.
4. `$ cp example.env .envrc`
5. `$ direnv allow .`
6. `$ pipenv install -r requirements/local.txt`.  If Python 3.11 is not the default Python version on your system you may need to explicitly create the virtual environment (`$ python3.11 -m venv .venv`) prior to running the install command. 
7. `$ pipenv run python manage.py migrate` to create the database.
8. `$ pipenv run python manage.py load_file_fixtures` to create the fixture file versions. This command also adds two test users.
9. `$ pipenv run python manage.py runserver 0.0.0.0:8001` to start the development server on port 8001.
10. Navigate to the client/doc-manager directory.
11. `$ npm install` to install the dependencies.
12. `$ npm start` to start the React development server.
13. Call `demo.sh`

[![Built with Cookiecutter Django](https://img.shields.io/badge/built%20with-Cookiecutter%20Django-ff69b4.svg?logo=cookiecutter)](https://github.com/cookiecutter/cookiecutter-django/)
[![Black code style](https://img.shields.io/badge/code%20style-black-000000.svg)](https://github.com/ambv/black)

## Settings

Moved to [settings](http://cookiecutter-django.readthedocs.io/en/latest/settings.html).

## Basic Commands

### Setting Up Your Users

- To create a **superuser account**, use this command:

      $ python manage.py createsuperuser


### Type checks

Running type checks with mypy:

    $ mypy propylon_document_manager

### Test coverage

To run the tests, check your test coverage, and generate an HTML coverage report:

    $ coverage run -m pytest
    $ coverage html
    $ open htmlcov/index.html

#### Running tests with pytest

    $ pytest


#### My Solution Description

Proposed solution consits of FileModel class, which is used to store files on filesystem by the path, based on FK.

Integer Foreign Key is converted to 9-digit filename, then split this string to 3 parts and then files are stored as 000/000/001/000000001 etc.

Reason: when many files are uploaded, performance deteriorates, as every file read operation will go over list of files on filesystem.

demo.sh bash script contains curl commands for authenticating using token, uploading file and retrieving file.
