FROM python:3.7

USER root

ADD requirements /digit/requirements
RUN pip3 install -r /digit/requirements/base.txt
RUN pip3 install -r /digit/requirements/testing.txt

# --------------------- Application ---------------------
ADD . /digit
RUN chmod a+x /digit/manage.py


ENV PYTHONPATH=/digit

# --------------------- Command/Entry point ---------------------
ENTRYPOINT ["python", "/digit/manage.py", "run_worker"]
