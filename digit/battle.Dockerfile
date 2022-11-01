FROM python:3.7

USER root

ADD requirements /digit/requirements
RUN pip3 install -r /digit/requirements/base.txt
RUN pip3 install -r /digit/requirements/testing.txt

# --------------------- Application ---------------------
ADD . /digit
RUN chmod a+x /digit/scripts/run_battle_wsgi_server.sh

ENV PYTHONPATH=/digit

# --------------------- Command/Entry point ---------------------
ENTRYPOINT ["/digit/scripts/run_battle_wsgi_server.sh"]
