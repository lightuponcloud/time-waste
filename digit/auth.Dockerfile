FROM python:3.7

USER root

ADD requirements /digit/requirements
RUN pip3 install -r /digit/requirements/base.txt
RUN apt-get update && apt-get install -y vim less netcat-openbsd

# --------------------- Application ---------------------
ADD . /digit
RUN chmod a+x /digit/scripts/run_auth_wsgi_server.sh

ENV PYTHONPATH=/digit

# --------------------- Command/Entry point ---------------------
ENTRYPOINT ["/digit/scripts/run_auth_wsgi_server.sh"]
