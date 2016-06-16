FROM debian:jessie

RUN apt-get update && apt-get install -y libpq-dev

COPY dist/floq-reports /floq-reports

EXPOSE 3000

ENTRYPOINT ["/floq-reports"]
