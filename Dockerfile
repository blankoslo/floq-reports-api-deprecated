FROM debian:9

RUN DEBIAN_FRONTEND=noninteractive \
    apt-get update && \
    apt-get install -y -q libpq-dev

COPY dist/floq-reports /floq-reports
COPY deployment/urlencode /urlencode

EXPOSE 3000

CMD exec /floq-reports --jwt-secret $POSTGREST_JWT_SECRET \
  postgres://${PG_ENV_POSTGRES_USER}:$(./urlencode "${PG_ENV_POSTGRES_PASS}")@${PG_PORT_5432_TCP_ADDR}/${PG_ENV_POSTGRES_DB}

