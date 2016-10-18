# Floq reporting API

[![Build Status](https://travis-ci.org/blankoslo/floq-reports-api.svg?branch=master)](https://travis-ci.org/blankoslo/floq-reports-api)

Report generation for `floq` time tracker.

## How to run locally
* Get Stack from https://github.com/commercialhaskell/stack/releases
* Setup: ```stack setup``` (installs GHC)
* Build: ```stack build``` (First time is really slow, but it caches)
* Run without docker: stack exec -- floq-reports --port 3000 --jwt-secret dev-secret-shhh \
  postgres://${PG_ENV_POSTGRES_USER}:$(./urlencode "${PG_ENV_POSTGRES_PASS}")@${PG_PORT_5432_TCP_ADDR}/${PG_ENV_POSTGRES_DB}
* * Add missing environment variables.
* Run with docker: [TODO]
