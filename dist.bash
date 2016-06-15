#!/bin/bash

VERSION=$(git describe --always --tags --long | sed -e 's,-,.,')

mkdir -p dist
stack install --local-bin-path=dist
mv -v dist/floq-reports dist/floq-reports-${VERSION}
