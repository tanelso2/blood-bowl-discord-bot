#!/bin/bash -x

npm install

npm run build

node build/index.js league.yaml
