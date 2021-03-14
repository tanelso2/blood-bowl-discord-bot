#!/bin/bash -x

export PATH="$HOME/.local/bin:$PATH"

nvm use

npm install

npm run build

node build/index.js league.yaml
