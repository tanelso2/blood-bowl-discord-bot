#!/bin/bash

set -x

cd /opt/bb_bot/

echo "Hello everybody"

OUTPUT=$(git pull)

if [[ $? -ne 0 ]]; then
    echo "Something went wrong, doing nothing"
    exit 1
fi
if [[ $OUTPUT == *"Already up to date."* ]]; then
    echo "We done here"
else
    chown -R bb_bot:bb_bot /opt/bb_bot
    systemctl daemon-reload
    systemctl restart discord-bot.service
fi

