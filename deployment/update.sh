#!/bin/bash -x

cd /opt/bb_bot/

OUTPUT=$(git pull)

if [[ $OUTPUT == "Already up to date" ]]; then
    echo "We done here"
else
    systemctl daemon-reload
    systemctl restart discord-bot.service
fi

