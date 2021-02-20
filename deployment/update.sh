#!/bin/bash -x

cd /opt/bb_bot/

git pull

systemctl daemon-reload
systemctl restart discord-bot.service
