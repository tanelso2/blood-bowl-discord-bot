#!/bin/bash -x

cd /opt/bb_bot/

git pull

systemctl restart discord-bot.service
