#!/bin/bash -x

apt-get update

# Install Node
curl -sL https://deb.nodesource.com/setup_18.x | bash -
apt-get install -y nodejs


BOT_USER="discord_bot"

BOT_DIR="/opt/${BOT_USER}"

git clone git@github.com:tanelso2/blood-bowl-discord-bot.git "${BOT_DIR}"

useradd -d "${BOT_DIR}" ${BOT_USER}

mkdir "${BOT_DIR}/.ssh"

chown -R "${BOT_USER}:${BOT_USER}" "${BOT_DIR}"

# Install nvm for root
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash

# Install nvm for ${BOT_USER}
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | sudo -u "${BOT_USER}" bash

cp -s "${BOT_DIR}/deployment/discord-bot.service" "/etc/systemd/system/"

systemctl daemon-reload

systemctl enable discord-bot.service
systemctl start discord-bot.service
