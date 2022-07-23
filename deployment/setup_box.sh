#!/bin/bash -x

apt-get update

# Install Node
curl -sL https://deb.nodesource.com/setup_12.x | bash -
apt-get install -y nodejs


BB_DIR=/opt/bb_bot

git clone git@github.com:tanelso2/blood-bowl-discord-bot.git "${BB_DIR}"

useradd -d "${BB_DIR}" bb_bot

mkdir "${BB_DIR}/.ssh"

chown -R bb_bot:bb_bot "${BB_DIR}"

# Give bb_bot permission to run update script as sudo (for automated deployments)
SUDOERS_FILE="/etc/sudoers.d/10-bot-ops"
cat <<EOF > "${SUDOERS_FILE}"

bb_bot ALL=(ALL) !ALL
bb_bot ALL=NOPASSWD: /opt/bb_bot/deployment/update.sh

EOF
chmod 0440 "${SUDOERS_FILE}"

# Install nvm for root
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash

# Install nvm for bb_bot
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | sudo -u bb_bot bash

cp -s "${BB_DIR}/deployment/discord-bot.service" "/etc/systemd/system/"

# Enable auto-updates
CRON_FILE=/etc/cron.d/bb_bot_update
cat <<EOF > "$CRON_FILE"
* * * * * bb_bot sudo $HOME/deployment/update.sh
EOF

systemctl daemon-reload

systemctl enable discord-bot.service
systemctl start discord-bot.service
