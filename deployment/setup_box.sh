#!/bin/bash -x

apt-get update

# Install Node
curl -sL https://deb.nodesource.com/setup_12.x | bash -
apt-get install -y nodejs

BB_DIR=/opt/bb_bot

git clone https://github.com/tanelso2/blood-bowl-discord-bot "${BB_DIR}"

useradd -d "${BB_DIR}" bb_bot

mkdir "${BB_DIR}/.ssh"

cat <<EOF > "${BB_DIR}/.ssh/authorized_keys"

ecdsa-sha2-nistp521 AAAAE2VjZHNhLXNoYTItbmlzdHA1MjEAAAAIbmlzdHA1MjEAAACFBAG6VWZqT4z9ooOzOMWShpaZZD6WcI61qQR+aDd2palxMmpQ3F9qL6rzw0D2MTUd+7Obz6iBx2Cd3zxD4W1PX879pQB5TROtTrd0u5x/E5DaVRqwuErMb5PgNINqycvDmJXwXuAPvrd3H/poLDghbKMKF1V15dgEk/UwdONczapW9S6/gA== tnelson@Thomass-MacBook-Pro.local

EOF

cp -s "${BB_DIR}/deployment/discord-bot.service" "/etc/systemd/system/"

systemctl daemon-reload

systemctl enable discord-bot.service
systemctl start discord-bot.service

chown -R bb_bot:bb_bot "${BB_DIR}"

