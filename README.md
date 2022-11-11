# Blood Bowl Discord Bot

[![Build Status](https://github.com/tanelso2/blood-bowl-discord-bot/actions/workflows/pr-testing.js.yml/badge.svg)](https://github.com/tanelso2/blood-bowl-discord-bot/actions/workflows/pr-testing.js.yml)

A discord bot to help manage leagues of [Blood Bowl 2](https://store.steampowered.com/app/236690/Blood_Bowl_2/).

This bot does not use the BB2 API and instead must be configured via a yaml file. (see [sample-league.yaml](./sample-league.yaml))

A [script](./scripts/make-league.ts) can be used to generate a league file, but it is very tailored to my use and opens temp files in your $EDITOR for you to edit, often without any prompting. I should clean that up.

## Why does this exist?

Because knowing what coach names in Blood Bowl 2 match up with which names in Discord is a pain. And so Discord was filled with people asking "Hey, who is playing the Lizards?" and it took forever for them to get an answer, and then they had to ping the other person. Also just opening the Blood Bowl 2 client to check your opponent's coach name was slow. The UI in the game can be slow. The bot remembers all that information so we don't have to do that routine all the time.

## What else does the bot do?

* Announces games
* prints your schedule for the league
* calculates odds for rolls
* displays info for the different team types
* displays info for the star players

Basically, a lot of these are info you can find in the game, but it is annoying or difficult to do for some reason.

# Development

## Dependencies

Use [nvm](https://github.com/nvm-sh/nvm) to use the proper version of Node.js

```bash
nvm install

nvm use
```

```bash
npm install
```

## Building

```bash
npm run build
```

## Testing

```bash
npm run test
```

## Linting

```bash
npm run lint
```

## Running in Dev mode

Requires a [config.json](https://discordjs.guide/creating-your-bot/#using-config-json) file with your [bot api token from Discord](https://discordjs.guide/preparations/setting-up-a-bot-application.html#your-bot-s-token)

```bash
npm run dev
```
