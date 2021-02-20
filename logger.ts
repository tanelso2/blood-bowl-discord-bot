import pino from 'pino';

export const logger = pino(
  {
    level: process.env.LOG_LEVEL || 'info',
    prettyPrint: {
      colorize: true,
      ignore: 'pid,hostname,module',
      translateTime: true,
      messageFormat: '({module}) {msg}'
    }
  });
