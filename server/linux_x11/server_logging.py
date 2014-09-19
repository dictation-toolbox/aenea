DEFAULT_CONFIG = {
    'version': 1,
    'formatters': {
        'generic': {
            'format': '%(asctime)s [%(levelname)-6s] [%(name)-s] %(message)s'
        }
    },
    'handlers': {
        'console': {
            'class': 'logging.StreamHandler',
            'level': 'DEBUG',
            'formatter': 'generic'
        }
    },
    'loggers': {
        'root': {
            'level': 'DEBUG',
            'handlers': [],
            'propagate': True
        },
        'server': {
            'level': 'DEBUG',
            'handlers': ['console']
        }
    }
}


def make_logging_config(aenea_config):
    log_file = getattr(aenea_config, 'LOG_FILE', None)
    console_level = getattr(aenea_config, 'CONSOLE_LOG_LEVEL', 'WARNING')
    file_level = getattr(aenea_config, 'FILE_LOG_LEVEL', 'INFO')

    logging_config = DEFAULT_CONFIG.copy()
    logging_config['handlers']['console']['level'] = console_level

    if log_file is not None:
        logging_config['handlers']['file'] = {
            'class': 'logging.handlers.RotatingFileHandler',
            'formatter': 'generic',
            'level': file_level,
            'filename': log_file,
            'mode': 'a',
            'backupCount': 3
        }
        logging_config['loggers']['server']['handlers'].append('file')

    return logging_config
