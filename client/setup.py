"""Installation script for aenea client."""
from setuptools import setup

setup(
    name='aenea',
    version='1.0',
    description='dragonfly via proxy',
    author='Alex Roper',
    author_email='alex@aroper.net',
    python_requires='>=2.7,<3',
    packages=['aenea'],
    install_requires=['dragonfly'])
