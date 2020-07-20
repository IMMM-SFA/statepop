from setuptools import setup, find_packages

def readme():
    with open('README.md') as f:
        return f.read()


def get_requirements():
    with open('requirements.txt') as f:
        return f.read().split()


setup(
    name='statepop',
    version='0.1.0',
    packages=find_packages(),
    url='https://github.com/IMMM-SFA/statepop',
    license='BSD 2-Clause',
    author='DeCiampa, CN',
    author_email='',
    description='TBD'
)
