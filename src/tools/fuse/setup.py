from setuptools import setup, find_packages
import pathlib

here = pathlib.Path(__file__).parent.resolve()
long_description = (here / 'README.md').read_text(encoding='utf-8')

setup(
    name='elektra_fuse',

    version='1.0.0',

    description='Mount libelektra\'s key database as a FUSE-filesystem.', 

    long_description=long_description,

    long_description_content_type='text/markdown',

    url='https://www.libelektra.org/', 

    author='Alexander Firbas',

    author_email='alexanderfirbas@gmail.com',

    keywords='fuse, elektra, kdb',

    package_dir={'': 'src'},

    packages=find_packages(where='src'),

    python_requires='>=3.6',

    install_requires=['fusepy>=3.0.1', "psutil>=5.8.0"], 
)
