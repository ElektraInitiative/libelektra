#!/usr/bin/env python

from distutils.core import setup

setup(name='kdb-gen',
      version='0.1',
      description='Elektra Front End Generator',
      author='Markus Raab',
      author_email='elektra@markus-raab.org',
      url='http://www.libelektra.org/',
      scripts=['gen.py'],
      py_modules = [
          'cpp_support',
          'c_support',
          'gen_support',
          'opt_support',
          ],
      data_files = [
          ('templates', [
                 'template_genopt.h',
                 'template_genopt.c',
                 'template.h',
                 'template.hpp',
                 'template.man',
                 'template.html',
                ]
          )
      ]
)
