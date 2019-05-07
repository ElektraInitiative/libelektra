#!/usr/bin/env python
#
# @author René Schwaiger <sanssecours@me.com>
# @brief This script generates YAML data containing simple mappings.
# @date 03.05.2019
# @tags generate

from uuid import uuid4
from random import choice, randint, random


def generate_yaml(chance_key=0.7, number_scalars=10000):
    indentation = 0
    last_was_value = True  # We have to start with a key
    print('generated:')

    for times in range(1, number_scalars):
        quote = choice(['"', "'", ""])
        indentation += 1
        scalar = "{0}{1}{2}{1}".format(indentation * ' ', quote, uuid4())
        if last_was_value or random() < chance_key:
            # Print key
            print("{}:".format(scalar))
            last_was_value = False
        else:
            # Print value
            print(scalar)
            indentation = randint(0, abs(indentation - 2))
            last_was_value = True


if __name__ == '__main__':
    generate_yaml()
