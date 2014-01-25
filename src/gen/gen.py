#!/usr/bin/python

import sys
import ConfigParser
from Cheetah.Template import Template

def main():
    if len(sys.argv) != 3:
        print "Usage: ", sys.argv[0], " <parameter specification> <template>"
        sys.exit()

    parser = ConfigParser.ConfigParser(allow_no_value=True)
    parser.read(sys.argv[1])

    parameterSpecification={}
    parameterSpecification['parameters']={}
    for section in parser.sections():
        parameterSpecification['parameters'][section]={}
        fallback={}
        override={}
        if parser.has_option(section, "fallback"):
                fallback = parser.get(section, "fallback").split(' ')
        if parser.has_option(section, "override"):
                override = parser.get(section, "override").split(' ')
        for item in parser.items(section):
            parameterSpecification['parameters'][section][item[0]] = item[1]

    #print parameterSpecification

    template = Template(file=sys.argv[2], searchList=[parameterSpecification])
    print template

main()
