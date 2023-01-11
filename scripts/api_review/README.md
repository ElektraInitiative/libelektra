# Elektra API Review Template Script

This python script generates new review files from the given template.

The script takes a list of methods as well as a template as input parameters.
The list of methods should be a file where each line contains a signature of a function that should be reviewed.
A review file will be generated for each line.
The template gets passed two variables, the signature and name of the function which can be accessed in the template via `{signature}` and `{name}` respectively.

For more infos about the usage of the script, one can run the command `./generate_review_files.py -h` to get a description about all the possible parameters that can be passed to the script.
