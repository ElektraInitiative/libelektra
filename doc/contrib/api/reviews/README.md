# Elektra API Review

This folder contains all API design reviews conducted on `libelektra`.
Below you can find a short description of the review process.

## General guidelines

This review is performed for each function separately.

Each function should be evaluated according to the checklist in the provided template.

The template contains a short section with metadata about the review (time, reviewer) that must also be filled out.

A function can be reviewed multiple times by different people in order to improve upon the results.
Different people find different issues and more reviewers usually means that more problems get uncovered.

Multiple reviews of the same function should be put into the same file below each other.

The creator of the function is the moderator of the review, persons performing the review are called the reviewers.

The review should be performed by a person that did not directly work on the design of the function that is being reviewed.

## Review process

The moderator creates the initial file for the review when submitting the PR.
The reviewer then creates a pull request, that adds his review to the review file.

The reviewer judges for every bullet point, whether the function does / does not fulfill the bullet point.

For every unfulfilled point a short explanation has to be given why the reviewer thinks the function does not fulfill the respective bullet point.
This is done by providing a short description of the issue below the respective bullet point.

For every bullet point that is deemed not applicable, the reviewer also has to give a short explanation of why he feels like the bullet point is not applicable.

If the reviewer notices issues with the function during the review, that are not covered by any existing bullet point, those should be added to the summary as well.

The reviewer can ask for clarification on design decisions that seem unclear in the discussion of the PR.

After the review has been completed, the moderator creates an issue for every unfulfilled bullet point so it can be discussed and subsequently fixed by the Elektra Initiative.
Then the review PR can be merged.

## Checklist Legend

- [ ] not fulfilled  
       This is the reason for why its is not fulfilled
- [x] fulfilled
- not applicable

## Templates

The template and the script for generating review files based on that template
can be found [here](/scripts/api_review/README.md)
