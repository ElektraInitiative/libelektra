# Use Case: Search for Users

## Summary

- **Title:** Search for users
- **Scope:** Search
- **Level:** User Goal
- **Actors:** Administrators
- **Brief:** Administrators can view and search all registered users.

## Scenarios

- **Precondition:** Authenticated, sufficient permissions
- **Main success scenario:** Administrator is displayed a list of possible candidates that match the entered search parameters.
- **Alternative scenario:** The search did not find any user matching the given search parameters, which will result in an empty list being displayed.
- **Error scenario:** Technical problems prevent the search from succeeding. The administrator is informed about the issue.
- **Postcondition:** The search results offer additional functionality like editing and deletion of users.
- **Non-functional Constraints:**
  - Administrative feature
