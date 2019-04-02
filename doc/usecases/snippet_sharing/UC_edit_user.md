# Use Case: Edit User

## Summary

- **Title:** Edit user
- **Scope:** User Management
- **Level:** User Goal
- **Actors:** Authenticated user, Administrator
- **Brief:** Users can edit their own account information (e.g. change password). Administrators can change all user accounts in the database. Administrators can also change permissions of users.

## Scenarios

- **Precondition:** Authenticated, sufficient permissions
- **Main success scenario:** Changes to the user account have been stored to the database successfully. The operating user is informed about the success.
- **Alternative scenario:** The submitted inputs could not be validated or the user entry for which changes are requested cannot be found. The user is informed about the error.
- **Error scenario:** Technical problems prevent the storage process from completing. The operating user is informed about the issue.
- **Postcondition:** Changes to the user account are stored in the database successfully and are from now on valid for further requests (e.g. authentication attempts).
- **Non-functional Constraints:**
  - Administrative feature
