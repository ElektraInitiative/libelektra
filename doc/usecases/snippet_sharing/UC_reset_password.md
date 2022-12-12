# Use Case: Reset Password

## Summary

- **Title:** Reset password
- **Scope:** Authentication
- **Level:** User Goal
- **Actors:** Anonymous user
- **Brief:** Any not logged-in user can reset their password by knowing their username and email address.

## Scenarios

- **Precondition:** Website opened
- **Main success scenario:** User successfully resets their password. An email containing the new password will be sent.
- **Alternative scenario:** The entered credentials could not be validated (do not match), which will result in an error being displayed.
- **Error scenario:** Technical problems prevent the password reset from completing. The user is informed about the issue.
- **Postcondition:** User has a newly generated password that they can change after a successful authentication.
- **Non-functional Constraints:**
  - Security mechanism
  - Recovery mechanism
