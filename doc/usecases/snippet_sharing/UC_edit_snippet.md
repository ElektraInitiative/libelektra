# Use Case: Edit Configuration Snippet

## Summary

- **Title:** Edit configuration snippet
- **Scope:** Entry Management
- **Level:** User Goal
- **Actors:** Authenticated user (snippet owner), Moderator
- **Brief:** Users can edit their own database entries. Moderators can edit all configuration snippets in the database, also the ones of other users.

## Scenarios

- **Precondition:** Authenticated, sufficient permissions
- **Main success scenario:** Changes to the configuration snippet have been stored to the database successfully. The user is informed about the success.
- **Alternative scenario:** The submitted inputs could not be validated or the entry for which changes are requested cannot be found. The user is informed about the error.
- **Error scenario:** Technical problems prevent the storage process from completing. The user is informed about the issue.
- **Postcondition:** Changes to the configuration snippet are stored in the database successfully and can be immediately seen in new requests of the snippet.
- **Non-functional Constraints:**
  - Moderative feature
