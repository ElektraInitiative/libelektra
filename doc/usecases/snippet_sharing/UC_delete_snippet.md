# Use Case: Delete Configuration Snippet

## Summary

- **Title:** Delete configuration snippet
- **Scope:** Entry Management
- **Level:** User Goal
- **Actors:** Authenticated user (snippet owner), Moderator
- **Brief:** Users can delete their own database entries. Moderators can also delete configuration snippets of other users.

## Scenarios

- **Precondition:** Authenticated, sufficient permissions, snippet details page opened
- **Main success scenario:** Configuration snippet has been deleted successfully. A success response is returned and the user, if necessary, redirected to the entry overview.
- **Alternative scenario:** The specified snippet cannot be found (wrong URI) or the user has insufficient permissions to delete it. Either way the user is informed about the error by a response and redirected to the overview if necessary.
- **Error scenario:** Technical problems prevent the deletion process from completing. The user is informed about the issue.
- **Postcondition:** Configuration snippet has been deleted from the database and can neither be found through search, nor be viewed in detail anymore.
- **Non-functional Constraints:**
  - Moderative feature
  - Essential functionality of the service
