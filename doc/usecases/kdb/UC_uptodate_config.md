# Use Case: Keeping Configuration Up-to-date

## Summary

- **Scope:** `libelektra-kdb`
- **Level:** Developer Goal
- **Actors:** Application, Elektra
- **Brief:** Application wants up-to-date configuration data

## Scenarios

- **Precondition:** -
- **Main success scenario:** Notifications
  - Application requests asynchronous callbacks via the notification system
  - [Application loads configuration](UC_load_config.md)
  - Every time the configuration changes afterwards, Elektra sends a notification to Application
  - When receiving a notification, Application may need to [load the configuration](UC_load_config.md) again
- **Alternative scenario:** Polling
  - [Application loads configuration](UC_load_config.md) repeatedly in regular intervals
- **Error scenario:** -
- **Postcondition:** -
- **Non-functional Constraints:**
  - Notifications SHOULD use standard communication systems
  - Notifications MUST be fast to send and receive, because there SHOULD NOT be a rate limit
