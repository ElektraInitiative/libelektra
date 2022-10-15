# Use Case: Remove keys in ansible-libelektra

## Summary

- **Title:** Remove keys in ansible-libelektra
- **Scope:** ansible-libelektra
- **Level:** User Goal
- **Actors:** User (usually sysadmin)
- **Brief:** It should be possible to remove keys from the KDB using ansible-libelektra

## Scenarios

- **Precondition:** User specifies which key to delete in the playbook
- **Main success scenario:** The key exists on the target machine and will be deleted
- **Alternative scenario:** The key does not exists, nothing will be deleted
- **Error scenario:** -
- **Postcondition:** The specified key does not exist on the target machine
- **Non-functional Constraints:** -
