# Use Case: Configure different hosts with the same playbook

## Summary

- **Title:** Configure different hosts with the same playbook
- **Scope:** Configuration
- **Level:** User goal
- **Actors:** User (usually sysadmin)
- **Brief:** It should be possible to configure different hosts with the same playbook. 
  Furthermore, when doing session recording and exporting the merge playbook, the changed keys should be updated in the appropriate spots in the playbook.

## Scenarios

- **Precondition:** -
- **Main success scenario:** The user runs a playbook
- **Alternative scenario:** 
  - The user changes keys that are specific to this host/group of hosts
  - The user exports a merged playbook
  - The user runs the exported playbook
- **Error scenario:** -
- **Postcondition:** All hosts have their appropriate configuration applied
- **Non-functional Constraints:** -

