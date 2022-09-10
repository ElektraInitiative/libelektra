# Use Case: Specification of how a mount point shall be transformed into an Ansible task

## Summary

- **Title:** Specification of how a mount point shall be transformed into an Ansible task
- **Scope:** Configuration
- **Level:** User Goal
- **Actors:** User (usually a sysadmin)
- **Brief:** Based on [Ansible Export](UC_ansible_export.md), it should be possible to describe how keys in Elektra are transformed into tasks in an Ansible playbook.

## Scenarios

- **Precondition:** -
- **Main success scenario:** 
  - The user defines (e.g. using meta keys) how to translate keys in a mountpoint into a task in an Ansible playbook
  - The user exports data of that mountpoints into an Ansible playbook
  - The correct task has been created
  
- **Alternative scenario:** 
  - The user does not explicitly specify how to map keys to an Ansible task
  - The user exports data of that mountpoints into an Ansible playbook
  - The ansible-elektra task is used as fallback
  
- **Error scenario:** -
- **Postcondition:** -
- **Non-functional Constraints:**
  -
