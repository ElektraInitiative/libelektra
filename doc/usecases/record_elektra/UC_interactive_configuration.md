# Use Case: Interactively creating system config

## Summary

- **Title:** Interactively creating system config
- **Scope:** Configuration
- **Level:** Summary
- **Actors:** System administrator
- **Brief:** In order to interactively create a workable system configuration, sometimes multiple iterations are required. 
It should be possible to modify the configuration on a single host, and then apply the changes onto all other hosts.
To integrate seamlessly into an Ansible workflow, it should also be possible to merge the changes into the original playbook.

## Scenarios

- **Precondition:** -
- **Main success scenario:** 
  - User connects to the host and changes settings in Elektra
  - User exports a playbook that contains either
    - only the changed keys
    - the original playbook with the current changes applied
- **Alternative scenario:** If the host was not configured using ansible-elektra, it is only possible to export a playbook with the changed keys
- **Error scenario:** -
- **Postcondition:** A working playbook is exported
- **Non-functional Constraints:** -

