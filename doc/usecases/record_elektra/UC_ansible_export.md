# Use Case: Exporting configuration as Ansible playbook

## Summary

- **Title:** Exporting configuration as Ansible playbook
- **Scope:** Configuration
- **Level:** User goal
- **Actors:** User (usually a sysadmin)
- **Brief:** User exports certain configuration as Ansible playbook

## Scenarios

- **Precondition:** -
- **Main success scenario:**
  - User specifies which configuration to include in playbook
  - It must be possible to select only configuration that has changed (was recorded in session recording)
  - Ansible playbook is generated
- **Alternative scenario:** When no configuration exists, an empty playbook is generated
- **Error scenario:** -
- **Postcondition:**
  - Ansible playbook exists
- **Non-functional Constraints:**
  -
