# record-elektra Use Cases

The contained use cases are both from a developer side and user side.

## Developer-oriented Use Cases
- [Allow plugins to be notified when data changes](UC_change_notifications.md)
- [Provide an API to know which keys caused a conflict on merge](UC_cmerge_conflict_keys.md)
 
## User-oriented Use Cases 
- Concerning libelektra:
  - [Allow the recording of changes in the KDB](UC_record_changes.md)
  - [Export certain keys as Ansible playbook](UC_ansible_export.md)
  - [Allow for specification of how keys should be mapped to Ansible tasks](UC_ansible_specification.md) - this feature is pretty much optional
  
- Concerning ansible-libelektra:
  - [Allow for keys to be deleted using ansible-libelektra](UC_ansible-libelektra_remove_keys.md)
  - [Allow specifying how conflicts are to be handled in ansible-libelektra](UC_ansible-libelektra_merge_strategies.md)
  - [Report conflicts to the user when using Ansible](UC_ansible-libelektra_report_conflicts.md)
  - [Fail application of ansible-libelektra if certain keys differ in value](UC_ansible-libelektra_marked_keys.md)

