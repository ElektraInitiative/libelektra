#!/usr/bin/fish

# Not intended to be executed directly, shebang exists only to suppress `checkbashisms` warning

# -- Functions -----------------------------------------------------------------------------------------------------------------------------

# ===========
# = General =
# ===========

function __includes_options -d 'Check if the values starting at position 3 contain the options specified at position one and two'
    set -l opt_long $argv[1]
    set -l opt_short $argv[2]
    set -l input $argv[3..-1]

    test -n $opt_long
    set opt_long "--$opt_long"
    test -n $opt_short
    set opt_short "-\w*$opt_short"
    set -l options (string join -- '|' $opt_long $opt_short)

    string match -r -- $options "$input"
end

# =========
# = Input =
# =========

function __input_includes -d 'Check if the current command buffer contains one of the given values'
    set -l times 1
    if string match -qr -- '\d+' argv[-1]
        set times $argv[-1]
        set -e argv[-1]
    end

    for input in (commandline -op)
        if contains -- $input $argv
            set times (math $times - 1)
            test $times -le 0
            and return 0
        end
    end
    return 1
end

function __input_includes_options -d 'Check if the current command buffer contains one of the given options'
    __includes_options $argv[1] $argv[2] (commandline -op)
end

function __input_left_includes_options -d 'Check if the input to the left of the current buffer contains one of the given options'
    __includes_options $argv[1] $argv[2] (commandline -opc)
end

# =======
# = KDB =
# =======

# << Input >>

function __fish_kdb_is_namespace -d 'Check if the given argument is a namespace'
    string match -r -- '^(dir|proc|spec|system|user|\/).*' "$argv" >/dev/null
end

function __fish_kdb__number_arguments_input_left -d 'Return the number of arguments to the left of the current cursor position'
    set -l input (commandline -opc)
    set -l number_arguments (count $input)

    # Ignore options
    set -l options (string match -ra -- '--(?:editor|namespace|plugins-config|profile|resolver|strategy)(\s*\w+)?|-\S+' $input)
    set -l number_options (count $options)
    set -l number_arguments (math $number_arguments - $number_options)

    echo $number_arguments
end

function __fish_kdb_subcommand -d 'Check for and print the current kdb subcommand'
    set -l input (commandline -opc)

    test (count $input) -le 1
    and return 1

    set -l subcommand $input[2]
    if contains -- $subcommand (__fish_kdb_print_subcommands)
        echo "$subcommand"
        return 0
    end

    return 1
end

function __fish_kdb_subcommand_exists_does_not_include -d 'Check if a kdb subcommand exist and does not include any of the given commands'
    set -l subcommand (__fish_kdb_subcommand)

    test -z $subcommand
    and return 1

    not contains -- "$subcommand" $argv
end

function __fish_kdb_subcommand_includes -d 'Check if the current kdb subcommand is one of the given subcommands'
    set -l subcommand (__fish_kdb_subcommand)
    contains -- "$subcommand" $argv
end

# << Completion Checks >>

function __fish_kdb_subcommand_needs_metanames -d 'Check if the current command needs a meta-name completion'
    not __fish_kdb_subcommand_includes meta-get meta-rm meta-set
    and return 1

    test (__fish_kdb__number_arguments_input_left) -eq 3
end

function __fish_kdb_needs_namespace -d 'Check if the current command needs a namespace completion'
    set times $argv[-1]
    set -e argv[-1]
    set commands $argv

    not __fish_kdb_subcommand_includes $commands
    and return 1

    __fish_kdb_is_namespace (commandline -t)
    and return 0

    for input in (commandline -opc)
        __fish_kdb_is_namespace $input
        and set times (math $times - 1)

        test $times -le 0
        and return 1
    end

    return 0
end

function __fish_kdb_needs_plugin -d 'Check if the current command needs a plugin completion'
    if __fish_kdb_subcommand_includes check info
        not __input_includes (__fish_kdb_print_plugins)
        return $status
    end
    __fish_kdb_subcommand_includes global-mount gmount
    and test (__fish_kdb__number_arguments_input_left) -eq 2
end

function __fish_kdb_subcommand_convert_needs_storage_plugin -d 'Check if the subcommand convert needs a storage plugin completion'
    __fish_kdb_subcommand_includes convert
    and not __input_includes (__fish_kdb_print_storage_plugins) 2
end

function __fish_kdb_subcommand_fstab_needs_filesystem -d 'Check if the subcommand fstab needs a filesystem completion'
    __fish_kdb_subcommand_includes fstab
    and test (__fish_kdb__number_arguments_input_left) -eq 5
end

function __fish_kdb_subcommand_info_needs_clause_name -d 'Check if the subcommand info needs a clause name completion'
    __fish_kdb_subcommand_includes info
    and test (__fish_kdb__number_arguments_input_left) -eq 3
end

function __fish_kdb_subcommand_mount_needs_namespace -d 'Check if the subcommand mount needs a namespace completion'
    __fish_kdb_subcommand_includes mount
    and test (__fish_kdb__number_arguments_input_left) -eq 3
end

function __fish_kdb_subcommand_mount_needs_plugin -d 'Check if the subcommand mount needs a plugin completion'
    __fish_kdb_subcommand_includes mount
    and test (__fish_kdb__number_arguments_input_left) -ge 4
end

function __fish_kdb_subcommand_needs_storage_plugin -d 'Check if the current subcommand need a storage plugin completion'
    set -l subcommands editor export import

    not __fish_kdb_subcommand_includes $subcommands
    and return 1

    __fish_kdb_needs_namespace $subcommands 1
    and return 1
    not __input_includes (__fish_kdb_print_storage_plugins)
end

function __fish_kdb_subcommand_remount_needs_namespace -d 'Check if the subcommand remount needs a namespace completion'
    not __fish_kdb_subcommand_includes remount
    and return 1

    set -l number_arguments (__fish_kdb__number_arguments_input_left)
    test $number_arguments -eq 3 -o $number_arguments -eq 4
end

function __fish_kdb_subcommand_smount_needs_plugin -d 'Check if the subcommand spec-mount needs a plugin completion'
    __fish_kdb_subcommand_includes spec-mount
    and test (__fish_kdb__number_arguments_input_left) -eq 3
end

# ============
# = Printers =
# ============

function __fish_kdb_print_clause_names -d 'Print a list of possible clause names'
    printf '%s\n' licence metadata needs placements provides recommends status version
end

function __fish_kdb_print_metanames -d 'Print a list of possible meta-names'
    set -l metanames 'order' 'comment' 'line' 'fallback/#' 'override/#' 'namespace/#' 'default' 'context' 'callback/_' 'binary' 'array'
    set metanames $metanames 'mountpoint' 'infos' 'config' 'opt' 'opt/long' 'env' 'see/#' 'rationale' 'description' 'example'
    set metanames $metanames 'rename/toupper' 'rename/tolower' 'rename/cut' 'rename/to' 'origname' 'conflict/_' 'array/range' 'required'
    set metanames $metanames 'logs/_/_' 'warnings' 'error' 'struct' 'check/type' 'check/type/min' 'check/type/max' 'check/format'
    set metanames $metanames 'check/path' 'check/validation' 'check/validation/message' 'check/validation/match'
    set metanames $metanames 'check/validation/ignorecase' 'check/validation/invert' 'check/range' 'check/enum' 'check/calculate'
    set metanames $metanames 'check/condition' 'deprecated'
    set metanames $metanames 'info/atime' 'info/ctime' 'info/gid' 'info/inode' 'info/mode' 'info/mtime' 'info/size' 'info/uid'
    set metanames $metanames 'internal/<plugin>/*' 'source' 'dependency/control' 'dependency/value'
    set metanames $metanames 'application/name' 'application/version' 'restrict/write' 'restrict/null' 'restrict/binary' 'restrict/remove'
    set metanames $metanames 'evaluate/<language>' 'uid' 'gid' 'mode' 'atime' 'mtime' 'ctime' 'spec' 'proc' 'dir' 'user' 'system'
    set metanames $metanames 'comment/#' 'comment/#/start' 'comment/#/space' 'csv/order' 'crypto/encrypt' 'crypto/salt'
    printf '%s\n' $metanames
end

function __fish_kdb_print_namespaces -d 'Print a list of possible namespace completions'
    set -l namespace (commandline -ct)
    kdb complete --max-depth=1 -- "$namespace"
end

function __fish_kdb_print_plugins -d 'Print a list of available plugins'
    kdb plugin-list
end

function __fish_kdb_print_non_resolver_plugins -d 'Print a list of non-resolver plugins'
    kdb plugin-list | string match -vr '.*resolver.*' | string match -vr (kdb plugin-list resolver | string join '|')
end

function __fish_kdb_print_resolver_plugins -d 'Print a list of available resolver plugins'
    kdb plugin-list resolver
end

function __fish_kdb_print_storage_plugins -d 'Print a list of available storage plugins'
    kdb plugin-list storage
end

function __fish_kdb_print_subcommands -d 'Print a list of kdb subcommands'
    set -l commands (kdb list-commands $argv)
    if contains -- $argv -v
        set commands (printf '%s\n' $commands | awk '{if(NR>1)print}' | sed 's/\.$//')
    end
    printf '%s\n' $commands
end

# ===========
# = Options =
# ===========

function __fish_kdb_add_option -d 'Add suggestions for a certain option to multiple kdb subcommands'
    set -l completion_function $argv[1]
    set -l opt_long $argv[2]
    set -l opt_short $argv[3]
    set -l description $argv[4]

    test (count $argv) -gt 4
    and set -l arguments $argv[5]

    test -n $opt_long
    and set -l options -l $opt_long
    test -n $opt_short
    and set -l options $options -s $opt_short

    if set -q arguments
        set -l exclusion '-x'
        test (count $argv) -gt 5
        and set -l exclusion $argv[6]

        set completion_function "if $completion_function
                                      __input_left_includes_options \"$opt_long\" \"$opt_short\"
                                      or not __input_includes_options \"$opt_long\" \"$opt_short\"
                                  else
                                      false
                                  end"
        complete -c kdb $exclusion -n "$completion_function" $options -a $arguments -d "$description"
    else
        set completion_function "$completion_function; and not __input_includes_options \"$opt_long\" \"$opt_short\""
        complete -c kdb -f -n "$completion_function" $options -d "$description"
    end
end

# << Completion Checks >>

function __fish_kdb_subcommand_supports_common_options -d 'Check if the current subcommand supports common options'
    __fish_kdb_subcommand_exists_does_not_include help list-tools qt-gui
end

function __fish_kdb_subcommand_supports_option_color -d 'Check if the current subcommand supports colored output'
    __fish_kdb_subcommand_exists_does_not_include complete help list-tools qt-gui
end

function __fish_kdb_subcommand_supports_option_debug -d 'Check if the current subcommand supports the option debug'
    __fish_kdb_subcommand_includes complete ls mount remount spec-mount
end

function __fish_kdb_subcommand_supports_option_force -d 'Check if the current subcommand supports the option force'
    __fish_kdb_subcommand_includes check merge mount rm
end

function __fish_kdb_subcommand_supports_option_null -d 'Check if the current subcommand supports binary null termination'
    __fish_kdb_subcommand_includes complete find list list-commands ls meta-ls mount
end

function __fish_kdb_subcommand_supports_option_plugins_config -d 'Check if the current subcommand supports plugin configuration'
    __fish_kdb_subcommand_includes check export import info mount spec-mount
end

function __fish_kdb_subcommand_supports_option_verbose -d 'Check if the current subcommand supports the option verbose'
    set -l commands export file meta-get global-mount gmount info mount qt-gui remount rm meta-rm sget shell test help list-tools qt-gui
    __fish_kdb_subcommand_exists_does_not_include $commands
end

# << Printers >>

function __fish_kdb_print_option_color_arguments -d 'Print possible arguments for the option color'
    echo -e 'always\tPrint colored output'
    echo -e 'auto\tAuto detect if colored output makes sense'
    echo -e 'never\tDo not print colored output'
end

function __fish_kdb_print_option_depth_arguments -d 'Print possible arguments for the options min-depth and max-depth'
    set -l description $argv[1]
    set -l start $argv[2]
    seq $start 10 | string replace -r '\d+' "\$0\tComplete at $description \$0 Level" | string replace -r '([02-9]|\d{2}) Level$' '$0s'
end

function __fish_kdb_print_option_editor_arguments -d 'Print possible arguments for the option editor'
    set -l editors atom ed emacs nano pico mate rmate subl vi vim
    for editor in $editors
        set -l editor_path (which $editor)
        and echo -e "$editor_path\t$editor"
    end
end

function __fish_kdb_print_option_namespace_arguments -d 'Print possible arguments for the option namespace'
    echo -e 'dir\tNamespace for directory specific configuration'
    echo -e 'user\tNamespace for user specific configuration'
    echo -e 'system\tNamespace for system wide configuration'
end

function __fish_kdb_print_option_strategy_arguments -d 'Print possible arguments for the option strategy'
    echo -e 'validate\tApply meta data as received from base, and then cut+append all keys as imported'
    __fish_kdb_print_option_strategy_arguments_merge
end

function __fish_kdb_print_option_strategy_arguments_merge -d 'Print possible arguments for the option strategy for the subcommand merge'
    echo -e 'preserve\tAutomatically merge only those keys where just one side deviates from base (default)'
    echo -e 'ours\tWhenever a conflict exists, use our version'
    echo -e 'theirs\tWhenever a conflict exists, use their version'
    echo -e 'cut\tRemoves existing keys below the resulting path and replaces them with the merged keyset'
    echo -e 'import\tPreserves existing keys in the resulting path if they do not exist in the merged keyset'
end

# -- Completions ---------------------------------------------------------------------------------------------------------------------------

# =============
# = Arguments =
# =============

complete -c kdb -n 'not __fish_kdb_subcommand' -x -a '(__fish_kdb_print_subcommands -v)'

set -l arguments complete editor export file fstab get meta-get import ls meta-ls rm meta-rm set meta-set sget spec-mount test umount
set -l completion_function "__fish_kdb_needs_namespace $arguments" 1
complete -c kdb -n "$completion_function" -x -a '(__fish_kdb_print_namespaces)'
complete -c kdb -n '__fish_kdb_needs_namespace cp mv 2' -x -a '(__fish_kdb_print_namespaces)'
complete -c kdb -n '__fish_kdb_needs_namespace merge 4' -x -a '(__fish_kdb_print_namespaces)'
complete -c kdb -n '__fish_kdb_subcommand_mount_needs_namespace' -x -a '(__fish_kdb_print_namespaces)'
complete -c kdb -n '__fish_kdb_subcommand_remount_needs_namespace' -x -a '(__fish_kdb_print_namespaces)'

complete -c kdb -n '__fish_kdb_needs_plugin' -x -a '(__fish_kdb_print_plugins)'

complete -c kdb -n '__fish_kdb_subcommand_mount_needs_plugin' -x -a '(__fish_kdb_print_non_resolver_plugins)'
complete -c kdb -n '__fish_kdb_subcommand_needs_plugin' -x -a '(__fish_kdb_print_non_resolver_plugins)'

complete -c kdb -n '__fish_kdb_subcommand_convert_needs_storage_plugin' -x -a '(__fish_kdb_print_storage_plugins)'
complete -c kdb -n '__fish_kdb_subcommand_needs_storage_plugin' -x -a '(__fish_kdb_print_storage_plugins)'

complete -c kdb -n '__fish_kdb_subcommand_fstab_needs_filesystem' -x -a '(__fish_print_filesystems)'

complete -c kdb -n '__fish_kdb_subcommand_info_needs_clause_name' -x -a '(__fish_kdb_print_clause_names)'

complete -c kdb -n '__fish_kdb_subcommand_needs_metanames' -x -a '(__fish_kdb_print_metanames)'

# ===========
# = Options =
# ===========

# --all -a
__fish_kdb_add_option '__fish_kdb_subcommand_includes get' 'all' 'a' 'Consider all of the keys'

# --color -C
set -l description 'Print never/auto(default)/always colored output'
set -l completion_function '__fish_kdb_subcommand_supports_option_color'
__fish_kdb_add_option "$completion_function" 'color' '' "$description" '(__fish_kdb_print_option_color_arguments)' -f
__fish_kdb_add_option "$completion_function" '' 'C' "Do not color the output"

# --debug -d
set -l description 'Give debug information or ask debug questions (in interactive mode)'
__fish_kdb_add_option '__fish_kdb_subcommand_supports_option_debug' 'debug' 'd' "$description"

# --editor -e
set -l argument_function '__fish_kdb_print_option_editor_arguments'
__fish_kdb_add_option '__fish_kdb_subcommand_includes editor' 'editor' 'e' "Specify which external editor to use" "($argument_function)"

# --first -1
__fish_kdb_add_option '__fish_kdb_subcommand_includes mount' 'first' '1' 'Suppress the first column'

# --force -f
__fish_kdb_add_option '__fish_kdb_subcommand_supports_option_force' 'force' 'f' 'Force the action to be done'

# --help -H
__fish_kdb_add_option '__fish_kdb_subcommand_supports_common_options' 'help' 'H' 'Show the man page'

# --interactive -i
__fish_kdb_add_option '__fish_kdb_subcommand_includes merge mount remount spec-mount' 'interactive' 'i' 'Ask the user interactively'

# --load -l
__fish_kdb_add_option '__fish_kdb_subcommand_includes info' 'load' 'l' 'Load plugin even if system:/elektra is available'

# --max-depth -M
set -l description 'Specify the maximum depth (unlimited by default, 1 to show only the next level), exclusive and relative to the name'
set -l argument_function '__fish_kdb_print_option_depth_arguments most 1'
__fish_kdb_add_option '__fish_kdb_subcommand_includes complete ls' 'max-depth' 'M' "$description" "($argument_function)"

# --min-depth -m
set -l description 'Specify the minimum depth (0 by default), inclusive and relative to the name'
set -l argument_function '__fish_kdb_print_option_depth_arguments least 0'
__fish_kdb_add_option '__fish_kdb_subcommand_includes complete ls' 'min-depth' 'm' "$description" "($argument_function)"

# --no-newline -n
__fish_kdb_add_option '__fish_kdb_subcommand_includes file get meta-get' 'no-newline' 'n' 'Suppress the newline at the end of the output'

# --null -0
__fish_kdb_add_option '__fish_kdb_subcommand_supports_option_null' 'null' '0' 'Use binary 0 termination'

# --plugins-config -c
__fish_kdb_add_option '__fish_kdb_subcommand_supports_option_plugins_config' 'plugins-config' 'c' 'Add a plugin configuration'

# --profile -p
set -l description 'Use a different profile for kdb configuration'
__fish_kdb_add_option '__fish_kdb_subcommand_supports_common_options' 'profile' 'p' "$description" 'current'

# --quiet -q
__fish_kdb_add_option '__fish_kdb_subcommand_includes mount set meta-set spec-mount' 'quiet' 'q' 'Only print error messages'

# --recursive -r
__fish_kdb_add_option '__fish_kdb_subcommand_includes cp mv rm' 'recursive' 'r' 'Work in a recursive mode'

# --resolver -R
set -l argument_function '__fish_kdb_print_resolver_plugins'
set -l description 'Specify the resolver plugin to use'
__fish_kdb_add_option '__fish_kdb_subcommand_includes mount spec-mount' 'resolver' 'R' "$description" "($argument_function)"

# --second -2
__fish_kdb_add_option '__fish_kdb_subcommand_includes mount' 'second' '2' 'Suppress the second column'

# --strategy -s
set -l argument_function '__fish_kdb_print_option_strategy_arguments'
set -l description 'Specify the strategy to resolve conflicts'
set -l options 'strategy' 's'
__fish_kdb_add_option '__fish_kdb_subcommand_includes editor import' $options "$description" "($argument_function)"
set -l argument_function '__fish_kdb_print_option_strategy_arguments_merge'
__fish_kdb_add_option '__fish_kdb_subcommand_includes merge' $options "$description" "($argument_function)"

# --third -3
__fish_kdb_add_option '__fish_kdb_subcommand_includes mount' 'third' '3' 'Suppress the third column'

# --verbose -v
__fish_kdb_add_option '__fish_kdb_subcommand_supports_option_verbose' 'verbose' 'v' 'Explain what is happening'

# --version -V
__fish_kdb_add_option "not __fish_kdb_subcommand; or __fish_kdb_subcommand_supports_common_options" 'version' 'V' 'Print version info'

# --without-elektra -E
__fish_kdb_add_option '__fish_kdb_subcommand_includes export import' 'without-elektra' 'E' 'Omit the `/elektra` directory'

# --with-recommends -W
set -l completion_function '__fish_kdb_subcommand_includes global-mount gmount mount spec-mount'
__fish_kdb_add_option "$completion_function" 'with-recommends' 'W' 'Add recommended plugins'
