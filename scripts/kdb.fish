# -- Functions -----------------------------------------------------------------------------------------------------------------------------

function __input_includes -d 'Check if the current command buffer contains one of the given values'
    for input in (commandline -opc)
        if contains -- $input $argv
            return 0
        end
    end
    return 1
end

function __fish_kdb_subcommand_includes -d 'Check if the current kdb subcommand is one of the given subcommands'
    set -l subcommand (__fish_kdb_subcommand)
    contains -- "$subcommand" $argv
end

function __fish_kdb_subcommand -d 'Check for and print the current kdb subcommand'
    set -l input (commandline -op)

    test (count $input) -le 1
    and return 1

    set -l subcommand $input[2]
    if contains -- $subcommand (__fish_kdb_print_subcommands) then
        echo "$subcommand"
        return 0
    end

    return 1
end

function __fish_kdb_is_namespace -d 'Check if the given argument is a namespace'
    string match -r -- '^(dir|proc|spec|system|user|\/).*' "$argv" >/dev/null
end

function __fish_kdb_needs_namespace -d 'Check if the current command needs a namespace completion'
    not __fish_kdb_subcommand_includes ls get set
    and return 1

    __fish_kdb_is_namespace (commandline -t)
    and return 0

    for input in (commandline -opc)
        __fish_kdb_is_namespace $input
        and return 1
    end

    return 0
end

function __fish_kdb_print_subcommands -d 'Print a list of kdb subcommands'
    set -l commands (kdb list-commands $argv)
    if contains -- $argv -v
        set commands (printf '%s\n' $commands | awk '{if(NR>1)print}' | sed 's/\.$//')
    end
    printf '%s\n' $commands
end

function __fish_kdb_print_namespaces -d 'Print a list of possible namespace completions'
    set -l namespace (commandline -ct)
    kdb complete --max-depth=1 "$namespace" | string match -vr '(dir|proc|spec|user)$'
end

function __fish_kdb_add_option -d 'Add suggestions for a certain option to multiple kdb subcommands'
    set -l completion_function $argv[1]
    set -l opt_long $argv[2]
    set -l opt_short $argv[3]
    set -l description $argv[4..-1]

    set completion_function "$completion_function; and not __input_includes --$opt_long -$opt_short"

    complete -c kdb -n "$completion_function" -l $opt_long -s $opt_short -d "$description"
end

# ===========
# = Options =
# ===========

function __fish_kdb_subcommand_supports_option_null -d 'Check if the current subcommand supports binary null termination'
    __fish_kdb_subcommand_includes complete list list-commands ls lsmeta mount
end

function __fish_kdb_subcommand_supports_option_verbose -d 'Check if the current subcommand supports the option verbose'
    not __fish_kdb_subcommand_includes export file getmeta global-mount gmount info mount qt-gui remount rm sget shell test vset help qt-gui
end

function __fish_kdb_subcommand_supports_common_options -d 'Check if the current subcommand supports common options'
    not __fish_kdb_subcommand_includes help qt-gui
end

# -- Completions ---------------------------------------------------------------------------------------------------------------------------

# =============
# = Arguments =
# =============

complete -c kdb -n 'not __fish_kdb_subcommand' -x -a '(__fish_kdb_print_subcommands -v)'
complete -c kdb -n '__fish_kdb_needs_namespace' -x -a '(__fish_kdb_print_namespaces)'

# ===========
# = Options =
# ===========

__fish_kdb_add_option '__fish_kdb_subcommand_supports_common_options' 'help' 'H' 'Show the man page'
__fish_kdb_add_option '__fish_kdb_subcommand_supports_option_null' 'null' '0' 'Use binary 0 termination.'
__fish_kdb_add_option '__fish_kdb_subcommand_supports_option_verbose' 'verbose' 'v' 'Explain what is happening'
__fish_kdb_add_option '__fish_kdb_subcommand_supports_common_options' 'version' 'V' 'Print version info'
