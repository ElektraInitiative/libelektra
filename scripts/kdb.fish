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
    and return 0

    return 1
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

function __fish_kdb_option_verbose -d 'Check if the current command allows the option verbose'
    set subcommand (__fish_kdb_subcommand)
    contains -- "$subcommand" export file getmeta global-mount gmount info mount qt-gui remount rm sget shell test vset help
    and return 1

    not __input_includes '-v' '--verbose'
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
    kdb complete --max-depth=1 "$namespace"
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

complete -c kdb -n '__fish_kdb_option_verbose' -l 'verbose' -s 'v' -d 'Explain what is happening'
