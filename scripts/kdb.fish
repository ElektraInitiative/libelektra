# -- Functions -----------------------------------------------------------------------------------------------------------------------------

function __input_includes -d 'Check if the current command buffer contains one of the given values'
    for input in (commandline -opc)
        if contains -- $input $argv
            return 0
        end
    end
    return 1
end

function __fish_kdb_no_subcommand -d 'Check if the current commandline buffer does not contain a subcommand'
    not __input_includes (__fish_kdb_print_subcommands)
end

function __fish_kdb_needs_namespace -d 'Check if the current command needs a namespace completion'
    __input_includes ls get set
end

function __fish_kdb_print_subcommands -d 'Print a list of kdb subcommands'
    set -l commands (kdb list-commands $argv)
    if contains -- $argv -v
        set commands (printf '%s\n' $commands | awk '{if(NR>1)print}')
    end
    printf '%s\n' $commands
end

function __fish_kdb_print_namespaces -d 'Print a list of possible namespace completions'
    set -l namespace (commandline -ct)
    kdb complete --max-depth=1 "$namespace"
end

# -- Completions ---------------------------------------------------------------------------------------------------------------------------

complete -c kdb -n '__fish_kdb_no_subcommand' -x -a '(__fish_kdb_print_subcommands -v)'
complete -c kdb -n '__fish_kdb_needs_namespace' -x -a '(__fish_kdb_print_namespaces)'
