function __fish_kdb_no_subcommand -d 'Check if the current commandline buffer does not contain a subcommand'
    set subcommands (__fish_kdb_print_subcommands)
    for input in (commandline -opc)
        if contains -- $input $subcommands
            return 1
        end
    end
    return 0
end

function __fish_kdb_print_subcommands -d 'Print a list of kdb subcommands'
    kdb list-commands $argv | awk '{if(NR>1)print}'
end

complete -c kdb -n '__fish_kdb_no_subcommand' -x -a '(__fish_kdb_print_subcommands -v)'
