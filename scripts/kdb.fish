function __fish_kdb_complete -d 'Print a list of kdb subcommands'
    kdb list-commands -v | awk '{if(NR>1)print}'
end

complete -c kdb -x -a '(__fish_kdb_complete)'
