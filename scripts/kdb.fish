function __fish_kdb_complete --description 'Print a list of kdb subcommands'
    kdb list-commands -v | awk '{if(NR>1)print}'
end

complete --command kdb --exclusive --arguments '(__fish_kdb_complete)'
