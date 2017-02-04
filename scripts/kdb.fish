# -- Functions -----------------------------------------------------------------------------------------------------------------------------

function __input_includes -d 'Check if the current command buffer contains one of the given values'
    for input in (commandline -op)
        if contains -- $input $argv
            return 0
        end
    end
    return 1
end

function __input_includes_options -d 'Check if the current command buffer contains one of the given options'
    set -l opt_long $argv[1]
    set -l opt_short $argv[2]

    test -n $opt_long
    and set -l options --$opt_long

    test -n $opt_short
    and set -l options $options -$opt_short

    __input_includes $options
end

function __input_left_includes_options -d 'Check if the input to the left of the current buffer contains one of the given options'
    set -l opt_long $argv[1]
    set -l opt_short $argv[2]
    set -l input (commandline -opc)

    test -n $opt_long
    set opt_long "--$opt_long"
    test -n $opt_short
    set opt_short "-\w*$opt_short"
    set -l options (__join '|' $opt_long $opt_short)

    string match -r -- $options "$input"
end

function __join -d 'Join variables into one variable using a given separator'
    set -l separator $argv[1]
    set -l joined ''

    for element in $argv[2..-2]
        test -n $element
        and set joined "$joined$element$separator"
    end
    test -n $argv[-1]
    and set joined "$joined$argv[-1]"

    echo $joined
end

function __fish_kdb_subcommand_includes -d 'Check if the current kdb subcommand is one of the given subcommands'
    set -l subcommand (__fish_kdb_subcommand)
    contains -- "$subcommand" $argv
end

function __fish_kdb_subcommand_does_not_include -d 'Check if the current kdb subcommand does not include any of the given subcommands'
    set -l subcommand (__fish_kdb_subcommand)

    test -z $subcommand
    and return 1

    not contains -- "$subcommand" $argv
end

function __fish_kdb_subcommand -d 'Check for and print the current kdb subcommand'
    set -l input (commandline -op)

    test (count $input) -le 1
    and return 1

    set -l subcommand $input[2]
    if contains -- $subcommand (__fish_kdb_print_subcommands)
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

function __fish_kdb_needs_plugin -d 'Check if the current command needs a plugin completion'
    __fish_kdb_subcommand_includes check
    and not __input_includes (__fish_kdb_print_plugins)
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
    kdb complete --max-depth=1 -- "$namespace" | string match -vr '(dir|proc|spec|user)$'
end

function __fish_kdb_print_plugins -d 'Print a list of available plugins'
    kdb list
end

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

# ===========
# = Options =
# ===========

function __fish_kdb_subcommand_supports_option_color -d 'Check if the current subcommand supports colored output'
    __fish_kdb_subcommand_does_not_include complete help list-tools qt-gui
end

function __fish_kdb_subcommand_supports_option_debug -d 'Check if the current subcommand supports the option debug'
    __fish_kdb_subcommand_includes complete mount remount smount spec-mount
end

function __fish_kdb_subcommand_supports_option_force -d 'Check if the current subcommand supports the option force'
    __fish_kdb_subcommand_includes check merge
end

function __fish_kdb_subcommand_supports_option_null -d 'Check if the current subcommand supports binary null termination'
    __fish_kdb_subcommand_includes complete list list-commands ls lsmeta mount
end

function __fish_kdb_subcommand_supports_option_plugins_config -d 'Check if the current subcommand supports plugin configuration'
    __fish_kdb_subcommand_includes check export import info mount smount spec-mount
end

function __fish_kdb_subcommand_supports_option_verbose -d 'Check if the current subcommand supports the option verbose'
    set -l commands export file getmeta global-mount gmount info mount qt-gui remount rm sget shell test vset help list-tools qt-gui
    __fish_kdb_subcommand_does_not_include $commands
end

function __fish_kdb_subcommand_supports_common_options -d 'Check if the current subcommand supports common options'
    __fish_kdb_subcommand_does_not_include help list-tools qt-gui
end

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

# -- Completions ---------------------------------------------------------------------------------------------------------------------------

# =============
# = Arguments =
# =============

complete -c kdb -n 'not __fish_kdb_subcommand' -x -a '(__fish_kdb_print_subcommands -v)'
complete -c kdb -n '__fish_kdb_needs_namespace' -x -a '(__fish_kdb_print_namespaces)'
complete -c kdb -n '__fish_kdb_needs_plugin' -x -a '(__fish_kdb_print_plugins)'

# ===========
# = Options =
# ===========

# --color -C
set -l description 'Print never/auto(default)/always colored output'
set -l completion_function '__fish_kdb_subcommand_supports_option_color'
__fish_kdb_add_option "$completion_function" 'color' '' "$description" '(__fish_kdb_print_option_color_arguments)' -f
__fish_kdb_add_option "$completion_function" '' 'C' "Do not color the output"

# --debug -d
set -l description 'Give debug information or ask debug questions (in interactive mode)'
__fish_kdb_add_option '__fish_kdb_subcommand_supports_option_debug' 'debug' 'd' "$description"

# --force -f
__fish_kdb_add_option '__fish_kdb_subcommand_supports_option_force' 'force' 'f' 'Force the action to be done'

# --help -H
__fish_kdb_add_option '__fish_kdb_subcommand_supports_common_options' 'help' 'H' 'Show the man page'

# --max-depth -M
set -l description 'Specify the maximum depth of completion suggestions (unlimited by default, 1 to show only the next level), inclusive.'
set -l argument_function '__fish_kdb_print_option_depth_arguments most 1'
__fish_kdb_add_option '__fish_kdb_subcommand_includes complete' 'max-depth' 'M' "$description" "($argument_function)"

# --min-depth -m
set -l description 'Specify the minimum depth of completion suggestions (0 by default), exclusive'
set -l argument_function '__fish_kdb_print_option_depth_arguments least 0'
__fish_kdb_add_option '__fish_kdb_subcommand_includes complete' 'min-depth' 'm' "$description" "($argument_function)"

# --null -0
__fish_kdb_add_option '__fish_kdb_subcommand_supports_option_null' 'null' '0' 'Use binary 0 termination'

# --plugins-config -c
__fish_kdb_add_option '__fish_kdb_subcommand_supports_option_plugins_config' 'plugins-config' 'c' 'Add a plugin configuration'

# --profile -p
set -l description 'Use a different profile for kdb configuration'
__fish_kdb_add_option '__fish_kdb_subcommand_supports_common_options' 'profile' 'p' "$description" 'current'

# --verbose -v
__fish_kdb_add_option '__fish_kdb_subcommand_supports_option_verbose' 'verbose' 'v' 'Explain what is happening'

# --version -V
__fish_kdb_add_option "not __fish_kdb_subcommand; or __fish_kdb_subcommand_supports_common_options" 'version' 'V' 'Print version info'
