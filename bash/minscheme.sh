#!/bin/bash -e

IFS=$'\n'

declare -r root_dir="$(dirname "$0")"

source "$root_dir/lib/builtins.sh"
source "$root_dir/lib/env.sh"
source "$root_dir/lib/eval.sh"
source "$root_dir/lib/forms.sh"
source "$root_dir/lib/parser.sh"
source "$root_dir/lib/utils.sh"
source "$root_dir/lib/values.sh"

main() {
    if [[ -z "$1" ]]; then
        die "REPL not implemented"
    fi

    parse_file "$1"
    local exprs=("${g_return[@]}")

    local env expr
    new_top_level_env
    env="$g_return"
    evaluate_list "$env" "${exprs[@]}"
}

main "$@"
