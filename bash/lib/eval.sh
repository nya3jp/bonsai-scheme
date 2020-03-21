evaluate_expr() {
    local env=$1
    local expr=$2
    if $DEBUG; then
        stringify "$expr"
        local expr_str="$g_return"
        echo "DEBUG: eval start: ${expr_str} on ${env}" >&2
    fi
    case "$expr" in
    _|T|F|"#"*) g_return="$expr";;
    "&"*)
        unwrap_symbol "$expr"
        get_env "$env" "$g_return"
        ;;
    "%"*)
        unwrap_pair "$expr"
        value_to_array "${g_cdrs[$g_return]}"
        local args=("${g_return[@]}")
        unwrap_pair "$expr"
        local top="${g_cars[$g_return]}"
        if [[ "$top" == "&"* ]]; then
            unwrap_symbol "$top"
            evaluate_form "$env" "$g_return" "${args[@]}"
            if [[ -n "$g_return" ]]; then
                if $DEBUG; then
                    local raw_return="$g_return"
                    stringify "$g_return"
                    echo "DEBUG: end eval: $expr_str => $g_return" >&2
                    g_return="$raw_return"
                fi
                return
            fi
        fi
        evaluate_expr "$env" "$top"
        unwrap_func "$g_return"
        local call="${g_return#* }"
        local i
        for (( i = 0; i < ${#args[@]}; i++ )); do
            evaluate_expr "$env" "${args[$i]}"
            call="$call $(printf %q "$g_return")"
        done
        #echo "DEBUG: start func: $call" >&2
        eval "$call"
        #echo "DEBUG: end func: $call = $g_return" >&2
        ;;
    *)
        stringify "$expr"
        die "not evaluatable: $g_return"
        ;;
    esac
    if $DEBUG; then
        local raw_return="$g_return"
        stringify "$g_return"
        echo "DEBUG: eval end: ${expr_str} => ${g_return} on ${env}" >&2
        g_return="$raw_return"
    fi
}

evaluate_list() {
    local env=$1
    shift
    g_return="_"
    local expr
    for expr in "$@"; do
        evaluate_expr "$env" "$expr"
    done
}
