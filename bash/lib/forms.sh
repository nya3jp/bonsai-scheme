run_lambda() {
    local env=$1
    local raw_names=$2
    local body=$3
    shift 3
    local args=("$@")

    value_to_array "$raw_names"
    local names=("${g_return[@]}")

    new_env "$env"
    env="$g_return"

    #echo "DEBUG: run_lambda: env=${env}" >&2

    local i
    for (( i = 0; i < ${#names[@]}; i++ )); do
        unwrap_symbol "${names[$i]}"
        put_env "$env" "$g_return" "${args[$i]}"
    done

    value_to_array "$body"
    evaluate_list "$env" "${g_return[@]}"
}

form_begin() {
    evaluate_list "$@"
}

form_quote() {
    g_return=$2
}

form_lambda() {
    local env=$1
    local raw_names=$2
    shift 2
    array_to_value "$@"
    local body="$g_return"
    g_return="@[<lambda> run_lambda $(printf %q "$env") $(printf %q "$raw_names") $(printf %q "$body")]"
}

form_define() {
    local env=$1
    local raw_proto=$2
    shift 2

    if [[ "$raw_proto" == "&"* ]]; then
        unwrap_symbol "$raw_proto"
        local name="$g_return"
        evaluate_list "$env" "$@"
        put_env "$env" "$name" "$g_return"
        g_return="_"
        return
    fi

    unwrap_pair "$raw_proto"
    unwrap_symbol "${g_cars[$g_return]}"
    local name="$g_return"
    unwrap_pair "$raw_proto"
    local raw_names="${g_cdrs[$g_return]}"
    array_to_value "$@"
    local body="$g_return"
    put_env "$env" "$name" "@[<func:${name}> run_lambda $(printf %q "$env") $(printf %q "$raw_names") $(printf %q "$body")]"
    g_return="_"
}

form_let() {
    local env=$1
    value_to_array "$2"
    local bindings=("${g_return[@]}")
    shift 2

    new_env "$env"
    local let_env="$g_return"

    local binding name value
    for binding in "${bindings[@]}"; do
        value_to_array "$binding"
        name="${g_return[0]}"
        value="${g_return[1]}"
        unwrap_symbol "$name"
        name="$g_return"
        evaluate_expr "$env" "$value"
        value="$g_return"
        put_env "$let_env" "$name" "$value"
    done

    evaluate_list "$let_env" "$@"
}

form_let_star() {
    local env=$1
    value_to_array "$2"
    local bindings=("${g_return[@]}")
    shift 2

    local let_env="$env"

    local binding name value
    for binding in "${bindings[@]}"; do
        value_to_array "$binding"
        name="${g_return[0]}"
        value="${g_return[1]}"
        unwrap_symbol "$name"
        name="$g_return"
        evaluate_expr "$let_env" "$value"
        value="$g_return"
        new_env "$let_env"
        let_env="$g_return"
        put_env "$let_env" "$name" "$value"
    done

    new_env "$let_env"
    let_env="$g_return"

    evaluate_list "$let_env" "$@"
}

form_letrec() {
    local env=$1
    value_to_array "$2"
    local bindings=("${g_return[@]}")
    shift 2

    new_env "$env"
    local let_env="$g_return"

    local binding name value
    for binding in "${bindings[@]}"; do
        value_to_array "$binding"
        name="${g_return[0]}"
        value="${g_return[1]}"
        unwrap_symbol "$name"
        name="$g_return"
        evaluate_expr "$let_env" "$value"
        value="$g_return"
        put_env "$let_env" "$name" "$value"
    done

    evaluate_list "$let_env" "$@"
}

form_if() {
    local env=$1
    local cond=$2
    local then=$3
    local else=$4

    evaluate_expr "$env" "$cond"
    if [[ "$g_return" == F ]]; then
        if [[ -n "$else" ]]; then
            evaluate_expr "$env" "$else"
        else
            g_return="_"
        fi
        return
    fi
    evaluate_expr "$env" "$then"
}

form_cond() {
    local env=$1
    shift 1

    local branch cond then
    for branch in "$@"; do
        value_to_array "$branch"
        cond="${g_return[0]}"
        then="${g_return[1]}"
        if [[ "$cond" == "&[else]" ]]; then
            evaluate_expr "$env" "$then"
            return
        fi
        evaluate_expr "$env" "$cond"
        if [[ "$g_return" != "F" ]]; then
            evaluate_expr "$env" "$then"
            return
        fi
    done
    g_return="_"
}

form_set() {
    local env=$1
    local name=$2
    local expr=$3

    unwrap_symbol "$name"
    name="$g_return"
    evaluate_expr "$env" "$expr"
    set_env "$env" "$name" "$g_return"
    g_return="_"
}

form_set_car() {
    local env=$1
    local target=$2
    local expr=$3

    evaluate_expr "$env" "$target"
    unwrap_pair "$g_return"
    local id="$g_return"
    evaluate_expr "$env" "$expr"
    g_cars[$id]="$g_return"
    g_return="_"
}

form_set_cdr() {
    local env=$1
    local target=$2
    local expr=$3

    evaluate_expr "$env" "$target"
    unwrap_pair "$g_return"
    local id="$g_return"
    evaluate_expr "$env" "$expr"
    g_cdrs[$id]="$g_return"
    g_return="_"
}

evaluate_form() {
    local env=$1
    local name=$2
    shift 2
    local args=("$@")
    case "$name" in
    begin) form_begin "$env" "${args[@]}";;
    quote) form_quote "$env" "${args[@]}";;
    lambda) form_lambda "$env" "${args[@]}";;
    define) form_define "$env" "${args[@]}";;
    let) form_let "$env" "${args[@]}";;
    "let*") form_let_star "$env" "${args[@]}";;
    letrec) form_letrec "$env" "${args[@]}";;
    if) form_if "$env" "${args[@]}";;
    cond) form_cond "$env" "${args[@]}";;
    "set!") form_set "$env" "${args[@]}";;
    "set-car!") form_set_car "$env" "${args[@]}";;
    "set-cdr!") form_set_cdr "$env" "${args[@]}";;
    *) g_return="";;
    esac
}
