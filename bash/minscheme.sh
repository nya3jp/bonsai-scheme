#!/bin/bash -e

IFS=$'\n'

declare -a g_cars g_cdrs
declare -r DEBUG=false

die() {
    echo "$@" >&2
    exit 1
}

make_pair() {
    local id=${#g_cars[@]}
    g_cars[$id]=$1
    g_cdrs[$id]=$2
    g_return="%[${id}]"
}

unwrap_generic() {
    local value=$1
    value="${value:2}"
    value="${value%]}"
    g_return="$value"
}

unwrap_integer() {
    local value=$1
    if [[ "$value" != "#"* ]]; then
        die "not an integer"
    fi
    unwrap_generic "$value"
}

unwrap_symbol() {
    local value=$1
    if [[ "$value" != "&"* ]]; then
        die "not a symbol"
    fi
    unwrap_generic "$value"
}

unwrap_pair() {
    local value=$1
    if [[ "$value" != "%"* ]]; then
        die "not a pair"
    fi
    unwrap_generic "$value"
}

unwrap_func() {
    local value=$1
    if [[ "$value" != "@"* ]]; then
        die "not a function"
    fi
    unwrap_generic "$value"
}

stringify() {
    local value=$1
    local id tmp
    case "$value" in
    _) g_return="#undef";;
    T) g_return="#t";;
    F) g_return="#f";;
    '$') g_return="()";;
    "#"*|"&"*)
        unwrap_generic "$value"
        ;;
    "@"*)
        unwrap_generic "$value"
        g_return="${g_return%% *}"
        ;;
    "%"*)
        unwrap_pair "$value"
        id="$g_return"
        stringify "${g_cars[$id]}"
        tmp="$g_return"
        stringify "${g_cdrs[$id]}"
        g_return="(${tmp} . ${g_return})"
        ;;
    *)
        die "stringify: corrupted value: $value"
        ;;
    esac
}

skip() {
    local c
    while :; do
        c="${g_code:0:1}"
        if [[ -z "$c" ]]; then
            break
        fi
        if [[ "$c" != ' ' && "$c" != $'\n' ]]; then
            break
        fi
        g_code="${g_code:1}"
    done
}

array_to_value() {
    local value='$'
    local i v
    for (( i = $#; i >= 1; i-- )); do
        v="${!i}"
        make_pair "$v" "$value"
        value="$g_return"
    done
    g_return="$value"
}

value_to_array() {
    local value=$1
    local array=()
    while [[ "$value" == '%'* ]]; do
        unwrap_pair "$value"
        array+=("${g_cars[$g_return]}")
        value="${g_cdrs[$g_return]}"
    done
    if [[ "$value" != '$' ]]; then
        die "not a list: $1"
    fi
    g_return=("${array[@]}")
}

parse_partial_value() {
    local value values token i j c
    if [[ "$g_code" == \'* ]]; then
        g_code=${g_code:1}
        parse_partial_value
        make_pair "$g_return" '$'
        make_pair '&[quote]' "$g_return"
        return
    fi

    if [[ "$g_code" == '('* ]]; then
        g_code=${g_code:1}
        parse_partial_list
        skip
        if [[ "$g_code" != ')'* ]]; then
            die "unexpected end of list"
        fi
        g_code=${g_code:1}
        array_to_value "${g_return[@]}"
        return
    fi

    token=
    i=0
    while :; do
        c=${g_code:$i:1}
        if [[ "$c" == '' || "$c" == ' ' || "$c" == $'\n' || "$c" == ')' || "$c" == ';' ]]; then
            break
        fi
        i=$((i + 1))
    done
    token=${g_code:0:$i}
    g_code=${g_code:$i}
    if [[ "$token" =~ ^-?[0-9]+$ ]]; then
        g_return="#[${token}]"
    else
        case "$token" in
        '#f') g_return='F';;
        '#t') g_return='T';;
        *) g_return="&[${token}]"
        esac
    fi
}

parse_partial_list() {
    local value
    local values=()
    while :; do
        skip
        if [[ -z "$g_code" || "$g_code" == ')'* ]]; then
            break
        fi
        parse_partial_value
        values+=("$g_return")
    done
    g_return=("${values[@]}")
}

parse_list() {
    g_code="$1"
    parse_partial_list
    skip
    if [[ -n "$g_code" ]]; then
        die "excess parentheses"
    fi
}

parse_file() {
    parse_list "$(cat -- "$1")"
}

env_next_id=1

new_env() {
    local parent=$1
    local env="env${env_next_id}"
    : $(( env_next_id++ ))
    eval "${env}_parent='$parent'"
    eval "${env}_vars=()"
    g_return="$env"
}

put_env() {
    local env=$1
    local key=$2
    local value=$3
    local vars_size
    eval "vars_size=\${#${env}_vars[@]}"
    local tmp i akey avalue
    for (( i = 0; i < vars_size; i++ )); do
        tmp="${env}_vars[${i}]"
        IFS=' ' read -r akey avalue <<< "${!tmp}"
        if [[ "$akey" == "$key" ]]; then
            if $DEBUG; then
                stringify "$value"
                echo "DEBUG: put_env: ${env}: $key = $g_return (overwrite)" >&2
            fi
            eval "${env}_vars[${i}]=\"\$key \$value\""
            return
        fi
    done
    if $DEBUG; then
        stringify "$value"
        echo "DEBUG: put_env: ${env}: $key = $g_return (new)" >&2
    fi
    eval "${env}_vars+=(\"\$key \$value\")"
}

set_env() {
    local env=$1
    local key=$2
    local value=$3
    local vars_size
    local tmp i akey avalue
    while [[ -n "$env" ]]; do
        eval "vars_size=\${#${env}_vars[@]}"
        for (( i = 0; i < vars_size; i++ )); do
            tmp="${env}_vars[${i}]"
            IFS=' ' read -r akey avalue <<< "${!tmp}"
            if [[ "$akey" == "$key" ]]; then
                if $DEBUG; then
                    stringify "$value"
                    echo "DEBUG: set_env: ${env}: $key = $g_return (overwrite)" >&2
                fi
                eval "${env}_vars[${i}]=\"\$key \$value\""
                return
            fi
        done
        tmp="${env}_parent"
        env="${!tmp}"
    done
    die "set_env: name not found: $key"
}

get_env() {
    local env=$1
    local key=$2
    local tmp entry akey avalue
    while [[ -n "$env" ]]; do
        tmp="${env}_vars[@]"
        for entry in "${!tmp}"; do
            IFS=' ' read -r akey avalue <<< "$entry"
            if [[ "$akey" == "$key" ]]; then
                g_return="$avalue"
                return
            fi
        done
        tmp="${env}_parent"
        env="${!tmp}"
    done
    die "get_env: name not found: $key"
}

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

builtin_print() {
    stringify "$1"
    echo "$g_return"
    g_return="_"
}

builtin_and() {
    local value=T
    local arg
    for arg in "$@"; do
        if [[ "$arg" == F ]]; then
            value=F
        fi
    done
    g_return="$value"
}

builtin_or() {
    local value=F
    local arg
    for arg in "$@"; do
        if [[ "$arg" != F ]]; then
            value=T
        fi
    done
    g_return="$value"
}

builtin_not() {
    if [[ "$1" == F ]]; then
        g_return=T
    else
        g_return=F
    fi
}

builtin_add() {
    local value=0
    local arg
    for arg in "$@"; do
        unwrap_integer "$arg"
        value=$(( value + g_return ))
    done
    g_return="#[$value]"
}

builtin_sub() {
    unwrap_integer "$1"
    local value="$g_return"
    shift 1
    local arg
    for arg in "$@"; do
        unwrap_integer "$arg"
        value=$(( value - g_return ))
    done
    g_return="#[$value]"
}

builtin_mul() {
    local value=1
    local arg
    for arg in "$@"; do
        unwrap_integer "$arg"
        value=$(( value * g_return ))
    done
    g_return="#[$value]"
}

builtin_div() {
    unwrap_integer "$1"
    local value="$g_return"
    shift 1
    local arg
    for arg in "$@"; do
        unwrap_integer "$arg"
        value=$(( value / g_return ))
    done
    g_return="#[$value]"
}

builtin_eq() {
    local value=$1
    local arg
    shift 1
    for arg in "$@"; do
        if [[ "$value" != "$arg" ]]; then
            g_return="F"
            return
        fi
    done
    g_return="T"
}

builtin_lt() {
    unwrap_integer "$1"
    local lhs="$g_return"
    shift 1
    local arg
    for arg in "$@"; do
        unwrap_integer "$arg"
        if (( lhs >= g_return )); then
            g_return="F"
            return
        fi
        lhs="$g_return"
    done
    g_return="T"
}

builtin_lte() {
    unwrap_integer "$1"
    local lhs="$g_return"
    shift 1
    local arg
    for arg in "$@"; do
        unwrap_integer "$arg"
        if (( lhs > g_return )); then
            g_return="F"
            return
        fi
        lhs="$g_return"
    done
    g_return="T"
}

builtin_gt() {
    unwrap_integer "$1"
    local lhs="$g_return"
    shift 1
    local arg
    for arg in "$@"; do
        unwrap_integer "$arg"
        if (( lhs <= g_return )); then
            g_return="F"
            return
        fi
        lhs="$g_return"
    done
    g_return="T"
}

builtin_gte() {
    unwrap_integer "$1"
    local lhs="$g_return"
    shift 1
    local arg
    for arg in "$@"; do
        unwrap_integer "$arg"
        if (( lhs < g_return )); then
            g_return="F"
            return
        fi
        lhs="$g_return"
    done
    g_return="T"
}

builtin_cons() {
    make_pair "$1" "$2"
}

builtin_car() {
    unwrap_pair "$1"
    g_return="${g_cars[$g_return]}"
}

builtin_cdr() {
    unwrap_pair "$1"
    g_return="${g_cdrs[$g_return]}"
}

register_builtin() {
    local env="$1"
    local name="$2"
    local func="$3"
    put_env "$env" "$name" "@[<builtin:$name> $func]"
}

new_top_level_env() {
    new_env
    local env="$g_return"
    register_builtin "$env" "print" builtin_print
    register_builtin "$env" "and" builtin_and
    register_builtin "$env" "or" builtin_or
    register_builtin "$env" "not" builtin_not
    register_builtin "$env" "+" builtin_add
    register_builtin "$env" "-" builtin_sub
    register_builtin "$env" "*" builtin_mul
    register_builtin "$env" "/" builtin_div
    register_builtin "$env" "=" builtin_eq
    register_builtin "$env" "<" builtin_lt
    register_builtin "$env" "<=" builtin_lte
    register_builtin "$env" ">" builtin_gt
    register_builtin "$env" ">=" builtin_gte
    register_builtin "$env" "eq?" builtin_eq
    register_builtin "$env" "cons" builtin_cons
    register_builtin "$env" "car" builtin_car
    register_builtin "$env" "cdr" builtin_cdr
    g_return="$env"
}

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
