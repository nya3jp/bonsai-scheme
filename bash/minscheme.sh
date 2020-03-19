#!/bin/bash -e

IFS=$'\n'

declare -a g_cars g_cdrs

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
    "#"*|"&"*|"@"*)
        unwrap_generic "$value"
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
        die "corrupted value: $value"
        ;;
    esac
}

# split_value() {
#     local value="$1"
#     local mark
#     case "$value" in
#     _*|T*|F*|'$'*)
#         g_return=("${value:0:1}" "${value:1}")
#         ;;
#     "#"*|"&"*)
#         mark="${value:0:1}"
#         value="${value:2}"
#         if [[ ! "$value" =~ ^[^]]*] ]]; then
#             die "corrupted value"
#         fi
#         g_return=("${mark}[${BASH_REMATCH%]}]" "${value#$BASH_REMATCH}")
#         ;;
#     "("*)
#         value="${value#(}"
#         split_value "$value"
#         value="${g_return[0]}"
#         split_value "${g_return[1]#.}"
#         g_return=("(${value}.${g_return[0]})" "${g_return[1]#)}")
#         ;;
#     *)
#         die "corrupted value: $value"
#         ;;
#     esac
# }

# car() {
#     local value="$1"
#     if [[ "$value" != '('*')' ]]; then
#         die "not a pair"
#     fi
#     value="${value#(}"
#     value="${value%)}"
#     split_value "$value"
#     g_return="${g_return[0]}"
# }

# cdr() {
#     local value="$1"
#     if [[ "$value" != '('*')' ]]; then
#         die "not a pair"
#     fi
#     value="${value#(}"
#     value="${value%)}"
#     split_value "$value"
#     g_return="${g_return[1]#.}"
# }

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
    eval "declare -r ${env}_parent='$parent'"
    eval "declare -a ${env}_vars=()"
    g_return="$env"
}

set_env() {
    local env=$1
    local key=$2
    local value=$3
    local tmp="#${env}_vars[@]"
    local env_size="${!tmp}"
    local i akey avalue
    for (( i = 0; i < env_size; i++ )); do
        tmp="${env}_vars[${i}]"
        IFS=' ' read -r akey avalue <<< "${!tmp}"
        if [[ "$akey" == "$key" ]]; then
            eval "${env}_vars[${i}]=\"\$key \$value\""
            return
        fi
    done
    eval "${env}_vars+=(\"\$key \$value\")"
}

lookup_env() {
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
    die "name not found: $key"
}

run_lambda() {
    die "run_lambda: $*"
}

form_begin() {
    local env=$1
    shift
    g_return="_"
    local expr
    for expr in "$@"; do
        evaluate "$env" "$expr"
    done
}

form_quote() {
    g_return=$2
}

form_lambda() {
    local env=$1
    local raw_params=$2
    shift 2
    array_to_value "$@"
    local body="$g_return"
    local f="run_lambda ${env@Q} ${raw_params@Q} ${body@Q}"

}

evaluate_form() {
    local env=$1
    local name=$2
    shift 2
    local args=("$@")
    case "$name" in
    begin) form_begin "$env" "${args[@]}";;
    quote) form_quote "$env" "${args[@]}";;
    *) g_return="";;
    esac
}

evaluate() {
    local env=$1
    local expr=$2
    local top args i
    stringify "$expr"
    echo "DEBUG: eval: $g_return" >&2
    case "$expr" in
    _|T|F|"#"*) g_return="$expr";;
    "&"*)
        unwrap_symbol "$expr"
        lookup_env "$env" "$g_return"
        ;;
    "%"*)
        unwrap_pair "$expr"
        value_to_array "${g_cdrs[$g_return]}"
        args=("${g_return[@]}")
        unwrap_pair "$expr"
        top="${g_cars[$g_return]}"
        if [[ "$top" == "&"* ]]; then
            unwrap_symbol "$top"
            # echo "DEBUG: form: $g_return ${args[@]}" >&2
            evaluate_form "$env" "$g_return" "${args[@]}"
            if [[ -n "$g_return" ]]; then
                return
            fi
        fi
        evaluate "$env" "$top"
        unwrap_func "$g_return"
        top="${g_return#* }"
        for (( i = 0; i < ${#args[@]}; i++ )); do
            evaluate "$env" "${args[$i]}"
            args[$i]="$g_return"
        done
        IFS=' '
        local call="$top ${args[*]}"
        IFS=$'\n'
        echo "DEBUG: start func: $call" >&2
        $top "${args[@]}"
        echo "DEBUG: end func: $call = $g_return" >&2
        ;;
    *)
        stringify "$expr"
        die "not evaluatable: $g_return"
        ;;
    esac
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
    set_env "$env" "$name" "@[$name $func]"
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
    for expr in "${exprs[@]}"; do
        evaluate "$env" "$expr"
        if [[ "$g_return" != "_" ]]; then
            stringify "$g_return"
            echo "$g_return"
        fi
    done
}

main "$@"
