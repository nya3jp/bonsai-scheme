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
